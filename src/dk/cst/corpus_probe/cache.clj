(ns dk.cst.corpus-probe.cache
  "Query results kept as CQP's own saved query results, so that paging or
  re-sorting a search does not re-run it.

  `save` writes a named query result to `<data directory>/<CORPUS>:<name>`,
  and a later process reads it back with the matches in the order they were
  saved in, so a page costs one `cat` rather than a query and a sort. Each
  corpus gets a directory of its own, because CQP registers every file of
  the one it is given on every startup (docs/research/gap-nqr-persistence.md
  section 2; the comment block at the end measures both).

  Invalidation is entirely this application's job, because CQP does none of
  it and fails silently when a file is wrong: a save file written against an
  earlier build of a corpus crashes CQP with SIGBUS and no message at all,
  and a half-written one is read back as garbage without an error. So the
  name of a result carries a build stamp of the corpus, and CQP saves under
  a name nothing looks up, the file being renamed into place afterwards.

  How many matches a query has is kept in memory instead (see `count!`),
  being one number rather than a result and the same however they are
  ordered. Nothing here is ever run twice at once: a caller that wants what
  another is already fetching waits for it (see `share!`)."
  (:require [clojure.core.cache :as c]
            [clojure.core.cache.wrapped :as cw]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [dk.cst.corpus-probe.corpus :as corpus]
            [dk.cst.corpus-probe.query :as query]
            [taoensso.telemere :as t])
  (:import [java.io File]
           [java.nio.file CopyOption Files StandardCopyOption]
           [java.security MessageDigest]))

(def default-ttl-ms
  "How long a saved query result nobody reads is kept, when `ctx` sets no
  :cache-ttl-ms: twenty minutes, the interval Korp uses."
  1200000)

(defn ttl-ms
  "How long `ctx` keeps a saved query result nobody reads."
  [ctx]
  (:cache-ttl-ms ctx default-ttl-ms))

(def default-max-bytes
  "How much disk the saved query results may take up together, when `ctx`
  sets no :cache-max-bytes. The arithmetic for sizing it is in
  resources/config.edn."
  2147483648)

(defn max-bytes
  "How much disk `ctx` lets its saved query results take up together."
  [ctx]
  (:cache-max-bytes ctx default-max-bytes))

(defn directory
  "The directory `ctx` keeps saved query results in (its :cache-dir), or
  nil when it keeps none and every request re-runs its query."
  ^File [{:keys [cache-dir]}]
  (when-not (str/blank? (str cache-dir))
    (io/file cache-dir)))

(defn digest
  "The MD5 digest of `s` as lowercase hex.

  A filename, never a secret: all that is asked of it is that two results
  holding different matches cannot land on one name."
  [s]
  (->> (.getBytes (str s) "UTF-8")
       (.digest (MessageDigest/getInstance "MD5"))
       (map #(format "%02x" (bit-and % 0xff)))
       (apply str)))

(defn data-file
  "The file holding the token stream of `corpus` under `ctx`: the data of
  its first positional attribute, which cwb-encode rewrites every time the
  corpus is encoded. nil when the registry entry does not say where the
  data are."
  ^File [ctx corpus]
  (let [entry (corpus/registry-file ctx corpus)]
    (when (corpus/registry-file? entry)
      (let [{:keys [home p-attrs]} (corpus/read-registry entry)]
        (when home
          (io/file home (str (name (or (first p-attrs) :word)) ".corpus")))))))

(defn build-stamp
  "What `corpus` reads as under `ctx`: the modification time and length of
  its registry entry, and the same of its token stream (see `data-file`)
  when that is there to read.

  Part of every result name, and it has to cover both. cwb-encode rewrites
  the entry only when passed -R, so rebuilding a corpus in place leaves the
  entry byte-identical while every word changes underneath it. The entry
  counts too, declaring the charset everything is read in, so correcting a
  mis-declared one changes which matches exist without touching the data."
  [ctx corpus]
  (let [^File entry (corpus/registry-file ctx corpus)
        ^File data  (data-file ctx corpus)]
    [(.lastModified entry) (.length entry)
     (when (and data (.isFile data)) [(.lastModified data) (.length data)])]))

(defn match-key
  "What decides which matches `query` has in `corpus` under `ctx`, and so
  how many of them: the registry, the corpus and its `build-stamp`, the
  query itself, and the metadata filter, the narrowings and the sample
  of `opts`.

  The registry is there because two of them can define one corpus name,
  and the filter's values are sorted because it holds them in sets, whose
  printed order is no part of their value. The narrowings (see
  dk.cst.corpus-probe.query/narrowing) and the sample belong here rather
  than in `result-key` because they decide which matches there are and
  how many, not what order they come in; the seed the sample is drawn
  with does not, being the same one every time (see
  dk.cst.corpus-probe.query/sample-seed). Nothing about ordering or
  display belongs here."
  [ctx corpus query {filter-by :filter sample :sample near :near
                     subset :subset}]
  [(:registry ctx)
   corpus
   (build-stamp ctx corpus)
   query
   (mapv (fn [[attr values patterns]] [attr (vec (sort values)) patterns])
         filter-by)
   subset
   near
   sample])

(defn result-key
  "What decides which matches the saved result of `query` in `corpus` under
  `opts` holds, and in what order: its `match-key`, the sorting and the
  collation.

  The sorting is the CQP command rather than the mode that names it, so
  that the modes which all mean corpus order share one result, and the
  collation is there because a result is paged in the order it was saved
  in, which is the order :sort-locale gave it.

  What the display does with the matches is deliberately absent: the
  context width, the attributes shown and the rows asked for are all
  applied when a result is read, not when it is saved."
  [ctx corpus query {sort-mode :sort :as opts}]
  (conj (match-key ctx corpus query opts)
        (query/sort-command sort-mode)
        (:sort-locale ctx)))

(defn result-name
  "The name of the saved query result of `query` in `corpus` under `opts`
  via `ctx`: `q_` followed by the digest of its `result-key`.

  The `q_` prefix is what keeps the name inside CQP's rule for one (see
  dk.cst.corpus-probe.query/valid-result-name)."
  [ctx corpus query opts]
  (str "q_" (digest (pr-str (result-key ctx corpus query opts)))))

(defonce ^{:doc "The calls in flight right now, one promise per key, which
  every caller waiting on that key parks on (see `share!`)."}
  in-flight
  (atom {}))

(defn share!
  "Call no-arg `f` for key `k`, sharing the one call with every caller
  asking for the same `k` while it runs.

  A cache miss on a large corpus costs minutes, so a reader who reloads
  while waiting would otherwise start a second one beside the first.

  A failure is shared like a value: every waiter gets the exception the
  first caller got. That is deliberate, the alternative being for each to
  run the same failing query itself and fail the same way minutes later.

  Waiters park on a promise rather than on a delay's monitor, because a
  virtual thread blocked on a monitor holds its carrier before JDK 24 and
  the server answers requests on virtual threads. The promise is delivered
  from a `finally` as well, so a caller that dies frees its waiters."
  [k f]
  (let [mine  (promise)
        claim (fn [m] (if (contains? m k) m (assoc m k mine)))
        held  (get (swap! in-flight claim) k)]
    (if (identical? held mine)
      (try
        (let [value (f)]
          (deliver mine {:value value})
          value)
        (catch Throwable t
          (deliver mine {:error t})
          (throw t))
        (finally
          ;; a no-op unless the call left by some path neither of the
          ;; above covers, in which case it is what frees the waiters
          (deliver mine {:error (ex-info "Shared call did not finish"
                                         {:error {:type :internal}})})
          (swap! in-flight
                 (fn [m] (cond-> m (identical? mine (get m k)) (dissoc k))))))
      (let [{:keys [value error]} @held]
        (if error (throw error) value)))))

(def max-counts
  "How many match counts are kept in memory at once: a generous bound, so
  that a long-running server cannot accumulate one entry per query ever
  asked."
  10000)

(defonce ^{:doc "The match counts remembered so far, as a bounded
  least-recently-used cache (see `count!`)."}
  counts
  (atom (c/lru-cache-factory {} :threshold max-counts)))

(defn forget-counts!
  "Discard every remembered match count."
  []
  (reset! counts (c/lru-cache-factory {} :threshold max-counts)))

(defn count!
  "How many matches `query` has in `corpus` under `ctx` and the filter of
  `opts`: remembered from an earlier count when there is one, else counted
  by calling no-arg `f` and remembered.

  Kept without a time limit, since a count cannot go stale while the build
  stamp in its key holds (see `match-key`), and bounded by entry count
  instead. Kept whatever :cache-dir says, too, being memory rather than
  disk; `forget-counts!` is what empties it."
  [ctx corpus query opts f]
  (let [k (match-key ctx corpus query opts)]
    ;; lookup-or-miss guarantees one call per caller, not one per key, so
    ;; without `share!` eight readers asking at once count eight times
    (cw/lookup-or-miss counts k (fn [_] (share! [::count k] f)))))

(defn pending-name
  "A name to save the result `nqr` under until `commit!` gives it that
  name, so that a file CQP is still writing is never the one a reader
  looks up."
  [nqr]
  (str nqr "_" (str/replace (str (random-uuid)) "-" "")))

(defn corpus-directory
  "The directory `ctx` keeps the saved query results of `corpus` in, or
  nil when it keeps no cache: one directory per corpus (see the namespace
  docstring). The corpus name is validated, since it becomes a path."
  ^File [ctx corpus]
  (when-let [dir (directory ctx)]
    (io/file dir (query/valid-corpus-name corpus))))

(defn corpus-directory!
  "`corpus-directory`, created if it is not there yet: CQP given a data
  directory that does not exist saves nothing and reports nothing."
  ^File [ctx corpus]
  (when-let [^File dir (corpus-directory ctx corpus)]
    (.mkdirs dir)
    dir))

(defn result-file
  "The file CQP saves the query result named `nqr` of `corpus` to under
  `ctx`, or nil when `ctx` keeps no cache.

  CQP names it after the corpus and the result, separated by a colon,
  inside the corpus's own directory."
  ^File [ctx corpus nqr]
  (when-let [dir (corpus-directory ctx corpus)]
    (io/file dir (str corpus ":" nqr))))

(defn result-file?
  "True when `f` is named like a saved query result this cache wrote: a
  corpus name, a colon and a `q_` result name.

  Reaping deletes files and the configured directory is not guaranteed to
  hold nothing else, so only the cache's own files are ever candidates."
  [^File f]
  (boolean (re-matches #"[A-Z][A-Z0-9_-]*:q_[A-Za-z0-9_-]+" (.getName f))))

(defn stored?
  "True when `ctx` holds a saved query result named `nqr` for `corpus`."
  [ctx corpus nqr]
  (boolean (some-> ^File (result-file ctx corpus nqr) (.isFile))))

(defn touch!
  "Record that the saved query result named `nqr` of `corpus` was read
  just now, so that reaping expires the results nobody is paging through
  rather than the ones that are merely old."
  [ctx corpus nqr]
  (some-> ^File (result-file ctx corpus nqr)
          (.setLastModified (System/currentTimeMillis))))

(defn discard!
  "Delete the saved query result named `nqr` of `corpus` under `ctx`."
  [ctx corpus nqr]
  (some-> ^File (result-file ctx corpus nqr) (.delete)))

(defn commit!
  "Give the saved query result `pending` of `corpus` under `ctx` its final
  name `nqr`, replacing whatever was stored under it.

  The rename is atomic, so a reader sees either the previous result or this
  one and never the file CQP was writing. Failing to store a result is no
  reason to fail the request that produced it, so a rename that cannot be
  made is logged and left at that.

  A file too small to hold `matches` is thrown away rather than named. CQP
  does not report a save it could write only part of, so a full disk
  otherwise leaves a short file that reads back without complaint and
  serves zero-filled rows past the cut. Every save file carries its matches
  as two 32-bit positions each, so anything under eight bytes a match was
  truncated."
  [ctx corpus pending nqr matches]
  (let [^File from (result-file ctx corpus pending)
        ^File to   (result-file ctx corpus nqr)]
    (when (and from (.isFile from))
      (if (< (.length from) (* 8 (or matches 0)))
        (do (t/event! ::truncated-save
                      {:level :error
                       :data  {:corpus corpus :matches matches
                               :bytes  (.length from)}})
            (.delete from))
        (t/catch->error! {:id ::commit-failed :catch-val nil}
          (Files/move (.toPath from) (.toPath to)
                      (into-array CopyOption
                                  [StandardCopyOption/ATOMIC_MOVE])))))))

(defn holds?
  "True when the saved query result named `nqr` of `corpus` under `ctx`
  is large enough to hold `matches` matches: the rule `commit!` applies
  when a result is saved, applied again when one is read whole. A file
  that has shrunk since it was written reads back zero-filled past the
  cut without CQP saying so, and a count over it would count position
  nought that many times."
  [ctx corpus nqr matches]
  (boolean (some-> ^File (result-file ctx corpus nqr)
                   (.length)
                   (>= (* 8 (or matches 0))))))

(defn stale?
  "True when `f` was last read more than `ttl-ms` before `now`."
  [ttl-ms now ^File f]
  (> (- now (.lastModified f)) ttl-ms))

(defn result-files
  "Every saved query result `ctx` holds, over all of its corpora."
  [ctx]
  (when-let [^File dir (directory ctx)]
    (for [^File corpus-dir (or (.listFiles dir) [])
          ^File f          (or (.listFiles corpus-dir) [])
          :when (result-file? f)]
      f)))

(defn excess-files
  "The `files` to delete to bring the rest within `budget` bytes, least
  recently read first; none when they already fit.

  Least recently read rather than oldest, because the result worth keeping
  is the one somebody is still paging through, however long ago they
  searched: reading one touches it (see `touch!`).

  A result larger than the whole budget is deleted on its own account and
  taken out of the reckoning first. Left in, it would never fit however
  much was deleted around it, so everything else would be evicted to make
  room for one that cannot be kept anyway."
  [budget files]
  (let [recent   (sort-by (fn [^File f] (.lastModified f)) > files)
        too-big? (fn [^File f] (> (.length f) budget))
        oversize (filterv too-big? recent)
        rest*    (remove too-big? recent)
        totals   (reductions + (map (fn [^File f] (.length f)) rest*))]
    (into oversize
          (->> (map vector rest* totals)
               (drop-while (fn [[_ total]] (<= total budget)))
               (mapv first)))))

(defn reap!
  "Delete the saved query results under `ctx` that no longer belong there,
  and return how many were deleted.

  First those nobody has read for its `ttl-ms`, then as many of the rest as
  it takes to fit the whole within its `max-bytes`. Age alone does not bound
  the disk: during a busy `ttl-ms` nothing is old enough to delete, which is
  exactly when the disk is filling. A result deleted while a request is
  about to read it costs that request a re-run rather than a failure, so
  nothing has to be locked."
  [ctx]
  (let [stale? (partial stale? (ttl-ms ctx) (System/currentTimeMillis))
        by-age (group-by stale? (result-files ctx))
        gone   (into (vec (by-age true))
                     (excess-files (max-bytes ctx) (by-age false)))]
    (reduce (fn [n ^File f] (if (.delete f) (inc n) n)) 0 gone)))

(def reap-interval-ms
  "How often at most the cache is reaped.

  Reading every corpus directory is not free (about 50 milliseconds for
  eight thousand results), so it does not run on every save; but it has to
  run far more often than `ttl-ms`, or a burst of searches writes past
  `max-bytes` faster than it can reclaim."
  1000)

(defonce ^{:doc "When the cache was last reaped, as a millisecond
  timestamp.

  Reaping is throttled through this rather than run on every save (see
  `reap-interval-ms`)."}
  last-reap
  (atom 0))

(defn reap-due!
  "Reap the cache of `ctx` (see `reap!`) when `reap-interval-ms` has
  passed since the last reaping, returning how many results were deleted;
  nil when it was not due.

  Called once a result has been saved, which already cost a whole query,
  so the cache needs no thread of its own and a server nobody searches
  reaps nothing. Only the caller that moves the timestamp reaps, so
  several saves at once still reap once between them."
  [ctx]
  (let [now      (System/currentTimeMillis)
        due      (- now reap-interval-ms)
        [before] (swap-vals! last-reap (fn [t] (if (< t due) now t)))]
    (when (< before due)
      ;; upkeep runs beside a request that has already succeeded, so a
      ;; failure here is logged rather than thrown: raised, it would be
      ;; read as the query failing and cost a second run of it
      (t/catch->error! {:id ::reap-failed :catch-val nil}
        (reap! ctx)))))

(comment
  (require '[babashka.fs :as fs]
           '[dk.cst.corpus-probe.search :as search])

  (def ctx {:registry  (str (System/getProperty "user.dir")
                            "/dev/corpus/registry")
            :cache-dir (str (System/getProperty "user.dir") "/dev/cache")})

  ;; the name moves with the corpus build stamp, so it is not written down
  (result-name ctx "VISER" "[pos=\"N.*\"]" {:sort "word"})

  (stored? ctx "VISER" (result-name ctx "VISER" "[]" {}))
  ;; => false

  (count! ctx "VISER" "[]" {} #(do (Thread/sleep 1000) 48))
  ;; => 48   (a second the first time, instant after)

  (forget-counts!)

  (share! ::probe (fn [] :once))
  ;; => :once

  (excess-files (max-bytes ctx) (result-files ctx))
  ;; => []

  (reap! ctx)
  ;; => 0

  ;; What the cache is for, measured against the two-million-token STOR
  ;; corpus that dev/encode-big.sh builds (the dev corpora are 42 to 48
  ;; tokens, where every query is instant and this proves nothing). Each
  ;; number is one page of 25 hits out of two million matches.
  (def big {:registry         (str (System/getProperty "user.dir")
                                   "/dev/corpus/registry-big")
            :sort-locale      "da_DK.UTF-8"
            ;; as the app runs a batch that sorts (see
            ;; dk.cst.corpus-probe.search/running-ctx)
            :query-timeout-ms 900000})

  (defn page-ms
    [ctx sort page]
    (let [started (System/nanoTime)]
      (search/kwic! ctx "STOR" "[]" {:sort sort
                                     :rows [(* page 25) (+ 24 (* page 25))]})
      (quot (- (System/nanoTime) started) 1000000)))

  (page-ms big "left" 100)
  (page-ms (assoc big :cache-dir (str (fs/create-temp-dir))) "left" 100)

  ;;   sort mode | uncached page | cached page | save file
  ;;   corpus    |         77 ms |       16 ms |     16 MB
  ;;   word      |       3144 ms |       20 ms |     24 MB
  ;;   left      |      18600 ms |       26 ms |     24 MB
  ;;
  ;; And at the size of the largest corpus at KU, built with
  ;; TOKENS=64600000 dev/encode-big.sh. These are the figures
  ;; :query-timeout-ms is set against: neither locale-aware sort can
  ;; finish a whole corpus of this size inside five minutes, which is
  ;; deliberate, and a realistic query sorts a few percent of it.
  ;;
  ;;   sort mode | uncached page | cached page | save file
  ;;   count     |       2400 ms |             |
  ;;   corpus    |       2300 ms |      200 ms |    517 MB
  ;;   word      |     649600 ms |             |    775 MB
  ;;   left      |    >800000 ms |             |
  #_.)
