(ns dk.cst.corpus-probe.search.cache
  "Query results kept as CQP's own saved query results, so paging or
  re-sorting a search does not re-run it: `save` writes a named result to
  a file, and a later process reads it back in the order it was saved in.
  Invalidation is this application's job, CQP failing silently on a wrong
  file, so the name of a result carries a build stamp of the corpus, and
  CQP saves under a name nothing looks up until the file is renamed into
  place. Match counts are kept in memory instead."
  (:require [clojure.core.cache :as c]
            [clojure.core.cache.wrapped :as cw]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [dk.cst.corpus-probe.cqp :as cqp]
            [dk.cst.corpus-probe.cwb.command :as command]
            [dk.cst.corpus-probe.cwb.registry :as registry]
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
  "The MD5 digest of `s` as lowercase hex: a filename, never a secret."
  [s]
  (->> (.getBytes (str s) "UTF-8")
       (.digest (MessageDigest/getInstance "MD5"))
       (map #(format "%02x" (bit-and % 0xff)))
       (apply str)))

(defn match-key
  "What decides which matches `query` has in `corpus` under `ctx`, and so
  how many of them: the registry, the corpus and its build stamp, the
  query, and the metadata filter, the narrowings and the sample of
  `opts`. Nothing about ordering or display belongs here."
  [ctx corpus query {filter-by :filter sample :sample near :near
                     subset :subset}]
  [;; two registries can define one corpus name
   (:registry ctx)
   corpus
   (registry/build-stamp ctx corpus)
   query
   ;; the values sorted: the filter holds them in sets, whose printed
   ;; order is no part of their value
   (mapv (fn [[attr values patterns]] [attr (vec (sort values)) patterns])
         filter-by)
   subset
   near
   sample])

(defn result-key
  "What decides, via `ctx`, which matches the saved result of `query` in
  `corpus` under `opts` holds, and in what order: its `match-key`, the
  sort command and the collation. What the display does with the matches
  is absent: the context, the attributes shown and the rows asked for are
  applied when a result is read, not when it is saved."
  [ctx corpus query {sort-mode :sort :as opts}]
  (conj (match-key ctx corpus query opts)
        ;; the command rather than the mode naming it, so that the modes
        ;; which all mean corpus order share one result
        (command/sort-command sort-mode)
        ;; a result is paged in the order it was saved in, which is the
        ;; order the locale gave it
        (:sort-locale ctx)))

(defn result-name
  "The name of the saved query result of `query` in `corpus` under `opts`
  via `ctx`: `q_` followed by the digest of its `result-key`, the prefix
  keeping the name inside CQP's rule for one."
  [ctx corpus query opts]
  (str "q_" (digest (pr-str (result-key ctx corpus query opts)))))

(defonce ^{:doc "The calls in flight right now, one promise per key, which
  every caller waiting on that key parks on (see `share!`)."}
  in-flight
  (atom {}))

(defn share!
  "Call no-arg `f` for key `k`, sharing the one call with every caller
  asking for the same `k` while it runs: a cache miss on a large corpus
  costs minutes, so a reader who reloads would otherwise start a second
  one. A failure is shared like a value, every waiter getting the
  exception the first caller got."
  [k f]
  ;; a promise rather than a delay: a virtual thread blocked on a monitor
  ;; holds its carrier before JDK 24, and requests run on virtual threads
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
  by calling no-arg `f` and remembered. Kept without a time limit, since a
  count cannot go stale while the build stamp in its key holds, and
  whatever :cache-dir says, being memory rather than disk."
  [ctx corpus query opts f]
  (let [k (match-key ctx corpus query opts)]
    ;; lookup-or-miss guarantees one call per caller, not one per key, so
    ;; without `share!` eight readers asking at once count eight times
    (cw/lookup-or-miss counts k (fn [_] (share! [::count k] f)))))

(defn known-count
  "How many matches `query` has in `corpus` under `ctx` and the filter of
  `opts` when `count!` has remembered it, else nil. Runs nothing."
  [ctx corpus query opts]
  (cw/lookup counts (match-key ctx corpus query opts)))

(defn pending-name
  "A name to save the result `nqr` under until `commit!` gives it that
  name, so that a file CQP is still writing is never the one a reader
  looks up."
  [nqr]
  (str nqr "_" (str/replace (str (random-uuid)) "-" "")))

(defn corpus-directory
  "The directory `ctx` keeps the saved query results of `corpus` in, or
  nil when it keeps no cache; the corpus name is validated, since it
  becomes a path."
  ^File [ctx corpus]
  ;; one directory per corpus: CQP registers every file of the one it is
  ;; given on every startup
  (when-let [dir (directory ctx)]
    (io/file dir (command/valid-corpus-name corpus))))

(defn corpus-directory!
  "Create the `corpus-directory` if it is not there yet: CQP given a data
  directory that does not exist saves nothing and reports nothing."
  ^File [ctx corpus]
  (when-let [^File dir (corpus-directory ctx corpus)]
    (.mkdirs dir)
    dir))

(defn result-file
  "The file CQP saves the query result named `nqr` of `corpus` to under
  `ctx`, or nil when `ctx` keeps no cache: the corpus and the result,
  colon-separated, in the corpus's own directory."
  ^File [ctx corpus nqr]
  (when-let [dir (corpus-directory ctx corpus)]
    (io/file dir (str corpus ":" nqr))))

(defn result-file?
  "True when `f` is named like a saved query result this cache wrote: a
  corpus name, a colon and a `q_` result name. Reaping deletes files and
  the configured directory may hold other things, so only the cache's own
  files are ever candidates."
  [^File f]
  (let [[corpus nqr] (str/split (.getName f) #":" 2)]
    (boolean (and nqr
                  (cqp/corpus-name? corpus)
                  (str/starts-with? nqr "q_")
                  (cqp/name? nqr)))))

(defn stored?
  "True when `ctx` holds a saved query result named `nqr` for `corpus`."
  [ctx corpus nqr]
  (boolean (some-> ^File (result-file ctx corpus nqr) (.isFile))))

(defn touch!
  "Record via `ctx` that the saved query result named `nqr` of `corpus`
  was read just now, so that reaping expires the results nobody is paging
  through rather than the ones that are merely old."
  [ctx corpus nqr]
  (some-> ^File (result-file ctx corpus nqr)
          (.setLastModified (System/currentTimeMillis))))

(defn discard!
  "Delete the saved query result named `nqr` of `corpus` under `ctx`."
  [ctx corpus nqr]
  (some-> ^File (result-file ctx corpus nqr) (.delete)))

(defn commit!
  "Give the saved query result `pending` of `corpus` under `ctx` its final
  name `nqr`, replacing whatever was stored under it, atomically, so that
  a reader sees either the previous result or this one. A rename that
  cannot be made is logged, a failed save being no reason to fail the
  request. A file too small to hold `matches` is thrown away rather than
  named."
  [ctx corpus pending nqr matches]
  (let [^File from (result-file ctx corpus pending)
        ^File to   (result-file ctx corpus nqr)]
    (when (and from (.isFile from))
      ;; two 32-bit positions a match; CQP does not report a save it could
      ;; write only part of, and the short file reads back zero-filled
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
  when a result is saved, applied again when one is read whole, since a
  file that has shrunk reads back zero-filled without CQP saying so."
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
  recently read first, the result worth keeping being the one somebody is
  still paging through; none when they already fit."
  [budget files]
  (let [recent   (sort-by (fn [^File f] (.lastModified f)) > files)
        too-big? (fn [^File f] (> (.length f) budget))
        ;; an oversize result goes on its own account and out of the
        ;; reckoning, or everything else would be evicted to make room for
        ;; one that cannot fit anyway
        oversize (filterv too-big? recent)
        rest*    (remove too-big? recent)
        totals   (reductions + (map (fn [^File f] (.length f)) rest*))]
    (into oversize
          (->> (map vector rest* totals)
               (drop-while (fn [[_ total]] (<= total budget)))
               (mapv first)))))

(defn reap!
  "Delete the saved query results under `ctx` that no longer belong there,
  and return how many were deleted: first those nobody has read for its
  `ttl-ms`, then as many of the rest as it takes to fit within its
  `max-bytes`.

  Age alone does not bound the disk, a busy `ttl-ms` being when nothing
  is old enough to delete and the disk is filling; and nothing is locked,
  since a result deleted under a reader costs a re-run, not a failure."
  [ctx]
  (let [stale? (partial stale? (ttl-ms ctx) (System/currentTimeMillis))
        by-age (group-by stale? (result-files ctx))
        gone   (into (vec (by-age true))
                     (excess-files (max-bytes ctx) (by-age false)))]
    (reduce (fn [n ^File f] (if (.delete f) (inc n) n)) 0 gone)))

(def reap-interval-ms
  "How often at most the cache is reaped: reading every corpus directory
  is not free, so not on every save, but far more often than `ttl-ms`, or
  a burst of searches writes past `max-bytes` faster than it can reclaim."
  1000)

(defonce ^{:doc "When the cache was last reaped, as a millisecond
  timestamp; reaping is throttled through it (see `reap-interval-ms`)."}
  last-reap
  (atom 0))

(defn reap-due!
  "Reap the cache of `ctx` (see `reap!`) when `reap-interval-ms` has
  passed since the last reaping, returning how many results were deleted;
  nil when it was not due. Only the caller that moves the timestamp
  reaps, so several saves at once still reap once between them."
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
