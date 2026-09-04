(ns dk.cst.corpus-probe.search
  "High-level search operations: one function call in, plain data out.

  Composes query generation (dk.cst.corpus-probe.query), the child-process
  driver (dk.cst.corpus-probe.cqp) and the output parsers
  (dk.cst.corpus-probe.parse) into complete round trips: a KWIC page or a
  match count for one corpus, and a concordance over several. Breakdowns
  of those matches are dk.cst.corpus-probe.frequency, composed on the
  corpus context, the bounded fan-out and the collator this owns.

  Every search takes a :filter option, a metadata filter (see
  dk.cst.corpus-probe.query/filter-query) restricting it to the matching
  regions of each corpus.

  These functions are the trust boundary for the web layer: only the CQP
  query itself is protected by the QueryLock sandbox, so every other
  parameter spliced into a command (corpus names and attribute names) is
  validated here against the corpus's own inventory first."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.cache :as cache]
            [dk.cst.corpus-probe.corpus :as corpus]
            [dk.cst.corpus-probe.cqp :as cqp]
            [dk.cst.corpus-probe.parse :as parse]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.tools :as tools]
            [taoensso.telemere :as t])
  (:import [java.text Collator]
           [java.util Comparator Locale]))

(defn corpus-ctx
  "Return `ctx` configured for `corpus`: validates the corpus name (it is
  spliced into commands outside the QueryLock sandbox) and sets the
  corpus's own charset for the CQP round trip."
  [ctx corpus]
  (query/valid-corpus-name corpus)
  (assoc ctx :charset (corpus/charset ctx corpus)))

(defn error-map
  "The error map for exception `e` thrown by a search: the CQP error it
  carries; a :rejected error with the message of one of this project's own
  guards (an ex-info without a CQP error); or an :internal error for
  anything else, whose details are logged rather than shown, since an
  exception message may name a server path."
  [e]
  (or (:error (ex-data e))
      (if (instance? clojure.lang.ExceptionInfo e)
        {:type :rejected :message (ex-message e)}
        (do (t/error! ::search-failed e)
            {:type :internal}))))

(defn pmap-n
  "Map `f` over `coll` with at most `n` calls running at once, in order.

  `pmap` alone starts a whole chunk of 32 futures at once, and each call
  here spawns a process, so the fan-out is bounded instead."
  [n f coll]
  (mapcat #(doall (pmap f %)) (partition-all n coll)))

(defn parallelism
  "How many corpora `ctx` queries at once (its :parallelism, default 8)."
  [ctx]
  (:parallelism ctx 8))

(defn deadline
  "The wall-clock deadline (a millisecond timestamp) of a search started
  now under `ctx`: its :search-budget-ms (default 60000) from now, after
  which no further corpus is queried, so a query that times out in every
  corpus cannot hold a request for the sum of all the timeouts."
  [{:keys [search-budget-ms] :or {search-budget-ms 60000} :as ctx}]
  (+ (System/currentTimeMillis) search-budget-ms))

(defn overdue?
  "True once `deadline` (see `deadline`) has passed."
  [deadline]
  (> (System/currentTimeMillis) deadline))

(defn attr-names
  "The names of the `attributes` matching `pred`."
  [pred attributes]
  (->> (filter pred attributes)
       (mapv :name)))

(defn annotated-s-attr?
  "True when attribute description `m` is an s-attribute carrying values."
  [{:keys [type values?] :as m}]
  (and (= :structural type) values?))

(defn corpus-filter!
  "Metadata `filter` (a map of attribute to the set of values accepted)
  as dk.cst.corpus-probe.query/filter-query takes it for `corpus` via
  `ctx`: [attr values] pairs, the attribute with the most regions first,
  that being the innermost one the filter query must anchor on.

  Every attribute must be an annotated s-attribute of the corpus, by the
  cached describe statistics that also count its regions; anything else
  is rejected, since the names are spliced into a command, sandboxed
  though the filter query is."
  [ctx corpus filter]
  (when (seq filter)
    (let [regions (into {}
                        (keep (fn [{:keys [name regions values?]}]
                                (when values? [name regions])))
                        (:s-attrs (tools/describe-corpus! ctx corpus)))]
      (when-let [bad (seq (remove regions (keys filter)))]
        (throw (ex-info "Not an annotated structural attribute of this corpus"
                        {:corpus corpus :attrs bad})))
      (vec (sort-by (juxt (comp - regions key) key) filter)))))

(defn cache-opts
  "`opts` with the cache directory and the name the result of `query` in
  `corpus` is saved under, when `ctx` keeps a cache and `opts` does not
  turn it off with a false :cache?.

  Creates the corpus's cache directory, CQP given one that does not exist
  saving nothing and reporting nothing. The name is taken from the filter
  the corpus was given rather than the one the request asked for, so that
  two spellings of one filter share a saved result."
  [ctx corpus query opts]
  (if-let [dir (and (:cache? opts true)
                    (cache/corpus-directory! ctx corpus))]
    (assoc opts
           :cache-dir (str dir)
           :nqr       (cache/result-name ctx corpus query opts))
    opts))

(defn kwic-opts!
  "The options one KWIC batch for `corpus` needs, from the `opts` of
  `kwic!` via `ctx`.

  The KWIC defaults, the corpus's positional attributes and the structural
  attributes to fetch per hit, its metadata filter as
  dk.cst.corpus-probe.query/filter-query takes it, and whatever the cache
  adds (see `cache-opts`). Requested structural attributes are checked
  against the corpus's inventory first, since their names are spliced into
  a command."
  [ctx corpus query opts]
  (let [attributes (corpus/attributes! ctx corpus)
        annotated  (attr-names annotated-s-attr? attributes)
        requested  (:struct-attrs opts)]
    (when-let [bad (seq (remove (set annotated) requested))]
      (throw (ex-info "Unknown struct attributes"
                      {:corpus corpus :struct-attrs bad})))
    (cache-opts ctx corpus query
                (merge query/kwic-defaults
                       opts
                       {:p-attrs      (attr-names #(= :positional (:type %))
                                                  attributes)
                        :struct-attrs (or requested annotated)
                        :filter       (corpus-filter! ctx corpus
                                                      (:filter opts))}))))

(defn run-kwic-batch!
  "Run KWIC `batch` for `query` in `corpus` via `ctx` and return its
  sections (see dk.cst.corpus-probe.query/batch-sections).

  Throws ex-info when CQP reports an error, times out or dies."
  [ctx corpus query batch]
  (let [{:keys [results error]} (cqp/run-batch! ctx (mapv second batch))]
    (when error
      (throw (ex-info "KWIC query failed"
                      {:corpus corpus :query query :error error})))
    (query/batch-sections batch results)))

(defn batch-matches
  "How many matches the `sections` of a KWIC batch report."
  [{[size-lines] :size}]
  (parse-long (first size-lines)))

(defn intact?
  "True when the `sections` of a KWIC batch describe a page that could
  have come from a real query result.

  A truncated save file is not reported as an error when it is read: the
  pages past the cut are zero-filled, so every row in them comes back at
  corpus position 0. No two matches of one query share a position, in any
  sort order, so a page whose positions repeat did not come from the
  result it claims to."
  [{[dump-lines] :dump}]
  (let [positions (map :match (parse/dump->anchors dump-lines))]
    (or (empty? positions) (apply distinct? positions))))

(defn timeout?
  "True when exception `e` reports that CQP was killed for taking too long
  rather than answering."
  [e]
  (= :timeout (get-in (ex-data e) [:error :type])))

(defn stored-sections!
  "The sections of one KWIC page read from the saved query result `:nqr`
  of `opts`, or nil when none is stored or the stored one does not read.

  A stored result CQP cannot read is discarded, leaving the caller to run
  the query: one left from an earlier build of the corpus kills CQP
  outright, one reaped between the check and the read leaves the batch
  reporting an undefined corpus, and a truncated one is caught by
  `intact?`. A timeout is not treated that way and the result is kept,
  since it says the machine is busy rather than that the file is bad."
  [ctx corpus query {:keys [nqr] :as opts}]
  (when (and nqr (cache/stored? ctx corpus nqr))
    (try
      (let [batch    (query/stored-kwic-batch corpus nqr opts)
            sections (run-kwic-batch! ctx corpus query batch)]
        (when-not (intact? sections)
          (throw (ex-info "Stored result read back damaged"
                          {:corpus corpus :error {:type :damaged}})))
        (cache/touch! ctx corpus nqr)
        ;; reads reclaim as well as saves, or a server that has stopped
        ;; saving sits at its high-water mark until it saves again
        (cache/reap-due! ctx)
        sections)
      (catch Exception e
        (when (timeout? e)
          (throw e))
        (t/event! ::stored-result-discarded
                  {:level :warn
                   :data  {:corpus corpus :error (ex-message e)}})
        (cache/discard! ctx corpus nqr)
        nil))))

(defn running-ctx
  "`ctx` with the longer timeout a batch that runs the query needs (its
  :query-timeout-ms), leaving batches that only read a result already
  saved on the ordinary :timeout-ms.

  The two are orders of magnitude apart, the figures being in
  resources/config.edn beside the settings. Counting and displaying run
  the same query, so they get the same budget: giving counting less would
  let a corpus time out while being counted and succeed while being shown,
  and a count that fails is left out of the total, quietly costing the
  search pages of hits it has."
  [ctx]
  (cond-> ctx
    (:query-timeout-ms ctx) (assoc :timeout-ms (:query-timeout-ms ctx))))

(defn within-deadline
  "`ctx` with its timeouts cut down to the time left before `deadline`.

  Without this, :search-budget-ms bounds only how many corpora a search
  starts, not how long the last one may run: one started just before the
  deadline would get a whole timeout of its own on top of the budget, and
  the retry in `fresh-sections!` another."
  [ctx deadline]
  (let [left (max 1000 (- deadline (System/currentTimeMillis)))]
    (cond-> ctx
      (:timeout-ms ctx)       (update :timeout-ms min left)
      (:query-timeout-ms ctx) (update :query-timeout-ms min left))))

(defn fresh-sections!
  "The sections of one KWIC page of `query` in `corpus` via `ctx`, run
  afresh and saved under the `:nqr` of `opts` when there is one.

  The batch that saves is the batch that does not plus the save, so a
  failure could be either, and CQP reports a directory it cannot write to
  like any other error. When the cache is on, the batch is therefore run
  once more without it before the failure is reported: a cache that has
  stopped working is no reason to stop answering. A timeout is not
  retried, having spent its whole budget already. Both runs get the query
  timeout (see `running-ctx`)."
  [ctx corpus query {:keys [nqr] :as opts}]
  (let [pending (when nqr (cache/pending-name nqr))
        run     #(run-kwic-batch! (running-ctx ctx) corpus query
                                  (query/kwic-batch corpus query %))]
    (try
      (let [sections (run (assoc opts :nqr pending))]
        (when nqr
          (cache/commit! ctx corpus pending nqr (batch-matches sections))
          ;; after the save rather than before, so the result just written
          ;; is what the disk budget is measured against
          (cache/reap-due! ctx))
        sections)
      (catch Exception e
        (when (or (nil? nqr) (timeout? e))
          (throw e))
        (let [sections (run (dissoc opts :nqr :cache-dir))]
          ;; logged only once the retry has answered, which is what says
          ;; the cache was at fault rather than the query
          (t/event! ::cache-bypassed
                    {:level :warn
                     :data  {:corpus corpus :error (ex-message e)}})
          sections)))))

(defn kwic-sections!
  "The sections of one KWIC page of `query` in `corpus` via `ctx` under
  `opts` (see `kwic-opts!`): read from the saved query result when one is
  stored (see `stored-sections!`), and run afresh otherwise (see
  `fresh-sections!`).

  Requests that want the same page of the same search share one run of it
  while it is in flight (see dk.cst.corpus-probe.cache/share!), so that a
  reader who reloads during a long sort waits for the sort already running
  rather than starting a second one. They want the same page when they
  would run the same batch, so the batch is the key: anything a future
  option changes about the output changes it too, without anyone having to
  remember to add it."
  [ctx corpus query {:keys [nqr] :as opts}]
  (let [fetch #(or (stored-sections! ctx corpus query opts)
                   (fresh-sections! ctx corpus query opts))]
    (if nqr
      (cache/share! (query/stored-kwic-batch corpus nqr opts) fetch)
      (fetch))))

(defn kwic!
  "Run CQP `query` against `corpus` (an uppercase CQP corpus name) through
  the installation described by `ctx` (see dk.cst.corpus-probe.cqp) and
  return the hits in one row range as data.

  Returns {:corpus ... :query ... :size <total hits> :rows [from to]
  :hits [hit ...]} where each hit combines the parsed KWIC line (:cpos :left
  :match :right), its anchors from `dump` (:anchors) and its structural
  metadata (:structs). `opts` accepts :rows (the [from to] row range, see
  dk.cst.corpus-probe.query/page-rows; default the first page), :context
  (tokens), :sort (a sort mode), :filter (a metadata filter) and
  :struct-attrs (defaults to every annotated s-attribute of the corpus;
  anything not in that inventory is rejected).

  When `ctx` keeps a cache (see dk.cst.corpus-probe.cache), the result is
  saved there and a later page of the same query, filter and sort mode is
  read back from it rather than queried and sorted again; a false :cache?
  in `opts` leaves it out of the cache, for a query nothing will ask for
  twice.

  Throws ex-info when CQP reports an error, times out or dies."
  ([ctx corpus query]
   (kwic! ctx corpus query {}))
  ([ctx corpus query opts]
   (let [ctx      (corpus-ctx ctx corpus)
         opts     (kwic-opts! ctx corpus query opts)
         {:keys [p-attrs struct-attrs rows]} opts
         sections (kwic-sections! ctx corpus query opts)
         {[cat-lines]  :cat
          [dump-lines] :dump
          tab-sections :tabulate} sections
         hits     (parse/kwic->hits p-attrs cat-lines)
         anchors  (parse/dump->anchors dump-lines)
         structs  (when (seq struct-attrs)
                    ;; one tabulate section per attribute: a whole line is
                    ;; one annotation value, so embedded TABs survive
                    (apply mapv
                           (fn [& values]
                             (zipmap struct-attrs values))
                           tab-sections))]
     (when (not= (count hits) (count anchors))
       ;; cat and dump disagree only when CQP printed something other
       ;; than the requested rows, so the page cannot be trusted
       (throw (ex-info "KWIC output misaligned"
                       {:corpus corpus
                        :error  {:type     :misaligned
                                 :expected (count hits)
                                 :received (count anchors)}})))
     {:corpus corpus
      :query  query
      :size   (batch-matches sections)
      :rows   rows
      :hits   (mapv (fn [hit anchor struct]
                      (cond-> (assoc hit :anchors anchor)
                        struct (assoc :structs struct)))
                    hits anchors (or structs (repeat nil)))})))

(defn run-size!
  "Count the matches of CQP `query` in `corpus` via `ctx`, within
  `filter-by` when there is one, by running the query.

  Throws ex-info when CQP reports an error, times out or dies."
  [ctx corpus query filter-by]
  (let [{:keys [results error]}
        (cqp/run-batch! ctx [(str corpus ";")
                             (query/restricted-query query filter-by)
                             "size Last;"])]
    (when error
      (throw (ex-info "Size query failed"
                      {:corpus corpus :query query :error error})))
    (parse-long (first (last results)))))

(defn size!
  "The number of matches of CQP `query` in `corpus` via `ctx`, within the
  :filter of `opts` when there is one.

  Counted once and then remembered (see
  dk.cst.corpus-probe.cache/count!), because paging a search over several
  corpora counts the ones contributing no rows to the page again on every
  page, and that is a whole query each time. Throws ex-info when CQP
  reports an error, times out or dies."
  ([ctx corpus query]
   (size! ctx corpus query {}))
  ([ctx corpus query {:keys [filter]}]
   (let [ctx       (corpus-ctx ctx corpus)
         filter-by (corpus-filter! ctx corpus filter)]
     (cache/count! ctx corpus query {:filter filter-by}
                   #(run-size! (running-ctx ctx) corpus query filter-by)))))

(defn corpus-size!
  "The size of `query`'s result in `corpus` via `ctx` (`opts` as for
  `size!`) without failing: {:corpus ... :size <n>}, or {:corpus ...
  :error <error map>} when the query cannot run there."
  [ctx corpus query opts]
  (try {:corpus corpus :size (size! ctx corpus query opts)}
       (catch Exception e
         {:corpus corpus :error (error-map e)})))

(defn corpus-sizes!
  "The sizes of `query`'s result in each of `corpora` via `ctx` (`opts`
  as for `size!`), queried in parallel (see `parallelism`) until `deadline`
  passes, after which the rest are reported as timed out. Returns one
  `corpus-size!` map per corpus in the given order."
  [ctx corpora query deadline opts]
  (vec (pmap-n (parallelism ctx)
               (fn [corpus]
                 (if (overdue? deadline)
                   {:corpus corpus :error {:type :timeout}}
                   (corpus-size! (within-deadline ctx deadline)
                                 corpus query opts)))
               corpora)))

(defn fill-page!
  "Query `corpora` one at a time via `ctx` until the `rows` [from to] of
  the combined result are filled or `deadline` passes.

  Each corpus contributes the rows of its own result that fall in the
  range, offset by the sizes of the corpora before it, and its count map
  (as from `corpus-size!`); a corpus that fails contributes no rows.
  `opts` are the display options of `kwic!`. Returns {:counts [...] :hits
  [...] :remaining [corpus ...]} where :remaining are the corpora not
  queried."
  [ctx corpora query [from to] deadline opts]
  (loop [[corpus & more :as remaining] corpora
         offset 0
         counts []
         hits   []]
    (if (or (nil? corpus) (> offset to) (overdue? deadline))
      {:counts counts :hits hits :remaining remaining}
      (let [rows [(max 0 (- from offset)) (- to offset)]
            res  (try (kwic! (within-deadline ctx deadline) corpus query
                             (assoc opts :rows rows))
                      (catch Exception e {:error (error-map e)}))]
        (recur more
               (+ offset (:size res 0))
               (conj counts (assoc (select-keys res [:size :error])
                                   :corpus corpus))
               (into hits (map #(assoc % :corpus corpus)) (:hits res)))))))

(defn concordance!
  "Run CQP `query` against `corpora` (uppercase names, in display order) via
  `ctx` and return one page of the combined concordance: the hits ordered
  by corpus, then in each corpus's own sort order.

  Follows Korp: the corpora are queried one at a time until the requested
  page is filled, then the remaining corpora are only counted, in parallel,
  all within the `deadline` of `ctx`. A corpus whose query fails (an
  attribute it lacks, a timeout) contributes no hits and carries its
  :error instead of its :size, so one bad corpus does not fail the whole
  search.

  `opts` accepts :page and :page-size (see
  dk.cst.corpus-probe.query/page-defaults) plus the display options of
  `kwic!` (:context, :sort, :filter). Returns {:query ... :filter ...
  :page ... :page-size ... :counts [{:corpus ... :size ...} ...] :size
  <hits in all readable corpora> :hits [hit ...]}, each hit tagged with
  its :corpus."
  ([ctx corpora query]
   (concordance! ctx corpora query {}))
  ([ctx corpora query opts]
   (let [{:keys [page page-size filter] :as opts} (merge query/page-defaults
                                                         opts)
         kwic-opts (dissoc opts :page :page-size)
         deadline  (deadline ctx)
         {:keys [counts hits remaining]}
         (fill-page! ctx corpora query (query/page-rows page page-size)
                     deadline kwic-opts)
         counts    (into counts (corpus-sizes! ctx remaining query deadline
                                              kwic-opts))]
     {:query     query
      :filter    filter
      :page      page
      :page-size page-size
      :counts    counts
      :size      (reduce + (keep :size counts))
      :hits      hits})))

(defn locale
  "The java.util.Locale named by LC_ALL value `s` (\"da_DK.UTF-8\"), or the
  root locale when it names none.

  A POSIX locale name is its BCP 47 tag with an underscore for the hyphen
  and a charset or modifier suffix, so dropping the suffix and putting the
  hyphen back is the whole conversion."
  [s]
  (-> (str s)
      (str/replace #"[.@].*$" "")
      (str/replace "_" "-")
      (Locale/forLanguageTag)))

(defn ->collator
  "A collator over annotation values in the locale `ctx` sorts in (its
  :sort-locale, see `locale`).

  The values come out of the corpora, and this is the locale CQP itself
  collates them in when it sorts a concordance, so a value list and a
  concordance agree on where æ, ø and å belong. Collators are stateful,
  so each caller gets its own."
  [ctx]
  (Collator/getInstance (locale (:sort-locale ctx))))
