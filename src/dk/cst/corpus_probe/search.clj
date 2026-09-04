(ns dk.cst.corpus-probe.search
  "High-level search operations: one function call in, plain data out.

  Composes query generation (dk.cst.corpus-probe.query), the child-process
  driver (dk.cst.corpus-probe.cqp) and the output parsers
  (dk.cst.corpus-probe.parse) into complete round trips: a KWIC page or a
  match count for one corpus, a concordance over several corpora, the
  frequency breakdown of a query (or, from the lexicon, of whole corpora)
  merged over several corpora into one table, and the metadata filters
  those corpora offer. Every search takes a :filter option, a metadata
  filter (see dk.cst.corpus-probe.query/filter-query) restricting it to
  the matching regions of each corpus.

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

  Creates the corpus's cache directory, CQP given a data directory that
  does not exist saving nothing and reporting nothing. The name is taken
  from the filter the corpus was given rather than the one the request
  asked for, so that two spellings of one filter share a saved result."
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
  against the corpus's own inventory first, since their names are spliced
  into a command."
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

  A saved result that was truncated is not reported as an error when it is
  read: the pages past the cut are zero-filled, so every row in them comes
  back at corpus position 0. No two matches of one query share a position,
  in any sort order, so a page whose positions repeat did not come from
  the result it claims to."
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
  the query: a save file left from an earlier build of the corpus kills
  CQP outright, one reaped between the check and the read leaves the
  batch reporting an undefined corpus, and one that was truncated reads
  back without complaint but is caught by `intact?`.

  A timeout is not treated that way and the result is kept, since it says
  the machine is busy rather than that the file is bad, and re-running the
  query would be the most expensive possible answer to that."
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

  The two are orders of magnitude apart: reading a page out of a saved
  result takes milliseconds, and running the query can take minutes on a
  large corpus (the figures are in resources/config.edn beside the two
  settings).

  Counting and displaying run the same query, so they get the same
  budget. Giving counting less would let a corpus time out while being
  counted and succeed while being shown, and a count that fails is left
  out of the total, which quietly costs the search pages of hits it
  actually has."
  [ctx]
  (cond-> ctx
    (:query-timeout-ms ctx) (assoc :timeout-ms (:query-timeout-ms ctx))))

(defn within-deadline
  "`ctx` with its timeouts cut down to the time left before `deadline`.

  Without this, :search-budget-ms bounds only how many corpora a search
  starts, not how long the last one may run: a corpus started just before
  the deadline gets a whole timeout of its own on top of the budget, and
  the retry in `fresh-sections!` another, so the real ceiling on a request
  is the budget plus two timeouts rather than the budget."
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
  stopped working is no reason to stop answering, and the page itself was
  never the problem. A timeout is not retried, having spent its whole
  budget already. Both runs get the query timeout (see `running-ctx`),
  the retry doing the same work as the batch it replaces."
  [ctx corpus query {:keys [nqr] :as opts}]
  (let [pending (when nqr (cache/pending-name nqr))]
    (try
      (let [batch    (query/kwic-batch corpus query (assoc opts :nqr pending))
            sections (run-kwic-batch! (running-ctx ctx) corpus query batch)]
        (when nqr
          (cache/commit! ctx corpus pending nqr (batch-matches sections))
          ;; after the save rather than before, so the result just written
          ;; is what the disk budget is measured against
          (cache/reap-due! ctx))
        sections)
      (catch Exception e
        (when (or (nil? nqr) (timeout? e))
          (throw e))
        (let [batch    (query/kwic-batch corpus query
                                         (dissoc opts :nqr :cache-dir))
              sections (run-kwic-batch! (running-ctx ctx) corpus query batch)]
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
  rather than starting a second one.

  They are the same page when they would run the same batch, so the batch
  is what they share by: it names the stored result and spells out every
  row, attribute and width that decides what CQP prints. Anything a future
  option changes about the output changes the batch, and so changes the
  key, without anyone having to remember to add it."
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

(defn groupable-attrs!
  "The attribute descriptions of `corpus` via `ctx` that a frequency
  breakdown can group by: its positional attributes and its annotated
  s-attributes, in registry order (a CQP round trip on a cache miss)."
  [ctx corpus]
  (filter #(or (= :positional (:type %)) (annotated-s-attr? %))
          (corpus/attributes! ctx corpus)))

(defn frequencies!
  "Group the matches of CQP `query` in `corpus` by `attr` at the match
  position via the installation described by `ctx`, within the :filter of
  `opts` when there is one, returning [{:values [...] :freq <n>} ...]
  sorted by frequency.

  A thin wrapper over CQP's `group`; `attr` must name one of the corpus's
  `groupable-attrs!`. Anything else is rejected, since attribute names are
  spliced into the command outside the QueryLock sandbox."
  ([ctx corpus query attr]
   (frequencies! ctx corpus query attr {}))
  ([ctx corpus query attr {:keys [filter]}]
   (let [ctx (corpus-ctx ctx corpus)]
     (when-not (some #(= (keyword attr) (:name %))
                     (groupable-attrs! ctx corpus))
       (throw (ex-info "Not a groupable attribute of this corpus"
                       {:corpus corpus :attr attr})))
     (let [commands [(str corpus ";")
                     (query/restricted-query
                      query (corpus-filter! ctx corpus filter))
                     (str "group Last match " (name attr) ";")]
           ;; a frequency breakdown runs the user's query like any other
           {:keys [results error]} (cqp/run-batch! (running-ctx ctx) commands)]
       (when error
         (throw (ex-info "Frequency query failed"
                         {:corpus corpus :query query :error error})))
       (parse/group->freqs (last results))))))

(defn corpus-frequencies!
  "Break the matches of CQP `query` in `corpus` down by `attr` via `ctx`
  (`opts` as for `frequencies!`) without failing: {:corpus ... :tokens
  <corpus size> :size <matches> :freqs [...]} (the maps of
  `frequencies!`), or {:corpus ... :error ...} when the breakdown cannot
  be made there.

  A blank `query` breaks the whole corpus down, read from its lexicon
  (dk.cst.corpus-probe.tools/lexicon!, positional attributes only) rather
  than by matching every token. Under a :filter it breaks down every
  token of the filtered regions instead, and the :tokens are theirs, so
  the rates per million stay relative to what was counted."
  [ctx corpus query attr {:keys [filter] :as opts}]
  (try
    (let [blank? (str/blank? query)
          freqs  (if (and blank? (empty? filter))
                   (tools/lexicon! ctx corpus attr)
                   (frequencies! ctx corpus (if blank? "[]" query) attr opts))
          tokens (if (empty? filter)
                   (:size (corpus/info! ctx corpus))
                   (size! ctx corpus "[]" opts))]
      {:corpus corpus
       :tokens tokens
       :size   (reduce + (map :freq freqs))
       :freqs  freqs})
    (catch Exception e
      {:corpus corpus :error (error-map e)})))

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

(defn row-order
  "A comparator putting merged frequency rows in display order: the
  largest total first, ties broken by value in the collation of
  `collator`."
  [^Comparator collator]
  (fn [a b]
    (let [c (compare (:total b) (:total a))]
      (if (zero? c)
        (.compare collator (:value a) (:value b))
        c))))

(defn frequency-rows
  "Merge the per-corpus breakdowns `results` (as from `corpus-frequencies!`,
  failures excluded) into the rows of one table, in no order: [{:value <s>
  :freqs {corpus <n>} :total <n>} ...]."
  [results]
  (->> (for [{:keys [corpus freqs]} results
             {:keys [values freq]}  freqs]
         [(first values) corpus freq])
       (reduce (fn [acc [value corpus freq]]
                 (assoc-in acc [value corpus] freq))
               {})
       (map (fn [[value freqs]]
              {:value value :freqs freqs :total (reduce + (vals freqs))}))))

(defn merge-frequencies
  "The `frequency-rows` of `results` in display order (see `row-order`,
  which orders by descending total and breaks ties with `collator`)."
  [collator results]
  (vec (sort (row-order collator) (frequency-rows results))))

(defn frequency-table!
  "Break the matches of CQP `query` in each of `corpora` (uppercase names,
  in display order) down by `attr` via `ctx`, in parallel, and merge the
  breakdowns into one table (see `parallelism`).

  Returns {:query ... :attr ... :counts [{:corpus ... :tokens ... :size
  ...} ...] :rows [{:value ... :freqs {corpus <n>} :total ...} ...]}; a
  corpus whose breakdown fails carries its :error instead of its counts
  and contributes no rows, like a failing corpus of `concordance!`. A blank
  `query` tables the whole corpora, or their filtered regions under the
  :filter of `opts`."
  ([ctx corpora query attr]
   (frequency-table! ctx corpora query attr {}))
  ([ctx corpora query attr opts]
   (let [results (vec (pmap-n (parallelism ctx)
                              #(corpus-frequencies! ctx % query attr opts)
                              corpora))]
     {:query  query
      :filter (:filter opts)
      :attr   (keyword attr)
      :counts (mapv #(dissoc % :freqs) results)
      :rows   (merge-frequencies (->collator ctx) (remove :error results))})))

(defn corpus-filters!
  "The metadata filters `corpus` offers via `ctx` without failing: one
  {:corpus ... :attr ... :freqs ...} per annotated s-attribute, in
  registry order, with the value list of
  dk.cst.corpus-probe.tools/annotation-values! (nil for an attribute with
  too many values to list); nothing, logged, when the corpus cannot be
  read."
  [ctx corpus]
  (try
    (vec (for [{attr :name} (filter annotated-s-attr?
                                    (corpus/attributes! ctx corpus))]
           {:corpus corpus
            :attr   attr
            :freqs  (tools/annotation-values! ctx corpus attr)}))
    (catch Exception e
      (t/event! ::filters-unavailable
                {:level :warn :error e :data {:corpus corpus}})
      nil)))

(defn filter-rows
  "The values of metadata attribute `attr` among the `entries` of
  `corpus-filters!`, merged over their corpora like a frequency table
  (the counts are regions) and sorted by value in the collation of
  `collator` (see `->collator`)."
  [collator attr entries]
  (->> (filter #(= attr (:attr %)) entries)
       (frequency-rows)
       (sort-by :value collator)
       (vec)))

(defn filter-options!
  "The metadata filters available over `corpora` via `ctx`, read in
  parallel (see `parallelism`): {:attrs [{:name <kw> :rows [{:value <s>
  :freqs {corpus <n>} :total <n>} ...]} ...] :unlisted [<kw> ...]}.

  The attributes keep the registry order of the first corpus reporting
  each, with the values of `filter-rows`; an attribute with too many
  values to list in any of the corpora is named under :unlisted instead."
  [ctx corpora]
  (let [entries  (->> (pmap-n (parallelism ctx) #(corpus-filters! ctx %)
                              corpora)
                      (apply concat))
        attrs    (distinct (map :attr entries))
        unlisted (set (map :attr (remove :freqs entries)))
        collator (->collator ctx)]
    {:attrs    (vec (for [attr (remove unlisted attrs)]
                      {:name attr :rows (filter-rows collator attr entries)}))
     :unlisted (vec (filter unlisted attrs))}))
