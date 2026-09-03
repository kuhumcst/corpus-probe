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
            [dk.cst.corpus-probe.corpus :as corpus]
            [dk.cst.corpus-probe.cqp :as cqp]
            [dk.cst.corpus-probe.parse :as parse]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.tools :as tools]
            [io.pedestal.log :as log]))

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
        (do (log/error :msg "search failed" :exception e)
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

  Throws ex-info when CQP reports an error, times out or dies."
  ([ctx corpus query]
   (kwic! ctx corpus query {}))
  ([ctx corpus query opts]
   (let [ctx        (corpus-ctx ctx corpus)
         attributes (corpus/attributes! ctx corpus)
         p-attrs    (attr-names #(= :positional (:type %)) attributes)
         annotated  (attr-names annotated-s-attr? attributes)
         requested  (:struct-attrs opts)
         _          (when-let [bad (seq (remove (set annotated) requested))]
                      (throw (ex-info "Unknown struct attributes"
                                      {:corpus corpus :struct-attrs bad})))
         opts       (merge query/kwic-defaults
                           opts
                           {:p-attrs      p-attrs
                            :struct-attrs (or requested annotated)
                            :filter       (corpus-filter! ctx corpus
                                                          (:filter opts))})
         commands   (query/kwic-commands corpus query opts)
         {:keys [results error]} (cqp/run-batch! ctx commands)]
     (when error
       (throw (ex-info "KWIC query failed"
                       {:corpus corpus :query query :error error})))
     (let [[_ _ _ size-lines _sort cat-lines dump-lines & tab-sections] results
           {:keys [struct-attrs rows]} opts
           hits    (parse/kwic->hits p-attrs cat-lines)
           anchors (parse/dump->anchors dump-lines)
           structs (when (seq struct-attrs)
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
        :size   (parse-long (first size-lines))
        :rows   rows
        :hits   (mapv (fn [hit anchor struct]
                        (cond-> (assoc hit :anchors anchor)
                          struct (assoc :structs struct)))
                      hits anchors (or structs (repeat nil)))}))))

(defn size!
  "The number of matches of CQP `query` in `corpus` via `ctx`, within the
  :filter of `opts` when there is one. Throws ex-info when CQP reports an
  error, times out or dies."
  ([ctx corpus query]
   (size! ctx corpus query {}))
  ([ctx corpus query {:keys [filter]}]
   (let [ctx (corpus-ctx ctx corpus)
         {:keys [results error]}
         (cqp/run-batch! ctx [(str corpus ";")
                              (query/restricted-query
                               query (corpus-filter! ctx corpus filter))
                              "size Last;"])]
     (when error
       (throw (ex-info "Size query failed"
                       {:corpus corpus :query query :error error})))
     (parse-long (first (last results))))))

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
                   (corpus-size! ctx corpus query opts)))
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
            res  (try (kwic! ctx corpus query (assoc opts :rows rows))
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
           {:keys [results error]} (cqp/run-batch! ctx commands)]
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

(defn merge-frequencies
  "Merge the per-corpus breakdowns `results` (as from `corpus-frequencies!`,
  failures excluded) into one table: [{:value <s> :freqs {corpus <n>}
  :total <n>} ...] sorted by descending total, then by value."
  [results]
  (->> (for [{:keys [corpus freqs]} results
             {:keys [values freq]}  freqs]
         [(first values) corpus freq])
       (reduce (fn [acc [value corpus freq]]
                 (assoc-in acc [value corpus] freq))
               {})
       (map (fn [[value freqs]]
              {:value value :freqs freqs :total (reduce + (vals freqs))}))
       (sort-by (juxt (comp - :total) :value))
       (vec)))

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
      :rows   (merge-frequencies (remove :error results))})))

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
      (log/warn :msg "metadata filters unavailable" :corpus corpus
                :exception e)
      nil)))

(defn filter-rows
  "The values of metadata attribute `attr` among the `entries` of
  `corpus-filters!`, merged over their corpora like a frequency table
  (the counts are regions) and sorted by value."
  [attr entries]
  (->> (filter #(= attr (:attr %)) entries)
       (merge-frequencies)
       (sort-by :value)
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
        unlisted (set (map :attr (remove :freqs entries)))]
    {:attrs    (vec (for [attr (remove unlisted attrs)]
                      {:name attr :rows (filter-rows attr entries)}))
     :unlisted (vec (filter unlisted attrs))}))
