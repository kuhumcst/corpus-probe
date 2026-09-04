(ns dk.cst.corpus-probe.frequency
  "Frequency breakdowns, and the value lists a metadata filter offers.

  Both are the same shape of answer: how often something occurs, counted
  per corpus and merged into one table ordered by the collation the
  corpora are read in. A breakdown counts CQP's `group` over the matches
  of a query, or a whole corpus read from its lexicon; a filter's values
  are counted the same way over the regions of a structural attribute,
  which is why they share the merging and the ordering.

  Composed on dk.cst.corpus-probe.search, which owns the corpus context,
  the bounded fan-out over corpora and the collator."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.corpus :as corpus]
            [dk.cst.corpus-probe.cqp :as cqp]
            [dk.cst.corpus-probe.parse :as parse]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.search :as search]
            [dk.cst.corpus-probe.tools :as tools]
            [taoensso.telemere :as t])
  (:import [java.util Comparator]))

(defn groupable-attrs!
  "The attribute descriptions of `corpus` via `ctx` that a frequency
  breakdown can group by: its positional attributes and its annotated
  s-attributes, in registry order (a CQP round trip on a cache miss)."
  [ctx corpus]
  (filter #(or (= :positional (:type %)) (search/annotated-s-attr? %))
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
   (let [ctx (search/corpus-ctx ctx corpus)]
     (when-not (some #(= (keyword attr) (:name %))
                     (groupable-attrs! ctx corpus))
       (throw (ex-info "Not a groupable attribute of this corpus"
                       {:corpus corpus :attr attr})))
     (let [commands [(str corpus ";")
                     (query/restricted-query
                      query (search/corpus-filter! ctx corpus filter))
                     (str "group Last match " (name attr) ";")]
           ;; a frequency breakdown runs the user's query like any other
           {:keys [results error]} (cqp/run-batch! (search/running-ctx ctx)
                                                   commands)]
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
                   (search/size! ctx corpus "[]" opts))]
      {:corpus corpus
       :tokens tokens
       :size   (reduce + (map :freq freqs))
       :freqs  freqs})
    (catch Exception e
      {:corpus corpus :error (search/error-map e)})))

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
  breakdowns into one table, in parallel (see
  `dk.cst.corpus-probe.search/parallelism`).

  Returns {:query ... :attr ... :counts [{:corpus ... :tokens ... :size
  ...} ...] :rows [{:value ... :freqs {corpus <n>} :total ...} ...]}; a
  corpus whose breakdown fails carries its :error instead of its counts
  and contributes no rows, like a failing corpus of `concordance!`. A blank
  `query` tables the whole corpora, or their filtered regions under the
  :filter of `opts`."
  ([ctx corpora query attr]
   (frequency-table! ctx corpora query attr {}))
  ([ctx corpora query attr opts]
   (let [results (vec (search/pmap-n
                       (search/parallelism ctx)
                       #(corpus-frequencies! ctx % query attr opts)
                       corpora))]
     {:query  query
      :filter (:filter opts)
      :attr   (keyword attr)
      :counts (mapv #(dissoc % :freqs) results)
      :rows   (merge-frequencies (search/->collator ctx)
                                 (remove :error results))})))

(defn corpus-filters!
  "The metadata filters `corpus` offers via `ctx` without failing: one
  {:corpus ... :attr ... :freqs ...} per annotated s-attribute, in
  registry order, with the value list of
  dk.cst.corpus-probe.tools/annotation-values! (nil for an attribute with
  too many values to list); nothing, logged, when the corpus cannot be
  read."
  [ctx corpus]
  (try
    (vec (for [{attr :name} (filter search/annotated-s-attr?
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
  `collator` (see `dk.cst.corpus-probe.search/->collator`)."
  [collator attr entries]
  (->> (filter #(= attr (:attr %)) entries)
       (frequency-rows)
       (sort-by :value collator)
       (vec)))

(defn filter-options!
  "The metadata filters available over `corpora` via `ctx`, read in
  parallel (see `dk.cst.corpus-probe.search/parallelism`): {:attrs
  [{:name <kw> :rows [{:value <s> :freqs {corpus <n>} :total <n>} ...]}
  ...] :unlisted [<kw> ...]}.

  The attributes keep the registry order of the first corpus reporting
  each, with the values of `filter-rows`; an attribute with too many
  values to list in any of the corpora is named under :unlisted instead."
  [ctx corpora]
  (let [entries  (->> (search/pmap-n (search/parallelism ctx)
                                     #(corpus-filters! ctx %) corpora)
                      (apply concat))
        attrs    (distinct (map :attr entries))
        unlisted (set (map :attr (remove :freqs entries)))
        collator (search/->collator ctx)]
    {:attrs    (vec (for [attr (remove unlisted attrs)]
                      {:name attr :rows (filter-rows collator attr entries)}))
     :unlisted (vec (filter unlisted attrs))}))
