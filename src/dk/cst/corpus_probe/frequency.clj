(ns dk.cst.corpus-probe.frequency
  "Frequency breakdowns, and the value lists a metadata filter offers.

  Both are the same shape of answer: how often something occurs, counted
  per corpus and merged into one table ordered by the collation the
  corpora are read in. A breakdown counts CQP's `group` over the matches
  of a query, or a whole corpus read from its lexicon; a filter's values
  are counted the same way over the regions of a structural attribute,
  which is why they share the merging and the ordering. A breakdown by a
  structural attribute also measures the text behind each value, so
  that its rate is per million tokens of that text rather than of the
  corpus, and a breakdown may count one attribute against another.

  Composed on dk.cst.corpus-probe.search, which owns the corpus context,
  the bounded fan-out over corpora and the collator."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.cache :as cache]
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
  breakdown can group by (see dk.cst.corpus-probe.search/countable-attr?),
  in registry order (a CQP round trip on a cache miss)."
  [ctx corpus]
  (filter search/countable-attr? (corpus/attributes! ctx corpus)))

(defn with-docs
  "The frequency maps `freqs` each given, as :docs, the frequency of
  their value among `doc-freqs`, the same values counted by the regions
  they occur in; none for a value counted in no region."
  [freqs doc-freqs]
  (let [docs (into {} (map (juxt (comp first :values) :freq)) doc-freqs)]
    (mapv #(assoc % :docs (get docs (first (:values %)) 0)) freqs)))

(defn groupable!
  "The `attrs` (names, strings or keywords) that a breakdown of `corpus`
  via `ctx` may group by, as keywords; throws for any that is none of the
  corpus's `groupable-attrs!`, since attribute names are spliced into
  the command outside the QueryLock sandbox."
  [ctx corpus attrs]
  (let [groupable (set (map :name (groupable-attrs! ctx corpus)))]
    (mapv (fn [attr]
            (or (groupable (keyword attr))
                (throw (ex-info "Not a groupable attribute of this corpus"
                                {:corpus corpus :attr attr}))))
          attrs)))

(defn run-breakdown!
  "The output sections of `commands` run as one batch against `corpus`
  via `ctx`, `query` being what they count, which the error names when
  CQP reports one, times out or dies."
  [ctx corpus query commands]
  ;; a frequency breakdown runs the user's query like any other
  (let [{:keys [results error]} (cqp/run-batch! (search/running-ctx ctx)
                                                commands)]
    (when error
      (throw (ex-info "Frequency query failed"
                      {:corpus corpus :query query :error error})))
    results))

(defn fresh-breakdown!
  "The output sections of the `counting` commands over the matches of
  `query` in `corpus` via `ctx`, run afresh: within the :filter of
  `opts` (see dk.cst.corpus-probe.query/restricted-query) and narrowed
  to its :subset and :near (see dk.cst.corpus-probe.query/narrowing)."
  [ctx corpus query {:keys [filter] :as opts} counting]
  (take-last (count counting)
             (run-breakdown! ctx corpus query
                             (-> [(str corpus ";")
                                  (query/restricted-query query filter)]
                                 (into (map second (query/narrowing opts)))
                                 (into counting)))))

(defn stored-breakdown!
  "The output sections of the `counting` commands over the saved query
  result `:nqr` of `opts` in `corpus` via `ctx`, or nil when none is
  stored or the stored one does not read, as
  dk.cst.corpus-probe.search/stored-sections! judges that: a result CQP
  cannot read is discarded, and the caller runs the query instead.

  What the concordance saved is what the breakdown counts, so switching
  a result to its frequency view runs no query. The result is sorted
  back into corpus order first, since it was saved in the order it was
  read in and a document frequency needs corpus order (see
  `frequencies!`). Its size is checked against the file (see
  dk.cst.corpus-probe.cache/holds?), because a whole result is read here
  and a file that has shrunk reads back short without CQP saying so."
  [ctx corpus query {:keys [nqr cache-dir] :as opts} counting]
  (when (and nqr (cache/stored? ctx corpus nqr))
    (try
      (let [results (run-breakdown!
                     ctx corpus query
                     (-> [(query/load-command corpus nqr cache-dir)
                          "size Last;"
                          "sort Last;"]
                         (into counting)))
            size    (parse-long (first (second results)))]
        (when-not (cache/holds? ctx corpus nqr size)
          (throw (ex-info "Stored result read back damaged"
                          {:corpus corpus :error {:type :damaged}})))
        (cache/touch! ctx corpus nqr)
        (cache/reap-due! ctx)
        (take-last (count counting) results))
      (catch Exception e
        (when (search/timeout? e)
          (throw e))
        (t/event! ::stored-result-discarded
                  {:level :warn
                   :data  {:corpus corpus :error (ex-message e)}})
        (cache/discard! ctx corpus nqr)
        nil))))

(defn breakdown!
  "The frequencies of `query`, kept within its unit already, in `corpus`
  by `attr` via `ctx` under `opts`, counted from the saved result or
  afresh: what `frequencies!` answers with once it knows there is
  something to count. See it for the arguments."
  [ctx corpus query attr {:keys [filter patterns subset at docs by]
                          :or   {at "match"}
                          :as   opts}]
  (let [[attr by] (groupable! ctx corpus (cond-> [attr] by (conj by)))
        ;; the options that decide which matches there are, as the
        ;; concordance that may have saved them had them (see
        ;; dk.cst.corpus-probe.search/kwic-opts!), so the two name one
        ;; saved result
        opts      (search/cache-opts
                   ctx corpus query
                   (assoc opts
                          :filter (search/corpus-filter! ctx corpus filter
                                                         patterns)
                          :subset (search/corpus-subset! ctx corpus subset)
                          :sample nil))
        whole?    (query/whole-match? at)
        text      (when (and docs (not whole?) (not by))
                    (search/within-attr! ctx corpus :text))
        counting  (cond-> [(query/count-command at attr {:by by})]
                    text (conj (query/count-command at attr {:within text})))
        parse     (cond
                    whole? parse/count->freqs
                    by     parse/group-pairs->freqs
                    :else  parse/group->freqs)
        [counts doc-counts] (map parse
                                 (or (stored-breakdown! ctx corpus query opts
                                                        counting)
                                     (fresh-breakdown! ctx corpus query opts
                                                       counting)))]
    (if text
      (with-docs counts doc-counts)
      counts)))

(defn frequencies!
  "Count the matches of CQP `query` in `corpus` by `attr` at the :at
  position of `opts` (a dk.cst.corpus-probe.query/positions entry; the
  start of the match by default) via the installation described by `ctx`,
  within the :filter of `opts` when there is one, kept within its :within
  unit (see dk.cst.corpus-probe.search/within-attr!) and narrowed to its
  :subset and :near (see dk.cst.corpus-probe.query/narrowing), returning
  [{:values [...] :freq <n>} ...] sorted by frequency. Under :docs, each
  map also carries the number of texts the value occurs in (its document
  frequency, see dk.cst.corpus-probe.query/count-command) as :docs, where
  the corpus marks texts. Under :by, another attribute of the corpus, the
  values are counted against each value of it at the match, and the
  :values of each map are then the value of `attr` and the value of :by
  (see dk.cst.corpus-probe.parse/group-pairs->freqs); no texts are
  counted then, one table not holding both. Neither applies over the
  whole match, which `count` gives no texts or pairs for.

  When `ctx` keeps a cache and a concordance has saved these matches
  under the :sort of `opts`, they are counted from the saved result
  rather than queried again (see `stored-breakdown!`); a sampled
  concordance is never read, a count of a sample being no count.

  A thin wrapper over CQP's `group`, or its `count` over the whole match
  (see dk.cst.corpus-probe.query/count-command); `attr` and :by must name
  the corpus's `groupable-attrs!` (see `groupable!`). A narrowing of
  nothing is answered without CQP (see
  dk.cst.corpus-probe.search/narrowing-nothing?)."
  ([ctx corpus query attr]
   (frequencies! ctx corpus query attr {}))
  ([ctx corpus query attr {:keys [within] :as opts}]
   (let [ctx   (search/corpus-ctx ctx corpus)
         query (search/corpus-query! ctx corpus query within)]
     (if (search/narrowing-nothing? ctx corpus query (dissoc opts :within))
       []
       (breakdown! ctx corpus query attr opts)))))
(defn sized-attr?
  "True when `attr` is an annotated s-attribute among the attribute
  descriptions `attributes`: one whose values each mark regions with a
  size of their own, against which a rate can be measured."
  [attributes attr]
  (boolean (some #(and (= (keyword attr) (:name %))
                       (search/annotated-s-attr? %))
                 attributes)))

(defn value-sizes!
  "How many tokens of `corpus` via `ctx` carry each value of `attr`,
  within the :filter and :patterns of `opts` when there are any:
  {<value> <tokens>}, or nil when `attr` is no annotated s-attribute of
  the corpus (see `sized-attr?`), its values marking no regions, or has
  too many regions to decode.

  What the rate per million of a value is measured against: the text
  carrying it rather than the whole corpus, so that a year with more
  text does not look busier. The whole corpus is read from the
  attribute's own regions (see
  dk.cst.corpus-probe.tools/annotation-sizes!); a filtered one is
  counted by grouping every token of the regions kept, as a blank query
  is."
  [ctx corpus attr {:keys [filter patterns] :as opts}]
  (when (sized-attr? (corpus/attributes! ctx corpus) attr)
    (if (or (seq filter) (seq patterns))
      (into {}
            (map (fn [{:keys [values freq]}] [(first values) freq]))
            ;; nothing saves a result of every token, so none is looked for
            (frequencies! ctx corpus "[]" attr
                          (assoc (select-keys opts [:filter :patterns])
                                 :cache? false)))
      (tools/annotation-sizes! ctx corpus attr))))

(defn sizes->freqs
  "The token counts `sizes` (value to tokens, see `value-sizes!`) as the
  frequency maps of `frequencies!`, sorted by frequency: what a whole
  corpus broken down by a structural attribute is, each value being as
  frequent as its regions are long."
  [sizes]
  (->> sizes
       (map (fn [[value n]] {:values [value] :freq n}))
       (sort-by :freq >)
       (vec)))

(defn corpus-frequencies!
  "Break the matches of CQP `query` in `corpus` down by `attr` via `ctx`
  (`opts` as for `frequencies!`) without failing: {:corpus ... :tokens
  <corpus size> :size <matches> :freqs [...]} (the maps of
  `frequencies!`), or {:corpus ... :error ...} when the breakdown cannot
  be made there. Where the values the rates are measured against, those
  of :by when there is one and of `attr` otherwise, mark text of their
  own (see `value-sizes!`), the map also carries their :sizes.

  A blank `query` breaks the whole corpus down, read from its lexicon
  (dk.cst.corpus-probe.tools/lexicon!) for a positional attribute and
  from the sizes of its regions for a structural one, rather than by
  matching every token. Under a :filter or :patterns it breaks down
  every token of the filtered regions instead, and the :tokens are
  theirs, so the rates per million stay relative to what was counted."
  [ctx corpus query attr {:keys [filter patterns at by] :as opts}]
  (try
    (let [blank?  (str/blank? query)
          whole?  (and (empty? filter) (empty? patterns))
          sizes   (when-not (query/whole-match? at)
                    (value-sizes! ctx corpus (or by attr) opts))
          freqs   (cond
                    (and blank? whole? (not by) sizes) (sizes->freqs sizes)
                    (and blank? whole?) (tools/lexicon! ctx corpus attr)
                    :else (frequencies! ctx corpus (if blank? "[]" query)
                                        attr opts))
          tokens  (if whole?
                    (:size (corpus/info! ctx corpus))
                    (search/size! ctx corpus "[]" opts))]
      (cond-> {:corpus corpus
               :tokens tokens
               :size   (reduce + (map :freq freqs))
               :freqs  freqs}
        sizes (assoc :sizes sizes)))
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
  :freqs {corpus <n>} :total <n>} ...], each row also carrying :docs
  {corpus <n>} where the breakdowns counted texts (see `frequencies!`)
  and :tokens {corpus <n>} where they measured the text of each value
  (see `value-sizes!`)."
  [results]
  (->> (for [{:keys [corpus freqs sizes]} results
             {:keys [values freq docs]} freqs]
         [(first values) corpus freq docs (get sizes (first values))])
       (reduce (fn [acc [value corpus freq docs tokens]]
                 (cond-> (assoc-in acc [value :freqs corpus] freq)
                   docs   (assoc-in [value :docs corpus] docs)
                   tokens (assoc-in [value :tokens corpus] tokens)))
               {})
       (map (fn [[value {:keys [freqs docs tokens]}]]
              (cond-> {:value value
                       :freqs freqs
                       :total (reduce + (vals freqs))}
                docs   (assoc :docs docs)
                tokens (assoc :tokens tokens))))))

(defn merge-frequencies
  "The `frequency-rows` of `results` in display order (see `row-order`,
  which orders by descending total and breaks ties with `collator`)."
  [collator results]
  (vec (sort (row-order collator) (frequency-rows results))))

(defn pair-rows
  "Merge the per-corpus breakdowns `results` counted against a second
  attribute (see `frequencies!` under :by; failures excluded) into the
  rows of one table, in no order, the corpora summed: [{:value <s>
  :cells {<value of the second> <n>} :total <n>} ...]."
  [results]
  (->> (for [{:keys [freqs]} results
             {[value by] :values :keys [freq]} freqs]
         [value by freq])
       (reduce (fn [acc [value by freq]]
                 (update-in acc [value by] (fnil + 0) freq))
               {})
       (map (fn [[value cells]]
              {:value value
               :cells cells
               :total (reduce + (vals cells))}))))

(def column-limit
  "The most values of the second attribute a table counted against one
  holds as columns: the most frequent ones, since a table with a column
  per text is no table, and a reader after fewer can filter."
  100)

(defn columns
  "The columns of the cross-tabulated `rows` (see `pair-rows`): the
  `column-limit` most frequent values of the second attribute, in the
  collation of `collator`, each with its :total and, where `results`
  measured the text of each value (their :sizes), its :tokens summed
  over the corpora."
  [collator results rows]
  (let [totals (reduce #(merge-with + %1 (:cells %2)) {} rows)
        sizes  (when (some :sizes results)
                 (apply merge-with + (map :sizes results)))]
    (->> (sort-by val > totals)
         (take column-limit)
         (map (fn [[value n]]
                (cond-> {:value value :total n}
                  sizes (assoc :tokens (get sizes value 0)))))
         (sort-by :value collator)
         (vec))))

(defn frequency-table!
  "Break the matches of CQP `query` in each of `corpora` (uppercase names,
  in display order) down by `attr` via `ctx`, in parallel, and merge the
  breakdowns into one table, in parallel (see
  `dk.cst.corpus-probe.search/parallelism`).

  Returns {:query ... :filter ... :subset ... :near ... :attr ... :at ...
  :docs <whether the rows count texts too> :sized <whether they measure
  the text of each value> :counts [{:corpus ... :tokens ... :size ...}
  ...] :rows [{:value ... :freqs {corpus <n>} :total ...} ...]} (see
  `frequency-rows`); a corpus whose breakdown fails carries its :error
  instead of its counts and contributes no rows, like a failing corpus
  of `concordance!`. A blank `query` tables the whole corpora, or their
  filtered regions under the :filter of `opts`.

  Under the :by of `opts`, an attribute the values are counted against,
  the table is a cross-tabulation with the corpora summed: its :rows
  are those of `pair-rows`, its :columns those of `columns`, of the
  :column-count values there were, and :sized then says whether the
  columns measure their text. Neither :by nor :docs applies over the
  whole match."
  ([ctx corpora query attr]
   (frequency-table! ctx corpora query attr {}))
  ([ctx corpora query attr {:keys [at docs by] :as opts}]
   (let [whole?   (query/whole-match? at)
         by       (when-not whole? (some-> by keyword))
         opts     (assoc opts :by by)
         results  (vec (search/pmap-n
                        (search/parallelism ctx)
                        #(corpus-frequencies! ctx % query attr opts)
                        corpora))
         counted  (remove :error results)
         collator (search/->collator ctx)
         table    {:query    query
                   :filter   (:filter opts)
                   :patterns (:patterns opts)
                   :subset   (:subset opts)
                   :near     (:near opts)
                   :attr     (keyword attr)
                   :at       (or at "match")
                   :docs     (boolean (and docs (not whole?) (not by)))
                   :sized    (boolean (some :sizes counted))
                   :counts   (mapv #(dissoc % :freqs :sizes) results)}]
     (if by
       (let [rows (vec (sort (row-order collator) (pair-rows counted)))]
         (assoc table
                :by           by
                :rows         rows
                :columns      (columns collator counted rows)
                :column-count (count (distinct (mapcat (comp keys :cells)
                                                       rows)))))
       (assoc table :rows (merge-frequencies collator counted))))))

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
