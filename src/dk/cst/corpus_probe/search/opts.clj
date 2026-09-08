(ns dk.cst.corpus-probe.search.opts
  "The options of a search as one corpus runs them: the trust boundary of
  the web layer. Only the CQP query itself is protected by the QueryLock
  sandbox, so every other parameter spliced into a command (attribute
  names above all) is validated here against the corpus's own inventory,
  and every unit of text and sort mode is resolved to the corpus's own
  attribute."
  (:require [dk.cst.corpus-probe.cwb.command :as command]
            [dk.cst.corpus-probe.cwb.corpus :as corpus]
            [dk.cst.corpus-probe.cwb.tools :as tools]
            [dk.cst.corpus-probe.search.batch :as batch]
            [dk.cst.corpus-probe.search.cache :as cache]))

(defn within-attr!
  "The s-attribute of `corpus` via `ctx` that a search kept within `unit`
  is restricted to; nil for no unit, and for a corpus that does not mark
  it, where the search runs unrestricted rather than not at all."
  [ctx corpus unit]
  (when unit
    (corpus/unit-attr (corpus/attributes! ctx corpus) unit)))

(defn corpus-query!
  "CQP `query` as `corpus` via `ctx` runs it: its sentence tags and its
  within clause renamed after the corpus's own attributes, or dropped
  where it marks no such unit, and kept within `unit` (see
  `within-attr!`; nil for no unit)."
  [ctx corpus query unit]
  (let [attributes (corpus/attributes! ctx corpus)
        attr       #(corpus/unit-attr attributes %)]
    (-> query
        (command/sentence-tags (attr :sentence))
        (command/within-clause (into {}
                                     (map (juxt identity attr))
                                     (keys corpus/unit-attrs)))
        (command/within-query (attr unit)))))

(defn corpus-subset!
  "The narrowing `subset` as `corpus` via `ctx` may run it: its attribute
  checked against the corpus's countable attributes, since the name is
  spliced into a command outside the QueryLock; nil for none."
  [ctx corpus {:keys [attr] :as subset}]
  (when subset
    (when-not (some-> (corpus/attribute (corpus/attributes! ctx corpus) attr)
                      (corpus/countable-attr?))
      (throw (ex-info "Not an attribute of this corpus"
                      {:corpus corpus :attr attr})))
    subset))

(defn corpus-sort!
  "The sort mode `mode` as `corpus` via `ctx` may run it: the positional
  attribute it names, if any, checked against the corpus's inventory,
  since the name is spliced into a command outside the QueryLock."
  [ctx corpus mode]
  (when-let [attr (command/sort-attr mode)]
    (when-not (corpus/positional?
               (corpus/attribute (corpus/attributes! ctx corpus) attr))
      (throw (ex-info "Not a positional attribute of this corpus"
                      {:corpus corpus :attr attr}))))
  mode)

(defn corpus-context
  "The width of context a corpus with `attributes` shows for `context`: a
  number of words as it is, and a unit of text as the corpus's own
  attribute for it, or as the default width where the corpus marks no
  such unit, since a hit shown with the usual context beats one not shown
  at all."
  [attributes context]
  (if (keyword? context)
    (or (corpus/unit-attr attributes context) (:context batch/kwic-defaults))
    context))

(defn corpus-filter!
  "Metadata `filter` (attribute to the set of values accepted) and
  `patterns` (attribute to the regexes accepted) as
  dk.cst.corpus-probe.cwb.command/filter-query takes them for `corpus`
  via `ctx`: [attr values patterns] triples, the attribute with the most
  regions first; nil when neither restricts anything. Every attribute
  must be an annotated s-attribute of the corpus, since the names are
  spliced into a command."
  [ctx corpus filter patterns]
  (when (or (seq filter) (seq patterns))
    (let [regions (into {}
                        (keep (fn [{:keys [name regions values?]}]
                                (when values? [name regions])))
                        (:s-attrs (tools/describe-corpus! ctx corpus)))
          attrs   (distinct (concat (keys filter) (keys patterns)))]
      (when-let [bad (seq (remove regions attrs))]
        (throw (ex-info "Not an annotated structural attribute of this corpus"
                        {:corpus corpus :attrs bad})))
      (vec (sort-by (juxt (comp - regions first) first)
                    (for [attr attrs]
                      [attr (get filter attr #{}) (get patterns attr)]))))))

(defn cache-opts!
  "Give `opts` the cache directory and the name the result of `query` in
  `corpus` is saved under, when `ctx` keeps a cache and `opts` does not
  turn it off with a false :cache?."
  [ctx corpus query opts]
  (if-let [dir (and (:cache? opts true)
                    (cache/corpus-directory! ctx corpus))]
    ;; named from the filter as the corpus was given it, so that two
    ;; spellings of one filter share a saved result
    (assoc opts
           :cache-dir (str dir)
           :nqr       (cache/result-name ctx corpus query opts))
    opts))

(defn kwic-opts!
  "The options one KWIC batch of `query` in `corpus` needs, from the
  `opts` of dk.cst.corpus-probe.search/kwic! via `ctx`: the KWIC
  defaults, the corpus's positional and structural attributes, its
  context, filter, narrowing and sort mode as the corpus runs them, and
  whatever the cache adds (see `cache-opts!`). Requested :struct-attrs
  are checked against the corpus's inventory, since their names are
  spliced into a command."
  [ctx corpus query opts]
  (let [attributes (corpus/attributes! ctx corpus)
        annotated  (corpus/attr-names corpus/annotated-s-attr? attributes)
        requested  (:struct-attrs opts)
        opts       (merge batch/kwic-defaults opts)]
    (when-let [bad (seq (remove (set annotated) requested))]
      (throw (ex-info "Unknown struct attributes"
                      {:corpus corpus :struct-attrs bad})))
    (cache-opts! ctx corpus query
                 (assoc opts
                        :p-attrs      (corpus/attr-names corpus/positional?
                                                         attributes)
                        :struct-attrs (or requested annotated)
                        :context      (corpus-context attributes
                                                      (:context opts))
                        :filter       (corpus-filter! ctx corpus
                                                      (:filter opts)
                                                      (:patterns opts))
                        :subset       (corpus-subset! ctx corpus
                                                      (:subset opts))
                        :sort         (corpus-sort! ctx corpus
                                                    (:sort opts))))))

(defn size-args!
  "The arguments a count of CQP `query` in `corpus` via `ctx` under `opts`
  is keyed and run by, as dk.cst.corpus-probe.search/size! resolves them
  for that corpus: [ctx query opts]."
  [ctx corpus query {:keys [filter patterns sample within near subset]}]
  (let [ctx (corpus/corpus-ctx ctx corpus)]
    [ctx
     (corpus-query! ctx corpus query within)
     {:filter (corpus-filter! ctx corpus filter patterns)
      :subset (corpus-subset! ctx corpus subset)
      :near   near
      :sample sample}]))
