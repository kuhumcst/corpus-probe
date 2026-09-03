(ns dk.cst.corpus-probe.search
  "High-level search operations: one function call in, plain data out.

  Composes query generation (dk.cst.corpus-probe.query), the child-process
  driver (dk.cst.corpus-probe.cqp) and the output parsers
  (dk.cst.corpus-probe.parse) into complete round trips.

  These functions are the trust boundary for the coming web layer: only the
  CQP query itself is protected by the QueryLock sandbox, so every other
  parameter spliced into a command (corpus names and attribute names) is
  validated here against the corpus's own inventory first."
  (:require [dk.cst.corpus-probe.corpus :as corpus]
            [dk.cst.corpus-probe.cqp :as cqp]
            [dk.cst.corpus-probe.parse :as parse]
            [dk.cst.corpus-probe.query :as query]))

(defn corpus-ctx
  "Return `ctx` configured for `corpus`: validates the corpus name (it is
  spliced into commands outside the QueryLock sandbox) and sets the
  corpus's own charset for the CQP round trip."
  [ctx corpus]
  (when-not (query/corpus-name? corpus)
    (throw (ex-info "Invalid corpus name" {:corpus corpus})))
  (assoc ctx :charset (corpus/charset ctx corpus)))

(defn attr-names
  "The names of the `attributes` matching `pred`."
  [pred attributes]
  (->> (filter pred attributes)
       (mapv :name)))

(defn annotated-s-attr?
  "True when attribute description `m` is an s-attribute carrying values."
  [{:keys [type values?] :as m}]
  (and (= :structural type) values?))

(defn kwic!
  "Run CQP `query` against `corpus` (an uppercase CQP corpus name) through
  the installation described by `ctx` (see dk.cst.corpus-probe.cqp) and
  return one KWIC page as data.

  Returns {:corpus ... :query ... :size <total hits> :page <n>
  :page-size <n> :hits [hit ...]} where each hit combines the parsed KWIC
  line (:cpos :left :match :right), its anchors from `dump` (:anchors) and
  its structural metadata (:structs). `opts` accepts :page, :page-size,
  :context (tokens), :sort (a sort mode) and :struct-attrs (defaults to every
  annotated s-attribute of the corpus; anything not in that inventory is
  rejected).

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
                            :struct-attrs (or requested annotated)})
         commands   (query/kwic-commands corpus query opts)
         {:keys [results error]} (cqp/run-batch! ctx commands)]
     (when error
       (throw (ex-info "KWIC query failed"
                       {:corpus corpus :query query :error error})))
     (let [[_ _ _ size-lines _sort cat-lines dump-lines & tab-sections] results
           {:keys [struct-attrs page page-size]} opts
           hits    (parse/kwic->hits p-attrs cat-lines)
           anchors (parse/dump->anchors dump-lines)
           structs (when (seq struct-attrs)
                     ;; one tabulate section per attribute: a whole line is
                     ;; one annotation value, so embedded TABs survive
                     (apply mapv
                            (fn [& values]
                              (zipmap struct-attrs values))
                            tab-sections))]
       {:corpus    corpus
        :query     query
        :size      (parse-long (first size-lines))
        :page      page
        :page-size page-size
        :hits      (mapv (fn [hit anchor struct]
                           (cond-> (assoc hit :anchors anchor)
                             struct (assoc :structs struct)))
                         hits anchors (or structs (repeat nil)))}))))

(defn frequencies!
  "Group the matches of CQP `query` in `corpus` by `attr` at the match
  position via the installation described by `ctx`, returning
  [{:values [...] :freq <n>} ...] sorted by frequency.

  A thin wrapper over CQP's `group`; `attr` must name one of the corpus's
  positional attributes or annotated s-attributes. Anything else is
  rejected, since attribute names are spliced into the command outside the
  QueryLock sandbox."
  [ctx corpus query attr]
  (let [ctx        (corpus-ctx ctx corpus)
        attributes (corpus/attributes! ctx corpus)
        groupable  (set (attr-names #(or (= :positional (:type %))
                                         (annotated-s-attr? %))
                                    attributes))]
    (when-not (groupable (keyword attr))
      (throw (ex-info "Not a groupable attribute of this corpus"
                      {:corpus corpus :attr attr :groupable groupable})))
    (let [commands [(str corpus ";")
                    (query/locked-query query)
                    (str "group Last match " (name attr) ";")]
          {:keys [results error]} (cqp/run-batch! ctx commands)]
      (when error
        (throw (ex-info "Frequency query failed"
                        {:corpus corpus :query query :error error})))
      (parse/group->freqs (last results)))))
