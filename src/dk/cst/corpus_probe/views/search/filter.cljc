(ns dk.cst.corpus-probe.views.search.filter
  "The metadata filter of the search form: the second instance of the
  chooser (see dk.cst.corpus-probe.views.chooser), over the values of the
  structural attributes the chosen corpora carry, a pattern row per
  attribute, and the pure rules turning the filter's options into the
  chooser's tree, which the client applies too."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.chooser :as chooser]
            [dk.cst.corpus-probe.views.widgets :as widgets]))

(defn filter-item
  "One metadata value, the leaf `m` of the filter's tree, as a checkbox
  named for the attribute's filter param, checked when the set
  `selected` holds the leaf's id, with how many regions carry the value
  in `ui` when known."
  [ui selected {[attr value :as id] :id :keys [total hidden?] :as m}]
  [:li (widgets/hidden-attrs hidden?)
   [:label
    [:input {:type    "checkbox"
             :name    (str (:value url/filter-prefixes) (name attr))
             :value   value
             :checked (contains? selected id)
             :on      {:change [:toggle-filter-values [attr [value]]]}}]
    " " (widgets/attribute-value attr value)
    (when total
      (list " " [:data {:value (str total)}
                 (str (i18n/group-digits ui total) " "
                      (i18n/trn ui "region" "regions" total))]))]])

(defn numeric-values?
  "True when every listed value of `rows` is an integer, so that a range
  from one to another can be asked for."
  [rows]
  (boolean (and (seq rows) (every? #(parse-long (:value %)) rows))))

(defn pattern-row
  "The controls asking for the values of `attr` a `pattern` matches and,
  over `rows` that are all numbers, those within `bounds` ([from to]),
  in `ui`; `hidden?` keeps the row in the document while only what is
  in force is shown."
  [ui attr rows pattern [from to :as bounds] hidden?]
  (let [field (fn [prefix value attrs]
                [:input (merge {:name         (str prefix (name attr))
                                :value        (or value "")
                                :autocomplete "off"
                                :spellcheck   "false"}
                               attrs)])
        ;; a text field with a numeric pattern: the browser then reports a
        ;; bound that is not a whole number, which the server would drop
        bound (fn [prefix value]
                (field prefix value
                       {:type      "text"
                        :inputmode "numeric"
                        :size      6
                        :pattern   "-?[0-9]*"
                        :title     (i18n/tr ui "a whole number")}))]
    [:p.pattern (widgets/hidden-attrs hidden?)
     [:label (i18n/tr ui "pattern") " "
      (field (:pattern url/filter-prefixes) pattern {:type "search"})]
     (when (numeric-values? rows)
       (list " "
             [:label (i18n/tr ui "from") " "
              (bound (:from url/filter-prefixes) from)]
             " "
             [:label (i18n/tr ui "to") " "
              (bound (:to url/filter-prefixes) to)]))]))

(defn filter-pairs
  "`selected`, each metadata attribute mapped to the values chosen under
  it, as the set of [attribute value] pairs the filter's tree names a
  value by."
  [selected]
  (into #{} (for [[attr values] selected, value values] [attr value])))

(defn filter-node
  "The node of attribute `attr` in the filter's tree: its listed `rows`
  as its leaves, then the `chosen` pairs the rows lack, so a selection
  is never lost on resubmit; in force while a `pattern` or either of
  the `bounds` stands for it."
  [attr rows chosen pattern bounds]
  (let [listed (set (map :value rows))
        rows   (into (vec rows)
                     (for [[a value] (sort chosen)
                           :when (and (= a attr) (not (listed value)))]
                       {:value value}))]
    {:id        attr
     :label     (name attr)
     :in-force? (boolean (some #(not (str/blank? %)) (cons pattern bounds)))
     :items     (mapv (fn [{:keys [value] :as row}]
                        (assoc row :id [attr value] :text value))
                      rows)
     :nodes     []}))

(defn filter-tree
  "The metadata `filters` as the tree the chooser takes, keeping the
  [attribute value] pairs in `chosen`: a node per listed attribute, then
  one per attribute only the chosen values, a pattern, a range or the
  unlisted names mention."
  [{:keys [attrs unlisted patterns ranges]} chosen]
  (let [listed (set (map :name attrs))
        node   (fn [attr rows]
                 (filter-node attr rows chosen
                              (get patterns attr) (get ranges attr)))]
    (into (mapv (fn [{attr :name :keys [rows]}] (node attr rows)) attrs)
          (for [attr  (sort (distinct (concat (map first chosen)
                                              (keys patterns)
                                              (keys ranges)
                                              unlisted)))
                :when (not (listed attr))]
            (node attr [])))))

(defn clear-toggle
  "The control emptying the whole metadata filter, over the `nodes` of
  its tree and the set of `selected` pairs, in `ui`: the corpus
  chooser's control with the one direction that means nothing here
  taken away."
  [ui nodes selected]
  (widgets/select-all (i18n/tr ui "Clear filter")
                      ;; every item, hidden ones included: a filter emptied
                      ;; by halves leaves a constraint the reader told the
                      ;; box to hide from them
                      (mapcat #(map :id (:items %)) nodes)
                      selected
                      [:clear-filter]
                      ;; taking every value is no filter at all, and would
                      ;; put a query parameter per value in the URL
                      {:clear-only? true}))

(defn filterable?
  "True when `filters` offer anything to filter by or hold a selection
  to show, which decides whether the fieldset is rendered at all."
  [{:keys [attrs unlisted selected]}]
  (boolean (or (seq attrs) (seq unlisted) (seq selected))))

(defn filter-fieldset
  "The metadata filter fieldset of the search form: the chooser over the
  tree of `filters` (see dk.cst.corpus-probe.search.frequency/filter-options!)
  with the chooser `opts`, what is `:held` among them as pairs, worded
  in `ui`; nil without metadata. `:selected` maps each attribute to its
  chosen values, `:patterns` to the pattern in force and `:ranges` to
  the [from to]; `:pending?` marks the fieldset busy while the client
  fetches the options of a changed corpus selection."
  [ui {:keys [unlisted selected patterns ranges] :as filters}
   {:keys [held pending?] :as opts}]
  (when (filterable? filters)
    (let [selected (filter-pairs selected)
          nodes    (filter-tree filters (or held selected))]
      (chooser/chooser
       ui :values nodes
       (assoc opts
              :class     "filters"
              :selected  selected
              :busy?     pending?
              :legend    (widgets/term ui :metadata false)
              :not-found (i18n/tr ui "No values found.")
              :control   (fn [_] (clear-toggle ui nodes selected))
              :toggle    (fn [{:keys [id offered]}]
                           (widgets/select-all
                            (str (i18n/tr ui "All values of") " " (name id))
                            offered selected
                            [:toggle-filter-values [id (mapv second offered)]]))
              :item      (partial filter-item ui selected)
              :summary   (fn [{:keys [label in-force?] :as node}]
                           (list [:code label] " "
                                 (chooser/node-count selected node)
                                 (when in-force?
                                   (str " · " (i18n/tr ui "pattern")))))
              :extra     (fn [{:keys [id items in-force?]} resting?]
                           (pattern-row ui id items
                                        (get patterns id) (get ranges id)
                                        (and resting? (not in-force?))))
              ;; a caveat about the control rather than part of it, which
              ;; is what <small> is for: these attributes are not on
              ;; offer here
              :after     (when (seq unlisted)
                           [:p [:small (i18n/tr ui "Too many values to list: ")
                                (interpose ", "
                                           (map (fn [attr] [:code (name attr)])
                                                unlisted))]]))))))
