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
  "One metadata value, the leaf `m` of the filter's tree (see
  `filter-tree`), as a filter entry: a checkbox named for the
  attribute's filter param, checked when the set `selected` holds the
  leaf's id, and, in `ui`, how many regions carry the value, when
  known: a chosen value the corpora no longer offer has no count.

  A value the filter box has hidden keeps its checkbox in the document,
  for the reason a filtered-out corpus does: a box the form cannot see is
  part of a filter dropped without anyone saying so."
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
  "The controls asking for the values of `attr` a pattern matches,
  `pattern` being the one in force, and, over `rows` that are all
  numbers, those from one number to another, `bounds` being the [from to]
  in force, in `ui`: the way to a decade of years, to one year of dates,
  or to any value of an attribute with too many to list.

  Text fields, so they apply on Enter, which submits the form: a pattern
  is typed rather than chosen.

  A bound takes a whole number and says so, so a bound that is not one
  is reported by the browser, in its own words, rather than dropped by
  the server, which reads no number out of it. `hidden?` keeps the row
  in the document while the reader is shown only what is in force."
  [ui attr rows pattern [from to :as bounds] hidden?]
  (let [field (fn [prefix value attrs]
                [:input (merge {:name         (str prefix (name attr))
                                :value        (or value "")
                                :autocomplete "off"
                                :spellcheck   "false"}
                               attrs)])
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
  it, as the set of [attribute value] pairs: how the filter's tree names
  a value (see `filter-tree`)."
  [selected]
  (into #{} (for [[attr values] selected, value values] [attr value])))

(defn filter-node
  "The node of attribute `attr` in the filter's tree (see `filter-tree`):
  its listed `rows` as its leaves, followed by the values among the
  `chosen` pairs that the rows lack, so a selection is never lost on
  resubmit, and marked in force while a `pattern` or either of the
  `bounds` stands for it."
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
  "The metadata `filters` (see `filter-fieldset`) as the tree the
  chooser takes (see dk.cst.corpus-probe.views.chooser), keeping the
  [attribute value] pairs in `chosen`: a node per listed attribute (see
  `filter-node`), then one per attribute the list lacks but the chosen
  values, a pattern, a range or the unlisted names mention."
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
  its tree and the set of `selected` pairs, in `ui`.

  The same control the corpus chooser carries in this position, with the
  one direction that has no meaning here taken away. Choosing every value
  of every attribute is not a filter at all: it accepts every region
  carrying the attribute, which is what choosing none already does, and it
  would put one query parameter per value into the URL, tens of thousands
  of them at the KU registry. So it is disabled while nothing is chosen,
  which is the only state it could do that from, and every state it is
  offered in clears.

  It empties the whole filter rather than the part the box is showing.
  A filter is not a thing to empty by halves: what survived would be a
  constraint the reader had just told the box to hide from them."
  [ui nodes selected]
  (widgets/select-all (i18n/tr ui "Clear filter")
                      (mapcat #(map :id (:items %)) nodes)
                      selected
                      [:clear-filter]
                      {:clear-only? true}))

(defn filterable?
  "True when `filters` (see `filter-fieldset`) offer anything to filter
  by, or hold a selection to show: what decides whether the fieldset is
  rendered at all, and so whether a reader could open it to ask for
  fresh ones (see dk.cst.corpus-probe.ui/filters-stale?)."
  [{:keys [attrs unlisted selected]}]
  (boolean (or (seq attrs) (seq unlisted) (seq selected))))

(defn filter-fieldset
  "The metadata filter fieldset of the search form from `filters` (see
  dk.cst.corpus-probe.search.frequency/filter-options!); nil without
  metadata (see `filterable?`).

  The chooser over the filter's tree (see `filter-tree` and
  dk.cst.corpus-probe.views.chooser/chooser, which the `opts` are for, the
  selection and what is `:held` as pairs), so a corpus with forty
  annotated attributes is one line rather than forty. Inside it, a
  disclosure per attribute holds a pattern row (see `pattern-row`),
  shown at rest only while something is in force in it, and a checkbox
  per value (see `filter-item`), then a note naming the `:unlisted`
  attributes, all worded in `ui`. `:selected` maps each attribute to
  the set of chosen values, `:patterns` to the pattern in force and
  `:ranges` to the [from to] in force. The count is of `:selected`,
  which is live: it follows the boxes as the reader ticks them.

  The controls are the corpus chooser's in the same places: one per
  attribute taking every value offered, and beside the whole fieldset
  the one that clears it (see `clear-toggle`). Which attributes there
  are to filter by depends on the corpora selected, and only the server
  knows: `:pending?` marks the fieldset busy while the client is
  fetching them for a selection that has changed. The fieldset is
  classed for the middle layout, which gives it the whole row."
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
