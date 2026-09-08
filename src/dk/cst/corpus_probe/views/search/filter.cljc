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

(defn value-count
  "How much of the corpus carries a metadata value, `n` of them, in
  words in `ui`: the unit the attribute `attr` annotates, which the
  prefix of its name gives (`text_year` annotates texts, `s_id`
  sentences), or regions where that prefix names no unit."
  [ui attr n]
  (str (i18n/group-digits ui n) " "
       (case (str/replace (name attr) #"_.*" "")
         "text"            (i18n/trn ui "text" "texts" n)
         ("s" "sentence")  (i18n/trn ui "sentence" "sentences" n)
         ("p" "paragraph") (i18n/trn ui "paragraph" "paragraphs" n)
         (i18n/trn ui "region" "regions" n))))

(defn filter-item
  "One metadata value, the leaf `m` of the filter's tree, as a checkbox
  named for the attribute's filter param, checked when the set
  `selected` holds the leaf's id, with how much of the corpus carries
  the value in `ui` when known (see `value-count`)."
  [ui selected {[attr value :as id] :id :keys [total hidden?] :as m}]
  [:li (widgets/hidden-attrs hidden?)
   [:label
    ;; a title is no part of a label's own text, so the box says the
    ;; count in its name too
    [:input (cond-> {:type    "checkbox"
                     :name    (str (:value url/filter-prefixes) (name attr))
                     :value   value
                     :checked (contains? selected id)
                     :on      {:change [:toggle-filter-values [attr [value]]]}}
              total (assoc :aria-label
                           (str value ", " (value-count ui attr total))))]
    " " (widgets/attribute-value attr value)
    ;; figures alone: a word beside them reads as a second value
    (when total
      (list " " (widgets/note [:data {:value (str total)}
                               (str "(" (i18n/group-digits ui total) ")")]
                              (value-count ui attr total))))]])

(defn numeric-values?
  "True when every listed value of `rows` is an integer, so that a range
  from one to another can be asked for."
  [rows]
  (boolean (and (seq rows) (every? #(parse-long (:value %)) rows))))

(defn value-span
  "The smallest and largest value of `rows`, which `numeric-values?` has
  said are all integers, as the strings a field shows."
  [rows]
  (let [ns (map (comp parse-long :value) rows)]
    [(str (apply min ns)) (str (apply max ns))]))

(defn range-bounds
  "The `bounds` ([from to]) of a range as the fields hold them over the
  value span [`lo` `hi`] (see `value-span`): an end left empty takes the
  end of the span, which is what its placeholder shows, so that writing
  one end asks for everything from or up to it. Both empty is no range."
  [[lo hi] [from to]]
  (cond
    (and (str/blank? from) (str/blank? to)) [nil nil]
    (str/blank? from)                       [lo to]
    (str/blank? to)                         [from hi]
    :else                                   [from to]))

(defn in-force?
  "True when a `pattern` or either of the `bounds` ([from to]) narrows an
  attribute beside the values chosen under it."
  [pattern bounds]
  (boolean (some #(not (str/blank? %)) (cons pattern bounds))))

(defn pattern-row
  "The controls narrowing the values of `attr` in `ui`: a `pattern` they
  must match and, over `rows` that are all numbers, the `bounds` ([from
  to]) they must lie between; `hidden?` keeps the row in the document
  while only what is in force is shown."
  [ui attr rows pattern bounds hidden?]
  (let [;; no label of its own: `label` is the placeholder and, with the
        ;; attribute after it, the accessible name, as chooser/filter-box.
        ;; Every keystroke goes into the state, so that what is in force
        ;; marks the attribute and is checked as the reader writes it
        field (fn [prefix value label action attrs]
                [:input (merge {:name         (str prefix (name attr))
                                :value        (or value "")
                                :placeholder  label
                                :aria-label   (str label " " (name attr))
                                :autocomplete "off"
                                :spellcheck   "false"
                                :on           {:input action}}
                               attrs)])
        ;; a text field with a numeric pattern: the browser then reports a
        ;; bound that is not a whole number, which the server would drop
        bound (fn [end value label placeholder]
                (field (end url/filter-prefixes) value label
                       [:set-filter-bound attr end :event.target/value]
                       {:type        "text"
                        :inputmode   "numeric"
                        :pattern     "-?[0-9]*"
                        :placeholder placeholder
                        :title       (i18n/tr ui "a whole number")}))]
    [:div.pattern (widgets/hidden-attrs hidden?)
     [:p.pattern-match
      (field (:pattern url/filter-prefixes) pattern (i18n/tr ui "pattern")
             [:set-filter-pattern attr :event.target/value]
             {:type "search"})
      ;; what the field takes has a glossary entry of its own, which a
      ;; placeholder of one word cannot say
      (widgets/help (i18n/tr ui "a regular expression") "regex")]
     (when (numeric-values? rows)
       ;; the span of the values as the placeholders, which says both that
       ;; the fields take a number and which numbers are there to ask for;
       ;; write one end and the other fills with the end it shows, so that
       ;; what the fields hold is what the search will read
       (let [[lo hi :as span] (value-span rows)
             [from to]        (range-bounds span bounds)]
         [:p.pattern-range
          (bound :from from (i18n/tr ui "from") lo)
          ;; the dash says the two are one range; both are named already,
          ;; so it is nothing for a screen reader to read between them
          [:span.range-mark {:aria-hidden "true"} "–"]
          (bound :to to (i18n/tr ui "to") hi)]))]))

(defn range-fault
  "What is wrong with the `ranges` asked of the `nodes` of the filter's
  tree, worded in `ui`: the first whose bounds no search can be made of
  (see dk.cst.corpus-probe.url/whole-range), which it would drop without
  a word, said with the span the attribute's own values run over; nil
  while every range stands."
  [ui nodes ranges]
  (some (fn [{:keys [id items]}]
          (let [bounds (get ranges id)]
            (when (and (some not-empty bounds) (numeric-values? items))
              (let [[lo hi :as span] (value-span items)]
                (when-not (url/whole-range (range-bounds span bounds))
                  (i18n/tr ui "Give {attribute} a range from {low} to {high}"
                           {:attribute (name id) :low lo :high hi}))))))
        nodes))

(defn filter-pairs
  "Turn `selected`, each metadata attribute mapped to the values chosen
  under it, into the set of [attribute value] pairs the filter's tree
  names a value by."
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
     :in-force? (in-force? pattern bounds)
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
                      ;; put a query parameter per value in the URL.
                      ;; A pattern or a range makes it live and mixed with
                      ;; no box ticked: it is the way back from one
                      {:clear-only? true
                       :mixed?      (boolean (some :in-force? nodes))}))

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
          nodes    (filter-tree filters (or held selected))
          noun     #(i18n/trn ui "value" "values" %)]
      (chooser/chooser
       ui :values nodes
       (assoc opts
              :class     "filters"
              :selected  selected
              :busy?     pending?
              :legend    (widgets/term ui :metadata false)
              :noun      noun
              :not-found (i18n/tr ui "No values found.")
              :invalid   (range-fault ui nodes ranges)
              :control   (fn [_] (clear-toggle ui nodes selected))
              ;; a pattern or a range narrows the attribute as its boxes
              ;; do, so its own box reads as neither all nor none, which
              ;; is what the summary would otherwise need words for
              :toggle    (fn [{:keys [id offered in-force?]}]
                           (widgets/select-all
                            (str (i18n/tr ui "All values of") " " (name id))
                            offered selected
                            [:toggle-filter-values [id (mapv second offered)]]
                            {:mixed? in-force?}))
              :item      (partial filter-item ui selected)
              :summary   (fn [{:keys [label] :as node}]
                           (list [:code label] " "
                                 (chooser/node-count ui noun selected node)))
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
