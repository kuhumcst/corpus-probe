(ns dk.cst.corpus-probe.views.page
  "Hiccup for the search page: the result summary, pagination and the
  inspection sidebar, plus the pieces shared with the frequency page:
  the search form, the error sections and the download links.

  The page these build is assembled by
  dk.cst.corpus-probe.views.app/search-view, which knows both views a
  result can be shown in. The server renders it for first paint with no
  selection; the client renders the same views from the same state, so
  clicking a token reveals the sidebar without a round trip. The markup
  uses the element HTML provides for each part: <search> for the query
  form, a named region for the outcome, <nav> for pagination, headings for
  errors and an <aside> for the inspector, so the document is meaningful
  without the stylesheet."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.controls :as controls]
            [dk.cst.corpus-probe.views.corpus :as corpus-views]
            [dk.cst.corpus-probe.views.kwic :as kwic]
            [dk.cst.corpus-probe.views.layout :as layout]
            [dk.cst.corpus-probe.views.tree :as tree]))

(def form-id
  "The id of the search form, so a control that acts on a result can sit
  beside the result and still submit the search that produced it."
  "search-form")

(defn sort-label
  "What the sort mode `value` (see dk.cst.corpus-probe.commands/sort-modes)
  is called, in `ui`; a mode naming a positional attribute (see
  dk.cst.corpus-probe.commands/sort-attr) is the match by that attribute.

  Naming them here rather than in the commands namespace keeps the CQP
  command table free of anything the interface decides."
  [ui value]
  (case value
    "corpus"  (i18n/tr ui "corpus order")
    "word"    (i18n/tr ui "match")
    "reverse" (i18n/tr ui "match from the end")
    "left"    (i18n/tr ui "left context")
    "right"   (i18n/tr ui "right context")
    "random"  (i18n/tr ui "random")
    (str (i18n/tr ui "match") " " value)))

(defn sort-control
  "The sort control of the concordance in `ui`: a select over
  the `sort-modes` values (see dk.cst.corpus-probe.commands/sort-modes)
  with `sort` chosen and each named by `sort-label`.

  It names the form it submits with, so it can sit beside the table it
  reorders rather than inside the query form: ordering a result is a
  different task from writing the query that produced it."
  [ui sort-modes sort]
  (list
   [:label {:for "sort"} (i18n/tr ui "Sort")]
   " "
   [:select {:id   "sort" :name "sort" :form form-id
             :on   {:change [:apply-view]}}
    (for [value sort-modes]
      [:option {:value value :selected (= value sort)}
       (sort-label ui value)])]))

(def sample-sizes
  "The sample sizes the concordance offers, in display order: as many
  hits as a reader might work through by hand, a result larger than that
  being read by sampling it rather than by paging to the end.

  A hand-written URL may name any other size, which `sample-control`
  then shows beside these."
  [50 100 500 1000])

(defn sample-control
  "The sample control of the concordance in `ui`: a select over the
  `sample-sizes` with `sample` chosen, or the whole result when it names
  none.

  A size the list does not hold is offered beside them, so that a URL
  naming one shows as the sample it is rather than as the whole result.
  It names the form it submits with for the reason `sort-control` does."
  [ui sample]
  (list
   [:label {:for "sample"} (i18n/tr ui "Sample")]
   " "
   [:select {:id   "sample" :name "sample" :form form-id
             :on   {:change [:apply-view]}}
    [:option {:value "" :selected (nil? sample)} (i18n/tr ui "all hits")]
    (for [n (sort (cond-> (set sample-sizes) sample (conj sample)))]
      [:option {:value n :selected (= n sample)}
       (i18n/group-digits ui n)])]))

(def context-widths
  "The widths of context the concordance offers, in display order: a few
  numbers of words, then the units of text a corpus marks (see
  dk.cst.corpus-probe.search/units), one region of which is shown either
  side. The first is the usual width (see
  dk.cst.corpus-probe.commands/kwic-defaults). A hand-written URL may name
  any other number of words, which `context-control` then shows beside
  these."
  [5 10 20 :sentence :paragraph])

(defn context-label
  "What the context width `context` (see `context-widths`) is called, in
  `ui`."
  [ui context]
  (case context
    :sentence  (i18n/tr ui "sentence")
    :paragraph (i18n/tr ui "paragraph")
    (str context " " (i18n/trn ui "word" "words" context))))

(defn context-control
  "The context control of the concordance in `ui`: a select over the
  `context-widths` with `context` (a number of words or a unit keyword)
  chosen, named by `context-label`. A number of words the list does not
  hold is offered among the numbers, in order. It names the form it
  submits with, for the reason `sort-control` does."
  [ui context]
  (let [widths (if (or (keyword? context) (some #{context} context-widths))
                 context-widths
                 (into (vec (sort (conj (filterv number? context-widths)
                                        context)))
                       (filter keyword? context-widths)))]
    (list
     [:label {:for "context"} (i18n/tr ui "Context")]
     " "
     [:select {:id   "context" :name "context" :form form-id
               :on   {:change [:apply-view]}}
      (for [width widths]
        [:option {:value    (if (keyword? width) (name width) width)
                  :selected (= width context)}
         (context-label ui width)])])))

(def near-distance
  "How many words away a nearby word may be unless the reader says: the
  distance of the manual's own example (section 3.7), and the window a
  collocation is usually counted in."
  5)

(def near-distances
  "The distances the near control offers, in display order."
  [1 2 3 5 10])

(defn near-control
  "The proximity control of a result in `ui`: the word every hit must
  have nearby and how many words away it may be, from `near` (the :word
  and :distance in force, if any) and the `near-distances`.

  The word is typed rather than chosen, so it applies once the reader is
  done with it: a text field reports a change on Enter and on focus
  leaving it, and the change applies the view as a select's does. Enter
  alone could not be relied on: implicit submission does not reach a
  form from a field that only names it. The distance applies itself as
  the sort does. A distance the list does not hold is offered beside
  them, as a sample size is."
  [ui {:keys [word distance]}]
  (let [distance (or distance near-distance)]
    (list
     [:label {:for "near"} (i18n/tr ui "Near")]
     " "
     [:input {:id           "near"
              :name         "near"
              :type         "search"
              :form         form-id
              :value        (or word "")
              :autocomplete "off"
              :on           {:change [:apply-view]}}]
     " "
     [:label {:for "distance"} (i18n/tr ui "within")]
     " "
     [:select {:id   "distance" :name "distance" :form form-id
               :on   {:change [:apply-view]}}
      (for [n (sort (conj (set near-distances) distance))]
        [:option {:value n :selected (= n distance)}
         (str n " " (i18n/trn ui "word" "words" n))])])))

(defn apply-button
  "The button applying a result's controls where no `client?` runs to
  apply them itself, in `ui`; nil where one does.

  Each control applies itself on being changed where the client runs, so
  there is no button: choosing an order is asking for it, and a control
  that needs a second control to take effect is one the reader has to be
  told about. Without a client nothing can act on a change, so the button
  is what applies it there.

  Inside <noscript>: the server renders every page for the reader without
  a script, and a browser with one showed the button for the split second
  before the client's first render took it away. Wrapped, that browser
  never shows it, while one without a script does."
  [ui client?]
  (when-not client?
    (list " " [:noscript
               [:button {:type "submit" :form form-id} (i18n/tr ui "Apply")]])))

(defn view-controls
  "The controls of a result in `ui`: the `reading` ones (hiccup), which
  decide how the hits are read rather than what was searched for, and
  behind a disclosure the `narrowing` ones (hiccup; nil for none), which
  keep only some of the hits, `open?` saying whether that disclosure
  starts open. Nil without either.

  They live with the result rather than in the query form, so re-ordering
  a concordance costs a click instead of a scroll back past the form.
  Narrowing one is here for the same reason though it runs the query
  again: which of the hits to keep is a question the reader has on
  seeing them. It is behind a disclosure because it is the rarer
  question, and a row of six controls reads as a form to fill in; the
  disclosure is open whenever a narrowing is in force, so what narrows a
  result is never hidden from the reader it narrows it for.

  Each row carries the `apply-button` where no `client?` runs."
  [ui client? reading narrowing open?]
  (when (or reading narrowing)
     [:div.viewctl
      (when reading
        [:p reading (apply-button ui client?)])
      (when narrowing
        [:details {:open (boolean open?)}
         [:summary (i18n/tr ui "Narrow the result")]
         [:p narrowing (apply-button ui client?)]])]))

(def filter-prefix
  "The query param prefix naming a metadata filter: the prefix followed by
  the attribute name, as in `f.text_year`, one param per chosen value."
  "f.")

(defn filter-phrase
  "The metadata `filter` (a map of attribute to the set of values
  accepted) and the `patterns` beside it (a map of attribute to the
  regexes accepted) in words: each attribute with its values, sorted,
  then its patterns between slashes; empty without either.

  (filter-phrase {:text_year #{\"1591\" \"1583\"}} {:text_title [\"Hav.*\"]})
  ;; => \"text_title /Hav.*/; text_year 1583, 1591\""
  [filter patterns]
  (str/join "; " (for [attr (sort (distinct (concat (keys filter)
                                                    (keys patterns))))]
                   (str (name attr) " "
                        (str/join ", " (concat (sort (get filter attr))
                                               (map #(str "/" % "/")
                                                    (get patterns attr))))))))

(defn within-phrase
  "The metadata `filter` and its `patterns` as a qualifier of a result
  in `ui`, or nil without either: \"within text_year 1591\"."
  [ui filter patterns]
  (when (or (seq filter) (seq patterns))
    (str (i18n/tr ui "within") " " (filter-phrase filter patterns))))

(defn position-label
  "What the `position` of a match (see
  dk.cst.corpus-probe.commands/positions) is called, in `ui`, worded to
  follow an attribute name; the position itself for one nothing names."
  [ui position]
  (case position
    "match[-1]"       (i18n/tr ui "before the match")
    "match"           (i18n/tr ui "at the start of the match")
    "match..matchend" (i18n/tr ui "over the whole match")
    "matchend"        (i18n/tr ui "at the end of the match")
    "matchend[1]"     (i18n/tr ui "after the match")
    position))

(defn subset-phrase
  "That a result holds only the hits whose token at the :anchor of
  `subset` has its :value as its :attr, in `ui`, the attribute and the
  value as the code they are; nil without one."
  [ui {:keys [anchor attr value] :as subset}]
  (when subset
    (list [:code (name attr)] " " (position-label ui anchor) " = "
          [:code value])))

(defn near-phrase
  "That a result holds only the hits with the :word of `near` nearby,
  in `ui`; nil without one."
  [ui {:keys [word] :as near}]
  (when near
    (list (i18n/tr ui "near") " " [:code word])))

(defn attribute-value
  "Render attribute value `v` semantically by its key `k`: a text title as
  `<cite>`, a four-digit year as `<time>`, otherwise plain text."
  [k v]
  (let [n (name k)]
    (cond
      (str/ends-with? n "_title")                              [:cite v]
      (and (str/ends-with? n "_year") (re-matches #"\d{4}" v)) [:time v]
      :else                                                    v)))

(defn filter-item
  "One metadata value, the leaf `m` of the filter's tree (see `tree`),
  as a filter entry: a checkbox named for the attribute's filter param,
  checked when the set `selected` holds the leaf's id, and, in `ui`,
  how many regions carry the value, when known: a chosen value the
  corpora no longer offer has no count.

  A value the filter box has hidden keeps its checkbox in the document,
  for the reason a filtered-out corpus does: a box the form cannot see is
  part of a filter dropped without anyone saying so."
  [ui selected {[attr value :as id] :id :keys [total hidden?] :as m}]
  [:li (cond-> {} hidden? (assoc :hidden true))
   [:label
    [:input {:type    "checkbox"
             :name    (str filter-prefix (name attr))
             :value   value
             :checked (contains? selected id)
             :on      {:change [:toggle-filter-values [attr [value]]]}}]
    " " (attribute-value attr value)
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
    [:p.pattern (cond-> {} hidden? (assoc :hidden true))
     [:label (i18n/tr ui "pattern") " " (field "fp." pattern {:type "search"})]
     (when (numeric-values? rows)
       (list " "
             [:label (i18n/tr ui "from") " " (bound "ff." from)]
             " "
             [:label (i18n/tr ui "to") " " (bound "ft." to)]))]))

(defn pairs
  "`selected`, each metadata attribute mapped to the values chosen under
  it, as the set of [attribute value] pairs: how the filter's tree names
  a value (see `tree`)."
  [selected]
  (into #{} (for [[attr values] selected, value values] [attr value])))

(defn attr-node
  "The node of attribute `attr` in the filter's tree (see `tree`): its
  listed `rows` as its leaves, followed by the values among the `chosen`
  pairs that the rows lack, so a selection is never lost on resubmit,
  and marked in force while a `pattern` or either of the `bounds` stands
  for it."
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

(defn tree
  "The metadata `filters` (see `filter-fieldset`) as the tree the
  chooser takes (see dk.cst.corpus-probe.views.tree), keeping the
  [attribute value] pairs in `chosen`: a node per listed attribute (see
  `attr-node`), then one per attribute the list lacks but the chosen
  values, a pattern, a range or the unlisted names mention."
  [{:keys [attrs unlisted patterns ranges]} chosen]
  (let [listed (set (map :name attrs))
        node   (fn [attr rows]
                 (attr-node attr rows chosen
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
  (controls/select-all (i18n/tr ui "Clear filter")
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
  dk.cst.corpus-probe.frequency/filter-options!); nil without metadata
  (see `filterable?`).

  The chooser over the filter's tree (see `tree` and
  dk.cst.corpus-probe.views.tree/chooser, which the `opts` are for, the
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
  fetching them for a selection that has changed."
  [ui {:keys [unlisted selected patterns ranges] :as filters}
   {:keys [held pending?] :as opts}]
  (when (filterable? filters)
    (let [selected (pairs selected)
          nodes    (tree filters (or held selected))]
      (tree/chooser
       ui :values nodes
       (assoc opts
              :selected  selected
              :busy?     pending?
              :legend    (layout/term ui :metadata false)
              :not-found (i18n/tr ui "No values found.")
              :control   (fn [_] (clear-toggle ui nodes selected))
              :toggle    (fn [{:keys [id offered]}]
                           (controls/select-all
                            (str (i18n/tr ui "All values of") " " (name id))
                            offered selected
                            [:toggle-filter-values [id (mapv second offered)]]))
              :item      (partial filter-item ui selected)
              :summary   (fn [{:keys [label in-force?] :as node}]
                           (list [:code label] " "
                                 (tree/node-count selected node)
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

(defn query-phrase
  "The query of `params` in words for a title, in `ui`: the text as
  typed, or, for a list, how many words it holds (see
  dk.cst.corpus-probe.query/words), a title being one line and a list
  not, or, for an extended search, the CQP its tokens compile to (see
  dk.cst.corpus-probe.query/->cqp), which is what the CQP mode shows
  too."
  [ui {:keys [q] :as params}]
  (case (url/mode params)
    "list"     (let [n (count (query/words q))]
                 (str n " " (i18n/trn ui "word" "words" n)))
    "extended" (query/->cqp (query/of params))
    q))

(def reading-id
  "The id of the line under the query field saying how its text is read
  (see `reading-line`), by which the field is described (see
  `query-field`)."
  "reading")

(defn query-field
  "The query field of the search form in `ui`, holding `text`: a text
  area, so that a list can be typed one word per line: one row, and one
  more for every line break in the text, the empty line a Shift+Enter
  has just opened included, up to eight, and by nothing else (the
  stylesheet takes the handle away); the text is read by its shape
  (see dk.cst.corpus-probe.url/shape and `reading-line`). On the client
  Enter submits the form and Shift+Enter starts a line (see
  dk.cst.corpus-probe.ui/handle!). No visible label: a field with a
  search button beside it needs
  none to say what it is, so the name it keeps is the one only a screen
  reader reads, and the line under it describes the field to the same
  reader when its id is given as `described-by`. Every key dispatches
  `:set-query`, so the state holds the text as typed and the answer can
  tell when the form has moved on from what ran (see `question`).

  Required when `required?`: a search of nothing is then reported by the
  browser before it is sent, rather than answered with the help again.
  The browser's own check passes whitespace, so the field reports a
  blank of any length itself, through the render hook `:set-validity`
  (see dk.cst.corpus-probe.ui/handle!), in the interface's words. The
  caller says when a blank query means something (see `search-form`)."
  [ui text required? described-by]
  (let [text  (str text)
        attrs (cond-> {:id           "q"
                       :name         "q"
                       :rows         (min 8 (inc (count (re-seq #"\r\n|[\r\n]"
                                                                text))))
                       :aria-label   (i18n/tr ui "Query")
                       :placeholder  (i18n/tr ui "words, a list or CQP")
                       :autocomplete "off"
                       :spellcheck   "false"
                       :enterkeyhint "search"
                       :required     required?
                       :on           {:input   [:set-query]
                                      :keydown [:submit-on-enter]}
                       :replicant/on-render
                       [:set-validity (when (and required?
                                                 (str/blank? text))
                                        (i18n/tr ui "Type a query"))]}
                described-by (assoc :aria-describedby described-by))]
    ;; the text is the element's content, which is how a document
    ;; carries it, a text area having no value attribute; the client
    ;; sets the value property too, since the content is only what the
    ;; area starts with
    [:textarea #?(:clj attrs :cljs (assoc attrs :value text)) text]))

(defn reading-line
  "How the field's text in `params` is read, under it in `ui`, where
  that wants saying: as CQP; or as words in order, or as any one of
  them, with the CQP they run as (see dk.cst.corpus-probe.query/->cqp),
  so that a reader sees what a phrase or a list becomes before a search
  is spent on it, and where CQP is learnt by example. Nothing for a
  blank field or one word, which read as they look. A paragraph rather
  than an output, for the reason `cqp-line` is, carrying `reading-id`
  so that the field can name it as its description."
  [ui params]
  (let [query (query/of params)
        line  (fn [reading]
                [:p.cqp {:id reading-id} reading " · " (i18n/tr ui "As CQP")
                 ": " [:code (query/->cqp query)]])
        n     (fn [xs] (i18n/group-digits ui (count xs)))]
    (case (url/mode params)
      "cqp"  [:p.cqp {:id reading-id} (i18n/tr ui "Read as CQP")]
      "list" (let [words (:conditions (first (:tokens query)))]
               (when (next words)
                 (line (i18n/tr ui "Any one of {n} words" {:n (n words)}))))
      (let [tokens (:tokens query)]
        (when (next tokens)
          (line (i18n/tr ui "{n} words in order" {:n (n tokens)})))))))

(defn attribute-label
  "What the positional attribute named `attr` is called in `ui`: the
  usual ones in the reader's words, since they stand in a sentence (see
  `search-form`), any other as its corpus names it."
  [ui attr]
  (case attr
    "word"  (i18n/trx ui "attribute" "word")
    "lemma" (i18n/trx ui "attribute" "lemma")
    "pos"   (i18n/trx ui "attribute" "POS")
    "msd"   (i18n/trx ui "attribute" "morphology")
    attr))

(defn attribute-options
  "The options of a select over the positional `attrs` (attribute
  keywords, word first) with `selected` (a string, word when blank)
  chosen, each called as `ui` calls it (see `attribute-label`).

  An attribute the list lacks is offered after them, so a hand-written
  URL shows what it searches rather than something else."
  [ui attrs selected]
  (let [selected (if (str/blank? selected) "word" selected)
        names    (map name attrs)
        offered  (cond-> names
                   (not (some #{selected} names)) (concat [selected]))]
    (for [n offered]
      [:option {:value n :selected (= n selected)} (attribute-label ui n)])))

(defn attribute-control
  "The control choosing which positional attribute a simple search
  matches, in `ui`: a select over `attrs` with `selected` chosen (see
  `attribute-options`), named for a screen reader only, since it stands
  in a sentence (see `search-form`)."
  [ui attrs selected]
  [:select {:name "in" :aria-label (i18n/tr ui "attribute")}
   (attribute-options ui attrs selected)])

(defn operator-label
  "What the operator `op` of an extended-search token is called, in `ui`
  (see dk.cst.corpus-probe.url/operators); equality for one it does not
  know."
  [ui op]
  (case op
    "not"       (i18n/tr ui "is not")
    "prefix"    (i18n/tr ui "starts with")
    "suffix"    (i18n/tr ui "ends with")
    "infix"     (i18n/tr ui "contains")
    "regex"     (i18n/tr ui "matches regex")
    "not-regex" (i18n/tr ui "does not match regex")
    "any"       (i18n/tr ui "any word")
    (i18n/tr ui "is")))

(defn value-list-id
  "The id of the datalist holding the values of positional attribute
  `attr` (a keyword or its name), which a value field offers as
  suggestions (see `token-fieldset`)."
  [attr]
  (str "values-" (name attr)))

(defn condition-row
  "One condition of the extended search in `ui`: condition `c`, counted
  from one, holding `condition` (see
  dk.cst.corpus-probe.url/form-tokens), of `token`, the facts of the
  token it belongs to: its number `:i`, the `:attrs` a condition may
  name (see `attribute-options`), the `:value-lists` some of them offer
  (see `value-list-id`), `:any?` when the token's first condition
  matches any word, and `:removable?` when a button may take the
  condition away. The attribute, the operator, the value and the
  ignore-case box, headed by how it joins the conditions before it when
  it is not the first.

  Its fields carry the token's number and, after the first, its own
  (see dk.cst.corpus-probe.url/token-key). The value is required when
  `required?`. Under `:any?` every control but that first operator is
  disabled: an any-word token has nothing else to say, and without the
  client they are as the search was submitted. Every control dispatches
  `:set-condition` with its field, so the state holds the condition as
  the reader has it: the operator and the attribute decide which
  controls are live and which values the field suggests, and all of
  them decide the CQP line under the tokens (see `cqp-line`)."
  [ui {:keys [i attrs value-lists any? removable?]} required? c
   {:keys [id attr op v ci join]}]
  (let [param  (fn [field] (url/token-key i c field))
        first? (= 1 c)
        op     (or op "is")
        dead?  (and any? (not first?))
        attr*  (keyword (if (str/blank? attr) "word" attr))]
    [:li {:replicant/key id}
     (when-not first?
       (list [:select {:name       (param :join)
                       :aria-label (i18n/tr ui "joined by")
                       :disabled   dead?
                       :on         {:change [:set-condition [i id :join]]}}
              (for [j url/joins]
                [:option {:value j :selected (= j (or join "and"))}
                 (case j "or" (i18n/tr ui "or") (i18n/tr ui "and"))])]
             " "))
     [:select {:name       (param :attr)
               :aria-label (i18n/tr ui "attribute")
               :disabled   any?
               :on         {:change [:set-condition [i id :attr]]}}
      (attribute-options ui attrs attr)]
     " "
     [:select {:name       (param :op)
               :aria-label (i18n/tr ui "operator")
               :disabled   dead?
               :on         {:change [:set-condition [i id :op]]}}
      (for [o (cond->> url/operators (not first?) (remove #{"any"}))]
        [:option {:value o :selected (= o op)} (operator-label ui o)])]
     " "
     [:input (cond-> {:type         "text"
                      :name         (param :v)
                      :value        (or v "")
                      :aria-label   (i18n/trx ui "field" "value")
                      :autocomplete "off"
                      :spellcheck   "false"
                      :required     (and required? (not any?))
                      :disabled     any?
                      :on           {:input [:set-condition [i id :v]]}}
               (contains? value-lists attr*)
               (assoc :list (value-list-id attr*)))]
     " "
     [:label [:input {:type     "checkbox" :name (param :ci) :value "on"
                      :checked  (some? ci)
                      :disabled any?
                      :on       {:change [:set-condition [i id :ci]]}}]
      (i18n/tr ui "ignore case")]
     (when removable?
       (list " "
             [:button {:type       "button"
                       :aria-label (str (i18n/tr ui "Remove condition") " " c)
                       :on         {:click [:remove-condition [i id]]}}
              "×"]))]))

(defn token-row
  "One token of the extended search in `ui`: token `i`, counted from
  one, which is the number its fields carry in the URL, holding `token`
  (see dk.cst.corpus-probe.url/form-tokens) over `attrs` and
  `value-lists` (see `condition-row`). A group of its own, named by
  number: its conditions as an ordered list, since each joins the ones
  before it, then the repeat as least and most, in a group named for
  what the pair is, whether it opens or closes a sentence, and, where
  `client?`, buttons adding a condition and taking the token away. The
  repeat and the edges dispatch `:set-token` with their field, as the
  conditions' controls do theirs.

  The first condition's value is required when `required?`; the others'
  only where the client runs, which is where a condition is added, so a
  reader without it can empty a condition to be rid of it. A condition
  can be taken away while the token has another."
  [ui attrs value-lists client? required? i
   {:keys [id conditions start end] lo :min hi :max}]
  (let [param      (fn [field] (url/token-key i 1 field))
        conditions (or (seq conditions) [{:id 1}])
        any?       (= "any" (:op (first conditions)))
        token      {:i           i
                    :attrs       attrs
                    :value-lists value-lists
                    :any?        any?
                    :removable?  (and client? (boolean (next conditions)))}]
    [:fieldset.token
     [:legend (str (i18n/tr ui "Token") " " i)]
     [:ol
      (map-indexed (fn [j condition]
                     (let [c (inc j)]
                       (condition-row ui token
                                      (and required? (or (= 1 c) client?))
                                      c condition)))
                   conditions)]
     [:p
      ;; the second number is labelled "to", which says nothing on its
      ;; own; the group says what the pair is
      [:span {:role "group" :aria-label (i18n/tr ui "repeat")}
       [:label (i18n/tr ui "repeat") " "
        [:input {:type "number" :name (param :min) :value (or lo "1")
                 :min  0 :max 99
                 :on   {:input [:set-token [i :min]]}}]]
       " "
       [:label (i18n/tr ui "to") " "
        [:input {:type "number" :name (param :max) :value (or hi "1")
                 :min  0 :max 99
                 :on   {:input [:set-token [i :max]]}}]]]
      " "
      [:label [:input {:type    "checkbox" :name (param :start) :value "on"
                       :checked (some? start)
                       :on      {:change [:set-token [i :start]]}}]
       (i18n/tr ui "sentence start")]
      " "
      [:label [:input {:type    "checkbox" :name (param :end) :value "on"
                       :checked (some? end)
                       :on      {:change [:set-token [i :end]]}}]
       (i18n/tr ui "sentence end")]]
     ;; the token's own actions on a row of their own, so they stay together
     (when client?
       [:p
        [:button {:type     "button"
                  :disabled any?
                  :on       {:click [:add-condition i]}}
         (i18n/tr ui "Add condition")]
        " "
        [:button {:type       "button"
                  :aria-label (str (i18n/tr ui "Remove token") " " i)
                  :on         {:click [:remove-token id]}}
         "×"]])]))

(defn token-fieldset
  "The tokens of the extended search in `ui`: one group per token of
  `tokens` (see `token-row`) over `attrs` and `value-lists`, as an
  ordered list inside a group of their own, since a token is one of a
  sequence and a screen reader says which, with the datalists the value
  fields draw on (see `value-list-id`). One blank token when there are
  none, since the client may have just switched to the mode; otherwise
  the tokens are the search's own plus the blank one the server ends
  them in (see dk.cst.corpus-probe.query/form-rows), so a reader
  without the client adds a token by filling it and searching again.

  When `required?`, every token must be filled but that blank last one,
  which only a reader without the client sees: the client drops it (see
  dk.cst.corpus-probe.ui/own-rows) and adds tokens by a button, so with
  it every token must be filled, and a token added and left empty is
  reported rather than silently dropped. A lone token must always be, or
  an extended search of nothing could be sent.

  Each list item is keyed by the token's :id rather than its place, so
  that taking a token away leaves what was typed in the ones after it."
  [ui attrs value-lists client? required? tokens]
  (let [rows (or (seq tokens) [(url/blank-token 1)])
        n    (count rows)]
    [:fieldset.tokens {:aria-label (i18n/tr ui "Extended search")}
     (for [[attr values] (sort value-lists)]
       [:datalist {:id (value-list-id attr)}
        (for [value values] [:option {:value value}])])
     [:ol
      (map-indexed (fn [i {:keys [id] :as token}]
                     (let [i (inc i)]
                       [:li {:replicant/key id}
                        (token-row ui attrs value-lists client?
                                   (and required? (or client? (= n 1) (< i n)))
                                   i token)]))
                   rows)]]))

(defn unit-label
  "What the unit of text `unit` a search is kept within is called, in
  `ui` (see dk.cst.corpus-probe.url/units)."
  [ui unit]
  (case unit
    "paragraph" (i18n/tr ui "paragraph")
    "text"      (i18n/tr ui "text")
    (i18n/tr ui "sentence")))

(defn within-control
  "The control choosing the unit of text a search of several tokens is
  kept within, in `ui`: a select over the dk.cst.corpus-probe.url/units
  with `within` chosen, the sentence when it names none (see
  dk.cst.corpus-probe.query/within)."
  [ui within]
  [:select {:name "within" :id "within"}
   (for [unit url/units]
     [:option {:value unit :selected (= unit (or within "sentence"))}
      (unit-label ui unit)])])

(defn match-option-label
  "What the `match` param value is called as the option of
  `match-control`, in `ui`, where it is read before the attribute in a
  sentence, `find whole word`: how much of the form is found."
  [ui match]
  (case match
    "prefix" (i18n/tr ui "start of")
    "suffix" (i18n/tr ui "end of")
    "infix"  (i18n/tr ui "part of")
    (i18n/tr ui "whole")))

(defn match-label
  "What the `match` param value is called, in `ui`: how much of the
  form a simple search must cover (see
  dk.cst.corpus-probe.query/match-op); the whole word for a value
  naming none."
  [ui match]
  (case match
    "prefix" (i18n/tr ui "start of word")
    "suffix" (i18n/tr ui "end of word")
    "infix"  (i18n/tr ui "part of word")
    (i18n/tr ui "whole word")))

(def match-values
  "The values of the match param, in display order: the whole form
  first, which is the one no URL carries."
  ["" "prefix" "suffix" "infix"])

(defn match-control
  "The control choosing how much of the form a simple search must
  cover, in `ui`: a select over the `match-values`, each named by
  `match-option-label`, with `match` chosen and the whole form when it
  names none; named for a screen reader only, since it stands in a
  sentence (see `search-form`).

  One control rather than a box for each end: two boxes both ticked
  meant any part of the form, and nothing said so."
  [ui match]
  [:select {:name "match" :aria-label (i18n/tr ui "match")}
   (for [value match-values]
     [:option {:value value :selected (= value (or match ""))}
      (match-option-label ui value)])])

(defn help
  "The search help, the hiccup `blocks` of its document (see
  dk.cst.corpus-probe.docs), standing where the results will once there
  are any, as a region named in `ui`; nil without a help document.

  It stands where the results will: help belongs in the empty answer
  space, not in the form. The document has a heading per form and no
  title, so the interface names the region.

  TODO: the help's heading used to be the h1 of the search page until
  an answer headed it, and now the page has no h1 until then. Does the
  empty page want one, and of what?"
  [ui blocks]
  (when (seq blocks)
    [:section.help {:aria-label (i18n/tr ui "Help")} blocks]))

(defn navigation-status
  "The live region reporting a routed navigation in flight in `ui`,
  which says so while `pending?` and holds nothing otherwise.

  Rendered whether or not there is anything to say, because a live region
  announces a change to what it holds: one created already full has no
  change to announce, and several screen readers say nothing at all. A
  <div> rather than a <p>, so that the empty one costs no margins.

  It sits with the query controls rather than with the results, because
  the wait starts at the submit button and the results it is about may
  not exist yet. Above 64rem the layout sets it between the query row
  and the answer, under that button."
  [ui pending?]
  [:div.status {:role "status"}
   ;; TODO: design this. A line of text arriving under the form is what
   ;; it says, not a thing anyone drew, and three questions are open:
   ;; where a reader is actually looking when they are waiting (the foot
   ;; of the rail is where the button is, but not where the answer will
   ;; be); whether it wants a minimum time on screen, since an answer at
   ;; 450ms still flashes past `dk.cst.corpus-probe.ui/pending-delay-ms`;
   ;; and whether waiting should say more than that it is waiting. The
   ;; metadata filter has the same decision pending (see
   ;; `filter-fieldset`), as does the count of a result still being made
   ;; (see `results-region`), and the three should be answered together.
   (when pending? [:p (i18n/tr ui "Loading …")])])

(defn form-query
  "The query the search form holds (see dk.cst.corpus-probe.query/of):
  that of its `params`, or, in the extended mode, of its `tokens` as the
  client keeps them (see dk.cst.corpus-probe.url/rows->params), kept
  within the unit the params name."
  [params tokens]
  (let [mode (url/mode params)]
    (query/of (if (= "extended" mode)
                (assoc (url/rows->params tokens)
                       :mode mode :within (:within params))
                params))))

(defn cqp-line
  "The CQP the extended form's `tokens` compile to (see `form-query`,
  with the `params`), under them in `ui`, so that a reader sees how the
  conditions' joins and repeats came out before a search is spent on
  them, and as they edit, since every control records itself in the
  state; nothing for no query. A paragraph rather than an output, whose
  implicit status role would have a screen reader read the string after
  every keystroke.

  TODO: is the line worth its place? The field's radio hands the same
  text to the field, with the unit, and switching back restores the
  tokens. Drop it if nobody reads it."
  [ui params tokens]
  (when-let [cqp (query/->cqp (form-query params tokens))]
    [:p.cqp (i18n/tr ui "As CQP") ": " [:code cqp]]))

(defn mode-label
  "What the query `mode` (see dk.cst.corpus-probe.url/modes) is called,
  in `ui`, as a word."
  [ui mode]
  (case mode
    "list"     (i18n/tr ui "List")
    "extended" (i18n/tr ui "Extended")
    "cqp"      "CQP"
    (i18n/tr ui "Default")))

(defn loss-sentence
  "What a change of form did to the query, `item` (see
  dk.cst.corpus-probe.query/loss), as a sentence in `ui`: that the
  extended form does not hold it."
  [ui [kind x]]
  (case kind
    :cqp  (list (i18n/tr ui (str "Extended cannot read CQP. "
                                 "The query is not kept:"))
                " " [:code x])
    :list (i18n/tr ui "A list of {n} words is not kept in Extended."
                   {:n (i18n/group-digits ui x)})))

(defn param-label
  "What the query param `k` a URL carried is called in `ui`, for the
  sentence naming the ones the mode did not read (see `switch-notice`):
  the query, the tokens, or the option's own label; nil for a key with
  no name."
  [ui k]
  (if (url/token-key? k)
    (i18n/tr ui "the tokens")
    (case k
      :q      (i18n/tr ui "the query")
      :in     (i18n/tr ui "attribute")
      :match  (i18n/tr ui "match")
      :ci     (i18n/tr ui "ignore case")
      :within (i18n/tr ui "within")
      nil)))

(defn switch-notice
  "What a change of the query's form to `form` could not keep, in `ui`,
  for the form's status line: the `loss` items (see
  dk.cst.corpus-probe.query/loss) as sentences, and the `unread` params
  a hand-written URL carried as one naming the mode, in the URL's own
  order. Nil when there is nothing to say, which is the line's empty
  state."
  [ui form loss unread]
  (let [sentences (concat
                   (map #(loss-sentence ui %) loss)
                   (when (seq unread)
                     [(i18n/tr ui "Not used in {form}: {params}."
                               {:form   (mode-label ui form)
                                :params (->> (sort-by url/rank unread)
                                             (keep #(param-label ui %))
                                             (distinct)
                                             (str/join ", "))})]))]
    (when (seq sentences)
      [:p (interpose " " sentences)])))

(defn search-form
  "The search form of `state`: over its `:folders` tree of corpus
  overviews, its metadata `:filter-controls` and the `:search-attrs` a
  simple search may match (see `attribute-control`), prefilled from its
  `:params` (:corpus, a vector of selected names, :mode, the form, :q,
  the field's text, :in :ci :match :within), submitted as GET to
  `action`, with the page's own `extra` hidden inputs.

  The query row comes first (see `query-field`, with how its text is
  read under it, see `reading-line`), or the tokens of an extended
  search (see `token-fieldset`, over the `:tokens` and `:value-lists` of
  `state`), then the boxes, in a wrapper of their own, which the wide
  layout makes a rail beside the answer while the query row stands
  above it, with room to type in. First everything that decides how the
  query is read, a box each: the form, with a status line for what a
  change of it could not keep (its `:switch`, see `switch-notice`), and
  the matching, a row each for what a simple search matches, how
  loosely, the unit of text a search of several tokens is kept within
  (see `within-control`) and the case, of those the mode reads. Then
  the scope of the search, the corpus chooser and the metadata filter.
  So the field the reader reaches for is the first control in the form,
  whatever the registry holds, and what qualifies what they typed is
  under their hand rather than past two disclosures.

  The form radios dispatch `:set-mode`, so choosing the extended form
  swaps the field for the tokens without a round trip. Which options are
  offered at all is the row of dk.cst.corpus-probe.url/fields for the
  mode the text is read in (see dk.cst.corpus-probe.url/shape), on both
  sides; without the client all of it is as the search was submitted.

  The query is required, except when the form is submitted from the
  frequency view (its `:view`), which counts every token of a blank one,
  and so is every token row of an extended search but the blank last
  one (see `token-fieldset`); the corpus chooser requires a corpus where
  the client runs. All of these are
  the browser's own checks, so missing input is reported before it is
  sent, in the browser's words, and the server's own answers stand for a
  request that never passed through the form.

  Wrapped in a <search> landmark; GET, so every search has a shareable URL
  and works without JavaScript. The form carries an id, so a control
  rendered outside it (the sort of the concordance, the grouping of the
  frequency table) still submits with it. The corpus chooser, the metadata
  filter, the modes and the matching are <fieldset> groups. No language
  is submitted with the search: which language the
  answer is worded in is the reader's own stored preference, not part of
  what they asked.

  Where the client runs, `navigation-status` follows the form inside the
  landmark, and the chooser and the metadata filter show what is chosen
  or everything there is to choose, by what `:lists` holds of each (see
  dk.cst.corpus-probe.ui/lists and
  dk.cst.corpus-probe.views.corpus/chooser)."
  [{:keys [ui view folders filter-controls search-attrs params tokens
           value-lists switch client? pending? lists filters-pending?]
    :as state}
   action extra]
  (let [{:keys [corpus q in ci match within]} params
        {:keys [corpora values]} lists
        form    (url/form params)
        mode    (url/mode params)
        ;; a control the mode does not read is taken away rather than
        ;; shown dead: a row of greyed controls is something to read past
        ;; before reaching one that can be used, and nothing is lost by
        ;; taking it away, since what the reader chose is held in the
        ;; params and comes back with the mode that reads it. A control
        ;; that is not there is not submitted either, so nothing about a
        ;; simple search rides along with a CQP one
        live?   (fn [k] (url/reads? mode k))
        button  [:button {:type "submit"} (i18n/trx ui "button" "Search")]]
    [:search
     [:form.search {:id form-id :method "get" :action action}
      extra
      ;; the query row: the field or the tokens, and the button, which
      ;; belongs against the field it submits rather than at the foot of
      ;; every control that qualifies it. In a wrapper of one kind whatever
      ;; the row holds, so that the group after it keeps its identity when
      ;; the row changes kind: measured, without it the switch back from
      ;; the tokens rebuilt the group, the radio the reader had pressed
      ;; included
      [:div.query
       (if (= form "extended")
         (list (token-fieldset ui search-attrs value-lists client?
                               (not= :frequencies view) tokens)
               [:p
                (when client?
                  (list [:button {:type "button" :on {:click [:add-token]}}
                         (i18n/tr ui "Add token")]
                        " "))
                button]
               (cqp-line ui params tokens))
         (let [line (reading-line ui params)]
           (list [:p (query-field ui q (not= :frequencies view)
                                  (when line reading-id))
                  " " button]
                 line)))]
      ;; one wrapper, which the wide layout makes a rail beside the answer
      [:div.rail
       ;; the query's box: its form, named by a legend like the boxes
       ;; beside it, so that they line up when they share a row
       [:fieldset.modes
        [:legend (i18n/trx ui "legend" "Query type")]
        [:p (interpose " "
                       (for [m url/forms]
                         [:label [:input {:type    "radio" :name "mode"
                                          :value   m
                                          :checked (= m form)
                                          :on      {:change [:set-mode m]}}]
                          (mode-label ui m)]))]
        ;; without the client a change of mode is a submit, which the field
        ;; a fresh form requires would refuse; this button submits without
        ;; that check, so a reader can leave an empty form for another mode.
        ;; Inside <noscript> for the reason `apply-button` is
        (when-not client?
          [:noscript
           [:p [:button {:type "submit" :formnovalidate true}
                (i18n/tr ui "Change mode")]]])
        ;; what a change of mode could not keep (see `switch-notice`):
        ;; rendered always, so that the live region exists before it fills,
        ;; and before everything a switch changes
        [:div.status {:role "status"}
         (switch-notice ui mode (:loss switch) (:unread switch))]]
       ;; the matching, read as a sentence: find how much of which
       ;; attribute, within which unit of text, which the words of a
       ;; simple search read as the tokens of an extended one do, and
       ;; the case under it. Only the parts the mode reads, and no box at
       ;; all for a mode that reads none of them: a CQP query writes all
       ;; four itself
       (when (some live? [:in :match :within :ci])
         [:fieldset.matching
          [:legend (i18n/trx ui "legend" "Scope")]
          (when (or (live? :match) (live? :in))
            [:p.find [:span (i18n/tr ui "find")]
             (when (live? :match) (match-control ui match))
             (when (live? :in) (attribute-control ui search-attrs in))])
          (when (live? :within)
            [:p.within [:label {:for "within"} (i18n/tr ui "in")]
             (within-control ui within)])
          (when (live? :ci)
            [:p.case
             [:label [:input {:type    "checkbox" :name "ci" :value "on"
                              :checked (some? ci)}]
              (i18n/tr ui "ignore case")]])])
       ;; marks a selection the reader actually made: without it, unticking
       ;; every corpus and submitting is indistinguishable from arriving
       ;; with no corpus named, which searches them all
       [:input {:type "hidden" :name "scope" :value "chosen"}]
       ;; each list's state is named for the chooser's options, and what
       ;; it holds beyond them the chooser ignores
       (corpus-views/chooser ui folders
                             (assoc corpora
                                    :selected (set corpus)
                                    :held     (into (set corpus)
                                                    (:unticked corpora))
                                    :client?  client?))
       (filter-fieldset ui filter-controls
                        (assoc values
                               :held     (into (pairs (:selected filter-controls))
                                               (:unticked values))
                               :pending? filters-pending?
                               :client?  client?))]]
     ;; only where the client runs: every other navigation is the
     ;; browser's own, and the browser reports those itself
     (when client? (navigation-status ui pending?))]))

(defn corpora-phrase
  "The corpus `names` in words in `ui`: the one name, or how
  many there were."
  [ui names]
  (if (= 1 (count names))
    (first names)
    (str (count names) " " (i18n/tr ui "corpora"))))

(defn hits-phrase
  "The number of hits `n` in words in `ui`."
  [ui n]
  (str (i18n/group-digits ui n) " "
       (i18n/trn ui "hit" "hits" n)))

(defn page-phrase
  "Where in a paged `result` the reader is, in `ui`: the page, and of how
  many once the result is counted (see `counting?`)."
  [ui {:keys [page pages] :as result}]
  (str (i18n/tr ui "page") " " (inc page)
       (when pages (str " " (i18n/tr ui "of") " " pages))))

(defn sample-phrase
  "That a result holds a random `sample` of the matches rather than all
  of them, in `ui`, over `corpora`; nil when it holds them all.

  The number is the sample asked for rather than the hits it came back
  with, the two differing wherever a corpus had fewer matches than that,
  and it is named as being per corpus over several, one sample being
  drawn in each (see dk.cst.corpus-probe.search/concordance!)."
  [ui sample corpora]
  (when sample
    (str (i18n/tr ui "a random sample of at most") " "
         (i18n/group-digits ui sample)
         (when (next corpora) (str " " (i18n/tr ui "per corpus"))))))

(defn query-mark
  "The query of `params` as the answer names it, in `ui`: a CQP query as
  the code it is, and so an extended search, as the CQP it compiles to
  (see `query-phrase`), a list as how many words it holds, and a simple
  search quoted, being a word spoken of rather than used."
  [ui params]
  (let [phrase (query-phrase ui params)]
    (case (url/mode params)
      ("cqp" "extended") [:code phrase]
      "list"             phrase
      [:q phrase])))

(defn asked?
  "True when the search `params` ask for anything (see
  dk.cst.corpus-probe.query/of). A search asking nothing counts every
  token, which only a frequency table wants."
  [params]
  (some? (query/of params)))

(defn hits-heading
  "What a search found, as the heading of its result in `ui`: how many
  hits, `size`, at least that many while `counting?`; every token when
  nothing was `asked?` of `params`. Not the query: the field above the
  answer holds it (see `question` for when it does not)."
  ([ui params size]
   (hits-heading ui params size false))
  ([ui params size counting?]
   (if (asked? params)
     (str (when counting? (str (i18n/tr ui "at least") " "))
          (hits-phrase ui size))
     (i18n/tr ui "All tokens"))))

(defn question
  "The query the result of `state` answered, named as `query-mark` names
  it, once the form above has moved on from it: retyped, or switched to
  a mode that could not keep it, so that the answer still says what it
  is of. Its `:asked` params are what ran (see
  dk.cst.corpus-probe.api/search-view-data); the form's query is read
  from its `:params` and `:tokens` (see `form-query`). Nil while the
  form holds the query, which then says it, and the answer names only
  how many."
  [ui {:keys [asked params tokens]}]
  (when (not= (query/of asked) (form-query params tokens))
    (query-mark ui asked)))

(defn counting?
  "True while the corpora of `result` are still being counted: some of
  them are `:remaining` (see dk.cst.corpus-probe.search/concordance!),
  and its size is the hits counted so far."
  [{:keys [remaining] :as result}]
  (boolean (seq remaining)))

(defn qualifiers
  "The question a `result` answered, less the query itself, as short
  phrases in `ui`, each naming what one control holds: the attribute a
  simple search of `params` matched and the part of the form, when not
  the usual ones and when the mode read them (see
  dk.cst.corpus-probe.url/reads?); the corpora searched, those still
  being counted among them; the metadata filter; the narrowings; the
  sample. For the line under the heading (see `results-region`), where
  the heading says what was found and this what was asked."
  [ui {:keys [in match] :as params}
   {:keys [counts size sample remaining] :as result}]
  (let [searched (concat (map :corpus (filter :size counts)) remaining)
        ;; an option the mode does not read may still ride in the params,
        ;; as memory for the form's disabled control; the search never saw
        ;; it, so the line must not say it did
        reads?   (partial url/reads? (url/mode params))]
    (remove nil?
            [(when (and (reads? :in) (not (contains? #{nil "" "word"} in)))
               (list (i18n/tr ui "attribute") " " [:code in]))
             (when (and (reads? :match) (not (str/blank? match)))
               (match-label ui match))
             (when (seq searched)
               (str (i18n/tr ui "in") " " (corpora-phrase ui searched)))
             (within-phrase ui (:filter result) (:patterns result))
             (subset-phrase ui (:subset result))
             (near-phrase ui (:near result))
             ;; a search that found nothing sampled nothing, and saying it
             ;; drew a sample of what it found reads as the reason it is
             ;; empty
             (when (pos? (or size 0))
               (sample-phrase ui sample searched))])))

(defn error-groups
  "The errors among the per-corpus `counts`, grouped by identical error:
  [[error [corpus ...]] ...] in first-seen order, so a query that fails the
  same way in every corpus is reported once."
  [counts]
  (let [failed (filter :error counts)]
    (for [error (distinct (map :error failed))]
      [error (map :corpus (filter #(= error (:error %)) failed))])))

(defn pager-links
  "The page links of a result around `position` (where in the sequence the
  reader is), labelled in `ui`; nil when neither
  `prev-href` nor `next-href` is in range.

  A list, so assistive technology can say how many options there are, and
  an absent direction is left out rather than held open by an empty
  element, which nothing positioned. The links carry the `rel` values
  browsers and crawlers use for sequential pages."
  [ui prev-href next-href position]
  (when (or prev-href next-href)
    [:ul.row.pager
     (when prev-href
       [:li [:a {:href prev-href :rel "prev"}
             (str "← " (i18n/tr ui "previous"))]])
     [:li position]
     (when next-href
       [:li [:a {:href next-href :rel "next"}
             (str (i18n/tr ui "next") " →")]])]))

(defn pagination
  "`pager-links` as a navigation landmark named in `ui`.

  Only one of a result's two pagers is a landmark: the APG asks each
  landmark of a repeated role to carry a name of its own, and two named
  Pagination cannot be told apart. The repeat below the table is the bare
  list, whose links stay operable and stay in the tab order."
  [ui prev-href next-href position]
  (when-let [links (pager-links ui prev-href next-href position)]
    [:nav.pagination {:aria-label (i18n/tr ui "Pagination")} links]))

(defn error-heading
  "What the error of `type` is called, in `ui`: the types this project
  reports itself, and CQP's own error for anything else, which is headed
  as such and carries its message."
  [ui type]
  (case type
    :timeout        (i18n/tr ui "The search did not finish in time")
    :no-corpus      (i18n/tr ui "No corpus selected")
    :no-texts       (i18n/tr ui "The corpus marks no texts")
    :unknown-corpus (i18n/tr ui "Unknown corpus")
    :rejected       (i18n/tr ui "Request rejected")
    :misaligned     (i18n/tr ui "Unreadable CQP output")
    :internal       (i18n/tr ui "Unexpected error")
    (i18n/tr ui "CQP error")))

(defn error-explanation
  "What the error of `type` means, in `ui`, for the types that carry no
  message of their own; nil for the rest, whose message says it."
  [ui type]
  (case type
    :no-corpus      (i18n/tr ui "Select at least one corpus to search.")
    :no-texts       (i18n/tr ui (str "Without a text attribute there is "
                                     "nothing to read as one text."))
    :unknown-corpus (i18n/tr ui "The registry has no corpus with that name.")
    :misaligned     (i18n/tr ui "CQP did not print the requested rows.")
    :internal       (i18n/tr ui (str "The search failed on the server. The "
                                     "server log has the details."))
    nil))

(defn not-cqp?
  "True when CQP's error `message` is the one a bare word in a CQP query
  gets: it is read as the name of a corpus or a query result, and
  refused as one there is none of."
  [message]
  (boolean (re-find #"Corpus ``.*'' is undefined" (str message))))

(defn error-body
  "The parts of an `error` under its heading in `ui`: the
  `corpora` it concerns, the explanation of a type that carries no message,
  what a bare word in CQP gets told (see `not-cqp?`), and cqp's own
  message verbatim, its `<--` position pointer included, as the sample
  output of another program."
  [ui {:keys [type message]} corpora]
  (list
   (when (seq corpora)
     [:p (str (i18n/tr ui "in") " ")
      (interpose ", " (map (fn [c] [:code c]) corpora))])
   (when-let [explanation (error-explanation ui type)]
     [:p explanation])
   (when (not-cqp? message)
     [:p (i18n/tr ui (str "CQP reads a bare word as the name of a query "
                          "result. To match a word, put it in quotation "
                          "marks."))])
   ;; the stylesheet scrolls this rather than letting cqp's column-aligned
   ;; pointer reflow, and a scroll container a keyboard cannot reach is
   ;; unreadable in the browsers that do not focus scrollers themselves
   (when message [:pre {:tabindex "0"} [:samp message]])))

(defn error-name
  "The heading naming `error` in `ui`: the type this project reports
  itself, or cqp's own error."
  [ui {:keys [type] :as error}]
  (error-heading ui type))

(defn error-section
  "An `error` map under a heading of its own in `ui`, naming
  the `corpora` it concerns when given.

  No live region: every error here arrives by a full page reload, where a
  region that is already populated announces nothing, while the alert role
  costs the section its own semantics and flattens its heading. The reader
  reaches the error because the search lands on it (see `result-section`).

  An h2, since it sits inside a results region headed by the page's own
  h1."
  [ui error corpora]
  [:section.error
   [:h2 (error-name ui error)]
   (error-body ui error corpora)])

(defn download-links
  "Links downloading the current table in each format of `hrefs` (format
  keyword to URL), with `note` (when given) qualifying what the download
  holds; nil without hrefs, worded in `ui`. The response itself
  asks to be saved (its Content-Disposition), so the links carry no
  download attribute."
  [ui hrefs note]
  (when (seq hrefs)
    [:p.downloads (i18n/tr ui "Download")
     (when note (str " " note))
     ": "
     (interpose " · "
                (for [[format href] (sort hrefs)]
                  [:a {:href href} (str/upper-case (name format))]))]))

(defn searched?
  "True when any corpus of `result` could be searched, or is still being
  counted (see `counting?`), so its counts are an answer rather than a
  report of failure."
  [{:keys [counts] :as result}]
  (boolean (or (some :size counts) (counting? result))))

(defn view-label
  "What the result view `k` is called, in `ui` (see
  dk.cst.corpus-probe.api/result-views): the concordance is KWIC, as
  CWB and KORP call it, expanded but not linked, since the label is
  itself a link."
  [ui k]
  (case k
    :kwic        (layout/term ui :kwic false)
    :frequencies (i18n/tr ui "Frequencies")
    (name k)))

(defn view-switch
  "The switch between the views of one result in `ui`: each of
  `hrefs` ([view url], see dk.cst.corpus-probe.api/view-hrefs) as a
  link named by `view-label`, `view` marked as the one being shown; nil
  without hrefs.

  Links rather than an ARIA tablist: each view is its own URL and its own
  question put to CQP, so following one is a navigation, which is what a
  link means. A tablist would promise a panel that is already loaded."
  [ui view hrefs]
  (when (seq hrefs)
    [:nav.views {:aria-label (i18n/tr ui "Result view")}
     [:ul.row
      (for [[k href] hrefs]
        [:li [:a (cond-> {:href href}
                   (= k view) (assoc :aria-current "page"))
              (view-label ui k)]])]]))

(defn result-heading
  "The heading naming the results region in `ui`: what the concordance
  `result` of the search `params` describe found (see `hits-heading`)
  when any corpus could be searched, else the name of the error that
  came instead, so a search that failed everywhere is not announced as a
  count of nothing."
  [ui params {:keys [counts size] :as result} error]
  (if (searched? result)
    (hits-heading ui params size (counting? result))
    (error-name ui (or error (some :error counts)))))

(defn results-region
  "The outcome of a search in `state` under `heading`, as a region named by
  that heading and focusable, so a GET search can land on it.

  Every view of a result shares this: a header holding the heading, the
  `subheading` phrases (see `qualifiers`) under it and the switch
  between the views at its end; a status line while the result is still
  being counted (see `counting?`); the error that replaced the result or
  the errors of individual corpora; and then `body`, the view's own
  content. The two views differ only in what they say about the same
  hits, so they differ only in what they pass here.

  The heading is the page's h1: the search page has no other, so what a
  search found, or why it found nothing, is what the page is about. It
  is the answer alone, how many: the query is in the field above, which
  is the page's headline, and the answer repeats it only once the form
  has moved on from it (see `question`), at the head of the line under
  the heading. That line is the rest of the question, grouped with the
  heading as heading and subheading, and the region is named by the
  heading alone, so a screen reader landing here hears the count and not
  the whole question, which the controls below restate anyway.

  Marked busy while a navigation is `pending?`, since until that one
  lands what this holds is the answer to the question before it."
  [{:keys [ui view view-hrefs result error pending?] :as state}
   heading subheading body]
  [:section.result (cond-> {:id              url/results-id
                            :tabindex        "-1"
                            :aria-labelledby "results-heading"}
                     ;; while the next question is in flight these hits
                     ;; are still the previous one's answer, and nothing
                     ;; about them says so
                     pending? (assoc :aria-busy "true"))
   [:header
    [:hgroup
     [:h1 {:id "results-heading"} heading]
     (when-let [phrases (seq (remove nil? (cons (question ui state)
                                                subheading)))]
       [:p (interpose " · " phrases)])]
    (view-switch ui view view-hrefs)]
   ;; always rendered, and before anything whose kind can change (see
   ;; `navigation-status`)
   [:div.status {:role "status"}
    (when (counting? result)
      [:p (str (i18n/tr ui "Counting hits in") " "
               (corpora-phrase ui (:remaining result)) " …")])]
   (when error (error-body ui error nil))
   (for [[e corpora] (error-groups (:counts result))]
     (error-section ui e corpora))
   body])

(defn result-section
  "The concordance view of the search in `state`: when any corpus could be
  searched and found something, the sort, context and sample controls
  with the near control behind its disclosure (see `view-controls`), the
  pagination above and below the table, the concordance with its
  `:expanded` hits, its `:langs` and the per-corpus counts that head its
  row groups, then the download links (`:export-hrefs`, exports holding
  at most `:export-limit` hits), all worded in the state's `:ui` and
  wrapped in the shared `results-region`. The result answers the params the search
  was `:asked` with, not the form's `:params`, which the client's form
  leaves behind at a change of mode."
  [{:keys [ui sort-modes asked result error langs expanded client?
           export-hrefs export-limit prev-href next-href]
    :as state}]
  (let [{:keys [counts hits size]} result
        position (when result (page-phrase ui result))]
    (results-region
     state
     (result-heading ui asked result error)
     (qualifiers ui asked result)
     (when (searched? result)
       ;; a search that found nothing has nothing to page, download or
       ;; count: the table would be a header over no rows and the exports
       ;; header-only files. A result emptied by the word its hits had
       ;; to be near keeps that one control, or the reader could not take
       ;; the word away again
       (if (zero? size)
         (list
          (when (:near result)
            (view-controls ui client? nil (near-control ui (:near result))
                           true))
          [:p (i18n/tr ui "No hits.")])
         (list
          (view-controls ui client?
                         (list (sort-control ui sort-modes (:sort asked))
                               " "
                               (context-control ui (:context result))
                               " "
                               (sample-control ui (:sample result)))
                         (near-control ui (:near result))
                         (:near result))
          (pagination ui prev-href next-href position)
          (kwic/concordance hits {:caption  (layout/term ui :kwic false)
                                  :ui       ui
                                  :langs    langs
                                  :counts   counts
                                  :expanded expanded
                                  :client?  client?
                                  :cursor   (:cursor state)})
          (pager-links ui prev-href next-href position)
          ;; what to do next with these hits, so it follows them: reading
          ;; the concordance is the task, taking it elsewhere is the one
          ;; after
          (download-links ui export-hrefs
                          (when (and export-limit (> size export-limit))
                            (str (i18n/tr ui "the first") " "
                                 (i18n/group-digits ui export-limit) " "
                                 (i18n/trn ui "hit" "hits"
                                           export-limit))))))))))

(defn attribute-list
  "A definition list of the attribute map `m`, each value rendered by
  `attribute-value`."
  [m]
  [:dl (mapcat (fn [[k v]] [[:dt (name k)] [:dd (attribute-value k v)]]) m)])

(defn detail-group
  "A titled group of attributes `m` in the sidebar, or nil when empty."
  [title m]
  (when (seq m)
    [:section
     [:h3 title]
     (attribute-list m)]))

(defn sidebar
  "The token inspection panel: what the concordance's cursor is on, in
  `ui`, from `selected` (its :token, :structs, :corpus and the :cpos and
  :matchend of its hit, which the link to the whole text takes); nil
  while nothing is selected.

  Above the rail's breakpoint it takes the query column, so it sits beside
  the hits it describes without narrowing them; below it, it is a sheet at
  the foot of the viewport.

  It is not given focus when it opens: the cursor stays on the token so
  the arrow keys keep moving, and the panel describes whatever the
  cursor is on. That is why it is not a popover, which would put itself
  in the top layer, out of the grid, and want focus of its own. Escape
  closes it from the concordance. It can take focus, so that a click
  anywhere in it lands focus in the panel rather than on the page, and
  it reports focus leaving it, since the client closes it once focus
  has left both it and the concordance (see
  dk.cst.corpus-probe.ui/leave-concordance!).

  The group titles are in `ui`; the attribute names inside them are the
  corpus's own."
  [ui {:keys [token structs corpus cpos matchend] :as selected}]
  (when selected
    [:aside.sidebar {:aria-label (i18n/tr ui "Token details")
                     :tabindex   "-1"
                     :on         {:focusout [:leave-concordance]}}
     [:h2 (i18n/tr ui "Token details")]
     [:button {:type "button" :on {:click [:close]}} (i18n/tr ui "Close")]
     (detail-group (i18n/tr ui "Token") (dissoc token :open :close))
     (detail-group (i18n/tr ui "Text") structs)
     (when (and corpus cpos)
       [:p [:a {:href (url/text corpus cpos matchend)}
            (i18n/tr ui "Read the whole text")]])
     (when corpus
       [:section
        [:h3 (i18n/tr ui "Corpus")]
        [:p [:a {:href (url/corpus corpus)} [:code corpus]]]])]))
