(ns dk.cst.corpus-probe.views.page
  "Hiccup for the search page: the result summary, per-corpus counts,
  pagination and the inspection sidebar, plus the pieces shared with the
  frequency page: the search form, the error sections and the download
  links.

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
            [dk.cst.corpus-probe.views.layout :as layout]))

(def form-id
  "The id of the search form, so a control that acts on a result can sit
  beside the result and still submit the search that produced it."
  "search-form")

(defn sort-label
  "What the sort mode `value` (see dk.cst.corpus-probe.commands/sort-modes)
  is called, in `ui`; a mode naming a positional attribute (see
  dk.cst.corpus-probe.commands/sort-attr) is the match by that attribute.

  Naming them here rather than in the query namespace keeps the CQP
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
  "One metadata value `m` of attribute `attr` as a filter entry: a checkbox
  named for the attribute's filter param, checked when `chosen` holds its
  value, and, in `ui`, how many regions carry it, when known: a
  chosen value the corpora no longer offer has no count.

  A value the filter box has hidden keeps its checkbox in the document,
  for the reason a filtered-out corpus does: a box the form cannot see is
  part of a filter dropped without anyone saying so."
  [ui attr chosen {:keys [value total hidden?] :as m}]
  [:li (cond-> {} hidden? (assoc :hidden true))
   [:label
    [:input {:type    "checkbox"
             :name    (str filter-prefix (name attr))
             :value   value
             :checked (contains? chosen value)
             :on      {:change [:toggle-filter-values [attr [value]]]}}]
    " " (attribute-value attr value)
    (when total
      (list " " [:data {:value (str total)}
                 (str (i18n/group-digits ui total) " "
                      (i18n/trn ui "region" "regions" total))]))]])

(defn attr-rows
  "The rows to offer for attribute `attr`: its listed `rows` followed by
  the `chosen` values missing from them, so a selection is never lost on
  resubmit."
  [rows chosen]
  (let [listed (set (map :value rows))]
    (into (vec rows)
          (for [value (sort chosen) :when (not (listed value))]
            {:value value}))))

(defn narrow-attrs
  "`attrs` (each with its rows already prepared) with every value that
  does not answer `q` marked `:hidden?`, and an attribute left with
  nothing showing marked too.

  An attribute whose own name answers `q` keeps all of its values, so
  naming an attribute is a way of asking for its values rather than a way
  of finding none. Marked rather than dropped, so that what a reader has
  chosen goes on being submitted while they look for something else."
  [q attrs]
  (mapv (fn [{:keys [name rows] :as attr}]
          (let [whole? (controls/answers? q name)
                rows   (mapv #(cond-> %
                                (not (or whole? (controls/answers? q (:value %))))
                                (assoc :hidden? true))
                             rows)]
            (cond-> (assoc attr :rows rows)
              (every? :hidden? rows) (assoc :hidden? true))))
        attrs))

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
  the server, which reads no number out of it."
  [ui attr rows pattern [from to :as bounds]]
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
    [:p.pattern
     [:label (i18n/tr ui "pattern") " " (field "fp." pattern {:type "search"})]
     (when (numeric-values? rows)
       (list " "
             [:label (i18n/tr ui "from") " " (bound "ff." from)]
             " "
             [:label (i18n/tr ui "to") " " (bound "ft." to)]))]))

(defn filter-details
  "The disclosure of one prepared metadata attribute (its `:name`, its
  `:rows` and whether the filter has left it `:hidden?`) in the filter
  fieldset, with what is in force for that attribute, the `:chosen`
  values, the `:pattern` and the `:range` (see `pattern-row`), and,
  where `client?`, a control taking every value showing.

  Closed whatever is chosen, its summary counting the values showing
  (see dk.cst.corpus-probe.views.controls/entry-count) and the selection,
  and saying when a pattern or range is in force: one attribute may carry
  hundreds of values, and reopening them on every resubmit grew the form
  exactly while the reader was refining it. The wording is in `ui`."
  [ui client? {attr :name :keys [rows hidden?]}
   {:keys [chosen pattern] bounds :range}]
  ;; a set even when nothing is chosen, since it is read as a predicate
  (let [chosen  (set chosen)
        showing (mapv :value (remove :hidden? rows))]
    (controls/toggled
     (when client?
       (controls/select-all (str (i18n/tr ui "All values of") " " (name attr))
                            showing chosen
                            [:toggle-filter-values [attr showing]]))
     [:details (cond-> {} hidden? (assoc :hidden true))
      [:summary [:code (name attr)] " "
       (controls/entry-count (count showing))
       (when (seq chosen)
         (str " · " (count chosen) " " (i18n/tr ui "selected")))
       (when (some #(not (str/blank? %)) (cons pattern bounds))
         (str " · " (i18n/tr ui "pattern")))]
      (pattern-row ui attr rows pattern bounds)
      [:ul (map (partial filter-item ui attr chosen) rows)]])))

(defn value-count
  "How many metadata values are chosen across every attribute of
  `selected`, a map of attribute to the set chosen under it."
  [selected]
  (reduce + (map count (vals selected))))

(defn clear-toggle
  "The control emptying the whole metadata filter, over the prepared
  `attrs` and the `selected` values, in `ui`.

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
  [ui attrs selected]
  (let [items (for [{attr :name :keys [rows]} attrs
                    {:keys [value]} rows]
                [attr value])]
    (controls/select-all (i18n/tr ui "Clear filter")
                         items
                         (fn [[attr value]]
                           (contains? (get selected attr) value))
                         [:clear-filter]
                         {:clear-only? true})))

(defn filter-fieldset
  "The metadata filter fieldset of the search form from `filters` (see
  dk.cst.corpus-probe.frequency/filter-options!); nil without metadata.

  One disclosure over the whole filter, counting the values chosen across
  every attribute, so a corpus with forty annotated attributes is one line
  rather than forty. Inside it, a disclosure per listed attribute
  (`:attrs`) holds a pattern row and a checkbox per value (see
  `filter-details`), followed by one per attribute the list lacks but
  the `:selected` values, the `:patterns`, the `:ranges` or the
  `:unlisted` name, with the pattern row alone, then a note naming the
  `:unlisted` attributes, all worded in `ui`. `:selected` maps each
  attribute to the set of chosen values, `:patterns` to the pattern in
  force and `:ranges` to the [from to] in force.

  Where `:client?`, it carries the same three controls the corpus chooser
  does, from the same namespace: a box narrowing it to the attributes and
  values answering `:filter`, a control per attribute taking every value
  showing, and one beside the whole fieldset (see `clear-toggle`).

  The count is of `:selected`, which is live: it follows the boxes as the
  reader ticks them, rather than saying what the last search was for.
  What is open follows `:served`, the selection the page arrived with,
  until `:open?` says the reader has opened or shut it themselves, for the
  reason the corpus chooser's does: a count that opened and shut the
  fieldset as it changed would be moving the controls while they were
  being used.

  Which attributes there are to filter by depends on the corpora
  selected, and only the server knows: `:pending?` marks the fieldset busy
  while the client is fetching them for a selection that has changed."
  [ui {:keys [attrs unlisted selected patterns ranges] :as filters}
   {:keys [served open? pending? client?] q :filter}]
  (when (or (seq attrs) (seq unlisted) (seq selected))
    (let [listed    (set (map :name attrs))
          n         (value-count selected)
          prepared  (concat
                     (for [{attr :name :keys [rows]} attrs]
                       {:name attr :rows (attr-rows rows (get selected attr))})
                     (for [attr  (sort (distinct (concat (keys selected)
                                                         (keys patterns)
                                                         (keys ranges)
                                                         unlisted)))
                           :when (not (listed attr))]
                       {:name attr :rows (attr-rows [] (get selected attr))}))
          filtering (not (str/blank? q))
          shown     (cond->> prepared
                      filtering (narrow-attrs (str/lower-case q)))
          nothing-found? (and filtering (every? :hidden? shown))]
      [:fieldset.filters
       [:legend (layout/term ui :metadata false)]
       (when client?
         (controls/filter-box "value-filter" (i18n/tr ui "Filter") q
                              [:filter-values]))
       (when client?
         (controls/filter-status (when nothing-found?
                                   (i18n/tr ui "No values found."))))
       (controls/toggled
        (when client? (clear-toggle ui prepared selected))
       ;; TODO: a visible in-flight treatment. aria-busy says it to a
       ;; screen reader and nothing says it to anyone else; the
       ;; role="status" pattern of `navigation-status` is the obvious one
       ;; to reach for when we decide what it should look like
       [:details (cond-> {:open (or filtering
                                    (if (some? open?)
                                      open?
                                      (pos? (value-count served))))
                          :on   {:toggle [:set-filters-open]}}
                   pending?       (assoc :aria-busy "true")
                   ;; nothing in it answers, so the message below stands
                   ;; where it was; hidden rather than dropped, because
                   ;; its checkboxes are what a search submits
                   nothing-found? (assoc :hidden true))
        [:summary (if (zero? n)
                    (i18n/tr ui "None selected")
                    (str n " " (i18n/tr ui "selected")))]
        (for [{attr :name :as m} shown]
          (filter-details ui client? m {:chosen  (get selected attr)
                                        :pattern (get patterns attr)
                                        :range   (get ranges attr)}))
        ;; a caveat about the control rather than part of it, which is
        ;; what <small> is for: these attributes are not on offer here
        (when (seq unlisted)
          [:p [:small (i18n/tr ui "Too many values to list: ")
               (interpose ", "
                          (map (fn [attr] [:code (name attr)]) unlisted))]])])])))

(defn query-example
  "The example query shown for `mode`, in `ui`: the modes take different
  input, so one example cannot serve them all."
  [ui mode]
  (case mode
    "cqp"  (i18n/tr ui "\"x\" or [lemma = \"x\"]")
    "list" (i18n/tr ui "one word per line")
    (i18n/tr ui "one word, or several words in order")))

(defn list-words
  "The words of the list `q` a list-mode search asked for, as
  dk.cst.corpus-probe.query/of reads them: one per line, or several to
  a line."
  [q]
  (remove str/blank? (str/split (str q) #"\s+")))

(defn query-phrase
  "The query of `params` in words for a title, in `ui`: the query as
  typed, or, for a list, how many words it holds, a title being one
  line and a list not, or, for an extended search, the CQP its tokens
  compiled to (its :cqp, see dk.cst.corpus-probe.api/search-view-data),
  which is what CQP mode shows too."
  [ui {:keys [q mode cqp]}]
  (case mode
    "list"     (let [n (count (list-words q))]
                 (str n " " (i18n/trn ui "word" "words" n)))
    "extended" cqp
    q))

(defn one-line
  "The query `q` on one line: each line break, with the space around it,
  as one space, and none at either end. What a search box holds, since
  it cannot hold a line break and drops one rather than keep it: a list
  typed one word per line and submitted under another mode arrives with
  its line breaks."
  [q]
  ;; TODO: the citation of such a search still carries the line breaks,
  ;; url/canonical keeping q as it arrived, while the box resubmits it
  ;; with spaces; the printer of the query value will cite the words
  (str/trim (str/replace (str q) #"\s*[\r\n]+\s*" " ")))

(defn query-field
  "The query field of the search form in `ui`, holding `q`: a search
  box, on one line (see `one-line`), or, in list `mode`, a text area
  taking one word per line. Both are named q, so either submits the
  query, and both carry the one id the client finds the field by.
  Neither has a visible label: a field with a search button beside it
  needs none to say what it is, so the name it keeps is the one only a
  screen reader reads.

  Required when `required?`: a search of nothing is then reported by the
  browser before it is sent, rather than answered with the guide again.
  The caller says when a blank query means something (see
  `search-form`)."
  [ui mode q required?]
  (let [attrs {:id           "q"
               :name         "q"
               :aria-label   (i18n/tr ui "Query")
               :placeholder  (query-example ui mode)
               :autocomplete "off"
               :spellcheck   "false"
               :required     required?}]
    (if (= mode "list")
      ;; the text is the element's content: a text area has no value
      ;; attribute for a document to carry it in
      [:textarea (assoc attrs :rows 4) (or q "")]
      [:input (assoc attrs :type "search" :value (one-line q))])))

(defn attribute-options
  "The options of a select over the positional `attrs` (attribute
  keywords, word first) with `selected` (a string, word when blank)
  chosen.

  An attribute the list lacks is offered after them, so a hand-written
  URL shows what it searches rather than something else."
  [attrs selected]
  (let [selected (if (str/blank? selected) "word" selected)
        names    (map name attrs)
        offered  (cond-> names
                   (not (some #{selected} names)) (concat [selected]))]
    (for [n offered]
      [:option {:value n :selected (= n selected)} n])))

(defn attribute-control
  "The control choosing which positional attribute a simple search
  matches, in `ui`: a select over `attrs` with `selected` chosen (see
  `attribute-options`), `disabled?` while the query names its own
  attributes."
  [ui attrs selected disabled?]
  [:label (i18n/tr ui "attribute") " "
   [:select {:name "in" :disabled disabled?}
    (attribute-options attrs selected)]])

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
  client they are as the search was submitted. The client re-renders the
  token when an operator or an attribute changes, the one to disable,
  the other to switch the value list."
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
                       :disabled   dead?}
              (for [j url/joins]
                [:option {:value j :selected (= j (or join "and"))}
                 (case j "or" (i18n/tr ui "or") (i18n/tr ui "and"))])]
             " "))
     [:select {:name       (param :attr)
               :aria-label (i18n/tr ui "attribute")
               :disabled   any?
               :on         {:change [:set-condition [i id :attr]]}}
      (attribute-options attrs attr)]
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
                      :disabled     any?}
               (contains? value-lists attr*)
               (assoc :list (value-list-id attr*)))]
     " "
     [:label [:input {:type     "checkbox" :name (param :ci) :value "on"
                      :checked  (some? ci)
                      :disabled any?}]
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
  `client?`, buttons adding a condition and taking the token away.

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
                 :min  0 :max 99}]]
       " "
       [:label (i18n/tr ui "to") " "
        [:input {:type "number" :name (param :max) :value (or hi "1")
                 :min  0 :max 99}]]]
      " "
      [:label [:input {:type    "checkbox" :name (param :start) :value "on"
                       :checked (some? start)}]
       (i18n/tr ui "sentence start")]
      " "
      [:label [:input {:type    "checkbox" :name (param :end) :value "on"
                       :checked (some? end)}]
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
  them in (see dk.cst.corpus-probe.api/token-fields), so a reader
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
  "The control choosing the unit of text an extended search of several
  tokens is kept within, in `ui`: a select over the
  dk.cst.corpus-probe.url/units with `within` chosen, the sentence when
  it names none (see dk.cst.corpus-probe.query/within)."
  [ui within]
  [:label (i18n/tr ui "within") " "
   [:select {:name "within"}
    (for [unit url/units]
      [:option {:value unit :selected (= unit (or within "sentence"))}
       (unit-label ui unit)])]])

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
  `match-label`, with `match` chosen and the whole form when it names
  none, `disabled?` while the query is CQP, which writes its own.

  One control rather than a box for each end: two boxes both ticked
  meant any part of the form, and nothing said so."
  [ui match disabled?]
  [:label (i18n/tr ui "match") " "
   [:select {:name "match" :disabled disabled?}
    (for [value match-values]
      [:option {:value value :selected (= value (or match ""))}
       (match-label ui value)])]])

(defn heading-id
  "The id of the first heading among the hiccup `blocks` of a document,
  which is the name the document gives itself; nil when none carries
  one."
  [blocks]
  (some (fn [[tag attrs]]
          (when (and (re-matches #"h[1-6]" (name tag)) (map? attrs))
            (:id attrs)))
        (filter vector? blocks)))

(defn guide
  "The search guide, the hiccup `blocks` of its document (see
  dk.cst.corpus-probe.docs), standing where the results will once there
  are any, as a region named by the guide's own heading; nil without a
  guide.

  It stands where the results will: help belongs in the empty answer
  space, not in the form."
  [blocks]
  (when (seq blocks)
    [:section.help {:aria-labelledby (heading-id blocks)} blocks]))

(defn navigation-status
  "The live region reporting a routed navigation in flight in `ui`,
  which says so while `pending?` and holds nothing otherwise.

  Rendered whether or not there is anything to say, because a live region
  announces a change to what it holds: one created already full has no
  change to announce, and several screen readers say nothing at all. A
  <div> rather than a <p>, so that the empty one costs no margins.

  It sits with the query controls rather than with the results, because
  the wait starts at the submit button and the results it is about may
  not exist yet. Above 64rem those controls are a sticky rail, so it
  stays in view while a page of hits is fetched too."
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

(defn search-form
  "The search form of `state`: over its `:folders` tree of corpus
  overviews, its metadata `:filter-controls` and the `:search-attrs` a
  simple search may match (see `attribute-control`), prefilled from its
  `:params` (:corpus, a vector of selected names, :q :mode :in :ci
  :match), submitted as GET to `action`, with the page's own `extra`
  hidden inputs.

  The query field comes first (see `query-field`), or the tokens of an
  extended search (see `token-fieldset`, over the `:tokens` and
  `:value-lists` of `state`) with the unit they are kept within (see
  `within-control`), then everything that decides how it is read, in
  one group:
  the mode, under it the options that only a simple query or a list has,
  what it matches on one row and how loosely on the next.
  Then the scope of the search, the corpus chooser and the metadata
  filter, each behind one disclosure. So the field the reader reaches for
  is the first control in the form, whatever the registry holds, and what
  qualifies what they typed is under their hand rather than past two
  disclosures.

  The query example is the placeholder of the mode in `:params`, and the
  mode radios dispatch `:set-mode`, so choosing a mode swaps the example
  and the shape of the field and disables the simple options without a
  round trip; without the client all three are as the search was
  submitted.

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
  filter and the query options are <fieldset> groups; inside the last, the
  mode and the options it governs are a named group each without a box of
  their own. No language is submitted with the search: which language the
  answer is worded in is the reader's own stored preference, not part of
  what they asked.

  Where the client runs, `navigation-status` follows the form inside the
  landmark, so a reader learns that their question is in flight beside
  the button they asked it with, and `:served-corpus`, the corpora this
  page was served for, decides which folders of the chooser start open
  while `:params` follows what the reader is choosing now."
  [{:keys [ui view folders filter-controls search-attrs params tokens
           value-lists client? pending? served-corpus served-filter
           corpus-filter value-filter chooser-open? filters-open?
           filters-pending?]
    :as state}
   action extra]
  (let [{:keys [corpus q mode in ci match within]} params
        ;; a CQP query and an extended search name their own attributes,
        ;; so the simple options mean nothing to them
        simple? (not (#{"cqp" "extended"} mode))
        button  [:button {:type "submit"} (i18n/trx ui "button" "Search")]]
    [:search
     [:form.search {:id form-id :method "get" :action action}
      extra
      ;; the button belongs against the field it submits, not at the foot
      ;; of every control that qualifies it
      (if (= mode "extended")
        (list (token-fieldset ui search-attrs value-lists client?
                              (not= :frequencies view) tokens)
              [:p (within-control ui within)]
              [:p
               (when client?
                 (list [:button {:type "button" :on {:click [:add-token]}}
                        (i18n/tr ui "Add token")]
                       " "))
               button])
        [:p (query-field ui mode q (not= :frequencies view)) " " button])
      ;; one group: everything here qualifies the query above it, and two
      ;; boxes said that twice. A row each, so the mode a reader is in
      ;; does not run into the options it decides the meaning of.
      ;;
      ;; The options are disabled rather than taken away when they mean
      ;; nothing: a reader who looks at CQP and comes back finds what they
      ;; had ticked still ticked, the form does not change height under
      ;; them, and a disabled control is not submitted, so nothing about a
      ;; simple search rides along with a CQP one.
      ;;
      ;; Named by a legend like the boxes beside it, so that the three
      ;; line up when they share a row; the word is the query's, since
      ;; everything in the box qualifies it
      [:fieldset.query-options
       [:legend (i18n/trx ui "legend" "Query")]
       ;; the radios are still a group of their own, and still named:
       ;; a fieldset is not the only thing that can say so
       [:p {:role "radiogroup" :aria-label (i18n/tr ui "Query mode")}
        [:label [:input {:type    "radio" :name "mode" :value "simple"
                         :checked (not (#{"cqp" "list" "extended"} mode))
                         :on      {:change [:set-mode "simple"]}}]
         (i18n/tr ui "Simple")]
        " "
        [:label [:input {:type    "radio" :name "mode" :value "list"
                         :checked (= mode "list")
                         :on      {:change [:set-mode "list"]}}]
         (i18n/tr ui "List")]
        " "
        [:label [:input {:type    "radio" :name "mode" :value "extended"
                         :checked (= mode "extended")
                         :on      {:change [:set-mode "extended"]}}]
         (i18n/tr ui "Extended")]
        " "
        ;; an abbreviation and no link: the click on a label belongs to
        ;; its radio, and the glossary is in the masthead
        [:label [:input {:type    "radio" :name "mode" :value "cqp"
                         :checked (= mode "cqp")
                         :on      {:change [:set-mode "cqp"]}}]
         (layout/term ui :cqp false)]]
       ;; two rows: what a simple search matches, then how loosely. One
       ;; row of four ran the attribute into the options it governs
       [:div {:role "group" :aria-label (i18n/tr ui "Simple-search options")}
        [:p (attribute-control ui search-attrs in (not simple?))]
        [:p
         (match-control ui match (not simple?))
         " "
         [:label [:input {:type "checkbox" :name "ci" :value "on"
                          :checked  (some? ci)
                          :disabled (not simple?)}]
          (i18n/tr ui "ignore case")]]]]
      ;; marks a selection the reader actually made: without it, unticking
      ;; every corpus and submitting is indistinguishable from arriving
      ;; with no corpus named, which searches them all
      [:input {:type "hidden" :name "scope" :value "chosen"}]
      ;; names the mode this form was rendered in, so that a submit whose
      ;; radio was changed can read the query field as it was typed (see
      ;; dk.cst.corpus-probe.api/token-fields); no URL cites it
      [:input {:type "hidden" :name "from" :value (url/mode params)}]
      (corpus-views/chooser ui folders
                            {:selected (set corpus)
                             :served   (set (or served-corpus corpus))
                             :client?  client?
                             :filter   corpus-filter
                             :open?    chooser-open?})
      (filter-fieldset ui filter-controls
                       {:served   served-filter
                        :open?    filters-open?
                        :pending? filters-pending?
                        :client?  client?
                        :filter   value-filter})]
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
  "The query of `params` as a heading names it, in `ui`: a CQP query as
  the code it is, and so an extended search, as the CQP it compiled to
  (see `query-phrase`), a list as how many words it holds, and a simple
  search quoted, being a word spoken of rather than used."
  [ui {:keys [q mode cqp] :as params}]
  (case mode
    "cqp"      [:code q]
    "extended" [:code cqp]
    "list"     (query-phrase ui params)
    [:q q]))

(defn asked?
  "True when the search `params` ask for anything (see
  dk.cst.corpus-probe.query/of). A search asking nothing counts every
  token, which only a frequency table wants."
  [params]
  (some? (query/of params)))

(defn hits-heading
  "What a search found, as the heading of its result in `ui`: how many
  hits, `size`, for the query of `params` (see `query-mark`), at least
  that many while `counting?`; every token when nothing was `asked?`."
  ([ui params size]
   (hits-heading ui params size false))
  ([ui params size counting?]
   (if (asked? params)
     (concat (when counting? [(i18n/tr ui "at least") " "])
             (list (hits-phrase ui size) " " (i18n/tr ui "for") " "
                   (query-mark ui params)))
     (i18n/tr ui "All tokens"))))

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

(defn counts-table
  "The per-corpus hit `counts` of a search over several corpora as a table,
  each corpus linking to its info page; a corpus whose query failed shows
  no count (its error is reported separately). The wording is in `ui`."
  [ui counts]
  [:table.counts
   [:caption (i18n/tr ui "Hits per corpus")]
   [:thead
    [:tr [:th {:scope "col"} (i18n/tr ui "corpus")]
     ;; a column heading takes the plural form of what it counts
     [:th {:scope "col"} (i18n/trn ui "hit" "hits" 2)]]]
   [:tbody
    (for [{:keys [corpus size error]} counts]
      [:tr
       [:th {:scope "row"}
        [:a {:href (url/corpus corpus)} [:code corpus]]]
       [:td.n (if error
                [:em (i18n/tr ui "error")]
                (i18n/group-digits ui size))]])]])

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

(defn error-body
  "The parts of an `error` under its heading in `ui`: the
  `corpora` it concerns, the explanation of a type that carries no message,
  and cqp's own message verbatim, its `<--` position pointer included, as
  the sample output of another program."
  [ui {:keys [type message]} corpora]
  (list
   (when (seq corpora)
     [:p (str (i18n/tr ui "in") " ")
      (interpose ", " (map (fn [c] [:code c]) corpora))])
   (when-let [explanation (error-explanation ui type)]
     [:p explanation])
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

  Every view of a result shares this: the heading, with the `subheading`
  phrases (see `qualifiers`) under it, a status line while the result is
  still being counted (see `counting?`), the switch between the views,
  the error that replaced the result or the errors of individual
  corpora, and then `body`, the view's own content. The two views differ
  only in what they say about the same hits, so they differ only in what
  they pass here.

  The heading is the page's h1: the search page has no other, so what a
  search found, or why it found nothing, is what the page is about. It
  is the answer alone. The question is the line under it, grouped with
  it as heading and subheading, and the region is named by the heading
  alone, so a screen reader landing here hears the count and not the
  whole question, which the controls below restate anyway.

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
   [:hgroup
    [:h1 {:id "results-heading"} heading]
    (when (seq subheading)
      [:p (interpose " · " subheading)])]
   ;; always rendered, and before anything whose kind can change (see
   ;; `navigation-status`)
   [:div.status {:role "status"}
    (when (counting? result)
      [:p (str (i18n/tr ui "Counting hits in") " "
               (corpora-phrase ui (:remaining result)) " …")])]
   (view-switch ui view view-hrefs)
   (when error (error-body ui error nil))
   (for [[e corpora] (error-groups (:counts result))]
     (error-section ui e corpora))
   body])

(defn result-section
  "The concordance view of the search in `state`: when any corpus could be
  searched and found something, the sort, context and sample controls
  with the near control behind its disclosure (see `view-controls`), the
  pagination above and below the table, the concordance with its
  `:expanded` hits and `:langs`, then the per-corpus counts as an aside
  and the download links (`:export-hrefs`, exports holding at most
  `:export-limit` hits), all worded in the state's `:ui` and wrapped in
  the shared `results-region`."
  [{:keys [ui sort-modes params result error langs expanded client?
           export-hrefs export-limit prev-href next-href]
    :as state}]
  (let [{:keys [counts hits size]} result
        position (when result (page-phrase ui result))]
    (results-region
     state
     (result-heading ui params result error)
     (qualifiers ui params result)
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
                         (list (sort-control ui sort-modes (:sort params))
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
                                  :expanded expanded
                                  :client?  client?
                                  :cursor   (:cursor state)})
          (pager-links ui prev-href next-href position)
          ;; an <aside>, after the hits rather than before them: it is
          ;; about the answer rather than part of it, and where the hits
          ;; came from is a question a reader has once they have read
          ;; them. After the page links too, which belong against the
          ;; foot of the table they turn, where a reader who has finished
          ;; reading reaches. Unnamed, so that inside the results
          ;; <section> it stays a container rather than becoming a second
          ;; complementary landmark: the table's own caption names it
          (when (next counts) [:aside (counts-table ui counts)])
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
