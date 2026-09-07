(ns dk.cst.corpus-probe.views.search
  "The search form: one text field read by its shape, or the tokens of
  the extended search (see dk.cst.corpus-probe.views.search.tokens), the
  boxes deciding how the query is read, the corpus chooser the page
  hands in (see dk.cst.corpus-probe.views.corpus/corpus-chooser) and the
  metadata filter (see dk.cst.corpus-probe.views.search.filter); and the
  help that stands where the results will until there are any.

  The page these build is assembled by dk.cst.corpus-probe.views/search-page.
  The server renders it for first paint; the client renders the same form
  from the same state, so choosing the extended form swaps the field for
  the tokens without a round trip. Wrapped in the <search> landmark HTML
  has for it, so the document is meaningful without the stylesheet."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.cqp :as cqp]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.query.mode :as mode]
            [dk.cst.corpus-probe.query.tokens :as tokens]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.result :as result]
            [dk.cst.corpus-probe.views.search.filter :as filter]
            [dk.cst.corpus-probe.views.search.tokens :as tokens-views]
            [dk.cst.corpus-probe.views.widgets :as widgets]))

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
  (see dk.cst.corpus-probe.query.mode/shape and `reading-line`). On the
  client Enter submits the form and Shift+Enter starts a line (see
  dk.cst.corpus-probe.ui/handle!). No visible label: a field with a
  search button beside it needs none to say what it is, so the name it
  keeps is the one only a screen reader reads, and the line under it
  describes the field to the same reader when its id is given as
  `described-by`. Every key dispatches `:set-query`, so the state holds
  the text as typed and the answer can tell when the form has moved on
  from what ran (see dk.cst.corpus-probe.views.result/question).

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
    (case (mode/mode params)
      "cqp"  [:p.cqp {:id reading-id} (i18n/tr ui "Read as CQP")]
      "list" (let [words (:conditions (first (:tokens query)))]
               (when (next words)
                 (line (i18n/tr ui "Any one of {n} words" {:n (n words)}))))
      (let [tokens (:tokens query)]
        (when (next tokens)
          (line (i18n/tr ui "{n} words in order" {:n (n tokens)})))))))

(defn attribute-control
  "The control choosing which positional attribute a simple search
  matches, in `ui`: a select over `attrs` with `selected` chosen (see
  dk.cst.corpus-probe.views.search.tokens/attribute-options), named for a
  screen reader only, since it stands in a sentence (see
  `matching-fieldset`)."
  [ui attrs selected]
  [:select {:name "in" :aria-label (i18n/tr ui "attribute")}
   (tokens-views/attribute-options ui attrs selected)])

(defn unit-label
  "What the unit of text `unit` a search is kept within is called, in
  `ui` (see dk.cst.corpus-probe.cqp/units)."
  [ui unit]
  (case unit
    "paragraph" (i18n/tr ui "paragraph")
    "text"      (i18n/tr ui "text")
    (i18n/tr ui "sentence")))

(defn within-control
  "The control choosing the unit of text a search of several tokens is
  kept within, in `ui`: a select over the dk.cst.corpus-probe.cqp/units
  with `within` chosen, the sentence when it names none (see
  dk.cst.corpus-probe.query/within)."
  [ui within]
  [:select {:name "within" :id "within"}
   (for [unit (map (comp name first) cqp/units)]
     (widgets/option (or within "sentence") unit (unit-label ui unit)))])

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

(defn match-control
  "The control choosing how much of the form a simple search must
  cover, in `ui`: a select over
  dk.cst.corpus-probe.query.tokens/match-ops, each named by
  `match-option-label`, with `match` chosen and the whole form when it
  names none; named for a screen reader only, since it stands in a
  sentence (see `matching-fieldset`).

  One control rather than a box for each end: two boxes both ticked
  meant any part of the form, and nothing said so."
  [ui match]
  [:select {:name "match" :aria-label (i18n/tr ui "match")}
   (for [value tokens/match-ops]
     (widgets/option (or match "") value (match-option-label ui value)))])

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
  which says so while `pending?` and holds nothing otherwise (see
  dk.cst.corpus-probe.views.widgets/status).

  It sits with the query controls rather than with the results, because
  the wait starts at the submit button and the results it is about may
  not exist yet. Above 64rem the layout sets it between the query row
  and the answer, under that button."
  [ui pending?]
  (widgets/status
   "navigation-status"
   ;; TODO: design this. A line of text arriving under the form is what
   ;; it says, not a thing anyone drew, and three questions are open:
   ;; where a reader is actually looking when they are waiting (the foot
   ;; of the rail is where the button is, but not where the answer will
   ;; be); whether it wants a minimum time on screen, since an answer at
   ;; 450ms still flashes past `dk.cst.corpus-probe.ui/pending-delay-ms`;
   ;; and whether waiting should say more than that it is waiting. The
   ;; metadata filter has the same decision pending (see
   ;; dk.cst.corpus-probe.views.search.filter/filter-fieldset), as does
   ;; the count of a result still being made (see
   ;; dk.cst.corpus-probe.views.result/results-region), and the three
   ;; should be answered together.
   (when pending? [:p (i18n/tr ui "Loading …")])))

(defn cqp-line
  "The CQP the extended form's `tokens` compile to (see
  dk.cst.corpus-probe.views.result/form-query, with the `params`), under
  them in `ui`, so that a reader sees how the conditions' joins and
  repeats came out before a search is spent on them, and as they edit,
  since every control records itself in the state; nothing for no
  query. A paragraph rather than an output, whose implicit status role
  would have a screen reader read the string after every keystroke.

  TODO: is the line worth its place? The field's radio hands the same
  text to the field, with the unit, and switching back restores the
  tokens. Drop it if nobody reads it."
  [ui params tokens]
  (when-let [cqp (query/->cqp (result/form-query params tokens))]
    [:p.cqp (i18n/tr ui "As CQP") ": " [:code cqp]]))

(defn mode-label
  "What the query `mode` (see dk.cst.corpus-probe.query.mode/modes) is
  called, in `ui`, as a word."
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
  (if (tokens/token-key? k)
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

(defn mode-radios
  "The radios choosing the form of the query in `ui`, `form` being the
  one chosen (see dk.cst.corpus-probe.query.mode/forms), each dispatching
  `:set-mode`, so choosing the extended form swaps the field for the
  tokens without a round trip."
  [ui form]
  [:p (interpose " "
                 (for [m mode/forms]
                   [:label [:input {:type    "radio" :name "mode"
                                    :value   m
                                    :checked (= m form)
                                    :on      {:change [:set-mode m]}}]
                    (mode-label ui m)]))])

(defn modes-fieldset
  "The query's box in the search form, in `ui`: its form (see
  `mode-radios`), chosen among the `params`, and a status line for what
  a change of it could not keep (its `switch`, see `switch-notice`),
  rendered always, so that the live region exists before it fills, and
  before everything a switch changes. Named by a legend like the boxes
  beside it, so that they line up when they share a row.

  Without the `client?` a change of mode is a submit, which the field a
  fresh form requires would refuse; a button submits without that check,
  so a reader can leave an empty form for another mode. Inside
  <noscript>, for the reason dk.cst.corpus-probe.views.result/apply-button
  is."
  [ui params client? switch]
  [:fieldset.modes.box
   [:legend (i18n/trx ui "legend" "Query type")]
   (mode-radios ui (mode/form params))
   (when-not client?
     [:noscript
      [:p [:button {:type "submit" :formnovalidate true}
           (i18n/tr ui "Change mode")]]])
   (widgets/status
    (switch-notice ui (mode/mode params) (:loss switch) (:unread switch)))])

(defn matching-fieldset
  "The matching box of the search form in `ui`, read as a sentence: find
  how much of which positional attribute, among `attrs` (see
  `match-control` and `attribute-control`), within which unit of text
  (see `within-control`), which the words of a simple search read as the
  tokens of an extended one do, and the case under it, prefilled from
  `params`. Only the parts the mode reads (see
  dk.cst.corpus-probe.query.mode/reads?), and no box at all for a mode
  that reads none of them: a CQP query writes all four itself.

  A control the mode does not read is taken away rather than shown dead:
  a row of greyed controls is something to read past before reaching one
  that can be used, and nothing is lost by taking it away, since what
  the reader chose is held in the params and comes back with the mode
  that reads it. A control that is not there is not submitted either, so
  nothing about a simple search rides along with a CQP one."
  [ui attrs {:keys [in ci match within] :as params}]
  (let [live? (partial mode/reads? (mode/mode params))]
    (when (some live? [:in :match :within :ci])
      [:fieldset.matching.box
       [:legend (i18n/trx ui "legend" "Scope")]
       (when (or (live? :match) (live? :in))
         [:p.matching-find [:span (i18n/tr ui "find")]
          (when (live? :match) (match-control ui match))
          (when (live? :in) (attribute-control ui attrs in))])
       (when (live? :within)
         [:p.matching-within [:label {:for "within"} (i18n/tr ui "in")]
          (within-control ui within)])
       (when (live? :ci)
         [:p.matching-case
          [:label [:input {:type    "checkbox" :name "ci" :value "on"
                           :checked (some? ci)}]
           (i18n/tr ui "ignore case")]])])))

(defn search-form
  "The search form of `state`: over its metadata `:filter-controls` and
  the `:search-attrs` a simple search may match (see
  `attribute-control`), prefilled from its `:params` (:mode, the form,
  :q, the field's text, :in :ci :match :within), submitted as GET to
  `action`, with the page's own `extra` hidden inputs and the `chooser`
  of its corpora (see dk.cst.corpus-probe.views.corpus/corpus-chooser),
  which the page renders over the selection the params name.

  The query row comes first (see `query-field`, with how its text is
  read under it, see `reading-line`), or the tokens of an extended
  search (see dk.cst.corpus-probe.views.search.tokens/token-fieldset, over
  the `:tokens` and `:value-lists` of `state`), then the boxes, in a
  wrapper of their own, which the wide layout makes a rail beside the
  answer while the query row stands above it, with room to type in.
  First everything that decides how the query is read, a box each: the
  form (see `modes-fieldset`) and the matching (see
  `matching-fieldset`). Then the scope of the search, the corpus chooser
  and the metadata filter (see
  dk.cst.corpus-probe.views.search.filter/filter-fieldset). So the field
  the reader reaches for is the first control in the form, whatever the
  registry holds, and what qualifies what they typed is under their
  hand rather than past two disclosures.

  The query is required, except when the form is submitted from the
  frequency view (its `:view`), which counts every token of a blank one,
  and so is every token row of an extended search but the blank last
  one; the corpus chooser requires a corpus where the client runs. All
  of these are the browser's own checks, so missing input is reported
  before it is sent, in the browser's words, and the server's own
  answers stand for a request that never passed through the form.

  Wrapped in a <search> landmark; GET, so every search has a shareable URL
  and works without JavaScript. The form carries an id, so a control
  rendered outside it (the sort of the concordance, the grouping of the
  frequency table) still submits with it. No language is submitted with
  the search: which language the answer is worded in is the reader's
  own stored preference, not part of what they asked.

  Where the client runs, `navigation-status` follows the form inside the
  landmark, and the metadata filter shows what is chosen or everything
  there is to choose, by what `:lists` holds of it (see
  dk.cst.corpus-probe.ui/lists)."
  [{:keys [ui view filter-controls search-attrs params tokens value-lists
           switch client? pending? lists filters-pending?]
    :as state}
   action extra chooser]
  (let [{:keys [q]} params
        {:keys [values]} lists
        extended? (= "extended" (mode/form params))
        button    [:button {:type "submit"} (i18n/trx ui "button" "Search")]
        held      (into (filter/filter-pairs (:selected filter-controls))
                        (:unticked values))]
    [:search
     [:form.search-form {:id url/form-id :method "get" :action action}
      extra
      ;; the query row: the field or the tokens, and the button, which
      ;; belongs against the field it submits rather than at the foot of
      ;; every control that qualifies it. In a wrapper of one kind whatever
      ;; the row holds, so that the group after it keeps its identity when
      ;; the row changes kind: measured, without it the switch back from
      ;; the tokens rebuilt the group, the radio the reader had pressed
      ;; included. Classed by its kind for the wide layout, which gives a
      ;; row of tokens the column
      [:div.query (cond-> {} extended? (assoc :class "query-extended"))
       (if extended?
         (list (tokens-views/token-fieldset ui search-attrs value-lists client?
                                            (not= :frequencies view) tokens)
               (tokens-views/add-token-row ui client? button)
               (cqp-line ui params tokens))
         (let [line (reading-line ui params)]
           (list [:p (query-field ui q (not= :frequencies view)
                                  (when line reading-id))
                  " " button]
                 line)))]
      ;; one wrapper, which the wide layout makes a rail beside the answer
      [:div.rail
       (modes-fieldset ui params client? switch)
       (matching-fieldset ui search-attrs params)
       ;; marks a selection the reader actually made: without it, unticking
       ;; every corpus and submitting is indistinguishable from arriving
       ;; with no corpus named, which searches them all
       [:input {:type "hidden" :name "scope" :value "chosen"}]
       chooser
       ;; the list's state is named for the chooser's options, and what
       ;; it holds beyond them the chooser ignores
       (filter/filter-fieldset ui filter-controls
                               (assoc values
                                      :held     held
                                      :pending? filters-pending?
                                      :client?  client?))]]
     ;; only where the client runs: every other navigation is the
     ;; browser's own, and the browser reports those itself
     (when client? (navigation-status ui pending?))]))
