(ns dk.cst.corpus-probe.views.search
  "The search form: one text field read by its shape, or the tokens of
  the extended search, the boxes deciding how the query is read, the
  corpus chooser the page hands in and the metadata filter; and the help
  that stands where the results will until there are any."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.cqp :as cqp]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.query.mode :as mode]
            [dk.cst.corpus-probe.query.tokens :as tokens]
            [dk.cst.corpus-probe.settings :as settings]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.result :as result]
            [dk.cst.corpus-probe.views.search.filter :as filter-views]
            [dk.cst.corpus-probe.views.search.tokens :as tokens-views]
            [dk.cst.corpus-probe.views.widgets :as widgets]))

(defn query-field
  "The query field of the search form in `ui`, holding `text`: a text
  area, so that a list can be typed one word per line, `required?` when
  a blank query means nothing."
  [ui text required?]
  (let [text  (str text)
        attrs {:id           "q"
               :name         "q"
               :rows         (min 8 (inc (count (re-seq #"\r\n|[\r\n]"
                                                        text))))
               :aria-label   (i18n/tr ui "Query")
               :placeholder  (i18n/tr ui "words, a list or CQP")
               :autocomplete "off"
               :spellcheck   "false"
               :enterkeyhint "search"
               :required     required?
               ;; every keystroke into the state, so the answer can
               ;; tell when the form has moved on from what ran (see
               ;; dk.cst.corpus-probe.views.result/question)
               :on           {:input   [:set-query
                                        :event.target/value]
                              :keydown [:submit-on-enter
                                        :event/key
                                        :event/shift?
                                        :event/composing?]}
               ;; required passes whitespace, so a blank of any
               ;; length is reported by the field itself
               :replicant/on-render
               [:set-validity (when (and required? (str/blank? text))
                                (i18n/tr ui "Type a query"))]}]
    ;; the text is the element's content, a text area having no value
    ;; attribute; the client sets the value property too, since the
    ;; content is only what the area starts with
    [:textarea #?(:clj attrs :cljs (assoc attrs :value text)) text]))

(defn attribute-control
  "The control choosing which positional attribute a simple search
  matches, in `ui`: a select over `attrs` with `selected` chosen, named
  for a screen reader only, since it stands in a sentence."
  [ui attrs selected]
  [:select {:name "in" :aria-label (i18n/tr ui "attribute")}
   (tokens-views/attribute-options ui attrs selected)])

(defn unit-label
  "What the unit of text `unit` a search is kept within is called, in
  `ui`."
  [ui unit]
  (case unit
    "paragraph" (i18n/tr ui "paragraph")
    "text"      (i18n/tr ui "text")
    (i18n/tr ui "sentence")))

(defn within-control
  "The control choosing the unit of text a search of several tokens is
  kept within, in `ui`: a select over the CQP units with `within`
  chosen, the sentence when it names none, under its label."
  [ui within]
  (list
   [:label {:for "within"} (i18n/tr ui "in")]
   [:select {:name "within" :id "within"}
    (for [unit (map (comp name first) cqp/units)]
      (widgets/option (or within "sentence") unit (unit-label ui unit)))]))

(defn match-option-label
  "What the `match` param value is called as an option of
  `match-control`, in `ui`, read before the attribute in a sentence:
  find whole word."
  [ui match]
  (case match
    "prefix" (i18n/tr ui "start of")
    "suffix" (i18n/tr ui "end of")
    "infix"  (i18n/tr ui "part of")
    (i18n/tr ui "whole")))

(defn match-control
  "The control choosing how much of the form a simple search must
  cover, in `ui`: a select over the match operators with `match`
  chosen, the whole form when it names none; named for a screen reader
  only, since it stands in a sentence."
  [ui match]
  [:select {:name "match" :aria-label (i18n/tr ui "match")}
   (for [value tokens/match-ops]
     (widgets/option (or match "") value (match-option-label ui value)))])

(defn help
  "The search help, the hiccup `blocks` of its document, as a region
  named in `ui`; nil without a help document."
  [ui blocks]
  ;; TODO: the page has no h1 until an answer heads it. Does the empty
  ;; page want one, and of what?
  (when (seq blocks)
    [:section.help {:aria-label (i18n/tr ui "Help")} blocks]))

(defn navigation-status
  "The live region reporting a routed navigation in flight in `ui`,
  which says so while `pending?` and holds nothing otherwise."
  [ui pending?]
  (widgets/status
   "navigation-status"
   ;; TODO: design this. Three questions are open: where a reader is
   ;; looking while they wait (the button is at the foot of the rail, the
   ;; answer is not); whether it wants a minimum time on screen, since a
   ;; fast answer flashes it past
   ;; (dk.cst.corpus-probe.client.effects/pending-delay-ms); and whether
   ;; it should say more than that it is waiting. The metadata filter's
   ;; busy state and the count still being made (see
   ;; dk.cst.corpus-probe.views.result/results-region) have the same
   ;; decision pending, and the three want answering together
   (when pending? [:p (i18n/tr ui "Loading …")])))

(defn cqp-line
  "The CQP the extended form's `tokens` compile to, with the `params`,
  as a line under them in `ui`; nil for no query."
  [ui params tokens]
  ;; TODO: is the line worth its place? The field's radio hands the same
  ;; text to the field, and switching back restores the tokens. Drop it
  ;; if nobody reads it
  (when-let [cqp (query/->cqp (result/form-query params tokens))]
    ;; a paragraph, not an output: its implicit status role would have a
    ;; screen reader read the string after every keystroke
    [:p.cqp (i18n/tr ui "As CQP") ": " [:code cqp]]))

(defn mode-label
  "What the query `mode` is called, in `ui`, as a word."
  [ui mode]
  (case mode
    "list"     (i18n/tr ui "List")
    "extended" (i18n/tr ui "Extended")
    "cqp"      "CQP"
    (i18n/tr ui "Default")))

(defn loss-sentence
  "What a change of form did to the query, the loss `item` (see
  dk.cst.corpus-probe.query/loss), as a sentence in `ui`."
  [ui [kind x]]
  (case kind
    :cqp  (list (i18n/tr ui (str "Extended cannot read CQP. "
                                 "The query is not kept:"))
                " " [:code x])
    :list (i18n/tr ui "A list of {n} words is not kept in Extended."
                   {:n (i18n/group-digits ui x)})))

(defn param-label
  "What the query param `k` a URL carried is called in `ui`; nil for a
  key with no name."
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
  as the form's status line: the `loss` items as sentences, and the
  `unread` params a hand-written URL carried as one naming the mode;
  nil when there is nothing to say."
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
  one chosen."
  [ui form]
  [:p (interpose " "
                 (for [m mode/forms]
                   [:label [:input {:type    "radio" :name "mode"
                                    :value   m
                                    :checked (= m form)
                                    :on      {:change
                                              [:set-mode m
                                               :event.target.form/params]}}]
                    (mode-label ui m)]))])

(defn modes-fieldset
  "The query's box in the search form, in `ui`: its form chosen among
  the `params`, the noscript button changing it without a `client?`,
  and a status line for what the `switch` of form could not keep."
  [ui params client? switch]
  [:fieldset.modes.box
   [:legend (i18n/trx ui "legend" "Query type")]
   (mode-radios ui (mode/form params))
   ;; without the client a change of mode is a submit, which a required
   ;; empty field would refuse; inside noscript for the reason
   ;; dk.cst.corpus-probe.views.result/apply-button is
   (when-not client?
     [:noscript
      [:p [:button {:type "submit" :formnovalidate true}
           (i18n/tr ui "Change mode")]]])
   ;; always rendered, so the live region exists before it fills, and
   ;; before the matching box a switch can take away
   (widgets/status
    (switch-notice ui (mode/mode params) (:loss switch) (:unread switch)))])

(defn matching-fieldset
  "The matching box of the search form in `ui`, read as a sentence: find
  how much of which positional attribute among `attrs`, within which
  unit of text, and the case under it, prefilled from `params`; only
  the parts the mode reads, and no box for a mode reading none."
  [ui attrs {:keys [in ci match within] :as params}]
  (let [live? (partial mode/reads? (mode/mode params))]
    ;; a control the mode does not read is left out rather than disabled:
    ;; it is then not submitted either, and the params keep what was
    ;; chosen for the mode that reads it
    (when (some live? [:in :match :within :ci])
      [:fieldset.matching.box
       [:legend (i18n/trx ui "legend" "Scope")]
       (when (or (live? :match) (live? :in))
         [:p.matching-find [:span (i18n/tr ui "find")]
          (when (live? :match) (match-control ui match))
          (when (live? :in) (attribute-control ui attrs in))])
       (when (live? :within)
         [:p.matching-within (within-control ui within)])
       (when (live? :ci)
         [:p.matching-case
          [:label [:input {:type    "checkbox" :name "ci" :value "on"
                           :checked (some? ci)}]
           (i18n/tr ui "ignore case")]])])))

(defn settings-now
  "The settings the form of `state` shows, as they would be stored,
  measured against its `:selectable` corpora.

  The client calls it when it stores, so the value a button offers and
  the value a change writes cannot drift apart."
  [{:keys [params selectable autosave?]}]
  (settings/string (cond-> params
                     (false? autosave?)
                     (assoc settings/autosave-key settings/autosave-off))
                   selectable))

(defn settings-buttons
  "The buttons in `ui` storing `now` (see `settings-now`) as the reader's
  own defaults, or forgetting what they have `stored`.

  Without a `client?` neither is disabled: their state is read as the
  page renders and goes stale as soon as a box is ticked, and a button
  that does nothing is kinder than one that refuses a change the reader
  really has made."
  [ui now stored client?]
  (let [departs? (seq now)
        unsaved? (not= now stored)
        button   (fn [value on? label]
                   [:button {:type     "submit" :form settings/form-id
                             :name     (name settings/cookie-key) :value value
                             :disabled (boolean (and client? (not on?)))}
                    label])]
    [:p.settings-buttons
     (button now (and departs? unsaved?) (i18n/trx ui "button" "Save"))
     ;; the empty settings are no settings, so forgetting is storing
     (button "" (or departs? (seq stored)) (i18n/trx ui "button" "Reset"))]))

(defn autosave-control
  "The checkbox in `ui` saying whether a change to the form stores the
  settings it leaves, ticked when `autosave?`.

  It stores itself the moment it changes, which cannot wait for the
  button beside it: a reader who turns storing off could never keep that
  choice if keeping it were the first thing the choice forbade. Without
  a `client?` it is posted with the buttons instead."
  [ui autosave? client?]
  [:p.settings-autosave
   [:label
    [:input (cond-> {:type    "checkbox"
                     :form    settings/form-id
                     :name    (name settings/autosave-key)
                     :value   "on"
                     :checked (boolean autosave?)}
              client? (assoc :on {:change [:set-autosave
                                           :event.target/checked]}))]
    (i18n/tr ui "save automatically")]])

(defn settings-announcement
  "The live region of the preferences box in `ui`, spoken and never seen,
  saying what `announcement` names and nothing for nil.

  Storing moves nothing and the button pressed goes quiet, so a reader
  not watching the screen is otherwise told nothing; off screen because
  one who is watching has the buttons. The announcement belongs to the
  act and not to the state it left, so the next action takes it away
  (see dk.cst.corpus-probe.client.actions/act) and a second save is
  spoken as the first was."
  [ui announcement]
  (widgets/status "spoken"
                  (when (= :saved announcement)
                    (i18n/tr ui "Settings saved"))))

(defn settings-fieldset
  "The preferences box of the search form of `state` in `ui`: what the
  form is stored as, forgotten with, and whether it is stored at all."
  [{:keys [client? autosave? stored announcement] :as state} ui]
  ;; the group takes focus where the button that had it goes quiet: the
  ;; box they are still in, rather than the button that would undo it
  [:fieldset.settings.box {:id settings/box-id :tabindex "-1"}
   [:legend (i18n/trx ui "legend" "Preferences")]
   (settings-buttons ui (settings-now state) stored client?)
   (autosave-control ui autosave? client?)
   (settings-announcement ui announcement)])

(defn settings-form
  "The form the `settings-fieldset`'s controls post to, returning to the
  search `params` describe.

  A preference is state, so it is posted rather than asked for in a URL
  (see dk.cst.corpus-probe.server/serve-preferences). The buttons carry
  what is stored and stand elsewhere, so this holds nothing but the
  return and the id they name."
  [params]
  [:form.settings-form {:id     settings/form-id
                        :method "post"
                        :action url/preferences}
   [:input {:type "hidden" :name "return" :value (url/search-href params)}]])

(defn search-form
  "The search form of `state`, submitted as GET to `action` with the
  page's own `extra` hidden inputs and the `chooser` of its corpora: the
  query row, then the boxes deciding how the query is read and the
  scope it is kept within, prefilled from the state's `:params`, and the
  buttons storing them as the reader's defaults.

  The query is required unless the form is submitted from the frequency
  `:view`, which counts every token of a blank one. A `:seeded?` form has
  run no search, so it asks for a query whatever view it was seeded in."
  [{:keys [ui view filter-controls search-attrs params tokens value-lists
           switch client? pending? lists filters-pending? seeded?]
    :as state}
   action extra chooser]
  (let [{:keys [q]} params
        {:keys [values]} lists
        extended? (= "extended" (mode/form params))
        ;; a stored :view seeds the form without a search behind it, and
        ;; the blank query the frequency view allows is for counting a
        ;; result the reader is already looking at
        required? (boolean (or (not= :frequencies view) seeded?))
        ;; the button says what pressing it does. A text shaped like CQP
        ;; runs as CQP, and the form says so nowhere else: the boxes it
        ;; takes away are an absence, not a statement
        button    [:button {:type "submit"}
                   (if (= "cqp" (mode/mode params))
                     (i18n/trx ui "button" "Run as CQP")
                     (i18n/trx ui "button" "Search"))]
        held      (into (filter-views/filter-pairs (:selected filter-controls))
                        (:unticked values))]
    ;; HTML's own landmark for a search form, so a screen reader can jump
    ;; to it. GET, so every search is a shareable URL and works without a
    ;; script; the id lets the controls beside a result submit with it
    [:search
     [:form.search-form {:id url/form-id :method "get" :action action}
      extra
      ;; the query row first, whatever the registry holds: the field or
      ;; the tokens, and the button against the field it submits. In a
      ;; wrapper of one kind whatever the row holds: an element that
      ;; changes kind rebuilds everything after it, the radio the reader
      ;; had just pressed included
      [:div.query (cond-> {} extended? (assoc :class "query-extended"))
       (if extended?
         (list (tokens-views/token-fieldset ui search-attrs value-lists client?
                                            required? tokens)
               (tokens-views/add-token-row ui client? button)
               (cqp-line ui params tokens))
         ;; TODO: a line here said how the text was read, and said
         ;; nothing at all for the single word most searches are. Find a
         ;; reading that earns its place, or leave it to the button
         [:p (query-field ui q required?) " " button])]
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
       (filter-views/filter-fieldset ui filter-controls
                                     (assoc values
                                            :held     held
                                            :pending? filters-pending?
                                            :client?  client?))
       (settings-fieldset state ui)]]
     (settings-form params)
     ;; only where the client runs: every other navigation is the
     ;; browser's own, and the browser reports those itself
     (when client? (navigation-status ui pending?))]))
