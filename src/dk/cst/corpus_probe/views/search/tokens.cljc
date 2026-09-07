(ns dk.cst.corpus-probe.views.search.tokens
  "The extended form of the search: its tokens as groups of conditions
  (see dk.cst.corpus-probe.query.tokens/form-tokens), each condition an
  attribute, an operator and a value, each token its conditions with
  its repeat and its edges, and the row under them that adds a token."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.query.tokens :as tokens]
            [dk.cst.corpus-probe.views.widgets :as widgets]))

(defn attribute-label
  "What the positional attribute named `attr` is called in `ui`: the
  usual ones in the reader's words, since they stand in a sentence (see
  dk.cst.corpus-probe.views.search/matching-fieldset), any other as its
  corpus names it."
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
      (widgets/option selected n (attribute-label ui n)))))

(defn operator-label
  "What the operator `op` of an extended-search token is called, in `ui`
  (see dk.cst.corpus-probe.query.tokens/operators); equality for one it
  does not know."
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

(defn join-select
  "The select saying how condition `c` of `token` (its facts, see
  `condition-row`) joins the ones before it, in `ui`: and or or, from
  the :join of `condition`, and when it names none; dead under the
  token's `:any?`."
  [ui {:keys [i any?]} c {:keys [id join]}]
  [:select.condition-join
   {:name       (tokens/token-key i c :join)
    :aria-label (i18n/tr ui "joined by")
    :disabled   any?
    :on         {:change [:set-condition [i id :join] :event.target/value]}}
   (for [j tokens/joins]
     (widgets/option (or join "and") j (case j
                                         "or" (i18n/tr ui "or")
                                         (i18n/tr ui "and"))))])

(defn attribute-select
  "The select choosing which positional attribute condition `c` of
  `token` (its facts, see `condition-row`) names, in `ui`: the token's
  `:attrs` (see `attribute-options`) with the :attr of `condition`
  chosen; dead under the token's `:any?`."
  [ui {:keys [i attrs any?]} c {:keys [id attr]}]
  [:select {:name       (tokens/token-key i c :attr)
            :aria-label (i18n/tr ui "attribute")
            :disabled   any?
            :on         {:change [:set-condition [i id :attr]
                                  :event.target/value]}}
   (attribute-options ui attrs attr)])

(defn operator-select
  "The select choosing the operator of condition `c` of `token` (its
  facts, see `condition-row`), in `ui`: the operators (see
  `operator-label`) with the :op of `condition` chosen, equality when it
  names none, and any word offered to a token's first condition alone,
  which under the token's `:any?` is the one live control."
  [ui {:keys [i any?]} c {:keys [id op]}]
  (let [first? (= 1 c)]
    [:select {:name       (tokens/token-key i c :op)
              :aria-label (i18n/tr ui "operator")
              :disabled   (and any? (not first?))
              :on         {:change [:set-condition [i id :op]
                                    :event.target/value]}}
     (for [o (cond->> tokens/operators (not first?) (remove #{"any"}))]
       (widgets/option (or op "is") o (operator-label ui o)))]))

(defn value-field
  "The field holding the value of condition `c` of `token` (its facts,
  see `condition-row`), in `ui`: the :v of `condition`, required when
  `required?`, suggesting the values the token's `:value-lists` hold
  for the condition's :attr (see `value-list-id`); dead under the
  token's `:any?`."
  [ui {:keys [i value-lists any?]} required? c {:keys [id attr v]}]
  (let [attr* (keyword (if (str/blank? attr) "word" attr))]
    [:input.condition-value
     (cond-> {:type         "text"
              :name         (tokens/token-key i c :v)
              :value        (or v "")
              :aria-label   (i18n/trx ui "field" "value")
              :autocomplete "off"
              :spellcheck   "false"
              :required     (and required? (not any?))
              :disabled     any?
              :on           {:input [:set-condition [i id :v]
                                     :event.target/value]}}
       (contains? value-lists attr*)
       (assoc :list (value-list-id attr*)))]))

(defn case-box
  "The box asking condition `c` of `token` (its facts, see
  `condition-row`) to ignore case, in `ui`, ticked when `condition` has
  a :ci; dead under the token's `:any?`."
  [ui {:keys [i any?]} c {:keys [id ci]}]
  [:label [:input {:type     "checkbox" :name (tokens/token-key i c :ci)
                   :value    "on"
                   :checked  (some? ci)
                   :disabled any?
                   :on       {:change [:set-condition [i id :ci]
                                       :event.target/checked]}}]
   (i18n/tr ui "ignore case")])

(defn condition-row
  "One condition of the extended search in `ui`: condition `c`, counted
  from one, holding `condition` (see
  dk.cst.corpus-probe.query.tokens/form-tokens), of `token`, the facts of
  the token it belongs to: its number `:i`, the `:attrs` a condition may
  name (see `attribute-options`), the `:value-lists` some of them offer
  (see `value-list-id`), `:any?` when the token's first condition
  matches any word, and `:removable?` when a button may take the
  condition away. The attribute, the operator, the value and the
  ignore-case box (see `attribute-select`, `operator-select`,
  `value-field` and `case-box`), headed by how it joins the conditions
  before it when it is not the first (see `join-select`).

  Its fields carry the token's number and, after the first, its own
  (see dk.cst.corpus-probe.query.tokens/token-key). The value is required
  when `required?`. Under `:any?` every control but that first operator
  is disabled: an any-word token has nothing else to say, and without
  the client they are as the search was submitted. Every control
  dispatches `:set-condition` with its field, so the state holds the
  condition as the reader has it: the operator and the attribute decide
  which controls are live and which values the field suggests, and all
  of them decide the CQP line under the tokens (see
  dk.cst.corpus-probe.views.search/cqp-line)."
  [ui {:keys [i removable?] :as token} required? c {:keys [id] :as condition}]
  [:li.condition (cond-> {:replicant/key id}
                   removable? (assoc :class "removable"))
   (when-not (= 1 c)
     (list (join-select ui token c condition) " "))
   (attribute-select ui token c condition)
   " "
   (operator-select ui token c condition)
   " "
   (value-field ui token required? c condition)
   " "
   (case-box ui token c condition)
   (when removable?
     (list " "
           [:button.condition-remove
            {:type       "button"
             :aria-label (str (i18n/tr ui "Remove condition") " " c)
             :on         {:click [:remove-condition [i id]]}}
            "×"]))])

(defn repeat-fields
  "The repeat of token `i` of the extended search in `ui`: at least `lo`
  and at most `hi` times, once each when nil, in a group named for what
  the pair is, since the second number is labelled only to."
  [ui i lo hi]
  [:span.token-repeat {:role "group" :aria-label (i18n/tr ui "repeat")}
   [:label (i18n/tr ui "repeat") " "
    [:input {:type "number" :name (tokens/token-key i 1 :min) :value (or lo "1")
             :min  0 :max 99
             :on   {:input [:set-token [i :min] :event.target/value]}}]]
   " "
   [:label (i18n/tr ui "to") " "
    [:input {:type "number" :name (tokens/token-key i 1 :max) :value (or hi "1")
             :min  0 :max 99
             :on   {:input [:set-token [i :max] :event.target/value]}}]]])

(defn edge-boxes
  "The boxes asking token `i` of the extended search to open a sentence,
  ticked under `start`, and to close one, ticked under `end`, in `ui`."
  [ui i start end]
  (list
   [:label.token-edges
    [:input {:type    "checkbox" :name (tokens/token-key i 1 :start)
             :value   "on"
             :checked (some? start)
             :on      {:change [:set-token [i :start]
                                :event.target/checked]}}]
    (i18n/tr ui "sentence start")]
   " "
   [:label.token-edges
    [:input {:type    "checkbox" :name (tokens/token-key i 1 :end)
             :value   "on"
             :checked (some? end)
             :on      {:change [:set-token [i :end]
                                :event.target/checked]}}]
    (i18n/tr ui "sentence end")]))

(defn token-actions
  "The buttons of token `i` of the extended search, with `id`, on a row
  of their own so they stay together, in `ui`: one adding a condition,
  dead under `any?`, since an any-word token has nothing else to say,
  and one taking the token away."
  [ui i id any?]
  [:p.token-actions
   [:button {:type     "button"
             :disabled any?
             :on       {:click [:add-condition i]}}
    (i18n/tr ui "Add condition")]
   " "
   [:button {:type       "button"
             :aria-label (str (i18n/tr ui "Remove token") " " i)
             :on         {:click [:remove-token id]}}
    "×"]])

(defn token-box
  "One token of the extended search in `ui`: token `i`, counted from
  one, which is the number its fields carry in the URL, holding `token`
  (see dk.cst.corpus-probe.query.tokens/form-tokens) over `attrs` and
  `value-lists` (see `condition-row`). A group of its own, named by
  number: its conditions as an ordered list, since each joins the ones
  before it, then the repeat (see `repeat-fields`) and the edges (see
  `edge-boxes`), and, where `client?`, the buttons adding a condition
  and taking the token away (see `token-actions`). The repeat and the
  edges dispatch `:set-token` with their field, as the conditions'
  controls do theirs.

  The first condition's value is required when `required?`; the others'
  only where the client runs, which is where a condition is added, so a
  reader without it can empty a condition to be rid of it. A condition
  can be taken away while the token has another."
  [ui attrs value-lists client? required? i
   {:keys [id conditions start end] lo :min hi :max}]
  (let [conditions (or (seq conditions) [{:id 1}])
        any?       (= "any" (:op (first conditions)))
        token      {:i           i
                    :attrs       attrs
                    :value-lists value-lists
                    :any?        any?
                    :removable?  (and client? (boolean (next conditions)))}]
    [:fieldset.token-box.box
     [:legend (str (i18n/tr ui "Token") " " i)]
     [:ol
      (map-indexed (fn [j condition]
                     (let [c (inc j)]
                       (condition-row ui token
                                      (and required? (or (= 1 c) client?))
                                      c condition)))
                   conditions)]
     [:p (repeat-fields ui i lo hi) " " (edge-boxes ui i start end)]
     (when client? (token-actions ui i id any?))]))

(defn token-fieldset
  "The tokens of the extended search in `ui`: one group per token of
  `tokens` (see `token-box`) over `attrs` and `value-lists`, with the
  buttons editing them where `client?` runs, as an ordered list inside a
  group of their own, since a token is one of a sequence and a screen
  reader says which, with the datalists the value fields draw on (see
  `value-list-id`). One blank token when there are none, since the
  client may have just switched to the mode; otherwise
  the tokens are the search's own plus the blank one the server ends
  them in (see dk.cst.corpus-probe.query/form-rows), so a reader
  without the client adds a token by filling it and searching again.

  When `required?`, every token must be filled but that blank last one,
  which only a reader without the client sees: the client drops it (see
  dk.cst.corpus-probe.query.tokens/own-rows) and adds tokens by a button,
  so with it every token must be filled, and a token added and left empty
  is reported rather than silently dropped. A lone token must always be,
  or an extended search of nothing could be sent.

  Each list item is keyed by the token's :id rather than its place, so
  that taking a token away leaves what was typed in the ones after it."
  [ui attrs value-lists client? required? tokens]
  (let [rows (or (seq tokens) [(tokens/blank-token 1)])
        n    (count rows)]
    [:fieldset.tokens {:aria-label (i18n/tr ui "Extended search")}
     (for [[attr values] (sort value-lists)]
       [:datalist {:id (value-list-id attr)}
        (for [value values] [:option {:value value}])])
     [:ol
      (map-indexed (fn [i {:keys [id] :as token}]
                     (let [i (inc i)]
                       [:li {:replicant/key id}
                        (token-box ui attrs value-lists client?
                                   (and required? (or client? (= n 1) (< i n)))
                                   i token)]))
                   rows)]]))

(defn add-token-row
  "The row under the tokens holding `submit`, the search button, and
  before it, where `client?` runs to answer it, the button adding a
  token, in `ui`."
  [ui client? submit]
  [:p
   (when client?
     (list [:button {:type "button" :on {:click [:add-token]}}
            (i18n/tr ui "Add token")]
           " "))
   submit])
