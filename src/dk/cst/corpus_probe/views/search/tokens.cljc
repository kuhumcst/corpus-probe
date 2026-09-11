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
  usual ones in the reader's words, any other as its corpus names it."
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
  chosen, each called as `ui` calls it."
  [ui attrs selected]
  (let [selected (if (str/blank? selected) "word" selected)
        names    (map name attrs)
        ;; an attribute the list lacks is offered too, so a hand-written
        ;; URL shows what it searches rather than something else
        offered  (cond-> names
                   (not (some #{selected} names)) (concat [selected]))]
    (for [n offered]
      (widgets/option selected n (attribute-label ui n)))))

(defn operator-label
  "What the operator `op` of an extended-search token is called, in `ui`;
  equality for one it does not know."
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
  `attr` (a keyword or its name)."
  [attr]
  (str "values-" (name attr)))

(defn join-select
  "The select saying how condition `c` of `token` (its facts, see
  `condition-row`) joins the ones before it, in `ui`: the :join of
  `condition`, and when it names none; dead under the token's `:any?`."
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
  `:attrs` with the :attr of `condition` chosen; dead under `:any?`."
  [ui {:keys [i attrs any?]} c {:keys [id attr]}]
  [:select {:name       (tokens/token-key i c :attr)
            :aria-label (i18n/tr ui "attribute")
            :disabled   any?
            :on         {:change [:set-condition [i id :attr]
                                  :event.target/value]}}
   (attribute-options ui attrs attr)])

(defn operator-select
  "The select choosing the operator of condition `c` of `token` (its
  facts, see `condition-row`), in `ui`: the :op of `condition`, equality
  when it names none, any word offered to the first condition alone,
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
  "The field holding the :v of `condition`, condition `c` of `token`
  (its facts, see `condition-row`), in `ui`, `required?` or not,
  suggesting the values the token's `:value-lists` hold for its :attr;
  dead under `:any?`."
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
  a :ci; dead under `:any?`."
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
  name, the `:value-lists` some of them offer, `:any?` when the token's
  first condition matches any word, and `:removable?` when a button may
  take the condition away. The value is `required?` or not."
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
  and at most `hi` times, once each when nil."
  [ui i lo hi]
  ;; a named group: the second field's own label is only "to"
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
  "The buttons of token `i` of the extended search, with `id`, in `ui`:
  one adding a condition, dead under `any?`, since an any-word token has
  nothing else to say, and one taking the token away."
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
  one, holding `token` (see dk.cst.corpus-probe.query.tokens/form-tokens)
  over `attrs` and `value-lists`, as a group of its conditions, its
  repeat and its edges, with the buttons editing it where `client?`
  runs; its first condition's value `required?` or not."
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
     [:ol widgets/list-attrs
      (map-indexed (fn [j condition]
                     (let [c (inc j)]
                       ;; without the client a condition is removed by
                       ;; emptying it, so only the first is required there
                       (condition-row ui token
                                      (and required? (or (= 1 c) client?))
                                      c condition)))
                   conditions)]
     [:p (repeat-fields ui i lo hi) " " (edge-boxes ui i start end)]
     (when client? (token-actions ui i id any?))]))

(defn token-fieldset
  "The tokens of the extended search in `ui`: a group per token of
  `tokens` over `attrs` and `value-lists`, with the buttons editing them
  where `client?` runs, as an ordered list with the datalists the value
  fields draw on; one blank token when there are none. When `required?`
  every token must be filled but the blank last one a reader without
  the client ends them in (see dk.cst.corpus-probe.query/form-rows)."
  [ui attrs value-lists client? required? tokens]
  (let [rows (or (seq tokens) [(tokens/blank-token 1)])
        n    (count rows)]
    [:fieldset.tokens {:aria-label (i18n/tr ui "Extended search")}
     (for [[attr values] (sort value-lists)]
       [:datalist {:id (value-list-id attr)}
        (for [value values] [:option {:value value}])])
     [:ol widgets/list-attrs
      (map-indexed (fn [i {:keys [id] :as token}]
                     (let [i (inc i)]
                       ;; keyed by id, not place, so taking a token away
                       ;; keeps what was typed in the ones after it
                       [:li {:replicant/key id}
                        ;; the client drops the blank last token and adds
                        ;; tokens by a button, so with it every token is
                        ;; required; a lone token always is, or a search
                        ;; of nothing could be sent
                        (token-box ui attrs value-lists client?
                                   (and required? (or client? (= n 1) (< i n)))
                                   i token)]))
                   rows)]]))

(defn add-token-row
  "The row under the tokens holding `submit`, the search button, and
  before it where `client?` runs, the button adding a token, in `ui`."
  [ui client? submit]
  [:p
   (when client?
     (list [:button {:type "button" :on {:click [:add-token]}}
            (i18n/tr ui "Add token")]
           " "))
   submit])
