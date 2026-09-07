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

(defn condition-row
  "One condition of the extended search in `ui`: condition `c`, counted
  from one, holding `condition` (see
  dk.cst.corpus-probe.query.tokens/form-tokens), of `token`, the facts of
  the token it belongs to: its number `:i`, the `:attrs` a condition may
  name (see `attribute-options`), the `:value-lists` some of them offer
  (see `value-list-id`), `:any?` when the token's first condition
  matches any word, and `:removable?` when a button may take the
  condition away. The attribute, the operator, the value and the
  ignore-case box, headed by how it joins the conditions before it when
  it is not the first.

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
  [ui {:keys [i attrs value-lists any? removable?]} required? c
   {:keys [id attr op v ci join]}]
  (let [param  (fn [field] (tokens/token-key i c field))
        first? (= 1 c)
        op     (or op "is")
        dead?  (and any? (not first?))
        attr*  (keyword (if (str/blank? attr) "word" attr))
        joined (fn [j] (case j "or" (i18n/tr ui "or") (i18n/tr ui "and")))]
    [:li.condition (cond-> {:replicant/key id}
                     removable? (assoc :class "removable"))
     (when-not first?
       (list [:select.condition-join
              {:name       (param :join)
               :aria-label (i18n/tr ui "joined by")
               :disabled   dead?
               :on         {:change [:set-condition [i id :join]
                                     :event.target/value]}}
              (for [j tokens/joins]
                (widgets/option (or join "and") j (joined j)))]
             " "))
     [:select {:name       (param :attr)
               :aria-label (i18n/tr ui "attribute")
               :disabled   any?
               :on         {:change [:set-condition [i id :attr]
                                     :event.target/value]}}
      (attribute-options ui attrs attr)]
     " "
     [:select {:name       (param :op)
               :aria-label (i18n/tr ui "operator")
               :disabled   dead?
               :on         {:change [:set-condition [i id :op]
                                     :event.target/value]}}
      (for [o (cond->> tokens/operators (not first?) (remove #{"any"}))]
        (widgets/option op o (operator-label ui o)))]
     " "
     [:input.condition-value
      (cond-> {:type         "text"
               :name         (param :v)
               :value        (or v "")
               :aria-label   (i18n/trx ui "field" "value")
               :autocomplete "off"
               :spellcheck   "false"
               :required     (and required? (not any?))
               :disabled     any?
               :on           {:input [:set-condition [i id :v]
                                      :event.target/value]}}
        (contains? value-lists attr*)
        (assoc :list (value-list-id attr*)))]
     " "
     [:label [:input {:type     "checkbox" :name (param :ci) :value "on"
                      :checked  (some? ci)
                      :disabled any?
                      :on       {:change [:set-condition [i id :ci]
                                          :event.target/checked]}}]
      (i18n/tr ui "ignore case")]
     (when removable?
       (list " "
             [:button.condition-remove
              {:type       "button"
               :aria-label (str (i18n/tr ui "Remove condition") " " c)
               :on         {:click [:remove-condition [i id]]}}
              "×"]))]))

(defn token-row
  "One token of the extended search in `ui`: token `i`, counted from
  one, which is the number its fields carry in the URL, holding `token`
  (see dk.cst.corpus-probe.query.tokens/form-tokens) over `attrs` and
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
  (let [param      (fn [field] (tokens/token-key i 1 field))
        conditions (or (seq conditions) [{:id 1}])
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
     [:p
      ;; the second number is labelled "to", which says nothing on its
      ;; own; the group says what the pair is
      [:span.token-repeat {:role "group" :aria-label (i18n/tr ui "repeat")}
       [:label (i18n/tr ui "repeat") " "
        [:input {:type "number" :name (param :min) :value (or lo "1")
                 :min  0 :max 99
                 :on   {:input [:set-token [i :min] :event.target/value]}}]]
       " "
       [:label (i18n/tr ui "to") " "
        [:input {:type "number" :name (param :max) :value (or hi "1")
                 :min  0 :max 99
                 :on   {:input [:set-token [i :max] :event.target/value]}}]]]
      " "
      [:label.token-edges
       [:input {:type    "checkbox" :name (param :start) :value "on"
                :checked (some? start)
                :on      {:change [:set-token [i :start]
                                   :event.target/checked]}}]
       (i18n/tr ui "sentence start")]
      " "
      [:label.token-edges
       [:input {:type    "checkbox" :name (param :end) :value "on"
                :checked (some? end)
                :on      {:change [:set-token [i :end]
                                   :event.target/checked]}}]
       (i18n/tr ui "sentence end")]]
     ;; the token's own actions on a row of their own, so they stay together
     (when client?
       [:p.token-actions
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
                        (token-row ui attrs value-lists client?
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
