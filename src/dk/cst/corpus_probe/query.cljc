(ns dk.cst.corpus-probe.query
  "The query a search asks, as one value: the tokens of an extended
  search kept within a unit of text, which the words of a simple search
  and the alternatives of a list are too; or CQP as the reader wrote it,
  which the app compiles into and never out of. Read from the params by
  `of`, printed back by `->params`, compiled by `->cqp` and held by a
  form as far as it can, `project` and `loss`."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.cqp :as cqp]
            [dk.cst.corpus-probe.query.mode :as mode]
            [dk.cst.corpus-probe.query.params :as params]
            [dk.cst.corpus-probe.query.tokens :as tokens]))

(def literal-ops
  "The operators that match an escaped literal as an equality with
  affixes: the ones a simple search or a list has, and the ones
  alternatives on one attribute compile as one alternation."
  #{"is" "prefix" "suffix" "infix"})

(defn affixed
  "`literal`, escaped already, with the affixes of the literal operator
  `op`: `.*` after it for prefix, before it for suffix, both for infix."
  [op literal]
  (case op
    "prefix" (str literal ".*")
    "suffix" (str ".*" literal)
    "infix"  (str ".*" literal ".*")
    literal))

(defn condition->cqp
  "The CQP condition of extended-search `condition`: its :attr related by
  :op to its :value, escaped and affixed as a literal or kept as written
  under a regex operator, with the %c flag under :ci?.

  (condition->cqp {:attr :lemma :op \"is\" :value \"hund\" :ci? true})
  ;; => lemma = \"hund\" %c"
  [{:keys [attr op value ci?] :or {attr :word op "is"}}]
  (str (name attr)
       (if (#{"not" "not-regex"} op) " != \"" " = \"")
       (if (#{"regex" "not-regex"} op)
         (cqp/regex-value value)
         (affixed op (cqp/escape-value (str value))))
       "\""
       (when ci? " %c")))

(defn literal-shape
  "What `condition` matches its value with: its attribute, its operator
  and its case flag, defaults filled in, so that two conditions of one
  shape differ in their value alone."
  [{:keys [attr op ci?]}]
  [(or attr :word) (or op "is") (boolean ci?)])

(defn alternatives?
  "True when `conditions`, one group of a token, are alternatives of one
  literal value: two or more, all of one `literal-shape` with an operator
  in `literal-ops`. What a list is, and what compiles as one alternation."
  [conditions]
  (let [[_ op] (literal-shape (first conditions))]
    (boolean (and (next conditions)
                  (contains? literal-ops op)
                  (apply = (map literal-shape conditions))))))

(defn alternation->cqp
  "The CQP condition of `conditions` that are `alternatives?` of one
  literal value: the escaped values as one alternation, affixed and
  flagged as the one condition they amount to.

  (alternation->cqp [{:attr :lemma :op \"prefix\" :value \"hund\"}
                     {:attr :lemma :op \"prefix\" :value \"kat\" :join \"or\"}])
  ;; => lemma = \"(hund|kat).*\""
  [conditions]
  (let [[attr op ci?] (literal-shape (first conditions))
        values        (map #(cqp/escape-value (str (:value %))) conditions)]
    (str (name attr) " = \""
         (affixed op (str "(" (str/join "|" values) ")"))
         "\""
         (when ci? " %c"))))

(defn condition-groups
  "The `conditions` of a token in the groups their :join makes: the first
  opens the first group, each `or` adds an alternative to the current one
  and anything else opens a new one."
  [conditions]
  (reduce (fn [groups {:keys [join] :as condition}]
            (if (and (seq groups) (= "or" join))
              (update groups (dec (count groups)) conj condition)
              (conj groups [condition])))
          []
          conditions))

(defn group->cqp
  "The CQP of one group of `conditions` (see `condition-groups`): one
  alternation when they are `alternatives?`, else each condition joined
  by |, in parentheses when `grouped?` and there are several, so that the
  ors bind tighter than the ands, as KORP reads them and the reverse of
  CQP's own."
  [conditions grouped?]
  (if (alternatives? conditions)
    (alternation->cqp conditions)
    (let [alts (str/join " | " (map condition->cqp conditions))]
      (if (and grouped? (next conditions)) (str "(" alts ")") alts))))

(defn token->cqp
  "The CQP token pattern of extended-search `token`: its :conditions in
  the groups their joins make (see `group->cqp`), joined by &; any word
  when the first is `any`; repeated :min to :max times when that is not
  once; and opening or closing a sentence under :start? and :end?.

  (token->cqp {:conditions
               [{:attr :lemma :op \"is\" :value \"hund\"}
                {:join \"or\" :attr :lemma :op \"is\" :value \"kat\"}
                {:join \"and\" :attr :pos :op \"prefix\" :value \"N\"}]})
  ;; => [lemma = \"(hund|kat)\" & pos = \"N.*\"]

  (token->cqp {:conditions [{:op \"any\"}] :min 0 :max 2})
  ;; => []{0,2}"
  [{:keys [conditions start? end?] lo :min hi :max :or {lo 1 hi 1}}]
  (let [groups (condition-groups conditions)
        body   (when-not (= "any" (:op (first conditions)))
                 (str/join " & " (map #(group->cqp % (boolean (next groups)))
                                      groups)))]
    ;; each corpus renames the sentence tags after its own attribute
    ;; (see dk.cst.corpus-probe.cwb.command/sentence-tags)
    (str (when start? "<s> ")
         "[" body "]"
         (when-not (= [1 1] [lo hi])
           (str "{" lo "," hi "}"))
         (when end? " </s>"))))

(defn extended->cqp
  "Compile the extended-search `tokens` (see `token->cqp`) into a CQP
  query string: one token pattern each, in order; nil without tokens.

  (extended->cqp [{:conditions [{:attr :pos :op \"prefix\" :value \"N\"}]}
                  {:conditions [{:op \"any\"}] :max 2}
                  {:conditions [{:attr :word :op \"is\" :value \"hund\"}]}])
  ;; => [pos = \"N.*\"] []{1,2} [word = \"hund\"]"
  [tokens]
  (when (seq tokens)
    (str/join " " (map token->cqp tokens))))

(defn condition
  "The condition matching `word` as the options of a simple search or a
  list say: the `in` attribute, the `match` operator and the `ci` flag."
  [{:keys [in ci match]} word]
  {:attr  (keyword (or (tokens/present in) (:in mode/defaults)))
   :op    (params/match-op match)
   :value word
   :ci?   (some? ci)})

(defn token
  "A token of `conditions`, once, at no sentence edge: what a word of a
  simple search or the alternatives of a list are."
  [conditions]
  {:conditions (vec conditions) :min 1 :max 1 :start? false :end? false})

(defn list-token
  "The one token a list is: `conditions` as alternatives of one another,
  each once, every one after the first joined by or."
  [conditions]
  (token (map-indexed (fn [i c] (cond-> c (pos? i) (assoc :join "or")))
                      (distinct conditions))))

(defn words
  "The words of `q`, whitespace-separated as a search box or a list holds
  them; none for a blank."
  [q]
  (remove str/blank? (str/split (str q) #"\s+")))

(defn of
  "The query the search `params` carry, read by their mode: CQP as typed,
  `{:cqp text}`; or tokens kept within a unit, `{:tokens [...] :within
  :sentence}`, the tokens of an extended search, the words of a simple
  search one token each, or the words of a list as one token of
  alternatives. Nil when nothing is asked, which counts every token."
  [{:keys [q within] :as params}]
  (let [mode   (mode/mode params)
        unit   (params/within-param (when (mode/reads? mode :within) within))
        tokens (fn [tokens]
                 (when (seq tokens) {:tokens (vec tokens) :within unit}))
        words  (words q)]
    (case mode
      "cqp"      (when-not (str/blank? q) {:cqp q})
      "extended" (tokens (params/token-params params))
      "list"     (tokens (when (seq words)
                           [(list-token (map #(condition params %) words))]))
      (tokens (map #(token [(condition params %)]) words)))))

(defn ->cqp
  "The CQP of `query` (see `of`): the text as typed, or the tokens
  compiled (see `extended->cqp`); nil for no query."
  [{:keys [cqp tokens]}]
  (or cqp (extended->cqp tokens)))

(defn within
  "The unit of text `query` (see `of`) is kept within: its unit for two
  or more tokens, or for a token that opens or closes a sentence; nil for
  one token, which cannot straddle a boundary and could only be refused
  where it stands outside every sentence, and nil for CQP, which says so
  itself."
  [{:keys [tokens within]}]
  (when (or (next tokens) (some #(or (:start? %) (:end? %)) tokens))
    within))

(defn ->params
  "The search params of `mode` that carry `query`, as the form of that
  mode submits them and its URL cites them: nothing at its default, and
  no mode, which the shape of the text says. The inverse of `of` for a
  query the form holds (see `project`)."
  [mode query]
  (cond
    (nil? query)         {}
    (= "cqp" mode)       {:q (->cqp query)}
    (= "extended" mode)  (let [{:keys [tokens within]} query
                               fields (map-indexed (fn [i t]
                                                     (params/token->params
                                                      (inc i) t))
                                                   tokens)]
                           (cond-> (into {} fields)
                             (not= :sentence within)
                             (assoc :within (name within))))
    :else                (params/word-params mode query)))

(def max-alternatives
  "The most words a list is carried into the extended form as, one
  condition each: as many rows of five controls as one token can show
  before the form is a page of its own. A longer list stays in the
  field, which holds any number (see `loss`)."
  50)

(defn loss
  "What the form of `mode` cannot hold of `query`, as items the interface
  words: nothing for the field, which holds every query, as CQP if not
  as words; for the extended form, a list past `max-alternatives`,
  `[:list n]`, and CQP, `[:cqp text]`, which the app never reads."
  [mode {:keys [cqp tokens] :as query}]
  (cond
    (or (nil? query) (not= "extended" mode)) []
    cqp [[:cqp cqp]]
    :else (into [] (keep (fn [{:keys [conditions]}]
                           (when (> (count conditions) max-alternatives)
                             [:list (count conditions)])))
                tokens)))

(defn project
  "`query` as the form of `mode` holds it (see `loss`): the field, under
  the CQP mode, holds CQP as it is and the tokens compiled, kept within
  their unit by name; the extended form holds no CQP and no list past
  `max-alternatives`, and starts blank, nil. Nil for no query."
  [mode {:keys [cqp] :as query}]
  (cond
    (nil? query)        nil
    (= "extended" mode) (when (empty? (loss mode query)) query)
    cqp                 query
    :else               {:cqp (str (->cqp query)
                                   (some->> (within query)
                                            (get (into {} cqp/units))
                                            (str " within ")))}))

(defn form-rows
  "The rows of the extended form holding `query` (see `project`): its
  tokens as the form shows them, then one blank token, which is how a
  reader without the client adds one, and which the client drops."
  [query]
  (tokens/form-tokens (concat (tokens/token-rows (->params "extended" query))
                              [{:conditions [{}]}])))

(defn arrived
  "What the search `params` of a submitted form ask, once a change of its
  mode radio is allowed for: the `:form` shown, the mode the query came
  `:from` when the radio changed it, the query `:held` by the form, the
  `:query` that runs, what the form could not keep as `:loss` and, for a
  hand-written URL, the `:unread` keys it carried.

  A form whose radio was changed submits the previous form's query under
  the new radio: the field's text seeds the tokens, read by its shape,
  and the tokens are handed to the field as CQP."
  [params]
  (let [form   (mode/form params)
        own    (of params)
        origin (mode/typed params)
        ;; a form with a query of its own is no switch, whatever else the
        ;; params carry
        switch (when (and (nil? own) origin (not= form (mode/form-of origin)))
                 (if (= "extended" form) :in :out))
        other  (when switch (of (dissoc params :mode)))
        mode   (case switch :in "extended" :out "cqp" (mode/mode params))
        held   (if switch (project mode other) own)
        loss   (if switch (loss mode other) [])]
    {:form   mode
     :from   (when switch origin)
     :held   held
     ;; nothing runs when part of the query was lost, so that the reader
     ;; is told before the loss
     :query  (when (empty? loss) held)
     :loss   loss
     ;; what arrived with a switch is the switch's own business
     :unread (if switch #{} (mode/unread params))}))
