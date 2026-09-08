(ns dk.cst.corpus-probe.query.params
  "The readers and writers of the query params: each `-param` reads one
  param's value into what the query holds, `token-params` reads the
  tokens of an extended search out of them, and the `->params` print a
  condition, a token or the words of a query back as the params its form
  submits, nothing at its default."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.cqp :as cqp]
            [dk.cst.corpus-probe.query.mode :as mode]
            [dk.cst.corpus-probe.query.tokens :as tokens]))

(defn match-op
  "The operator the `match` query param value `v` gives every word of a
  simple search or a list: one of the match-ops, or equality for the
  whole form, which is what a URL leaves out."
  [v]
  (if (some #{v} (remove str/blank? tokens/match-ops)) v "is"))

(defn repeat-param
  "The repeat query param value `v` as a number of tokens: an integer
  from 0 to 99, else `default`."
  [v default]
  (let [n (some-> v str parse-long)]
    (if (and n (<= 0 n 99)) n default)))

(defn within-param
  "The unit of text the `within` query param value `v` names (see
  dk.cst.corpus-probe.cqp/units): the sentence unless it names another."
  [v]
  (if (some #{v} (map (comp name first) cqp/units)) (keyword v) :sentence))

(defn condition-params
  "The condition `row` of an extended-search token as the compiler takes
  it: its :attr, its :op, its :value as typed, :ci? for its box and its
  :join as typed, which `joined` reads by its place among its token's
  conditions."
  [{:keys [attr op v ci join]}]
  ;; spliced into the query, so only a name CQP's lexer takes is kept
  {:attr  (keyword (if (cqp/name? (str attr)) attr "word"))
   :op    (if (some #{op} tokens/operators) op "is")
   :value (str v)
   :ci?   (some? ci)
   :join  join})

(defn joined
  "Read the :join of each of `conditions` (see `condition-params`) after
  the first as one of the joins, and as `and` where it is not one; the
  first has none, since it joins nothing."
  [conditions]
  (into []
        (map-indexed (fn [i {:keys [join] :as condition}]
                       (if (zero? i)
                         (dissoc condition :join)
                         (assoc condition
                                :join (if (some #{join} tokens/joins)
                                        join
                                        "and")))))
        conditions))

(defn token-params
  "The tokens the extended search `params` describe, as the compiler
  takes them: each token that asks for anything, in order, with the
  :conditions of it that ask, its repeat as :min and :max, the most
  never below the least, and :start? and :end? for the sentence edges."
  [params]
  (into []
        (comp (filter tokens/asks?)
              (map (fn [{:keys [conditions start end] lo :min hi :max}]
                     (let [lo (repeat-param lo 1)]
                       {:conditions (joined
                                     (map condition-params
                                          (filter tokens/condition-asks?
                                                  conditions)))
                        :min        lo
                        :max        (max lo (repeat-param hi lo))
                        :start?     (some? start)
                        :end?       (some? end)}))))
        (tokens/token-rows params)))

(defn condition->params
  "The fields of `condition`, the `c`th of token `n`, as the extended
  form submits them and its URL carries them, nothing at its default."
  [n c {:keys [attr op value ci? join]}]
  (let [k (fn [field] (keyword (tokens/token-key n c field)))]
    (cond-> {}
      (not= (:attr tokens/token-defaults) (name attr))
      (assoc (k :attr) (name attr))

      (not= (:op tokens/token-defaults) op)
      (assoc (k :op) op)

      (not (str/blank? value))
      (assoc (k :v) value)

      ci?
      (assoc (k :ci) "on")

      (and (> c 1) (not= (:join tokens/token-defaults) join))
      (assoc (k :join) join))))

(defn token->params
  "The fields of `token`, the `n`th of an extended search, as its form
  submits them and its URL carries them: those of its conditions, its
  repeat and its sentence edges, nothing at its default."
  [n {:keys [conditions start? end?] lo :min hi :max}]
  (let [k (fn [field] (keyword (tokens/token-key n 1 field)))]
    (cond-> (into {}
                  (map-indexed (fn [i c]
                                 (condition->params n (inc i) c)))
                  conditions)
      (not= 1 lo) (assoc (k :min) (str lo))
      (not= 1 hi) (assoc (k :max) (str hi))
      start?      (assoc (k :start) "on")
      end?        (assoc (k :end) "on"))))

(defn word-params
  "The params of the words of `query` as a simple search or a list
  spells them, by `mode`: the values of its tokens in order, or of its
  one token's alternatives one line each, in the field, with the first
  condition's attribute, operator and case flag as the options every
  word shares."
  [mode {:keys [tokens within]}]
  (let [{:keys [attr op ci?] :or {attr :word op "is"}}
        (first (:conditions (first tokens)))

        list?  (= "list" mode)
        values (if list?
                 (map :value (:conditions (first tokens)))
                 (map (comp :value first :conditions) tokens))]
    (cond-> {:q (str/join (if list? "\n" " ") values)}
      (not= (:in mode/defaults) (name attr)) (assoc :in (name attr))
      (not= "is" op)                         (assoc :match op)
      ci?                                    (assoc :ci "on")
      (and (not list?) (not= :sentence within))
      (assoc :within (name within)))))
