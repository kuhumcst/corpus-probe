(ns dk.cst.corpus-probe.query.tokens
  "The rows and fields of the extended form: the tokens of an extended
  search as its params spell them, `t2.v` for a field of a token's first
  condition and `t2.3.v` for one of its third, read into rows (see
  `token-rows`) and printed back (see `rows->params`), and the
  vocabularies the form's controls offer (see `operators`, `joins` and
  `match-ops`). Shared by the query (dk.cst.corpus-probe.query), the URL
  rule (dk.cst.corpus-probe.url) and the views."
  (:require [clojure.string :as str]))

(def operators
  "The operators of an extended-search condition, in display order: how
  the value of the condition's attribute must relate to what the reader
  typed, each as its `op` param value. `any`, which matches any word, is
  a token's first condition or none of them. Compiled by
  dk.cst.corpus-probe.query/condition->cqp; what each is called is the
  interface's business (see
  dk.cst.corpus-probe.views.search.tokens/operator-label)."
  ["is" "not" "prefix" "suffix" "infix" "regex" "not-regex" "any"])

(def joins
  "How a condition after a token's first joins the ones before it: `and`
  opens a new group, `or` adds an alternative to the current one, as
  KORP's builder has it (see dk.cst.corpus-probe.query/token->cqp)."
  ["and" "or"])

(def match-ops
  "How much of the form a word of a simple search or a list must cover,
  in display order, each as its `match` param value: the whole form
  first, which is what a URL leaves out, then its start, its end and any
  part (see dk.cst.corpus-probe.query.params/match-op)."
  ["" "prefix" "suffix" "infix"])

(def token-defaults
  "What each field of an extended-search token means when a URL leaves it
  out: the surface form, equality, a new group, once."
  {:attr "word" :op "is" :join "and" :min "1" :max "1"})

(def own-fields
  "The fields of an extended-search token that belong to the token
  itself rather than to one of its conditions: its repeat, and whether
  it must open or close a sentence."
  #{:min :max :start :end})

(defn token-field
  "The [n c field] an extended-search token param key `k` names, `t2.v`
  being [2 1 :v] and `t2.3.v` [2 3 :v]: the token's number, the number
  of the condition among its conditions (the first when the key names
  none) and one of :attr, :op, :v, :ci and :join of a condition, or
  :min, :max, :start and :end of the token. nil for any other key."
  [k]
  (when k
    (when-let [[_ n c field]
               (re-matches #"t(\d+)(?:\.(\d+))?\.(attr|op|v|ci|join|min|max|start|end)"
                           (name k))]
      [(parse-long n) (if c (parse-long c) 1) (keyword field)])))

(defn token-key
  "The param key of `field` of condition `c` of token `n`, the inverse of
  `token-field`: `t2.v` for a first condition, `t2.3.v` for a third, and
  the token's own fields under the first."
  [n c field]
  (str "t" n (when (> c 1) (str "." c)) "." (name field)))

(defn token-key?
  "True when param key `k` names a field of an extended-search token (see
  `token-field`)."
  [k]
  (some? (token-field k)))

(defn condition-asks?
  "True when extended-search `condition` (see `token-rows`) asks for
  anything: an any-word one, or one with a value."
  [{:keys [op v]}]
  (or (= "any" op) (not (str/blank? (str v)))))

(defn asks?
  "True when extended-search token `row` (see `token-rows`) asks for
  anything: one of its conditions does (see `condition-asks?`). A token
  without any is the blank one the form ends in for a reader without the
  client."
  [{:keys [conditions]}]
  (boolean (some condition-asks? conditions)))

(defn present
  "Param value `v` with what says nothing taken out: a blank string is
  nil, a vector keeps its non-blank strings and is nil without any."
  [v]
  (if (vector? v)
    (not-empty (filterv (complement str/blank?) (map str v)))
    (when-not (str/blank? (str v)) (str v))))

(defn token-rows
  "The extended-search tokens among `params`, one map per numbered token
  in numeric order: its :n, its own fields (see `own-fields`) and its
  :conditions, one map per numbered condition in numeric order with its
  :c and its fields, all as strings (see `token-field`)."
  [params]
  (->> params
       (keep (fn [[k v]]
               (when-let [[n c field] (token-field k)]
                 [n c field v])))
       (reduce (fn [m [n c field v]]
                 (update m n (fnil assoc-in (sorted-map)) [c field] v))
               (sorted-map))
       (mapv (fn [[n conditions]]
               (-> (select-keys (get conditions 1) own-fields)
                   (assoc :n n
                          :conditions
                          (mapv (fn [[c fields]]
                                  (assoc (apply dissoc fields own-fields)
                                         :c c))
                                conditions)))))))

(defn numbered
  "`rows` with :id 1, 2 and so on in order, their `key` (:n of a token,
  :c of a condition) dropped."
  [rows key]
  (into []
        (map-indexed (fn [i row] (assoc (dissoc row key) :id (inc i))))
        rows))

(defn blank-token
  "The blank token numbered `id` the form starts with and ends in: one
  condition asking nothing (see `form-tokens`)."
  [id]
  {:id id :conditions [{:id 1}]})

(defn form-tokens
  "Token `rows` (see `token-rows`) as the extended-search form shows
  them: tokens and their conditions numbered afresh under :id (see
  `numbered`), which the client keeps them apart by as they are added
  and taken away."
  [rows]
  (numbered (map #(update % :conditions numbered :c) rows) :n))

(defn rows->params
  "The params of the extended form's `rows` (see `form-tokens`), as the
  form would submit them, the inverse of `token-rows` for rows numbered
  by their place: each condition's fields under its token's number and
  its own, and the token's own fields (see `own-fields`) under its first
  condition, present fields only."
  [rows]
  (into {}
        (mapcat (fn [n {:keys [conditions] :as row}]
                  (concat (for [[field v] (select-keys row own-fields)
                                :when (some? v)]
                            [(keyword (token-key n 1 field)) v])
                          (mapcat (fn [c condition]
                                    (for [[field v] (dissoc condition :id)
                                          :when (some? v)]
                                      [(keyword (token-key n c field)) v]))
                                  (map inc (range))
                                  conditions)))
                (map inc (range))
                rows)))

(defn own-rows
  "The token `rows` of the extended form (see `form-tokens`) as a client
  shows them: less the blank last one the server ends them in (see
  dk.cst.corpus-probe.query/form-rows) for a reader without a client,
  who has no button to add one. Kept when it is the only one."
  [rows]
  (if (and (next rows) (not (asks? (last rows))))
    (vec (butlast rows))
    rows))
