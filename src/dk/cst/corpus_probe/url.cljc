(ns dk.cst.corpus-probe.url
  "The app's URLs: the path of each page, and the one query string a
  search has.

  A result URL is a citation, so the server and the client build it by
  the same rule, `canonical` then `query-string`: the params in a fixed
  order, every default left out, the corpora as one comma-joined param,
  and no corpus named when every readable one is chosen."
  (:require [clojure.string :as str])
  #?(:clj (:import [java.net URLEncoder])))

(def home
  "The frontpage."
  "/")

(def search
  "The search page: the form, and the result of what it asked."
  "/search")

(def corpora
  "The corpus index; each corpus is a page under it (see `corpus`)."
  "/corpora")

(defn corpus
  "The URL of the corpus page of `id`, under `corpora`."
  [id]
  (str corpora "/" (str/lower-case id)))

(def hit-id
  "The id of the hit marked on a reading page, which its URL lands on."
  "hit")

(defn text
  "The URL of the reading page of the text of corpus `id` holding
  corpus position `cpos`, the hit from `cpos` to `matchend` marked and
  landed on: `/corpora/viser/text?cpos=9&matchend=10#hit`. A hit of one
  token names no end."
  [id cpos matchend]
  (str (corpus id) "/text?cpos=" cpos
       (when (and matchend (not= cpos matchend)) (str "&matchend=" matchend))
       "#" hit-id))

(def glossary
  "The glossary."
  "/glossary")

(defn glossary-entry
  "The URL of the glossary entry with id `id`."
  [id]
  (str glossary "#" id))

(defn export
  "The path of the `view` of a result (`:kwic` or `:frequencies`) as a
  file in `format`: `/search/kwic.tsv`."
  [view format]
  (str search "/" (name view) "." (name format)))

(def results-id
  "The id of the results region a search lands on."
  "results")

(def results-fragment
  "The fragment every form action and every link to a result ends in, so
  a submit or a page turn lands the reader on the answer rather than at
  the top of the form that asked for it. Named once, so the URLs and the
  region they name cannot drift apart."
  (str "#" results-id))

(def defaults
  "What each param means when a URL leaves it out, so the value no URL
  carries. Each is the default its reader in dk.cst.corpus-probe.api or
  .query applies, restated as the string a URL would carry;
  `defaults-test` holds the two together."
  {:mode        "simple"
   :in          "word"
   :within      "sentence"
   :sort        "corpus"
   :context     "5"
   :distance    "5"
   :subset-at   "match"
   :subset-attr "word"
   :view        "kwic"
   :attr        "word"
   :at          "match"
   :page        "1"})

(def modes
  "The query modes, in display order, each as its `mode` param value: words
  in order, a list of words, a builder of tokens, and CQP as the reader
  wrote it (see dk.cst.corpus-probe.query/->cqp). The first is the default,
  which no URL carries."
  ["simple" "list" "extended" "cqp"])

(def fields
  "What each of the `modes` reads of the query params: the keys that say
  what was asked, by the mode that reads them. `::tokens` stands for the
  fields of an extended search's tokens (see `token-key?`). A param outside
  its mode's set says nothing to the search, so a URL does not carry it
  (see `canonical`), and the form's control for it is disabled (see
  dk.cst.corpus-probe.views.page/search-form)."
  {"simple"   #{:q :in :ci :match :within}
   "list"     #{:q :in :ci :match}
   "extended" #{::tokens :within}
   "cqp"      #{:q}})

(def operators
  "The operators of an extended-search condition, in display order: how
  the value of the condition's attribute must relate to what the reader
  typed, each as its `op` param value. `any`, which matches any word, is
  a token's first condition or none of them. Compiled by
  dk.cst.corpus-probe.query/condition->cqp; what each is called is the
  interface's business (see
  dk.cst.corpus-probe.views.page/operator-label)."
  ["is" "not" "prefix" "suffix" "infix" "regex" "not-regex" "any"])

(def joins
  "How a condition after a token's first joins the ones before it: `and`
  opens a new group, `or` adds an alternative to the current one, as
  KORP's builder has it (see dk.cst.corpus-probe.query/token->cqp)."
  ["and" "or"])

(def units
  "The units of text a search of several tokens can be kept within, in
  display order, each as its `within` param value (see
  dk.cst.corpus-probe.search/units)."
  ["sentence" "paragraph" "text"])

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
  "Token `rows` (see `token-rows`) as the
  extended-search form shows them: tokens and their conditions numbered
  afresh under :id (see `numbered`), which the client keeps them apart
  by as they are added and taken away."
  [rows]
  (numbered (map #(update % :conditions numbered :c) rows) :n))

(defn default
  "The value param key `k` has when a URL leaves it out (see `defaults`,
  and `token-defaults` for the field of a token); nil for a key that has
  none."
  [k]
  (if-let [[_ _ field] (token-field k)]
    (get token-defaults field)
    (get defaults k)))

(def param-order
  "Every param a search URL may carry, in the order it carries them: what
  was asked, where, which hits were kept, how they are shown, the page.
  A param not named here is dropped from every URL the app builds.
  `::tokens` stands for the fields of an extended search's tokens (see
  `token-key?`) and `::filter` for the metadata filter's params (see
  `metadata-key?`)."
  [:q :mode ::tokens :within :in :ci :match
   :corpus :scope ::filter
   :near :distance :subset :subset-at :subset-attr :sample
   :view :sort :context :attr :at :by :docs
   :page :expand])

(defn metadata-key?
  "True when param key `k` names part of the metadata filter: a chosen
  value (`f.text_year`), a pattern (`fp.`) or a bound of a range (`ff.`
  and `ft.`), each followed by the attribute name."
  [k]
  (boolean (and k (re-matches #"f[pft]?\..+" (name k)))))

(defn rank
  "Where param key `k` sorts in a query string: its place in
  `param-order`, then its name, so the metadata filter's params and the
  tokens' fields each keep one order among themselves; -1 for a key the
  order lacks, which `known?` refuses."
  [k]
  (let [k* (cond
             (metadata-key? k) ::filter
             (token-key? k)    ::tokens
             :else             k)]
    [(.indexOf param-order k*) (name k)]))

(defn known?
  "True when param key `k` is one the app reads, and so one a URL it
  builds should carry."
  [k]
  (and k (not (neg? (first (rank k))))))

(defn corpora-param
  "The corpus names selected by the `corpus` query param value `v`: a
  string (one name, or several joined by commas as in Korp URLs) or a
  vector of such strings when the param repeats. Names are uppercased
  and deduplicated; nothing is validated here, an unknown or hostile
  name is reported by the search as that corpus's error."
  [v]
  (->> (if (vector? v) v [v])
       (mapcat #(str/split (str %) #","))
       (remove str/blank?)
       (map str/upper-case)
       (distinct)
       (vec)))

(defn present
  "Param value `v` with what says nothing taken out: a blank string is
  nil, a vector keeps its non-blank strings and is nil without any."
  [v]
  (if (vector? v)
    (not-empty (filterv (complement str/blank?) (map str v)))
    (when-not (str/blank? (str v)) (str v))))

(defn with-corpora
  "The search `params` with their corpus selection as one param: the
  `corpora-param` names comma-joined, or none when they are every corpus
  of the set `all`, since naming none searches them all.

  The `scope` marker survives only where it means something: a selection
  the reader emptied, which is the one case where naming no corpus does
  not mean every corpus."
  [params all]
  (let [corpora (corpora-param (:corpus params))
        all?    (and (seq corpora) (= (set corpora) (set all)))]
    (cond-> (dissoc params :corpus :scope)
      (and (seq corpora) (not all?))
      (assoc :corpus (str/join "," corpora))

      (and (empty? corpora) (contains? params :scope))
      (assoc :scope "chosen"))))

(defn without-orphans
  "Canonical `params` less a param that only qualifies one that is not
  there: a distance without a word to be near, the anchor and attribute
  of a subset without a value, and the fields of an extended-search
  token that asks for nothing (see `asks?`) or of a condition that does
  not (see `condition-asks?`)."
  [params]
  (let [rows (token-rows params)
        idle (into #{} (comp (remove asks?) (map :n)) rows)
        ;; a condition asking nothing, of a token that asks
        blank (into #{} (for [{:keys [n conditions]} rows
                              {:keys [c] :as condition} conditions
                              :when (not (condition-asks? condition))]
                          [n c]))
        orphan? (fn [[k _]]
                  (when-let [[n c] (token-field k)]
                    (or (idle n) (blank [n c]))))]
    (cond-> (into {} (remove orphan?) params)
      (nil? (:near params))   (dissoc :distance)
      (nil? (:subset params)) (dissoc :subset-at :subset-attr))))

(defn mode
  "The mode of the search `params`: the one of the `modes` their `mode`
  param names, or the default when it names none or one the app does
  not know."
  [params]
  (let [m (:mode params)]
    (if (contains? fields m) m (:mode defaults))))

(defn query-key?
  "True when param key `k` says what was asked: the mode, a key some mode
  reads (see `fields`) or the field of a token."
  [k]
  (boolean (and k (not= ::tokens k)
                (or (= :mode k)
                    (some #(contains? % k) (vals fields))
                    (token-key? k)))))

(defn reads?
  "True when mode `m` reads param key `k` (see `fields`): the mode itself,
  one of the mode's keys or, where it reads tokens, the field of one.
  The marker standing for the tokens is no key of its own."
  [m k]
  (let [own (get fields m)]
    (boolean (and (not= ::tokens k)
                  (or (= :mode k)
                      (contains? own k)
                      (and (contains? own ::tokens) (token-key? k)))))))

(defn read-keys
  "The query params among `params` that the form of `mode` reads (see
  `reads?`), as keys, the mode itself aside: what a form holds of a
  query, and so what its query replaces when it changes."
  [mode params]
  (filter #(and (query-key? %) (not= :mode %) (reads? mode %))
          (keys params)))

(defn unread
  "The keys of the query params among `params` that their mode (see
  `mode`) does not read: what the form of another mode, or a hand-written
  URL, carried along, which the search never sees."
  [params]
  (let [m (mode params)]
    (into #{}
          (filter #(and (query-key? %) (not (reads? m %))))
          (keys params))))

(defn without-unread
  "`params` less what their mode does not read (see `unread`)."
  [params]
  (apply dissoc params (unread params)))

(defn unread-query?
  "True when `params` carry a query their mode does not read (see
  `unread`) that says something: a q with words in it under the extended
  mode, or a token that asks (see `asks?`) under any other. What the
  form submits when its mode radio is changed before the query is
  retyped, and what a hand-written URL may carry; the blank field or the
  blank trailing token every form submits is no query."
  [params]
  (let [unread (unread params)]
    (boolean (or (and (unread :q) (present (:q params)))
                 (and (some token-key? unread)
                      (some asks? (token-rows params)))))))

(defn canonical
  "The search `params` (param keys to their string or vector values, as a
  request or a form carries them) as the URL cites them, against the set
  `all` of every corpus that can be searched: nothing nil, blank or
  default (see `defaults`), nothing that qualifies an absent param (see
  `without-orphans`), nothing the mode does not read (see
  `without-unread`), nothing the app does not read (see `known?`), and
  the corpora as one param (see `with-corpora`).

  Applying it to its own result changes nothing, so a link can be built
  from canonical params and canonicalised again."
  ([params]
   (canonical params nil))
  ([params all]
   (->> (with-corpora params all)
        (keep (fn [[k v]]
                (let [v (present v)]
                  (when (and (known? k) v (not= v (default k)))
                    [k v]))))
        (into {})
        (without-orphans)
        (without-unread))))

(defn pairs
  "Canonical `params` as [name value] string pairs in `param-order`, a
  vector value one pair per element."
  [params]
  (for [[k v] (sort-by (comp rank key) params)
        v     (if (vector? v) v [v])]
    [(name k) (str v)]))

(defn query-string
  "The query string of search `params`, canonicalised: the `pairs`
  form-encoded, which is how a browser encodes a GET submit, so a URL
  the app builds and one the browser built from the same form are the
  same string.

  The comma and the colon are put back after encoding: RFC 3986 allows
  both in a query, every decoder reads them the same, and they separate
  the corpora and the expanded hits a reader should be able to read in
  the bar."
  [params]
  (let [pairs (pairs (canonical params))]
    (-> #?(:clj  (str/join "&" (map (fn [[k v]]
                                      (str k "=" (URLEncoder/encode
                                                  ^String v "UTF-8")))
                                    pairs))
           :cljs (.toString (js/URLSearchParams. (clj->js pairs))))
        (str/replace "%2C" ",")
        (str/replace "%3A" ":"))))

(defn search-href
  "The URL of the search page for `params`: the page itself when they
  say nothing."
  [params]
  (let [qs (query-string params)]
    (cond-> search (seq qs) (str "?" qs))))

(defn results-href
  "`search-href` ending in the `results-fragment`: the URL of a result,
  which a link should land on."
  [params]
  (str (search-href params) results-fragment))

(defn export-href
  "The URL of the `view` of the search `params` describe as a file in
  `format` (see `export`)."
  [view format params]
  (let [qs (query-string params)]
    (cond-> (export view format) (seq qs) (str "?" qs))))

(comment
  (canonical {:q "hund" :mode "simple" :in "word" :corpus ["PROBE" "VISER"]
              :scope "chosen" :sort "corpus" :sample "" :distance "5"}
             #{"PROBE" "VISER" "TALER"})
  ;; => {:q "hund", :corpus "PROBE,VISER"}

  (results-href {:q "[lemma = \"hund\"]" :mode "cqp" :page 2
                 :corpus ["PROBE" "VISER"]})
  ;; => "/search?q=%5Blemma+%3D+%22hund%22%5D&mode=cqp&corpus=PROBE,VISER&page=2#results"
  #_.)
