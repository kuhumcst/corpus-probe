(ns dk.cst.corpus-probe.url
  "The app's URLs: the path of each page, the ids a page lands on and its
  form carries, and the one query string a search has, which the server
  and the client build by the same rule (see `canonical`), since a result
  URL is a citation."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.query.mode :as mode]
            [dk.cst.corpus-probe.query.tokens :as tokens])
  #?(:clj (:import [java.net URLDecoder URLEncoder])))

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

(def cqp-guide
  "The CQP guide: the query language by example."
  "/cqp")

(defn export
  "The path of the `view` of a result (`:kwic` or `:frequencies`) as a
  file in `format`: `/search/kwic.tsv`."
  [view format]
  (str search "/" (name view) "." (name format)))

(def preferences
  "Where a setting is stored: a preference is not a place, so choosing one
  changes state and sends the reader back where they were."
  "/preferences")

(def cookie-max-age
  "How long a stored preference outlives the visit that set it, in
  seconds: a year, so a reader states it once."
  31536000)

(defn cookie
  "The Set-Cookie string storing `v` under setting `k` for
  `cookie-max-age`, site-wide and on same-site requests only, as the
  server writes it in a header and the client to the document."
  [k v]
  ;; a setting stored as nothing is forgotten, so storing and clearing
  ;; are one path and a reset needs no writer of its own
  (str (name k) "=" v ";Path=/"
       ";Max-Age=" (if (str/blank? (str v)) 0 cookie-max-age)
       ";SameSite=Lax"))

(def context-api
  "The data behind one hit shown with wider context, for the client."
  "/api/context")

(def filters-api
  "The metadata filters the chosen corpora offer, for the client."
  "/api/filters")

(def counts-api
  "The counts of a search still being counted when its page was served,
  for the client."
  "/api/counts")

(def results-id
  "The id of the results region a search lands on."
  "results")

(def results-fragment
  "The fragment every form action and every link to a result ends in, so
  a submit or a page turn lands the reader on the answer rather than at
  the top of the form that asked for it."
  (str "#" results-id))

(def form-id
  "The id of the search form, so a control that acts on a result can sit
  beside the result and still submit the search that produced it."
  "search-form")

(def transit-type
  "The content type the client asks a route for and is answered with:
  the data behind the page rather than the document."
  "application/transit+json")

(def result-views
  "The views a search result can be shown in, in display order: the
  keyword naming each and its `view` param value."
  [[:kwic "kwic"]
   [:frequencies "frequencies"]])

(def export-formats
  "The formats a view of a result is exported in, in display order, each
  the extension of its `export` path."
  ["tsv" "csv"])

(def default-distance
  "How many words away a nearby word may stand when no URL says: the
  manual's own example (section 3.7) and the window a collocation is
  usually counted in."
  5)

(def defaults
  "What each param means when a URL leaves it out, as the string a URL
  would carry: the default its reader applies, which `defaults-test`
  holds it to."
  (merge mode/defaults
         {:sort        "corpus"
          :context     "5"
          :distance    (str default-distance)
          :subset-at   "match"
          :subset-attr "word"
          :view        "kwic"
          :attr        "word"
          :at          "match"
          :page        "1"}))

(def filter-prefixes
  "The query param prefixes of the metadata filter, each followed by the
  attribute name, as in `f.text_year`: a chosen value, one param per
  value; a pattern the values must match; and the bounds of a range of
  them."
  {:value   "f."
   :pattern "fp."
   :from    "ff."
   :to      "ft."})

(defn whole-range
  "The `bounds` ([from to]) of a metadata range as the two whole numbers
  they name, lower first; nil when they name anything else, which is a
  range no search can be made of."
  [bounds]
  (let [[a b] (map #(some-> % not-empty parse-long) bounds)]
    (when (and a b (<= a b)) [a b])))

(defn metadata-key?
  "True when param key `k` names part of the metadata filter: one of the
  `filter-prefixes` followed by an attribute name."
  [k]
  (boolean (when k
             (let [s (name k)]
               (some #(and (str/starts-with? s %) (not= s %))
                     (vals filter-prefixes))))))

(def param-order
  "Every param a search URL may carry, in the order it carries them; a
  param not named here is dropped from every URL the app builds.
  `::mode/tokens` stands for the fields of an extended search's tokens
  and `::filter` for the metadata filter's params."
  [:q ::mode/tokens :within :in :ci :match
   :corpus :scope ::filter
   :near :distance :subset :subset-at :subset-attr :sample
   :view :sort :context :attr :at :by :docs
   :page :expand])

(defn rank
  "Where param key `k` sorts in a query string: its place in
  `param-order`, then its name; -1 first for a key the order lacks,
  which `known?` refuses."
  [k]
  (let [k* (cond
             (metadata-key? k)     ::filter
             (tokens/token-key? k) ::mode/tokens
             :else                 k)]
    [(.indexOf param-order k*) (name k)]))

(defn known?
  "True when param key `k` is one the app reads, and so one a URL it
  builds should carry."
  [k]
  (and k (not (neg? (first (rank k))))))

(defn corpora-param
  "The corpus names the `corpus` query param value `v` selects: a string
  of one name or several joined by commas, or a vector of such strings
  when the param repeats; uppercased and deduplicated, nothing validated,
  since the search reports an unknown name as that corpus's error."
  [v]
  (->> (if (vector? v) v [v])
       (mapcat #(str/split (str %) #","))
       (remove str/blank?)
       (map str/upper-case)
       (distinct)
       (vec)))

(defn expand-param
  "The hits the `expand` query param value `v` names, as a set of
  [corpus cpos] keys: `CORPUS:cpos` items, comma-joined or repeated,
  an item naming no position ignored. nil for none."
  [v]
  (when-let [items (tokens/present v)]
    (into #{}
          (comp (mapcat #(str/split % #","))
                (keep (fn [item]
                        (let [[corpus cpos] (str/split item #":" 2)]
                          (when-let [n (some-> cpos parse-long)]
                            [corpus n])))))
          (if (vector? items) items [items]))))

(defn with-expanded
  "The search `params` with the `expand` param naming the `hits` shown
  expanded ([corpus cpos] keys) as `CORPUS:cpos` items in order, the
  inverse of `expand-param`; without it when there are none."
  [params hits]
  (if (seq hits)
    (assoc params :expand (str/join "," (map (fn [[corpus cpos]]
                                               (str corpus ":" cpos))
                                             (sort hits))))
    (dissoc params :expand)))

(def chosen-scope
  "The `scope` value marking a selection the reader made, so that having
  emptied it is not read as having named no corpus."
  "chosen")

(def all-scope
  "The `scope` value naming every corpus the reader may choose. No URL
  carries it: a URL says the same by naming no corpus. But a form is not
  a search, since a chooser with every box ticked and one with none are
  different and only one of them can be searched, so the stored settings
  say it (see dk.cst.corpus-probe.settings/string)."
  "all")

(defn with-corpora
  "The search `params` with their corpus selection as one param: the
  `corpora-param` names comma-joined, or none when they are every corpus
  of the set `all`, since naming none searches them all."
  [params all]
  (let [corpora (corpora-param (:corpus params))
        all?    (and (seq corpora) (= (set corpora) (set all)))]
    (cond-> (dissoc params :corpus :scope)
      (and (seq corpora) (not all?))
      (assoc :corpus (str/join "," corpora))

      ;; a selection the reader emptied: the one case where naming no
      ;; corpus does not mean every corpus
      (and (empty? corpora) (contains? params :scope))
      (assoc :scope chosen-scope))))

(defn with-every-corpus
  "The search `params` with a selection of `all-scope` written out as the
  `all` corpora it stands for, the inverse of what `with-corpora` folds
  away, so that nothing downstream reads a scope no form submits."
  [params all]
  (cond-> params
    (= all-scope (:scope params))
    (assoc :corpus (vec all) :scope chosen-scope)))

(defn without-orphans
  "Canonical `params` less a param that only qualifies one that is not
  there: a distance without a word to be near, the anchor and attribute
  of a subset without a value, and the fields of a token, or of a
  condition, that asks for nothing."
  [params]
  (let [rows (tokens/token-rows params)
        idle (into #{} (comp (remove tokens/asks?) (map :n)) rows)
        ;; a condition asking nothing, of a token that asks
        blank (into #{} (for [{:keys [n conditions]} rows
                              {:keys [c] :as condition} conditions
                              :when (not (tokens/condition-asks? condition))]
                          [n c]))
        orphan? (fn [[k _]]
                  (when-let [[n c] (tokens/token-field k)]
                    (or (idle n) (blank [n c]))))]
    (cond-> (into {} (remove orphan?) params)
      (nil? (:near params))   (dissoc :distance)
      (nil? (:subset params)) (dissoc :subset-at :subset-attr))))

(defn default
  "The value param key `k` has when a URL leaves it out, from `defaults`
  or a token field's own; nil for a key that has none."
  [k]
  (if-let [[_ _ field] (tokens/token-field k)]
    (get tokens/token-defaults field)
    (get defaults k)))

(defn canonical
  "The search `params` (as a request or a form carries them) as the URL
  cites them against the set `all` of every searchable corpus: nothing
  nil, blank, default, orphaned, unread by the mode or unknown, the
  corpora as one param and a text area's line breaks as one character.

  Idempotent, so a link can be built from canonical params and
  canonicalised again."
  ([params]
   (canonical params nil))
  ([params all]
   ;; by the mode the radio says, which no URL carries: what is kept once
   ;; the radio is gone reads as the same mode, so a second pass agrees
   (let [m (mode/mode params)]
     (-> (into {}
               (keep (fn [[k v]]
                       (let [v (tokens/present v)]
                         (when (and (known? k) v (not= v (default k)))
                           [k (cond-> v
                                (= :q k) (str/replace "\r\n" "\n"))]))))
               (with-corpora params all))
         (without-orphans)
         (mode/without-unread m)))))

(defn pairs
  "Canonical `params` as [name value] string pairs in `param-order`, a
  vector value one pair per element."
  [params]
  (for [[k v] (sort-by (comp rank key) params)
        v     (if (vector? v) v [v])]
    [(name k) (str v)]))

(defn form-encode
  "The [name value] `pairs` as a query string, encoded as a browser
  encodes a GET submit, so a URL the app builds and one the browser
  built from the same form are the same."
  [pairs]
  #?(:clj  (str/join "&" (map (fn [[k v]]
                                (str k "=" (URLEncoder/encode
                                            ^String v "UTF-8")))
                              pairs))
     :cljs (.toString (js/URLSearchParams. (clj->js pairs)))))

(defn form-decode
  "The params of query string `s`, keyed as `form-encode` names them; a
  key that repeats keeps its last value, since nothing this reads back
  is written more than once."
  [s]
  (let [decode (fn [x]
                 ;; a form encodes a space as a plus, which neither
                 ;; percent-decoder undoes on its own
                 (let [x (str/replace (str x) "+" " ")]
                   #?(:clj  (URLDecoder/decode x "UTF-8")
                      :cljs (js/decodeURIComponent x))))]
    (into {}
          (comp (remove str/blank?)
                (map (fn [item]
                       (let [[k v] (str/split item #"=" 2)]
                         [(keyword (decode k)) (decode v)]))))
          (str/split (str s) #"&"))))

(defn query-string
  "The query string of search `params`, canonicalised: the `pairs`
  `form-encode`d."
  [params]
  (-> (form-encode (pairs (canonical params)))
      ;; RFC 3986 allows both in a query, and they separate the corpora
      ;; and the expanded hits a reader should be able to read in the bar
      (str/replace "%2C" ",")
      (str/replace "%3A" ":")))

(defn search-href
  "The URL of the search page for `params`: the page itself when they
  say nothing."
  [params]
  (let [qs (query-string params)]
    (cond-> search (seq qs) (str "?" qs))))

(defn results-href
  "The `search-href` of `params` ending in the `results-fragment`: the
  URL of a result, which a link should land on."
  [params]
  (str (search-href params) results-fragment))

(defn export-href
  "The URL of the `view` of the search `params` describe as a file in
  `format` (see `export`)."
  [view format params]
  (let [qs (query-string params)]
    (cond-> (export view format) (seq qs) (str "?" qs))))

(defn search-params
  "The `params` that identify a search, for linking the views of the same
  hits: its corpora, its query as any mode reads it, the metadata filter,
  the narrowings of its hits and the sample of them. Which hits there are
  is part of the search; the order they are read in, and the reader's
  language, are not."
  [params]
  ;; the sample too, though a frequency table draws none: returning to
  ;; the concordance returns to the sample it was left in
  (into (select-keys params [:corpus :subset :subset-at :subset-attr
                             :near :distance :sample])
        (filter (comp (some-fn mode/query-key? metadata-key?) key))
        params))

(defn page-href
  "The URL of page `page` of the search `params` cite, counted from
  nought here and from one in the URL; without `expand`, which names
  positions on the current page and does not carry to another's hits."
  [params page]
  (results-href (assoc (dissoc params :expand) :page (inc page))))

(defn page-count
  "The number of pages a `result` of `size` hits spans."
  [{:keys [size page-size]}]
  (max 1 (long (Math/ceil (/ size (double page-size))))))

(defn page-hrefs
  "The links from page `page` of the concordance `result` of the search
  `params` cite to the pages before and after it, as `:prev-href` and
  `:next-href`, nil where there is none."
  [params page result]
  {:prev-href (when (pos? page) (page-href params (dec page)))
   ;; a result still being counted has no last page yet, but a next page
   ;; the hits counted so far reach is there whatever the rest hold
   :next-href (when (and result (< (inc page) (page-count result)))
                (page-href params (inc page)))})

(defn export-hrefs
  "The URLs of the exports of `view` (`:kwic` or `:frequencies`) of the
  search described by `params` in each of the `formats` (see
  `export-formats`), by format keyword."
  [view formats params]
  (into {} (for [format formats]
             [(keyword format) (export-href view format params)])))

(defn subset-href
  "The URL of the concordance of the search described by `params` kept to
  the hits whose token at `anchor` has `value` as its `attr`: what one
  row of the frequency table grouped by `attr` at `anchor` counted."
  [params attr anchor value]
  (results-href (assoc (search-params params)
                       :view        "kwic"
                       :subset      value
                       :subset-at   anchor
                       :subset-attr (name attr))))

(defn view-hrefs
  "Each result view (see `result-views`) of the search described by
  `params`, for the switch at the top of the results region: [view
  keyword url], in display order."
  [params]
  (for [[k value] result-views]
    [k (results-href (assoc (search-params params)
                            :view    value
                            :attr    (:attr params)
                            :at      (:at params)
                            :by      (:by params)
                            :docs    (:docs params)
                            :sort    (:sort params)
                            :context (:context params)))]))

(defn nav-hrefs
  "The URL of each top-level page for `params`: the search keeping the
  current query, so that returning to it from the corpus index does not
  lose it."
  [params]
  (let [asked (search-params params)]
    {:search          (if (seq asked)
                        (results-href asked)
                        search)
     :corpora-heading corpora
     :glossary        glossary}))

(comment
  (canonical {:q "hund" :mode "simple" :in "word" :corpus ["PROBE" "VISER"]
              :scope "chosen" :sort "corpus" :sample "" :distance "5"}
             #{"PROBE" "VISER" "TALER"})
  ;; => {:q "hund", :corpus "PROBE,VISER"}

  (results-href {:q "[lemma = \"hund\"]" :corpus ["PROBE" "VISER"]})
  ;; => "/search?q=%5Blemma+%3D+%22hund%22%5D&corpus=PROBE,VISER#results"

  (expand-param "PROBE:9,PROBE:12")
  ;; => #{["PROBE" 9] ["PROBE" 12]}
  #_.)
