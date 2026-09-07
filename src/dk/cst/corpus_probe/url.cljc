(ns dk.cst.corpus-probe.url
  "The app's URLs: the path of each page, the ids a page lands on and its
  form carries, and the one query string a search has.

  A result URL is a citation, so the server and the client build it by
  the same rule, `canonical` then `query-string`: the params in a fixed
  order, every default left out, the corpora as one comma-joined param,
  and no corpus named when every readable one is chosen. Every link of a
  result (see `page-hrefs`, `view-hrefs`, `export-hrefs`, `subset-href`
  and `nav-hrefs`) is built from its params by that rule."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.query.mode :as mode]
            [dk.cst.corpus-probe.query.tokens :as tokens])
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
  (str (name k) "=" v ";Path=/;Max-Age=" cookie-max-age ";SameSite=Lax"))

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
  the top of the form that asked for it. Named once, so the URLs and the
  region they name cannot drift apart."
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
  keyword naming each and its `view` param value. What each is called is
  the interface's business (see
  dk.cst.corpus-probe.views.result/view-label).

  A frequency table is not another page, it is the same search counted
  rather than listed, so it is a view of the result rather than a place of
  its own."
  [[:kwic "kwic"]
   [:frequencies "frequencies"]])

(def export-formats
  "The formats a view of a result is exported in, in display order, each
  as the extension of its `export` path (see
  dk.cst.corpus-probe.search.export/formats, which renders each)."
  ["tsv" "csv"])

(def default-distance
  "How many words away a nearby word may stand when no URL says: the
  manual's own example (section 3.7) and the window a collocation is
  usually counted in."
  5)

(def defaults
  "What each param means when a URL leaves it out, so the value no URL
  carries. Each is the default its reader in
  dk.cst.corpus-probe.server.request, .cwb.command or .query.params
  applies, restated as the string a URL would carry, but for the query
  keys' (see dk.cst.corpus-probe.query.mode/defaults) and the distance
  (see `default-distance`); `defaults-test` holds each reader to its
  default."
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

(defn metadata-key?
  "True when param key `k` names part of the metadata filter: one of the
  `filter-prefixes` followed by an attribute name."
  [k]
  (boolean (when k
             (let [s (name k)]
               (some #(and (str/starts-with? s %) (not= s %))
                     (vals filter-prefixes))))))

(def param-order
  "Every param a search URL may carry, in the order it carries them: what
  was asked, where, which hits were kept, how they are shown, the page.
  A param not named here is dropped from every URL the app builds.
  `::mode/tokens` stands for the fields of an extended search's tokens
  (see dk.cst.corpus-probe.query.tokens/token-key?) and `::filter` for
  the metadata filter's params (see `metadata-key?`)."
  [:q ::mode/tokens :within :in :ci :match
   :corpus :scope ::filter
   :near :distance :subset :subset-at :subset-attr :sample
   :view :sort :context :attr :at :by :docs
   :page :expand])

(defn rank
  "Where param key `k` sorts in a query string: its place in
  `param-order`, then its name, so the metadata filter's params and the
  tokens' fields each keep one order among themselves; -1 for a key the
  order lacks, which `known?` refuses."
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
  token that asks for nothing (see dk.cst.corpus-probe.query.tokens/asks?)
  or of a condition that does not (see
  dk.cst.corpus-probe.query.tokens/condition-asks?)."
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
  "The value param key `k` has when a URL leaves it out (see `defaults`,
  and dk.cst.corpus-probe.query.tokens/token-defaults for the field of a
  token); nil for a key that has none."
  [k]
  (if-let [[_ _ field] (tokens/token-field k)]
    (get tokens/token-defaults field)
    (get defaults k)))

(defn canonical
  "The search `params` (param keys to their string or vector values, as a
  request or a form carries them) as the URL cites them, against the set
  `all` of every corpus that can be searched: nothing nil, blank or
  default (see `defaults`), nothing that qualifies an absent param (see
  `without-orphans`), nothing the mode does not read (see
  dk.cst.corpus-probe.query.mode/without-unread), nothing the app does
  not read (see `known?`), the corpora as one param (see `with-corpora`)
  and the field's line breaks as one character each, where a text area
  submits two.

  Applying it to its own result changes nothing, so a link can be built
  from canonical params and canonicalised again."
  ([params]
   (canonical params nil))
  ([params all]
   ;; by the mode the params are read in, which a submitted form's radio
   ;; and the text's shape say and no URL carries: what the trimmed
   ;; params say once the radio is gone is what was kept, so a second
   ;; pass reads the same
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
  "The `params` that identify a search: its corpora, its query as any
  mode reads it (see dk.cst.corpus-probe.query.mode/query-key?; what the
  mode does not read, `canonical` drops from a link), the metadata
  filter, the narrowings of its hits and the sample of them, for linking
  the views of the same hits. The interface language is not among them:
  it is the reader's preference, not part of the search.

  The narrowings and the sample are here and the sort is not, because
  which hits there are is part of the search while the order they are
  read in is not. The frequency view draws no sample, but carries the
  param so that returning to the concordance returns to the sample it
  was left in."
  [params]
  (into (select-keys params [:corpus :subset :subset-at :subset-attr
                             :near :distance :sample])
        (filter (comp (some-fn mode/query-key? metadata-key?) key))
        params))

(defn page-href
  "The URL of page `page` of the search `params` cite, counted from
  nought here and from one in the URL.

  Ends in the results fragment, so a page turn lands on the hits rather
  than at the top of the query form. Drops `expand`, which names corpus
  positions on the current page and does not carry to another page's
  hits."
  [params page]
  (results-href (assoc (dissoc params :expand) :page (inc page))))

(defn page-count
  "The number of pages a `result` of `size` hits spans."
  [{:keys [size page-size]}]
  (max 1 (long (Math/ceil (/ size (double page-size))))))

(defn page-hrefs
  "The links from page `page` of the concordance `result` of the search
  `params` cite to the pages before and after it, as `:prev-href` and
  `:next-href`, nil where there is none.

  A result still being counted (see
  dk.cst.corpus-probe.search/concordance!) has no last page yet, but the
  hits counted so far may already reach past this page, and then the
  next one is there whatever the rest turn out to hold."
  [params page result]
  {:prev-href (when (pos? page) (page-href params (dec page)))
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
  keyword url], in display order.

  Every view of one search shares its URL but for the `view` param, so
  moving between them keeps the query, the corpora and the filter by
  construction rather than by carrying them across."
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
  "The URL of each top-level page for `params`.

  No URL names a language: which language a reader reads in is their own
  preference, so none of these carries one. The search keeps the current
  query, so returning to it from the corpus index does not lose it. The
  frequency table is not here: it is a view of a search result, reached by
  the switch at the top of the results region (see `view-hrefs`)."
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
