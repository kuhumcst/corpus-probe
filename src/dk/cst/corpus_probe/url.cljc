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
   :sort        "corpus"
   :context     "5"
   :distance    "5"
   :subset-at   "match"
   :subset-attr "word"
   :view        "kwic"
   :attr        "word"
   :at          "match"
   :page        "1"})

(def param-order
  "Every param a search URL may carry, in the order it carries them: what
  was asked, where, which hits were kept, how they are shown, the page.
  A param not named here is dropped from every URL the app builds.
  `::filter` stands for the metadata filter's params (see
  `metadata-key?`)."
  [:q :mode :in :ci :prefix :suffix
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
  `param-order`, then its name, so the metadata filter's params keep one
  order among themselves; -1 for a key the order lacks, which `known?`
  refuses."
  [k]
  (let [k* (if (metadata-key? k) ::filter k)]
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
  of a subset without a value."
  [params]
  (cond-> params
    (nil? (:near params))   (dissoc :distance)
    (nil? (:subset params)) (dissoc :subset-at :subset-attr)))

(defn canonical
  "The search `params` (param keys to their string or vector values, as a
  request or a form carries them) as the URL cites them, against the set
  `all` of every corpus that can be searched: nothing nil, blank or
  default (see `defaults`), nothing that qualifies an absent param (see
  `without-orphans`), nothing the app does not read (see `known?`), and
  the corpora as one param (see `with-corpora`).

  Applying it to its own result changes nothing, so a link can be built
  from canonical params and canonicalised again."
  ([params]
   (canonical params nil))
  ([params all]
   (->> (with-corpora params all)
        (keep (fn [[k v]]
                (let [v (present v)]
                  (when (and (known? k) v (not= v (get defaults k)))
                    [k v]))))
        (into {})
        (without-orphans))))

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
