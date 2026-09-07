(ns dk.cst.corpus-probe.server.request
  "What a request asks: its query params read into typed values, one
  reader per param with a default for anything else; the language a
  reader is served in, negotiated from the preference they stored and
  what they accept; the cookies that store a preference; and whether the
  client asked for the data behind a route rather than its document."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.cwb.command :as command]
            [dk.cst.corpus-probe.cwb.corpus :as corpus]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.search.batch :as batch]
            [dk.cst.corpus-probe.url :as url]))

(defn multi-param?
  "True when query param key `k` may repeat: the corpus selection and the
  chosen values of the metadata filter, one param per value under the
  value prefix, as in `f.text_year` (see
  dk.cst.corpus-probe.url/filter-prefixes).

  A query param written without a `=` arrives under a nil key, which
  names no attribute."
  [k]
  (boolean (and k (or (= k :corpus)
                      (str/starts-with? (name k)
                                        (:value url/filter-prefixes))))))

(defn scalar-params
  "The query `params` with every value but the `multi-param?` ones reduced
  to one string: a repeated scalar param arrives as a vector, of which the
  first value counts, so a stray duplicate cannot fail the request."
  [params]
  (into {} (map (fn [[k v]]
                  [k (if (or (multi-param? k) (not (vector? v))) v (first v))]))
        params))

(defn prefixed-params
  "The params of `params` whose key opens with `prefix`, as a map of the
  attribute the rest of the key names to the value; a key that is the
  prefix alone names no attribute and is dropped."
  [params prefix]
  (into {} (for [[k v] params
                 :when (and k (str/starts-with? (name k) prefix)
                            (< (count prefix) (count (name k))))]
             [(keyword (subs (name k) (count prefix))) v])))

(defn filter-params
  "The metadata filter selected by `params`: a map of attribute name to
  the set of non-blank values of its `f.<attribute>` params (the value
  prefix of dk.cst.corpus-probe.url/filter-prefixes), as
  dk.cst.corpus-probe.search/concordance! takes it; empty when nothing
  is selected."
  [params]
  (into {} (for [[attr v] (prefixed-params params (:value url/filter-prefixes))
                 :let  [values (set (remove str/blank? (if (vector? v) v [v])))]
                 :when (seq values)]
             [attr values])))

(def range-limit
  "The most values a range of integers is spelt out as: enough for the
  years any corpus spans, and a bound on the query it becomes."
  1000)

(defn range-pattern
  "The pattern matching every integer from `from` to `to` inclusive (both
  query param values): an alternation of them, the first `range-limit`
  of them at most; nil unless both are integers in order."
  [from to]
  (let [a (some-> from parse-long)
        b (some-> to parse-long)]
    (when (and a b (<= a b))
      (str/join "|" (take range-limit (range a (inc b)))))))

(defn pattern-params
  "The patterns `params` ask each metadata attribute's values to match
  instead of, or beside, the values chosen: the regex of its
  `fp.<attribute>` param as a reader wrote it, and the integers from its
  `ff.<attribute>` to its `ft.<attribute>` param (see `range-pattern`).
  A map of attribute to its patterns; empty when there are none."
  [params]
  (let [to (prefixed-params params (:to url/filter-prefixes))]
    (reduce (fn [m [attr pattern]] (update m attr (fnil conj []) pattern))
            {}
            (concat (remove (comp str/blank? val)
                            (prefixed-params params
                                             (:pattern url/filter-prefixes)))
                    (keep (fn [[attr from]]
                            (some->> (range-pattern from (get to attr))
                                     (vector attr)))
                          (prefixed-params params
                                           (:from url/filter-prefixes)))))))

(defn pattern-fields
  "What the pattern and range fields of the metadata filter hold, from
  `params`: the `:patterns`, attribute to its `fp.` param, and the
  `:ranges`, attribute to its [`ff.` `ft.`] params, as the form shows
  them back (see dk.cst.corpus-probe.views.search.filter/pattern-row)."
  [params]
  (let [from (prefixed-params params (:from url/filter-prefixes))
        to   (prefixed-params params (:to url/filter-prefixes))]
    {:patterns (prefixed-params params (:pattern url/filter-prefixes))
     :ranges   (into {} (for [attr (distinct (concat (keys from) (keys to)))]
                          [attr [(get from attr) (get to attr)]]))}))

(defn page-param
  "The page the `page` query param value `v` names, counted from nought
  as the result does; the URL counts from one, and anything else names
  the first page."
  [v]
  (max 0 (dec (or (some-> v parse-long) 1))))

(defn sample-param
  "How many hits the `sample` query param value `v` asks to be shown at
  random: a positive integer, or nil for the whole result.

  A sample of none of the hits is no sample rather than an empty result,
  so zero and anything that is not a number name none."
  [v]
  (when-let [n (some-> v parse-long)]
    (when (pos? n) n)))

(defn attr-param
  "The attribute the query param value `v` names, word when it names
  none: the one attribute every corpus has."
  [v]
  (if (str/blank? v) "word" v))

(defn position-param
  "The position of the match the `at` query param value `v` names, among
  dk.cst.corpus-probe.cwb.command/positions; the start of the match for
  anything else."
  [v]
  (if (some #{v} command/positions) v "match"))

(defn by-param
  "The attribute the `by` query param value `v` asks a frequency table to
  count its values against (see
  dk.cst.corpus-probe.search.frequency/frequency-table!), as a keyword;
  nil when it names none, the corpora being the columns then. The
  attribute is checked against each corpus by the breakdown, as every
  attribute is."
  [v]
  (when-not (str/blank? v) (keyword v)))

(defn subset-param
  "The narrowing the `subset`, `subset-at` and `subset-attr` query params
  of `params` ask for: {:anchor ... :attr ... :value ...} as
  dk.cst.corpus-probe.cwb.command/subset-command takes it, or nil without
  a value. The attribute is checked against each corpus by the search,
  as every attribute is."
  [{:keys [subset subset-at subset-attr]}]
  (when-not (str/blank? subset)
    {:anchor (position-param subset-at)
     :attr   (keyword (attr-param subset-attr))
     :value  subset}))

(defn context-param
  "The width of context the `context` query param value `v` asks for: a
  positive number of words, or the unit of text it names (a key of
  dk.cst.corpus-probe.cwb.corpus/units); the default width for anything
  else."
  [v]
  (let [n (some-> v parse-long)]
    (cond
      (and n (pos? n))                             n
      (and v (contains? corpus/units (keyword v))) (keyword v)
      :else                                        (:context
                                                    batch/kwic-defaults))))

(defn near-param
  "The word the `near` query param value `word` asks every hit to have
  nearby, at most `distance` (the query param value) words away: {:word
  ... :distance ...} as dk.cst.corpus-probe.cwb.command/near-command
  takes it, or nil for a blank word. A distance that is not a positive
  integer is the default (see dk.cst.corpus-probe.url/defaults)."
  [word distance]
  (when-not (str/blank? word)
    {:word     (str/trim word)
     :distance (let [n (some-> distance parse-long)]
                 (if (and n (pos? n))
                   n
                   (parse-long (:distance url/defaults))))}))

(defn view-param
  "The result view named by the `view` query param value `v` (see
  dk.cst.corpus-probe.url/result-views): the concordance for anything
  that does not name another view."
  [v]
  (or (some (fn [[k value]] (when (= v value) k)) url/result-views) :kwic))

(defn accepted-languages
  "The languages the `Accept-Language` header value `s` offers, most
  preferred first: each once, by descending quality.

  Only the primary subtag counts, so `da-DK` counts as Danish. A tag
  offered at quality 0 is refused rather than preferred (RFC 9110), so
  it is left out."
  [s]
  (->> (str/split (str s) #",")
       (keep (fn [item]
               (let [[tag q] (str/split item #"(?i)\s*;\s*q\s*=")
                     lang    (first (str/split (str/trim tag) #"-"))]
                 (when-not (str/blank? lang)
                   [(str/lower-case lang)
                    (or (some-> q str/trim parse-double) 1.0)]))))
       (filter (comp pos? second))
       (sort-by (comp - second))
       (map first)
       (distinct)))

(def preference-keys
  "The settings a reader may store, by the name each is stored under, with
  the predicate saying which values that setting accepts.

  An allowlist rather than a free cookie jar: the endpoint behind this
  writes cookies, and a caller who chooses both the name and the value of
  a cookie can fill a reader's jar until their requests no longer fit in a
  header, or shadow a cookie this app comes to rely on. A setting not
  named here cannot be stored, and a value the predicate refuses is not
  stored either, so whatever comes back out is a value the app has already
  agreed to."
  {:lang i18n/supported?})

(defn cookie-value
  "The value stored under `k` in the `Cookie` header value `s`, when it is
  one that `preference-keys` accepts; nil otherwise."
  [s k]
  (->> (str/split (str s) #";")
       (keep (fn [item]
               (let [[cookie v] (str/split (str/trim item) #"=" 2)]
                 (when (= (name k) cookie) v))))
       (some (fn [v] (when ((preference-keys k) v) v)))))

(defn request-languages
  "The languages `request` reads, most preferred first: the one it
  stored, then its `Accept-Language` by quality (see
  `accepted-languages`), then Danish and English. The one negotiation:
  the interface takes the first it has a translation for (see
  `request-language`), a document the first it has a file in (see
  dk.cst.corpus-probe.docs/document). Not the URL: a reader's language is
  their preference, so a shared link does not impose the sharer's."
  [request]
  (distinct (concat (some-> (cookie-value (get-in request [:headers "cookie"])
                                          :lang)
                            vector)
                    (accepted-languages
                     (get-in request [:headers "accept-language"]))
                    [i18n/default-language i18n/source-language])))

(defn request-language
  "The UI language `request` is served in: the first of its
  `request-languages` the interface has (see
  dk.cst.corpus-probe.i18n/languages), and Danish is among them, so
  there always is one."
  [request]
  (some #(when (i18n/supported? %) %) (request-languages request)))

(def cookie-max-age
  "How long a stored preference outlives the visit that set it: a year, so
  a reader states it once."
  31536000)

(defn preference-cookies
  "The Set-Cookie headers storing every `preference-keys` setting that
  `params` names with a value that setting accepts.

  A value the setting refuses stores nothing rather than storing a
  fallback: a reader who never asked for Danish should not be given it
  because something mangled their request."
  [params]
  (into []
        (keep (fn [[k valid?]]
                (let [v (get params k)]
                  (when (and (some? v) (valid? v))
                    (str (name k) "=" v ";Path=/;Max-Age=" cookie-max-age
                         ";SameSite=Lax")))))
        preference-keys))

(defn safe-return
  "The path `s` to send a reader back to after a preference change, or the
  search page when it names anywhere but this app.

  A redirect target that arrives in a form field is an open redirect
  unless it is checked: only a path of our own is followed, never an
  absolute URL and never a protocol-relative one."
  [s]
  (let [s (str s)]
    (if (and (str/starts-with? s "/") (not (str/starts-with? s "//")))
      s
      "/")))

(defn wants-transit?
  "True when `request` asks for the data behind a route rather than a
  document: the client router fetching a page it will render itself."
  [request]
  (boolean (some-> (get-in request [:headers "accept"])
                   (str/includes? url/transit-type))))
