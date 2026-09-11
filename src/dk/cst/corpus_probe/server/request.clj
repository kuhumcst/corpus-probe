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
            [dk.cst.corpus-probe.storage :as storage]
            [dk.cst.corpus-probe.storage.settings :as settings]
            [dk.cst.corpus-probe.url :as url]))

(defn multi-param?
  "True when query param key `k` may repeat: the corpus selection and the
  chosen values of the metadata filter, one param per value under the
  value prefix, as in `f.text_year`.

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
  the set of non-blank values of its `f.<attribute>` params, as
  dk.cst.corpus-probe.search/concordance! takes it; empty when nothing is
  selected."
  [params]
  (into {} (for [[attr v] (prefixed-params params (:value url/filter-prefixes))
                 :let  [values (set (remove str/blank? (if (vector? v) v [v])))]
                 :when (seq values)]
             [attr values])))

(defn pattern-params
  "The patterns `params` ask each metadata attribute's values to match
  instead of, or beside, the values chosen: the regex of its
  `fp.<attribute>` param as a reader wrote it. A map of attribute to its
  patterns; empty when there are none."
  [params]
  (into {} (for [[attr pattern] (prefixed-params params
                                                 (:pattern url/filter-prefixes))
                 :when (not (str/blank? pattern))]
             [attr [pattern]])))

(defn range-params
  "The ranges `params` ask each metadata attribute's values to lie in:
  the whole numbers its `ff.<attribute>` and `ft.<attribute>` params
  name (see dk.cst.corpus-probe.url/whole-range), which the corpus
  resolves to the values it has in them (see
  dk.cst.corpus-probe.search.opts/corpus-filter!). A map of attribute to
  its [from to]; an attribute whose bounds are no whole range has none,
  since no search can be made of it."
  [params]
  (let [to (prefixed-params params (:to url/filter-prefixes))]
    (into {} (for [[attr from] (prefixed-params params
                                                (:from url/filter-prefixes))
                   :let  [bounds (url/whole-range [from (get to attr)])]
                   :when bounds]
               [attr bounds]))))

(defn pattern-fields
  "What the pattern and range fields of the metadata filter hold, from
  `params`: the `:patterns`, attribute to its `fp.` param, and the
  `:ranges`, attribute to its [`ff.` `ft.`] params, as the form shows
  them back."
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
  count its values against, as a keyword; nil when it names none, the
  corpora being the columns then. The attribute is checked against each
  corpus by the breakdown, as every attribute is."
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
  dk.cst.corpus-probe.cwb.corpus/unit-attrs); the default width for
  anything else."
  [v]
  (let [n (some-> v parse-long)]
    (cond
      (and n (pos? n))                                  n
      (and v (contains? corpus/unit-attrs (keyword v))) (keyword v)
      :else                                             (:context
                                                         batch/kwic-defaults))))

(def reach-limit
  "The furthest a page may be asked to reach either side of a match, in
  words. Past this the whole text is the bound and the reading page is
  the way on."
  ;; two reasons for a cap, and this number answers the second: a reader
  ;; travelling on must not be able to ask for a page of any size, and
  ;; the concordance's context columns are given a width that holds this
  ;; many words (style.css, `.kwic-left`), since a column that grew with
  ;; the page would shift the table sideways each time one arrived. The
  ;; two must move together.
  ;; TODO: it is also the furthest a reader can travel, which is a
  ;; question about reading rather than about layout. If travelling to
  ;; the end of a long text turns out to be wanted, the answer is not a
  ;; bigger number here but handing over to the reading page at the edge
  120)

(defn reach-param
  "How far either side of a match the `reach` query param value `v` asks
  the page to hold: a positive number of words, up to `reach-limit`; nil
  for anything else, leaving the width to the context asked for. The
  client asks for this as it travels; no URL the app builds carries it."
  [v]
  (when-let [n (some-> v parse-long)]
    (when (pos? n)
      (min n reach-limit))))

(defn near-param
  "The word the `near` query param value `word` asks every hit to have
  nearby, at most `distance` (the query param value) words away: {:word
  ... :distance ...} as dk.cst.corpus-probe.cwb.command/near-command
  takes it, or nil for a blank word. A distance that is not a positive
  integer is the default."
  [word distance]
  (when-not (str/blank? word)
    {:word     (str/trim word)
     :distance (let [n (some-> distance parse-long)]
                 (if (and n (pos? n)) n url/default-distance))}))

(defn view-param
  "The result view named by the `view` query param value `v`: the
  concordance for anything that does not name another view."
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

  An allowlist rather than a free cookie jar: a caller who chooses both
  the name and the value of a cookie can fill a reader's jar until their
  requests no longer fit in a header, or shadow a cookie this app relies
  on. Whatever comes back out is a value the app has already agreed to."
  ;; the search settings are one setting holding many, so forgetting them
  ;; clears one cookie and leaves the reader's language alone
  {:lang               i18n/supported?
   settings/cookie-key settings/storable?})

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
  stored, then its `Accept-Language` by quality, then Danish and English.
  The interface takes the first it has a translation for, a document the
  first it has a file in. Not the URL: a reader's language is their
  preference, so a shared link does not impose the sharer's."
  [request]
  (distinct (concat (some-> (cookie-value (get-in request [:headers "cookie"])
                                          :lang)
                            vector)
                    (accepted-languages
                     (get-in request [:headers "accept-language"]))
                    [i18n/default-language i18n/source-language])))

(defn request-language
  "The UI language `request` is served in: the first of its
  `request-languages` the interface has, and Danish is among them, so
  there always is one."
  [request]
  (some #(when (i18n/supported? %) %) (request-languages request)))

(defn stored-settings
  "The settings `request` stored, as written; nil for a reader who has
  stored none."
  [request]
  (cookie-value (get-in request [:headers "cookie"]) settings/cookie-key))

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
                    (storage/cookie k v)))))
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
