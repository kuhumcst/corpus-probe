(ns dk.cst.corpus-probe.api
  "HTTP routes and handlers: the frontpage, the server-rendered search
  page (a concordance or a frequency table over the selected corpora),
  its TSV/CSV exports, the corpus index and info pages, the bootstrap
  payload the client takes over from, and the compiled client assets.
  Where each of these is, and how a search is spelt as a URL, is
  dk.cst.corpus-probe.url's.

  Responses are rendered from the shared .cljc views with Replicant's string
  renderer, so the client renders identical markup. On the search page the
  same view data is embedded as transit for the client; the corpus pages
  are read-only and ship no payload or script. Hostile corpus content
  survives the round trip because each channel is protected:
  `correct-quote-escaping` fixes the SSR body, transit-JSON escapes true
  control bytes (a carriage return) in the payload, and `script-safe`
  escapes `<` (which transit passes through verbatim) so a token containing
  `</script>` cannot break out."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cognitect.transit :as transit]
            [dk.cst.corpus-probe.commands :as commands]
            [dk.cst.corpus-probe.corpus :as corpus]
            [dk.cst.corpus-probe.docs :as docs]
            [dk.cst.corpus-probe.export :as export]
            [dk.cst.corpus-probe.frequency :as frequency]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.search :as search]
            [dk.cst.corpus-probe.tools :as tools]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.corpus :as corpus-views]
            [dk.cst.corpus-probe.views.frequencies :as freq-views]
            [dk.cst.corpus-probe.views.layout :as layout]
            [dk.cst.corpus-probe.views.page :as page]
            [dk.cst.corpus-probe.views.text :as text-views]
            [dk.cst.corpus-probe.views.app :as app-views]
            [replicant.string :as replicant])
  (:import [java.io ByteArrayOutputStream]))

(defn ->transit
  "Encode `x` as a transit-JSON string."
  [x]
  (let [out (ByteArrayOutputStream.)]
    (transit/write (transit/writer out :json) x)
    (.toString out "UTF-8")))

(defn correct-quote-escaping
  "Work around a bug in Replicant's string renderer: it escapes `\"` as
  `&#39;` (an apostrophe) instead of `&#34;`, in both attributes and text.
  Real apostrophes are emitted as `&apos;`, so `&#39;` unambiguously marks a
  corrupted double quote and can be restored globally."
  ;; Still present in 2026.07.1, and fixed on Replicant's main branch but
  ;; in no release. Two conditions before dropping this, not one: the
  ;; escape has to emit `&#34;` for a double quote **and** still emit
  ;; `&apos;` for an apostrophe. If a release ever fixed the quote by
  ;; moving apostrophes to `&#39;`, this replacement would turn every
  ;; apostrophe in Danish corpus text into a double quote.
  ;; TODO: report upstream.
  [html]
  (str/replace html "&#39;" "&#34;"))

(defn script-safe
  "Escape `<` in transit text `s` as `\\u003c` so it cannot terminate the
  enclosing <script> element; JSON readers decode the escape back to `<`."
  [s]
  (str/replace s "<" "\\u003c"))

(defn page-title
  "The document title: the page-specific `parts` (most specific first,
  blanks skipped) followed by the app name, so tabs and bookmarks are
  meaningful."
  [& parts]
  (str/join " · " (concat (remove str/blank? parts) ["corpus-probe"])))

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

(def preferences
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
  one that preference accepts; nil otherwise."
  [s k]
  (->> (str/split (str s) #";")
       (keep (fn [item]
               (let [[name v] (str/split (str/trim item) #"=" 2)]
                 (when (= (clojure.core/name k) name) v))))
       (some (fn [v] (when ((preferences k) v) v)))))

(defn request-languages
  "The languages `request` reads, most preferred first: the one it
  stored, then its `Accept-Language` by quality (see
  `accepted-languages`), then Danish and English. The one negotiation:
  the interface takes the first it has a translation for (see
  `request-language`), a document the first it has a file in (see
  dk.cst.corpus-probe.docs/hiccup). Not the URL: a reader's language is
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

(def transit-type
  "The content type the client router asks for and is answered with."
  "application/transit+json")

(defn document
  "The complete HTML document from `opts`: its `:lang`, its `:title`, the
  bypass link, the site header with its `:path` (the page being served,
  which its navigation marks) and navigation `:nav` (see
  dk.cst.corpus-probe.views.layout/site-header), the rendered
  `:body` hiccup (the page's <main>) in #app, and the site footer. The
  `:payload` is the same view data as transit, embedded as the #bootstrap
  script the client takes over from.

  The masthead and the footer sit outside #app, so they are the document's
  banner and contentinfo rather than part of the main content. Each has
  a mount point of its own, because a routed navigation must re-render
  it or it goes stale: the masthead's links carry the current search,
  and the footer's words are in the UI language, which the language
  switch changes without reloading. The plain <div> around each scopes
  no landmark, so the <header> and <footer> inside are still the
  document's banner and contentinfo. Every page mounts the client, so
  every page routes: the client swaps those three regions rather than
  reloading, and the server keeps serving the same complete page for
  anything that does not run it. The document shell and the bootstrap
  script are emitted as strings rather than through Replicant, so the
  transit payload's double quotes are not mangled by the renderer bug
  (see `correct-quote-escaping`). The document language is the UI
  language; corpus text carries its own `lang`."
  [{:keys [lang path title body nav payload] :as opts}]
  (let [ui (i18n/->ui lang)]
    (str "<!DOCTYPE html>"
         "<html lang=\"" lang "\"><head>"
         "<meta charset=\"utf-8\">"
         "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
         (correct-quote-escaping
          (replicant/render
           [:meta {:name    "description"
                   :content (i18n/tr ui (str "Search CWB corpora and read "
                                             "KWIC concordances."))}]))
         (correct-quote-escaping (replicant/render [:title title]))
         "<link rel=\"stylesheet\" href=\"/css/reset.css\">"
         "<link rel=\"stylesheet\" href=\"/css/tokens.css\">"
         "<link rel=\"stylesheet\" href=\"/css/style.css\">"
         "</head><body>"
         (correct-quote-escaping (replicant/render (layout/skip-link ui)))
         "<div id=\"masthead\">"
         (correct-quote-escaping
          (replicant/render (layout/site-header ui path nav)))
         "</div>"
         "<div id=\"app\">"
         (correct-quote-escaping (replicant/render body))
         "</div>"
         "<div id=\"footer\">"
         (correct-quote-escaping (replicant/render (layout/site-footer ui)))
         "</div>"
         (when payload
           (str "<script type=\"" transit-type "\" id=\"bootstrap\">"
                (script-safe payload)
                "</script>"))
         "<script defer src=\"/js/main.js\"></script>"
         "</body></html>")))

(defn wants-transit?
  "True when `request` asks for the data behind a route rather than a
  document: the client router fetching a page it will render itself."
  [request]
  (boolean (some-> (get-in request [:headers "accept"])
                   (str/includes? transit-type))))

(defn transit-response
  "The view data `x` as transit, for the client router.

  It varies by the same things the document does, and is not stored: the
  same URL answers with a document or with data depending on the request,
  and a search is as fresh as the corpora behind it."
  [x]
  {:status  200
   :headers {"Content-Type"  (str transit-type "; charset=utf-8")
             "Vary"          "Accept, Accept-Language, Cookie"
             "Cache-Control" "no-store"}
   :body    (->transit x)})

(defn html-response
  "A complete HTML page response with `html` as its body.

  The page is served in the language its request asks for, so it varies by
  `Accept-Language` and says so; a shared cache would otherwise hand one
  reader's language to the next."
  [html]
  {:status  200
   :headers {"Content-Type" "text/html; charset=utf-8"
             "Vary"         "Accept, Accept-Language, Cookie"}
   :body    html})

(defn filter-key?
  "True when query param key `k` names a metadata filter: the filter
  prefix followed by the attribute name, as in `f.text_year` (see
  dk.cst.corpus-probe.views.page/filter-prefix).

  A query param written without a `=` arrives under a nil key, which names
  no attribute."
  [k]
  (boolean (and k (str/starts-with? (name k) page/filter-prefix))))

(defn multi-param?
  "True when query param key `k` may repeat: the corpus selection and the
  metadata filters, one param per selected value."
  [k]
  (or (= k :corpus) (filter-key? k)))

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
  the set of non-blank values of its `f.<attribute>` params (see
  `filter-key?`), as dk.cst.corpus-probe.search/concordance! takes it;
  empty when nothing is selected."
  [params]
  (into {} (for [[attr v] (prefixed-params params page/filter-prefix)
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
  (let [to (prefixed-params params "ft.")]
    (reduce (fn [m [attr pattern]] (update m attr (fnil conj []) pattern))
            {}
            (concat (remove (comp str/blank? val)
                            (prefixed-params params "fp."))
                    (keep (fn [[attr from]]
                            (some->> (range-pattern from (get to attr))
                                     (vector attr)))
                          (prefixed-params params "ff."))))))

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
  dk.cst.corpus-probe.commands/positions; the start of the match for
  anything else."
  [v]
  (if (some #{v} commands/positions) v "match"))

(defn by-param
  "The attribute the `by` query param value `v` asks a frequency table to
  count its values against (see
  dk.cst.corpus-probe.frequency/frequency-table!), as a keyword; nil when
  it names none, the corpora being the columns then. The attribute is
  checked against each corpus by the breakdown, as every attribute is."
  [v]
  (when-not (str/blank? v) (keyword v)))

(defn subset-param
  "The narrowing the `subset`, `subset-at` and `subset-attr` query params
  of `params` ask for: {:anchor ... :attr ... :value ...} as
  dk.cst.corpus-probe.commands/subset-command takes it, or nil without a
  value. The attribute is checked against each corpus by the search, as
  every attribute is."
  [{:keys [subset subset-at subset-attr]}]
  (when-not (str/blank? subset)
    {:anchor (position-param subset-at)
     :attr   (keyword (attr-param subset-attr))
     :value  subset}))

(defn context-param
  "The width of context the `context` query param value `v` asks for: a
  positive number of words, or the unit of text it names (a key of
  dk.cst.corpus-probe.search/units); the default width for anything
  else."
  [v]
  (let [n (some-> v parse-long)]
    (cond
      (and n (pos? n))                             n
      (and v (contains? search/units (keyword v))) (keyword v)
      :else                                        (:context
                                                    commands/kwic-defaults))))

(defn near-param
  "The word the `near` query param value `word` asks every hit to have
  nearby, at most `distance` (the query param value) words away: {:word
  ... :distance ...} as dk.cst.corpus-probe.commands/near-command takes it,
  or nil for a blank word. A distance that is not a positive integer is
  dk.cst.corpus-probe.views.page/near-distance."
  [word distance]
  (when-not (str/blank? word)
    {:word     (str/trim word)
     :distance (let [n (some-> distance parse-long)]
                 (if (and n (pos? n)) n page/near-distance))}))

(defn search-title
  "The document title of the search page for `params` in `ui`:
  the query, how many hits it found (from `result`, when given), the
  selected corpora (`:corpus`, a vector of names, when any), the metadata
  filter and the page number when past the first; just the app name when
  nothing was searched for.

  The hit count is in the title because a full page reload announces the
  title and nothing else, so the title is where the outcome of a search
  first reaches a screen reader. It is left out when no corpus could be
  searched, since then there is no count to report."
  ([ui params]
   (search-title ui params nil))
  ([ui {:keys [corpus] :as params} result]
   (if-not (page/asked? params)
     (page-title (i18n/tr ui "Search"))
     (let [page-n (:page result 0)
           ;; a search every corpus refused still has a result, of size
           ;; 0; titling that "0 hits" reports an answer the search never
           ;; got, and contradicts the results heading
           hits   (when (page/searched? result)
                    (page/hits-phrase ui (:size result)))]
       (page-title (page/query-phrase ui params)
                   hits
                   ;; only ever beside a count it could have drawn from:
                   ;; a search that found nothing sampled nothing
                   (when (and hits (pos? (:size result 0)))
                     (page/sample-phrase ui (:sample result) corpus))
                   (when (seq corpus) (page/corpora-phrase ui corpus))
                   (page/filter-phrase (filter-params params)
                                       (pattern-params params))
                   (when (pos? page-n)
                     (str (i18n/tr ui "page") " " (inc page-n))))))))

(defn frequency-title
  "The document title of the frequency view for `params` in `ui`: what
  was counted, in which corpora, within the metadata filter, and by what.

  A frequency result counts values rather than hits, so it cannot borrow
  the concordance's title: there is no hit count to report."
  [ui {:keys [corpus attr by] :as params}]
  (page-title (if (page/asked? params)
                (page/query-phrase ui params)
                (i18n/tr ui "All tokens"))
              (when (seq corpus) (page/corpora-phrase ui corpus))
              (page/filter-phrase (filter-params params)
                                  (pattern-params params))
              (str (i18n/tr ui "by") " " attr
                   (when-not (str/blank? by)
                     (str " " (i18n/tr ui "and") " " by)))
              (i18n/tr ui "Frequencies")))

(defn result-title
  "The document title of the search described by `params` in `ui`, shown
  in `view` with `result`: each view names what it shows, since a full
  page load announces the title and nothing else."
  [ui view params result]
  (if (= :frequencies view)
    (frequency-title ui params)
    (search-title ui params result)))

(defn page-href
  "The URL of page `page` of the search `params` cite, counted from
  nought here and from one in the URL.

  Ends in the results fragment, so a page turn lands on the hits rather
  than at the top of the query form. Drops `expand`, which names corpus
  positions on the current page and does not carry to another page's
  hits."
  [params page]
  (url/results-href (assoc (dissoc params :expand) :page (inc page))))

(defn search-params
  "The `params` that identify a search: its corpora, its query as any
  mode reads it (see dk.cst.corpus-probe.url/query-key?; what the mode
  does not read, the URL rule drops from a link), the metadata filter,
  the narrowings of its hits and the sample of them, for linking the
  views of the same hits.
  The interface language is not among them: it is the reader's
  preference, not part of the search.

  The narrowings and the sample are here and the sort is not, because
  which hits there are is part of the search while the order they are
  read in is not. The frequency view draws no sample, but carries the
  param so that returning to the concordance returns to the sample it
  was left in."
  [params]
  (into (select-keys params [:corpus :subset :subset-at :subset-attr
                             :near :distance :sample])
        (filter (comp (some-fn url/query-key? url/metadata-key?) key))
        params))

(defn export-hrefs
  "The URLs of the TSV and CSV exports of `view` (`:kwic` or
  `:frequencies`) of the search described by `params`, by format."
  [view params]
  (into {} (for [format (keys export/formats)]
             [(keyword format) (url/export-href view format params)])))

(defn read-keys
  "The query params among `params` that the form of `mode` reads (see
  dk.cst.corpus-probe.url/reads?), as keys."
  [mode params]
  (filter #(and (url/query-key? %) (not= :mode %) (url/reads? mode %))
          (keys params)))

(defn value-lists!
  "The values of each positional attribute among `attrs` (keywords) that
  every one of `corpora` via `ctx` can list (see
  dk.cst.corpus-probe.tools/attribute-values!): attribute to its values
  over all of them, collated. An attribute one corpus cannot list, or
  lacks, has no entry, since a list missing part of what a reader may
  search for would mislead. The value fields of the extended search
  offer them as suggestions."
  [ctx corpora attrs]
  (let [collator (search/->collator ctx)
        lists    (search/pmap-n (search/parallelism ctx)
                                (fn [corpus]
                                  (into {}
                                        (for [attr attrs]
                                          [attr (try (tools/attribute-values!
                                                      ctx corpus attr)
                                                     (catch Exception _ nil))])))
                                corpora)]
    (into {}
          (for [attr attrs
                :let [values (map #(get % attr) lists)]
                :when (and (seq values) (every? some? values))]
            [attr (vec (sort collator (distinct (apply concat values))))]))))

(def follow-on-errors
  "The errors CQP adds for the later commands of a batch once an earlier
  one has failed: a query without an activated corpus, and every command
  on the then undefined `Last` or the metadata filter's subcorpus."
  ["CQP Error:\n\tNo corpus activated"
   "CQP Error:\n\tCorpus ``Last'' is undefined"
   "CQP Error:\n\tCorpus ``Filter'' is undefined"])

(defn drop-follow-on-errors
  "Remove the `follow-on-errors` from CQP stderr text `message` when it
  reports anything else, so the user sees the failing command's own error;
  a message of nothing but follow-on errors is kept as it is."
  [message]
  (let [trimmed (-> (reduce #(str/replace %1 %2 "") message follow-on-errors)
                    (str/replace #"\n{2,}" "\n")
                    (str/trim))]
    (if (str/blank? trimmed) message trimmed)))

(defn public-error
  "Prepare CQP `error` for display: drop `CL warning:` lines from its
  message, which concern the server installation rather than the query and
  may name absolute server paths (never to reach a rendered page), and the
  follow-on errors our own batch commands add after a failed query. The
  query error text itself stays verbatim, `<--` pointer included."
  [error]
  (if-let [message (:message error)]
    (assoc error :message (->> (str/split message #"\n")
                               (remove #(str/starts-with? % "CL warning:"))
                               (str/join "\n")
                               (drop-follow-on-errors)
                               (not-empty)))
    error))

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

(defn content-lang
  "The language code of the corpus named `corpus` among the registry entry
  maps `corpora`, when its entry records a plausible one."
  [corpora corpus]
  (some (fn [{:keys [id] :as m}]
          (when (= corpus (str/upper-case id))
            (corpus/language m)))
        corpora))

(defn folder-ids
  "Every corpus ID named by the configured `folders` tree."
  [folders]
  (set (mapcat corpus-views/folder-corpora folders)))

(defn empty-folder?
  "True when resolved `folder` holds no corpora at any depth."
  [{:keys [corpora folders] :as folder}]
  (and (empty? corpora) (every? empty-folder? folders)))

(defn resolve-folder
  "Replace the corpus IDs of configured `folder` (and of its subfolders)
  with their overview maps from `by-id`, dropping IDs the registry does not
  know and subfolders left empty by that."
  [by-id {:keys [label corpora folders] :as folder}]
  {:label   label
   :corpora (into [] (keep by-id) corpora)
   :folders (into [] (comp (map #(resolve-folder by-id %))
                           (remove empty-folder?))
                  folders)})

(defn grouped-corpora
  "Group the corpus `overviews` by the configured `folders` tree; corpora no
  folder claims follow as a final label-less folder, so a corpus never
  disappears because the configuration lags behind the registry. Folders
  the registry leaves empty are dropped."
  [folders overviews]
  (let [by-id     (into {} (map (juxt :id identity)) overviews)
        unclaimed (vec (remove (comp (folder-ids folders) :id) overviews))]
    (cond-> (into [] (comp (map #(resolve-folder by-id %))
                           (remove empty-folder?))
                  folders)
      (seq unclaimed) (conj {:label nil :corpora unclaimed :folders []}))))

(defn overview!
  "The overview of registry entry map `m` via `ctx`, sizeless (and
  uncached) when the size cannot be read right now."
  [ctx m]
  (try (corpus/overview! ctx m)
       (catch Exception _ (corpus/overview m))))

(defn corpus-tree!
  "The registry `corpora` (maps as from dk.cst.corpus-probe.corpus/corpora)
  summarized via `ctx`, in parallel since each summary is a CQP round trip
  on a cache miss, and grouped by its configured folder tree."
  [ctx corpora]
  (grouped-corpora (:folders ctx)
                   (vec (search/pmap-n (search/parallelism ctx)
                                       #(overview! ctx %)
                                       corpora))))

(defn split-known
  "Split the `selected` corpus names into [known unknown] by the registry
  `corpora`, so that only names the registry has reach a command and the
  rest are reported without spawning anything."
  [corpora selected]
  (let [known? (set (map (comp str/upper-case :id) corpora))]
    [(filterv known? selected) (vec (remove known? selected))]))

(defn unknown-counts
  "The count entries reporting the `unknown` corpus names as such."
  [unknown]
  (mapv (fn [corpus] {:corpus corpus :error {:type :unknown-corpus}})
        unknown))

(defn public-counts
  "The per-corpus counts of concordance `result` with each error prepared
  for display by `public-error`."
  [result]
  (update result :counts
          (partial mapv #(cond-> % (:error %) (update :error public-error)))))

(defn public-result
  "`result` with its counts prepared for display (see `public-counts`)
  and its :pages, once every corpus is counted."
  [result]
  (cond-> (public-counts result)
    (not (:remaining result)) (assoc :pages (page-count result))))

(defn search-outcome!
  "Search the `known` corpora for `cqp` via `ctx` with `opts` (the :page,
  :sort, :context, :sample, :filter, :near, :within and :incremental? of
  dk.cst.corpus-probe.search/concordance!): {:result <concordance with
  its :pages, once every corpus is counted>}, the `unknown` corpus names
  reported among its counts, or {:error ...} when no corpus was selected
  at all. Per-corpus errors travel inside the result."
  [ctx known unknown cqp opts]
  (if (and (empty? known) (empty? unknown))
    {:error {:type :no-corpus}}
    {:result (-> (search/concordance! ctx known cqp opts)
                 (update :counts into (unknown-counts unknown))
                 (public-result))}))

(defn pattern-fields
  "What the pattern and range fields of the metadata filter hold, from
  `params`: the `:patterns`, attribute to its `fp.` param, and the
  `:ranges`, attribute to its [`ff.` `ft.`] params, as the form shows
  them back (see dk.cst.corpus-probe.views.page/pattern-row)."
  [params]
  (let [from (prefixed-params params "ff.")
        to   (prefixed-params params "ft.")]
    {:patterns (prefixed-params params "fp.")
     :ranges   (into {} (for [attr (distinct (concat (keys from) (keys to)))]
                          [attr [(get from attr) (get to attr)]]))}))

(defn filter-controls!
  "The metadata filter controls of the search form over the `known`
  corpora via `ctx`: the filters they offer (see
  dk.cst.corpus-probe.frequency/filter-options!) plus the `:selected`
  values of `params` (see `filter-params`) and what its pattern and range
  fields hold (see `pattern-fields`)."
  [ctx known params]
  (merge (frequency/filter-options! ctx known)
         {:selected (filter-params params)}
         (pattern-fields params)))

(defn readable-corpora
  "The names of the registry `corpora` CWB can read right now, via `ctx`,
  in registry order.

  This is what a request that names no corpus searches: exactly the set
  the chooser would let a reader tick, since it disables the rest. The
  overviews are cached, and the chooser asks for the same ones on every
  page, so this costs nothing on a warm cache."
  [ctx corpora]
  (into []
        (comp (filter :size) (map (comp str/upper-case :id)))
        (search/pmap-n (search/parallelism ctx) #(overview! ctx %) corpora)))

(defn selected-corpora
  "The corpus names `params` asks for against the registry `corpora` via
  `ctx`: those it names, or every readable corpus when it names none and
  the selection is not one the reader made.

  The search form submits a `scope` param alongside its checkboxes, so an
  empty selection a reader ticked their way to is answered with the no
  corpus error rather than silently widened to the whole registry."
  [ctx corpora params]
  (let [named (url/corpora-param (:corpus params))]
    (if (or (seq named) (contains? params :scope))
      named
      (readable-corpora ctx corpora))))

(defn search-request
  "What `request` asks of `ctx`: its scalar query params, the registry's
  corpora, the corpus names `named` in the params, the names `selected`
  to search, those split into the `known` and the `unknown` (see
  `split-known`), what `arrived` with the form, a change of its mode
  allowed for (see dk.cst.corpus-probe.query/arrived), the `query` that
  runs, the CQP it compiles to (see dk.cst.corpus-probe.query/->cqp) and
  the `opts` every search of it
  takes: its metadata :filter (see `filter-params`) and the :patterns
  beside it (see `pattern-params`), the unit of text it is kept :within
  (see dk.cst.corpus-probe.query/within), the :subset of its hits kept
  (see `subset-param`) and the word its hits are :near (see
  `near-param`). A request naming no corpus searches every readable one
  (see `selected-corpora`).

  Every handler that answers a search starts from this."
  [ctx request]
  (let [params   (scalar-params (:query-params request))
        arrived  (query/arrived params)
        query    (:query arrived)
        corpora  (corpus/corpora ctx)
        selected (selected-corpora ctx corpora params)
        [known unknown] (split-known corpora selected)]
    {:params   params
     :arrived  arrived
     :query    query
     :corpora  corpora
     :named    (url/corpora-param (:corpus params))
     :selected selected
     :known    known
     :unknown  unknown
     :cqp      (query/->cqp query)
     :opts     {:filter   (filter-params params)
                :patterns (pattern-params params)
                :within   (query/within query)
                :subset   (subset-param params)
                :near     (near-param (:near params) (:distance params))}}))

(defn attr-options!
  "The attribute descriptions ({:type :name}) offered for grouping the
  `corpora` via `ctx`: their union, positional attributes first.

  Each kind keeps the registry order of the first corpus reporting it; a
  corpus that cannot be read contributes none. Falls back to word, the one
  attribute every corpus has. Every attribute is offered whatever the
  query: a structural one cannot table a whole corpus, and that request
  is then rejected with its reason, so the form still shows what was
  asked."
  [ctx corpora]
  (let [attrs (->> corpora
                   (mapcat (fn [c] (try (frequency/groupable-attrs! ctx c)
                                        (catch Exception _ nil))))
                   (map #(select-keys % [:type :name]))
                   (distinct)
                   (sort-by (comp {:positional 0 :structural 1} :type))
                   (vec))]
    (if (seq attrs) attrs [{:type :positional :name :word}])))

(defn subset-href
  "The URL of the concordance of the search described by `params` kept to
  the hits whose token at `anchor` has `value` as its `attr`: what one
  row of the frequency table grouped by `attr` at `anchor` counted."
  [params attr anchor value]
  (url/results-href (assoc (search-params params)
                           :view        "kwic"
                           :subset      value
                           :subset-at   anchor
                           :subset-attr (name attr))))

(defn linked-rows
  "The frequency `result` with a `subset-href` on each of the rows the
  table shows (its first dk.cst.corpus-probe.views.frequencies/row-limit),
  for the search described by `params`: the rows past those go
  unlinked, since the table does not show them and an export reads no
  links."
  [params {:keys [attr at] :as result}]
  (update result :rows
          (fn [rows]
            (into (mapv #(assoc % :href (subset-href params attr at (:value %)))
                        (take freq-views/row-limit rows))
                  (drop freq-views/row-limit rows)))))

(defn frequency-outcome!
  "Table the `known` corpora for `cqp` (nil for the whole corpora) by
  `attr` via `ctx` with `opts` (the :at, :by, :docs, :filter, :within,
  :subset and :near of dk.cst.corpus-probe.frequency/frequency-table!):
  {:result ...}, the `unknown` corpus names reported among its counts,
  or {:error ...} when no corpus was selected at all. Per-corpus errors
  travel inside the result."
  [ctx known unknown cqp attr opts]
  (if (and (empty? known) (empty? unknown))
    {:error {:type :no-corpus}}
    {:result (-> (frequency/frequency-table! ctx known (or cqp "") attr opts)
                 (update :counts into (unknown-counts unknown))
                 (public-counts))}))

(def result-views
  "The views a search result can be shown in, in display order: the
  keyword naming each and its `view` param value. What each is called is
  the interface's business (see
  dk.cst.corpus-probe.views.page/view-label).

  A frequency table is not another page, it is the same search counted
  rather than listed, so it is a view of the result rather than a place of
  its own."
  [[:kwic "kwic"]
   [:frequencies "frequencies"]])

(defn view-param
  "The result view named by the `view` query param value `v`: the
  concordance for anything that does not name another view."
  [v]
  (or (some (fn [[k value]] (when (= v value) k)) result-views) :kwic))

(defn view-hrefs
  "Each result view of the search described by `params`, for the switch at
  the top of the results region: [view-keyword url], in display order.

  Every view of one search shares its URL but for the `view` param, so
  moving between them keeps the query, the corpora and the filter by
  construction rather than by carrying them across."
  [params]
  (for [[k value] result-views]
    [k (url/results-href (assoc (search-params params)
                                :view    value
                                :attr    (:attr params)
                                :at      (:at params)
                                :by      (:by params)
                                :docs    (:docs params)
                                :sort    (:sort params)
                                :context (:context params)))]))

(defn sort-options
  "The sort modes offered over corpora whose positional attributes are
  `attrs` (keywords): the fixed modes (see
  dk.cst.corpus-probe.commands/sort-modes), then a sort by each attribute
  but word, which the match sort already is."
  [attrs]
  (into (mapv first commands/sort-modes)
        (comp (remove #{:word}) (map name))
        attrs))

(defn search-view-data
  "The data dk.cst.corpus-probe.views.app/search-view renders one
  search page for `request` against `ctx` from: the state of the form, the
  outcome of the search when the params describe one, and the links out
  of it.

  One search, two views: the `view` param decides whether its hits are
  listed as a concordance or counted as a frequency table, and each view
  contributes only the controls and links it has (a sort and pagination
  for the concordance, a grouping for the table).

  Links are built from `:cited`, the params as the URL cites them (see
  dk.cst.corpus-probe.url/canonical); `:params`, which fills the form,
  holds the query the form holds in the form's own spelling (see
  dk.cst.corpus-probe.query/arrived and /params), over what arrived, so
  that a control the form's mode does not read keeps what it carried, as
  memory. It names every corpus searched, or only what the URL named
  when nothing was searched: a reader arriving at the form starts with
  no corpus selected, while a URL naming no corpus still searches every
  readable one and shows them all. It also carries `:cqp`, the CQP the
  query compiled to, which the heading and the title of an extended
  search show; no URL carries it, the URL rule not knowing it. The
  tokens of the extended form are `:tokens` (see
  dk.cst.corpus-probe.query/form-rows), the values its fields suggest
  `:value-lists` (see `value-lists!`), and what a change of mode could
  not keep is `:switch`, its `:loss` and the `:unread` params, for the
  form's status line (see dk.cst.corpus-probe.views.page/switch-notice).

  The same map is embedded as transit for the client to take over from,
  so it holds corpus overviews only: the full registry maps carry
  absolute server paths and stay here."
  [ctx request]
  (let [{:keys [params arrived corpora named selected known unknown cqp
                opts]}
        (search-request ctx request)
        {:keys [form held loss unread]} arrived
        lang    (request-language request)
        view    (view-param (:view params))
        attr    (attr-param (:attr params))
        at      (position-param (:at params))
        docs    (some? (:docs params))
        page-n  (page-param (:page params))
        freq?   (= :frequencies view)
        outcome (cond
                  ;; a blank query counts every token of the corpora, but
                  ;; not one blank only because the mode was changed under
                  ;; a query the new mode could not keep whole: that request
                  ;; shows the form and searches nothing (see `arrived`)
                  (and freq? (or cqp (and (not (url/unread-query? params))
                                          (or (seq known) (seq unknown)))))
                  (-> (frequency-outcome! ctx known unknown cqp attr
                                          (assoc opts
                                                 :at   at
                                                 :docs docs
                                                 :by   (by-param (:by params))
                                                 ;; the concordance saved
                                                 ;; its result under this
                                                 :sort (:sort params)))
                      (update :result #(some->> % (linked-rows params))))

                  cqp
                  (search-outcome! ctx known unknown cqp
                                   (assoc opts
                                          :page    page-n
                                          :sort    (:sort params)
                                          :context (context-param
                                                    (:context params))
                                          :sample  (sample-param
                                                    (:sample params))
                                          ;; a document waits for the count;
                                          ;; the client asks for it itself
                                          ;; (see `counts-page`)
                                          :incremental? (wants-transit?
                                                         request))))
        ;; the form's marker of the mode it was rendered in is no param of
        ;; the search; what its mode reads is the query it holds
        params* (-> (apply dissoc params :from (read-keys form params))
                    (merge (query/params form held))
                    (assoc :mode   form
                           :corpus (if outcome selected named)
                           :attr   attr
                           :at     at
                           :cqp    cqp))
        cited   (url/canonical params* (set (readable-corpora ctx corpora)))
        attrs   (attr-options! ctx known)
        ;; what a simple search may match, and a concordance sort by: the
        ;; positional attributes of the corpora it is over
        p-attrs (search/attr-names #(= :positional (:type %)) attrs)]
    (cond->
     {:lang            lang
      :view            view
      :folders         (corpus-tree! ctx corpora)
      :filter-controls (filter-controls! ctx known params)
      :search-attrs    p-attrs
      :tokens          (query/form-rows (when (= "extended" form) held))
      :switch          {:loss loss :unread unread}
      :value-lists     (value-lists! ctx known p-attrs)
      :params          params*
      :cited           cited
      :result          (:result outcome)
      :error           (:error outcome)
      :view-hrefs      (view-hrefs cited)
      :langs           (into {}
                             (map (juxt identity
                                        #(content-lang corpora %)))
                             selected)}
      freq?
      (assoc :attrs        attrs
             :positions    commands/positions
             :export-hrefs (when (:result outcome)
                             (export-hrefs :frequencies
                                           (assoc (search-params cited)
                                                  :attr attr
                                                  :at   at
                                                  :by   (:by params)
                                                  :docs (:docs params)))))

      (not freq?)
      (merge {:sort-modes   (sort-options p-attrs)
              :export-limit export/hit-limit
              :export-hrefs (when (:result outcome)
                              (export-hrefs :kwic
                                            (assoc (search-params cited)
                                                   :sort    (:sort params)
                                                   :context (:context params))))}
             (page-hrefs cited page-n (:result outcome))))))

(defn nav-hrefs
  "The URL of each top-level page for `params`.

  No URL names a language: which language a reader reads in is their own
  preference, so none of these carries one. The search keeps the current
  query, so returning to it from the corpus index does not lose it. The
  frequency table is not here: it is a view of a search result, reached by
  the switch at the top of the results region (see `view-hrefs`)."
  [params]
  (let [search (search-params params)]
    {:search          (if (seq search)
                        (url/results-href search)
                        url/search)
     :corpora-heading url/corpora
     :glossary        url/glossary}))

(defn shell-data
  "The parts of a page the masthead is built from, for `request` with
  search `params`: the `:path` being served, which its navigation marks as
  current and its language switch returns to, and the navigation `:nav`
  itself.

  They travel in the view data because the client re-renders the masthead,
  and the navigation depends on the search the reader is looking at."
  [request params]
  {:path (:uri request)
   :nav  (nav-hrefs params)})

(defn page-response
  "Answer `request` with the page `data` describes under `title`: as
  transit when the client router asked for it, else as the document its
  route renders from the same data.

  The masthead's own parts are merged in here, built for `nav-params`,
  the search its navigation carries. Every page is this one shape, and
  both representations come from one place, so the page the server paints
  and the page the client renders can never describe different things."
  ([request title data]
   (page-response request title data {}))
  ([request title data nav-params]
   (let [lang (:lang data)
         data (merge data (shell-data request nav-params))]
     (if (wants-transit? request)
       (transit-response (assoc data :title title))
       (html-response
        (document {:lang    lang
                   :path    (:path data)
                   :title   title
                   :nav     (:nav data)
                   :body    (app-views/page data)
                   :payload (->transit (assoc data :title title))}))))))

(defn search-page
  "Handle a search-page `request` against `ctx`: render the form, and when
  the query params describe a search, its concordance or the reason there
  is none, else the search guide (see dk.cst.corpus-probe.docs) where
  the results will be.

  `:cited` goes to the masthead's navigation and not to the client, which
  does not read it."
  [ctx request]
  (let [{:keys [lang view params result error cited]
         :as   data} (search-view-data ctx request)
        data (cond-> (assoc (dissoc data :cited) :route :search)
               (not (or result error))
               (assoc :guide (docs/hiccup "guide"
                                          (request-languages request))))]
    (page-response request
                   (result-title (i18n/->ui lang) view params result)
                   data
                   cited)))

(defn document-page
  "Handle a `request` for the document called `name` (see
  dk.cst.corpus-probe.docs): the frontpage, where the app says what it
  is and where a reader goes from here, or the glossary. Served in the
  first language the request reads that the document has, and titled as
  the document titles itself."
  [_ctx name request]
  (let [langs  (request-languages request)
        blocks (docs/hiccup name langs)]
    (page-response request
                   (page-title (docs/title blocks))
                   {:route :document
                    :lang  (first (filter i18n/supported? langs))
                    :data  {:body blocks}})))

(defn text-response
  "A 200 response serving `body` (text, or a function writing it to the
  response stream as it goes) in export `format` (a key of
  dk.cst.corpus-probe.export/formats) as a download named `name`."
  [format name body]
  {:status  200
   :headers {"Content-Type"        (:content-type (export/formats format))
             "Content-Disposition" (str "attachment; filename=\"" name "."
                                        format "\"")}
   :body    body})

(defn export-failure
  "A 400 response explaining, corpus by corpus, why the search behind an
  export produced nothing to export, from the per-corpus `counts`."
  [counts]
  {:status  400
   :headers {"Content-Type" "text/plain; charset=utf-8"}
   :body    (->> (:counts (public-counts {:counts counts}))
                 (map (fn [{:keys [corpus error]}]
                        (str corpus ": " (name (:type error :cqp))
                             (some->> (:message error) (str "\n")))))
                 (str/join "\n\n"))})

(defn export-response
  "The download of `rows` (a header and data rows of strings, see
  dk.cst.corpus-probe.export) rendered in `format` under `name`, or the
  `export-failure` when `readable?` says no corpus of the `counts` could
  be searched, so a failed search never downloads as an empty file."
  [format name readable? {:keys [counts]} rows]
  (if (some readable? counts)
    (text-response format name ((:render (export/formats format)) rows))
    (export-failure counts)))

(defn export-columns!
  "The annotation columns a concordance export over `corpora` via `ctx`
  has: the union of their positional attributes but word, which the
  match column is, and of their annotated s-attributes, each kind in
  the registry order of the first corpus reporting it; a corpus that
  cannot be read contributes none."
  [ctx corpora]
  (let [attrs (mapcat (fn [c] (try (corpus/attributes! ctx c)
                                   (catch Exception _ nil)))
                      corpora)]
    {:p-attrs      (vec (distinct (search/attr-names
                                   #(and (= :positional (:type %))
                                         (not= :word (:name %)))
                                   attrs)))
     :struct-attrs (vec (distinct (search/attr-names search/annotated-s-attr?
                                                     attrs)))}))

(defn export-kwic
  "Handle a concordance export `request` against `ctx` in `format` (a
  key of dk.cst.corpus-probe.export/formats): the hits of the query in
  the selected corpora, the first dk.cst.corpus-probe.export/hit-limit
  of them in the requested sort, as a TSV or CSV download; 400 without
  a query, known corpora or a known format, or when no corpus could be
  searched.

  The corpora are exported one at a time (see
  dk.cst.corpus-probe.search/export-corpora!) and written as each
  answers, so the download holds one corpus's rows at a time. The first
  corpus to answer is waited for before the download starts, because a
  download once started can no longer be a 400: until one answers, the
  corpora that failed are collected, and if every one fails their
  reasons are the answer, as `export-failure` gives them."
  [ctx request format]
  (let [{:keys [params known cqp opts]} (search-request ctx request)]
    (if-not (and cqp (seq known) (export/formats format))
      {:status 400 :body "bad request"}
      (let [{:keys [line preamble]} (export/formats format)
            {:keys [p-attrs struct-attrs]} (export-columns! ctx known)
            header    (export/kwic-header p-attrs struct-attrs)
            deadline  (search/deadline ctx)
            opts      (assoc opts
                             :sort    (:sort params)
                             :context (context-param (:context params))
                             :sample  (sample-param (:sample params)))
            [failed [head]] (split-with :error
                                        (search/export-corpora!
                                         ctx known cqp deadline
                                         export/hit-limit opts))
            remaining (drop (inc (count failed)) known)
            left      (- export/hit-limit (count (:rows head)))]
        (if (nil? head)
          (export-failure failed)
          (text-response
           format "kwic"
           (fn [out]
             (with-open [w (io/writer out :encoding "UTF-8")]
               (.write w (str preamble (line header)))
               ;; the corpora after the first are exported inside the
               ;; stream, and nothing outside it holds their rows
               (doseq [export (cons head (search/export-corpora!
                                          ctx remaining cqp deadline left
                                          opts))
                       row    (export/kwic-rows p-attrs struct-attrs export)]
                 (.write w ^String (line row)))))))))))

(defn export-frequencies
  "Handle a frequency table export `request` against `ctx` in `format`
  (a key of dk.cst.corpus-probe.export/formats): every row of the
  breakdown of the query (or of the whole corpora) by the `attr` param,
  against the `by` param when there is one, as a TSV or CSV download;
  400 without known corpora or a known format, or when no corpus could
  be counted."
  [ctx request format]
  (let [{:keys [params known cqp opts]} (search-request ctx request)]
    (if-not (and (seq known) (export/formats format)
                 (not (and (nil? cqp) (url/unread-query? params))))
      {:status 400 :body "bad request"}
      (let [table (frequency/frequency-table!
                   ctx known (or cqp "") (attr-param (:attr params))
                   (assoc opts
                          :at   (position-param (:at params))
                          :by   (by-param (:by params))
                          :docs (some? (:docs params))
                          :sort (:sort params)))]
        (export-response format "frequencies" :tokens table
                         (if (:by table)
                           (export/crosstab-table table)
                           (export/frequency-table table)))))))

(def export-file
  "What an export is named as under the search path: the view of the
  result as its name and the format as its extension, `kwic.tsv` (see
  dk.cst.corpus-probe.url/export)."
  #"(kwic|frequencies)\.(tsv|csv)")

(defn export-page
  "Handle an export `request` against `ctx`: its `:file` path parameter
  names the view exported and the format it takes (see `export-file`);
  404 for a file that names neither."
  [ctx request]
  (let [[_ view format] (re-matches export-file
                                    (str (get-in request [:path-params :file])))]
    (case view
      "kwic"        (export-kwic ctx request format)
      "frequencies" (export-frequencies ctx request format)
      {:status 404 :body "not found"})))

(defn corpora-page
  "Handle the corpus index `request` against `ctx`: every registry corpus,
  summarized and grouped by the configured folder tree."
  [ctx request]
  (let [lang (request-language request)
        ui   (i18n/->ui lang)]
    (page-response request
                   (page-title (i18n/tr ui "Corpora"))
                   {:route :corpora
                    :lang  lang
                    :data  {:folders (corpus-tree! ctx (corpus/corpora ctx))}})))

(defn corpus-page
  "Handle a corpus info page `request` against `ctx`, rendering the corpus
  named by the :id path parameter (case-insensitively); 404 when it is not
  a registry corpus."
  [ctx request]
  (let [lang   (request-language request)
        corpus (str/upper-case (str (get-in request [:path-params :id])))
        file   (when (commands/corpus-name? corpus)
                 (corpus/registry-file ctx corpus))]
    (if-not (and file (corpus/registry-file? file))
      {:status 404 :body "not found"}
      (let [registry (corpus/read-registry file)
            outcome  (try {:stats (tools/describe-corpus! ctx corpus)
                           :info  (corpus/info! ctx corpus)}
                          (catch Exception e
                            {:error    (search/error-map e)
                             :phantom? (corpus/phantom? e)}))]
        (page-response request
                       (page-title corpus)
                       {:route :corpus
                        :lang  lang
                        ;; the corpus's own language lives in :data, where
                        ;; info-view reads it; the UI language is the page's
                        :data  (assoc outcome
                                      :corpus corpus
                                      :title  (not-empty (:name registry))
                                      :lang   (corpus/language registry))})))))

(defn text-page
  "Handle a reading page `request` against `ctx`: the text of the corpus
  named by the :id path parameter that holds the corpus position of the
  `cpos` query param, with the hit from there to the `matchend` param
  (`cpos` itself when absent) marked (see
  dk.cst.corpus-probe.views.text/reading-view); 404 when the corpus is
  not a registry corpus, the position is not a number or no text holds
  it. Without any position the reader is sent to the corpus page, there
  being no text to pick without one. A corpus that marks no texts, or a
  CQP failure, is a page saying so."
  [ctx request]
  (let [lang     (request-language request)
        corpus   (str/upper-case (str (get-in request [:path-params :id])))
        file     (when (commands/corpus-name? corpus)
                   (corpus/registry-file ctx corpus))
        {:keys [cpos matchend]} (:query-params request)
        cpos*    (parse-long (str cpos))]
    (cond
      (not (and file (corpus/registry-file? file)))
      {:status 404 :body "not found"}

      (str/blank? (str cpos))
      {:status 303 :headers {"Location" (url/corpus corpus)} :body ""}

      (nil? cpos*)
      {:status 404 :body "not found"}

      :else
      (let [outcome (try (search/text! ctx corpus cpos*)
                         (catch Exception e
                           {:error (public-error (search/error-map e))}))
            ;; a hit ends where it starts unless told otherwise, and
            ;; never before it starts
            end     (max cpos* (or (some-> matchend str parse-long) cpos*))]
        (if (nil? outcome)
          {:status 404 :body "not found"}
          (page-response
           request
           (page-title (text-views/text-name (i18n/->ui lang)
                                             (:structs outcome))
                       corpus)
           {:route :text
            :lang  lang
            ;; the corpus's own language lives in :data, as on the
            ;; corpus page; the UI language is the page's
            :data  (assoc outcome
                          :corpus corpus
                          :hit    [cpos* end]
                          :lang   (corpus/language
                                   (corpus/read-registry file)))}))))))

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

(def cookie-max-age
  "How long a stored preference outlives the visit that set it: a year, so
  a reader states it once."
  31536000)

(defn preference-cookies
  "The Set-Cookie headers storing every `preferences` setting that
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
        preferences))

(defn preferences-page
  "Store the settings a reader chose and send them back where they were.

  A preference is state, so it is set with a POST and answered with a
  redirect: the page they return to is the one they were reading, with
  their choice applied, and a refresh does not re-submit the form they
  came from. Cookies are the whole persistence: no URL names a
  preference, so a link can be shared without imposing the sharer's
  settings on whoever opens it."
  [_ctx request]
  (let [params  (:form-params request)
        cookies (preference-cookies params)]
    {:status  303
     :headers (cond-> {"Location" (safe-return (:return params))}
                (seq cookies) (assoc "Set-Cookie" cookies))
     :body    ""}))

(defn resource-response
  "A 200 response serving classpath `resource` as `content-type`, uncached
  so dev assets always refetch."
  [content-type resource]
  {:status  200
   :headers {"Content-Type"  content-type
             "Cache-Control" "no-store"}
   :body    (slurp resource)})

(def expanded-context
  "Context width, in tokens each side, for an expanded hit."
  50)

(defn filters-page
  "Return the metadata filters the corpora named in the request offer, as
  transit, so the client can refresh the filter fieldset when the corpus
  selection changes without submitting a search.

  Only names the registry has reach CQP (see `split-known`); an unknown
  one simply contributes nothing, since nothing is being searched here.

  The values a reader has chosen are not answered: those are the reader's
  and the client is already holding them. An attribute list that no
  longer offers a chosen value leaves that value where it is (see
  dk.cst.corpus-probe.views.page/filter-details), so narrowing the corpora
  never quietly drops part of a filter.

  The per-corpus half of this is cached against each registry file (see
  dk.cst.corpus-probe.tools/annotation-values!), so a repeat selection
  costs the merge and the collated sort rather than a CQP round trip."
  [ctx request]
  (let [corpora   (corpus/corpora ctx)
        named     (url/corpora-param (:corpus (:query-params request)))
        [known _] (split-known corpora named)]
    {:status  200
     :headers {"Content-Type"  "application/transit+json; charset=utf-8"
               "Cache-Control" "no-store"}
     :body    (->transit (frequency/filter-options! ctx known))}))

(defn context-page
  "Return the hit at the requested corpus position with wider context, as
  transit, for the client's context expansion.

  Rejects an invalid corpus or non-integer `cpos`/`matchend`, since those are
  interpolated into a CQP query (as validated integers) outside the sandbox."
  [ctx request]
  (let [{:keys [corpus cpos matchend]} (:query-params request)
        cpos*     (parse-long (str cpos))
        matchend* (parse-long (str matchend))]
    (if-not (and (commands/corpus-name? corpus) cpos* matchend*)
      {:status 400 :body "bad request"}
      (try
        (let [q      (commands/position-query cpos* matchend*)
              ;; one hit at a position nothing will ask for again, so
              ;; saving it would only fill the cache (see
              ;; dk.cst.corpus-probe.cache)
              result (search/kwic! ctx corpus q {:context      expanded-context
                                                 :rows         [0 0]
                                                 :struct-attrs []
                                                 :cache?       false})]
          (if-let [hit (first (:hits result))]
            {:status  200
             :headers {"Content-Type"  "application/transit+json; charset=utf-8"
                       "Cache-Control" "no-store"}
             :body    (->transit hit)}
            ;; a position that matches nothing (out of range) is not found
            {:status 404 :body "not found"}))
        (catch Exception _
          {:status 404 :body "not found"})))))

(defn counts-page
  "Answer the count of the search `request` describes against `ctx`, as
  transit: the per-corpus `:counts`, the `:size` and `:pages` of the
  whole result, the `:prev-href` and `:next-href` of the page the
  request names and the document `:title`, everything about a page that
  the count decides. For the client, whose page arrived while its
  corpora were still being counted (see `search-view-data`).

  Asked with the page's own params, so it counts the question the page
  answered. The corpora the page showed were remembered as it was filled
  (see dk.cst.corpus-probe.search/remember-size!), so only the rest cost
  a query, and each count is remembered for the next page. A request
  describing no search is refused."
  [ctx request]
  (let [{:keys [params corpora selected known unknown cqp opts]}
        (search-request ctx request)]
    (if-not cqp
      {:status 400 :body "bad request"}
      (let [page-n  (page-param (:page params))
            params* (assoc params :corpus selected :cqp cqp)
            counts  (-> (search/corpus-sizes!
                         ctx known cqp (search/deadline ctx)
                         (assoc opts :sample (sample-param (:sample params))))
                        (into (unknown-counts unknown)))
            result  (public-result (assoc commands/page-defaults
                                          :page   page-n
                                          :counts counts
                                          :size   (reduce + (keep :size counts))))
            cited   (url/canonical params* (set (readable-corpora ctx corpora)))]
        (transit-response
         (merge (select-keys result [:counts :size :pages])
                (page-hrefs cited page-n result)
                {:title (search-title (i18n/->ui (request-language request))
                                      params* result)}))))))

(defn public-file
  "Serve the file under public/`dir` named by the splat `:path` of
  `request` as `content-type`: a stylesheet, or a compiled client asset.

  Rejects `..` segments directly: `io/resource` follows them out of the
  directory, so a normalising router is not relied on as the only guard."
  [content-type dir request]
  (let [path (get-in request [:path-params :path])]
    (if-let [resource (and (not (str/includes? path ".."))
                           (io/resource (str "public/" dir "/" path)))]
      (resource-response content-type resource)
      {:status 404 :body "not found"})))

(defn routes
  "The route table, with handlers closed over `ctx`."
  [ctx]
  #{[url/home                     :get (partial document-page ctx "frontpage")
     :route-name ::home]
    [url/glossary                 :get (partial document-page ctx "glossary")
     :route-name ::glossary]
    [url/search                   :get (partial search-page ctx)
     :route-name ::search]
    [(str url/search "/:file")    :get (partial export-page ctx)
     :route-name ::export]
    [layout/preferences-path      :post (partial preferences-page ctx)
     :route-name ::preferences]
    [url/corpora                  :get (partial corpora-page ctx)
     :route-name ::corpora]
    [(str url/corpora "/:id")     :get (partial corpus-page ctx)
     :route-name ::corpus]
    [(str url/corpora "/:id/text") :get (partial text-page ctx)
     :route-name ::text]
    ["/api/context"               :get (partial context-page ctx)
     :route-name ::context]
    ["/api/filters"               :get (partial filters-page ctx)
     :route-name ::filters]
    ["/api/counts"                :get (partial counts-page ctx)
     :route-name ::counts]
    ["/css/*path"                 :get (partial public-file
                                                "text/css; charset=utf-8"
                                                "css")
     :route-name ::css]
    ["/js/*path"                  :get (partial public-file
                                                "text/javascript; charset=utf-8"
                                                "js")
     :route-name ::js]})
