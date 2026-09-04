(ns dk.cst.corpus-probe.api
  "HTTP routes and handlers: the server-rendered search page (a concordance
  over the selected corpora), the frequency page, their TSV/CSV exports,
  the corpus index and info pages, the bootstrap payload the client takes
  over from, and the compiled client assets.

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
            [dk.cst.corpus-probe.corpus :as corpus]
            [dk.cst.corpus-probe.export :as export]
            [dk.cst.corpus-probe.frequency :as frequency]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.search :as search]
            [dk.cst.corpus-probe.tools :as tools]
            [dk.cst.corpus-probe.views.corpus :as corpus-views]
            [dk.cst.corpus-probe.views.frequencies :as freq-views]
            [dk.cst.corpus-probe.views.layout :as layout]
            [dk.cst.corpus-probe.views.page :as page]
            [replicant.string :as replicant])
  (:import [java.io ByteArrayOutputStream]
           [java.net URLEncoder]))

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
  ;; TODO: report upstream and drop this once Replicant fixes the escape.
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

(defn accept-language
  "The first language among the `Accept-Language` header value `s` that
  this app has (see dk.cst.corpus-probe.i18n/languages), by descending
  quality; nil when it names none.

  Only the primary subtag counts, so `da-DK` counts as Danish. A tag
  offered at quality 0 is refused rather than preferred (RFC 9110)."
  [s]
  (->> (str/split (str s) #",")
       (keep (fn [item]
               (let [[tag q] (str/split item #"(?i)\s*;\s*q\s*=")]
                 (when-let [lang (first (str/split (str/trim tag) #"-"))]
                   [(str/lower-case lang)
                    (or (some-> q str/trim parse-double) 1.0)]))))
       (filter (comp pos? second))
       (sort-by (comp - second))
       (some (fn [[lang _]] (when (i18n/supported? lang) lang)))))

(defn request-language
  "The UI language `request` is served in: its `lang` query param when the
  app has that language, else the best match for its `Accept-Language`
  header, else Danish (see dk.cst.corpus-probe.i18n/default-language).

  A repeated `lang` param counts by its first value, as `scalar-params`
  treats every other scalar param."
  [request]
  (let [param (get-in request [:query-params :lang])
        param (if (vector? param) (first param) param)]
    (or (when (i18n/supported? param) param)
        (accept-language (get-in request [:headers "accept-language"]))
        i18n/default-language)))

(defn alternate-links
  "The <link rel=alternate> tags naming this page in each language of
  `switch` (a map of language code to that URL), so a crawler finds the
  translations of a page it landed on."
  [switch]
  (apply str (for [[lang href] (sort switch)]
               (correct-quote-escaping
                (replicant/render [:link {:rel      "alternate"
                                          :hreflang lang
                                          :href     href}])))))

(defn document
  "The complete HTML document in UI language `lang` titled `title`: the
  bypass link, the site header with its language `switch` (see
  dk.cst.corpus-probe.views.layout/site-header), the rendered `body`
  hiccup (the page's <main>) and the site footer; a transit `payload`,
  when given, wraps the body in #app and is embedded as the #bootstrap
  script along with the client script that takes over from it.

  The masthead and the footer sit outside #app, so they are the document's
  banner and contentinfo rather than part of the main content, and the
  client never re-renders them. #app is the client's mount point, so only
  a page served the bootstrap gets one; the rest emit their <main>
  directly. The document shell and the bootstrap script are emitted as
  strings rather than through Replicant, so the transit payload's double
  quotes are not mangled by the renderer bug (see
  `correct-quote-escaping`). The document language is the UI language;
  corpus text carries its own `lang`."
  ([lang switch title body]
   (document lang switch title body nil))
  ([lang switch title body payload]
   (str "<!DOCTYPE html>"
        "<html lang=\"" lang "\"><head>"
        "<meta charset=\"utf-8\">"
        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
        (correct-quote-escaping
         (replicant/render [:meta {:name    "description"
                                   :content (i18n/tr lang :description)}]))
        (correct-quote-escaping (replicant/render [:title title]))
        (alternate-links switch)
        "<link rel=\"stylesheet\" href=\"/css/style.css\">"
        "</head><body>"
        (correct-quote-escaping (replicant/render (layout/skip-link lang)))
        (correct-quote-escaping
         (replicant/render (layout/site-header lang switch)))
        (if payload
          (str "<div id=\"app\">"
               (correct-quote-escaping (replicant/render body))
               "</div>")
          (correct-quote-escaping (replicant/render body)))
        (correct-quote-escaping (replicant/render (layout/site-footer lang)))
        (when payload
          (str "<script type=\"application/transit+json\" id=\"bootstrap\">"
               (script-safe payload)
               "</script>"
               "<script defer src=\"/js/main.js\"></script>"))
        "</body></html>")))

(defn html-response
  "A complete HTML page response with `html` as its body.

  The page is served in the language its request asks for, so it varies by
  `Accept-Language` and says so; a shared cache would otherwise hand one
  reader's language to the next."
  [html]
  {:status  200
   :headers {"Content-Type" "text/html; charset=utf-8"
             "Vary"         "Accept-Language"}
   :body    html})

(defn query-string
  "Encode map `m` as a URL query string, skipping nil values and repeating
  the key of a vector value once per element.

  A query param written without a `=` arrives under a nil key, which names
  nothing and is skipped too."
  [m]
  (->> (remove (fn [[k v]] (or (nil? k) (nil? v))) m)
       (mapcat (fn [[k v]]
                 (for [v (if (vector? v) v [v])]
                   (str (name k) "=" (URLEncoder/encode (str v) "UTF-8")))))
       (str/join "&")))

(defn language-hrefs
  "The URL of the page `request` asks for in each supported language: its
  own path and params with `lang` set to that language, so the switch in
  the site header keeps the reader where they are."
  [request]
  (into {} (for [lang i18n/languages]
             [lang (str (:uri request) "?"
                        (query-string (assoc (:query-params request)
                                             :lang lang)))])))

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

(defn filter-params
  "The metadata filter selected by `params`: a map of attribute name to
  the set of non-blank values of its `f.<attribute>` params (see
  `filter-key?`), as dk.cst.corpus-probe.search/concordance! takes it;
  empty when nothing is selected. A param naming no attribute is dropped
  like a blank value."
  [params]
  (into {} (for [[k v] params
                 :when (filter-key? k)
                 :let  [attr   (subs (name k) (count page/filter-prefix))
                        values (set (remove str/blank? (if (vector? v) v [v])))]
                 :when (and (seq attr) (seq values))]
             [(keyword attr) values])))

(defn page-param
  "The page number named by the `page` query param value `v`: the first
  page for anything that is not a non-negative integer."
  [v]
  (max 0 (or (some-> v parse-long) 0)))

(defn corpora-param
  "The corpus names selected by the `corpus` query param value `v`: a string
  (one name, or several joined by commas as in Korp URLs) or a vector of
  such strings when the param repeats. Names are uppercased and
  deduplicated; nothing is validated here, an unknown or hostile name is
  reported by the search as that corpus's error."
  [v]
  (->> (if (vector? v) v [v])
       (mapcat #(str/split (str %) #","))
       (remove str/blank?)
       (map str/upper-case)
       (distinct)
       (vec)))

(defn search-title
  "The document title of the search page for `params` in language `lang`:
  the query, how many hits it found (from `result`, when given), the
  selected corpora (`:corpus`, a vector of names, when any), the metadata
  filter and the page number when past the first; just the app name when
  nothing was searched for.

  The hit count is in the title because a full page reload announces the
  title and nothing else, so the title is where the outcome of a search
  first reaches a screen reader. It is left out when no corpus could be
  searched, since then there is no count to report."
  ([lang params]
   (search-title lang params nil))
  ([lang {:keys [q corpus] :as params} result]
   (if (str/blank? q)
     (page-title)
     (let [page-n (:page result 0)]
       (page-title q
                   ;; a search every corpus refused still has a result, of
                   ;; size 0; titling that "0 hits" reports an answer the
                   ;; search never got, and contradicts the results heading
                   (when (page/searched? result)
                     (page/hits-phrase lang (:size result)))
                   (when (seq corpus) (page/corpora-phrase lang corpus))
                   (page/filter-phrase (filter-params params))
                   (when (pos? page-n)
                     (str (i18n/tr lang :page) " " (inc page-n))))))))

(defn page-href
  "The URL of page `page` of the search described by `params`.

  Ends in the results fragment, so a page turn lands on the hits rather
  than at the top of the query form. Drops `expand`, which names corpus
  positions on the current page and does not carry to another page's
  hits."
  [params page]
  (str "/?" (query-string (assoc (dissoc params :expand) :page page))
       page/results-fragment))

(defn search-params
  "The `params` that identify a search (its corpora, query, metadata
  filter and UI language), for linking the concordance and frequency views
  of the same hits."
  [params]
  (into (select-keys params [:corpus :q :mode :ci :prefix :suffix :lang])
        (filter (comp filter-key? key))
        params))

(defn export-hrefs
  "The URLs of the TSV and CSV exports at `path` of the search described
  by `params`."
  [path params]
  (into {} (for [format (keys export/formats)]
             [(keyword format)
              (str path "?" (query-string (assoc params :format format)))])))

(defn attr-param
  "The grouping attribute named by the `attr` query param value `v`,
  defaulting to word, the one attribute every corpus has."
  [v]
  (if (str/blank? v) "word" v))

(defn ->cqp
  "The CQP query for `params`, compiling simple-mode input; nil when there
  is nothing to search for."
  [{:keys [q mode ci prefix suffix]}]
  (when-not (str/blank? q)
    (if (= mode "simple")
      (query/simple->cqp q {:case-insensitive? (some? ci)
                            :prefix?           (some? prefix)
                            :suffix?           (some? suffix)})
      q)))

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

(defn search-outcome!
  "Search the `known` corpora for `cqp` via `ctx` with `opts` (the :page,
  :sort and :filter of dk.cst.corpus-probe.search/concordance!): {:result
  <concordance with its :pages>}, the `unknown` corpus names reported
  among its counts, or {:error ...} when no corpus was selected at all.
  Per-corpus errors travel inside the result."
  [ctx known unknown cqp opts]
  (if (and (empty? known) (empty? unknown))
    {:error {:type :no-corpus}}
    (let [result (-> (search/concordance! ctx known cqp opts)
                     (update :counts into (unknown-counts unknown)))]
      {:result (assoc (public-counts result) :pages (page-count result))})))

(defn filter-controls!
  "The metadata filter controls of the search form over the `known`
  corpora via `ctx`: the filters they offer (see
  dk.cst.corpus-probe.frequency/filter-options!) plus the `:selected`
  values of `params` (see `filter-params`)."
  [ctx known params]
  (assoc (frequency/filter-options! ctx known)
         :selected (filter-params params)))

(defn search-request
  "What `request` asks of `ctx`: its scalar query params, the registry's
  corpora, the corpus names selected, those split into the `known` and the
  `unknown` (see `split-known`) and the CQP query the params compile to
  (see `->cqp`).

  Every handler that answers a search starts from this."
  [ctx request]
  (let [params   (scalar-params (:query-params request))
        corpora  (corpus/corpora ctx)
        selected (corpora-param (:corpus params))
        [known unknown] (split-known corpora selected)]
    {:params   params
     :corpora  corpora
     :selected selected
     :known    known
     :unknown  unknown
     :cqp      (->cqp params)}))

(defn search-view-data
  "The data dk.cst.corpus-probe.views.page/app-view renders one search
  page for `request` against `ctx` from: the state of the form, the
  outcome of the search when the params describe one, and the links out
  of it.

  The same map is embedded as transit for the client to take over from,
  so it holds corpus overviews only: the full registry maps carry
  absolute server paths and stay here."
  [ctx request]
  (let [{:keys [params corpora selected known unknown cqp]}
        (search-request ctx request)
        lang    (request-language request)
        page-n  (page-param (:page params))
        outcome (when cqp
                  (search-outcome! ctx known unknown cqp
                                   {:page   page-n
                                    :sort   (:sort params)
                                    :filter (filter-params params)}))
        pages   (some-> outcome :result :pages)
        params* (assoc params :corpus selected :lang lang)]
    {:lang       lang
                   :folders    (corpus-tree! ctx corpora)
                   :filter-controls (filter-controls! ctx known params)
                   :sort-modes (mapv (fn [[value label _]] [value label])
                                     query/sort-modes)
                   :params     params*
                   :result     (:result outcome)
                   :error      (:error outcome)
                   :langs      (into {}
                                     (map (juxt identity
                                                #(content-lang corpora %)))
                                     selected)
                   :freq-href  (when cqp
                                 (str "/frequencies?"
                                      (query-string
                                       (assoc (search-params params*)
                                              :attr (attr-param nil)))
                                      page/results-fragment))
                   :export-hrefs (when (:result outcome)
                                   (export-hrefs "/export/kwic"
                                                 (assoc (search-params params*)
                                                        :sort (:sort params))))
                   :export-limit export/hit-limit
                   :prev-href  (when (pos? page-n)
                                 (page-href params* (dec page-n)))
     :next-href  (when (and pages (< (inc page-n) pages))
                   (page-href params* (inc page-n)))}))

(defn search-page
  "Handle a search-page `request` against `ctx`: render the form, and when
  the query params describe a search, its concordance or the reason there
  is none."
  [ctx request]
  (let [view-data (search-view-data ctx request)
        lang      (:lang view-data)]
    (html-response
     (document lang
               (language-hrefs request)
               (search-title lang (:params view-data) (:result view-data))
               (page/app-view view-data)
               (->transit view-data)))))

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

(defn frequency-outcome!
  "Table the `known` corpora for `cqp` (nil for the whole corpora) by
  `attr` via `ctx` with `opts` (the :filter of
  dk.cst.corpus-probe.frequency/frequency-table!): {:result ...}, the
  `unknown` corpus names reported among its counts, or {:error ...} when
  no corpus was selected at all. Per-corpus errors travel inside the
  result."
  [ctx known unknown cqp attr opts]
  (if (and (empty? known) (empty? unknown))
    {:error {:type :no-corpus}}
    {:result (-> (frequency/frequency-table! ctx known (or cqp "") attr opts)
                 (update :counts into (unknown-counts unknown))
                 (public-counts))}))

(defn frequencies-page
  "Handle a frequency page `request` against `ctx`: render the form, and
  once it has been submitted (the `attr` param is present) the breakdown
  of the query's hits, or of the whole selected corpora when the query is
  blank, by the chosen attribute."
  [ctx request]
  (let [{:keys [params corpora selected known unknown cqp]}
        (search-request ctx request)
        lang      (request-language request)
        attr      (attr-param (:attr params))
        submitted (contains? params :attr)
        outcome   (when submitted
                    (frequency-outcome! ctx known unknown cqp attr
                                        {:filter (filter-params params)}))
        params*   (assoc params :corpus selected :attr attr :lang lang)
        view-data {:lang      lang
                   :folders   (corpus-tree! ctx corpora)
                   :filter-controls (filter-controls! ctx known params)
                   :params    params*
                   :attrs     (attr-options! ctx known)
                   :result    (:result outcome)
                   :error     (:error outcome)
                   :kwic-href (when cqp
                                (str "/?"
                                     (query-string (search-params params*))
                                     page/results-fragment))
                   :export-hrefs (when (:result outcome)
                                   (export-hrefs "/export/frequencies"
                                                 (assoc (search-params params*)
                                                        :attr attr)))}]
    (html-response
     (document lang
               (language-hrefs request)
               (if submitted
                 (page-title (if cqp
                               (:q params)
                               (i18n/tr lang :all-tokens))
                             (page/corpora-phrase lang selected)
                             (page/filter-phrase (filter-params params))
                             (str (i18n/tr lang :by) " " attr)
                             (i18n/tr lang :frequencies))
                 (page-title (i18n/tr lang :frequencies)))
               (freq-views/frequencies-view view-data)))))

(defn text-response
  "A 200 response serving `text` in export `format` (a key of
  dk.cst.corpus-probe.export/formats) as a download named `name`."
  [format name text]
  {:status  200
   :headers {"Content-Type"        (:content-type (export/formats format))
             "Content-Disposition" (str "attachment; filename=\"" name "."
                                        format "\"")}
   :body    text})

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

(defn export-kwic
  "Handle a concordance export `request` against `ctx`: the hits of the
  query in the selected corpora (the first
  dk.cst.corpus-probe.export/hit-limit of them, in the requested sort) as
  a TSV or CSV download; 400 without a query, known corpora or a known
  format, or when no corpus could be searched."
  [ctx request]
  (let [{:keys [params known cqp]} (search-request ctx request)
        format (:format params)]
    (if-not (and cqp (seq known) (export/formats format))
      {:status 400 :body "bad request"}
      (let [result (search/concordance! ctx known cqp
                                        {:page      0
                                         :page-size export/hit-limit
                                         :sort      (:sort params)
                                         :filter    (filter-params params)})]
        (export-response format "kwic" :size result
                         (export/kwic-table result))))))

(defn export-frequencies
  "Handle a frequency table export `request` against `ctx`: every row of
  the breakdown of the query (or of the whole corpora) by the `attr`
  param as a TSV or CSV download; 400 without known corpora or a known
  format, or when no corpus could be counted."
  [ctx request]
  (let [{:keys [params known cqp]} (search-request ctx request)
        format (:format params)]
    (if-not (and (seq known) (export/formats format))
      {:status 400 :body "bad request"}
      (let [table (frequency/frequency-table! ctx known (or cqp "")
                                           (attr-param (:attr params))
                                           {:filter (filter-params params)})]
        (export-response format "frequencies" :tokens table
                         (export/frequency-table table))))))

(defn corpora-page
  "Handle the corpus index `request` against `ctx`: every registry corpus,
  summarized and grouped by the configured folder tree."
  [ctx request]
  (let [lang (request-language request)]
    (html-response
     (document lang
               (language-hrefs request)
               (page-title (i18n/tr lang :corpora-heading))
               (corpus-views/index-view
                lang
                {:folders (corpus-tree! ctx (corpus/corpora ctx))})))))

(defn corpus-page
  "Handle a corpus info page `request` against `ctx`, rendering the corpus
  named by the :id path parameter (case-insensitively); 404 when it is not
  a registry corpus."
  [ctx request]
  (let [lang   (request-language request)
        corpus (str/upper-case (str (get-in request [:path-params :id])))
        file   (when (query/corpus-name? corpus)
                 (corpus/registry-file ctx corpus))]
    (if-not (and file (corpus/registry-file? file))
      {:status 404 :body "not found"}
      (let [registry (corpus/read-registry file)
            outcome  (try {:stats (tools/describe-corpus! ctx corpus)
                           :info  (corpus/info! ctx corpus)}
                          (catch Exception e
                            {:error    (search/error-map e)
                             :phantom? (corpus/phantom? e)}))]
        (html-response
         (document lang
                   (language-hrefs request)
                   (page-title corpus)
                   (corpus-views/info-view
                    lang
                    (assoc outcome
                           :corpus corpus
                           :title  (not-empty (:name registry))
                           :lang   (corpus/language registry)))))))))

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

(defn context-page
  "Return the hit at the requested corpus position with wider context, as
  transit, for the client's context expansion.

  Rejects an invalid corpus or non-integer `cpos`/`matchend`, since those are
  interpolated into a CQP query (as validated integers) outside the sandbox."
  [ctx request]
  (let [{:keys [corpus cpos matchend]} (:query-params request)
        cpos*     (parse-long (str cpos))
        matchend* (parse-long (str matchend))]
    (if-not (and (query/corpus-name? corpus) cpos* matchend*)
      {:status 400 :body "bad request"}
      (try
        (let [q      (query/position-query cpos* matchend*)
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

(defn stylesheet
  "Serve the bundled stylesheet."
  [_request]
  (resource-response "text/css; charset=utf-8"
                     (io/resource "public/css/style.css")))

(defn js-file
  "Serve a compiled client asset from public/js by its splat `:path`.

  Rejects `..` segments directly: `io/resource` follows them out of the
  directory, so a normalising router is not relied on as the only guard."
  [request]
  (let [path (get-in request [:path-params :path])]
    (if-let [resource (and (not (str/includes? path ".."))
                           (io/resource (str "public/js/" path)))]
      (resource-response "text/javascript; charset=utf-8" resource)
      {:status 404 :body "not found"})))

(defn routes
  "The route table, with handlers closed over `ctx`."
  [ctx]
  #{["/"              :get (partial search-page ctx)  :route-name ::search]
    ["/frequencies"   :get (partial frequencies-page ctx)
     :route-name ::frequencies]
    ["/export/kwic"   :get (partial export-kwic ctx) :route-name ::export-kwic]
    ["/export/frequencies" :get (partial export-frequencies ctx)
     :route-name ::export-frequencies]
    ["/corpora"       :get (partial corpora-page ctx) :route-name ::corpora]
    ["/corpus/:id"    :get (partial corpus-page ctx)  :route-name ::corpus]
    ["/api/context"   :get (partial context-page ctx) :route-name ::context]
    ["/css/style.css" :get stylesheet                 :route-name ::stylesheet]
    ["/js/*path"      :get js-file                    :route-name ::js]})
