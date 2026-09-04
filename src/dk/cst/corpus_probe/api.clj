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
            [dk.cst.corpus-probe.views.layout :as layout]
            [dk.cst.corpus-probe.views.page :as page]
            [dk.cst.corpus-probe.views.app :as app-views]
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

(defn request-language
  "The UI language `request` is served in: the language it remembers
  choosing, else the best match for its `Accept-Language` header, else
  Danish (see dk.cst.corpus-probe.i18n/default-language).

  Not the URL: which language a reader reads in is their preference, not a
  property of the page, so the same URL serves either and a shared link
  does not impose the sharer's language on whoever opens it."
  [request]
  (or (cookie-value (get-in request [:headers "cookie"]) :lang)
      (accept-language (get-in request [:headers "accept-language"]))
      i18n/default-language))

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
  banner and contentinfo rather than part of the main content. The
  masthead has a mount point of its own because its links carry the
  current search, so a routed navigation must re-render it or it goes
  stale; the plain <div> around it scopes no landmark, so the <header>
  inside is still the document's banner. Every page mounts the client, so
  every page routes: the client swaps those two regions rather than
  reloading, and the server keeps serving the same complete page for
  anything that does not run it. The document shell and the bootstrap
  script are emitted as strings rather than through Replicant, so the
  transit payload's double quotes are not mangled by the renderer bug
  (see `correct-quote-escaping`). The document language is the UI
  language; corpus text carries its own `lang`."
  [{:keys [lang path title body nav payload] :as opts}]
  (str "<!DOCTYPE html>"
       "<html lang=\"" lang "\"><head>"
       "<meta charset=\"utf-8\">"
       "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
       (correct-quote-escaping
        (replicant/render [:meta {:name    "description"
                                  :content (i18n/tr lang :description)}]))
       (correct-quote-escaping (replicant/render [:title title]))
       "<link rel=\"stylesheet\" href=\"/css/style.css\">"
       "</head><body>"
       (correct-quote-escaping (replicant/render (layout/skip-link lang)))
       "<div id=\"masthead\">"
       (correct-quote-escaping
        (replicant/render (layout/site-header lang path nav)))
       "</div>"
       "<div id=\"app\">"
       (correct-quote-escaping (replicant/render body))
       "</div>"
       (correct-quote-escaping (replicant/render (layout/site-footer lang)))
       (when payload
         (str "<script type=\"" transit-type "\" id=\"bootstrap\">"
              (script-safe payload)
              "</script>"))
       "<script defer src=\"/js/main.js\"></script>"
       "</body></html>"))

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

(defn frequency-title
  "The document title of the frequency view for `params` in language
  `lang`: what was counted, in which corpora, within the metadata filter,
  and by what.

  A frequency result counts values rather than hits, so it cannot borrow
  the concordance's title: there is no hit count to report."
  [lang {:keys [q corpus attr] :as params}]
  (page-title (if (str/blank? q) (i18n/tr lang :all-tokens) q)
              (when (seq corpus) (page/corpora-phrase lang corpus))
              (page/filter-phrase (filter-params params))
              (str (i18n/tr lang :by) " " attr)
              (i18n/tr lang :frequencies)))

(defn result-title
  "The document title of the search described by `params` in language
  `lang`, shown in `view` with `result`: each view names what it shows,
  since a full page load announces the title and nothing else."
  [lang view params result]
  (if (= :frequencies view)
    (frequency-title lang params)
    (search-title lang params result)))

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
  filter), for linking the views of the same hits. The interface language
  is not among them: it is the reader's preference, not part of the
  search."
  [params]
  (into (select-keys params [:corpus :q :mode :ci :prefix :suffix])
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
  is nothing to search for.

  Simple is the default and CQP mode is opt-in: a request naming no mode
  is read as a plain word search, since CQP mode answers a bare word with
  a parse error naming a corpus the reader never mentioned. Every URL the
  form builds names its mode, so only a hand-written one changes meaning."
  [{:keys [q mode ci prefix suffix]}]
  (when-not (str/blank? q)
    (if (= mode "cqp")
      q
      (query/simple->cqp q {:case-insensitive? (some? ci)
                            :prefix?           (some? prefix)
                            :suffix?           (some? suffix)}))))

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
  (let [named (corpora-param (:corpus params))]
    (if (or (seq named) (contains? params :scope))
      named
      (readable-corpora ctx corpora))))

(defn search-request
  "What `request` asks of `ctx`: its scalar query params, the registry's
  corpora, the corpus names selected, those split into the `known` and the
  `unknown` (see `split-known`) and the CQP query the params compile to
  (see `->cqp`). A request naming no corpus searches every readable one
  (see `selected-corpora`).

  Every handler that answers a search starts from this."
  [ctx request]
  (let [params   (scalar-params (:query-params request))
        corpora  (corpus/corpora ctx)
        selected (selected-corpora ctx corpora params)
        [known unknown] (split-known corpora selected)]
    {:params   params
     :corpora  corpora
     :selected selected
     :known    known
     :unknown  unknown
     :cqp      (->cqp params)}))

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

(def result-views
  "The views a search result can be shown in, in display order: the
  keyword naming each, its `view` param value and the dictionary key
  heading it.

  A frequency table is not another page, it is the same search counted
  rather than listed, so it is a view of the result rather than a place of
  its own."
  [[:kwic "kwic" :concordance]
   [:frequencies "frequencies" :frequencies]])

(defn view-param
  "The result view named by the `view` query param value `v`: the
  concordance for anything that does not name another view."
  [v]
  (or (some (fn [[k value _]] (when (= v value) k)) result-views) :kwic))

(defn view-hrefs
  "Each result view of the search described by `params`, for the switch at
  the top of the results region: [view-keyword label-key url], in display
  order.

  Every view of one search shares its URL but for the `view` param, so
  moving between them keeps the query, the corpora and the filter by
  construction rather than by carrying them across."
  [params]
  (for [[k value label] result-views]
    [k label (str "/?" (query-string (assoc (search-params params)
                                            :view  value
                                            :attr  (:attr params)
                                            :sort  (:sort params)))
                  page/results-fragment)]))

(defn search-view-data
  "The data dk.cst.corpus-probe.views.app/search-view renders one
  search page for `request` against `ctx` from: the state of the form, the
  outcome of the search when the params describe one, and the links out
  of it.

  One search, two views: the `view` param decides whether its hits are
  listed as a concordance or counted as a frequency table, and each view
  contributes only the controls and links it has (a sort and pagination
  for the concordance, a grouping for the table).

  The same map is embedded as transit for the client to take over from,
  so it holds corpus overviews only: the full registry maps carry
  absolute server paths and stay here."
  [ctx request]
  (let [{:keys [params corpora selected known unknown cqp]}
        (search-request ctx request)
        lang    (request-language request)
        view    (view-param (:view params))
        attr    (attr-param (:attr params))
        page-n  (page-param (:page params))
        freq?   (= :frequencies view)
        outcome (cond
                  (and freq? (or cqp (seq known) (seq unknown)))
                  (frequency-outcome! ctx known unknown cqp attr
                                      {:filter (filter-params params)})

                  cqp
                  (search-outcome! ctx known unknown cqp
                                   {:page   page-n
                                    :sort   (:sort params)
                                    :filter (filter-params params)}))
        pages   (some-> outcome :result :pages)
        params* (assoc params :corpus selected :lang lang :attr attr)]
    (cond->
     {:lang            lang
      :view            view
      :folders         (corpus-tree! ctx corpora)
      :filter-controls (filter-controls! ctx known params)
      :params          params*
      :result          (:result outcome)
      :error           (:error outcome)
      :view-hrefs      (view-hrefs params*)
      :langs           (into {}
                             (map (juxt identity
                                        #(content-lang corpora %)))
                             selected)}
      freq?
      (assoc :attrs        (attr-options! ctx known)
             :export-hrefs (when (:result outcome)
                             (export-hrefs "/export/frequencies"
                                           (assoc (search-params params*)
                                                  :attr attr))))

      (not freq?)
      (assoc :sort-modes   (mapv (fn [[value label _]] [value label])
                                 query/sort-modes)
             :export-limit export/hit-limit
             :export-hrefs (when (:result outcome)
                             (export-hrefs "/export/kwic"
                                           (assoc (search-params params*)
                                                  :sort (:sort params))))
             :prev-href    (when (pos? page-n)
                             (page-href params* (dec page-n)))
             :next-href    (when (and pages (< (inc page-n) pages))
                             (page-href params* (inc page-n)))))))

(defn nav-hrefs
  "The URL of each top-level page for `params`.

  No URL names a language: which language a reader reads in is their own
  preference, so none of these carries one. The search keeps the current
  query, so returning to it from the corpus index does not lose it. The
  frequency table is not here: it is a view of a search result, reached by
  the switch at the top of the results region (see `view-hrefs`)."
  [params]
  (let [search (search-params params)]
    {:search          (str "/?" (query-string search) page/results-fragment)
     :corpora-heading "/corpora"}))

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
  is none."
  [ctx request]
  (let [data (assoc (search-view-data ctx request) :route :search)]
    (page-response request
                   (result-title (:lang data) (:view data) (:params data)
                                 (:result data))
                   data
                   (:params data))))

(defn frequencies-page
  "Redirect a frequency-page `request` to the same search shown in its
  frequency view.

  The frequency table used to be a page of its own; it is now a view of a
  search result, so the old URL names the same thing and says so with a
  permanent redirect rather than growing a second way to reach it."
  [_ctx request]
  (let [params (scalar-params (:query-params request))]
    {:status  301
     :headers {"Location" (str "/?" (query-string
                                     (assoc (search-params params)
                                            :view "frequencies"
                                            :attr (attr-param (:attr params))))
                              page/results-fragment)}
     :body    ""}))

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
      (let [table (frequency/frequency-table!
                   ctx known (or cqp "") (attr-param (:attr params))
                   {:filter (filter-params params)})]
        (export-response format "frequencies" :tokens table
                         (export/frequency-table table))))))

(defn corpora-page
  "Handle the corpus index `request` against `ctx`: every registry corpus,
  summarized and grouped by the configured folder tree."
  [ctx request]
  (let [lang (request-language request)]
    (page-response request
                   (page-title (i18n/tr lang :corpora-heading))
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
        named     (corpora-param (:corpus (:query-params request)))
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
    ["/preferences"   :post (partial preferences-page ctx)
     :route-name ::preferences]
    ["/corpora"       :get (partial corpora-page ctx) :route-name ::corpora]
    ["/corpus/:id"    :get (partial corpus-page ctx)  :route-name ::corpus]
    ["/api/context"   :get (partial context-page ctx) :route-name ::context]
    ["/api/filters"   :get (partial filters-page ctx) :route-name ::filters]
    ["/css/style.css" :get stylesheet                 :route-name ::stylesheet]
    ["/js/*path"      :get js-file                    :route-name ::js]})
