(ns dk.cst.corpus-probe.api
  "HTTP routes and handlers: the server-rendered search page, the bootstrap
  payload the client takes over from, and the compiled client assets.

  Responses are rendered from the shared .cljc views with Replicant's string
  renderer, so the client renders identical markup. The same view data is
  embedded as transit for the client. Hostile corpus content survives the
  round trip because each channel is protected: `correct-quote-escaping`
  fixes the SSR body, transit-JSON escapes true control bytes (a carriage
  return) in the payload, and `script-safe` escapes `<` (which transit passes
  through verbatim) so a token containing `</script>` cannot break out."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cognitect.transit :as transit]
            [dk.cst.corpus-probe.corpus :as corpus]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.search :as search]
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
  "The document title for `params`: the query and corpus when a search was
  made, so tabs and bookmarks are meaningful, else the app name."
  [{:keys [q corpus]}]
  (if (str/blank? q)
    "corpus-probe"
    (str q " · " corpus " — corpus-probe")))

(defn render-page
  "The complete HTML document for `view-data`: the server-rendered page body
  in #app, the same data embedded for the client in the #bootstrap script,
  and the client script.

  The document shell and the bootstrap script are emitted as strings rather
  than through Replicant, so the transit payload's double quotes are not
  mangled by the renderer bug (see `correct-quote-escaping`). The document
  language is the UI language (English); the corpus text carries its own
  `lang` inside the concordance."
  [view-data]
  (str "<!DOCTYPE html>"
       "<html lang=\"en\"><head>"
       "<meta charset=\"utf-8\">"
       "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
       "<meta name=\"description\" "
       "content=\"Search CWB corpora and read KWIC concordances.\">"
       (correct-quote-escaping
        (replicant/render [:title (page-title (:params view-data))]))
       "<link rel=\"stylesheet\" href=\"/css/style.css\">"
       "</head><body>"
       "<div id=\"app\">"
       (correct-quote-escaping (replicant/render (page/app-view view-data)))
       "</div>"
       "<script type=\"application/transit+json\" id=\"bootstrap\">"
       (script-safe (->transit view-data))
       "</script>"
       "<script defer src=\"/js/main.js\"></script>"
       "</body></html>"))

(defn html-response
  "A complete HTML page response for `view-data`."
  [view-data]
  {:status  200
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body    (render-page view-data)})

(defn query-string
  "Encode map `m` as a URL query string, skipping nil values."
  [m]
  (->> (remove (comp nil? val) m)
       (map (fn [[k v]]
              (str (name k) "=" (URLEncoder/encode (str v) "UTF-8"))))
       (str/join "&")))

(defn page-href
  "The URL of page `page` of the search described by `params`.

  Drops `expand`, which names corpus positions on the current page and does
  not carry to another page's hits."
  [params page]
  (str "/?" (query-string (assoc (dissoc params :expand) :page page))))

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

(defn page-count
  "The number of pages a `result` of `size` hits spans."
  [{:keys [size page-size]}]
  (max 1 (long (Math/ceil (/ size (double page-size))))))

(defn content-lang
  "The language code of the corpus named `corpus` among `corpora`, when the
  registry records a plausible one (a two- or three-letter code)."
  [corpora corpus]
  (some (fn [{:keys [id language]}]
          (when (and (= corpus (str/upper-case id))
                     (re-matches #"[a-z]{2,3}" (str language)))
            language))
        corpora))

(defn search-page
  "Handle a search-page `request` against `ctx`: render the form, and when
  the query params describe a search, its KWIC result or CQP error."
  [ctx request]
  (let [params  (:query-params request)
        corpora (corpus/corpora ctx)
        corpus  (or (:corpus params) (some-> (first corpora) :id str/upper-case))
        cqp     (->cqp params)
        page-n  (or (some-> (:page params) parse-long) 0)
        outcome (when (and corpus cqp)
                  (try
                    (let [result (search/kwic! ctx corpus cqp
                                               {:page page-n
                                                :sort (:sort params)})]
                      {:result (assoc result :pages (page-count result))})
                    (catch Exception e
                      {:error (or (:error (ex-data e))
                                  {:message (ex-message e)})})))
        pages   (some-> outcome :result :pages)]
    (html-response
     ;; the page (and the embedded payload) exposes only corpus :id; the full
     ;; registry maps carry absolute server paths and stay server-side
     {:corpora      (mapv #(select-keys % [:id]) corpora)
      :sort-modes   (mapv (fn [[value label _]] [value label]) query/sort-modes)
      :params       (assoc params :corpus corpus)
      :result       (:result outcome)
      :error        (:error outcome)
      :content-lang (content-lang corpora corpus)
      :prev-href    (when (pos? page-n) (page-href params (dec page-n)))
      :next-href    (when (and pages (< (inc page-n) pages))
                      (page-href params (inc page-n)))})))

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
              result (search/kwic! ctx corpus q {:context      expanded-context
                                                 :page-size    1
                                                 :struct-attrs []})]
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
    ["/api/context"   :get (partial context-page ctx) :route-name ::context]
    ["/css/style.css" :get stylesheet                 :route-name ::stylesheet]
    ["/js/*path"      :get js-file                    :route-name ::js]})
