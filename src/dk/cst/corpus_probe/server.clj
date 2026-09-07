(ns dk.cst.corpus-probe.server
  "The HTTP server: configuration, the route table with the handlers
  behind it, start/stop and the main entry point.

  The handlers of a search, of the corpus pages and of the exports are
  the server's helpers; the documents, the preferences and the compiled
  client assets are served from here. Where each of these is, and how a
  search is spelt as a URL, is dk.cst.corpus-probe.url's.

  Startup vets the installation (see dk.cst.corpus-probe.server.vet):
  the CWB programs and the sort collation before the port is bound, the
  registry once it is open, since reading every corpus can be slow on a
  large or ailing one. None of it stops the server."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [dk.cst.corpus-probe.docs :as docs]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.search.cache :as cache]
            [dk.cst.corpus-probe.server.corpora :as corpora]
            [dk.cst.corpus-probe.server.export :as export]
            [dk.cst.corpus-probe.server.request :as request]
            [dk.cst.corpus-probe.server.response :as response]
            [dk.cst.corpus-probe.server.search :as search]
            [dk.cst.corpus-probe.server.vet :as vet]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views :as views]
            [io.pedestal.connector :as conn]
            [io.pedestal.http.http-kit :as http-kit]
            [io.pedestal.interceptor :as interceptor]
            [taoensso.telemere :as t])
  (:gen-class))

(defn content-security-policy
  "A single-origin CSP for `config`: the app serves its own script, style
  and assets, so host-based `'self'` replaces Pedestal's default
  nonce-based policy (which blocks a plain script tag). The page has no
  inline scripts or styles, so `'unsafe-inline'` is omitted.

  `:dev-client`, the origin of a shadow-cljs watch, widens it by exactly
  what such a watch needs: `eval` for the dev module loader, and a socket
  to the watch itself, which is where recompiled code is pushed from and
  is a different origin from this server. Both are concessions no
  deployment should make, so a configuration has to ask for them: without
  one the policy is the strict one, which is what ships."
  [{:keys [dev-client]}]
  (str "default-src 'self'; "
       "script-src 'self'" (when dev-client " 'unsafe-eval'") "; "
       (when dev-client (str "connect-src 'self' " dev-client "; "))
       "style-src 'self'; "
       "img-src 'self' data:"))

(defn csp-interceptor
  "Overrides the Content-Security-Policy set by the default interceptors
  with the one `config` describes. Placed first in the chain so its
  :leave runs last and wins."
  [config]
  (let [policy (content-security-policy config)]
    (interceptor/interceptor
     {:name  ::csp
      :leave (fn [ctx]
               (assoc-in ctx [:response :headers "Content-Security-Policy"]
                         policy))})))

(def config-property
  "The system property naming a configuration file to read on top of the
  built-in one. The environment variable CORPUS_PROBE_CONFIG does the
  same, for a service manager that sets no properties."
  "corpus-probe.config")

(defn config-file
  "The configuration file to read on top of the one on the classpath, from
  the `config-property` system property or the CORPUS_PROBE_CONFIG
  environment variable; nil when neither names one.

  The property wins, so one run started by hand can override what a
  service manager sets for every run."
  []
  (or (not-empty (System/getProperty config-property))
      (not-empty (System/getenv "CORPUS_PROBE_CONFIG"))))

(defn read-config
  "Read config.edn from the classpath, merge the `config-file` over it
  when there is one, and resolve the :registry and :cache-dir paths to
  absolute ones so cqp finds them regardless of working directory.

  The merge is shallow, so an installation's own file need only carry what
  it changes: the registry it serves, where the query result cache goes
  and how large it may grow, the timeouts. What it leaves out stays as the
  built-in file has it, the :folders tree above all, which describes the
  corpora rather than the machine.

  Without this there is nowhere to put any of that. config.edn is read
  from the classpath, so in a packaged jar it is inside the jar, and every
  setting would be fixed at build time. A file that is named but cannot be
  read stops the server rather than being passed over: a configuration
  silently ignored is the failure this exists to prevent."
  []
  (let [built-in (edn/read-string (slurp (io/resource "config.edn")))
        path     (config-file)
        override (when path
                   (try
                     (edn/read-string (slurp (io/file path)))
                     (catch Exception e
                       (throw (ex-info "Cannot read the configuration file"
                                       {:config-file path} e)))))
        config   (merge built-in override)
        absolute #(.getAbsolutePath (io/file %))]
    (cond-> (update config :registry absolute)
      (:cache-dir config) (update :cache-dir absolute)
      path                (assoc :config-file path))))

(defn serve-document
  "Handle a `request` for the document called `name` (see
  dk.cst.corpus-probe.docs): the frontpage, where the app says what it
  is and where a reader goes from here, the CQP guide or the glossary.
  Served in the first language the request reads that the document has,
  and titled as the document titles itself."
  [_ctx name request]
  (let [langs (request/request-languages request)
        data  {:route :document
               :lang  (first (filter i18n/supported? langs))
               :data  {:body (docs/document name langs)}}]
    (response/page-response request (views/title data) data)))

(defn serve-preferences
  "Store the settings a reader chose with `request` and send them back
  where they were.

  A preference is state, so it is set with a POST and answered with a
  redirect: the page they return to is the one they were reading, with
  their choice applied, and a refresh does not re-submit the form they
  came from. Cookies are the whole persistence (see
  dk.cst.corpus-probe.server.request/preference-cookies): no URL names a
  preference, so a link can be shared without imposing the sharer's
  settings on whoever opens it."
  [_ctx request]
  (let [params  (:form-params request)
        cookies (request/preference-cookies params)]
    {:status  303
     :headers (cond-> {"Location" (request/safe-return (:return params))}
                (seq cookies) (assoc "Set-Cookie" cookies))
     :body    ""}))

(defn serve-file
  "Serve the file under public/`dir` named by the splat `:path` of
  `request` as `content-type`: a stylesheet, or a compiled client asset.

  Rejects `..` segments directly: `io/resource` follows them out of the
  directory, so a normalising router is not relied on as the only guard."
  [content-type dir request]
  (let [path (get-in request [:path-params :path])]
    (if-let [resource (and (not (str/includes? path ".."))
                           (io/resource (str "public/" dir "/" path)))]
      (response/resource-response content-type resource)
      response/not-found)))

(defn routes
  "The route table, with handlers closed over `ctx`."
  [ctx]
  #{[url/home                     :get (partial serve-document ctx "frontpage")
     :route-name ::home]
    [url/glossary                 :get (partial serve-document ctx "glossary")
     :route-name ::glossary]
    [url/cqp-guide                :get (partial serve-document ctx "cqp-guide")
     :route-name ::cqp-guide]
    [url/search                   :get (partial search/serve-search ctx)
     :route-name ::search]
    [(str url/search "/:file")    :get (partial export/serve-export ctx)
     :route-name ::export]
    [url/preferences              :post (partial serve-preferences ctx)
     :route-name ::preferences]
    [url/corpora                  :get (partial corpora/serve-corpora ctx)
     :route-name ::corpora]
    [(str url/corpora "/:id")     :get (partial corpora/serve-corpus ctx)
     :route-name ::corpus]
    [(str url/corpora "/:id/text") :get (partial corpora/serve-text ctx)
     :route-name ::text]
    [url/context-api              :get (partial search/serve-context ctx)
     :route-name ::context]
    [url/filters-api              :get (partial search/serve-filters ctx)
     :route-name ::filters]
    [url/counts-api               :get (partial search/serve-counts ctx)
     :route-name ::counts]
    ["/css/*path"                 :get (partial serve-file
                                                "text/css; charset=utf-8"
                                                "css")
     :route-name ::css]
    ["/js/*path"                  :get (partial serve-file
                                                "text/javascript; charset=utf-8"
                                                "js")
     :route-name ::js]})

(defonce ^{:doc "The running Pedestal connector, nil when stopped."}
  server
  (atom nil))

(defn start!
  "Start the web server from `config` (default: config.edn); no-op when
  already running. Returns the connector.

  Vets the CWB programs, the sort collation and the query result cache
  first, so a broken installation is in the log before the port is bound,
  then vets the registry in the background, since reading every corpus of
  a large one takes a while and nothing it finds stops the server. A
  cache that cannot be written is dropped from the running configuration
  rather than left to fail every search, and whatever an earlier run left
  in it is swept once before the first request rather than after it."
  ([]
   (start! (read-config)))
  ([{:keys [port] :as config}]
   (or @server
       (do
         (vet/tool-problems! config)
         (vet/collation-problems! config)
         ;; the folder tree is long and says nothing about the
         ;; installation; everything else is what an operator needs to
         ;; confirm which config.edn this process actually read
         (t/event! ::configured
                   {:data (assoc (dissoc config :folders)
                                 :java (System/getProperty "java.version"))})
         (let [config    (cond-> config
                           (some #{:cache-unusable}
                                 (vet/cache-problems! config))
                           (dissoc :cache-dir))
               ;; reaping otherwise waits for the first search, so
               ;; whatever a crash left behind sits there until then
               _         (t/catch->error! {:id ::reaping-failed :catch-val nil}
                           (cache/reap! config))
               connector (-> (conn/default-connector-map port)
                             (conn/with-default-interceptors)
                             (update :interceptors
                                     #(into [(csp-interceptor config)] %))
                             (conn/with-routes (routes config))
                             (http-kit/create-connector nil)
                             (conn/start!))]
           ;; nothing waits on the vetting, so its own failure has to be
           ;; logged where it happens or it is lost
           (future (t/catch->error! {:id ::registry-vetting-failed
                                     :catch-val nil}
                                    (vet/registry-problems! config)))
           (reset! server connector))))))

(defn stop!
  "Stop the web server when it is running."
  []
  (when-let [s @server]
    (conn/stop! s)
    (reset! server nil)))

(defn -main
  "Start the server from the configuration it reads (see `read-config`)
  and say where it listens; `_args` are ignored."
  [& _args]
  (let [{:keys [port] :as config} (read-config)]
    (start! config)
    (println (str "corpus-probe running on http://localhost:" port))))
