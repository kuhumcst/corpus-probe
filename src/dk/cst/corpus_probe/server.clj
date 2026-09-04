(ns dk.cst.corpus-probe.server
  "Server lifecycle: configuration, start/stop and the main entry point.

  Startup vets the installation (see dk.cst.corpus-probe.vet): the CWB
  programs and the sort collation before the port is bound, the registry
  once it is open, since reading every corpus can be slow on a large or
  ailing one. None of it stops the server."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [dk.cst.corpus-probe.cache :as cache]
            [io.pedestal.connector :as conn]
            [io.pedestal.http.http-kit :as http-kit]
            [io.pedestal.interceptor :as interceptor]
            [taoensso.telemere :as t]
            [dk.cst.corpus-probe.api :as api]
            [dk.cst.corpus-probe.vet :as vet])
  (:gen-class))

(def content-security-policy
  "A single-origin CSP: the app serves its own script, style and assets, so
  host-based `'self'` replaces Pedestal's default nonce-based policy (which
  blocks a plain script tag). The page has no inline scripts or styles, so
  `'unsafe-inline'` is omitted; `'unsafe-eval'` is the one exception the
  shadow-cljs dev module loader requires."
  (str "default-src 'self'; "
       "script-src 'self' 'unsafe-eval'; "
       "style-src 'self'; "
       "img-src 'self' data:"))

(def csp-interceptor
  "Overrides the Content-Security-Policy set by the default interceptors.
  Placed first in the chain so its :leave runs last and wins."
  (interceptor/interceptor
   {:name  ::csp
    :leave (fn [ctx]
             (assoc-in ctx [:response :headers "Content-Security-Policy"]
                       content-security-policy))}))

(defn read-config
  "Read config.edn from the classpath, resolving the :registry and
  :cache-dir paths to absolute ones so cqp finds them regardless of
  working directory."
  []
  (let [config   (edn/read-string (slurp (io/resource "config.edn")))
        absolute #(.getAbsolutePath (io/file %))]
    (cond-> (update config :registry absolute)
      (:cache-dir config) (update :cache-dir absolute))))

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
         (vet/tools! config)
         (vet/collation! config)
         ;; the folder tree is long and says nothing about the
         ;; installation; everything else is what an operator needs to
         ;; confirm which config.edn this process actually read
         (t/event! ::configured
                   {:data (assoc (dissoc config :folders)
                                 :java (System/getProperty "java.version"))})
         (let [config    (cond-> config
                           (some #{:cache-unusable} (vet/cache! config))
                           (dissoc :cache-dir))
               ;; reaping otherwise waits for the first search, so
               ;; whatever a crash left behind sits there until then
               _         (t/catch->error! {:id ::reaping-failed :catch-val nil}
                           (cache/reap! config))
               connector (-> (conn/default-connector-map port)
                             (conn/with-default-interceptors)
                             (update :interceptors #(into [csp-interceptor] %))
                             (conn/with-routes (api/routes config))
                             (http-kit/create-connector nil)
                             (conn/start!))]
           ;; nothing waits on the vetting, so its own failure has to be
           ;; logged where it happens or it is lost
           (future (t/catch->error! {:id ::registry-vetting-failed
                                     :catch-val nil}
                                    (vet/registry! config)))
           (reset! server connector))))))

(defn stop!
  "Stop the web server when it is running."
  []
  (when-let [s @server]
    (conn/stop! s)
    (reset! server nil)))

(defn -main
  [& _args]
  (let [{:keys [port] :as config} (read-config)]
    (start! config)
    (println (str "corpus-probe running on http://localhost:" port))))
