(ns dk.cst.corpus-probe.server
  "Server lifecycle: configuration, start/stop and the main entry point."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [io.pedestal.connector :as conn]
            [io.pedestal.http.http-kit :as http-kit]
            [dk.cst.corpus-probe.api :as api])
  (:gen-class))

(defn read-config
  "Read config.edn from the classpath, resolving the :registry path to an
  absolute one so cqp finds it regardless of working directory."
  []
  (-> (io/resource "config.edn")
      (slurp)
      (edn/read-string)
      (update :registry #(.getAbsolutePath (io/file %)))))

(defonce ^{:doc "The running Pedestal connector, nil when stopped."}
  server
  (atom nil))

(defn start!
  "Start the web server from `config` (default: config.edn); no-op when
  already running. Returns the connector."
  ([]
   (start! (read-config)))
  ([{:keys [port] :as config}]
   (or @server
       (reset! server
               (-> (conn/default-connector-map port)
                   (conn/with-default-interceptors)
                   (conn/with-routes (api/routes config))
                   (http-kit/create-connector nil)
                   (conn/start!))))))

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
