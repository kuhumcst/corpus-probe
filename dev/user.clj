(ns user
  "The development environment in one JVM: `(start!)` runs the web server
  and a shadow-cljs watch.

  Loaded by any JVM started with the :dev alias. Run dev/encode.sh once
  first; the dev corpora are not in the repository."
  (:require [dk.cst.corpus-probe.cwb :as cwb]
            [dk.cst.corpus-probe.cwb.corpus :as corpus]
            [dk.cst.corpus-probe.cwb.registry :as registry]
            [dk.cst.corpus-probe.cwb.tools :as tools]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.search :as search]
            [dk.cst.corpus-probe.search.frequency :as frequency]
            [dk.cst.corpus-probe.server :as server]
            [shadow.cljs.devtools.api :as shadow]
            [shadow.cljs.devtools.config :as shadow.config]
            [shadow.cljs.devtools.server :as shadow.server]))

(def overrides
  "What a development machine adds to resources/config.edn."
  ;; the watch's origin, read from shadow-cljs.edn so the two cannot drift
  {:dev-client (str "ws://localhost:"
                    (get-in (shadow.config/load-cljs-edn) [:http :port]))})

(def config
  "The settings this environment runs on. The CWB layer takes them as its
  `ctx`, so the calls below pass this."
  (merge (server/read-config) overrides))

(defn watch!
  "Start the shadow-cljs watch of the :app build in this JVM."
  []
  ;; shadow-cljs refuses to run twice in one project, so a watch started
  ;; outside this JVM is left where it is and its complaint returned
  (try
    (shadow.server/start!)
    (shadow/watch :app)
    (catch Exception e
      (ex-message e))))

(defn start!
  "Start the web server and the watch, and say where the app is. Both are
  no-ops when they already run."
  []
  (server/start! config)
  {:app (str "http://localhost:" (:port config)) :watch (watch!)})

(defn stop!
  "Stop the web server and the watch."
  []
  (server/stop!)
  (shadow.server/stop!))

(defn restart!
  "Stop the web server and start it again, leaving the watch as it is.
  Needed after reloading a handler namespace: the route table holds the
  handlers it was built with."
  []
  (server/stop!)
  (start!))

(comment
  (start!)
  (restart!)
  (stop!)

  config
  (server/read-config)

  (cwb/version! config)
  ;; => "CQP version 3.5.0"

  (registry/entries config)
  (corpus/attributes! config "PROBE")
  (corpus/info! config "TALER")
  (mapv #(corpus/overview! config %) (registry/entries config))
  (tools/describe-corpus! config "VISER")

  (search/kwic! config "PROBE" "\"hund.*\" %c")
  (search/concordance! config ["PROBE" "VISER" "TALER"] "[word = \".*en\" %c]"
                       {:page-size 5})
  (search/concordance! config ["TALER" "PROBE"] "[lemma = \"hund\"]")
  (search/kwic! config "PROBE"
                (query/->cqp (query/of {:q "hund" :match "prefix"})))
  (frequency/frequencies! config "PROBE" "[pos = \"N.*\"]" :lemma)
  (frequency/frequency-table! config ["PROBE" "VISER" "TALER"]
                              "[pos = \"N.*\"]" :lemma)
  (frequency/frequency-table! config ["PROBE"] "" :lemma)
  (tools/lexicon! config "TALER" :word)
  #_.)
