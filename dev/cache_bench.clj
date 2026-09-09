(ns cache-bench
  "REPL benchmark of the saved query result cache
  (dk.cst.corpus-probe.search.cache): what it is for, measured against the
  two-million-token STOR corpus that dev/encode-big.sh builds. The dev
  corpora are 42 to 48 tokens, where every query is instant and a
  measurement proves nothing."
  (:require [babashka.fs :as fs]
            [dk.cst.corpus-probe.search :as search]
            [dk.cst.corpus-probe.search.cache :as cache]
            [dk.cst.corpus-probe.server :as server]))

(def config
  "The settings the app runs on, which the CWB layer takes as its `ctx`."
  (server/read-config))

(def big-config
  "The same over the STOR corpus, which dev/encode-big.sh writes beside the
  registry of the others, as the app runs a batch that sorts (see
  dk.cst.corpus-probe.cwb/running-ctx)."
  ;; no :cache-dir: the measurements add one where they want a result saved,
  ;; and the timeout goes past what config.edn deliberately refuses
  (-> config
      (dissoc :cache-dir)
      (assoc :registry (str (:registry config) "-big")
             :query-timeout-ms 900000)))

(defn page-ms!
  "How many milliseconds one page of 25 hits, `page` of every match in
  STOR under `sort`, takes via `ctx`."
  [ctx sort page]
  (let [started (System/nanoTime)]
    (search/kwic! ctx "STOR" "[]" {:sort sort
                                   :rows [(* page 25) (+ 24 (* page 25))]})
    (quot (- (System/nanoTime) started) 1000000)))

(comment
  ;; the name moves with the corpus build stamp, so it is not written down
  (cache/result-name config "VISER" "[pos=\"N.*\"]" {:sort "word"})

  (cache/stored? config "VISER" (cache/result-name config "VISER" "[]" {}))
  ;; => false

  (cache/count! config "VISER" "[]" {} #(do (Thread/sleep 1000) 48))
  ;; => 48   (a second the first time, instant after)

  (cache/forget-counts!)

  (cache/share! ::probe (fn [] :once))
  ;; => :once

  (cache/excess-files (cache/max-bytes config) (cache/result-files config))
  ;; => []

  (cache/reap! config)
  ;; => 0

  ;; Each number is one page of 25 hits out of two million matches.
  (page-ms! big-config "left" 100)
  (page-ms! (assoc big-config :cache-dir (str (fs/create-temp-dir)))
            "left" 100)

  ;;   sort mode | uncached page | cached page | save file
  ;;   corpus    |         77 ms |       16 ms |     16 MB
  ;;   word      |       3144 ms |       20 ms |     24 MB
  ;;   left      |      18600 ms |       26 ms |     24 MB
  ;;
  ;; And at the size of the largest corpus at KU, built with
  ;; TOKENS=64600000 dev/encode-big.sh. These are the figures
  ;; :query-timeout-ms is set against: neither locale-aware sort can
  ;; finish a whole corpus of this size inside five minutes, which is
  ;; deliberate, and a realistic query sorts a few percent of it.
  ;;
  ;;   sort mode | uncached page | cached page | save file
  ;;   count     |       2400 ms |             |
  ;;   corpus    |       2300 ms |      200 ms |    517 MB
  ;;   word      |     649600 ms |             |    775 MB
  ;;   left      |    >800000 ms |             |
  #_.)
