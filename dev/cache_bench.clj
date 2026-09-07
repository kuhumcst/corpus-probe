(ns cache-bench
  "REPL benchmark of the saved query result cache
  (dk.cst.corpus-probe.search.cache): what it is for, measured against the
  two-million-token STOR corpus that dev/encode-big.sh builds. The dev
  corpora are 42 to 48 tokens, where every query is instant and a
  measurement proves nothing."
  (:require [babashka.fs :as fs]
            [dk.cst.corpus-probe.search :as search]
            [dk.cst.corpus-probe.search.cache :as cache]))

(def ctx
  {:registry  (str (System/getProperty "user.dir") "/dev/corpus/registry")
   :cache-dir (str (System/getProperty "user.dir") "/dev/cache")})

(def big
  "A context over the STOR corpus, as the app runs a batch that sorts
  (see dk.cst.corpus-probe.cwb/running-ctx)."
  {:registry         (str (System/getProperty "user.dir")
                          "/dev/corpus/registry-big")
   :sort-locale      "da_DK.UTF-8"
   :query-timeout-ms 900000})

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
  (cache/result-name ctx "VISER" "[pos=\"N.*\"]" {:sort "word"})

  (cache/stored? ctx "VISER" (cache/result-name ctx "VISER" "[]" {}))
  ;; => false

  (cache/count! ctx "VISER" "[]" {} #(do (Thread/sleep 1000) 48))
  ;; => 48   (a second the first time, instant after)

  (cache/forget-counts!)

  (cache/share! ::probe (fn [] :once))
  ;; => :once

  (cache/excess-files (cache/max-bytes ctx) (cache/result-files ctx))
  ;; => []

  (cache/reap! ctx)
  ;; => 0

  ;; Each number is one page of 25 hits out of two million matches.
  (page-ms! big "left" 100)
  (page-ms! (assoc big :cache-dir (str (fs/create-temp-dir))) "left" 100)

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
