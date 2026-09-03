(ns user
  "Dev entry point: a ctx against the dev corpus (run dev/encode.sh once)."
  (:require [dk.cst.corpus-probe.corpus :as corpus]
            [dk.cst.corpus-probe.cqp :as cqp]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.search :as search]
            [dk.cst.corpus-probe.tools :as tools]))

(def ctx
  {:registry (str (System/getProperty "user.dir") "/dev/corpus/registry")})

(comment
  (cqp/version! ctx)
  ;; => "CQP version 3.5.0"

  (corpus/corpora ctx)
  (corpus/attributes! ctx "PROBE")
  (corpus/info! ctx "TALER")
  (mapv #(corpus/overview! ctx %) (corpus/corpora ctx))
  (tools/describe-corpus! ctx "VISER")

  (search/kwic! ctx "PROBE" "\"hund.*\" %c")
  (search/concordance! ctx ["PROBE" "VISER" "TALER"] "[word = \".*en\" %c]"
                       {:page-size 5})
  (search/concordance! ctx ["TALER" "PROBE"] "[lemma = \"hund\"]")
  (search/kwic! ctx "PROBE" (query/simple->cqp "hund" {:prefix? true}))
  (search/frequencies! ctx "PROBE" "[pos = \"N.*\"]" :lemma)
  (search/frequency-table! ctx ["PROBE" "VISER" "TALER"] "[pos = \"N.*\"]"
                           :lemma)
  (search/frequency-table! ctx ["PROBE"] "" :lemma)
  (tools/lexicon! ctx "TALER" :word)
  #_.)
