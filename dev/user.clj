(ns user
  "Dev entry point: a ctx against the dev corpus (run dev/encode.sh once)."
  (:require [dk.cst.corpus-probe.corpus :as corpus]
            [dk.cst.corpus-probe.cqp :as cqp]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.search :as search]))

(def ctx
  {:registry (str (System/getProperty "user.dir") "/dev/corpus/registry")})

(comment
  (cqp/version! ctx)
  ;; => "CQP version 3.5.0"

  (corpus/corpora ctx)
  (corpus/attributes! ctx "PROBE")

  (search/kwic! ctx "PROBE" "\"hund.*\" %c")
  (search/kwic! ctx "PROBE" (query/simple->cqp "hund" {:prefix? true}))
  (search/frequencies! ctx "PROBE" "[pos = \"N.*\"]" :lemma)
  #_.)
