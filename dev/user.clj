(ns user
  "Dev entry point: a ctx against the dev corpus (run dev/encode.sh once)."
  (:require [dk.cst.corpus-probe.cwb :as cwb]
            [dk.cst.corpus-probe.cwb.corpus :as corpus]
            [dk.cst.corpus-probe.cwb.registry :as registry]
            [dk.cst.corpus-probe.cwb.tools :as tools]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.search :as search]
            [dk.cst.corpus-probe.search.frequency :as frequency]))

(def ctx
  {:registry (str (System/getProperty "user.dir") "/dev/corpus/registry")})

(comment
  (cwb/version! ctx)
  ;; => "CQP version 3.5.0"

  (registry/entries ctx)
  (corpus/attributes! ctx "PROBE")
  (corpus/info! ctx "TALER")
  (mapv #(corpus/overview! ctx %) (registry/entries ctx))
  (tools/describe-corpus! ctx "VISER")

  (search/kwic! ctx "PROBE" "\"hund.*\" %c")
  (search/concordance! ctx ["PROBE" "VISER" "TALER"] "[word = \".*en\" %c]"
                       {:page-size 5})
  (search/concordance! ctx ["TALER" "PROBE"] "[lemma = \"hund\"]")
  (search/kwic! ctx "PROBE"
                (query/->cqp (query/of {:q "hund" :match "prefix"})))
  (frequency/frequencies! ctx "PROBE" "[pos = \"N.*\"]" :lemma)
  (frequency/frequency-table! ctx ["PROBE" "VISER" "TALER"] "[pos = \"N.*\"]"
                              :lemma)
  (frequency/frequency-table! ctx ["PROBE"] "" :lemma)
  (tools/lexicon! ctx "TALER" :word)
  #_.)
