(ns dk.cst.corpus-probe.search-test
  "Integration tests for the full search round trip (milestone 1's exit
  criterion); skipped when CWB or the dev corpus is missing."
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [dk.cst.corpus-probe.cwb :as cwb]
            [dk.cst.corpus-probe.cwb.command :as command]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.search :as search]
            [dk.cst.corpus-probe.search.result :as result]
            [dk.cst.corpus-probe.test.cwb
             :refer [ctx reset-cache! when-cwb]]))

(use-fixtures :each reset-cache!)

(deftest kwic-test
  (when-cwb
   (let [{:keys [size hits] :as page}
         (search/kwic! ctx "PROBE" "\"hund.*\" %c" {:rows [0 2]})]
     (is (= 5 size))
     (is (= 3 (count hits)))
     (testing "hits carry tokens, anchors and structural metadata"
       (let [{:keys [cpos match anchors structs]} (second hits)]
         (is (= 9 cpos))
         (is (= [{:word "hund" :pos "NCSI" :lemma "hund"}] match))
         (is (= {:match 9 :matchend 9 :target nil :keyword nil} anchors))
         (is (= {:s_id "2" :text_id "t1" :text_title "Hverdag"
                 :text_year "2023"}
                structs))))
     (testing "a later row range"
       (let [page2 (search/kwic! ctx "PROBE" "\"hund.*\" %c" {:rows [3 5]})]
         (is (= 2 (count (:hits page2))))
         (is (= 34 (-> page2 :hits first :cpos))))))))

(deftest simple-search-round-trip-test
  (when-cwb
   (is (= 5 (:size (search/kwic! ctx "PROBE"
                                 (query/->cqp (query/of {:q     "hund"
                                                         :match "prefix"
                                                         :ci    "on"}))))))))

(deftest within-test
  (when-cwb
   ;; a full stop ends one sentence and Hunde opens the next
   (let [q "[word = \"\\.\"] [word = \"Hunde\"]"]
     (testing "the two words match across the boundary unless kept within
               a sentence, in the count as in the concordance"
       (is (= 1 (search/size! ctx "PROBE" q)))
       (is (= 0 (search/size! ctx "PROBE" q {:within :sentence})))
       (is (= 0 (:size (search/kwic! ctx "PROBE" q {:within :sentence})))))
     (testing "the clause names the corpus's own sentence attribute"
       (is (= (str q " within s")
              (:query (search/kwic! ctx "PROBE" q {:within :sentence}))))))))

(deftest pattern-filter-test
  (when-cwb
   (let [q "[word = \".*\"]"]
     (testing "a pattern accepts every value it matches, beside the values"
       (is (= (search/size! ctx "VISER" q {:filter {:text_year #{"1583"}}})
              (search/size! ctx "VISER" q {:patterns {:text_year ["158."]}})))
       (is (= (search/size! ctx "VISER" q)
              (search/size! ctx "VISER" q {:patterns {:text_year ["15.."]}})
              (search/size! ctx "VISER" q {:filter   {:text_year #{"1583"}}
                                           :patterns {:text_year ["1591"]}})))))))

(deftest context-unit-test
  (when-cwb
   (let [hit (fn [context]
               (first (:hits (search/kwic! ctx "PROBE" "[word = \"hund\"]"
                                           {:rows [0 0] :context context}))))]
     (testing "a hit shown with its sentence: the whole of it, however long"
       (let [{:keys [left right]} (hit :sentence)]
         (is (= ["Katten" "jagter" "en" "lille"] (mapv :word left)))
         (is (= ["i" "haven" "."] (mapv :word right)))))
     (testing "a unit the corpus lacks shows the usual width instead"
       ;; the same hit as the default width gives, rather than a count:
       ;; a page fetches past the width it shows (see
       ;; dk.cst.corpus-probe.search.batch/fetch-context)
       (is (= (hit 5) (hit :paragraph)))))))

(deftest context-overshoot-test
  (when-cwb
   (testing "a page holds more context than the width asked for, cut back
             to the text the hit is in: VISER's first text runs to the
             word before its second one begins"
     (let [{:keys [left match right]}
           (first (:hits (search/kwic! ctx "VISER" "[word = \"ved\"]"
                                       {:rows [0 0] :context 5})))]
       (is (= ["ved"] (mapv :word match)))
       (is (= ["Ridderen" "red" "over" "den" "grønne" "eng" "." "Fruen"
               "stod"]
              (mapv :word left)))
       (is (= ["borgens" "port" "." "Hunden" "fulgte" "ridderen" "til"
               "borgen" "."]
              (mapv :word right)))))))

(deftest subset-test
  (when-cwb
   (let [q    "[pos = \"N.*\"]"
         size (fn [subset] (search/size! ctx "PROBE" q {:subset subset}))
         page (fn [subset] (search/kwic! ctx "PROBE" q {:subset subset}))]
     (is (= 15 (search/size! ctx "PROBE" q)))
     (testing "kept to a value at the start or the end of the match, of a
               positional or a structural attribute"
       (is (= 5 (size {:anchor "match" :attr :lemma :value "hund"})))
       (is (= 7 (size {:anchor "matchend" :attr :text_year :value "2024"}))))
     (testing "kept to a value on the token beside the match, which is
               then marked as the keyword"
       (let [{:keys [size hits]}
             (page {:anchor "match[-1]" :attr :pos :value "D"})]
         (is (= 1 size))
         (is (= 33 (-> hits first :anchors :keyword))))
       (is (= 2 (size {:anchor "matchend[1]" :attr :lemma :value "i"}))))
     (testing "an attribute the corpus lacks is refused before any command"
       (is (thrown? Exception
                    (size {:anchor "match" :attr :nonesuch :value "x"}))))
     (testing "the whole match, narrowed to one of the strings it matched"
       (let [q "[pos = \"D\"] [pos = \"A.*\"]? [pos = \"N.*\"]"]
         (is (= [33] (->> (search/kwic! ctx "PROBE" q
                                        {:subset {:anchor "match..matchend"
                                                  :attr   :word
                                                  :value  "en hund"}})
                          :hits
                          (mapv :cpos)))))))))

(deftest near-test
  (when-cwb
   ;; two noun phrases, only one of them within five words of Katten
   (let [q    "[pos = \"D\"] [pos = \"A.*\"]? [pos = \"N.*\"]"
         near {:word "katten" :distance 5}
         page (search/kwic! ctx "PROBE" q {:near near})]
     (testing "only the hits with the word nearby remain, the word marked
               as their keyword, in the count as in the concordance"
       (is (= 2 (search/size! ctx "PROBE" q)))
       (is (= 1 (search/size! ctx "PROBE" q {:near near})))
       (is (= 1 (:size page)))
       (is (= 5 (-> page :hits first :anchors :keyword))))
     (testing "and the word is reported back with the result"
       (is (= near (:near (search/concordance! ctx ["PROBE"] q
                                               {:near near}))))))))

(deftest narrowing-nothing-test
  (when-cwb
   (let [q      "[pos = \"N.*\"]"
         none   "[word = \"nonesuch\"]"
         filter {:text_year #{"1591"}}
         near   {:word "katten" :distance 5}
         subset {:anchor "match" :attr :lemma :value "hund"}]
     (testing "a narrowing of nothing is nothing, not CQP's refusal to
               narrow an empty result"
       (is (= 0 (search/size! ctx "PROBE" none {:near near})))
       (is (= 0 (search/size! ctx "PROBE" none {:subset subset})))
       (is (= {:size 0 :hits []}
              (select-keys (search/kwic! ctx "PROBE" none {:near near})
                           [:size :hits])))
       (is (= 0 (:size (search/kwic! ctx "PROBE" none {:subset subset}))))
       (is (= [] (:rows (search/export! ctx "PROBE" none
                                       {:near near :limit 10})))))
     (testing "which a metadata filter that leaves a corpus no region makes
               of any query"
       (is (= 0 (search/size! ctx "PROBE" q {:filter filter})))
       (is (= 0 (search/size! ctx "PROBE" q {:filter filter :near near})))
       (is (= 0 (:size (search/kwic! ctx "PROBE" q {:filter filter
                                                    :subset subset})))))
     (testing "and of a chain of narrowings, the first of which empties
               the result for the second"
       (let [none {:anchor "match" :attr :lemma :value "nonesuch"}]
         (is (= 0 (search/size! ctx "PROBE" q {:subset none :near near})))
         (is (= 0 (:size (search/kwic! ctx "PROBE" q {:subset none
                                                      :near   near}))))))
     (testing "while a narrowing of something still narrows"
       (is (= 5 (search/size! ctx "PROBE" q {:subset subset})))
       ;; two of the five nouns with the lemma hund have katten nearby
       (is (= 2 (search/size! ctx "PROBE" q {:subset subset :near near})))))))

(deftest sort-test
  (when-cwb
   (let [order   (fn [opts]
                   (->> (merge {:rows [0 49]} opts)
                        (search/kwic! ctx "PROBE" "[]")
                        :hits
                        (mapv :cpos)))
         natural (order {})
         sorted  (order {:sort "word"})]
     (is (= 47 (count sorted)))
     (testing "word sort reorders the hits away from corpus order"
       (is (not= natural sorted)))
     (testing "context sorts also run and cover the whole result"
       (is (= 47 (count (order {:sort "left"}))))
       (is (= 47 (count (order {:sort "right"})))))
     (testing "an unknown sort mode is corpus order"
       (is (= natural (order {:sort "no such mode"})))))))

(deftest sample-test
  (when-cwb
   (let [all    (search/size! ctx "PROBE" "[]")
         drawn  (fn [n opts]
                  (mapv :cpos (:hits (search/kwic! ctx "PROBE" "[]"
                                                   (merge {:sample n
                                                           :rows   [0 49]}
                                                          opts)))))
         sample (drawn 5 {})]
     (is (= 47 all))
     (testing "a sample holds that many of the matches, and the size the
               search reports is the sample's"
       (is (= 5 (count sample)))
       (is (= 5 (search/size! ctx "PROBE" "[]" {:sample 5}))))
     (testing "the same hits every time, so one URL names one sample"
       (is (= sample (drawn 5 {}))))
     (testing "the sample is of the matches rather than of an order of
               them, so sorting shows the same hits in another order"
       (is (= (set sample) (set (drawn 5 {:sort "word"})))))
     (testing "a sample of more hits than there are is the whole result"
       (is (= all (search/size! ctx "PROBE" "[]" {:sample 1000}))))
     (testing "and asking for no sample leaves the result whole"
       (is (= all (search/size! ctx "PROBE" "[]" {:sample nil})))
       (is (= all (search/size! ctx "PROBE" "[]" {:sample 0})))))))

(deftest danish-collation-test
  ;; requires gawk + the da_DK.UTF-8 locale for CQP's ExternalSort
  (when-cwb
   (let [words (->> (search/kwic! (assoc ctx :sort-locale "da_DK.UTF-8")
                                  "PROBE" "[]" {:sort "word" :rows [0 49]})
                    :hits
                    (mapv (comp :word first :match)))]
     (testing "collation is case-folded Danish, not byte order"
       ;; byte order would sort uppercase Det before lowercase dag
       (is (< (.indexOf words "dag") (.indexOf words "Det"))))
     (testing "o-slash sorts after regular letters within a word"
       (is (< (.indexOf words "Katten") (.indexOf words "København")))))))

(deftest context-expansion-test
  (when-cwb
   (testing "a hit re-fetched by position returns wider context"
     (let [q (command/position-query 9 9)
           {:keys [hits size]} (search/kwic! ctx "PROBE" q
                                             {:context      50
                                              :rows         [0 0]
                                              :struct-attrs []})]
       (is (= 1 size))
       (is (= ["hund"] (map :word (:match (first hits)))))
       (is (pos? (count (:left (first hits)))))))))

(deftest size-test
  (when-cwb
   (is (= 5 (search/size! ctx "PROBE" "\"hund.*\" %c")))
   (testing "corpus-size! reports a failing corpus instead of throwing"
     (is (= 5 (:size (search/corpus-size! ctx "PROBE" "\"hund.*\" %c" {}))))
     (is (= :cqp (-> (search/corpus-size! ctx "TALER" "[lemma = \"x\"]" {})
                     :error :type))))
   (testing "corpus-sizes! keeps the order and stops at the deadline"
     (is (= ["VISER" "PROBE"]
            (map :corpus (search/corpus-sizes! ctx ["VISER" "PROBE"] "[]"
                                               (cwb/deadline ctx) {}))))
     (is (= [:timeout :timeout]
            (map (comp :type :error)
                 (search/corpus-sizes! ctx ["VISER" "PROBE"] "[]" 0 {})))))))

(deftest filter-test
  (when-cwb
   (let [q "[lemma = \"hund\"]"]
     (testing "a filter restricts the hits to the matching regions"
       (is (= 1 (search/size! ctx "VISER" q {:filter {:text_year #{"1591"}}})))
       (is (= 0 (search/size! ctx "VISER" q {:filter {:text_year #{"1583"}}})))
       (is (= [13] (->> (search/kwic! ctx "VISER" q
                                      {:filter {:text_year #{"1591"}}})
                        :hits
                        (map :cpos)))))
     (testing "several attributes must all hold"
       (is (= 1 (search/size! ctx "VISER" q
                              {:filter {:text_year   #{"1591" "1583"}
                                        :text_author #{"ukendt"}}})))
       (is (= 0 (search/size! ctx "VISER" q
                              {:filter {:text_year   #{"1591"}
                                        :text_author #{"nobody"}}}))))
     (testing "attributes from two levels anchor on the innermost"
       (is (= 6 (search/size! ctx "VISER" "[]"
                              {:filter {:text_year #{"1591"} :s_id #{"2"}}}))))
     (testing "a corpus lacking the attribute is rejected before any command"
       (let [{:keys [error]} (search/corpus-size! ctx "TALER" "[]"
                                                  {:filter {:text_author
                                                            #{"x"}}})]
         (is (= :rejected (:type error)))
         (is (re-find #"Not an annotated structural attribute"
                      (:message error))))
       (is (thrown-with-msg? Exception #"Not an annotated structural attribute"
                             (search/kwic! ctx "VISER" "[]"
                                           {:filter {:text #{"x"}}}))))
     (testing "a concordance filters every corpus, counted ones included"
       (let [page (search/concordance! ctx ["VISER" "PROBE"] "[]"
                                       {:page-size 1
                                        :filter    {:text_year #{"1591"
                                                                 "2023"}}})]
         (is (= [{:corpus "VISER" :size 19} {:corpus "PROBE" :size 20}]
                (:counts page))))))))

(deftest concordance-test
  (when-cwb
   (let [q      "[word = \".*en\" %c]"
         sizes  (fn [corpora] (mapv #(search/size! ctx % q) corpora))
         result (search/concordance! ctx ["PROBE" "VISER" "TALER"] q
                                     {:page-size 5})]
     (testing "the counts cover every corpus in order and sum to the size"
       (is (= ["PROBE" "VISER" "TALER"] (mapv :corpus (:counts result))))
       (is (= (sizes ["PROBE" "VISER" "TALER"])
              (mapv :size (:counts result))))
       (is (= (reduce + (sizes ["PROBE" "VISER" "TALER"])) (:size result))))
     (testing "the first page fills from the first corpus"
       (is (= 5 (count (:hits result))))
       (is (= ["PROBE"] (distinct (map :corpus (:hits result))))))
     (testing "a page straddling two corpora continues into the next"
       (let [[n1] (sizes ["PROBE"])
             page (search/concordance! ctx ["PROBE" "VISER"] q
                                       {:page 1 :page-size (dec n1)})]
         (is (= (dec n1) (count (:hits page))))
         (is (= ["PROBE" "VISER"] (distinct (map :corpus (:hits page)))))
         (is (= (first (mapv :cpos (:hits (search/kwic! ctx "VISER" q))))
                (:cpos (second (:hits page)))))))
     (testing "a page past every corpus has no hits but full counts"
       (let [page (search/concordance! ctx ["PROBE" "VISER"] q {:page 99})]
         (is (empty? (:hits page)))
         (is (= (sizes ["PROBE" "VISER"]) (mapv :size (:counts page))))))
     (testing "a corpus lacking a queried attribute fails alone"
       (let [page (search/concordance! ctx ["TALER" "PROBE"]
                                       "[lemma = \"hund\"]")]
         (is (= :cqp (-> page :counts first :error :type)))
         (is (= 5 (-> page :counts second :size)))
         (is (= 5 (:size page)))
         (is (= ["PROBE"] (distinct (map :corpus (:hits page)))))))
     (testing "an exhausted budget stops querying and reports timeouts"
       (let [page (search/concordance! (assoc ctx :search-budget-ms -1)
                                       ["PROBE" "VISER"] q)]
         (is (= [:timeout :timeout] (map (comp :type :error) (:counts page))))
         (is (empty? (:hits page))))))))

(deftest incremental-concordance-test
  (when-cwb
   (let [q       "[]"
         corpora ["PROBE" "VISER" "TALER"]
         page    #(select-keys (search/concordance! ctx corpora q
                                                    {:incremental? true})
                               [:counts :size :remaining])]
     (testing "the corpora past the page are left uncounted, and named"
       (is (= {:counts    [{:corpus "PROBE" :size 47}]
               :size      47
               :remaining ["VISER" "TALER"]}
              (page))))
     (testing "a count made before is reported from memory"
       (search/size! ctx "VISER" q)
       (is (= {:counts    [{:corpus "PROBE" :size 47} {:corpus "VISER" :size 48}]
               :size      95
               :remaining ["TALER"]}
              (page))))
     (testing "and once every corpus is counted nothing remains"
       (search/size! ctx "TALER" q)
       (is (= {:counts [{:corpus "PROBE" :size 47} {:corpus "VISER" :size 48}
                        {:corpus "TALER" :size 42}]
               :size   137}
              (page)))))))

(deftest remember-size-test
  (when-cwb
   (let [q "\"hund.*\" %c"]
     (is (nil? (search/known-size ctx "PROBE" q {})))
     (testing "a page remembers the count its batch made"
       (search/concordance! ctx ["PROBE"] q {:incremental? true})
       (is (= 5 (search/known-size ctx "PROBE" q {})))
       (with-redefs [result/run-result!
                     (fn [& _] (throw (ex-info "counted again" {})))]
         (is (= 5 (search/size! ctx "PROBE" q)))))
     (testing "a narrowing of nothing is remembered like any other count"
       (let [opts {:near {:word "x" :distance 5}}]
         (is (= 0 (search/size! ctx "PROBE" "\"nonesuch\"" opts)))
         (is (= 0 (search/known-size ctx "PROBE" "\"nonesuch\"" opts))))))))

(deftest error-reporting-test
  (when-cwb
   (testing "a bad query throws with the CQP error attached"
     (let [e (try (search/kwic! ctx "PROBE" "[pos = ")
                  (catch Exception e (ex-data e)))]
       (is (= :cqp (-> e :error :type)))))))

(deftest query-lock-test
  (when-cwb
   (testing "redirection smuggled after the query is rejected, not executed"
     (let [canary "/tmp/corpus-probe-pwned"]
       (fs/delete-if-exists canary)
       (is (thrown? Exception
                    (search/kwic! ctx "PROBE"
                                  (str "\"hund\"; cat Last > \"| touch "
                                       canary "\""))))
       (is (not (fs/exists? canary)))))))

(deftest interpolation-guard-test
  (testing "hostile corpus names are rejected before any command is built"
    (is (thrown-with-msg? Exception #"Invalid corpus name"
                          (search/size! {} "PROBE; exit" "[]")))
    (is (thrown-with-msg? Exception #"Invalid corpus name"
                          (search/kwic! {} "probe" "[]"))))
  (when-cwb
   (testing "struct-attrs outside the corpus inventory are rejected"
     (is (thrown? Exception
                  (search/kwic! ctx "PROBE" "\"hund\""
                                {:struct-attrs [:bogus_attr]}))))))

(deftest attribute-sort-test
  ;; requires gawk + the da_DK.UTF-8 locale, like danish-collation-test
  (when-cwb
   (let [danish (assoc ctx :sort-locale "da_DK.UTF-8")]
     (testing "the hits sort by any positional attribute, under the collation"
       (is (= ["bord" "dag" "hav" "have" "hund"]
              (->> (search/kwic! danish "PROBE" "[pos = \"N.*\"]"
                                 {:sort "lemma" :rows [0 4]})
                   :hits
                   (map (comp :lemma first :match))))))
     (testing "and by the word read from its end"
       (is (= ["hund" "hund" "hund" "Hunde" "katte"]
              (->> (search/kwic! danish "PROBE" "[pos = \"N.*\"]"
                                 {:sort "reverse" :rows [0 4]})
                   :hits
                   (map (comp :word first :match)))))))
   (testing "an attribute the corpus lacks is rejected, not sorted around"
     (is (thrown-with-msg? Exception #"Not a positional attribute"
                           (search/kwic! ctx "TALER" "[]" {:sort "lemma"}))))))

(deftest blocks-test
  (let [tokens [{:word "a" :open [:s]} {:word "b"} {:word "c" :open [:s]}]]
    (is (= [[{:word "a" :open [:s]} {:word "b"}] [{:word "c" :open [:s]}]]
           (search/blocks :s tokens)))
    (testing "without a unit the text is one block"
      (is (= [tokens] (search/blocks nil tokens))))
    (testing "a text starting inside a region still starts a block"
      (is (= [[{:word "b"}]] (search/blocks :s [{:word "b"}]))))))

(deftest text-test
  (when-cwb
   (let [text (search/text! ctx "PROBE" 9)]
     (testing "the whole text holding the position, sentence by sentence"
       (is (= "PROBE" (:corpus text)))
       (is (= [0 19] [(:from text) (:to text)]))
       (is (= {:s_id "1" :text_id "t1" :text_title "Hverdag" :text_year "2023"}
              (:structs text)))
       (is (= [["Hunden" "sover" "under" "bordet" "."]
               ["Katten" "jagter" "en" "lille" "hund" "i" "haven" "."]
               ["Hunde" "og" "katte" "er" "gode" "venner" "."]]
              (:blocks text)))))
   (testing "a position no text holds is nothing"
     (is (nil? (search/text! ctx "PROBE" 999))))
   (testing "a hostile corpus name is rejected before any command is built"
     (is (thrown-with-msg? Exception #"Invalid corpus name"
                           (search/text! ctx "PROBE; exit" 9))))))

(deftest export-test
  (when-cwb
   (let [export (search/export! ctx "PROBE" "\"hund.*\" %c"
                                {:limit 100 :sort "corpus"})]
     (testing "every hit as a row of strings: positions, contexts, match,
               then the annotations"
       (is (= 5 (:size export)))
       (is (= [:pos :lemma :s_id :text_id :text_title :text_year]
              (:annotations export)))
       (is (= ["9" "9" ". Katten jagter en lille" "hund" "i haven . Hunde og"
               "NCSI" "hund" "2" "t1" "Hverdag" "2023"]
              (second (:rows export)))))
     (testing "a hit at the corpus edge has a short context, not blanks"
       (is (= ["0" "0" "" "Hunden" "sover under bordet . Katten"
               "NCSD" "hund" "1" "t1" "Hverdag" "2023"]
              (first (:rows export))))))
   (testing "the limit cuts the rows and not the size"
     (let [export (search/export! ctx "PROBE" "\"hund.*\" %c" {:limit 2})]
       (is (= 5 (:size export)))
       (is (= 2 (count (:rows export))))))
   (testing "a unit of context becomes a number of words"
     (is (= "Hunden sover under bordet . Katten jagter en lille"
            (nth (second (:rows (search/export! ctx "PROBE" "\"hund.*\" %c"
                                                {:limit   5
                                                 :context :sentence})))
                 2))))))

(deftest export-corpora-test
  (when-cwb
   (let [exports (search/export-corpora! ctx ["PROBE" "NOSUCH" "VISER"]
                                         "\"hund.*\" %c" (cwb/deadline ctx)
                                         6 {})]
     (testing "corpus by corpus, a failing one carrying its error"
       (is (= ["PROBE" "NOSUCH" "VISER"] (map :corpus exports)))
       (is (= 5 (count (:rows (first exports)))))
       (is (:error (second exports))))
     (testing "each within what is left of the limit"
       (is (= 1 (count (:rows (nth exports 2)))))
       (is (= 1 (count (search/export-corpora! ctx ["PROBE" "VISER"]
                                               "\"hund.*\" %c"
                                               (cwb/deadline ctx) 5 {})))))
     (testing "and none once the deadline has passed"
       (is (= [{:type :timeout}]
              (map :error (search/export-corpora! ctx ["PROBE"] "[]" 0 5
                                                  {}))))))))
