(ns dk.cst.corpus-probe.search-test
  "Integration tests for the full search round trip (milestone 1's exit
  criterion); skipped when CWB or the dev corpus is missing."
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.cqp-test :refer [ctx when-cwb]]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.search :as search]))

(deftest kwic-test
  (when-cwb
   (let [{:keys [size hits] :as page}
         (search/kwic! ctx "PROBE" "\"hund.*\" %c" {:page-size 3})]
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
     (testing "paging"
       (let [page2 (search/kwic! ctx "PROBE" "\"hund.*\" %c"
                                 {:page-size 3 :page 1})]
         (is (= 2 (count (:hits page2))))
         (is (= 34 (-> page2 :hits first :cpos))))))))

(deftest simple-search-round-trip-test
  (when-cwb
   (is (= 5 (:size (search/kwic! ctx "PROBE"
                                 (query/simple->cqp "hund"
                                                    {:prefix?           true
                                                     :case-insensitive? true})))))))

(deftest sort-test
  (when-cwb
   (let [order   (fn [opts] (mapv :cpos (:hits (search/kwic! ctx "PROBE" "[]"
                                                             (merge {:page-size 50}
                                                                    opts)))))
         natural (order {})
         sorted  (order {:sort "word"})]
     (is (= 47 (count sorted)))
     (testing "word sort reorders the hits away from corpus order"
       (is (not= natural sorted)))
     (testing "context sorts also run and cover the whole result"
       (is (= 47 (count (order {:sort "left"}))))
       (is (= 47 (count (order {:sort "right"})))))
     (testing "an unknown sort mode is corpus order"
       (is (= natural (order {:sort "bogus"})))))))

(deftest danish-collation-test
  ;; requires gawk + the da_DK.UTF-8 locale for CQP's ExternalSort
  (when-cwb
   (let [words (->> (search/kwic! (assoc ctx :sort-locale "da_DK.UTF-8")
                                  "PROBE" "[]" {:sort "word" :page-size 50})
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
     (let [q (query/position-query 9 9)
           {:keys [hits size]} (search/kwic! ctx "PROBE" q
                                             {:context      50
                                              :page-size    1
                                              :struct-attrs []})]
       (is (= 1 size))
       (is (= ["hund"] (map :word (:match (first hits)))))
       (is (pos? (count (:left (first hits)))))))))

(deftest frequencies-test
  (when-cwb
   (let [freqs (search/frequencies! ctx "PROBE" "[pos = \"N.*\"]" :lemma)]
     (is (= {:values ["hund"] :freq 5} (first freqs)))
     (is (= 10 (count freqs))))))

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
    (is (thrown? Exception (search/corpus-ctx {} "PROBE; exit")))
    (is (thrown? Exception (search/corpus-ctx {} "probe"))))
  (when-cwb
   (testing "attribute names outside the corpus inventory are rejected"
     (let [canary "/tmp/corpus-probe-pwned-attr"]
       (fs/delete-if-exists canary)
       (is (thrown? Exception
                    (search/frequencies!
                     ctx "PROBE" "\"hund\""
                     (str "lemma > \"| touch " canary "\""))))
       (is (not (fs/exists? canary)))))
   (testing "struct-attrs outside the corpus inventory are rejected"
     (is (thrown? Exception
                  (search/kwic! ctx "PROBE" "\"hund\""
                                {:struct-attrs [:bogus_attr]}))))))
