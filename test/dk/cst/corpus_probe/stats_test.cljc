(ns dk.cst.corpus-probe.stats-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.stats :as stats]))

(deftest per-million-test
  (is (= 106383.0 (stats/per-million 5 47)))
  (is (= 0.5 (stats/per-million 1 2000000)))
  (testing "an empty corpus has no rate"
    (is (nil? (stats/per-million 0 0)))))

(def counts
  "The counts of a result over two corpora that could be counted and one
  that could not."
  [{:corpus "PROBE" :tokens 47 :size 5}
   {:corpus "VISER" :tokens 48 :size 2}
   {:corpus "GONE" :error {:type :timeout}}])

(deftest counts-test
  (testing "a corpus that could not be counted measures nothing"
    (is (= ["PROBE" "VISER"] (map :corpus (stats/readable-counts counts))))
    (is (= 95 (stats/tokens counts))))
  (testing "a total stands beside several corpora, not beside one"
    (is (stats/total? counts))
    (is (not (stats/total? (take 1 counts))))
    (is (not (stats/total? [])))))

(deftest row-test
  (let [row {:value  "hund"
             :freqs  {"PROBE" 3 "VISER" 1}
             :total  4
             :docs   {"PROBE" 2 "VISER" 1}
             :tokens {"PROBE" 20 "VISER" 10}}]
    (testing "a frequency is measured against its corpus, and the total
              against every corpus counted"
      (is (= 47 (stats/row-tokens false 47 "PROBE" row)))
      (is (= 95 (stats/row-tokens false 95 row))))
    (testing "unless the result is sized, when the value's own text is"
      (is (= 20 (stats/row-tokens true 47 "PROBE" row)))
      (is (= 0 (stats/row-tokens true 47 "TALER" row)))
      (is (= 30 (stats/row-tokens true 95 row))))
    (testing "the texts it occurs in, per corpus and in all"
      (is (= 2 (stats/row-docs "PROBE" row)))
      (is (= 0 (stats/row-docs "TALER" row)))
      (is (= 3 (stats/row-docs row)))
      (is (= 0 (stats/row-docs (dissoc row :docs)))))))
