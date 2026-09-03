(ns dk.cst.corpus-probe.views.kwic-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.views.kwic :as kwic]))

(deftest token-title-test
  (testing "non-word attributes join into the tooltip"
    (is (= "NCSI · hund" (kwic/token-title {:word "hund" :pos "NCSI"
                                            :lemma "hund"}))))
  (testing "structure tags and blank values are excluded"
    (is (= "NCSI" (kwic/token-title {:word "hund" :pos "NCSI" :lemma ""
                                     :open [:s]})))))

(deftest struct-summary-test
  (is (= "Hverdag" (kwic/struct-summary {:text_id "t1" :text_title "Hverdag"})))
  (testing "falls back to text_id, then any value"
    (is (= "t1" (kwic/struct-summary {:text_id "t1"})))
    (is (= "x" (kwic/struct-summary {:other "x"})))))

(deftest hit-row-test
  (let [row (kwic/hit-row {:cpos 9
                           :left  [{:word "lille" :pos "AN" :lemma "lille"}]
                           :match [{:word "hund" :pos "NCSI" :lemma "hund"}]
                           :right [{:word "i" :pos "PP" :lemma "i"}]
                           :structs {:text_title "Hverdag"}})]
    (testing "the match is wrapped in a mark element"
      (is (= :mark (get-in row [4 1 0]))))
    (testing "the corpus position is shown"
      (is (= "9" (get-in row [1 1]))))))
