(ns dk.cst.corpus-probe.views.text-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.views.hiccup :refer [da deep en]]
            [dk.cst.corpus-probe.views.text :as text]))

(def hverdag
  "A text of the dev corpus, as dk.cst.corpus-probe.api/text-page hands
  it to the view."
  {:corpus  "PROBE"
   :from    0
   :to      12
   :structs {:text_id "t1" :text_title "Hverdag" :text_year "2023"}
   :blocks  [["Hunden" "sover" "under" "bordet" "."]
             ["Katten" "jagter" "en" "lille" "hund" "i" "haven" "."]]
   :hit     [9 9]
   :lang    "da"})

(deftest text-name-test
  (is (= "Hverdag" (text/text-name en {:text_title "Hverdag" :text_id "t1"})))
  (is (= "t1" (text/text-name en {:text_id "t1"})))
  (is (= "Tekst" (text/text-name da nil))))

(deftest marked-test
  (testing "the hit is marked inside its block, and lands the page"
    (is (= ["Katten jagter en lille" " " [:mark {:id "hit"} "hund"] " "
            "i haven ."]
           (text/marked 5 [9 9] true
                        ["Katten" "jagter" "en" "lille" "hund" "i" "haven"
                         "."]))))
  (testing "a mark elsewhere carries no id"
    (is (= [[:mark {} "Hunden sover"] " " "under bordet ."]
           (text/marked 0 [0 1] false ["Hunden" "sover" "under" "bordet" "."]))))
  (testing "without a hit the block is plain text"
    (is (= "Hunden sover" (text/marked 0 nil false ["Hunden" "sover"])))))

(deftest reading-view-test
  (let [html (text/reading-view en hverdag)
        all  (deep html)]
    (testing "a document named by the text, its prose in the corpus's language"
      (is (= :main.document (first html)))
      (is (some #{[:h1 "Hverdag"]} all))
      (is (some #(and (map? %) (= "da" (:lang %))) all)))
    (testing "it says which corpus the text is from"
      (is (some #(and (map? %) (= "/corpora/probe" (:href %))) all)))
    (testing "the hit is marked once, in the block that holds it"
      (is (= 1 (count (filter #{[:mark {:id "hit"} "hund"]} all))))
      (is (not (some #(and (vector? %) (= :mark (first %)) (= {} (second %)))
                     all)))))
  (testing "an error stands in for the text"
    (let [html (deep (text/reading-view en {:corpus "X"
                                            :error  {:type :no-texts}}))]
      (is (some #{"The corpus marks no texts"} html))
      (is (some #{[:h1 "Text"]} html)))))
