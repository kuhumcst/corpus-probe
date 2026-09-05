(ns dk.cst.corpus-probe.export-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.export :as export]))

(def hits
  [{:corpus  "PROBE"
    :cpos    9
    :anchors {:matchend 9}
    :left    [{:word "en" :pos "D" :lemma "en"}]
    :match   [{:word "hund" :pos "NCSI" :lemma "hund"}]
    :right   [{:word "i" :pos "PP" :lemma "i"}]
    :structs {:text_title "Hverdag" :text_year "2023"}}
   {:corpus  "TALER"
    :cpos    4
    :anchors {:matchend 5}
    :left    []
    :match   [{:word "den"} {:word "grønne"}]
    :right   [{:word "omstilling"}]
    :structs {:text_party "S"}}])

(deftest kwic-table-test
  (let [[header row1 row2] (export/kwic-table {:hits hits})]
    (testing "columns cover the annotations any hit carries"
      (is (= ["corpus" "cpos" "matchend" "left" "match" "right"
              "match pos" "match lemma" "text_title" "text_year" "text_party"]
             header)))
    (testing "contexts are words, annotations space-joined per match token"
      (is (= ["PROBE" "9" "9" "en" "hund" "i" "NCSI" "hund" "Hverdag" "2023" ""]
             row1))
      (is (= ["TALER" "4" "5" "" "den grønne" "omstilling" "" "" "" "" "S"]
             row2)))))

(deftest frequency-table-test
  (let [rows (export/frequency-table
              {:attr   :lemma
               :counts [{:corpus "PROBE" :tokens 47 :size 15}
                        {:corpus "VISER" :tokens 48 :size 16}
                        {:corpus "TALER" :error {:type :cqp}}]
               :rows   [{:value "hund"
                         :freqs {"PROBE" 5 "VISER" 1}
                         :total 6}]})]
    (is (= ["lemma" "PROBE frequency" "PROBE per million" "VISER frequency"
            "VISER per million" "total frequency" "total per million"]
           (first rows)))
    (is (= ["hund" "5" "106383.0" "1" "20833.3" "6" "63157.9"] (second rows)))
    (testing "the texts counted make a third column per group"
      (let [rows (export/frequency-table
                  {:attr   :lemma
                   :docs   true
                   :counts [{:corpus "PROBE" :tokens 47 :size 15}
                            {:corpus "VISER" :tokens 48 :size 16}]
                   :rows   [{:value "hund"
                             :freqs {"PROBE" 5 "VISER" 1}
                             :docs  {"PROBE" 3 "VISER" 1}
                             :total 6}]})]
        (is (= ["lemma" "PROBE frequency" "PROBE per million" "PROBE texts"
                "VISER frequency" "VISER per million" "VISER texts"
                "total frequency" "total per million" "total texts"]
               (first rows)))
        (is (= ["hund" "5" "106383.0" "3" "1" "20833.3" "1" "6" "63157.9" "4"]
               (second rows)))))
    (testing "one corpus means no totals"
      (is (= ["word" "PROBE frequency" "PROBE per million"]
             (first (export/frequency-table
                     {:attr :word :counts [{:corpus "PROBE" :tokens 47}]
                      :rows []})))))))

(deftest tsv-test
  (is (= "a\tb\nc\td\n" (export/tsv [["a" "b"] ["c" "d"]])))
  (testing "TAB and line breaks inside a value become spaces"
    (is (= "a b c\n" (export/tsv [["a\tb\nc"]])))))

(deftest csv-test
  (testing "plain values are bare, others quoted with doubled quotes"
    (is (= "a,\"b,c\",\"12\"\"\"\r\n"
           (subs (export/csv [["a" "b,c" "12\""]]) 1))))
  (testing "the text starts with a byte order mark"
    (is (str/starts-with? (export/csv [["a"]]) "\ufeff"))))
