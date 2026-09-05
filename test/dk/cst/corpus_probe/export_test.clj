(ns dk.cst.corpus-probe.export-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.export :as export]))

(deftest kwic-header-test
  (is (= ["corpus" "cpos" "matchend" "left" "match" "right" "match pos"
          "match lemma" "text_title" "text_party"]
         (export/kwic-header [:pos :lemma] [:text_title :text_party]))))

(deftest kwic-rows-test
  (testing "a corpus's rows take the columns of the union, empty where it
            lacks one"
    (is (= [["TALER" "4" "5" "" "den grønne" "omstilling" "" "" "" "S"]]
           (export/kwic-rows [:pos :lemma] [:text_title :text_party]
                             {:corpus      "TALER"
                              :annotations [:text_party]
                              :rows        [["4" "5" "" "den grønne"
                                             "omstilling" "S"]]})))
    (is (= [["PROBE" "9" "9" "en" "hund" "i" "NCSI" "hund" "Hverdag" ""]]
           (export/kwic-rows [:pos :lemma] [:text_title :text_party]
                             {:corpus      "PROBE"
                              :annotations [:pos :lemma :text_title]
                              :rows        [["9" "9" "en" "hund" "i" "NCSI"
                                             "hund" "Hverdag"]]})))))

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

(deftest sized-frequency-table-test
  (let [rows (export/frequency-table
              {:attr   :text_year
               :sized  true
               :counts [{:corpus "PROBE" :tokens 47 :size 15}
                        {:corpus "VISER" :tokens 48 :size 16}]
               :rows   [{:value  "2023"
                         :freqs  {"PROBE" 8}
                         :total  8
                         :tokens {"PROBE" 20}}]})]
    (testing "a sized table adds the tokens of each value and rates against them"
      (is (= ["text_year" "PROBE frequency" "PROBE per million" "PROBE tokens"
              "VISER frequency" "VISER per million" "VISER tokens"
              "total frequency" "total per million" "total tokens"]
             (first rows)))
      (is (= ["2023" "8" "400000.0" "20" "0" "" "0" "8" "400000.0" "20"]
             (second rows))))))

(deftest crosstab-table-test
  (let [result {:attr    :lemma
                :by      :text_year
                :sized   true
                :counts  [{:corpus "PROBE" :tokens 47 :size 15}]
                :columns [{:value "2023" :total 3 :tokens 20}
                          {:value "2024" :total 2 :tokens 27}]
                :rows    [{:value "hund" :cells {"2023" 3 "2024" 2} :total 5}]}
        [header tokens row] (export/crosstab-table result)]
    (is (= ["lemma" "2023 frequency" "2023 per million" "2024 frequency"
            "2024 per million" "total frequency" "total per million"]
           header))
    (testing "the tokens each column measures against come first"
      (is (= ["tokens" "20" "" "27" "" "47" ""] tokens)))
    (is (= ["hund" "3" "150000.0" "2" "74074.1" "5" "106383.0"] row))
    (testing "unsized, a column is its frequency alone"
      (is (= [["lemma" "2023 frequency" "total frequency"] ["hund" "3" "3"]]
             (export/crosstab-table
              {:attr    :lemma
               :counts  [{:corpus "PROBE" :tokens 47}]
               :columns [{:value "2023" :total 3}]
               :rows    [{:value "hund" :cells {"2023" 3} :total 3}]}))))))

(deftest lines-test
  (is (= "a\tb\n" (export/tsv-line ["a" "b"])))
  (is (= "a,\"b,c\"\r\n" (export/csv-line ["a" "b,c"])))
  (testing "the formats render a table as they render its lines"
    (is (= (export/tsv [["a"] ["b"]])
           (apply str (map (:line (export/formats "tsv")) [["a"] ["b"]]))))
    (is (= (export/csv [["a"]])
           (str (:preamble (export/formats "csv"))
                ((:line (export/formats "csv")) ["a"]))))))
