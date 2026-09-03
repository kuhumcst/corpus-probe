(ns dk.cst.corpus-probe.views.frequencies-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.views.hiccup :refer [deep]]
            [dk.cst.corpus-probe.views.frequencies :as freq]))

(deftest attr-control-test
  (let [html (freq/attr-control [{:type :positional :name :word}
                                 {:type :positional :name :lemma}
                                 {:type :structural :name :text_year}]
                                "lemma")]
    (testing "positional and structural attributes are separate option groups"
      (is (= ["positional attributes" "structural attributes"]
             (keep :label (deep html)))))
    (testing "the chosen attribute is selected"
      (is (some #(and (map? %) (= "lemma" (:value %)) (:selected %))
                (deep html)))
      (is (not (some #(and (map? %) (= "word" (:value %)) (:selected %))
                     (deep html)))))))

(def sample-result
  {:query  "[pos = \"N.*\"]"
   :attr   :lemma
   :counts [{:corpus "PROBE" :tokens 47 :size 15}
            {:corpus "VISER" :tokens 48 :size 16}
            {:corpus "TALER" :error {:type :cqp :message "no lemma"}}]
   :rows   [{:value "hund" :freqs {"PROBE" 5 "VISER" 1} :total 6}
            {:value "borg" :freqs {"VISER" 2} :total 2}]})

(deftest frequency-summary-test
  (testing "only the corpora that could be counted are counted"
    (is (= "31 hits in 2 corpora by lemma · 2 values"
           (freq/frequency-summary sample-result 2))))
  (testing "a cut table says so"
    (is (re-find #"the 1 most frequent shown"
                 (freq/frequency-summary sample-result 1))))
  (testing "a whole-corpus table counts all tokens"
    (is (= "All tokens in PROBE by word · 0 values"
           (freq/frequency-summary {:query  ""
                                    :attr   :word
                                    :counts [{:corpus "PROBE" :tokens 47
                                              :size 47}]
                                    :rows   []}
                                   0)))))

(deftest frequency-table-test
  (let [table (freq/frequency-table sample-result)
        [_ _ _ colgroups thead tbody] table
        row   (first (nth tbody 1))]
    (testing "a column group per readable corpus and one for the total"
      (is (= [[:colgroup {:span 2}] [:colgroup {:span 2}] [:colgroup {:span 2}]]
             colgroups)))
    (testing "the counts are headed frequency, CWB's word"
      (is (some #{"frequency"} (deep thead)))
      (is (not (some #{"hits"} (deep thead)))))
    (testing "the header names the attribute, each readable corpus and total"
      (is (= [[:code "lemma"] [:code "PROBE"] [:code "VISER"] "total"]
             (filter #(or (and (vector? %) (= :code (first %))) (= "total" %))
                     (deep (nth thead 1))))))
    (testing "a value missing from a corpus counts as zero there"
      (let [borg-row (nth (second tbody) 1)]
        (is (= [:td.n "0"] (first (first (nth borg-row 2)))))))
    (testing "the value cell renders like an attribute value"
      (is (= [:th {:scope "row"} "hund"] (nth row 1))))
    (testing "one corpus gets no total columns"
      (let [single (freq/frequency-table
                    (update sample-result :counts (partial take 1)))]
        (is (not (some #{"total"} (deep single))))))))
