(ns dk.cst.corpus-probe.views.frequencies-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.views.hiccup :refer [deep]]
            [dk.cst.corpus-probe.views.frequencies :as freq]
            [dk.cst.corpus-probe.views.layout :as layout]
            [dk.cst.corpus-probe.views.page :as page]))

(def counted
  "A frequency result of one corpus that could be counted."
  {:attr   :word
   :query  "\"hund\""
   :counts [{:corpus "PROBE" :tokens 47 :size 5}]
   :rows   [{:value "hund" :freqs {"PROBE" 3} :total 3}]})

(deftest counted?-test
  (is (freq/counted? counted))
  (is (not (freq/counted? {:counts [{:corpus "X" :error {:type :timeout}}]})))
  (is (not (freq/counted? nil))))

(deftest frequency-heading-test
  (testing "a table that could be counted is headed by its summary"
    (is (= "5 hits in PROBE by word · 1 value"
           (freq/frequency-heading "en" counted nil 1))))
  (testing "a request no corpus answered is headed by its error instead"
    (is (= "The query timed out"
           (freq/frequency-heading
            "en" {:counts [{:corpus "X" :error {:type :timeout}}]} nil 0)))
    (is (= "No corpus selected"
           (freq/frequency-heading "en" nil {:type :no-corpus} 0)))))

(deftest frequency-section-test
  (let [html (freq/frequency-section {:lang "en" :result counted})]
    (testing "the region names itself and can be landed on"
      (is (= {:id              page/results-id
              :tabindex        "-1"
              :aria-labelledby "results-heading"}
             (second html))))
    (testing "its heading is the summary the caption used to carry"
      (is (some #{[:h2 {:id "results-heading"}
                   "5 hits in PROBE by word · 1 value"]}
                (deep html))))))

(deftest frequencies-view-test
  (let [html (freq/frequencies-view {:lang "en" :folders [] :params {}})]
    (testing "the page names itself and the bypass link can reach it"
      (is (= layout/main-attrs (second html)))
      (is (some #{[:h1 "Frequencies"]} (deep html))))
    (testing "the form submits to the results"
      (is (= (str "/frequencies" page/results-fragment)
             (get-in html [3 1 1 :action]))))))

(deftest attr-control-test
  (let [html (freq/attr-control "en"
                                [{:type :positional :name :word}
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
           (freq/frequency-summary "en" sample-result 2))))
  (testing "a metadata filter qualifies the corpora"
    (is (= "31 hits in 2 corpora within text_year 1591 by lemma · 2 values"
           (freq/frequency-summary "en"
                                   (assoc sample-result
                                          :filter {:text_year #{"1591"}})
                                   2))))
  (testing "a cut table says so"
    (is (re-find #"the 1 most frequent shown"
                 (freq/frequency-summary "en" sample-result 1))))
  (testing "a whole-corpus table counts all tokens"
    (is (= "All tokens in PROBE by word · 0 values"
           (freq/frequency-summary "en"
                                   {:query  ""
                                    :attr   :word
                                    :counts [{:corpus "PROBE" :tokens 47
                                              :size 47}]
                                    :rows   []}
                                   0))))
  (testing "the same caption in Danish, the attribute name untranslated"
    (is (= "31 træf i 2 korpusser efter lemma · 2 værdier"
           (freq/frequency-summary "da" sample-result 2)))
    (is (re-find #", de 1 hyppigste vises"
                 (freq/frequency-summary "da" sample-result 1)))))

(deftest frequency-table-test
  (let [table (freq/frequency-table "en" sample-result)
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
                    "en" (update sample-result :counts (partial take 1)))]
        (is (not (some #{"total"} (deep single))))))
    (testing "the headings are translated, the attribute name is not"
      (let [da (deep (freq/frequency-table "da" sample-result))]
        (is (some #{"frekvens"} da))
        (is (some #{"i alt"} da))
        (is (some #{[:code "lemma"]} da))))))
