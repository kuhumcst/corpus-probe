(ns dk.cst.corpus-probe.views.frequencies-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.views.hiccup :refer [da deep en]]
            [dk.cst.corpus-probe.views.frequencies :as freq]
            [dk.cst.corpus-probe.views.page :as page]))

(def counted
  "A frequency result of one corpus that could be counted."
  {:attr   :word
   :query  "\"hund\""
   :counts [{:corpus "PROBE" :tokens 47 :size 5}]
   :rows   [{:value "hund" :freqs {"PROBE" 3} :total 3}]})

(deftest tabled?-test
  (is (freq/tabled? counted))
  (is (not (freq/tabled? {:counts [{:corpus "X" :error {:type :timeout}}]})))
  (is (not (freq/tabled? nil))))

(deftest frequency-heading-test
  (testing "a table that could be counted is headed by its summary"
    (is (= "5 hits in PROBE by word · 1 value"
           (freq/frequency-heading en counted nil 1))))
  (testing "a request no corpus answered is headed by its error instead"
    (is (= "The query timed out"
           (freq/frequency-heading
            "en" {:counts [{:corpus "X" :error {:type :timeout}}]} nil 0)))
    (is (= "No corpus selected"
           (freq/frequency-heading en nil {:type :no-corpus} 0)))))

(deftest frequency-section-test
  (let [html (freq/frequency-section
              {:lang       "en"
               :result     counted
               :view       :frequencies
               :view-hrefs [[:kwic "/?view=kwic"]
                            [:frequencies "/?view=frequencies"]]})]
    (testing "it is the shared results region, so a search lands on it"
      (is (= {:id              page/results-id
              :tabindex        "-1"
              :aria-labelledby "results-heading"}
             (second html))))
    (testing "its heading is the summary the caption used to carry"
      (is (some #{[:h2 {:id "results-heading"}
                   "5 hits in PROBE by word · 1 value"]}
                (deep html))))
    (testing "the view switch marks the frequency view as the current one"
      (is (some #(and (map? %) (= "/?view=frequencies" (:href %))
                      (= "page" (:aria-current %)))
                (deep html)))
      (is (some #(and (map? %) (= "/?view=kwic" (:href %))
                      (not (:aria-current %)))
                (deep html))))))

(deftest attr-control-test
  (let [html (freq/attr-control en
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

(deftest position-control-test
  (let [positions ["match[-1]" "match" "match..matchend" "matchend"
                   "matchend[1]"]
        html      (freq/position-control en positions "matchend[1]")]
    (testing "one option per position, worded, the chosen one marked"
      (is (= ["before the match" "at the start of the match"
              "over the whole match" "at the end of the match"
              "after the match"]
             (keep #(when (and (vector? %) (= :option (first %))) (last %))
                   (deep html))))
      (is (some #(and (map? %) (= "matchend[1]" (:value %)) (:selected %))
                (deep html))))
    (testing "it submits the query form and applies itself, as the
              grouping does"
      (is (some #(and (map? %) (= "at" (:name %)) (= page/form-id (:form %))
                      (= [:apply-view] (get-in % [:on :change])))
                (deep html))))
    (testing "in Danish"
      (is (some #{"før matchet"} (deep (freq/position-control da positions nil)))))))

(deftest linked-rows-test
  (testing "a row with a link links its value to the hits it counted"
    (is (some #{[:th {:scope "row"} [:a {:href "/?subset=hund"} "hund"]]}
              (deep (freq/frequency-table
                     en (assoc counted
                               :rows [{:value "hund" :freqs {"PROBE" 3}
                                       :total 3 :href "/?subset=hund"}]))))))
  (testing "and one without a link is plain text"
    (is (some #{[:th {:scope "row"} "hund"]}
              (deep (freq/frequency-table en counted))))))

(deftest docs-test
  (let [result (assoc counted
                      :docs true
                      :rows [{:value "hund" :freqs {"PROBE" 3}
                              :docs  {"PROBE" 2} :total 3}])
        html   (freq/frequency-table en result)]
    (testing "a table counting texts has a third column per corpus"
      (is (some #{[:th {:scope "col"} "texts"]} (deep html)))
      (is (some #{[:td.n "2"]} (deep html)))
      (is (some #{[:colgroup {:span 3}]} (deep html))))
    (testing "and one that does not has two"
      (is (not (some #{[:th {:scope "col"} "texts"]}
                     (deep (freq/frequency-table en counted)))))
      (is (some #{[:colgroup {:span 2}]} (deep (freq/frequency-table en counted))))))
  (testing "the control applies itself through the query form"
    (is (some #(and (map? %) (= "docs" (:name %)) (:checked %)
                    (= page/form-id (:form %))
                    (= [:apply-view] (get-in % [:on :change])))
              (deep (freq/docs-control en true))))
    (is (some #{"tæl tekster"} (deep (freq/docs-control da false))))))

(deftest frequency-summary-test
  (testing "where in the match the table counts is said after the attribute"
    (is (= "5 hits in PROBE by word before the match · 1 value"
           (freq/frequency-summary en (assoc counted :at "match[-1]") 1)))))

(deftest frequency-summary-original-test
  (testing "only the corpora that could be counted are counted"
    (is (= "31 hits in 2 corpora by lemma · 2 values"
           (freq/frequency-summary en sample-result 2))))
  (testing "a metadata filter qualifies the corpora"
    (is (= "31 hits in 2 corpora within text_year 1591 by lemma · 2 values"
           (freq/frequency-summary en
                                   (assoc sample-result
                                          :filter {:text_year #{"1591"}})
                                   2))))
  (testing "a cut table says so"
    (is (re-find #"the 1 most frequent shown"
                 (freq/frequency-summary en sample-result 1))))
  (testing "a whole-corpus table counts all tokens"
    (is (= "All tokens in PROBE by word · 0 values"
           (freq/frequency-summary en
                                   {:query  ""
                                    :attr   :word
                                    :counts [{:corpus "PROBE" :tokens 47
                                              :size 47}]
                                    :rows   []}
                                   0))))
  (testing "the same caption in Danish, the attribute name untranslated"
    (is (= "31 træf i 2 korpusser efter lemma · 2 værdier"
           (freq/frequency-summary da sample-result 2)))
    (is (re-find #", de 1 hyppigste vises"
                 (freq/frequency-summary da sample-result 1)))))

(deftest frequency-table-test
  (let [table (freq/frequency-table en sample-result)
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
      (let [da (deep (freq/frequency-table da sample-result))]
        (is (some #{"frekvens"} da))
        (is (some #{"i alt"} da))
        (is (some #{[:code "lemma"]} da))))))
