(ns dk.cst.corpus-probe.views.frequencies-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.url :as url]
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

(defn text
  "The strings of hiccup `x`, joined: what it reads as."
  [x]
  (apply str (filter string? (tree-seq coll? seq x))))

(deftest frequency-heading-test
  (testing "a table that could be counted is headed by what was found,
            the hits it counted, for the query"
    (is (= (list "5 hits" " " "for" " " [:q "hund"])
           (freq/frequency-heading en {:q "hund"} counted nil)))
    (is (= "All tokens"
           (freq/frequency-heading en {:q ""} counted nil))))
  (testing "a request no corpus answered is headed by its error instead"
    (is (= "The search did not finish in time"
           (freq/frequency-heading
            "en" {:q "hund"} {:counts [{:corpus "X" :error {:type :timeout}}]}
            nil)))
    (is (= "No corpus selected"
           (freq/frequency-heading en {:q "hund"} nil {:type :no-corpus})))))

(deftest grouping-phrase-test
  (testing "by what, where in the match, and against what"
    (is (= "by word before the match"
           (text (freq/grouping-phrase en (assoc counted :at "match[-1]")))))
    (is (= "efter lemma og text_year"
           (text (freq/grouping-phrase da {:attr :lemma :by :text_year}))))
    (testing "the attribute names as the code they are"
      (is (some #{[:code "word"]} (deep (freq/grouping-phrase en counted)))))))

(deftest frequency-section-test
  (let [html (freq/frequency-section
              {:lang       "en"
               :params     {:q "hund"}
               :result     counted
               :view       :frequencies
               :view-hrefs [[:kwic "/?view=kwic"]
                            [:frequencies "/?view=frequencies"]]})]
    (testing "it is the shared results region, so a search lands on it"
      (is (= {:id              url/results-id
              :tabindex        "-1"
              :aria-labelledby "results-heading"}
             (second html))))
    (testing "its heading is the answer, and under it how the table counted
              comes before the rest of the question"
      (let [[tag h1 sub] (nth html 2)]
        (is (= :hgroup tag))
        (is (= "5 hits for hund" (text (drop 2 h1))))
        (is (= "by word · in PROBE" (text sub)))))
    (testing "the table's size is its caption's to say"
      (is (some #(and (vector? %) (= :caption (first %))
                      (= "Frequencies · 1 value" (text %)))
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

(deftest table-caption-test
  (testing "the table is named and sized"
    (is (= "Frequencies · 2 values"
           (text (freq/table-caption en sample-result))))
    (is (= "Frekvenser · 2 værdier"
           (text (freq/table-caption da sample-result)))))
  (testing "a cut table says so"
    (let [cut (assoc sample-result :rows (repeat (inc freq/row-limit) {}))]
      (is (re-find #"values, the \d+ most frequent shown"
                   (text (freq/table-caption en cut))))
      (is (re-find #", de \d+ hyppigste vises"
                   (text (freq/table-caption da cut))))))
  (testing "only the corpora that could be counted are counted, in the
            heading"
    (is (= "31 hits for hund"
           (text (freq/frequency-heading en {:q "hund"} sample-result nil))))))

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

(deftest by-control-test
  (let [attrs [{:type :positional :name :word}
               {:type :structural :name :text_year}]
        html  (freq/by-control en attrs :text_year)]
    (testing "the corpora are the columns unless an attribute is chosen"
      (is (some #(and (map? %) (= "" (:value %)) (not (:selected %)))
                (deep html)))
      (is (some #(and (map? %) (= "" (:value %)) (:selected %))
                (deep (freq/by-control en attrs nil))))
      (is (some #{"corpora"} (deep html))))
    (testing "the structural attributes come first"
      (is (= ["structural attributes" "positional attributes"]
             (keep :label (deep html)))))
    (testing "the chosen attribute is selected, and the control applies itself"
      (is (some #(and (map? %) (= "text_year" (:value %)) (:selected %))
                (deep html)))
      (is (some #(and (map? %) (= "by" (:name %)) (= page/form-id (:form %))
                      (= [:apply-view] (get-in % [:on :change])))
                (deep html))))
    (is (some #{"kolonner"} (deep (freq/by-control da attrs nil))))))

(def crosstab
  "A frequency result counted against a second, structural attribute."
  {:query        "[pos = \"N.*\"]"
   :attr         :lemma
   :by           :text_year
   :at           "match"
   :sized        true
   :counts       [{:corpus "PROBE" :tokens 47 :size 15}]
   :columns      [{:value "2023" :total 3 :tokens 20}
                  {:value "2024" :total 2 :tokens 27}]
   :column-count 2
   :rows         [{:value "hund" :cells {"2023" 3 "2024" 2} :total 5
                   :href  "/?subset=hund"}]})

(deftest crosstab-table-test
  (let [html (freq/crosstab-table en crosstab)]
    (testing "it scrolls inside its own region"
      (is (= :div.scroll (first html))))
    (testing "a column per value of the second attribute, headed by it"
      (is (some #{[:th {:scope "col"} [:time "2023"]]} (deep html)))
      (is (some #{[:th {:scope "col"} "total"]} (deep html))))
    (testing "the tokens of each column head the rows"
      (is (some #{[:th {:scope "row"} "tokens"]} (deep html)))
      (is (some #{[:td.n "20"]} (deep html))))
    (testing "a cell is the count with its rate per million of the column"
      (is (some #{[:td.n "3" " (150,000.0)"]} (deep html)))
      (is (some #{[:td.n "5" " (106,383.0)"]} (deep html))))
    (testing "the value links to its hits"
      (is (some #{[:th {:scope "row"} [:a {:href "/?subset=hund"} "hund"]]}
                (deep html))))
    (testing "unsized, a cell is the count alone and there is no tokens row"
      (let [html (deep (freq/crosstab-table en (assoc crosstab :sized false)))]
        (is (some #{[:td.n "3" nil]} html))
        (is (not (some #{[:th {:scope "row"} "tokens"]} html)))))))

(deftest crosstab-caption-test
  (is (= "by lemma at the start of the match and text_year"
         (text (freq/grouping-phrase en crosstab))))
  (is (re-find #"^Frequencies · 1 value · 2 columns"
               (text (freq/table-caption en crosstab))))
  (testing "cut columns say so"
    (is (re-find #"3 columns, the 2 most frequent shown"
                 (text (freq/table-caption en (assoc crosstab
                                                     :column-count 3))))))
  (testing "the section shows the cross-tabulation, without a text count"
    (let [html (deep (freq/frequency-section
                      {:lang      "en"
                       :result    crosstab
                       :view      :frequencies
                       :attrs     [{:type :structural :name :text_year}]
                       :positions ["match"]
                       :params    {:attr "lemma"}}))]
      (is (some #{:table.frequencies.crosstab} html))
      (is (not (some #(and (map? %) (= "docs" (:name %))) html))))))

(deftest sized-table-test
  (let [result (assoc counted
                      :attr  :text_year
                      :sized true
                      :rows  [{:value  "2023" :freqs {"PROBE" 3} :total 3
                               :tokens {"PROBE" 20}}])
        html   (deep (freq/frequency-table en result))]
    (testing "a sized table has a tokens column per corpus and rates against it"
      (is (some #{[:th {:scope "col"} "tokens"]} html))
      (is (some #{[:td.n "20"]} html))
      (is (some #{[:td.n "150,000.0"]} html))
      (is (some #{[:colgroup {:span 3}]} html)))))
