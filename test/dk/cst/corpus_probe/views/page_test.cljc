(ns dk.cst.corpus-probe.views.page-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.views.hiccup :refer [deep]]
            [dk.cst.corpus-probe.views.layout :as layout]
            [dk.cst.corpus-probe.views.page :as page]))

(deftest search-form-test
  (let [state {:lang    "en"
               :folders [{:label nil :folders []
                          :corpora [{:id "PROBE" :size 47}]}]
               :params  {:corpus ["PROBE"] :q "hund" :sort "word"}}
        html  (page/search-form state "/" nil)]
    (testing "the form is wrapped in a <search> landmark"
      (is (= :search (first html))))
    (testing "the form submits to the given action"
      (is (= "/" (get-in html [1 1 :action]))))
    (testing "the selected corpus is checked in the chooser"
      (is (some #(and (map? %) (= "corpus" (:name %)) (:checked %))
                (deep html))))
    (testing "the query field is a search input"
      (is (some #{"search"} (deep html))))
    (testing "grouped controls have legends"
      (is (some #{:legend} (deep html))))
    (testing "the example is the one for the mode being searched in"
      (is (some #(and (map? %) (= "q" (:id %))
                      (= "hund, or several words in order" (:placeholder %)))
                (deep html))))
    (testing "the selection the reader made is marked as one they made"
      (is (some #{{:type "hidden" :name "scope" :value "chosen"}}
                (deep html))))
    (testing "the sort is not here: it orders a result, it does not ask one"
      (is (not (some #{"sort"} (deep html)))))
    (testing "no language travels with the search: it is not part of one"
      (is (not (some #(and (map? %) (= "lang" (:name %))) (deep html)))))
    (testing "extra hidden inputs ride along with the search"
      (is (some #{{:type "hidden" :name "attr" :value "word"}}
                (deep (page/search-form
                       state "/" [:input {:type  "hidden" :name "attr"
                                          :value "word"}])))))
    (testing "the query options are one group, under the query field"
      (let [order (fn [x] (.indexOf (vec (deep html)) x))]
        (is (< (order :input) (order :fieldset.query-options)))
        (is (< (order :fieldset.query-options) (order :fieldset.corpora)))
        ;; the two boxes said the same thing twice; the groups they named
        ;; are still named, without a box each
        (is (not (some #{:fieldset.mode :fieldset.options} (deep html))))
        (is (some #{{:role "radiogroup" :aria-label "Query mode"}}
                  (deep html)))
        (is (some #{{:role "group" :aria-label "Simple-search options"}}
                  (deep html)))))
    (testing "and the options are live only for a query they can qualify"
      (let [disabled (fn [mode]
                       (->> (deep (page/search-form
                                   (assoc-in state [:params :mode] mode)
                                   "/" nil))
                            (filter #(and (map? %) (= "checkbox" (:type %))
                                          (#{"ci" "prefix" "suffix"} (:name %))))
                            (map :disabled)))]
        (is (= [false false false] (disabled "simple")))
        (is (= [true true true] (disabled "cqp")))))
    (testing "a disabled fieldset submits nothing, so no simple option
              rides along with a CQP query"
      ;; the checkboxes are still there and still ticked, so looking at
      ;; CQP and coming back does not lose what the reader had chosen
      (let [html (deep (page/search-form
                        (-> state
                            (assoc-in [:params :mode] "cqp")
                            (assoc-in [:params :ci] "on"))
                        "/" nil))]
        (is (some #(and (map? %) (= "ci" (:name %)) (:checked %)
                        (:disabled %))
                  html))))
    (testing "no status region without a client to put anything in it"
      (is (not (some #{:div.status} (deep html)))))
    (testing "with one it follows the form, inside the same landmark"
      (let [live (page/search-form (assoc state :client? true :pending? true)
                                   "/" nil)]
        (is (= :div.status (first (last live))))
        (is (some #{"Loading …"} (deep live)))))
    (testing "the same form in Danish"
      (let [da (deep (page/search-form (assoc state :lang "da") "/" nil))]
        (is (some #{"Søg"} da))
        (is (some #{"Forespørgsel"} da))
        (is (not (some #(and (map? %) (= "lang" (:name %))) da)))))))

(deftest navigation-status-test
  (testing "the region is rendered before it has anything to announce"
    (is (= [:div.status {:role "status"} nil]
           (page/navigation-status "en" false))))
  (testing "and reports a navigation in flight in either language"
    (is (some #{"Loading …"} (deep (page/navigation-status "en" true))))
    (is (some #{"Henter …"} (deep (page/navigation-status "da" true))))))

(deftest view-controls-test
  (let [sort* (fn [lang] (page/sort-control lang [["word" :sort-word]] "word"))]
    (testing "no controls, nothing rendered"
      (is (nil? (page/view-controls "en" false nil))))
    (testing "a control that acts on a result submits the form that made it"
      (let [html (page/view-controls "en" false (sort* "en"))]
        (is (some #(and (map? %) (= page/form-id (:form %))) (deep html)))))
    (testing "without a client, a button is what applies it"
      (is (some #{"Apply"} (deep (page/view-controls "en" false (sort* "en")))))
      (is (some #{"Anvend"} (deep (page/view-controls "da" false
                                                      (sort* "da"))))))
    (testing "with one, choosing an order is asking for it: no button"
      (let [html (page/view-controls "en" true (sort* "en"))]
        (is (not (some #{"Apply"} (deep html))))
        (is (not (some #(and (vector? %) (= :button (first %))) (deep html))))))
    (testing "and the control itself is what applies it"
      (is (some #(and (map? %) (= [:apply-view] (get-in % [:on :change])))
                (deep (sort* "en")))))))

(deftest query-mode-test
  (let [radios (fn [params]
                 (->> (deep (page/search-form
                             {:lang "en" :folders [] :params params} "/" nil))
                      (filter #(and (map? %) (= "mode" (:name %))))))]
    (testing "Simple comes first, since it is what most searches want"
      (is (= ["simple" "cqp"] (map :value (radios {})))))
    (testing "and it is the default, so a bare word is not a parse error"
      (is (= [true false] (map (comp boolean :checked) (radios {}))))
      (is (= [true false] (map (comp boolean :checked)
                               (radios {:mode "simple"})))))
    (testing "CQP mode is opt-in and stays selected once chosen"
      (is (= [false true] (map (comp boolean :checked)
                               (radios {:mode "cqp"})))))
    (testing "each radio dispatches the mode it selects, so the client can
              swap the example without a round trip"
      (is (= [[:set-mode "simple"] [:set-mode "cqp"]]
             (map (comp :change :on) (radios {})))))))

(deftest query-example-test
  (testing "each mode gets the example for the input it takes"
    (is (= :query-example-simple (page/query-example nil)))
    (is (= :query-example-simple (page/query-example "simple")))
    (is (= :query-example-cqp (page/query-example "cqp"))))
  (testing "the placeholder follows the mode"
    (let [ph (fn [mode]
               (->> (deep (page/search-form
                           {:lang "en" :folders [] :params {:mode mode}}
                           "/" nil))
                    (some #(when (and (map? %) (= "q" (:id %)))
                             (:placeholder %)))))]
      (is (= "hund, or several words in order" (ph nil)))
      (is (= "[lemma = \"hund\"] or [pos = \"N.*\"]" (ph "cqp")))
      (is (= "[lemma = \"hund\"] eller [pos = \"N.*\"]"
             (->> (deep (page/search-form
                         {:lang "da" :folders [] :params {:mode "cqp"}}
                         "/" nil))
                  (some #(when (and (map? %) (= "q" (:id %)))
                           (:placeholder %)))))))))

(deftest filter-phrase-test
  (is (= "" (page/filter-phrase {})))
  (is (= "text_author ukendt; text_year 1583, 1591"
         (page/filter-phrase {:text_year   #{"1591" "1583"}
                              :text_author #{"ukendt"}})))
  (is (nil? (page/within-phrase "en" nil)))
  (is (= " within text_year 1591"
         (page/within-phrase "en" {:text_year #{"1591"}})))
  (is (= " inden for text_year 1591"
         (page/within-phrase "da" {:text_year #{"1591"}}))))

(deftest filter-fieldset-test
  (testing "no metadata renders nothing"
    (is (nil? (page/filter-fieldset "en" nil {})))
    (is (nil? (page/filter-fieldset "en" {:attrs    []
                                          :unlisted []
                                          :selected {}}
                                    {}))))
  (let [selected {:text_year  #{"1591" "1600"}
                  :text_title #{"Havfruens sang"}}
        html (page/filter-fieldset
              "en"
              {:attrs    [{:name :text_year
                           :rows [{:value "1583" :total 1}
                                  {:value "1591" :total 2}]}
                          {:name :text_party
                           :rows [{:value "S" :total 2}]}]
               :unlisted [:text_title]
               :selected selected}
              {:served selected})
        inputs (filter #(and (map? %) (= "checkbox" (:type %))) (deep html))]
    (testing "each value is a checkbox under the attribute's filter param"
      (is (= ["f.text_year" "f.text_year" "f.text_year" "f.text_party"
              "f.text_title"]
             (map :name inputs)))
      (is (= ["1583" "1591" "1600" "S" "Havfruens sang"] (map :value inputs))))
    (testing "chosen values are checked, whether the corpora offer them or not"
      (is (= [false true true false true] (map :checked inputs))))
    (testing "every value reports its change, so the count can be live"
      (is (every? #(= [:toggle-filter-values [(keyword (subs (:name %) 2))
                                              [(:value %)]]]
                      (get-in % [:on :change]))
                  inputs)))
    (testing "the count is of what the boxes say now"
      (is (some #{"3 selected"} (deep html))))
    (testing "but what is open is of what the page was served"
      ;; ticking a first value must not open the fieldset under the reader,
      ;; nor unticking the last one shut it
      (is (= [false] (->> (deep (page/filter-fieldset
                                 "en" {:attrs [] :unlisted []
                                       :selected {:text_year #{"1591"}}}
                                 {}))
                          (filter #(and (map? %) (contains? % :open)))
                          (map :open))))
      (is (= [true] (->> (deep (page/filter-fieldset
                                "en" {:attrs    [{:name :text_year :rows []}]
                                      :unlisted [] :selected {}}
                                {:served {:text_year #{"1591"}}}))
                         (filter #(and (map? %) (contains? % :open)))
                         (map :open)))))
    (testing "the attributes not on offer are a caveat, so small print"
      (is (some #(and (vector? %) (= :small (first %))) (deep html)))
      (is (some #{[:code "text_title"]} (deep html))))
    (testing "the reader owns the disclosure once they have touched it"
      (let [open* (fn [opts] (->> (deep (page/filter-fieldset
                                         "en" {:attrs [{:name :a :rows []}]
                                               :unlisted [] :selected {}}
                                         opts))
                                  (filter #(and (map? %) (contains? % :open)))
                                  first :open))]
        (is (false? (open* {})))
        (is (true? (open* {:served {:a #{"1"}}})))
        (is (true? (open* {:open? true})))
        ;; and a reader who shut it is not overruled by what was served
        (is (false? (open* {:open? false :served {:a #{"1"}}})))))
    (testing "it is marked busy while its attributes are being fetched"
      (let [busy (fn [opts] (->> (deep (page/filter-fieldset
                                        "en" {:attrs [{:name :a :rows []}]
                                              :unlisted [] :selected {}}
                                        opts))
                                 (filter #(and (map? %) (contains? % :open)))
                                 first :aria-busy))]
        (is (= "true" (busy {:pending? true})))
        (is (nil? (busy {})))))
    (testing "each attribute carries a control over the values it shows"
      (let [alls (->> (deep (page/filter-fieldset
                             "en"
                             {:attrs    [{:name :text_year
                                          :rows [{:value "1583"}
                                                 {:value "1591"}]}]
                              :unlisted [] :selected {}}
                             {:client? true}))
                      (filter #(and (map? %) (contains? % :replicant/on-render))))]
        ;; the fieldset's own control comes first, then one per attribute
        (is (= ["Clear filter" "All values of text_year"]
               (map :aria-label alls)))
        (is (= [[:clear-filter] [:toggle-filter-values [:text_year ["1583" "1591"]]]]
               (map #(get-in % [:on :change]) alls)))))
    (testing "and takes only the values the filter box leaves showing"
      (let [alls (->> (deep (page/filter-fieldset
                             "en"
                             {:attrs    [{:name :text_year
                                          :rows [{:value "1583"}
                                                 {:value "1591"}]}]
                              :unlisted [] :selected {}}
                             {:client? true :filter "1591"}))
                      (filter #(and (map? %) (contains? % :replicant/on-render))))]
        (is (= [[:clear-filter] [:toggle-filter-values [:text_year ["1591"]]]]
               (map #(get-in % [:on :change]) alls))))
      ;; and the value it hid keeps its box, so the filter is not narrowed
      (let [html (deep (page/filter-fieldset
                        "en" {:attrs    [{:name :text_year
                                          :rows [{:value "1583"}
                                                 {:value "1591"}]}]
                              :unlisted [] :selected {:text_year #{"1583"}}}
                        {:client? true :filter "1591"}))]
        (is (some #(and (map? %) (= "1583" (:value %)) (:checked %)) html))
        (is (some #{{:hidden true}} html))))
    (testing "the region saying nothing was found is there before it says it"
      (let [region (fn [opts]
                     (->> (page/filter-fieldset
                           "en" {:attrs    [{:name :text_year
                                             :rows [{:value "1591"}]}]
                                 :unlisted [] :selected {}}
                           (merge {:client? true} opts))
                          (tree-seq coll? seq)
                          (filter #(and (vector? %) (= :div.empty (first %))))
                          first))]
        (is (= [:div.empty {:role "status"} nil] (region {})))
        (is (= [:div.empty {:role "status"} nil] (region {:filter "1591"})))
        (is (= [:div.empty {:role "status"} "No values found."]
               (region {:filter "zzz"})))))
    (testing "the filter box is there only with a client, and submits nothing"
      (let [box (fn [opts] (->> (deep (page/filter-fieldset
                                       "en" {:attrs [{:name :a :rows []}]
                                             :unlisted [] :selected {}}
                                       opts))
                                (filter #(and (map? %)
                                              (= "value-filter" (:id %))))
                                first))]
        (is (nil? (box {})))
        (is (= "search" (:type (box {:client? true}))))
        ;; and its own label, not the corpus filter's
        (is (some #{[:label {:for "value-filter"} "Filter values"]}
                  (deep (page/filter-fieldset
                         "en" {:attrs [{:name :a :rows []}]
                               :unlisted [] :selected {}}
                         {:client? true}))))
        (is (nil? (:name (box {:client? true}))))))
    (testing "the fieldset's own control is the chooser's, minus one half"
      (let [root (fn [selected]
                   (->> (deep (page/filter-fieldset
                               "en" {:attrs    [{:name :a :rows [{:value "1"}
                                                                 {:value "2"}]}]
                                     :unlisted [] :selected selected}
                               {:client? true}))
                        (filter #(and (map? %) (= "Clear filter" (:aria-label %))))
                        first))]
        (testing "nothing chosen: offered, but not from the one state it
                  must not act from"
          (is (true? (:disabled (root {}))))
          (is (false? (:checked (root {}))))
          (is (= [:set-indeterminate false]
                 (:replicant/on-render (root {})))))
        (testing "something chosen: live, partly checked, and it clears"
          (is (false? (:disabled (root {:a #{"1"}}))))
          (is (= [:set-indeterminate true]
                 (:replicant/on-render (root {:a #{"1"}}))))
          (is (= [:clear-filter] (get-in (root {:a #{"1"}}) [:on :change]))))
        (testing "everything chosen: checked, and it still only clears"
          (is (true? (:checked (root {:a #{"1" "2"}}))))
          (is (false? (:disabled (root {:a #{"1" "2"}})))))))
    (testing "and it answers for the whole filter, not the part on show"
      ;; emptying by halves would leave a constraint the box is hiding
      (let [root (->> (deep (page/filter-fieldset
                             "en" {:attrs    [{:name :a :rows [{:value "1"}]}
                                              {:name :b :rows [{:value "2"}]}]
                                   :unlisted [] :selected {:b #{"2"}}}
                             {:client? true :filter "a"}))
                      (filter #(and (map? %) (= "Clear filter" (:aria-label %))))
                      first)]
        (is (false? (:disabled root)))
        (is (= [:set-indeterminate true] (:replicant/on-render root)))))
    (testing "one disclosure over the filter, open only while it is active"
      (is (= [true]
             (keep #(when (and (map? %) (contains? % :open)) (:open %))
                   (deep html)))))
    (testing "an attribute counts its selection without reopening itself"
      (is (some #{" · 2 selected"} (deep html))))
    (testing "the whole filter counts what is chosen across attributes"
      (is (some #{"3 selected"} (deep html))))
    (testing "the region counts are machine-readable, with their unit"
      (is (some #{[:data {:value "2"} "2 regions"]} (deep html)))
      (is (some #{[:data {:value "1"} "1 region"]} (deep html))))
    (testing "values render as the sidebar shows them"
      (is (some #{[:time "1591"]} (deep html))))
    (testing "unlisted attributes are named"
      (is (some #{[:code "text_title"]} (deep html))))))

(deftest page-phrase-test
  (is (= "page 3 of 6" (page/page-phrase "en" {:page 2 :pages 6})))
  (is (= "side 3 af 6" (page/page-phrase "da" {:page 2 :pages 6}))))

(deftest results-fragment-test
  (testing "the fragment names the region the search lands on"
    (is (= (str "#" page/results-id) page/results-fragment))))

(deftest pager-links-test
  (testing "no links renders nothing"
    (is (nil? (page/pager-links "en" nil nil "page 1 of 1"))))
  (testing "links carry the rel values browsers use for a sequence"
    (let [html (page/pager-links "en" "/?page=0" "/?page=2" "page 2 of 3")]
      (is (some #{"prev"} (deep html)))
      (is (some #{"next"} (deep html)))
      (is (some #{"next →"} (deep html)))))
  (testing "the position rides between the two directions"
    (let [html (page/pager-links "en" "/?page=0" "/?page=2" "page 2 of 3")]
      (is (= [:li "page 2 of 3"]
             (second (filter #(and (vector? %) (= :li (first %)))
                             (deep html)))))))
  (testing "a direction that is out of range is left out, not held open"
    (let [html (page/pager-links "en" nil "/?page=1" "page 1 of 3")]
      (is (= 2 (count (filter #(and (vector? %) (= :li (first %)))
                              (deep html)))))
      (is (not (some #{"prev"} (deep html))))))
  (testing "in Danish"
    (let [html (page/pager-links "da" "/?page=0" "/?page=2" "side 2 af 3")]
      (is (some #{"← forrige"} (deep html)))
      (is (some #{"næste →"} (deep html))))))

(deftest pagination-test
  (testing "nothing to page is no landmark at all"
    (is (nil? (page/pagination "en" nil nil "page 1 of 1"))))
  (testing "the links are wrapped in a navigation landmark, named"
    (let [html (page/pagination "en" "/?page=0" "/?page=2" "page 2 of 3")]
      (is (= :nav.pagination (first html)))
      (is (= "Pagination" (:aria-label (second html))))
      (is (= "Sidenavigation"
             (:aria-label (second (page/pagination "da" "/?page=0" nil "x")))))
      (is (some #{:ul.row.pager} (deep html))))))

(deftest searched?-test
  (testing "a corpus that answered makes the counts an answer"
    (is (page/searched? {:counts [{:corpus "PROBE" :size 5}]}))
    (testing "including an answer of none"
      (is (page/searched? {:counts [{:corpus "PROBE" :size 0}]}))))
  (testing "a search every corpus refused is not an answer"
    (is (not (page/searched?
              {:counts [{:corpus "X" :error {:type :timeout}}]})))
    (is (not (page/searched? nil)))))

(deftest error-name-test
  (is (= "CQP error" (page/error-name "en" {:type :cqp})))
  (is (= "The query timed out" (page/error-name "en" {:type :timeout})))
  (is (= "Forespørgslen tog for lang tid"
         (page/error-name "da" {:type :timeout}))))

(deftest error-section-test
  (let [html (page/error-section "en" {:type :cqp :message "boom"} ["TALER"])]
    (testing "no live region: it is in the document before the page is parsed"
      (is (not (some #{"alert"} (deep html)))))
    (testing "it heads itself below the region's own h2"
      (is (some #{[:h3 "CQP error"]} (deep html))))
    (testing "cqp's message is the sample output of another program"
      (is (some #{[:samp "boom"]} (deep html))))
    (is (some #{"boom"} (deep html)))
    (testing "an error CQP itself reported is headed as such"
      (is (some #{"CQP error"} (deep html))))
    (testing "the corpora concerned are named"
      (is (some #{[:code "TALER"]} (deep html)))))
  (testing "no corpus selected is explained without a CQP message"
    (let [html (page/error-section "en" {:type :no-corpus} nil)]
      (is (some #{"No corpus selected"} (deep html)))
      (is (some #{"Select at least one corpus to search."} (deep html)))
      (is (not (some #{:pre} (deep html))))))
  (testing "our own rejections and internal failures are not CQP errors"
    (is (some #{"Request rejected"}
              (deep (page/error-section "en" {:type    :rejected
                                              :message "x"} nil))))
    (is (some #{"Unexpected error"}
              (deep (page/error-section "en" {:type :internal} nil))))
    (is (some #{"Unknown corpus"}
              (deep (page/error-section "en" {:type :unknown-corpus} ["X"])))))
  (testing "the headings and explanations are translated, CQP's message not"
    (let [html (page/error-section "da" {:type :no-corpus} nil)]
      (is (some #{"Intet korpus valgt"} (deep html)))
      (is (some #{"Vælg mindst ét korpus at søge i."} (deep html))))
    (let [html (page/error-section "da" {:type :cqp :message "boom"} ["X"])]
      (is (some #{"CQP-fejl"} (deep html)))
      (is (some #{"boom"} (deep html))))))

(def sample-result
  {:size 6 :page 0 :page-size 25 :pages 1
   :counts [{:corpus "PROBE" :size 5}
            {:corpus "VISER" :size 1}
            {:corpus "TALER" :error {:type :cqp :message "no lemma"}}
            {:corpus "GONE" :error {:type :cqp :message "no lemma"}}]
   :hits []})

(deftest error-groups-test
  (testing "identical errors are reported once, naming every corpus"
    (is (= [[{:type :cqp :message "no lemma"} ["TALER" "GONE"]]]
           (page/error-groups (:counts sample-result)))))
  (is (empty? (page/error-groups [{:corpus "PROBE" :size 1}]))))

(deftest hits-phrase-test
  (is (= "1 hit" (page/hits-phrase "en" 1)))
  (is (= "0 hits" (page/hits-phrase "en" 0)))
  (testing "Danish has one form for both, with its own digit grouping"
    (is (= "1 træf" (page/hits-phrase "da" 1)))
    (is (= "1.000 træf" (page/hits-phrase "da" 1000)))))

(deftest result-summary-test
  (testing "only the corpora that could be searched are counted"
    (is (= "6 hits in 2 corpora · page 1 of 1"
           (page/result-summary "en" sample-result))))
  (is (= "5 hits in PROBE · page 1 of 1"
         (page/result-summary "en" {:size 5 :page 0 :pages 1
                                    :counts [{:corpus "PROBE" :size 5}]})))
  (testing "a metadata filter qualifies the corpora"
    (is (= "5 hits in PROBE within text_year 1591 · page 1 of 1"
           (page/result-summary "en" {:size   5 :page 0 :pages 1
                                      :filter {:text_year #{"1591"}}
                                      :counts [{:corpus "PROBE"
                                                :size   5}]}))))
  (testing "the same summary in Danish, the attribute name untranslated"
    (is (= "5 træf i PROBE inden for text_year 1591 · side 1 af 1"
           (page/result-summary "da" {:size   5 :page 0 :pages 1
                                      :filter {:text_year #{"1591"}}
                                      :counts [{:corpus "PROBE"
                                                :size   5}]})))))

(deftest download-links-test
  (is (nil? (page/download-links "en" nil nil)))
  (let [html (page/download-links "en"
                                  {:tsv "/x?format=tsv" :csv "/x?format=csv"}
                                  "the first 10 hits")]
    (testing "one download link per format, in a fixed order"
      (is (= [[:a {:href "/x?format=csv"} "CSV"]
              [:a {:href "/x?format=tsv"} "TSV"]]
             (filter #(and (vector? %) (= :a (first %))) (deep html)))))
    (testing "the note qualifies the download"
      (is (some #{" the first 10 hits"} (deep html))))))

(deftest result-section-test
  (let [state {:lang         "en"
               :view         :kwic
               :view-hrefs   [[:kwic :concordance "/?v=k"]
                              [:frequencies :frequencies "/?v=f"]]
               :sort-modes   [["corpus" :sort-corpus]
                              ["word" :sort-word]]
               :result       sample-result
               :next-href    "/?page=1"
               :export-hrefs {:tsv "/e?format=tsv"}
               :export-limit 5}
        html  (page/result-section state)]
    (testing "what to do next with the hits follows them, not precedes them"
      (let [order (fn [x] (.indexOf (vec (deep html)) x))]
        (is (< (order :table.kwic) (order :p.downloads)))))
    (testing "the other view of the same hits is offered above them"
      (let [order (fn [x] (.indexOf (vec (deep html)) x))]
        (is (some #{:nav.views} (deep html)))
        (is (< (order :nav.views) (order :table.kwic)))))
    (testing "a cut export is announced"
      (is (some #{" the first 5 hits"} (deep html))))
    (testing "the region names itself and can be landed on"
      (is (= {:id              "results"
              :tabindex        "-1"
              :aria-labelledby "results-heading"}
             (second html))))
    (testing "and is busy while the answer to the next question is coming"
      (is (= "true" (:aria-busy (second (page/result-section
                                         (assoc state :pending? true)))))))
    (testing "its heading is the summary the caption used to carry"
      (is (some #{[:h2 {:id "results-heading"}
                   "6 hits in 2 corpora · page 1 of 1"]}
                (deep html))))
    (testing "errors are headed sections before the counts and concordance"
      (is (some #{[:h3 "CQP error"]} (deep html)))
      (is (some #{[:caption "Hits per corpus"]} (deep html)))
      (is (some #{:table.kwic} (deep html))))
    (testing "the sort travels with the result, not with the query form"
      (is (some #{"corpus order"} (deep html)))
      (is (some #(and (map? %) (= "sort" (:id %)) (= page/form-id (:form %)))
                (deep html))))
    (testing "the page links are rendered above the table as well as below"
      (is (= 2 (count (filter #(and (vector? %) (= :ul.row.pager (first %)))
                              (deep html))))))
    (testing "but only the first is a landmark, since both would share a name"
      (is (= 1 (count (filter #(and (vector? %) (= :nav.pagination (first %)))
                              (deep html))))))
    (testing "the errors come before the counts and the concordance"
      (let [order (fn [x] (.indexOf (vec (deep html)) x))]
        (is (< (order [:h3 "CQP error"]) (order :table.counts)))
        (is (< (order [:h3 "CQP error"]) (order :table.kwic)))))
    (testing "but the counts break the hits down, so they follow them"
      (let [order (fn [x] (.indexOf (vec (deep html)) x))]
        (is (< (order :table.kwic) (order :table.counts)))
        ;; and after the *last* page links, which belong against the foot
        ;; of the table they turn, not merely after the first
        (is (< (.lastIndexOf (vec (deep html)) :ul.row.pager)
               (order :table.counts)))
        (is (< (order :table.counts) (order :p.downloads)))))
    (testing "and they are an aside: about the hits rather than part of them"
      (is (some #(and (vector? %) (= :aside (first %))
                      (= :table.counts (first (second %))))
                (deep html))))
    (testing "an erroring corpus shows no count in the table"
      (is (some #{[:em "error"]} (deep html))))
    (testing "a single corpus gets no counts table"
      (let [html (page/result-section
                  {:lang   "en"
                   :result (assoc sample-result
                                  :counts [{:corpus "PROBE" :size 5}])})]
        (is (not (some #{[:caption "Hits per corpus"]} (deep html)))))))
  (testing "nothing searchable means only the errors, and the heading names one"
    (let [html (page/result-section
                {:lang   "en"
                 :result {:page   0 :pages 1 :hits []
                          :counts [{:corpus "X" :error {:type :timeout}}]}})]
      (is (not (some #{:table.kwic} (deep html))))
      (is (some #{[:h2 {:id "results-heading"} "The query timed out"]}
                (deep html)))))
  (testing "a search that failed outright is a results region too"
    (let [html (page/result-section {:lang  "en"
                                     :error {:type :no-corpus}})]
      (is (some #{[:h2 {:id "results-heading"} "No corpus selected"]}
                (deep html)))
      (is (some #{"Select at least one corpus to search."} (deep html)))))
  (testing "a search that found nothing offers no table and no downloads"
    (let [html (page/result-section
                {:lang         "en"
                 :result       {:size   0 :page 0 :pages 1 :hits []
                                :counts [{:corpus "PROBE" :size 0}]}
                 :export-hrefs {:tsv "/e?format=tsv"}})]
      (is (some #{"No hits."} (deep html)))
      (is (not (some #{:table.kwic} (deep html))))
      (is (not (some #{"/e?format=tsv"} (deep html))))
      (is (not (some #{"/frequencies?q=x"} (deep html)))))))

(deftest attribute-value-test
  (testing "a title is a cited work"
    (is (= [:cite "Hverdag"] (page/attribute-value :text_title "Hverdag"))))
  (testing "a four-digit year is a time"
    (is (= [:time "2023"] (page/attribute-value :text_year "2023"))))
  (testing "a non-year value under a _year key stays plain"
    (is (= "n/a" (page/attribute-value :text_year "n/a"))))
  (testing "anything else is plain text"
    (is (= "NCSI" (page/attribute-value :pos "NCSI")))))

(deftest sidebar-test
  (testing "nothing selected, no panel: it describes the cursor or nothing"
    (is (nil? (page/sidebar "en" nil))))
  (let [html (page/sidebar "en" {:token   {:word "hund" :pos "NCSI"}
                                 :structs {:text_title "Hverdag"}
                                 :corpus  "PROBE"})]
    (testing "it is a named complementary region, not a popover"
      (is (= :aside.sidebar (first html)))
      (is (= "Token details" (:aria-label (second html))))
      (is (not (contains? (second html) :popover))))
    (testing "it never takes focus, so the cursor can keep moving"
      (is (not (some #(and (map? %) (contains? % :autofocus)) (deep html)))))
    (testing "the token, its text and its corpus are named groups"
      (is (some #{[:h3 "Token"]} (deep html)))
      (is (some #{[:h3 "Text"]} (deep html)))
      (is (some #{[:h3 "Corpus"]} (deep html)))
      (is (some #{"NCSI"} (deep html)))
      (is (some #{"Hverdag"} (deep html))))
    (testing "closing is a plain button, since Escape is handled by the grid"
      (is (some #{[:button {:type "button" :on {:click [:close]}} "Close"]}
                (deep html))))))

