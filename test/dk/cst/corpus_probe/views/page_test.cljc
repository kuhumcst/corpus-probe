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
        html  (page/search-form state "/"
                                (page/sort-control "en"
                                                   [["corpus" :sort-corpus]
                                                    ["word" :sort-word]]
                                                   "word"))]
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
    (testing "the sort control offers the given modes"
      (is (some #{"corpus order"} (deep html)))
      (is (some #{"match"} (deep html))))
    (testing "the language travels with the search as a hidden field"
      (is (some #{{:type "hidden" :name "lang" :value "en"}} (deep html))))
    (testing "the same form in Danish"
      (let [da (deep (page/search-form
                      (assoc state :lang "da") "/"
                      (page/sort-control "da" [["corpus" :sort-corpus]]
                                         "corpus")))]
        (is (some #{"Søg"} da))
        (is (some #{"Forespørgsel"} da))
        (is (some #{"korpusrækkefølge"} da))
        (is (some #{{:type "hidden" :name "lang" :value "da"}} da))))))

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
    (is (nil? (page/filter-fieldset "en" nil)))
    (is (nil? (page/filter-fieldset "en" {:attrs    []
                                          :unlisted []
                                          :selected {}}))))
  (let [html (page/filter-fieldset
              "en"
              {:attrs    [{:name :text_year
                           :rows [{:value "1583" :total 1}
                                  {:value "1591" :total 2}]}
                          {:name :text_party
                           :rows [{:value "S" :total 2}]}]
               :unlisted [:text_title]
               :selected {:text_year  #{"1591" "1600"}
                          :text_title #{"Havfruens sang"}}})
        inputs (filter #(and (map? %) (= "checkbox" (:type %))) (deep html))]
    (testing "each value is a checkbox under the attribute's filter param"
      (is (= ["f.text_year" "f.text_year" "f.text_year" "f.text_party"
              "f.text_title"]
             (map :name inputs)))
      (is (= ["1583" "1591" "1600" "S" "Havfruens sang"] (map :value inputs))))
    (testing "chosen values are checked, whether the corpora offer them or not"
      (is (= [false true true false true] (map :checked inputs))))
    (testing "an attribute with a selection starts open and counts it"
      (is (= [true false true]
             (keep #(when (and (map? %) (contains? % :open)) (:open %))
                   (deep html))))
      (is (some #{" · 2 selected"} (deep html))))
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
      (is (some #{:ul.pager} (deep html))))))

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
  (let [html (page/result-section {:lang         "en"
                                   :result       sample-result
                                   :next-href    "/?page=1"
                                   :freq-href    "/frequencies?q=x"
                                   :export-hrefs {:tsv "/e?format=tsv"}
                                   :export-limit 5})]
    (testing "the frequencies of the same hits are linked"
      (is (some #{"/frequencies?q=x"} (deep html))))
    (testing "a cut export is announced"
      (is (some #{" the first 5 hits"} (deep html))))
    (testing "the region names itself and can be landed on"
      (is (= {:id              "results"
              :tabindex        "-1"
              :aria-labelledby "results-heading"}
             (second html))))
    (testing "its heading is the summary the caption used to carry"
      (is (some #{[:h2 {:id "results-heading"}
                   "6 hits in 2 corpora · page 1 of 1"]}
                (deep html))))
    (testing "errors are headed sections before the counts and concordance"
      (is (some #{[:h3 "CQP error"]} (deep html)))
      (is (some #{[:caption "Hits per corpus"]} (deep html)))
      (is (some #{:table.kwic} (deep html))))
    (testing "the page links are rendered above the table as well as below"
      (is (= 2 (count (filter #(and (vector? %) (= :ul.pager (first %)))
                              (deep html))))))
    (testing "but only the first is a landmark, since both would share a name"
      (is (= 1 (count (filter #(and (vector? %) (= :nav.pagination (first %)))
                              (deep html))))))
    (testing "the errors come before the counts and the concordance"
      (let [order (fn [x] (.indexOf (vec (deep html)) x))]
        (is (< (order [:h3 "CQP error"]) (order :table.counts)))
        (is (< (order [:h3 "CQP error"]) (order :table.kwic)))))
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
                 :freq-href    "/frequencies?q=x"
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
  (testing "the aside is always present as an auto popover"
    (let [html (page/sidebar "en" nil)]
      (is (= :aside.sidebar (first html)))
      (is (= "auto" (:popover (second html))))
      (testing "with no content when nothing is selected"
        (is (nil? (last html))))))
  (testing "a selection lists the token's own attributes and its hit's structs"
    (let [html (page/sidebar "en"
                             {:token   {:word "hund" :pos "NCSI" :open [:s]}
                              :structs {:text_title "Hverdag"}
                              :corpus  "PROBE"})]
      (is (some #{"Token"} (deep html)))
      (is (some #{"Hverdag"} (deep html)))
      (testing "the corpus links to its info page"
        (is (some #{[:a {:href "/corpus/probe?lang=en"} [:code "PROBE"]]}
                  (deep html))))
      (testing "structure tags are not shown as attributes"
        (is (not (some #{"open"} (deep html))))))))

(deftest app-view-test
  (testing "the page names itself and the bypass link can reach it"
    (let [html (page/app-view {:lang "en" :folders [] :params {}})]
      (is (= layout/main-attrs (second html)))
      (is (some #{[:h1 "Search"]} (deep html)))))
  (testing "the form submits to the results, so a search lands on its answer"
    (let [html (page/app-view {:lang "en" :folders [] :params {}})]
      (is (= (str "/" page/results-fragment)
             (get-in html [3 1 1 :action])))))
  (testing "an error is shown as the outcome of the search"
    (let [html (page/app-view {:lang "en" :folders [] :params {}
                               :error {:type :cqp :message "boom"}})]
      (is (some #{"boom"} (deep html)))
      (is (some #{"results"} (deep html)))))
  (testing "no query renders no results region at all"
    (let [html (page/app-view {:lang "en" :folders [] :params {}})]
      (is (not (some #{"results"} (deep html)))))))
