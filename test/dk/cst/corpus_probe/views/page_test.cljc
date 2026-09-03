(ns dk.cst.corpus-probe.views.page-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.views.hiccup :refer [deep]]
            [dk.cst.corpus-probe.views.page :as page]))

(deftest search-form-test
  (let [html (page/search-form [{:label nil :folders []
                                 :corpora [{:id "PROBE" :size 47}]}]
                               {:corpus ["PROBE"] :q "hund" :sort "word"}
                               "/"
                               (page/sort-control [["corpus" "corpus order"]
                                                   ["word" "match"]]
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
      (is (some #{"match"} (deep html))))))

(deftest pagination-test
  (testing "no links renders nothing"
    (is (nil? (page/pagination nil nil))))
  (testing "links carry rel and the nav is labelled"
    (let [html (page/pagination "/?page=0" "/?page=2")]
      (is (= "Pagination" (:aria-label (second html))))
      (is (some #{"prev"} (deep html)))
      (is (some #{"next"} (deep html))))))

(deftest error-section-test
  (let [html (page/error-section {:type :cqp :message "boom"} ["TALER"])]
    (is (= "alert" (:role (second html))))
    (is (some #{"boom"} (deep html)))
    (testing "the corpora concerned are named"
      (is (some #{[:code "TALER"]} (deep html)))))
  (testing "no corpus selected is explained without a CQP message"
    (let [html (page/error-section {:type :no-corpus} nil)]
      (is (some #{"No corpus selected"} (deep html)))
      (is (not (some #{:pre} (deep html))))))
  (testing "our own rejections and internal failures are not CQP errors"
    (is (some #{"Request rejected"}
              (deep (page/error-section {:type :rejected :message "x"} nil))))
    (is (some #{"Unexpected error"}
              (deep (page/error-section {:type :internal} nil))))
    (is (some #{"Unknown corpus"}
              (deep (page/error-section {:type :unknown-corpus} ["X"]))))))

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
  (is (= "1 hit" (page/hits-phrase 1)))
  (is (= "0 hits" (page/hits-phrase 0))))

(deftest result-summary-test
  (testing "only the corpora that could be searched are counted"
    (is (= "6 hits in 2 corpora · page 1 of 1"
           (page/result-summary sample-result))))
  (is (= "5 hits in PROBE · page 1 of 1"
         (page/result-summary {:size 5 :page 0 :pages 1
                               :counts [{:corpus "PROBE" :size 5}]}))))

(deftest download-links-test
  (is (nil? (page/download-links nil nil)))
  (let [html (page/download-links {:tsv "/x?format=tsv" :csv "/x?format=csv"}
                                  "the first 10 hits")]
    (testing "one download link per format, in a fixed order"
      (is (= [[:a {:href "/x?format=csv"} "CSV"]
              [:a {:href "/x?format=tsv"} "TSV"]]
             (filter #(and (vector? %) (= :a (first %))) (deep html)))))
    (testing "the note qualifies the download"
      (is (some #{" the first 10 hits"} (deep html))))))

(deftest result-section-test
  (let [html (page/result-section {:result       sample-result
                                   :next-href    "/?page=1"
                                   :freq-href    "/frequencies?q=x"
                                   :export-hrefs {:tsv "/e?format=tsv"}
                                   :export-limit 5})]
    (testing "the frequencies of the same hits are linked"
      (is (some #{"/frequencies?q=x"} (deep html))))
    (testing "a cut export is announced"
      (is (some #{" the first 5 hits"} (deep html))))
    (testing "errors come as alerts before the counts and concordance"
      (is (= "alert" (get-in (vec (nth html 2)) [0 1 :role])))
      (is (some #{[:caption "Hits per corpus"]} (deep html)))
      (is (some #{:table.kwic} (deep html))))
    (testing "an erroring corpus shows no count in the table"
      (is (some #{[:em "error"]} (deep html))))
    (testing "a single corpus gets no counts table"
      (let [html (page/result-section
                  {:result (assoc sample-result
                                  :counts [{:corpus "PROBE" :size 5}])})]
        (is (not (some #{[:caption "Hits per corpus"]} (deep html)))))))
  (testing "nothing searchable means only the alerts"
    (let [html (page/result-section
                {:result {:counts [{:corpus "X" :error {:type :timeout}}]
                          :hits   []}})]
      (is (not (some #{:table.kwic} (deep html)))))))

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
    (let [html (page/sidebar nil)]
      (is (= :aside.sidebar (first html)))
      (is (= "auto" (:popover (second html))))
      (testing "with no content when nothing is selected"
        (is (nil? (last html))))))
  (testing "a selection lists the token's own attributes and its hit's structs"
    (let [html (page/sidebar {:token   {:word "hund" :pos "NCSI" :open [:s]}
                              :structs {:text_title "Hverdag"}
                              :corpus  "PROBE"})]
      (is (some #{"Token"} (deep html)))
      (is (some #{"Hverdag"} (deep html)))
      (testing "the corpus links to its info page"
        (is (some #{[:a {:href "/corpus/probe"} [:code "PROBE"]]}
                  (deep html))))
      (testing "structure tags are not shown as attributes"
        (is (not (some #{"open"} (deep html))))))))

(deftest app-view-test
  (testing "an error hides the result and shows the CQP message"
    (let [html (page/app-view {:folders [] :params {}
                               :error {:type :cqp :message "boom"}})]
      (is (some #{"boom"} (flatten html)))))
  (testing "no query renders just the form"
    (is (nil? (nth (page/app-view {:folders [] :params {}}) 2)))))
