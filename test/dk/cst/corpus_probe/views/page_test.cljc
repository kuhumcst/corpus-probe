(ns dk.cst.corpus-probe.views.page-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.views.page :as page]))

(defn deep
  "All nodes of hiccup `form`, descending into attribute maps too, so tests
  can look for attribute values as well as tags and text."
  [form]
  (tree-seq coll? seq form))

(deftest search-form-test
  (let [html (page/search-form [{:id "probe"}]
                               [["corpus" "corpus order"] ["word" "match"]]
                               {:corpus "PROBE" :q "hund" :sort "word"})]
    (testing "the form is wrapped in a <search> landmark"
      (is (= :search (first html))))
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
  (let [html (page/error-section {:type :cqp :message "boom"})]
    (is (= "alert" (:role (second html))))
    (is (some #{"boom"} (deep html)))))

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
                              :structs {:text_title "Hverdag"}})]
      (is (some #{"Token"} (deep html)))
      (is (some #{"Hverdag"} (deep html)))
      (testing "structure tags are not shown as attributes"
        (is (not (some #{"open"} (deep html))))))))

(deftest app-view-test
  (testing "an error hides the result and shows the CQP message"
    (let [html (page/app-view {:corpora [] :params {}
                               :error {:type :cqp :message "boom"}})]
      (is (some #{"boom"} (flatten html)))))
  (testing "no query renders just the form"
    (is (nil? (nth (page/app-view {:corpora [] :params {}}) 3)))))
