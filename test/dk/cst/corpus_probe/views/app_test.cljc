(ns dk.cst.corpus-probe.views.app-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.hiccup :refer [da deep en]]
            [dk.cst.corpus-probe.views.layout :as layout]
            [dk.cst.corpus-probe.views.app :as app-views]))

(def guide
  "A search guide, as api/search-page puts one in the data."
  [[:h2 {:id "query-help"} "Query help"] [:p "Type a word."]])

(def base
  "A search page with nothing searched for yet."
  {:ui en :folders [] :params {} :guide guide})

(def views
  "The two views of one result, as api/view-hrefs builds them."
  [[:kwic "/search?q=hund#results"]
   [:frequencies "/search?q=hund&view=frequencies#results"]])

(deftest search-view-test
  (testing "the page names itself and the bypass link can reach it"
    (let [html (app-views/search-view base)]
      (is (= layout/main-attrs (second html)))
      (is (some #{[:h1 "Search"]} (deep html)))))
  (testing "the form submits to the results, so a search lands on its answer"
    (is (= "/search#results"
           (get-in (app-views/search-view base) [3 1 1 :action])))
    (is (= (str url/search url/results-fragment)
           (get-in (app-views/search-view base) [3 1 1 :action]))))
  (testing "no query renders no results region at all, but the guide
            where the results will be"
    (is (not (some #{"results"} (deep (app-views/search-view base)))))
    (is (= :section.help (first (last (app-views/search-view base)))))
    (is (some #{"Type a word."} (deep (app-views/search-view base)))))
  (testing "and the guide gives way to an answer"
    (is (not (some #{:section.help}
                   (deep (app-views/search-view
                          (assoc base :error {:type :timeout})))))))
  (testing "a page without a guide simply has none"
    (is (nil? (last (app-views/search-view (dissoc base :guide))))))
  (testing "an error is shown as the outcome of the search"
    (let [html (app-views/search-view (assoc base :error {:type :cqp
                                                       :message "boom"}))]
      (is (some #{"boom"} (deep html)))
      (is (some #{"results"} (deep html))))))

(deftest search-view-carries-the-view-test
  (let [views (fn [v] (->> (deep (app-views/search-view (assoc base :view v)))
                           (filter #(and (map? %) (= "view" (:name %))))))]
    (testing "a form submitted from the frequency view answers in it"
      ;; without this a regrouped result comes back as a concordance
      (is (= [{:type "hidden" :name "view" :value "frequencies"}]
             (views :frequencies))))
    (testing "the concordance is the default, so it names nothing"
      (is (empty? (views :kwic))))))

(deftest result-view-test
  (let [result {:size 1 :page 0 :pages 1 :hits []
                :counts [{:corpus "PROBE" :size 1}]}
        state  (assoc base :result result :view-hrefs views)]
    (testing "the concordance is the default view of a result"
      (let [html (app-views/result-view state)]
        (is (some #{:table.kwic} (deep html)))
        (is (not (some #{:table.frequencies} (deep html))))))
    (testing "the same search counted rather than listed is the other view"
      (let [freq {:attr   :word
                  :query  "hund"
                  :counts [{:corpus "PROBE" :tokens 47 :size 1}]
                  :rows   [{:value "hund" :freqs {"PROBE" 1} :total 1}]}
            html (app-views/result-view (assoc state
                                            :view   :frequencies
                                            :result freq
                                            :attrs  [{:type :positional
                                                      :name :word}]))]
        (is (some #{:table.frequencies} (deep html)))
        (is (not (some #{:table.kwic} (deep html))))))
    (testing "both views are offered, whichever is being shown"
      (doseq [view [:kwic :frequencies]]
        (let [html (app-views/result-view (assoc state :view view))]
          (is (some #{"/search?q=hund#results"} (deep html)))
          (is (some #{"/search?q=hund&view=frequencies#results"}
                    (deep html))))))))

(deftest document-view-test
  (let [body [[:h1 {:id "corpus-search"} "Corpus search"] [:p "prose"]
              [:dl [:dt {:id "kwic"} "KWIC"] [:dd "key word " [:em "in"] " context"]]]
        html (app-views/page {:route :document :lang "en" :data {:body body}})]
    (testing "the document is the page's content, under its own heading"
      (is (= [:main.document layout/main-attrs body] html)))
    (testing "the page claims no heading of its own: the document names it"
      (is (= 1 (count (filter #{:h1} (deep html))))))
    (testing "the element the location's fragment names is marked"
      (let [marked (app-views/page {:route :document :lang "en"
                                    :data {:body body} :fragment "kwic"})]
        (is (some #{[:dt {:id "kwic"} [:mark "KWIC"]]} (deep marked)))
        (is (= 1 (count (filter #{:mark} (deep marked)))))))
    (testing "and nothing is, without a fragment or with one nothing carries"
      (is (not (some #{:mark} (deep html))))
      (is (not (some #{:mark}
                     (deep (app-views/page {:route :document :lang "en"
                                            :data {:body body}
                                            :fragment "nonesuch"}))))))))

(deftest mark-target-test
  (testing "the whole content of the element goes in the mark"
    (is (= [[:dt {:id "a"} [:mark "x " [:code "y"]]]]
           (app-views/mark-target "a" [[:dt {:id "a"} "x " [:code "y"]]])))))
