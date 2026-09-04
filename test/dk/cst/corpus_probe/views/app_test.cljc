(ns dk.cst.corpus-probe.views.app-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.views.hiccup :refer [deep]]
            [dk.cst.corpus-probe.views.layout :as layout]
            [dk.cst.corpus-probe.views.page :as page]
            [dk.cst.corpus-probe.views.app :as app-views]))

(def base
  "A search page with nothing searched for yet."
  {:lang "en" :folders [] :params {}})

(def views
  "The two views of one result, as api/view-hrefs builds them."
  [[:kwic :concordance "/?q=hund&view=kwic#results"]
   [:frequencies :frequencies "/?q=hund&view=frequencies#results"]])

(deftest search-view-test
  (testing "the page names itself and the bypass link can reach it"
    (let [html (app-views/search-view base)]
      (is (= layout/main-attrs (second html)))
      (is (some #{[:h1 "Search"]} (deep html)))))
  (testing "the form submits to the results, so a search lands on its answer"
    (is (= (str "/" page/results-fragment)
           (get-in (app-views/search-view base) [3 1 1 :action]))))
  (testing "no query renders no results region at all"
    (is (not (some #{"results"} (deep (app-views/search-view base))))))
  (testing "an error is shown as the outcome of the search"
    (let [html (app-views/search-view (assoc base :error {:type :cqp
                                                       :message "boom"}))]
      (is (some #{"boom"} (deep html)))
      (is (some #{"results"} (deep html))))))

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
          (is (some #{"/?q=hund&view=kwic#results"} (deep html)))
          (is (some #{"/?q=hund&view=frequencies#results"} (deep html))))))))
