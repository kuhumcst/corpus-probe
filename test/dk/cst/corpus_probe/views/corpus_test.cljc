(ns dk.cst.corpus-probe.views.corpus-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.views.hiccup :refer [deep]]
            [dk.cst.corpus-probe.views.corpus :as corpus]))

(deftest group-digits-test
  (is (= "0" (corpus/group-digits 0)))
  (is (= "999" (corpus/group-digits 999)))
  (is (= "1,000" (corpus/group-digits 1000)))
  (is (= "64,600,000" (corpus/group-digits 64600000))))

(deftest corpus-item-test
  (testing "a titled corpus links its title and shows its ID"
    (let [item (corpus/corpus-item {:id "VISER" :title "Folkeviser" :size 48})]
      (is (= [:a {:href "/corpus/viser"} "Folkeviser"] (second item)))
      (is (some #{[:code "VISER"]} (deep item)))))
  (testing "an untitled corpus links its ID"
    (is (= [:a {:href "/corpus/probe"} "PROBE"]
           (second (corpus/corpus-item {:id "PROBE" :size 47})))))
  (testing "an unreadable corpus is marked unavailable"
    (is (some #{[:em "unavailable"]}
              (deep (corpus/corpus-item {:id "GONE" :size nil}))))))

(deftest chooser-item-test
  (let [checkbox (fn [selected m]
                   (second (get-in (corpus/chooser-item selected m) [1 1])))]
    (testing "a corpus is a checkbox named corpus, checked when selected"
      (is (= {:type "checkbox" :name "corpus" :value "VISER"
              :checked true :disabled false}
             (checkbox #{"VISER"} {:id "VISER" :size 48})))
      (is (not (:checked (checkbox #{} {:id "VISER" :size 48})))))
    (testing "an unreadable corpus is disabled"
      (is (:disabled (checkbox #{} {:id "GONE" :size nil}))))))

(deftest folder-view-test
  (let [litteratur {:label   "Litteratur"
                    :corpora [{:id "VISER" :size 48}]
                    :folders []}]
    (testing "a labelled folder is a disclosure, open as told"
      (let [[tag attrs summary] (corpus/folder-view corpus/corpus-item
                                                    (constantly true)
                                                    litteratur)]
        (is (= :details tag))
        (is (:open attrs))
        (is (= [:summary "Litteratur"] summary)))
      (is (not (:open (second (corpus/folder-view corpus/corpus-item
                                                  (constantly false)
                                                  litteratur))))))
    (testing "the label-less tail folder is a bare list"
      (is (= :ul (first (corpus/folder-view corpus/corpus-item
                                            (constantly true)
                                            {:label   nil
                                             :corpora [{:id "PROBE" :size 1}]
                                             :folders []})))))))

(deftest chooser-test
  (let [folders [{:label   "Litteratur"
                  :corpora []
                  :folders [{:label   "Folkeviser"
                             :corpora [{:id "VISER" :size 48}]
                             :folders []}]}
                 {:label "Folketinget" :corpora [{:id "TALER" :size 42}]
                  :folders []}]
        open    (fn [selected]
                  (->> (deep (corpus/chooser folders selected))
                       (filter #(and (map? %) (contains? % :open)))
                       (map :open)))]
    (testing "folders holding a selected corpus start open, others closed"
      (is (= [true true false] (open #{"VISER"})))
      (is (= [false false false] (open #{}))))))

(deftest labelled-folders-test
  (testing "a lone label-less folder stays as it is"
    (is (= [{:label nil}] (corpus/labelled-folders [{:label nil}]))))
  (testing "among labelled siblings the tail is labelled Other"
    (is (= ["A" "Other"]
           (map :label
                (corpus/labelled-folders [{:label "A"} {:label nil}]))))))

(deftest count-cell-test
  (is (= [:td.n "1,000"] (corpus/count-cell 1000)))
  (testing "a missing count is the tool's own NO DATA"
    (is (= [:td.n [:em "no data"]] (corpus/count-cell nil)))))

(deftest info-view-test
  (let [data {:corpus "VISER"
              :title  "Folkeviser"
              :lang   "da"
              :stats  {:size    48
                       :charset "utf8"
                       :p-attrs [{:name :word :tokens 48 :types 38}
                                 {:name :lemma}]
                       :s-attrs [{:name :text_title :regions 2 :values? true}]
                       :a-attrs []}
              :info   {:properties {:language "da" :charset "utf8"}
                       :info       "Om korpusset."}}
        html (pr-str (corpus/info-view data))]
    (testing "the title is the registry name, the ID its subtitle"
      (is (re-find #":h2 .*\"Folkeviser\"" html))
      (is (re-find #":code \"VISER\"" html)))
    (testing "the corpus's language marks the title and the .info text only"
      (is (re-find #"\[:h2 \{:lang \"da\"\} \"Folkeviser\"\]" html))
      (is (re-find #"\[:pre \{:lang \"da\"\} \"Om korpusset.\"\]" html))
      (is (not (re-find #":article.corpus-info \{" html))))
    (testing "the duplicate charset property is not repeated in the facts"
      (is (= 1 (count (re-seq #"\"utf8\"" html)))))
    (testing "an attribute without data is marked as such"
      (is (re-find #"no data" html)))
    (testing "an error becomes a fixed alert leaking nothing of its message"
      (let [html (pr-str (corpus/info-view {:corpus "GONE"
                                            :error  {:message "/srv/secret"}}))]
        (is (re-find #"Could not read corpus" html))
        (is (not (re-find #"/srv/secret" html)))
        (is (not (re-find #":table" html)))))))
