(ns dk.cst.corpus-probe.views.corpus-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.views.hiccup :refer [deep]]
            [dk.cst.corpus-probe.views.corpus :as corpus]))

(deftest corpus-item-test
  (testing "a titled corpus links its title and shows its ID"
    (let [item (corpus/corpus-item "en" {:id    "VISER"
                                         :title "Folkeviser"
                                         :size  48})]
      (is (= [:a {:href "/corpus/viser?lang=en"} "Folkeviser"] (second item)))
      (is (some #{[:code "VISER"]} (deep item)))))
  (testing "an untitled corpus links its ID"
    (is (= [:a {:href "/corpus/probe?lang=en"} "PROBE"]
           (second (corpus/corpus-item "en" {:id "PROBE" :size 47})))))
  (testing "the info link keeps the language"
    (is (= [:a {:href "/corpus/probe?lang=da"} "PROBE"]
           (second (corpus/corpus-item "da" {:id "PROBE" :size 47})))))
  (testing "an unreadable corpus is marked unavailable in either language"
    (is (some #{[:em "unavailable"]}
              (deep (corpus/corpus-item "en" {:id "GONE" :size nil}))))
    (is (some #{[:em "utilgængelig"]}
              (deep (corpus/corpus-item "da" {:id "GONE" :size nil}))))))

(deftest token-count-test
  (testing "the digits are grouped as the language groups them"
    (is (= [:data.size {:value "64600000"} "64,600,000 tokens"]
           (corpus/token-count "en" 64600000)))
    (is (= [:data.size {:value "64600000"} "64.600.000 tokens"]
           (corpus/token-count "da" 64600000)))))

(deftest chooser-item-test
  (let [checkbox (fn [selected m]
                   (second (get-in (corpus/chooser-item "en" selected m)
                                   [1 1])))]
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
                    :folders []}
        item       (partial corpus/corpus-item "en")]
    (testing "a labelled folder is a disclosure, open as told"
      (let [[tag attrs summary] (corpus/folder-view item
                                                    (constantly true)
                                                    litteratur)]
        (is (= :details tag))
        (is (:open attrs))
        (is (= [:summary "Litteratur"] summary)))
      (is (not (:open (second (corpus/folder-view item
                                                  (constantly false)
                                                  litteratur))))))
    (testing "the label-less tail folder is a bare list"
      (is (= :ul (first (corpus/folder-view item
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
                  (->> (deep (corpus/chooser "en" folders selected))
                       (filter #(and (map? %) (contains? % :open)))
                       (map :open)))]
    (testing "folders holding a selected corpus start open, others closed"
      (is (= [true true false] (open #{"VISER"})))
      (is (= [false false false] (open #{}))))
    (testing "the legend is in the chosen language"
      (is (some #{[:legend "Korpusser"]}
                (deep (corpus/chooser "da" folders #{})))))))

(deftest labelled-folders-test
  (testing "a lone label-less folder stays as it is"
    (is (= [{:label nil}] (corpus/labelled-folders "en" [{:label nil}]))))
  (testing "among labelled siblings the tail is labelled in the language"
    (is (= ["A" "Other"]
           (map :label
                (corpus/labelled-folders "en" [{:label "A"} {:label nil}]))))
    (is (= ["A" "Andre"]
           (map :label
                (corpus/labelled-folders "da"
                                         [{:label "A"} {:label nil}]))))))

(deftest count-cell-test
  (is (= [:td.n "1,000"] (corpus/count-cell "en" 1000)))
  (is (= [:td.n "1.000"] (corpus/count-cell "da" 1000)))
  (testing "a missing count is the tool's own NO DATA"
    (is (= [:td.n [:em "no data"]] (corpus/count-cell "en" nil)))
    (is (= [:td.n [:em "ingen data"]] (corpus/count-cell "da" nil)))))

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
        html (pr-str (corpus/info-view "en" data))]
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
    (testing "the page is otherwise in the UI language, not the corpus's"
      (let [da (pr-str (corpus/info-view "da" data))]
        (is (re-find #"Positionelle attributter" da))
        (is (re-find #"Søg i" da))
        (testing "and its own links keep it"
          (is (re-find #"/\?corpus=VISER&lang=da" da)))))
    (testing "an error becomes a fixed alert leaking nothing of its message"
      (let [html (pr-str (corpus/info-view "en" {:corpus "GONE"
                                                 :error  {:message
                                                          "/srv/secret"}}))]
        (is (re-find #"Could not read corpus" html))
        (is (not (re-find #"/srv/secret" html)))
        (is (not (re-find #":table" html)))))))
