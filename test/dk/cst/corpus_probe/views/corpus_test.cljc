(ns dk.cst.corpus-probe.views.corpus-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.hiccup :refer [deep]]
            [dk.cst.corpus-probe.test.hiccup :refer [da en open-states]]
            [dk.cst.corpus-probe.views.corpus :as corpus-views]
            [dk.cst.corpus-probe.views.widgets :as widgets]))

(deftest corpus-item-test
  (testing "a titled corpus links its title and shows its ID"
    (let [item (corpus-views/corpus-item en {:id    "VISER"
                                             :title "Folkeviser"
                                             :size  48})]
      (is (= [:a {:href "/corpora/viser"} "Folkeviser"] (second item)))
      (is (some #{[:code "VISER"]} (deep item)))))
  (testing "an untitled corpus links its ID"
    (is (= [:a {:href "/corpora/probe"} "PROBE"]
           (second (corpus-views/corpus-item en {:id "PROBE" :size 47})))))
  (testing "the info link names no language"
    (is (= [:a {:href "/corpora/probe"} "PROBE"]
           (second (corpus-views/corpus-item da {:id "PROBE" :size 47})))))
  (testing "an unreadable corpus is marked unavailable in either language,
            where its size would stand"
    (is (some #{[:em.size "unavailable"]}
              (deep (corpus-views/corpus-item en {:id "GONE" :size nil}))))
    (is (some #{[:em.size "utilgængelig"]}
              (deep (corpus-views/corpus-item da {:id "GONE" :size nil}))))))

(deftest chooser-item-test
  (let [checkbox (fn [selected m]
                   (second (get-in (corpus-views/chooser-item en selected m)
                                   [2 1])))]
    (testing "a corpus is a checkbox named corpus, checked when selected"
      (is (= {:type "checkbox" :name "corpus" :value "VISER"
              :checked true :disabled false
              :on {:change [:toggle-corpora ["VISER"]]}}
             (checkbox #{"VISER"} {:id "VISER" :size 48})))
      (is (not (:checked (checkbox #{} {:id "VISER" :size 48})))))
    (testing "an unreadable corpus is disabled"
      (is (:disabled (checkbox #{} {:id "GONE" :size nil}))))
    (testing "a hidden entry stays in the document"
      (is (= {:hidden true}
             (second (corpus-views/chooser-item en #{} {:id "X" :size 1
                                                        :hidden? true})))))))

(deftest corpus-tree-test
  (let [folders [{:label   "Litteratur"
                  :corpora [{:id "VISER" :title "Folkeviser" :size 48}
                            {:id "GONE" :size nil}]
                  :folders [{:label   nil
                             :corpora [{:id "PROBE" :size 1}]
                             :folders []}]}
                 {:label nil :corpora [{:id "TALER" :size 42}] :folders []}]
        [litteratur tail] (corpus-views/corpus-tree en folders)]
    (testing "a folder is a node named by the path of labels down to it,
              the label-less one by its lack of one, in either language"
      (is (= ["Litteratur"] (:id litteratur)))
      (is (= [["Litteratur" nil]] (map :id (:nodes litteratur))))
      (is (= [nil] (:id tail)))
      (is (= [nil] (:id (second (corpus-views/corpus-tree da folders))))))
    (testing "the tail folder is labelled among labelled siblings"
      (is (= "Other" (:label tail)))
      (is (= "Andre" (:label (second (corpus-views/corpus-tree da folders)))))
      (is (nil? (:label (first (corpus-views/corpus-tree en [(second folders)]))))))
    (testing "a corpus is a leaf named by its ID, read by its ID and its
              title, and disabled where it cannot be read"
      (is (= [["VISER" "VISER Folkeviser" false] ["GONE" "GONE " true]]
             (map (juxt :id :text (comp boolean :disabled?))
                  (:items litteratur))))
      (testing "with everything the chooser item draws it from"
        (is (= "Folkeviser" (:title (first (:items litteratur)))))))))

(deftest corpus-chooser-test
  (let [folders [{:label   "Litteratur"
                  :corpora []
                  :folders [{:label   "Folkeviser"
                             :corpora [{:id "VISER" :size 48}]
                             :folders []}]}
                 {:label "Folketinget" :corpora [{:id "TALER" :size 42}]
                  :folders []}]
        summary (fn [selected & [{:keys [total]}]]
                  (let [fs (cond-> folders
                             (= 3 total) (conj {:label   "Andet"
                                                :corpora [{:id "PROBE" :size 1}]
                                                :folders []}))]
                    ;; [:fieldset {} [:legend] [:div.chooser-group <toggle>
                    ;;  [:details {} [:summary {} <box> [:small.count]]]]
                    ;;  <status>]
                    (get-in (corpus-views/corpus-chooser en fs {:selected selected})
                            [3 2 2])))]
    (testing "the chooser over the registry's tree, named for the client
              as the corpora list, with the corpus boxes as its leaves"
      (let [html (corpus-views/corpus-chooser en folders {:selected #{"VISER"}})]
        (is (= :fieldset.chooser.box (first html)))
        (is (= "corpora" (:data-list (second html))))
        (is (= ["VISER" "TALER"]
               (keep #(when (and (map? %) (= "corpus" (:name %))) (:value %))
                     (deep html))))
        (is (= [true false]
               (keep #(when (and (map? %) (= "corpus" (:name %))) (:checked %))
                     (deep html))))))
    (testing "a disclosure stands open exactly while part of what it holds
              is chosen and the rest is not: [chooser Litteratur
              Folkeviser Folketinget]"
      (is (= [true false false false]
             (open-states (corpus-views/corpus-chooser en folders
                                                       {:selected #{"VISER"}})))))
    (testing "a corpus that cannot be read cannot be chosen, so a folder
              holding one is not partly chosen for ever"
      (let [unreadable [{:label   "Litteratur"
                         :corpora [{:id "VISER" :size 48} {:id "GONE"}]
                         :folders []}]]
        (is (= [false false]
               (open-states (corpus-views/corpus-chooser en unreadable
                                                         {:selected #{"VISER"}}))))))
    (testing "the summary counts the selection rather than naming it: two
              figures read the same way in every such list, where a
              sentence about corpora is one more thing to learn"
      (is (= [:small.count "(2/2)"] (last (summary #{"VISER" "TALER"}))))
      (is (= [:small.count "(1/2)"] (last (summary #{"VISER"}))))
      (is (= [:small.count "(0/2)"] (last (summary #{}))))
      (is (= [:small.count "(1/3)"] (last (summary #{"VISER"} {:total 3}))))
      (testing "and says aloud what the figures do not"
        (is (= {:aria-label "1 of 2 selected"} (second (summary #{"VISER"}))))
        (is (= {:aria-label "1 af 2 valgt"}
               (second (get-in (corpus-views/corpus-chooser
                                da folders {:selected #{"VISER"}})
                               [3 2 2]))))))
    (testing "no folder toggles without a client to answer them"
      (is (not (some #(and (map? %) (contains? % :replicant/on-render))
                     (deep (corpus-views/corpus-chooser
                            en folders {:selected #{"VISER"}}))))))
    (testing "with one, each labelled folder carries its own and so does
              the whole registry, which is otherwise a click per folder"
      (let [toggles (fn [opts]
                      (->> (deep (corpus-views/corpus-chooser
                                  en folders (merge {:selected #{"VISER"}
                                                     :client?  true} opts)))
                           (filter #(and (map? %)
                                         (contains? % :replicant/on-render)))
                           (map (juxt :aria-label #(get-in % [:on :change])))))]
        (is (= [["All corpora" [:toggle-corpora ["VISER" "TALER"]]]
                ["All corpora in Litteratur" [:toggle-corpora ["VISER"]]]
                ["All corpora in Folkeviser" [:toggle-corpora ["VISER"]]]
                ["All corpora in Folketinget" [:toggle-corpora ["TALER"]]]]
               (toggles {:choosing? true})))
        (is (= "Alle korpusser i Litteratur"
               (-> (corpus-views/corpus-chooser da folders {:selected #{}
                                                            :client? true
                                                            :choosing? true})
                   (deep)
                   (->> (filter #(and (map? %) (:replicant/on-render %)))
                        (map :aria-label))
                   (second))))
        ;; at rest a folder nothing is chosen in is not shown, and
        ;; neither is what is under one chosen whole, whose own row says
        ;; it; a control for a folder nobody can see is a control for
        ;; nothing
        (is (= ["All corpora" "All corpora in Litteratur"]
               (map first (toggles {}))))
        (testing "and under a filter each acts on what the reader can see"
          (is (= [["All corpora" [:toggle-corpora ["TALER"]]]
                  ["All corpora in Folketinget" [:toggle-corpora ["TALER"]]]]
                 (toggles {:filter "taler"}))))))
    (testing "the root toggle takes the whole registry at once"
      (is (= [:toggle-corpora ["VISER" "TALER"]]
             (get-in (second (corpus-views/all-toggle en #{} ["VISER" "TALER"]))
                     [:on :change]))))
    (testing "and is invalid while nothing is selected, saying what the
              summary says, so the browser refuses a search of no corpus
              on the control that can put it right"
      (let [invalid (fn [ui opts]
                      (->> (deep (corpus-views/corpus-chooser
                                  ui folders (assoc opts :client? true)))
                           (filter #(and (map? %) (:replicant/on-render %)))
                           (keep (comp :invalid second :replicant/on-render))))]
        (is (= ["Select at least one corpus"] (invalid en {:selected #{}})))
        (is (= ["Vælg mindst ét korpus"] (invalid da {:selected #{}})))
        (is (empty? (invalid en {:selected #{"VISER"}})))
        ;; a selection the filter hides is still a selection
        (is (empty? (invalid en {:selected #{"VISER"} :filter "taler"})))))
    (testing "a filter answering nothing says so in the chooser's words,
              and every box is still in the document"
      (let [html (deep (corpus-views/corpus-chooser en folders
                                                    {:selected #{"VISER"}
                                                     :client?  true
                                                     :filter   "zzz"}))]
        (is (some #{[:div.status {:class "chooser-status" :role "status"}
                     "No corpora found."]}
                  html))
        (is (some #(and (map? %) (= "VISER" (:value %)) (:checked %)) html))
        (is (some #(and (map? %) (= "TALER" (:value %))) html))))
    (testing "the box is named for the list, and only with a client"
      (is (some #(and (map? %) (= "corpora-filter" (:id %)))
                (deep (corpus-views/corpus-chooser en folders
                                                   {:selected #{"VISER"}
                                                    :client?  true}))))
      (is (not (some #(and (map? %) (= "corpora-filter" (:id %)))
                     (deep (corpus-views/corpus-chooser
                            en folders {:selected #{"VISER"}}))))))
    (testing "the legend is in the chosen language"
      (is (some #{[:legend "Korpusser"]}
                (deep (corpus-views/corpus-chooser da folders {:selected #{}})))))))

(deftest index-page-test
  (let [folders [{:label   "Litteratur"
                  :corpora []
                  :folders [{:label   "Folkeviser"
                             :corpora [{:id "VISER" :title "Folkeviser"
                                        :size 48}]
                             :folders []}]}
                 {:label   nil
                  :corpora [{:id "PROBE" :size 47}]
                  :folders []}]
        html    (corpus-views/index-page en {:folders folders})
        tags    (fn [html] (->> (deep html)
                                (filter #(and (vector? %) (not (map-entry? %))
                                              (keyword? (first %))))
                                (map first)))]
    (testing "a document: the page's heading, then a heading per folder,
              a level deeper per folder inside, and a list per folder's
              corpora, with nothing folded away"
      (is (= widgets/main-attrs (second html)))
      (is (= [:main :h1 :h2 :h3 :ul.index :li :a :code :data.size
              :h2 :ul.index :li :a :data.size]
             (tags html)))
      (is (= [[:h1 "Corpora"] [:h2 "Litteratur"] [:h3 "Folkeviser"]
              [:h2 "Other"]]
             (filter #(and (vector? %) (#{:h1 :h2 :h3} (first %)))
                     (deep html))))
      (is (not (some #{:details :summary} (tags html)))))
    (testing "each corpus links to its page"
      (is (some #{[:a {:href "/corpora/viser"} "Folkeviser"]} (deep html)))
      (is (some #{[:a {:href "/corpora/probe"} "PROBE"]} (deep html))))
    (testing "a registry without folders is one list under the heading"
      (is (= [:main :h1 :ul.index :li :a :data.size]
             (tags (corpus-views/index-page en {:folders [(second folders)]})))))
    (testing "in Danish"
      (is (some #{[:h2 "Andre"]}
                (deep (corpus-views/index-page da {:folders folders})))))))

(deftest info-page-navigation-test
  (let [html (deep (corpus-views/info-page en {:corpus "VISER" :stats {} :info {}}))]
    (testing "where the page leads is a named navigation, not a paragraph"
      (is (some #{{:aria-label "This corpus"}} html))
      (is (some #{:ul.row} html))
      (is (= ["/search?corpus=VISER"
              "/search?corpus=VISER&view=frequencies#results"]
             (map :href (filter #(and (map? %) (:href %)) html)))))
    (testing "a phantom entry cannot be searched, so it is offered nothing"
      (is (not (some #{{:aria-label "This corpus"}}
                     (deep (corpus-views/info-page en {:corpus   "GONE"
                                                       :error    {}
                                                       :phantom? true}))))))))

(deftest labelled-folders-test
  (testing "a lone label-less folder stays as it is"
    (is (= [{:label nil}] (corpus-views/labelled-folders en [{:label nil}]))))
  (testing "among labelled siblings the tail is labelled in the language"
    (is (= ["A" "Other"]
           (map :label
                (corpus-views/labelled-folders en [{:label "A"} {:label nil}]))))
    (is (= ["A" "Andre"]
           (map :label
                (corpus-views/labelled-folders da
                                               [{:label "A"} {:label nil}]))))))

(deftest stat-cell-test
  (is (= [:td.num "1,000"] (corpus-views/stat-cell en 1000)))
  (is (= [:td.num "1.000"] (corpus-views/stat-cell da 1000)))
  (testing "a missing count is the tool's own NO DATA"
    (is (= [:td.num [:em "no data"]] (corpus-views/stat-cell en nil)))
    (is (= [:td.num [:em "ingen data"]] (corpus-views/stat-cell da nil)))))

(deftest info-page-test
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
        html (pr-str (corpus-views/info-page en data))]
    (testing "the corpus names the page, the ID its subtitle"
      (is (re-find #":h1 .*\"Folkeviser\"" html))
      (is (re-find #":code \"VISER\"" html)))
    (testing "the corpus's language marks the title and the .info text only"
      (is (re-find #"\[:h1 \{:lang \"da\"\} \"Folkeviser\"\]" html))
      (is (re-find #"\[:pre \{:lang \"da\"\} \"Om korpusset.\"\]" html))
      (is (not (re-find #":article" html))))
    (testing "the facts are a definition list, the size machine-readable"
      (is (re-find #":dl.facts" html))
      (is (re-find #"\[:dt \"size\"\]" html))
      (is (re-find #":data.size \{:value \"48\"\}" html)))
    (testing "the duplicate charset property is not repeated in the facts"
      (is (= 1 (count (re-seq #"\"utf8\"" html)))))
    (testing "an attribute without data is marked as such"
      (is (re-find #"no data" html)))
    (testing "the page is otherwise in the UI language, not the corpus's"
      (let [da (pr-str (corpus-views/info-page da data))]
        (is (re-find #"Positionelle attributter" da))
        (is (re-find #"Søg i" da))
        (testing "and its own links keep it"
          (is (re-find #"/search\?corpus=VISER\"" da)))))
    (testing "an error becomes a fixed alert leaking nothing of its message"
      (let [html (pr-str (corpus-views/info-page en {:corpus "GONE"
                                                     :error  {:message
                                                              "/srv/secret"}}))]
        (is (re-find #"Unreadable corpus" html))
        (is (not (re-find #"/srv/secret" html)))
        (is (not (re-find #":table" html)))))
    (testing "an entry CWB has no data for says so, not that reading failed"
      (let [html (pr-str (corpus-views/info-page en {:corpus   "GONE"
                                                     :error    {:message "x"}
                                                     :phantom? true}))]
        (is (re-find #"The registry lists this corpus" html))
        (is (not (re-find #"could not read this corpus" html)))
        (testing "and it is not offered for searching either"
          (is (not (re-find #"Search in GONE" html))))))
    (testing "one that failed to be read this time is still offered"
      (is (re-find #"Search in GONE"
                   (pr-str (corpus-views/info-page
                            en {:corpus "GONE"
                                :error  {:message "x"}})))))))

(deftest unreadable-section-test
  (testing "a phantom entry and a failed read are told apart"
    (is (some #{"The registry lists this corpus, but CWB has no data for it."}
              (deep (corpus-views/unreadable-section en true))))
    (is (some #{"CWB cannot read the data files of this corpus."}
              (deep (corpus-views/unreadable-section en false))))
    (is (some #{"Korpusset står i registret, men CWB har ingen data til det."}
              (deep (corpus-views/unreadable-section da true)))))
  (testing "both are one section under the same heading"
    (is (= :section.error (first (corpus-views/unreadable-section en true))))
    (is (some #{[:h2 "Unreadable corpus"]}
              (deep (corpus-views/unreadable-section en true)))))
  (testing "no live region: it is in the document before the page is parsed"
    (is (not (some #{"alert"} (deep (corpus-views/unreadable-section en false)))))))

(def hverdag
  "A text of the dev corpus, as dk.cst.corpus-probe.server.corpora/serve-text
  hands it to the view."
  {:corpus  "PROBE"
   :from    0
   :to      12
   :structs {:text_id "t1" :text_title "Hverdag" :text_year "2023"}
   :blocks  [["Hunden" "sover" "under" "bordet" "."]
             ["Katten" "jagter" "en" "lille" "hund" "i" "haven" "."]]
   :hit     [9 9]
   :lang    "da"})

(deftest text-name-test
  (is (= "Hverdag" (corpus-views/text-name en {:text_title "Hverdag" :text_id "t1"})))
  (is (= "t1" (corpus-views/text-name en {:text_id "t1"})))
  (is (= "Tekst" (corpus-views/text-name da nil))))

(deftest marked-test
  (testing "the hit is marked inside its block, and lands the page"
    (is (= ["Katten jagter en lille" " " [:mark {:id "hit"} "hund"] " "
            "i haven ."]
           (corpus-views/marked 5 [9 9] true
                                ["Katten" "jagter" "en" "lille" "hund" "i" "haven"
                                 "."]))))
  (testing "a mark elsewhere carries no id"
    (is (= [[:mark {} "Hunden sover"] " " "under bordet ."]
           (corpus-views/marked 0 [0 1] false ["Hunden" "sover" "under" "bordet" "."]))))
  (testing "without a hit the block is plain text"
    (is (= "Hunden sover" (corpus-views/marked 0 nil false ["Hunden" "sover"])))))

(deftest reading-page-test
  (let [html (corpus-views/reading-page en hverdag)
        all  (deep html)]
    (testing "a document named by the text, its prose in the corpus's language"
      (is (= :main.document (first html)))
      (is (some #{[:h1 "Hverdag"]} all))
      (is (some #(and (map? %) (= "da" (:lang %))) all)))
    (testing "it says which corpus the text is from"
      (is (some #(and (map? %) (= "/corpora/probe" (:href %))) all)))
    (testing "its metadata are the facts of the text"
      (is (some #{:dl.facts} all))
      (is (some #{[:dd [:time "2023"]]} all)))
    (testing "the hit is marked once, in the block that holds it"
      (is (= 1 (count (filter #{[:mark {:id "hit"} "hund"]} all))))
      (is (not (some #(and (vector? %) (= :mark (first %)) (= {} (second %)))
                     all)))))
  (testing "an error stands in for the text"
    (let [html (deep (corpus-views/reading-page en {:corpus "X"
                                                    :error  {:type :no-texts}}))]
      (is (some #{"The corpus marks no texts"} html))
      (is (some #{:section.error} html))
      (is (some #{[:h1 "Text"]} html)))))
