(ns dk.cst.corpus-probe.views.corpus-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.views.hiccup :refer [da deep en]]
            [dk.cst.corpus-probe.views.corpus :as corpus]
            [dk.cst.corpus-probe.views.layout :as layout]))

(deftest corpus-item-test
  (testing "a titled corpus links its title and shows its ID"
    (let [item (corpus/corpus-item en {:id    "VISER"
                                         :title "Folkeviser"
                                         :size  48})]
      (is (= [:a {:href "/corpora/viser"} "Folkeviser"] (second item)))
      (is (some #{[:code "VISER"]} (deep item)))))
  (testing "an untitled corpus links its ID"
    (is (= [:a {:href "/corpora/probe"} "PROBE"]
           (second (corpus/corpus-item en {:id "PROBE" :size 47})))))
  (testing "the info link names no language"
    (is (= [:a {:href "/corpora/probe"} "PROBE"]
           (second (corpus/corpus-item da {:id "PROBE" :size 47})))))
  (testing "an unreadable corpus is marked unavailable in either language"
    (is (some #{[:em "unavailable"]}
              (deep (corpus/corpus-item en {:id "GONE" :size nil}))))
    (is (some #{[:em "utilgængelig"]}
              (deep (corpus/corpus-item da {:id "GONE" :size nil}))))))

(deftest token-count-test
  (testing "the digits are grouped as the language groups them"
    (is (= [:data.size {:value "64600000"} "64,600,000 tokens"]
           (corpus/token-count en 64600000)))
    (is (= [:data.size {:value "64600000"} "64.600.000 tokens"]
           (corpus/token-count da 64600000)))))

(deftest chooser-item-test
  (let [checkbox (fn [selected m]
                   (second (get-in (corpus/chooser-item en selected m)
                                   [2 1])))]
    (testing "a corpus is a checkbox named corpus, checked when selected"
      (is (= {:type "checkbox" :name "corpus" :value "VISER"
              :checked true :disabled false
              :on {:change [:toggle-corpora ["VISER"]]}}
             (checkbox #{"VISER"} {:id "VISER" :size 48})))
      (is (not (:checked (checkbox #{} {:id "VISER" :size 48})))))
    (testing "an unreadable corpus is disabled"
      (is (:disabled (checkbox #{} {:id "GONE" :size nil}))))))

(deftest tree-test
  (let [folders [{:label   "Litteratur"
                  :corpora [{:id "VISER" :title "Folkeviser" :size 48}
                            {:id "GONE" :size nil}]
                  :folders [{:label   nil
                             :corpora [{:id "PROBE" :size 1}]
                             :folders []}]}
                 {:label nil :corpora [{:id "TALER" :size 42}] :folders []}]
        [litteratur tail] (corpus/tree en folders)]
    (testing "a folder is a node named by the path of labels down to it,
              the label-less one by its lack of one, in either language"
      (is (= ["Litteratur"] (:id litteratur)))
      (is (= [["Litteratur" nil]] (map :id (:nodes litteratur))))
      (is (= [nil] (:id tail)))
      (is (= [nil] (:id (second (corpus/tree da folders))))))
    (testing "the tail folder is labelled among labelled siblings"
      (is (= "Other" (:label tail)))
      (is (= "Andre" (:label (second (corpus/tree da folders)))))
      (is (nil? (:label (first (corpus/tree en [(second folders)]))))))
    (testing "a corpus is a leaf named by its ID, read by its ID and its
              title, and disabled where it cannot be read"
      (is (= [["VISER" "VISER Folkeviser" false] ["GONE" "GONE " true]]
             (map (juxt :id :text (comp boolean :disabled?))
                  (:items litteratur))))
      (testing "with everything the chooser item draws it from"
        (is (= "Folkeviser" (:title (first (:items litteratur)))))))))

(deftest chooser-test
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
                    ;; [:fieldset {} [:legend] [:div.group <toggle>
                    ;;  [:details {} [:summary {} <box> [:small.count]]]]
                    ;;  <status>]
                    (get-in (corpus/chooser en fs {:selected selected})
                            [3 2 2])))]
    (testing "the chooser over the registry's tree, named for the client
              as the corpora list, with the corpus boxes as its leaves"
      (let [html (corpus/chooser en folders {:selected #{"VISER"}})]
        (is (= :fieldset.chooser.corpora (first html)))
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
             (->> (deep (corpus/chooser en folders {:selected #{"VISER"}}))
                  (filter #(and (map? %) (contains? % :open)))
                  (map :open)))))
    (testing "a corpus that cannot be read cannot be chosen, so a folder
              holding one is not partly chosen for ever"
      (let [unreadable [{:label   "Litteratur"
                         :corpora [{:id "VISER" :size 48} {:id "GONE"}]
                         :folders []}]]
        (is (= [false false]
               (->> (deep (corpus/chooser en unreadable
                                          {:selected #{"VISER"}}))
                    (filter #(and (map? %) (contains? % :open)))
                    (map :open))))))
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
               (second (get-in (corpus/chooser da folders {:selected #{"VISER"}})
                               [3 2 2]))))))
    (testing "no folder toggles without a client to answer them"
      (is (not (some #(and (map? %) (contains? % :replicant/on-render))
                     (deep (corpus/chooser en folders {:selected #{"VISER"}}))))))
    (testing "with one, each labelled folder carries its own and so does
              the whole registry, which is otherwise a click per folder"
      (let [toggles (fn [opts]
                      (->> (deep (corpus/chooser
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
               (-> (corpus/chooser da folders {:selected #{} :client? true
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
             (get-in (second (corpus/all-toggle en #{} ["VISER" "TALER"]))
                     [:on :change]))))
    (testing "and is invalid while nothing is selected, saying what the
              summary says, so the browser refuses a search of no corpus
              on the control that can put it right"
      (let [invalid (fn [ui opts]
                      (->> (deep (corpus/chooser ui folders
                                                 (assoc opts :client? true)))
                           (filter #(and (map? %) (:replicant/on-render %)))
                           (keep (comp :invalid second :replicant/on-render))))]
        (is (= ["Select at least one corpus"] (invalid en {:selected #{}})))
        (is (= ["Vælg mindst ét korpus"] (invalid da {:selected #{}})))
        (is (empty? (invalid en {:selected #{"VISER"}})))
        ;; a selection the filter hides is still a selection
        (is (empty? (invalid en {:selected #{"VISER"} :filter "taler"})))))
    (testing "a filter answering nothing says so in the chooser's words,
              and every box is still in the document"
      (let [html (deep (corpus/chooser en folders {:selected #{"VISER"}
                                                     :client?  true
                                                     :filter   "zzz"}))]
        (is (some #{[:div.empty {:role "status"} "No corpora found."]} html))
        (is (some #(and (map? %) (= "VISER" (:value %)) (:checked %)) html))
        (is (some #(and (map? %) (= "TALER" (:value %))) html))))
    (testing "the box is named for the list, and only with a client"
      (is (some #(and (map? %) (= "corpora-filter" (:id %)))
                (deep (corpus/chooser en folders {:selected #{"VISER"}
                                                  :client?  true}))))
      (is (not (some #(and (map? %) (= "corpora-filter" (:id %)))
                     (deep (corpus/chooser en folders
                                           {:selected #{"VISER"}}))))))
    (testing "the legend is in the chosen language"
      (is (some #{[:legend "Korpusser"]}
                (deep (corpus/chooser da folders {:selected #{}})))))))

(deftest index-view-test
  (let [folders [{:label   "Litteratur"
                  :corpora []
                  :folders [{:label   "Folkeviser"
                             :corpora [{:id "VISER" :title "Folkeviser"
                                        :size 48}]
                             :folders []}]}
                 {:label   nil
                  :corpora [{:id "PROBE" :size 47}]
                  :folders []}]
        html    (corpus/index-view en {:folders folders})
        tags    (fn [html] (->> (deep html)
                                (filter #(and (vector? %) (not (map-entry? %))
                                              (keyword? (first %))))
                                (map first)))]
    (testing "a document: the page's heading, then a heading per folder,
              a level deeper per folder inside, and a list per folder's
              corpora, with nothing folded away"
      (is (= layout/main-attrs (second html)))
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
             (tags (corpus/index-view en {:folders [(second folders)]})))))
    (testing "a folder nested too deep for HTML's headings keeps the last"
      (is (= :h6 (corpus/heading 7)))
      (is (= :h2 (corpus/heading 2))))
    (testing "in Danish"
      (is (some #{[:h2 "Andre"]}
                (deep (corpus/index-view da {:folders folders})))))))

(deftest info-view-navigation-test
  (let [html (deep (corpus/info-view en {:corpus "VISER" :stats {} :info {}}))]
    (testing "where the page leads is a named navigation, not a paragraph"
      (is (some #{{:aria-label "This corpus"}} html))
      (is (some #{:ul.row} html))
      (is (= ["/search?corpus=VISER"
              "/search?corpus=VISER&view=frequencies#results"]
             (map :href (filter #(and (map? %) (:href %)) html)))))
    (testing "a phantom entry cannot be searched, so it is offered nothing"
      (is (not (some #{{:aria-label "This corpus"}}
                     (deep (corpus/info-view en {:corpus   "GONE"
                                                   :error    {}
                                                   :phantom? true}))))))))

(deftest labelled-folders-test
  (testing "a lone label-less folder stays as it is"
    (is (= [{:label nil}] (corpus/labelled-folders en [{:label nil}]))))
  (testing "among labelled siblings the tail is labelled in the language"
    (is (= ["A" "Other"]
           (map :label
                (corpus/labelled-folders en [{:label "A"} {:label nil}]))))
    (is (= ["A" "Andre"]
           (map :label
                (corpus/labelled-folders da
                                         [{:label "A"} {:label nil}]))))))

(deftest count-cell-test
  (is (= [:td.n "1,000"] (corpus/count-cell en 1000)))
  (is (= [:td.n "1.000"] (corpus/count-cell da 1000)))
  (testing "a missing count is the tool's own NO DATA"
    (is (= [:td.n [:em "no data"]] (corpus/count-cell en nil)))
    (is (= [:td.n [:em "ingen data"]] (corpus/count-cell da nil)))))

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
        html (pr-str (corpus/info-view en data))]
    (testing "the corpus names the page, the ID its subtitle"
      (is (re-find #":h1 .*\"Folkeviser\"" html))
      (is (re-find #":code \"VISER\"" html)))
    (testing "the corpus's language marks the title and the .info text only"
      (is (re-find #"\[:h1 \{:lang \"da\"\} \"Folkeviser\"\]" html))
      (is (re-find #"\[:pre \{:lang \"da\"\} \"Om korpusset.\"\]" html))
      (is (not (re-find #":article.corpus-info \{" html))))
    (testing "the duplicate charset property is not repeated in the facts"
      (is (= 1 (count (re-seq #"\"utf8\"" html)))))
    (testing "an attribute without data is marked as such"
      (is (re-find #"no data" html)))
    (testing "the page is otherwise in the UI language, not the corpus's"
      (let [da (pr-str (corpus/info-view da data))]
        (is (re-find #"Positionelle attributter" da))
        (is (re-find #"Søg i" da))
        (testing "and its own links keep it"
          (is (re-find #"/search\?corpus=VISER\"" da)))))
    (testing "an error becomes a fixed alert leaking nothing of its message"
      (let [html (pr-str (corpus/info-view en {:corpus "GONE"
                                                 :error  {:message
                                                          "/srv/secret"}}))]
        (is (re-find #"Unreadable corpus" html))
        (is (not (re-find #"/srv/secret" html)))
        (is (not (re-find #":table" html)))))
    (testing "an entry CWB has no data for says so, not that reading failed"
      (let [html (pr-str (corpus/info-view en {:corpus   "GONE"
                                                 :error    {:message "x"}
                                                 :phantom? true}))]
        (is (re-find #"The registry lists this corpus" html))
        (is (not (re-find #"could not read this corpus" html)))
        (testing "and it is not offered for searching either"
          (is (not (re-find #"Search in GONE" html))))))
    (testing "one that failed to be read this time is still offered"
      (is (re-find #"Search in GONE"
                   (pr-str (corpus/info-view
                            en {:corpus "GONE"
                                :error  {:message "x"}})))))))

(deftest unreadable-section-test
  (testing "a phantom entry and a failed read are told apart"
    (is (some #{"The registry lists this corpus, but CWB has no data for it."}
              (deep (corpus/unreadable-section en true))))
    (is (some #{"CWB cannot read the data files of this corpus."}
              (deep (corpus/unreadable-section en false))))
    (is (some #{"Korpusset står i registret, men CWB har ingen data til det."}
              (deep (corpus/unreadable-section da true)))))
  (testing "both are one section under the same heading"
    (is (some #{[:h2 "Unreadable corpus"]}
              (deep (corpus/unreadable-section en true)))))
  (testing "no live region: it is in the document before the page is parsed"
    (is (not (some #{"alert"} (deep (corpus/unreadable-section en false)))))))
