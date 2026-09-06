(ns dk.cst.corpus-probe.views.corpus-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.views.hiccup :refer [da deep en]]
            [dk.cst.corpus-probe.views.corpus :as corpus]
            [dk.cst.corpus-probe.views.layout :as layout]))

(defn summaries
  "The text of every disclosure summary in hiccup `html`, in order."
  [html]
  (->> (deep html)
       (filter #(and (vector? %) (= :summary (first %))))
       (map #(apply str (filter string? (tree-seq coll? seq (rest %)))))))

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

(deftest folder-view-test
  (let [litteratur {:label   "Litteratur"
                    :corpora [{:id "VISER" :size 48}]
                    :folders []}
        item       (partial corpus/corpus-item "en")
        label      :label]
    (testing "a labelled folder is a disclosure, open as told"
      (let [[tag attrs summary] (corpus/folder-view
                                 {:item item :summary label
                                  :open? (constantly true)}
                                 litteratur)]
        (is (= :details tag))
        (is (:open attrs))
        (is (= [:summary "Litteratur"] summary)))
      (is (not (:open (second (corpus/folder-view
                               {:item item :summary label
                                :open? (constantly false)}
                               litteratur))))))
    (testing "the summary is computed, so a closed folder can still count,
              the count a side note beside the name"
      (is (= [:summary (list "Litteratur" " " [:small.count "(0/1)"])]
             (nth (corpus/folder-view
                   {:item    item
                    :summary (partial corpus/folder-summary #{})
                    :open?   (constantly false)}
                   litteratur)
                  2)))
      (testing "and how much of it the selection is, which needs no words
                and so says the same in either language"
        (is (= ["Litteratur (1/1)"]
               (summaries (corpus/folder-view
                           {:item    item
                            :summary (partial corpus/folder-summary
                                              #{"VISER"})
                            :open?   (constantly false)}
                           litteratur))))))
    (testing "a toggle takes its place beside the disclosure, not inside it"
      (let [[tag control disclosure]
            (corpus/folder-view {:item item :summary label
                                 :open? (constantly true)
                                 :toggle (constantly [:input {:type "checkbox"}])}
                                litteratur)]
        (is (= :div.group tag))
        (is (= [:input {:type "checkbox"}] control))
        (is (= :details (first disclosure)))))
    (testing "and a tree without one is the disclosure itself, unwrapped"
      (is (= :details (first (corpus/folder-view
                              {:item item :summary label
                               :open? (constantly true)
                               :toggle (constantly nil)}
                              litteratur)))))
    (testing "the label-less tail folder is a bare list"
      (is (= :ul (first (corpus/folder-view
                         {:item item :summary label
                          :open? (constantly true)}
                         {:label   nil
                          :corpora [{:id "PROBE" :size 1}]
                          :folders []})))))))

(deftest folder-toggle-test
  (let [folder {:label   "Folketinget"
                :corpora [{:id "TALER" :size 42}]
                :folders [{:label   "Udvalg"
                           :corpora [{:id "REFERAT" :size 7}]
                           :folders []}]}
        attrs  (fn [selected] (second (corpus/folder-toggle en selected
                                                            folder)))]
    (testing "one control answers for the whole folder, subfolders included"
      (is (= [:toggle-corpora ["TALER" "REFERAT"]]
             (get-in (attrs #{}) [:on :change]))))
    (testing "checked only when the folder is wholly selected"
      (is (:checked (attrs #{"TALER" "REFERAT"})))
      (is (not (:checked (attrs #{"TALER"}))))
      (is (not (:checked (attrs #{})))))
    (testing "part of a folder is the third state, set as a property"
      (is (= [:set-checkbox-state {:indeterminate true :invalid nil}]
             (:replicant/on-render (attrs #{"TALER"}))))
      (is (= [:set-checkbox-state {:indeterminate false :invalid nil}]
             (:replicant/on-render (attrs #{}))))
      (is (= [:set-checkbox-state {:indeterminate false :invalid nil}]
             (:replicant/on-render (attrs #{"TALER" "REFERAT"})))))
    (testing "a folder is never required: only the whole chooser is"
      (is (nil? (:invalid (second (:replicant/on-render (attrs #{})))))))
    (testing "it names the folder, having no visible label of its own"
      (is (= "All corpora in Folketinget" (:aria-label (attrs #{}))))
      (is (= "Alle korpusser i Folketinget"
             (:aria-label (second (corpus/folder-toggle da #{} folder))))))
    (testing "a corpus that cannot be read is not something to select"
      (is (= [:toggle-corpora ["TALER"]]
             (get-in (second (corpus/folder-toggle
                              "en" #{}
                              {:label   "Folketinget"
                               :corpora [{:id "TALER" :size 42}
                                         {:id "GONE" :size nil}]
                               :folders []}))
                     [:on :change]))))
    (testing "under a filter it acts on what the reader can see"
      (let [narrowed (corpus/narrow "taler" folder)]
        (is (= [:toggle-corpora ["TALER"]]
               (get-in (second (corpus/folder-toggle en #{} narrowed))
                       [:on :change])))))
    (testing "and a folder of nothing selectable has no toggle at all"
      (is (nil? (corpus/folder-toggle en #{}
                                      {:label   "Tom"
                                       :corpora [{:id "GONE" :size nil}]
                                       :folders []}))))))

(deftest answers?-test
  (testing "a corpus answers a fragment of its ID or of its title"
    (is (corpus/answers? "vis" {:id "VISER" :title "Folkeviser"}))
    (is (corpus/answers? "vise" {:id "VISER" :title "Folkeviser"}))
    (is (corpus/answers? "folke" {:id "VISER" :title "Folkeviser"}))
    (is (not (corpus/answers? "taler" {:id "VISER" :title "Folkeviser"}))))
  (testing "an untitled corpus is matched on its ID alone"
    (is (corpus/answers? "pro" {:id "PROBE"}))
    (is (not (corpus/answers? "folke" {:id "PROBE"})))))

(deftest narrow-test
  (let [folder {:label   "Litteratur"
                :corpora [{:id "VISER" :title "Folkeviser" :size 48}]
                :folders [{:label   "Folketinget"
                           :corpora [{:id "TALER" :size 42}]
                           :folders []}]}]
    (testing "what does not answer is marked, not removed"
      (let [narrowed (corpus/narrow "taler" folder)]
        (is (:hidden? (first (:corpora narrowed))))
        (is (not (:hidden? (first (:corpora (first (:folders narrowed)))))))
        ;; the corpus is still in the tree, so its checkbox is still in the
        ;; document and the search it belongs to is unchanged
        (is (= 1 (count (:corpora narrowed))))))
    (testing "a folder with nothing left is itself marked"
      (let [narrowed (corpus/narrow "viser" folder)]
        (is (:hidden? (first (:folders narrowed))))
        (is (not (:hidden? narrowed)))))
    (testing "naming a folder asks for everything in it"
      (let [narrowed (corpus/narrow "folketinget" folder)]
        (is (not (:hidden? (first (:folders narrowed)))))
        (is (not (:hidden? (first (:corpora (first (:folders narrowed)))))))))
    (testing "and nothing anywhere marks the whole folder"
      (is (:hidden? (corpus/narrow "zzz" folder))))))

(deftest corpus-filter-test
  (let [attrs (fn [q] (get-in (corpus/corpus-filter en q) [3 1]))]
    (testing "it is not part of the search: no name to submit under"
      (is (nil? (:name (attrs "vis")))))
    (testing "it holds what was typed and reports every change"
      (is (= "vis" (:value (attrs "vis"))))
      (is (= "" (:value (attrs nil))))
      (is (= [:filter-corpora] (get-in (attrs nil) [:on :input]))))
    (testing "it is labelled by the one word: the legend says what it filters"
      (is (= [:label {:for "corpus-filter"} "Filter"]
             (get-in (corpus/corpus-filter en nil) [1]))))
    (testing "and watches for the Enter that would submit the form"
      (is (= [:swallow-enter] (get-in (attrs nil) [:on :keydown]))))))

(deftest chooser-test
  (let [folders [{:label   "Litteratur"
                  :corpora []
                  :folders [{:label   "Folkeviser"
                             :corpora [{:id "VISER" :size 48}]
                             :folders []}]}
                 {:label "Folketinget" :corpora [{:id "TALER" :size 42}]
                  :folders []}]
        chooser-summary
        (fn [selected & [{:keys [total]}]]
          (let [fs (cond-> folders
                     (= 3 total) (conj {:label "Andet"
                                        :corpora [{:id "PROBE" :size 1}]
                                        :folders []}))]
            ;; [:fieldset [:legend] <filter> <status> [:details [:summary]]]
            (-> (corpus/chooser en fs {:selected selected})
                (get-in [4 2])
                second)))
        open    (fn [selected]
                  (->> (deep (corpus/chooser en folders {:selected selected :served selected}))
                       (filter #(and (map? %) (contains? % :open)))
                       (map :open)))]
    (testing "the chooser is one disclosure, closed unless nothing is chosen"
      (is (= false (first (open #{"VISER"}))))
      (is (= true (first (open #{})))))
    (testing "inside it, a folder holding part of the selection starts open"
      (is (= [false true true false] (open #{"VISER"}))))
    (testing "the whole registry selected opens nothing: there is no part"
      (is (= [false false false false] (open #{"VISER" "TALER"}))))
    (testing "nothing selected opens nothing inside either"
      (is (= [true false false false] (open #{}))))
    (testing "the summary says what is selected"
      (is (some #{"All corpora"} (deep (corpus/chooser en folders
                                                       {:selected #{"VISER" "TALER"}}))))
      ;; one corpus is named rather than counted, so a reader sees which
      ;; one it is without opening the tree at all
      (is (= "VISER" (chooser-summary #{"VISER"})))
      (is (some #{"Select at least one corpus"}
                (deep (corpus/chooser en folders {:selected #{}})))))
    (testing "what is open follows the selection served, not the live one"
      (let [open* (fn [selected served]
                    (->> (deep (corpus/chooser en folders
                                               {:selected selected
                                                :served   served}))
                         (filter #(and (map? %) (contains? % :open)))
                         (map :open)))]
        ;; the reader clearing their last corpus in a folder must not shut
        ;; the folder they are working in
        (is (= (open* #{"VISER"} #{"VISER"}) (open* #{} #{"VISER"})))
        (is (= (open* #{"VISER"} #{"VISER"}) (open* #{"VISER" "TALER"}
                                                    #{"VISER"})))))
    (testing "no folder toggles without a client to answer them"
      (is (not (some #(and (map? %) (contains? % :replicant/on-render))
                     (deep (corpus/chooser en folders {:selected #{"VISER"}}))))))
    (testing "with one, each labelled folder carries its own and so does
              the whole registry, which is otherwise a click per folder"
      (is (= ["All corpora" "All corpora in Litteratur"
              "All corpora in Folkeviser" "All corpora in Folketinget"]
             (->> (deep (corpus/chooser en folders {:selected #{"VISER"}
                                                      :client?  true}))
                  (filter #(and (map? %) (contains? % :replicant/on-render)))
                  (map :aria-label)))))
    (testing "the root toggle takes the whole registry at once"
      (is (= [:toggle-corpora ["VISER" "TALER"]]
             (get-in (second (corpus/all-toggle en #{} folders))
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
    (testing "a filter hides what does not answer it and opens what does"
      (let [html (deep (corpus/chooser en folders {:selected #{}
                                                     :client?  true
                                                     :filter   "taler"}))]
        ;; VISER's box is still there to be submitted, just not shown
        (is (some #(and (map? %) (= "VISER" (:value %))) html))
        (is (some #{{:hidden true}} html))
        ;; both numbers are of what the filter left showing
        (is (some #{"Folketinget (0/1)"} (summaries html)))
        (is (some #{"Litteratur (0/0)"} (summaries html)))))
    (testing "the region saying nothing was found is there before it says it"
      (let [region (fn [opts]
                     (->> (corpus/chooser en folders
                                          (merge {:selected #{"VISER"}
                                                  :client?  true} opts))
                          (tree-seq coll? seq)
                          (filter #(and (vector? %) (= :div.empty (first %))))
                          first))]
        ;; a live region created already full has no change to announce,
        ;; so it is rendered whether or not it holds anything
        (is (= [:div.empty {:role "status"} nil] (region {})))
        (is (= [:div.empty {:role "status"} nil] (region {:filter "viser"})))
        (is (= [:div.empty {:role "status"} "No corpora found."]
               (region {:filter "zzz"})))))
    (testing "it stands where the tree was, which is hidden rather than gone"
      (let [[_ _ box region disclosure]
            (corpus/chooser en folders {:selected #{"VISER"}
                                          :client?  true
                                          :filter   "zzz"})]
        (is (= :p.find (first box)))
        (is (= :details (first disclosure)))
        (is (true? (:hidden (second disclosure))))
        (is (= [:div.empty {:role "status"} "No corpora found."] region))
        ;; and the checkboxes are still in it, so a search still carries
        ;; every corpus the reader had chosen
        (is (some #(and (map? %) (= "VISER" (:value %)) (:checked %))
                  (deep disclosure)))))
    (testing "a folder the filter empties is hidden, toggle or no toggle"
      (let [html (deep (corpus/chooser en folders {:selected #{}
                                                     :client?  true
                                                     :filter   "viser"}))]
        ;; Folketinget holds nothing answering "viser", and having nothing
        ;; selectable left it has no toggle to be hidden along with
        (is (= [{:open false :hidden true}]
               (filter #(and (map? %) (contains? % :open) (:hidden %))
                       html)))))
    (testing "a filter answering nothing says so beside the box, not in it"
      (let [html (deep (corpus/chooser en folders {:selected #{"VISER"}
                                                     :client?  true
                                                     :filter   "zzz"}))]
        (is (some #{[:div.empty {:role "status"} "No corpora found."]} html))
        ;; every box is still in the document: a filter narrows what is
        ;; shown, never what the form submits
        (is (some #(and (map? %) (= "VISER" (:value %))) html))
        (is (some #(and (map? %) (= "TALER" (:value %))) html))
        (is (some #(and (map? %) (= "VISER" (:value %)) (:checked %)) html))))
    (testing "the filter box is outside the disclosure it narrows"
      (let [[_ _ box region disclosure]
            (corpus/chooser en folders {:selected #{"VISER"} :client? true})]
        (is (= :p.find (first box)))
        ;; the region reporting what the box found stands between the two,
        ;; where the tree is when the tree has been hidden
        (is (= :div.empty (first region)))
        ;; the toggle row, holding the root select-all and the tree
        (is (= :div.group (first disclosure)))))
    (testing "a filter opens the tree, and emptying it again leaves it open"
      (let [open* (fn [opts] (->> (corpus/chooser en folders
                                                  (merge {:selected #{"VISER"}
                                                          :served   #{"VISER"}}
                                                         opts))
                                  (tree-seq coll? seq)
                                  (filter #(and (map? %) (contains? % :open)))
                                  first :open))]
        (is (true? (open* {:filter "viser"})))
        ;; the reader has since opened it, so emptying the box is not a
        ;; reason to take the tree away again
        (is (true? (open* {:open? true})))
        (is (false? (open* {:open? false})))))
    (testing "no filter box without a client to narrow anything"
      (is (not (some #(and (map? %) (= "corpus-filter" (:id %)))
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
