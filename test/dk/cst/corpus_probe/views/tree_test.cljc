(ns dk.cst.corpus-probe.views.tree-test
  "The chooser both fieldsets are: its rules over a small tree, and the
  markup of one node and of the whole."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.views.hiccup :refer [deep en]]
            [dk.cst.corpus-probe.views.tree :as tree]))

(def nodes
  "Two folders, one with a subfolder, and one corpus that cannot be read:
  [Litteratur [Folkeviser]] [Folketinget]."
  [{:id    ["Litteratur"]
    :label "Litteratur"
    :items [{:id "VISER" :text "VISER Folkeviser"} {:id "ANDEN" :text "ANDEN"}]
    :nodes [{:id    ["Litteratur" "Folkeviser"]
             :label "Folkeviser"
             :items [{:id "DIGTE" :text "DIGTE"} {:id "VERS" :text "VERS"}]
             :nodes []}]}
   {:id    ["Folketinget"]
    :label "Folketinget"
    :items [{:id "TALER" :text "TALER"} {:id "GONE" :text "GONE" :disabled? true}]
    :nodes []}])

(defn item
  "A leaf as a bare checkbox, hidden when marked."
  [{:keys [id hidden?]}]
  [:li (cond-> {} hidden? (assoc :hidden true))
   [:label [:input {:type "checkbox" :name "x" :value id}]]])

(defn opens
  "The open state of every disclosure in hiccup `html`, in order."
  [html]
  (->> (deep html)
       (filter #(and (map? %) (contains? % :open)))
       (map :open)))

(defn hidden-leaves
  "The ids of the leaves hidden in hiccup `html`."
  [html]
  (->> (deep html)
       (filter #(and (vector? %) (= :li (first %)) (:hidden (second %))))
       (map #(get-in % [2 1 1 :value]))))

(defn summaries
  "The text of every disclosure summary in hiccup `html`, in order."
  [html]
  (->> (deep html)
       (filter #(and (vector? %) (= :summary (first %))))
       (map #(apply str (filter string? (tree-seq coll? seq (rest %)))))))

(deftest narrow-test
  (let [[litteratur folketinget] nodes]
    (testing "what does not answer is marked, not removed"
      (let [narrowed (tree/narrow "anden" litteratur)]
        (is (:hidden? (first (:items narrowed))))
        (is (not (:hidden? (second (:items narrowed)))))
        (is (:hidden? (first (:nodes narrowed))))
        ;; still in the tree, so still in the document and the search
        (is (= 2 (count (:items narrowed))))))
    (testing "a leaf answers by its text, a folder by its label"
      (is (not (:hidden? (first (:items (tree/narrow "folkev" litteratur))))))
      (is (not (:hidden? (tree/narrow "litt" litteratur)))))
    (testing "naming a node asks for everything in it"
      (let [narrowed (tree/narrow "folkeviser" litteratur)]
        (is (not (:hidden? (first (:nodes narrowed)))))
        (is (not (:hidden? (first (:items (first (:nodes narrowed)))))))))
    (testing "and nothing anywhere marks the whole node"
      (is (:hidden? (tree/narrow "zzz" folketinget))))))

(deftest counted-test
  (let [[litteratur folketinget] (map tree/counted nodes)]
    (testing "a node offers what can be chosen in it, the nodes under it
              included, and a disabled leaf left out"
      (is (= ["VISER" "ANDEN" "DIGTE" "VERS"] (:offered litteratur)))
      (is (= ["DIGTE" "VERS"] (:offered (first (:nodes litteratur)))))
      (is (= ["TALER"] (:offered folketinget))))
    (testing "and not what a filter has hidden"
      (is (= ["ANDEN"]
             (:offered (tree/counted (tree/narrow "anden" litteratur))))))))

(deftest open-at-rest-test
  (testing "open at rest is exactly what is chosen in part: the root
            while the selection as a whole is, and each such node"
    (is (= #{} (tree/open-at-rest nodes #{})))
    (is (= #{} (tree/open-at-rest nodes #{"VISER" "ANDEN" "DIGTE" "VERS" "TALER"})))
    (is (= #{:root ["Litteratur"]} (tree/open-at-rest nodes #{"VISER"})))
    (is (= #{:root ["Litteratur"] ["Litteratur" "Folkeviser"]}
           (tree/open-at-rest nodes #{"VISER" "DIGTE"})))
    (testing "a node chosen whole is shut, and so is one with nothing:
              its box and its count say which"
      (is (= #{:root ["Litteratur"]}
             (tree/open-at-rest nodes #{"VISER" "DIGTE" "VERS"})))
      (is (= #{:root} (tree/open-at-rest nodes #{"TALER"})))))
  (testing "a leaf that cannot be chosen never leaves a node part chosen"
    (is (= #{} (tree/open-at-rest [(second nodes)] #{"TALER"})))))

(deftest matching-test
  (testing "typing opens every node holding something that answers,
            parents first, and none that does not"
    (is (= [["Litteratur"] ["Litteratur" "Folkeviser"]]
           (tree/matching "vers" nodes)))
    (is (= [["Litteratur"]] (tree/matching "anden" nodes)))
    (is (= [["Folketinget"]] (tree/matching "TALER" nodes)))
    (is (= [] (tree/matching "zzz" nodes)))))

(deftest only-chosen-test
  (let [[litteratur] (map tree/counted nodes)
        hidden       (fn [held node]
                       (->> (tree-seq coll? seq (tree/only-chosen held node))
                            (filter #(and (map? %) (:hidden? %)))
                            (map #(or (:id %) :node))))]
    (testing "a leaf not held is hidden, and so is a node with nothing
              held under it"
      (is (= ["ANDEN" ["Litteratur" "Folkeviser"] "DIGTE" "VERS"]
             (hidden #{"VISER"} litteratur))))
    (testing "a node held whole hides everything under it: its own row
              says it"
      (is (= ["VISER" "ANDEN" ["Litteratur" "Folkeviser"] "DIGTE" "VERS"]
             (hidden #{"VISER" "ANDEN" "DIGTE" "VERS"} litteratur))))
    (testing "and nothing held hides the node itself, unless something
              else is in force in it"
      (is (:hidden? (tree/only-chosen #{} litteratur)))
      (is (not (:hidden? (tree/only-chosen #{} (assoc litteratur :in-force? true))))))))

(deftest node-view-test
  (let [litteratur (tree/counted (first nodes))
        opts       {:item item :summary :label :open? (constantly true)}]
    (testing "a labelled node is a disclosure in a row of its own, open
              as told"
      (let [[tag _ [kind attrs summary]] (tree/node-view opts litteratur)]
        (is (= :div.group tag))
        (is (= :details kind))
        (is (:open attrs))
        (is (= [:summary "Litteratur"] summary)))
      (is (not (:open (second (nth (tree/node-view
                                    (assoc opts :open? (constantly false))
                                    litteratur)
                                   2))))))
    (testing "it reports its own toggling, so the reader's word on it can
              be kept"
      (is (= {:toggle [:toggle-open :corpora ["Litteratur"]]}
             (-> (tree/node-view (assoc opts :on-toggle
                                        (fn [{:keys [id]}]
                                          [:toggle-open :corpora id]))
                                 litteratur)
                 (nth 2) (second) (:on)))))
    (testing "the summary is computed, so a shut node can still count"
      (is (= ["Litteratur (1/4)" "Folkeviser (0/2)"]
             (summaries (tree/node-view
                         (assoc opts :summary (partial tree/node-summary
                                                       #{"VISER"}))
                         litteratur)))))
    (testing "a control takes its place beside the disclosure, not inside
              it, and the row is there with or without one"
      (let [[tag control disclosure]
            (tree/node-view (assoc opts :toggle (constantly [:input {}]))
                            litteratur)]
        (is (= :div.group tag))
        (is (= [:input {}] control))
        (is (= :details (first disclosure))))
      (is (= [:div.group nil]
             (take 2 (tree/node-view (assoc opts :toggle (constantly nil))
                                     litteratur))))
      (testing "and a hidden node has none: it would float beside nothing"
        (is (nil? (second (tree/node-view
                           (assoc opts :toggle (constantly [:input {}]))
                           (assoc litteratur :hidden? true)))))))
    (testing "what else the instance puts in a node comes before its list"
      (is (= [:p "extra"]
             (nth (nth (tree/node-view (assoc opts :extra (constantly [:p "extra"]))
                                       litteratur)
                       2)
                  3))))
    (testing "a label-less node is a bare list"
      (is (= :ul (first (tree/node-view opts (assoc litteratur :label nil))))))))

(deftest chooser-test
  (let [chooser (fn [opts]
                  (tree/chooser en :corpora nodes
                                (merge {:item item :legend "Corpora"
                                        :not-found "Nothing."}
                                       opts)))]
    (testing "a fieldset classed as a chooser and by the list's name, so
              one stylesheet and one client serve both instances"
      (is (= :fieldset.chooser.corpora (first (chooser {}))))
      (is (some #{[:legend "Corpora"]} (deep (chooser {})))))
    (testing "without a set of what is open it rests: part chosen opens
              the root and the node, whole and nothing shut them
              [root Litteratur Folkeviser Folketinget]"
      (is (= [true true false false] (opens (chooser {:selected #{"VISER"}}))))
      (is (= [false false false false] (opens (chooser {:selected #{}}))))
      (is (= [false false false false]
             (opens (chooser {:selected #{"VISER" "ANDEN" "DIGTE" "VERS"
                                          "TALER"}})))))
    (testing "given the set, it is that and nothing else, whatever is
              chosen and whether or not the reader is choosing"
      (is (= [true false true false]
             (opens (chooser {:selected #{} :open #{:root ["Litteratur" "Folkeviser"]}}))))
      (is (= [false false false false]
             (opens (chooser {:selected #{"VISER"} :open #{} :choosing? true})))))
    (testing "at rest only what is held is shown, a node held whole as
              one row; while choosing nothing is hidden"
      (is (= ["ANDEN" "DIGTE" "VERS" "TALER" "GONE"]
             (hidden-leaves (chooser {:selected #{"VISER"}}))))
      ;; ANDEN unticked at rest and held: its row stays
      (is (= ["DIGTE" "VERS" "TALER" "GONE"]
             (hidden-leaves (chooser {:selected #{"VISER"}
                                      :held     #{"VISER" "ANDEN"}}))))
      (is (= [] (hidden-leaves (chooser {:selected #{} :choosing? true})))))
    (testing "the summary counts the selection over what is offered"
      (is (= [:small.count "(1/5)"]
             (last (get-in (chooser {:selected #{"VISER"}}) [3 2 2]))))
      (is (= {:aria-label "1 of 5 selected"}
             (second (get-in (chooser {:selected #{"VISER"}}) [3 2 2])))))
    (testing "the controls, the box and the toggles come only with a
              client to answer them"
      (let [html (chooser {:client? true :choosing? true
                           :control (fn [offered] [:input.all {:offered (vec offered)}])
                           :toggle  (fn [{:keys [id]}] [:input.node {:id id}])})]
        (is (some #(and (map? %) (= [:filter :corpora] (get-in % [:on :input]))
                        (= [:engage :corpora] (get-in % [:on :focus]))
                        (= "corpora-filter" (:id %)))
                  (deep html)))
        (is (= [["VISER" "ANDEN" "DIGTE" "VERS" "TALER"]]
               (keep #(when (and (map? %) (:offered %)) (:offered %)) (deep html))))
        (is (= [["Litteratur"] ["Litteratur" "Folkeviser"] ["Folketinget"]]
               (keep #(when (and (vector? %) (= :input.node (first %)))
                        (:id (second %)))
                     (deep html))))
        (is (= [[:toggle-open :corpora :root]
                [:toggle-open :corpora ["Litteratur"]]
                [:toggle-open :corpora ["Litteratur" "Folkeviser"]]
                [:toggle-open :corpora ["Folketinget"]]]
               (keep #(when (map? %) (get-in % [:on :toggle])) (deep html))))
        (is (= [:leave :corpora] (get-in html [1 :on :focusout]))))
      (let [html (deep (chooser {:control (constantly [:input.all])
                                 :toggle  (constantly [:input.node])}))]
        (is (not (some #{:input.all :input.node :input.find} html)))))
    (testing "a filter hides what does not answer it at every level, and
              the count follows it"
      (let [html (chooser {:client? true :filter "taler"})]
        (is (= ["VISER" "ANDEN" "DIGTE" "VERS" "GONE"] (hidden-leaves html)))
        (is (= ["Litteratur (0/0)" "Folkeviser (0/0)" "Folketinget (0/1)"]
               (rest (summaries html))))
        (is (= [{:open   false
                 :on     {:toggle [:toggle-open :corpora ["Litteratur"]]}
                 :hidden true}]
               (filter #(and (map? %) (contains? % :open) (:hidden %)
                             (= ["Litteratur"] (get-in % [:on :toggle 2])))
                       (deep html))))))
    (testing "the region saying nothing was found is there before it says
              it, under the tree, which stays"
      (let [region (fn [opts] (last (chooser (assoc opts :client? true))))]
        (is (= [:div.empty {:role "status"} nil] (region {})))
        (is (= [:div.empty {:role "status"} nil] (region {:filter "viser"})))
        (is (= [:div.empty {:role "status"} "Nothing."] (region {:filter "zzz"})))
        (is (nil? (:hidden (get-in (chooser {:client? true :filter "zzz"})
                                   [3 2 1]))))))
    (testing "what the instance says comes after the tree, and busy marks
              the disclosure"
      (is (= [:p "after"] (last (get-in (chooser {:after [:p "after"]}) [3 2]))))
      (is (= "true" (get-in (chooser {:busy? true}) [3 2 1 :aria-busy]))))))
