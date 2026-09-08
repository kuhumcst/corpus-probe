(ns dk.cst.corpus-probe.views.chooser-test
  "The chooser both fieldsets are: its rules over a small tree, and the
  markup of one node and of the whole."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.hiccup :refer [deep]]
            [dk.cst.corpus-probe.test.hiccup
             :refer [en hidden-ids open-states summary-texts]]
            [dk.cst.corpus-probe.views.chooser :as chooser]))

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

(deftest narrow-test
  (let [[litteratur folketinget] nodes]
    (testing "what does not answer is marked, not removed"
      (let [narrowed (chooser/narrow "anden" litteratur)]
        (is (:hidden? (first (:items narrowed))))
        (is (not (:hidden? (second (:items narrowed)))))
        (is (:hidden? (first (:nodes narrowed))))
        ;; still in the tree, so still in the document and the search
        (is (= 2 (count (:items narrowed))))))
    (testing "a leaf answers by its text, a folder by its label"
      (is (not (:hidden? (first (:items (chooser/narrow "folkev" litteratur))))))
      (is (not (:hidden? (chooser/narrow "litt" litteratur)))))
    (testing "naming a node asks for everything in it"
      (let [narrowed (chooser/narrow "folkeviser" litteratur)]
        (is (not (:hidden? (first (:nodes narrowed)))))
        (is (not (:hidden? (first (:items (first (:nodes narrowed)))))))))
    (testing "and nothing anywhere marks the whole node"
      (is (:hidden? (chooser/narrow "zzz" folketinget))))))

(deftest counted-test
  (let [[litteratur folketinget] (map chooser/counted nodes)]
    (testing "a node offers what can be chosen in it, the nodes under it
              included, and a disabled leaf left out"
      (is (= ["VISER" "ANDEN" "DIGTE" "VERS"] (:offered litteratur)))
      (is (= ["DIGTE" "VERS"] (:offered (first (:nodes litteratur)))))
      (is (= ["TALER"] (:offered folketinget))))
    (testing "and not what a filter has hidden"
      (is (= ["ANDEN"]
             (:offered (chooser/counted (chooser/narrow "anden" litteratur))))))))

(deftest open-at-rest-test
  (testing "the nodes open at rest are exactly those chosen in part"
    (is (= #{} (chooser/open-at-rest nodes #{})))
    (is (= #{:root ["Litteratur"]} (chooser/open-at-rest nodes #{"VISER"})))
    (is (= #{:root ["Litteratur"] ["Litteratur" "Folkeviser"]}
           (chooser/open-at-rest nodes #{"VISER" "DIGTE"})))
    (testing "a node chosen whole is shut, and so is one with nothing:
              its box and its count say which"
      (is (= #{:root ["Litteratur"]}
             (chooser/open-at-rest nodes #{"VISER" "DIGTE" "VERS"})))
      (is (= #{:root} (chooser/open-at-rest nodes #{"TALER"})))))
  (testing "the root stands open whenever the resting view shows
            anything, a whole selection included: it has no label of its
            own, so shut it would name nothing that is chosen"
    (is (= #{:root}
           (chooser/open-at-rest nodes #{"VISER" "ANDEN" "DIGTE" "VERS"
                                         "TALER"})))
    (is (= #{:root} (chooser/open-at-rest [(second nodes)] #{"TALER"}))))
  (testing "and over a node in force with nothing chosen under it, which
            the resting view shows too"
    (let [in-force [(assoc (second nodes) :in-force? true)]]
      (is (= #{} (chooser/open-at-rest [(second nodes)] #{})))
      (is (= #{:root} (chooser/open-at-rest in-force #{})))))
  (testing "a leaf that cannot be chosen never leaves a node part chosen"
    (is (= #{:root} (chooser/open-at-rest [(second nodes)] #{"TALER"})))))

(deftest matching-test
  (testing "typing opens every node holding something that answers,
            parents first, and none that does not"
    (is (= [["Litteratur"] ["Litteratur" "Folkeviser"]]
           (chooser/matching "vers" nodes)))
    (is (= [["Litteratur"]] (chooser/matching "anden" nodes)))
    (is (= [["Folketinget"]] (chooser/matching "TALER" nodes)))
    (is (= [] (chooser/matching "zzz" nodes)))))

(deftest only-chosen-test
  (let [[litteratur] (map chooser/counted nodes)
        hidden       (fn [held node]
                       (->> (deep (chooser/only-chosen held node))
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
    (testing "but a node without a label has no row to say it with, so
              its held leaves stay on show"
      (let [bare (chooser/counted (dissoc litteratur :label))]
        ;; VISER and ANDEN stay; the labelled node under them still
        ;; collapses, having a row of its own
        (is (= ["DIGTE" "VERS"] (hidden #{"VISER" "ANDEN" "DIGTE" "VERS"}
                                        bare)))))
    (testing "and nothing held hides the node itself, unless something
              else is in force in it"
      (is (:hidden? (chooser/only-chosen #{} litteratur)))
      (is (not (:hidden? (chooser/only-chosen #{} (assoc litteratur :in-force? true))))))))

(deftest node-view-test
  (let [litteratur (chooser/counted (first nodes))
        opts       {:item item :summary :label :open? (constantly true)}]
    (testing "a labelled node is a disclosure in a row of its own, open
              as told"
      (let [[tag _ [kind attrs summary]] (chooser/node-view opts litteratur)]
        (is (= :div.chooser-group tag))
        (is (= :details kind))
        (is (:open attrs))
        (is (= [:summary.chooser-summary "Litteratur"] summary)))
      (is (not (:open (second (nth (chooser/node-view
                                    (assoc opts :open? (constantly false))
                                    litteratur)
                                   2))))))
    (testing "it reports its own toggling, so the reader's word on it can
              be kept"
      (is (= {:toggle [:toggle-open :corpora ["Litteratur"]]}
             (-> (chooser/node-view (assoc opts :on-toggle
                                           (fn [{:keys [id]}]
                                             [:toggle-open :corpora id]))
                                    litteratur)
                 (nth 2) (second) (:on)))))
    (testing "the summary is computed, so a shut node can still count"
      (is (= ["Litteratur (1/4)" "Folkeviser (0/2)"]
             (summary-texts (chooser/node-view
                             (assoc opts :summary (partial chooser/node-summary
                                                           en nil #{"VISER"}))
                             litteratur)))))
    (testing "and the figures say aloud what they count, where the list
              says what that is"
      (is (= [:small.note {:title "1 of 4 corpora selected"} "(1/4)"]
             (last (chooser/node-summary en (constantly "corpora") #{"VISER"}
                                         (chooser/counted litteratur)))))
      (is (= [:small.note {:title "1 of 4 selected"} "(1/4)"]
             (last (chooser/node-summary en nil #{"VISER"}
                                         (chooser/counted litteratur))))))
    (testing "and a node narrowed by something other than its boxes is
              marked, the figures counting boxes alone"
      (let [in-force (assoc (chooser/counted litteratur) :in-force? true)
            title    (fn [selected]
                       (-> (chooser/node-summary en (constantly "corpora")
                                                 selected in-force)
                           (last) (second) (:title)))]
        (is (= [:small.note {:title "active filter"}
                "(0/4)" [:span {:aria-hidden "true"} "*"]]
               (last (chooser/node-summary en (constantly "corpora")
                                           #{} in-force))))
        (testing "the figures worded beside it only where boxes are ticked
                  too: none of them beside a filter says nothing"
          (is (= "active filter" (title #{})))
          (is (= "1 of 4 corpora selected + active filter" (title #{"VISER"}))))))
    (testing "a control takes its place beside the disclosure, not inside
              it, and the row is there with or without one"
      (let [[tag control disclosure]
            (chooser/node-view (assoc opts :toggle (constantly [:input {}]))
                               litteratur)]
        (is (= :div.chooser-group tag))
        (is (= [:input {}] control))
        (is (= :details (first disclosure))))
      (is (= [:div.chooser-group nil]
             (take 2 (chooser/node-view (assoc opts :toggle (constantly nil))
                                        litteratur))))
      (testing "and a hidden node has none: it would float beside nothing"
        (is (nil? (second (chooser/node-view
                           (assoc opts :toggle (constantly [:input {}]))
                           (assoc litteratur :hidden? true)))))))
    (testing "what else the instance puts in a node comes before its list"
      (is (= [:p "extra"]
             (nth (nth (chooser/node-view (assoc opts :extra (constantly [:p "extra"]))
                                          litteratur)
                       2)
                  3))))
    (testing "the list is classed, so a value list of hundreds can scroll"
      ;; after the summary and the extra's place, held even when empty
      (is (= :ul.chooser-list
             (first (nth (nth (chooser/node-view opts litteratur) 2) 4)))))
    (testing "a label-less node is a bare list"
      (is (= :ul.chooser-list
             (first (chooser/node-view opts (assoc litteratur :label nil))))))))

(deftest chooser-test
  (let [chooser (fn [opts]
                  (chooser/chooser en :corpora nodes
                                   (merge {:item item :legend "Corpora"
                                           :not-found "Nothing."}
                                          opts)))]
    (testing "a fieldset classed as a chooser and a box, named for the
              client by the list's name in a data attribute, so one
              stylesheet and one client serve both instances"
      (is (= :fieldset.chooser.box (first (chooser {}))))
      (is (= "corpora" (:data-list (second (chooser {})))))
      (is (not (contains? (second (chooser {})) :class)))
      (is (some #{[:legend "Corpora"]} (deep (chooser {})))))
    (testing "classed by the instance where a layout places it"
      (is (= "filters" (:class (second (chooser {:class "filters"}))))))
    (testing "without a set of what is open it rests: part chosen opens
              the root and the node, whole shuts the node but leaves the
              root open over it, nothing shuts both
              [root Litteratur Folkeviser Folketinget]"
      (is (= [true true false false]
             (open-states (chooser {:selected #{"VISER"}}))))
      (is (= [false false false false]
             (open-states (chooser {:selected #{}}))))
      (is (= [true false false false]
             (open-states (chooser {:selected #{"VISER" "ANDEN" "DIGTE" "VERS"
                                                "TALER"}})))))
    (testing "given the set, it is that and nothing else, whatever is
              chosen and whether or not the reader is choosing"
      (is (= [true false true false]
             (open-states
              (chooser {:selected #{}
                        :open     #{:root ["Litteratur" "Folkeviser"]}}))))
      (is (= [false false false false]
             (open-states
              (chooser {:selected #{"VISER"} :open #{} :choosing? true})))))
    (testing "at rest only what is held is shown, a node held whole as
              one row; while choosing nothing is hidden"
      (is (= ["ANDEN" "DIGTE" "VERS" "TALER" "GONE"]
             (hidden-ids (chooser {:selected #{"VISER"}}))))
      ;; ANDEN unticked at rest and held: its row stays
      (is (= ["DIGTE" "VERS" "TALER" "GONE"]
             (hidden-ids (chooser {:selected #{"VISER"}
                                   :held     #{"VISER" "ANDEN"}}))))
      (is (= [] (hidden-ids (chooser {:selected #{} :choosing? true})))))
    (testing "the summary counts the selection over what is offered"
      (is (= [:small.note {:title "1 of 5 selected"} "(1/5)"]
             (last (get-in (chooser {:selected #{"VISER"}}) [3 2 2]))))
      (is (= :summary.chooser-summary
             (first (get-in (chooser {:selected #{"VISER"}}) [3 2 2]))))
      (is (= {:aria-label "1 of 5 selected"}
             (second (get-in (chooser {:selected #{"VISER"}}) [3 2 2])))))
    (testing "the controls, the box and the toggles come only with a
              client to answer them"
      (let [html (chooser {:client? true :choosing? true
                           :control (fn [offered] [:input.all {:offered (vec offered)}])
                           :toggle  (fn [{:keys [id]}] [:input.node {:id id}])})]
        (is (some #(and (map? %)
                        (= [:filter :corpora :event.target/value]
                           (get-in % [:on :input]))
                        (= [:engage :corpora] (get-in % [:on :focus]))
                        (= "corpora-filter" (:id %)))
                  (deep html)))
        (is (= [["VISER" "ANDEN" "DIGTE" "VERS" "TALER"]]
               (keep #(when (and (map? %) (:offered %)) (:offered %)) (deep html))))
        (is (= [["Litteratur"] ["Litteratur" "Folkeviser"] ["Folketinget"]]
               (keep #(when (and (vector? %) (= :input.node (first %)))
                        (:id (second %)))
                     (deep html))))
        (is (= [[:toggle-open :corpora :root :event.target/open]
                [:toggle-open :corpora ["Litteratur"] :event.target/open]
                [:toggle-open :corpora ["Litteratur" "Folkeviser"]
                 :event.target/open]
                [:toggle-open :corpora ["Folketinget"] :event.target/open]]
               (keep #(when (map? %) (get-in % [:on :toggle])) (deep html))))
        (is (= [:leave :corpora :event/focus-left?]
               (get-in html [1 :on :focusout]))))
      (let [html (deep (chooser {:control (constantly [:input.all])
                                 :toggle  (constantly [:input.node])}))]
        (is (not (some #{:input.all :input.node :input.chooser-find} html)))))
    (testing "a filter hides what does not answer it at every level, and
              the count follows it"
      (let [html (chooser {:client? true :filter "taler"})]
        (is (= ["VISER" "ANDEN" "DIGTE" "VERS" "GONE"] (hidden-ids html)))
        (is (= ["Litteratur (0/0)" "Folkeviser (0/0)" "Folketinget (0/1)"]
               (rest (summary-texts html))))
        (is (= [{:open   false
                 :on     {:toggle [:toggle-open :corpora ["Litteratur"]
                                   :event.target/open]}
                 :hidden true}]
               (filter #(and (map? %) (contains? % :open) (:hidden %)
                             (= ["Litteratur"] (get-in % [:on :toggle 2])))
                       (deep html))))))
    (testing "the region saying nothing was found is there before it says
              it, under the tree, which stays"
      (let [region (fn [opts] (last (chooser (assoc opts :client? true))))]
        (is (= [:div.status {:class "chooser-status" :role "status"} nil]
               (region {})))
        (is (= [:div.status {:class "chooser-status" :role "status"} nil]
               (region {:filter "viser"})))
        (is (= [:div.status {:class "chooser-status" :role "status"} "Nothing."]
               (region {:filter "zzz"})))
        (is (nil? (:hidden (get-in (chooser {:client? true :filter "zzz"})
                                   [3 2 1]))))))
    (testing "what the instance says comes after the tree, and busy marks
              the disclosure"
      (is (= [:p "after"] (last (get-in (chooser {:after [:p "after"]}) [3 2]))))
      (is (= "true" (get-in (chooser {:busy? true}) [3 2 1 :aria-busy]))))))
