(ns dk.cst.corpus-probe.client.lists-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.client.lists :as lists]))

(def folders
  [{:label   "Litteratur"
    :corpora [{:id "VISER" :title "Folkeviser" :size 48}
              {:id "DIGTE" :size 12}]
    :folders []}
   {:label nil :corpora [{:id "TALER" :size 42}] :folders []}])

(def filters
  {:attrs    [{:name :text_year
               :rows [{:value "1583" :total 1} {:value "1591" :total 2}]}
              {:name :text_party :rows [{:value "S" :total 2}]}]
   :unlisted []
   :selected {:text_year #{"1591"}}})

(defn at-rest
  "The state with `corpus` selected and both lists at rest, the root of
  the values list open when `values-open?`."
  ([corpus] (at-rest corpus true))
  ([corpus values-open?]
   (let [state {:lang            "en"
                :folders         folders
                :params          {:corpus corpus}
                :filter-controls filters
                :filters-for     (vec (sort corpus))
                :lists           {:corpora {:open      #{}
                                            :choosing? false
                                            :unticked  #{}}
                                  :values  {:open      #{}
                                            :choosing? false
                                            :unticked  #{}}}}]
     (-> state
         (lists/settle :corpora)
         (update-in [:lists :corpora :open] conj :root)
         (cond-> values-open? (update-in [:lists :values :open] conj :root))
         (lists/settle :values)))))

(deftest select-corpora-test
  (testing "added and removed, sorted, never twice"
    (is (= ["TALER" "VISER"] (lists/select-corpora ["VISER"] ["TALER"] true)))
    (is (= ["VISER"] (lists/select-corpora ["VISER" "TALER"] ["TALER"] false)))
    (is (= ["VISER"] (lists/select-corpora ["VISER"] ["VISER"] true)))
    (is (= [] (lists/select-corpora ["VISER"] ["VISER"] false)))))

(deftest choose-values-test
  (testing "a value is added, an attribute partly chosen fills, one wholly
            chosen clears, and an emptied attribute goes"
    (is (= {:text_year #{"1591"}}
           (lists/choose-values {} :text_year ["1591"])))
    (is (= {:text_year #{"1583" "1591"}}
           (lists/choose-values {:text_year #{"1591"}} :text_year
                                ["1583" "1591"])))
    (is (= {}
           (lists/choose-values {:text_year #{"1583" "1591"}} :text_year
                                ["1583" "1591"])))
    (is (= {:text_party #{"S"}}
           (lists/choose-values {:text_year #{"1591"} :text_party #{"S"}}
                                :text_year ["1591"])))))

(deftest chosen-corpora-test
  (is (= ["TALER" "VISER"]
         (lists/chosen-corpora {:params {:corpus ["VISER" "TALER"]}})))
  (is (= [] (lists/chosen-corpora {}))))

(deftest rest-and-settle-test
  (let [state (at-rest ["VISER"])]
    (testing "at rest a folder chosen in part stands open, and so does the
              root"
      (is (= #{:root ["Litteratur"]} (get-in state [:lists :corpora :open])))
      (is (= #{:root ["Litteratur"]}
             (lists/rest-open state :corpora #{"VISER"}))))
    (testing "settling keeps a root the reader shut"
      (is (= #{["Litteratur"]}
             (-> state
                 (update-in [:lists :corpora :open] disj :root)
                 (lists/settle :corpora)
                 (get-in [:lists :corpora :open])))))
    (testing "held is the selection plus what was unticked at rest"
      (is (= #{"VISER" "DIGTE"}
             (lists/held (assoc-in state [:lists :corpora :unticked]
                                   #{"DIGTE"})
                         :corpora))))))

(deftest tick-test
  (let [state (at-rest ["VISER"])]
    (testing "unticking at rest keeps the box in place, and only what the
              change leaves whole or empty shuts"
      (let [ticked (-> state
                       (assoc-in [:params :corpus] [])
                       (lists/tick :corpora ["VISER"] true))]
        (is (= #{"VISER"} (get-in ticked [:lists :corpora :unticked])))
        (is (= #{:root ["Litteratur"]}
               (get-in ticked [:lists :corpora :open])))))
    (testing "ticking again takes it out of the unticked"
      (is (= #{} (-> state
                     (assoc-in [:lists :corpora :unticked] #{"VISER"})
                     (lists/tick :corpora ["VISER"] false)
                     (get-in [:lists :corpora :unticked])))))
    (testing "while choosing nothing moves"
      (is (= #{:root ["Litteratur"]}
             (-> state
                 (lists/engage :corpora)
                 (assoc-in [:params :corpus] ["VISER" "DIGTE"])
                 (lists/tick :corpora ["DIGTE"] false)
                 (get-in [:lists :corpora :open])))))))

(deftest engage-and-leave-test
  (let [state (at-rest ["VISER"])]
    (testing "engaging opens the root and marks the reader choosing"
      (let [engaged (lists/engage (update-in state [:lists :corpora :open]
                                             disj :root)
                                  :corpora)]
        (is (true? (get-in engaged [:lists :corpora :choosing?])))
        (is (contains? (get-in engaged [:lists :corpora :open]) :root))
        (testing "and leaving settles it"
          (let [left (lists/leave engaged :corpora)]
            (is (false? (get-in left [:lists :corpora :choosing?])))
            (is (= #{} (get-in left [:lists :corpora :unticked])))))))
    (testing "a list at rest with nothing unticked is left as it is"
      (is (identical? state (lists/leave state :corpora))))
    (testing "a filter in force keeps the list as it is"
      (let [filtering (-> state
                          (lists/engage :corpora)
                          (assoc-in [:lists :corpora :filter] "dig"))]
        (is (identical? filtering (lists/leave filtering :corpora)))))))

(deftest toggle-open-test
  (let [state (at-rest ["VISER"])]
    (testing "the echo of the client's own render changes nothing"
      (is (identical? state (lists/toggle-open state :corpora :root true)))
      (is (identical? state
                      (lists/toggle-open state :corpora ["Litteratur"] true))))
    (testing "a disclosure coming open while nobody is choosing engages"
      (let [opened (lists/toggle-open state :corpora [nil] true)]
        (is (contains? (get-in opened [:lists :corpora :open]) [nil]))
        (is (true? (get-in opened [:lists :corpora :choosing?])))))
    (testing "the root shutting leaves"
      (let [shut (-> state
                     (lists/engage :corpora)
                     (lists/toggle-open :corpora :root false))]
        (is (false? (get-in shut [:lists :corpora :choosing?])))
        (is (not (contains? (get-in shut [:lists :corpora :open]) :root)))))
    (testing "a node shutting while choosing only shuts"
      (let [shut (-> state
                     (lists/engage :corpora)
                     (lists/toggle-open :corpora ["Litteratur"] false))]
        (is (true? (get-in shut [:lists :corpora :choosing?])))
        (is (not (contains? (get-in shut [:lists :corpora :open])
                            ["Litteratur"])))))))

(deftest apply-filter-test
  (let [state (update-in (at-rest ["VISER"]) [:lists :corpora :open]
                         disj ["Litteratur"])]
    (testing "typing opens every disclosure holding an answer"
      (let [narrowed (lists/apply-filter state :corpora "digte")]
        (is (= "digte" (get-in narrowed [:lists :corpora :filter])))
        (is (contains? (get-in narrowed [:lists :corpora :open])
                       ["Litteratur"]))))
    (testing "and emptying the box opens nothing"
      (is (= (get-in state [:lists :corpora :open])
             (get-in (lists/apply-filter state :corpora "")
                     [:lists :corpora :open]))))))

(deftest filters-stale?-test
  (testing "fresh while the filters describe the selection"
    (is (not (lists/filters-stale? (at-rest ["VISER"])))))
  (testing "stale once the selection has changed and the reader is looking"
    (is (lists/filters-stale? (assoc (at-rest ["VISER"])
                                     :filters-for ["TALER"]))))
  (testing "not while the filter is shut with something on show"
    (is (not (lists/filters-stale? (assoc (at-rest ["VISER"] false)
                                          :filters-for ["TALER"])))))
  (testing "but always when nothing is on show at all"
    (is (lists/filters-stale? (-> (at-rest ["VISER"] false)
                                  (assoc :filters-for ["TALER"])
                                  (assoc :filter-controls {})))))
  (testing "and never for no corpora"
    (is (not (lists/filters-stale? (assoc (at-rest []) :filters-for
                                          ["TALER"]))))))
