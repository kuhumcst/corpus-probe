(ns dk.cst.corpus-probe.views.search.filter-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.hiccup :refer [deep]]
            [dk.cst.corpus-probe.test.hiccup :refer [da en open-states]]
            [dk.cst.corpus-probe.views.search.filter :as filter]))

(deftest pattern-row-test
  (let [years [{:value "1583"} {:value "1591"}]
        html  (filter/pattern-row en :text_year years "15.." ["1583" nil] false)]
    (testing "a pattern field under the attribute's pattern param, holding
              what is in force"
      (is (= :p.pattern (first html)))
      (is (some #(and (map? %) (= "fp.text_year" (:name %)) (= "15.." (:value %)))
                (deep html))))
    (testing "a range over values that are all numbers, either end blank
              when not in force"
      (is (some #(and (map? %) (= "ff.text_year" (:name %)) (= "1583" (:value %)))
                (deep html)))
      (is (some #(and (map? %) (= "ft.text_year" (:name %)) (= "" (:value %)))
                (deep html)))
      (testing "either end takes a whole number, and says so, so the
                browser reports anything else rather than the server
                dropping it"
        (is (= [["-?[0-9]*" "a whole number"] ["-?[0-9]*" "a whole number"]]
               (->> (deep html)
                    (filter #(and (map? %) (:pattern %)))
                    (map (juxt :pattern :title)))))
        (is (not (some #(and (map? %) (= "fp.text_year" (:name %)) (:pattern %))
                       (deep html))))))
    (testing "and no range over values that are not"
      (is (not (some #(and (map? %) (= "ff.text_title" (:name %)))
                     (deep (filter/pattern-row en :text_title
                                               [{:value "Havfruens sang"}]
                                               nil nil false)))))
      (is (not (filter/numeric-values? []))))
    (testing "hidden at rest, the row stays in the document"
      (is (= {:hidden true}
             (second (filter/pattern-row en :text_year years nil nil true)))))
    (testing "in Danish"
      (is (some #{"mønster" "fra" "til" "et helt tal"}
                (deep (filter/pattern-row da :text_year years nil nil false)))))))

(deftest filter-tree-test
  (testing "the pairs a selection names"
    (is (= #{[:a "1"] [:a "2"] [:b "x"]}
           (filter/filter-pairs {:a #{"1" "2"} :b #{"x"}}))))
  (let [[a b] (filter/filter-tree {:attrs    [{:name :a :rows [{:value "1" :total 2}]}]
                                   :unlisted [:b]
                                   :patterns {:b "x."}}
                                  #{[:a "9"]})]
    (testing "a node per listed attribute, its rows as leaves named by
              their pair, then a chosen value the rows lack"
      (is (= :a (:id a)))
      (is (= "a" (:label a)))
      (is (= [[:a "1"] [:a "9"]] (map :id (:items a))))
      (is (= ["1" "9"] (map :text (:items a))))
      (is (false? (:in-force? a))))
    (testing "then one per attribute the list lacks, in force while a
              pattern stands for it"
      (is (= :b (:id b)))
      (is (empty? (:items b)))
      (is (true? (:in-force? b))))))

(deftest filter-fieldset-test
  (testing "no metadata renders nothing, which is what the client asks
            before deciding whether fresh filters are worth fetching"
    (is (nil? (filter/filter-fieldset en nil {})))
    (is (nil? (filter/filter-fieldset en {:attrs    []
                                          :unlisted []
                                          :selected {}}
                                      {})))
    (is (false? (filter/filterable? {:attrs [] :unlisted [] :selected {}})))
    (is (true? (filter/filterable? {:attrs [] :unlisted [:text_title] :selected {}})))
    (is (true? (filter/filterable? {:attrs [] :unlisted [] :selected {:a #{"1"}}}))))
  (let [selected {:text_year  #{"1591" "1600"}
                  :text_title #{"Havfruens sang"}}
        html (filter/filter-fieldset
              "en"
              {:attrs    [{:name :text_year
                           :rows [{:value "1583" :total 1}
                                  {:value "1591" :total 2}]}
                          {:name :text_party
                           :rows [{:value "S" :total 2}]}]
               :unlisted [:text_title]
               :selected selected}
              {})
        inputs (filter #(and (map? %) (= "checkbox" (:type %))) (deep html))]
    (testing "the chooser named for the client as the values list, and
              classed for the layout that gives it the whole row"
      (is (= :fieldset.chooser.box (first html)))
      (is (= "values" (:data-list (second html))))
      (is (= "filters" (:class (second html)))))
    (testing "each value is a checkbox under the attribute's filter param"
      (is (= ["f.text_year" "f.text_year" "f.text_year" "f.text_party"
              "f.text_title"]
             (map :name inputs)))
      (is (= ["1583" "1591" "1600" "S" "Havfruens sang"] (map :value inputs))))
    (testing "chosen values are checked, whether the corpora offer them or not"
      (is (= [false true true false true] (map :checked inputs))))
    (testing "every value reports its change, so the count can be live"
      (is (every? #(= [:toggle-filter-values [(keyword (subs (:name %) 2))
                                              [(:value %)]]]
                      (get-in % [:on :change]))
                  inputs)))
    (testing "the count is of what the boxes say now, out of the values
              the fieldset offers"
      (is (some #{[:small.count "(3/5)"]} (deep html))))
    (testing "and so is what is open, the fieldset's disclosure and each
              attribute's: part of the values chosen and the rest not is
              the one state the count cannot show (see `open-at-rest`)"
      (is (= [true true]
             (open-states (filter/filter-fieldset
                           "en" {:attrs    [{:name :text_year
                                             :rows  [{:value "1591"}
                                                     {:value "1592"}]}]
                                 :unlisted []
                                 :selected {:text_year #{"1591"}}}
                           {}))))
      (is (= [false false]
             (open-states (filter/filter-fieldset
                           "en" {:attrs    [{:name :text_year
                                             :rows  [{:value "1591"}]}]
                                 :unlisted []
                                 :selected {:text_year #{"1591"}}}
                           {})))))
    (testing "the attributes not on offer are a caveat, so small print"
      (is (some #(and (vector? %) (= :small (first %))) (deep html)))
      (is (some #{[:code "text_title"]} (deep html))))
    (testing "given the set of what is open, that decides it, whatever is
              chosen: the view never opens or shuts anything itself"
      (let [open* (fn [chosen opts]
                    (open-states (filter/filter-fieldset
                                  "en" {:attrs [{:name :a
                                                 :rows [{:value "1"}
                                                        {:value "2"}]}]
                                        :unlisted [] :selected chosen}
                                  opts)))]
        (is (= [false false] (open* {} {})))
        (is (= [true true] (open* {:a #{"1"}} {})))
        ;; every value chosen is as settled as none, and says so itself
        (is (= [false false] (open* {:a #{"1" "2"}} {})))
        (is (= [true false] (open* {} {:open #{:root}})))
        (is (= [false true] (open* {:a #{"1"}} {:open #{:a}})))
        (is (= [false false] (open* {:a #{"1"}} {:open #{} :choosing? true})))))
    (testing "what is held stays in view at rest, and the pattern row only
              while something is in force in it"
      (let [hidden (fn [filters opts]
                     (->> (deep (filter/filter-fieldset
                                 "en" (merge {:attrs    [{:name :a
                                                          :rows [{:value "1"}
                                                                 {:value "2"}
                                                                 {:value "3"}]}]
                                              :unlisted []}
                                             filters)
                                 opts))
                          (filter #(and (vector? %) (map? (second %))
                                        (:hidden (second %))))
                          (map first)))]
        ;; at rest: the unchosen values and the empty pattern row
        (is (= [:p.pattern :li :li] (hidden {:selected {:a #{"1"}}} {})))
        ;; the value unticked at rest is held, so its row stays
        (is (= [:p.pattern :li] (hidden {:selected {:a #{"1"}}}
                                        {:held #{[:a "1"] [:a "2"]}})))
        ;; nothing chosen and nothing in force: the attribute goes
        (is (= [:details :p.pattern :li :li :li] (hidden {:selected {}} {})))
        ;; a pattern in force keeps its attribute and its row
        (is (= [:li :li :li] (hidden {:selected {} :patterns {:a "1."}} {})))
        (testing "and while the reader is choosing nothing is hidden"
          (is (= [] (hidden {:selected {}} {:choosing? true}))))))
    (testing "it is marked busy while its attributes are being fetched"
      (let [busy (fn [opts] (->> (deep (filter/filter-fieldset
                                        "en" {:attrs [{:name :a :rows []}]
                                              :unlisted [] :selected {}}
                                        opts))
                                 (filter #(and (map? %) (contains? % :open)))
                                 first :aria-busy))]
        (is (= "true" (busy {:pending? true})))
        (is (nil? (busy {})))))
    (testing "each attribute carries a control over the values it shows"
      (let [alls (->> (deep (filter/filter-fieldset
                             "en"
                             {:attrs    [{:name :text_year
                                          :rows [{:value "1583"}
                                                 {:value "1591"}]}]
                              :unlisted [] :selected {}}
                             {:client? true :choosing? true}))
                      (filter #(and (map? %) (contains? % :replicant/on-render))))]
        ;; the fieldset's own control comes first, then one per attribute
        (is (= ["Clear filter" "All values of text_year"]
               (map :aria-label alls)))
        (is (= [[:clear-filter] [:toggle-filter-values [:text_year ["1583" "1591"]]]]
               (map #(get-in % [:on :change]) alls)))))
    (testing "and takes only the values the filter box leaves showing"
      (let [alls (->> (deep (filter/filter-fieldset
                             "en"
                             {:attrs    [{:name :text_year
                                          :rows [{:value "1583"}
                                                 {:value "1591"}]}]
                              :unlisted [] :selected {}}
                             {:client? true :filter "1591"}))
                      (filter #(and (map? %) (contains? % :replicant/on-render))))]
        (is (= [[:clear-filter] [:toggle-filter-values [:text_year ["1591"]]]]
               (map #(get-in % [:on :change]) alls))))
      ;; and the value it hid keeps its box, so the filter is not narrowed
      (let [html (deep (filter/filter-fieldset
                        "en" {:attrs    [{:name :text_year
                                          :rows [{:value "1583"}
                                                 {:value "1591"}]}]
                              :unlisted [] :selected {:text_year #{"1583"}}}
                        {:client? true :filter "1591"}))]
        (is (some #(and (map? %) (= "1583" (:value %)) (:checked %)) html))
        (is (some #{{:hidden true}} html))))
    (testing "the region saying nothing was found is there before it says it"
      (let [region (fn [opts]
                     (->> (filter/filter-fieldset
                           "en" {:attrs    [{:name :text_year
                                             :rows [{:value "1591"}]}]
                                 :unlisted [] :selected {}}
                           (merge {:client? true} opts))
                          (deep)
                          (filter #(and (vector? %) (= :div.status (first %))))
                          first))]
        (is (= [:div.status {:class "chooser-status" :role "status"} nil]
               (region {})))
        (is (= [:div.status {:class "chooser-status" :role "status"} nil]
               (region {:filter "1591"})))
        (is (= [:div.status {:class "chooser-status" :role "status"}
                "No values found."]
               (region {:filter "zzz"})))))
    (testing "the filter box is there only with a client, and submits nothing"
      (let [box (fn [opts] (->> (deep (filter/filter-fieldset
                                       "en" {:attrs [{:name :a :rows []}]
                                             :unlisted [] :selected {}}
                                       opts))
                                (filter #(and (map? %)
                                              (= "values-filter" (:id %))))
                                first))]
        (is (nil? (box {})))
        (is (= "search" (:type (box {:client? true}))))
        ;; named by its placeholder, and in the summary of the disclosure
        ;; it narrows, as the corpus chooser's box is
        (is (= "Filter" (:placeholder (box {:client? true}))))
        (is (= "Filter" (:aria-label (box {:client? true}))))
        (is (= :input.chooser-find
               (first (nth (get-in (filter/filter-fieldset
                                    "en" {:attrs    [{:name :a :rows []}]
                                          :unlisted [] :selected {}}
                                    {:client? true})
                                   [3 2 2])
                           2))))
        (is (nil? (:name (box {:client? true}))))))
    (testing "the fieldset's own control is the chooser's, minus one half"
      (let [root (fn [selected]
                   (->> (deep (filter/filter-fieldset
                               "en" {:attrs    [{:name :a :rows [{:value "1"}
                                                                 {:value "2"}]}]
                                     :unlisted [] :selected selected}
                               {:client? true}))
                        (filter #(and (map? %) (= "Clear filter" (:aria-label %))))
                        first))]
        (testing "nothing chosen: offered, but not from the one state it
                  must not act from"
          (is (true? (:disabled (root {}))))
          (is (false? (:checked (root {}))))
          (is (= [:set-checkbox-state {:indeterminate false :invalid nil}]
                 (:replicant/on-render (root {})))))
        (testing "something chosen: live, partly checked, and it clears"
          (is (false? (:disabled (root {:a #{"1"}}))))
          (is (= [:set-checkbox-state {:indeterminate true :invalid nil}]
                 (:replicant/on-render (root {:a #{"1"}}))))
          (is (= [:clear-filter] (get-in (root {:a #{"1"}}) [:on :change]))))
        (testing "everything chosen: checked, and it still only clears"
          (is (true? (:checked (root {:a #{"1" "2"}}))))
          (is (false? (:disabled (root {:a #{"1" "2"}})))))))
    (testing "and it answers for the whole filter, not the part on show"
      ;; emptying by halves would leave a constraint the box is hiding
      (let [root (->> (deep (filter/filter-fieldset
                             "en" {:attrs    [{:name :a :rows [{:value "1"}]}
                                              {:name :b :rows [{:value "2"}]}]
                                   :unlisted [] :selected {:b #{"2"}}}
                             {:client? true :filter "a"}))
                      (filter #(and (map? %) (= "Clear filter" (:aria-label %))))
                      first)]
        (is (false? (:disabled root)))
        (is (= [:set-checkbox-state {:indeterminate true :invalid nil}]
               (:replicant/on-render root)))))
    (testing "one disclosure over the filter, open only while it is active,
              and one per attribute, open while chosen in part"
      (is (= [true true false false]
             (keep #(when (and (map? %) (contains? % :open)) (:open %))
                   (deep html)))))
    (testing "an attribute counts its selection without reopening itself"
      (is (some #{[:small.count "(2/3)"]} (deep html))))
    (testing "the whole filter counts what is chosen across attributes,
              the same two figures an attribute of it shows"
      (is (some #{[:small.count "(3/5)"]} (deep html))))
    (testing "the region counts are machine-readable, with their unit"
      (is (some #{[:data {:value "2"} "2 regions"]} (deep html)))
      (is (some #{[:data {:value "1"} "1 region"]} (deep html))))
    (testing "values render as the inspector shows them"
      (is (some #{[:time "1591"]} (deep html))))
    (testing "unlisted attributes are named"
      (is (some #{[:code "text_title"]} (deep html))))))
