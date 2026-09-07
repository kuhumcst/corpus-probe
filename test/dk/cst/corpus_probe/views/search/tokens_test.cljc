(ns dk.cst.corpus-probe.views.search.tokens-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.hiccup :refer [deep]]
            [dk.cst.corpus-probe.test.hiccup :refer [da en]]
            [dk.cst.corpus-probe.views.search :as search]
            [dk.cst.corpus-probe.views.search.tokens :as tokens]))

(deftest labels-test
  (testing "the usual attributes are named in the reader's words, any
            other as the corpus names it"
    (is (= "word" (tokens/attribute-label en "word")))
    (is (= "ord" (tokens/attribute-label da "word")))
    (is (= "msd2" (tokens/attribute-label da "msd2"))))
  (testing "every operator is named, equality for one nothing knows"
    (is (= "starts with" (tokens/operator-label en "prefix")))
    (is (= "any word" (tokens/operator-label en "any")))
    (is (= "is" (tokens/operator-label en "nonesuch"))))
  (testing "the datalist of an attribute is named for it"
    (is (= "values-pos" (tokens/value-list-id :pos)))
    (is (= "values-pos" (tokens/value-list-id "pos")))))

(deftest attribute-options-test
  (let [options (fn [attrs selected]
                  (map (juxt :value :selected)
                       (filter map? (deep (tokens/attribute-options
                                           en attrs selected)))))]
    (is (= [["word" true] ["pos" false] ["lemma" false]]
           (options [:word :pos :lemma] "")))
    (is (= [["word" false] ["pos" false] ["lemma" true]]
           (options [:word :pos :lemma] "lemma")))
    (testing "and one the list lacks is offered rather than replaced"
      (is (= [["word" false] ["pos" false] ["lemma" false] ["msd" true]]
             (options [:word :pos :lemma] "msd"))))))

(deftest extended-form-test
  (let [state {:lang         "en"
               :folders      [{:label nil :folders []
                               :corpora [{:id "PROBE" :size 47}]}]
               :search-attrs [:word :pos :lemma]
               :value-lists  {:pos ["N" "V"]}
               :params       {:corpus ["PROBE"] :mode "extended" :q "hund"}
               :tokens       [{:id 1 :conditions [{:id 1 :attr "lemma" :v "hund"
                                                   :ci "on"}
                                                  {:id 2 :join "or" :attr "pos"
                                                   :v "N"}]}
                              {:id 2 :min "0" :max "2" :start "on"
                               :conditions [{:id 1 :op "any"}]}
                              {:id 3 :conditions [{:id 1}]}]}
        form  (fn [state] (search/search-form state "/" nil nil))
        html  (form state)
        named (fn [html n]
                (some #(when (and (map? %) (= n (:name %))) %) (deep html)))
        options (fn [html n]
                  (->> (deep (some #(when (and (vector? %)
                                               (#{:select :select.condition-join}
                                                (first %))
                                               (= n (:name (second %))))
                                      %)
                                   (deep html)))
                       (filter #(and (map? %) (contains? % :selected)))))
        chosen (fn [html n]
                 (map :value (filter :selected (options html n))))]
    (testing "the tokens stand where the query field would, their fields
              numbered by token and, after the first, by condition"
      (is (not (some #(and (map? %) (= "q" (:id %))) (deep html))))
      (is (= "hund" (:value (named html "t1.v"))))
      (is (:checked (named html "t1.ci")))
      (is (= "N" (:value (named html "t1.2.v"))))
      (is (= ["or"] (chosen html "t1.2.join")))
      (is (nil? (named html "t1.join")))
      (is (= "0" (:value (named html "t2.min"))))
      (is (:checked (named html "t2.start")))
      (is (not (:checked (named html "t2.end"))))
      (is (= "1" (:value (named html "t3.min")))))
    (testing "the tokens are an ordered list of boxes, each named by its
              number, each keyed by its token rather than its place, and
              so are the conditions within one"
      (is (some #{:ol} (deep html)))
      (is (= [1 1 2 2 1 3 1]
             (keep #(when (and (vector? %) (#{:li :li.condition} (first %)))
                      (:replicant/key (second %)))
                   (deep html))))
      (is (= [[:legend "Token 1"] [:legend "Token 2"] [:legend "Token 3"]]
             (filter #(and (vector? %) (= :legend (first %))
                           (str/starts-with? (str (second %)) "Token"))
                     (deep html))))
      (is (= 3 (count (filter #{:fieldset.token-box.box} (deep html))))))
    (testing "a value field suggests the values of an attribute that has a
              list of them"
      (is (some #(and (vector? %) (= :datalist (first %))
                      (= "values-pos" (:id (second %))))
                (deep html)))
      (is (some #{[:option {:value "V"}]} (deep html)))
      (is (= "values-pos" (:list (named html "t1.2.v"))))
      (is (nil? (:list (named html "t1.v")))))
    (testing "every token must be filled but the blank last one, and never
              an any-word token, whose value is dead"
      (let [required (fn [state]
                       (->> (deep (form state))
                            (filter #(and (map? %) (:name %)
                                          (str/ends-with? (:name %) ".v")))
                            (map (comp boolean :required))))]
        (is (= [true false false false] (required state)))
        (testing "a lone token must be, or nothing could be asked"
          (is (= [true] (required (assoc state :tokens
                                         [{:id 1 :conditions [{:id 1}]}]))))
          (is (= [true] (required (dissoc state :tokens)))))
        (testing "except from the frequency view, which counts every token"
          (is (= [false false false false]
                 (required (assoc state :view :frequencies)))))
        (testing "with the client, which shows no blank token and adds
                  tokens and conditions by a button, every one must be"
          (is (= [true true false true]
                 (required (assoc state :client? true)))))))
    (testing "every control records itself, so the state holds the tokens
              as typed"
      (is (= {:input [:set-condition [1 1 :v]]} (:on (named html "t1.v"))))
      (is (= {:change [:set-condition [1 1 :ci]]} (:on (named html "t1.ci"))))
      (is (= {:change [:set-condition [1 2 :join]]}
             (:on (named html "t1.2.join"))))
      (is (= {:input [:set-token [2 :max]]} (:on (named html "t2.max"))))
      (is (= {:change [:set-token [2 :start]]} (:on (named html "t2.start")))))
    (testing "an any-word token has nothing to say beyond its first operator"
      (is (:disabled (named html "t2.v")))
      (is (:disabled (named html "t2.attr")))
      (is (not (:disabled (named html "t2.op"))))
      (is (not (:disabled (named html "t1.v")))))
    (testing "the operators are offered by name, the condition's chosen, and
              any word only to a token's first condition"
      (is (= ["is"] (chosen html "t1.op")))
      (is (= ["any"] (chosen html "t2.op")))
      (is (some #{"starts with"} (deep html)))
      (is (some #{"any word"} (deep html)))
      (is (some #{"any"} (map :value (options html "t1.op"))))
      (is (not (some #{"any"} (map :value (options html "t1.2.op"))))))
    (testing "the repeat pair is a group, so its second number, labelled
              only to, is heard in context, and the parts are classed for
              the stylesheet"
      (is (some #{[:span.token-repeat {:role "group" :aria-label "repeat"}
                   [:label "repeat" " "
                    [:input {:type "number" :name "t1.min" :value "1"
                             :min  0 :max 99
                             :on   {:input [:set-token [1 :min]]}}]]
                   " "
                   [:label "to" " "
                    [:input {:type "number" :name "t1.max" :value "1"
                             :min  0 :max 99
                             :on   {:input [:set-token [1 :max]]}}]]]}
                (deep html)))
      (is (= 6 (count (filter #{:label.token-edges} (deep html)))))
      (is (some #{:select.condition-join} (deep html)))
      (is (some #{:input.condition-value} (deep html))))
    (testing "the unit the tokens are kept within is chosen among the
              options, and is offered"
      (is (= ["sentence"] (chosen html "within")))
      (is (not (:disabled (named html "within"))))
      (is (= ["paragraph"]
             (chosen (form (assoc-in state [:params :within] "paragraph"))
                     "within"))))
    (testing "the simple options are not offered at all, an extended query
              writing its own"
      (is (empty? (->> (deep html)
                       (filter #(and (map? %)
                                     (#{"in" "ci" "match"} (:name %))))))))
    (testing "the mode is marked"
      (is (some #(and (map? %) (= "mode" (:name %)) (= "extended" (:value %))
                      (:checked %))
                (deep html))))
    (testing "no buttons to add or take away a token or a condition without
              the client"
      (is (not (some #{"Add token"} (deep html))))
      (is (not (some #{"Add condition"} (deep html))))
      (is (not (some #{"×"} (deep html))))
      (is (not (some #{:p.token-actions} (deep html)))))
    (testing "with it, all of them, and the search button after the tokens"
      (let [live (deep (form (assoc state :client? true)))]
        (is (some #{"Add token"} live))
        (is (= 3 (count (filter #{"Add condition"} live))))
        (is (= 3 (count (filter #{:p.token-actions} live))))
        (is (some #{"Remove token 2"} live))
        (testing "a condition can be taken away while its token has another,
                  and the row says so for the stylesheet"
          (is (= ["Remove condition 1" "Remove condition 2"]
                 (filter #(and (string? %)
                               (str/starts-with? % "Remove condition"))
                         live)))
          (is (= 2 (count (filter #(and (map? %) (= "removable" (:class %)))
                                  live))))
          (is (= 2 (count (filter #{:button.condition-remove} live)))))
        (is (< (.indexOf (vec live) :fieldset.tokens)
               (.indexOf (vec live) "Search")))))
    (testing "a client that just switched to the mode gets one blank token"
      (is (= 1 (count (filter #(and (vector? %) (= :fieldset.token-box.box (first %)))
                              (deep (form (dissoc state :tokens))))))))
    (testing "in Danish"
      (let [da (deep (form (assoc state :ui da)))]
        (is (some #{"Udvidet"} da))
        (is (some #{"begynder med"} da))
        (is (some #{"ethvert ord"} da))
        (is (some #{"eller"} da))
        (is (some #{"sætningsbegyndelse"} da))))))

(deftest add-token-row-test
  (testing "the search button alone without a client"
    (is (= [:p nil [:button {:type "submit"} "Search"]]
           (tokens/add-token-row en false [:button {:type "submit"} "Search"]))))
  (testing "and the button adding a token before it with one"
    (let [html (tokens/add-token-row en true [:button {:type "submit"} "Search"])]
      (is (some #{[:button {:type "button" :on {:click [:add-token]}} "Add token"]}
                (deep html)))
      (is (= [:button {:type "submit"} "Search"] (last html))))))
