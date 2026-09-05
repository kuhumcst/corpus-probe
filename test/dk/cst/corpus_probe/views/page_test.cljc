(ns dk.cst.corpus-probe.views.page-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.views.hiccup :refer [da deep en]]
            [dk.cst.corpus-probe.commands :as commands]
            [dk.cst.corpus-probe.views.layout :as layout]
            [dk.cst.corpus-probe.views.page :as page]))

(defn text
  "The strings of hiccup `x`, joined: what it reads as."
  [x]
  (apply str (filter string? (tree-seq coll? seq x))))

(deftest search-form-test
  (let [state {:lang         "en"
               :folders      [{:label nil :folders []
                               :corpora [{:id "PROBE" :size 47}]}]
               :search-attrs [:word :pos :lemma]
               :params       {:corpus ["PROBE"] :q "hund" :sort "word"}}
        html  (page/search-form state "/" nil)]
    (testing "the form is wrapped in a <search> landmark"
      (is (= :search (first html))))
    (testing "the form submits to the given action"
      (is (= "/" (get-in html [1 1 :action]))))
    (testing "the selected corpus is checked in the chooser"
      (is (some #(and (map? %) (= "corpus" (:name %)) (:checked %))
                (deep html))))
    (testing "the query field is a search input"
      (is (some #{"search"} (deep html))))
    (testing "the query is required, except from the frequency view, which
              counts every token of a blank one"
      (let [required (fn [state]
                       (->> (deep (page/search-form state "/" nil))
                            (filter #(and (map? %) (= "q" (:id %))))
                            (first)
                            (:required)))]
        (is (true? (required state)))
        (is (false? (required (assoc state :view :frequencies))))))
    (testing "grouped controls have legends"
      (is (some #{:legend} (deep html))))
    (testing "the example is the one for the mode being searched in"
      (is (some #(and (map? %) (= "q" (:id %))
                      (= "one word, or several words in order" (:placeholder %)))
                (deep html))))
    (testing "the selection the reader made is marked as one they made"
      (is (some #{{:type "hidden" :name "scope" :value "chosen"}}
                (deep html))))
    (testing "the sort is not here: it orders a result, it does not ask one"
      (is (not (some #{"sort"} (deep html)))))
    (testing "no language travels with the search: it is not part of one"
      (is (not (some #(and (map? %) (= "lang" (:name %))) (deep html)))))
    (testing "extra hidden inputs ride along with the search"
      (is (some #{{:type "hidden" :name "attr" :value "word"}}
                (deep (page/search-form
                       state "/" [:input {:type  "hidden" :name "attr"
                                          :value "word"}])))))
    (testing "the query options are one group, under the query field"
      (let [order (fn [x] (.indexOf (vec (deep html)) x))]
        (is (< (order :input) (order :fieldset.query-options)))
        (is (< (order :fieldset.query-options) (order :fieldset.corpora)))
        ;; the two boxes said the same thing twice; the groups they named
        ;; are still named, without a box each
        (is (not (some #{:fieldset.mode :fieldset.options} (deep html))))
        (is (some #{{:role "radiogroup" :aria-label "Query mode"}}
                  (deep html)))
        (is (some #{{:role "group" :aria-label "Simple-search options"}}
                  (deep html)))
        (testing "with what a simple search matches on a row of its own,
                  above how loosely"
          (let [group (some #(when (and (vector? %) (map? (second %))
                                        (= "group" (:role (second %))))
                               %)
                            (deep html))]
            (is (= :div (first group)))
            (is (= [:p :p] (map first (drop 2 group))))))))
    (testing "and the options are live only for a query they can qualify"
      (let [disabled (fn [mode]
                       (->> (deep (page/search-form
                                   (assoc-in state [:params :mode] mode)
                                   "/" nil))
                            (filter #(and (map? %)
                                          (#{"in" "ci" "match"} (:name %))))
                            (map :disabled)))]
        (is (= [false false false] (disabled "simple")))
        (is (= [true true true] (disabled "cqp")))))
    (testing "a simple search matches one of the corpora's attributes,
              the surface form unless the URL says otherwise"
      (let [options (fn [state]
                      (->> (deep (page/search-form state "/" nil))
                           (filter #(and (vector? %) (= :select (first %))
                                         (= "in" (:name (second %)))))
                           (first)
                           (deep)
                           (filter #(and (map? %) (contains? % :selected)))
                           (map (juxt :value :selected))))]
        (is (= [["word" true] ["pos" false] ["lemma" false]]
               (options state)))
        (is (= [["word" false] ["pos" false] ["lemma" true]]
               (options (assoc-in state [:params :in] "lemma"))))
        (testing "and one the list lacks is offered rather than replaced"
          (is (= [["word" false] ["pos" false] ["lemma" false] ["msd" true]]
                 (options (assoc-in state [:params :in] "msd")))))))
    (testing "a disabled fieldset submits nothing, so no simple option
              rides along with a CQP query"
      ;; the checkboxes are still there and still ticked, so looking at
      ;; CQP and coming back does not lose what the reader had chosen
      (let [html (deep (page/search-form
                        (-> state
                            (assoc-in [:params :mode] "cqp")
                            (assoc-in [:params :ci] "on"))
                        "/" nil))]
        (is (some #(and (map? %) (= "ci" (:name %)) (:checked %)
                        (:disabled %))
                  html))))
    (testing "no status region without a client to put anything in it"
      (is (not (some #{:div.status} (deep html)))))
    (testing "with one it follows the form, inside the same landmark"
      (let [live (page/search-form (assoc state :client? true :pending? true)
                                   "/" nil)]
        (is (= :div.status (first (last live))))
        (is (some #{"Loading …"} (deep live)))))
    (testing "the same form in Danish"
      (let [da (deep (page/search-form (assoc state :ui da) "/" nil))]
        (is (some #{"Søg"} da))
        (is (some #{"Søgeudtryk"} da))
        (is (not (some #(and (map? %) (= "lang" (:name %))) da)))))))

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
        html  (page/search-form state "/" nil)
        named (fn [html n]
                (some #(when (and (map? %) (= n (:name %))) %) (deep html)))
        options (fn [html n]
                  (->> (deep (some #(when (and (vector? %) (= :select (first %))
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
    (testing "the tokens are an ordered list of groups, each named by its
              number, each keyed by its token rather than its place, and
              so are the conditions within one"
      (is (some #{:ol} (deep html)))
      (is (= [1 1 2 2 1 3 1]
             (keep #(when (and (vector? %) (= :li (first %)))
                      (:replicant/key (second %)))
                   (deep html))))
      (is (= [[:legend "Token 1"] [:legend "Token 2"] [:legend "Token 3"]]
             (filter #(and (vector? %) (= :legend (first %))
                           (str/starts-with? (str (second %)) "Token"))
                     (deep html)))))
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
                       (->> (deep (page/search-form state "/" nil))
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
              only to, is heard in context"
      (is (some #{{:role "group" :aria-label "repeat"}} (deep html))))
    (testing "the unit the tokens are kept within is chosen beside them"
      (is (= ["sentence"] (chosen html "within")))
      (is (= ["paragraph"]
             (chosen (page/search-form (assoc-in state [:params :within]
                                                 "paragraph")
                                       "/" nil)
                     "within"))))
    (testing "the simple options are dead, as for a CQP query"
      (is (= [true true true]
             (->> (deep html)
                  (filter #(and (map? %) (#{"in" "ci" "match"} (:name %))))
                  (map :disabled)))))
    (testing "the mode is marked"
      (is (some #(and (map? %) (= "mode" (:name %)) (= "extended" (:value %))
                      (:checked %))
                (deep html))))
    (testing "no buttons to add or take away a token or a condition without
              the client"
      (is (not (some #{"Add token"} (deep html))))
      (is (not (some #{"Add condition"} (deep html))))
      (is (not (some #{"×"} (deep html)))))
    (testing "with it, all of them, and the search button after the tokens"
      (let [live (deep (page/search-form (assoc state :client? true) "/" nil))]
        (is (some #{"Add token"} live))
        (is (= 3 (count (filter #{"Add condition"} live))))
        (is (some #{"Remove token 2"} live))
        (testing "a condition can be taken away while its token has another"
          (is (= ["Remove condition 1" "Remove condition 2"]
                 (filter #(and (string? %)
                               (str/starts-with? % "Remove condition"))
                         live))))
        (is (< (.indexOf (vec live) :fieldset.tokens)
               (.indexOf (vec live) "Search")))))
    (testing "a client that just switched to the mode gets one blank token"
      (is (= 1 (count (filter #(and (vector? %) (= :fieldset.token (first %)))
                              (deep (page/search-form
                                     (dissoc state :tokens) "/" nil)))))))
    (testing "the heading names the CQP the tokens compiled to"
      (is (= [:code "[lemma = \"hund\"]"]
             (page/query-mark en {:mode "extended" :cqp "[lemma = \"hund\"]"})))
      (is (= "6 hits for [lemma = \"hund\"]"
             (text (page/hits-heading en {:mode    "extended"
                                          :t1.attr "lemma"
                                          :t1.v    "hund"
                                          :cqp     "[lemma = \"hund\"]"}
                                      6))))
      (testing "and every token when no token asked for anything"
        (is (= "All tokens" (page/hits-heading en {:mode "extended"} 47)))
        (is (page/asked? {:q "hund"}))
        (is (not (page/asked? {:q "hund" :mode "extended"}))))
      (is (= "[lemma = \"hund\"]"
             (page/query-phrase en {:mode "extended" :cqp "[lemma = \"hund\"]"}))))
    (testing "in Danish"
      (let [da (deep (page/search-form (assoc state :ui da) "/" nil))]
        (is (some #{"Udvidet"} da))
        (is (some #{"begynder med"} da))
        (is (some #{"ethvert ord"} da))
        (is (some #{"eller"} da))
        (is (some #{"sætningsbegyndelse"} da))))))

(deftest guide-test
  (let [blocks [[:h1 {:id "query-help"} "Query help"]
                [:ul [:li [:code "\"hund\""] " finds a word form."]]]
        html   (page/guide blocks)]
    (testing "a region named by the guide's own heading, holding it"
      (is (= [:section.help {:aria-labelledby "query-help"} blocks] html)))
    (testing "a guide without a heading is a section without a name"
      (is (= [:section.help {:aria-labelledby nil} [[:p "x"]]]
             (page/guide [[:p "x"]]))))
    (testing "no guide, no section"
      (is (nil? (page/guide nil)))
      (is (nil? (page/guide []))))
    (testing "it is not in the form: it stands where the results will"
      (is (not (some #{:section.help}
                     (deep (page/search-form {:lang "en" :folders []
                                              :params {}}
                                             "/" nil))))))))

(deftest navigation-status-test
  (testing "the region is rendered before it has anything to announce"
    (is (= [:div.status {:role "status"} nil]
           (page/navigation-status en false))))
  (testing "and reports a navigation in flight in either language"
    (is (some #{"Loading …"} (deep (page/navigation-status en true))))
    (is (some #{"Henter …"} (deep (page/navigation-status da true))))))

(deftest view-controls-test
  (let [sort* (fn [lang] (page/sort-control lang [["word" :sort-word]] "word"))]
    (testing "no controls, nothing rendered"
      (is (nil? (page/view-controls en false nil nil false))))
    (testing "a control that acts on a result submits the form that made it"
      (let [html (page/view-controls en false (sort* "en") nil false)]
        (is (some #(and (map? %) (= page/form-id (:form %))) (deep html)))))
    (testing "without a client, a button is what applies it, one a browser
              with a script never shows"
      (is (some #{"Apply"} (deep (page/view-controls en false (sort* "en")
                                                  nil false))))
      (is (some #(and (vector? %) (= :noscript (first %)))
                (deep (page/view-controls en false (sort* "en") nil false))))
      (is (some #{"Anvend"} (deep (page/view-controls da false
                                                      (sort* "da")
                                                      nil false)))))
    (testing "with one, choosing an order is asking for it: no button"
      (let [html (page/view-controls en true (sort* "en") nil false)]
        (is (not (some #{"Apply"} (deep html))))
        (is (not (some #(and (vector? %) (= :button (first %))) (deep html))))))
    (testing "and the control itself is what applies it"
      (is (some #(and (map? %) (= [:apply-view] (get-in % [:on :change])))
                (deep (sort* "en")))))
    (testing "what narrows a result sits behind a disclosure, closed until
              a narrowing is in force, open while one is"
      (let [narrowing (page/sample-control en nil)
            closed    (page/view-controls en true (sort* "en") narrowing false)
            open      (page/view-controls en true (sort* "en") narrowing true)
            details   (fn [html] (some #(when (and (vector? %)
                                                   (= :details (first %)))
                                          %)
                                       (deep html)))]
        (is (= :div.viewctl (first closed)))
        (is (false? (:open (second (details closed)))))
        (is (true? (:open (second (details open)))))
        (is (some #{"Narrow the result"} (deep closed)))
        (is (some #{"Afgræns resultatet"}
                  (deep (page/view-controls da true (sort* "da")
                                            narrowing false))))
        (testing "each row gets its own button without a client"
          (is (= 2 (count (filter #{"Apply"}
                                  (deep (page/view-controls en false
                                                            (sort* "en")
                                                            narrowing
                                                            false)))))))
        (testing "and a narrowing alone, with nothing to read differently,
                  is the disclosure alone"
          (let [html (page/view-controls en true nil narrowing true)]
            (is (nil? (second html)))
            (is (details html))))))))

(deftest sample-control-test
  (testing "no sample is the whole result, and it is what is chosen"
    (let [html (page/sample-control en nil)]
      (is (some #{"all hits"} (deep html)))
      (is (some #(and (map? %) (= "" (:value %)) (:selected %)) (deep html)))))
  (testing "the offered sizes are the ones the reader can choose between"
    (is (= page/sample-sizes
           (keep #(when (number? (:value %)) (:value %))
                 (deep (page/sample-control en nil))))))
  (testing "the chosen size is the one marked, and it submits the query
            form as the sort control does"
    (let [html (page/sample-control en 100)]
      (is (some #(and (map? %) (= 100 (:value %)) (:selected %)) (deep html)))
      (is (some #(and (map? %) (= "sample" (:name %))
                      (= page/form-id (:form %))
                      (= [:apply-view] (get-in % [:on :change])))
                (deep html)))))
  (testing "a size the list does not hold is offered beside them, in
            order, so a hand-written URL shows as the sample it is"
    (is (= [50 77 100 500 1000]
           (keep #(when (number? (:value %)) (:value %))
                 (deep (page/sample-control en 77)))))))

(deftest context-control-test
  (let [values (fn [html] (->> (deep html) (filter map?) (keep :value)
                               (map str)))]
    (testing "the widths offered, the chosen one marked, units by name"
      (let [html (page/context-control en :sentence)]
        (is (= ["5" "10" "20" "sentence" "paragraph"] (values html)))
        (is (some #(and (map? %) (= "sentence" (:value %)) (:selected %))
                  (deep html)))
        (is (some #{"5 words" "sentence" "paragraph"} (deep html)))))
    (testing "a number of words the list lacks is offered among the
              numbers, in order"
      (is (= ["5" "7" "10" "20" "sentence" "paragraph"]
             (values (page/context-control en 7)))))
    (testing "it submits the query form as the sort control does"
      (is (some #(and (map? %) (= "context" (:name %))
                      (= page/form-id (:form %))
                      (= [:apply-view] (get-in % [:on :change])))
                (deep (page/context-control en 5)))))
    (testing "in Danish"
      (is (some #{"sætning" "5 ord" "Kontekst"}
                (deep (page/context-control da 5)))))))

(deftest near-control-test
  (testing "no word in force: an empty field and the default distance"
    (let [html (page/near-control en nil)]
      (is (some #(and (map? %) (= "near" (:name %)) (= "" (:value %))
                      (= page/form-id (:form %)))
                (deep html)))
      (is (some #(and (map? %) (= page/near-distance (:value %)) (:selected %))
                (deep html)))))
  (testing "the word and distance in force, the distance applying itself"
    (let [html (page/near-control en {:word "kat" :distance 3})]
      (is (some #(and (map? %) (= "near" (:name %)) (= "kat" (:value %)))
                (deep html)))
      (is (some #(and (map? %) (= 3 (:value %)) (:selected %)) (deep html)))
      (is (some #(and (map? %) (= "distance" (:name %))
                      (= [:apply-view] (get-in % [:on :change])))
                (deep html))))
    (testing "and so does the word, once the reader is done typing it"
      (is (some #(and (map? %) (= "near" (:name %))
                      (= [:apply-view] (get-in % [:on :change])))
                (deep (page/near-control en {:word "kat" :distance 3}))))))
  (testing "a distance the list lacks is offered beside them, in order"
    (is (= [1 2 3 4 5 10]
           (keep #(when (number? (:value %)) (:value %))
                 (deep (page/near-control en {:word "kat" :distance 4}))))))
  (testing "in Danish"
    (is (some #{"Sammen med"} (deep (page/near-control da nil))))
    (is (some #{"1 ord"} (deep (page/near-control da nil))))))

(deftest subset-phrase-test
  (is (= (list [:code "lemma"] " " "before the match" " = " [:code "kat"])
         (page/subset-phrase en {:anchor "match[-1]" :attr :lemma
                                 :value  "kat"})))
  (is (= "lemma før matchet = kat"
         (text (page/subset-phrase da {:anchor "match[-1]" :attr :lemma
                                       :value  "kat"}))))
  (is (nil? (page/subset-phrase en nil)))
  (testing "a position nothing names is shown as CQP names it"
    (is (= "target" (page/position-label en "target"))))
  (is (= "over hele matchet" (page/position-label da "match..matchend"))))

(deftest near-phrase-test
  (is (= (list "near" " " [:code "kat"])
         (page/near-phrase en {:word "kat" :distance 5})))
  (is (= "sammen med kat"
         (text (page/near-phrase da {:word "kat" :distance 5}))))
  (is (nil? (page/near-phrase en nil))))

(deftest sample-phrase-test
  (testing "no sample, nothing said"
    (is (nil? (page/sample-phrase en nil ["PROBE"]))))
  (testing "the size named is the one asked for, a corpus with fewer
            matches than that contributing all it has"
    (is (= "a random sample of at most 100"
           (page/sample-phrase en 100 ["PROBE"]))))
  (testing "over several corpora it says that each was sampled, one
            sample being drawn in every corpus"
    (is (= "a random sample of at most 100 per corpus"
           (page/sample-phrase en 100 ["PROBE" "VISER"])))
    (is (= "en tilfældig stikprøve på højst 100 pr. korpus"
           (page/sample-phrase da 100 ["PROBE" "VISER"])))))

(deftest sort-label-test
  (testing "every sort mode the query namespace offers is named here"
    ;; in Danish, where no label can coincide with the param value
    (is (= ["korpusrækkefølge" "match" "match bagfra" "venstre kontekst"
            "højre kontekst" "tilfældig"]
           (map (comp (partial page/sort-label da) first) commands/sort-modes)))
    (doseq [[value] commands/sort-modes]
      (is (not (str/blank? (page/sort-label en value)))
          (str "sort mode " value " has no label"))))
  (testing "a mode naming an attribute is the match by that attribute"
    (is (= "match lemma" (page/sort-label en "lemma")))
    (is (= "match lemma" (page/sort-label da "lemma")))))

(deftest view-label-test
  (is (= [:abbr {:title "key word in context"} "KWIC"]
         (page/view-label en :kwic)))
  (is (= [:abbr {:title "søgeord i kontekst"} "KWIC"]
         (page/view-label da :kwic)))
  (is (= "Frekvenser" (page/view-label da :frequencies))))

(deftest query-mode-test
  (let [radios (fn [params]
                 (->> (deep (page/search-form
                             {:ui en :folders [] :params params} "/" nil))
                      (filter #(and (map? %) (= "mode" (:name %))))))]
    (testing "Simple comes first, since it is what most searches want"
      (is (= ["simple" "list" "extended" "cqp"] (map :value (radios {})))))
    (testing "and it is the default, so a bare word is not a parse error"
      (is (= [true false false false]
             (map (comp boolean :checked) (radios {}))))
      (is (= [true false false false]
             (map (comp boolean :checked) (radios {:mode "simple"})))))
    (testing "the other modes are opt-in and stay selected once chosen"
      (is (= [false true false false]
             (map (comp boolean :checked) (radios {:mode "list"}))))
      (is (= [false false true false]
             (map (comp boolean :checked) (radios {:mode "extended"}))))
      (is (= [false false false true]
             (map (comp boolean :checked) (radios {:mode "cqp"})))))
    (testing "each radio dispatches the mode it selects, so the client can
              swap the example without a round trip"
      (is (= [[:set-mode "simple"] [:set-mode "list"] [:set-mode "extended"]
              [:set-mode "cqp"]]
             (map (comp :change :on) (radios {})))))))

(deftest query-example-test
  (testing "each mode gets the example for the input it takes"
    (is (= "one word, or several words in order" (page/query-example en nil)))
    (is (= "one word, or several words in order" (page/query-example en "simple")))
    (is (= "\"x\" or [lemma = \"x\"]"
           (page/query-example en "cqp")))
    (testing "and is translated like any other string"
      (is (= "ét eller flere ord i rækkefølge"
             (page/query-example da nil)))))
  (testing "the placeholder follows the mode"
    (let [ph (fn [mode]
               (->> (deep (page/search-form
                           {:ui en :folders [] :params {:mode mode}}
                           "/" nil))
                    (some #(when (and (map? %) (= "q" (:id %)))
                             (:placeholder %)))))]
      (is (= "one word, or several words in order" (ph nil)))
      (is (= "\"x\" or [lemma = \"x\"]" (ph "cqp")))
      (is (= "\"x\" eller [lemma = \"x\"]"
             (->> (deep (page/search-form
                         {:ui da :folders [] :params {:mode "cqp"}}
                         "/" nil))
                  (some #(when (and (map? %) (= "q" (:id %)))
                           (:placeholder %)))))))))

(deftest filter-phrase-test
  (is (= "" (page/filter-phrase {} nil)))
  (is (= "text_author ukendt; text_year 1583, 1591"
         (page/filter-phrase {:text_year   #{"1591" "1583"}
                              :text_author #{"ukendt"}}
                             nil)))
  (is (nil? (page/within-phrase en nil nil)))
  (is (= "within text_year 1591"
         (page/within-phrase en {:text_year #{"1591"}} nil)))
  (is (= "inden for text_year 1591"
         (page/within-phrase da {:text_year #{"1591"}} nil)))
  (testing "patterns follow the values, between slashes"
    (is (= "text_title /Hav.*/; text_year 1583, 1591, /16../"
           (page/filter-phrase {:text_year #{"1591" "1583"}}
                               {:text_title ["Hav.*"] :text_year ["16.."]})))
    (is (= "within text_title /Hav.*/"
           (page/within-phrase en nil {:text_title ["Hav.*"]})))))

(deftest pattern-row-test
  (let [years [{:value "1583"} {:value "1591"}]
        html  (page/pattern-row en :text_year years "15.." ["1583" nil])]
    (testing "a pattern field under the attribute's pattern param, holding
              what is in force"
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
                     (deep (page/pattern-row en :text_title
                                             [{:value "Havfruens sang"}]
                                             nil nil)))))
      (is (not (page/numeric-values? []))))
    (testing "in Danish"
      (is (some #{"mønster" "fra" "til" "et helt tal"}
                (deep (page/pattern-row da :text_year years nil nil)))))))

(deftest filter-fieldset-test
  (testing "no metadata renders nothing"
    (is (nil? (page/filter-fieldset en nil {})))
    (is (nil? (page/filter-fieldset en {:attrs    []
                                          :unlisted []
                                          :selected {}}
                                    {}))))
  (let [selected {:text_year  #{"1591" "1600"}
                  :text_title #{"Havfruens sang"}}
        html (page/filter-fieldset
              "en"
              {:attrs    [{:name :text_year
                           :rows [{:value "1583" :total 1}
                                  {:value "1591" :total 2}]}
                          {:name :text_party
                           :rows [{:value "S" :total 2}]}]
               :unlisted [:text_title]
               :selected selected}
              {:served selected})
        inputs (filter #(and (map? %) (= "checkbox" (:type %))) (deep html))]
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
    (testing "the count is of what the boxes say now"
      (is (some #{"3 selected"} (deep html))))
    (testing "but what is open is of what the page was served"
      ;; ticking a first value must not open the fieldset under the reader,
      ;; nor unticking the last one shut it
      (is (= [false] (->> (deep (page/filter-fieldset
                                 "en" {:attrs [] :unlisted []
                                       :selected {:text_year #{"1591"}}}
                                 {}))
                          (filter #(and (map? %) (contains? % :open)))
                          (map :open))))
      (is (= [true] (->> (deep (page/filter-fieldset
                                "en" {:attrs    [{:name :text_year :rows []}]
                                      :unlisted [] :selected {}}
                                {:served {:text_year #{"1591"}}}))
                         (filter #(and (map? %) (contains? % :open)))
                         (map :open)))))
    (testing "the attributes not on offer are a caveat, so small print"
      (is (some #(and (vector? %) (= :small (first %))) (deep html)))
      (is (some #{[:code "text_title"]} (deep html))))
    (testing "the reader owns the disclosure once they have touched it"
      (let [open* (fn [opts] (->> (deep (page/filter-fieldset
                                         "en" {:attrs [{:name :a :rows []}]
                                               :unlisted [] :selected {}}
                                         opts))
                                  (filter #(and (map? %) (contains? % :open)))
                                  first :open))]
        (is (false? (open* {})))
        (is (true? (open* {:served {:a #{"1"}}})))
        (is (true? (open* {:open? true})))
        ;; and a reader who shut it is not overruled by what was served
        (is (false? (open* {:open? false :served {:a #{"1"}}})))))
    (testing "it is marked busy while its attributes are being fetched"
      (let [busy (fn [opts] (->> (deep (page/filter-fieldset
                                        "en" {:attrs [{:name :a :rows []}]
                                              :unlisted [] :selected {}}
                                        opts))
                                 (filter #(and (map? %) (contains? % :open)))
                                 first :aria-busy))]
        (is (= "true" (busy {:pending? true})))
        (is (nil? (busy {})))))
    (testing "each attribute carries a control over the values it shows"
      (let [alls (->> (deep (page/filter-fieldset
                             "en"
                             {:attrs    [{:name :text_year
                                          :rows [{:value "1583"}
                                                 {:value "1591"}]}]
                              :unlisted [] :selected {}}
                             {:client? true}))
                      (filter #(and (map? %) (contains? % :replicant/on-render))))]
        ;; the fieldset's own control comes first, then one per attribute
        (is (= ["Clear filter" "All values of text_year"]
               (map :aria-label alls)))
        (is (= [[:clear-filter] [:toggle-filter-values [:text_year ["1583" "1591"]]]]
               (map #(get-in % [:on :change]) alls)))))
    (testing "and takes only the values the filter box leaves showing"
      (let [alls (->> (deep (page/filter-fieldset
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
      (let [html (deep (page/filter-fieldset
                        "en" {:attrs    [{:name :text_year
                                          :rows [{:value "1583"}
                                                 {:value "1591"}]}]
                              :unlisted [] :selected {:text_year #{"1583"}}}
                        {:client? true :filter "1591"}))]
        (is (some #(and (map? %) (= "1583" (:value %)) (:checked %)) html))
        (is (some #{{:hidden true}} html))))
    (testing "the region saying nothing was found is there before it says it"
      (let [region (fn [opts]
                     (->> (page/filter-fieldset
                           "en" {:attrs    [{:name :text_year
                                             :rows [{:value "1591"}]}]
                                 :unlisted [] :selected {}}
                           (merge {:client? true} opts))
                          (tree-seq coll? seq)
                          (filter #(and (vector? %) (= :div.empty (first %))))
                          first))]
        (is (= [:div.empty {:role "status"} nil] (region {})))
        (is (= [:div.empty {:role "status"} nil] (region {:filter "1591"})))
        (is (= [:div.empty {:role "status"} "No values found."]
               (region {:filter "zzz"})))))
    (testing "the filter box is there only with a client, and submits nothing"
      (let [box (fn [opts] (->> (deep (page/filter-fieldset
                                       "en" {:attrs [{:name :a :rows []}]
                                             :unlisted [] :selected {}}
                                       opts))
                                (filter #(and (map? %)
                                              (= "value-filter" (:id %))))
                                first))]
        (is (nil? (box {})))
        (is (= "search" (:type (box {:client? true}))))
        ;; and its own label, not the corpus filter's
        (is (some #{[:label {:for "value-filter"} "Filter"]}
                  (deep (page/filter-fieldset
                         "en" {:attrs [{:name :a :rows []}]
                               :unlisted [] :selected {}}
                         {:client? true}))))
        (is (nil? (:name (box {:client? true}))))))
    (testing "the fieldset's own control is the chooser's, minus one half"
      (let [root (fn [selected]
                   (->> (deep (page/filter-fieldset
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
      (let [root (->> (deep (page/filter-fieldset
                             "en" {:attrs    [{:name :a :rows [{:value "1"}]}
                                              {:name :b :rows [{:value "2"}]}]
                                   :unlisted [] :selected {:b #{"2"}}}
                             {:client? true :filter "a"}))
                      (filter #(and (map? %) (= "Clear filter" (:aria-label %))))
                      first)]
        (is (false? (:disabled root)))
        (is (= [:set-checkbox-state {:indeterminate true :invalid nil}]
               (:replicant/on-render root)))))
    (testing "one disclosure over the filter, open only while it is active"
      (is (= [true]
             (keep #(when (and (map? %) (contains? % :open)) (:open %))
                   (deep html)))))
    (testing "an attribute counts its selection without reopening itself"
      (is (some #{" · 2 selected"} (deep html))))
    (testing "the whole filter counts what is chosen across attributes"
      (is (some #{"3 selected"} (deep html))))
    (testing "the region counts are machine-readable, with their unit"
      (is (some #{[:data {:value "2"} "2 regions"]} (deep html)))
      (is (some #{[:data {:value "1"} "1 region"]} (deep html))))
    (testing "values render as the sidebar shows them"
      (is (some #{[:time "1591"]} (deep html))))
    (testing "unlisted attributes are named"
      (is (some #{[:code "text_title"]} (deep html))))))

(deftest page-phrase-test
  (is (= "page 3 of 6" (page/page-phrase en {:page 2 :pages 6})))
  (is (= "side 3 af 6" (page/page-phrase da {:page 2 :pages 6}))))

(deftest pager-links-test
  (testing "no links renders nothing"
    (is (nil? (page/pager-links en nil nil "page 1 of 1"))))
  (testing "links carry the rel values browsers use for a sequence"
    (let [html (page/pager-links en "/?page=0" "/?page=2" "page 2 of 3")]
      (is (some #{"prev"} (deep html)))
      (is (some #{"next"} (deep html)))
      (is (some #{"next →"} (deep html)))))
  (testing "the position rides between the two directions"
    (let [html (page/pager-links en "/?page=0" "/?page=2" "page 2 of 3")]
      (is (= [:li "page 2 of 3"]
             (second (filter #(and (vector? %) (= :li (first %)))
                             (deep html)))))))
  (testing "a direction that is out of range is left out, not held open"
    (let [html (page/pager-links en nil "/?page=1" "page 1 of 3")]
      (is (= 2 (count (filter #(and (vector? %) (= :li (first %)))
                              (deep html)))))
      (is (not (some #{"prev"} (deep html))))))
  (testing "in Danish"
    (let [html (page/pager-links da "/?page=0" "/?page=2" "side 2 af 3")]
      (is (some #{"← forrige"} (deep html)))
      (is (some #{"næste →"} (deep html))))))

(deftest pagination-test
  (testing "nothing to page is no landmark at all"
    (is (nil? (page/pagination en nil nil "page 1 of 1"))))
  (testing "the links are wrapped in a navigation landmark, named"
    (let [html (page/pagination en "/?page=0" "/?page=2" "page 2 of 3")]
      (is (= :nav.pagination (first html)))
      (is (= "Pagination" (:aria-label (second html))))
      (is (= "Sidenavigation"
             (:aria-label (second (page/pagination da "/?page=0" nil "x")))))
      (is (some #{:ul.row.pager} (deep html))))))

(deftest searched?-test
  (testing "a corpus that answered makes the counts an answer"
    (is (page/searched? {:counts [{:corpus "PROBE" :size 5}]}))
    (testing "including an answer of none"
      (is (page/searched? {:counts [{:corpus "PROBE" :size 0}]}))))
  (testing "a search every corpus refused is not an answer"
    (is (not (page/searched?
              {:counts [{:corpus "X" :error {:type :timeout}}]})))
    (is (not (page/searched? nil)))))

(deftest error-name-test
  (is (= "CQP error" (page/error-name en {:type :cqp})))
  (is (= "The search did not finish in time" (page/error-name en {:type :timeout})))
  (is (= "Søgningen tog for lang tid"
         (page/error-name da {:type :timeout}))))

(deftest error-section-test
  (let [html (page/error-section en {:type :cqp :message "boom"} ["TALER"])]
    (testing "no live region: it is in the document before the page is parsed"
      (is (not (some #{"alert"} (deep html)))))
    (testing "it heads itself below the region's own h1"
      (is (some #{[:h2 "CQP error"]} (deep html))))
    (testing "cqp's message is the sample output of another program"
      (is (some #{[:samp "boom"]} (deep html))))
    (is (some #{"boom"} (deep html)))
    (testing "an error CQP itself reported is headed as such"
      (is (some #{"CQP error"} (deep html))))
    (testing "the corpora concerned are named"
      (is (some #{[:code "TALER"]} (deep html)))))
  (testing "no corpus selected is explained without a CQP message"
    (let [html (page/error-section en {:type :no-corpus} nil)]
      (is (some #{"No corpus selected"} (deep html)))
      (is (some #{"Select at least one corpus to search."} (deep html)))
      (is (not (some #{:pre} (deep html))))))
  (testing "our own rejections and internal failures are not CQP errors"
    (is (some #{"Request rejected"}
              (deep (page/error-section en {:type    :rejected
                                              :message "x"} nil))))
    (is (some #{"Unexpected error"}
              (deep (page/error-section en {:type :internal} nil))))
    (is (some #{"Unknown corpus"}
              (deep (page/error-section en {:type :unknown-corpus} ["X"])))))
  (testing "the headings and explanations are translated, CQP's message not"
    (let [html (page/error-section da {:type :no-corpus} nil)]
      (is (some #{"Intet korpus valgt"} (deep html)))
      (is (some #{"Vælg mindst ét korpus at søge i."} (deep html))))
    (let [html (page/error-section da {:type :cqp :message "boom"} ["X"])]
      (is (some #{"CQP-fejl"} (deep html)))
      (is (some #{"boom"} (deep html))))))

(def example-result
  {:size 6 :page 0 :page-size 25 :pages 1
   :counts [{:corpus "PROBE" :size 5}
            {:corpus "VISER" :size 1}
            {:corpus "TALER" :error {:type :cqp :message "no lemma"}}
            {:corpus "GONE" :error {:type :cqp :message "no lemma"}}]
   :hits []})

(deftest error-groups-test
  (testing "identical errors are reported once, naming every corpus"
    (is (= [[{:type :cqp :message "no lemma"} ["TALER" "GONE"]]]
           (page/error-groups (:counts example-result)))))
  (is (empty? (page/error-groups [{:corpus "PROBE" :size 1}]))))

(deftest hits-phrase-test
  (is (= "1 hit" (page/hits-phrase en 1)))
  (is (= "0 hits" (page/hits-phrase en 0)))
  (testing "Danish, with its own digit grouping"
    (is (= "1 forekomst" (page/hits-phrase da 1)))
    (is (= "1.000 forekomster" (page/hits-phrase da 1000)))))

(deftest hits-heading-test
  (testing "the heading is the answer, with the query as its subject"
    (is (= (list "6 hits" " " "for" " " [:q "hund"])
           (page/hits-heading en {:q "hund"} 6)))
    (is (= "6 forekomster af hund"
           (text (page/hits-heading da {:q "hund"} 6)))))
  (testing "a CQP query is code, a list is its length, a word is quoted"
    (is (= [:code "[lemma = \"hund\"]"]
           (page/query-mark en {:q "[lemma = \"hund\"]" :mode "cqp"})))
    (is (= "2 words" (page/query-mark en {:q "hund\nkat" :mode "list"})))
    (is (= [:q "hund"] (page/query-mark en {:q "hund" :mode "simple"}))))
  (testing "a blank query counts every token, which only a table asks for"
    (is (= "All tokens" (page/hits-heading en {:q ""} 47)))))

(deftest qualifiers-test
  (let [phrases (fn [ui params result]
                  (map text (page/qualifiers ui params result)))]
    (testing "only the corpora that could be searched are counted"
      (is (= ["in 2 corpora"] (phrases en {} example-result))))
    (testing "the attribute and the part of the form, when not the usual"
      (is (= ["attribute lemma" "part of word" "in 2 corpora"]
             (phrases en {:in "lemma" :match "infix"} example-result)))
      (is (= ["in 2 corpora"]
             (phrases en {:in "word" :match ""} example-result)))
      (testing "and only where the mode read them: a CQP query names its
                own attribute, and the options ride along as memory"
        (is (= ["in 2 corpora"]
               (phrases en {:q "[]" :mode "cqp" :in "lemma" :match "infix"}
                        example-result)))
        (is (= ["in 2 corpora"]
               (phrases en {:mode "extended" :t1.v "x" :in "lemma"}
                        example-result)))))
    (testing "the filter, the narrowings and the sample, in that order"
      (is (= ["in 2 corpora" "within text_year 1591"
              "lemma at the start of the match = hund" "near og"
              "a random sample of at most 100 per corpus"]
             (phrases en {} (assoc example-result
                                   :filter {:text_year #{"1591"}}
                                   :subset {:anchor "match" :attr :lemma
                                            :value  "hund"}
                                   :near   {:word "og"}
                                   :sample 100)))))
    (testing "a search that found nothing sampled nothing, and saying it
              drew a sample reads as the reason the result is empty"
      (is (= ["in PROBE"]
             (phrases en {} {:size   0 :sample 100
                             :counts [{:corpus "PROBE" :size 0}]}))))
    (testing "in Danish, the attribute name untranslated"
      (is (= ["i 2 korpusser" "inden for text_year 1591"]
             (phrases da {} (assoc example-result
                                   :filter {:text_year #{"1591"}})))))))

(deftest download-links-test
  (is (nil? (page/download-links en nil nil)))
  (let [html (page/download-links en
                                  {:tsv "/x?format=tsv" :csv "/x?format=csv"}
                                  "the first 10 hits")]
    (testing "one download link per format, in a fixed order"
      (is (= [[:a {:href "/x?format=csv"} "CSV"]
              [:a {:href "/x?format=tsv"} "TSV"]]
             (filter #(and (vector? %) (= :a (first %))) (deep html)))))
    (testing "the note qualifies the download"
      (is (some #{" the first 10 hits"} (deep html))))))

(deftest result-section-test
  (let [state {:ui           en
               :view         :kwic
               :view-hrefs   [[:kwic "/?v=k"]
                              [:frequencies "/?v=f"]]
               :sort-modes   ["corpus" "word"]
               :params       {:q "hund"}
               :result       example-result
               :next-href    "/?page=1"
               :export-hrefs {:tsv "/e?format=tsv"}
               :export-limit 5}
        html  (page/result-section state)]
    (testing "what to do next with the hits follows them, not precedes them"
      (let [order (fn [x] (.indexOf (vec (deep html)) x))]
        (is (< (order :table.kwic) (order :p.downloads)))))
    (testing "the other view of the same hits is offered above them"
      (let [order (fn [x] (.indexOf (vec (deep html)) x))]
        (is (some #{:nav.views} (deep html)))
        (is (< (order :nav.views) (order :table.kwic)))))
    (testing "a cut export is announced"
      (is (some #{" the first 5 hits"} (deep html))))
    (testing "the region names itself and can be landed on"
      (is (= {:id              "results"
              :tabindex        "-1"
              :aria-labelledby "results-heading"}
             (second html))))
    (testing "and is busy while the answer to the next question is coming"
      (is (= "true" (:aria-busy (second (page/result-section
                                         (assoc state :pending? true)))))))
    (testing "its heading is the answer, the page's own h1, and the question
              stands under it as a subheading"
      (let [[tag h1 sub] (nth html 2)]
        (is (= :hgroup tag))
        (is (= [:h1 {:id "results-heading"}] (subvec h1 0 2)))
        (is (= "6 hits for hund" (text (drop 2 h1))))
        (is (= :p (first sub)))
        (is (= "in 2 corpora" (text sub)))))
    (testing "errors are headed sections before the counts and concordance"
      (is (some #{[:h2 "CQP error"]} (deep html)))
      (is (some #{[:caption "Hits per corpus"]} (deep html)))
      (is (some #{:table.kwic} (deep html))))
    (testing "the sort travels with the result, not with the query form"
      (is (some #{"corpus order"} (deep html)))
      (is (some #(and (map? %) (= "sort" (:id %)) (= page/form-id (:form %)))
                (deep html))))
    (testing "and so does the sample, which is a question the reader has
              on seeing how many hits there are"
      (is (some #(and (map? %) (= "sample" (:id %))
                      (= page/form-id (:form %)))
                (deep html))))
    (testing "the page links are rendered above the table as well as below"
      (is (= 2 (count (filter #(and (vector? %) (= :ul.row.pager (first %)))
                              (deep html))))))
    (testing "but only the first is a landmark, since both would share a name"
      (is (= 1 (count (filter #(and (vector? %) (= :nav.pagination (first %)))
                              (deep html))))))
    (testing "the errors come before the counts and the concordance"
      (let [order (fn [x] (.indexOf (vec (deep html)) x))]
        (is (< (order [:h2 "CQP error"]) (order :table.counts)))
        (is (< (order [:h2 "CQP error"]) (order :table.kwic)))))
    (testing "but the counts break the hits down, so they follow them"
      (let [order (fn [x] (.indexOf (vec (deep html)) x))]
        (is (< (order :table.kwic) (order :table.counts)))
        ;; and after the *last* page links, which belong against the foot
        ;; of the table they turn, not merely after the first
        (is (< (.lastIndexOf (vec (deep html)) :ul.row.pager)
               (order :table.counts)))
        (is (< (order :table.counts) (order :p.downloads)))))
    (testing "and they are an aside: about the hits rather than part of them"
      (is (some #(and (vector? %) (= :aside (first %))
                      (= :table.counts (first (second %))))
                (deep html))))
    (testing "an erroring corpus shows no count in the table"
      (is (some #{[:em "error"]} (deep html))))
    (testing "a single corpus gets no counts table"
      (let [html (page/result-section
                  {:lang   "en"
                   :result (assoc example-result
                                  :counts [{:corpus "PROBE" :size 5}])})]
        (is (not (some #{[:caption "Hits per corpus"]} (deep html)))))))
  (testing "nothing searchable means only the errors, and the heading names one"
    (let [html (page/result-section
                {:lang   "en"
                 :result {:page   0 :pages 1 :hits []
                          :counts [{:corpus "X" :error {:type :timeout}}]}})]
      (is (not (some #{:table.kwic} (deep html))))
      (is (some #{[:h1 {:id "results-heading"}
                   "The search did not finish in time"]}
                (deep html)))))
  (testing "a search that failed outright is a results region too"
    (let [html (page/result-section {:lang  "en"
                                     :error {:type :no-corpus}})]
      (is (some #{[:h1 {:id "results-heading"} "No corpus selected"]}
                (deep html)))
      (is (some #{"Select at least one corpus to search."} (deep html)))))
  (testing "a search that found nothing offers no table and no downloads"
    (let [html (page/result-section
                {:lang         "en"
                 :result       {:size   0 :page 0 :pages 1 :hits []
                                :counts [{:corpus "PROBE" :size 0}]}
                 :export-hrefs {:tsv "/e?format=tsv"}})]
      (is (some #{"No hits."} (deep html)))
      (is (not (some #{:table.kwic} (deep html))))
      (is (not (some #{"/e?format=tsv"} (deep html))))
      (is (not (some #{"/frequencies?q=x"} (deep html)))))))

(deftest counting-test
  (testing "a result is being counted while corpora remain"
    (is (page/counting? {:remaining ["X"]}))
    (is (not (page/counting? {:remaining []})))
    (is (not (page/counting? {}))))
  (testing "the heading gives the hits counted so far as a floor"
    (is (= "at least 6 hits for hund"
           (text (page/hits-heading en {:q "hund"} 6 true))))
    (is (= "mindst 6 forekomster af hund"
           (text (page/hits-heading da {:q "hund"} 6 true)))))
  (testing "the page is placed without a last page"
    (is (= "page 3" (page/page-phrase en {:page 2})))
    (is (= "side 3" (page/page-phrase da {:page 2}))))
  (testing "corpora still being counted count as searched"
    (is (page/searched? {:counts [] :remaining ["X"]}))
    (is (= ["in 3 corpora"]
           (map text (page/qualifiers en {} {:counts    [{:corpus "A" :size 1}]
                                             :remaining ["B" "C"]})))))
  (let [state {:ui        en
               :view      :kwic
               :params    {:q "hund"}
               :next-href "/?page=2"}
        html  (page/result-section
               (assoc state :result (assoc example-result
                                           :pages     nil
                                           :remaining ["X" "Y"])))]
    (testing "the heading says at least, and a status line says what is
              still being counted"
      (is (= "at least 6 hits for hund"
             (text (drop 2 (second (nth html 2))))))
      (is (some #{[:p "Counting hits in 2 corpora …"]} (deep html)))
      (is (some #{[:li "page 1"]} (deep html))))
    (testing "the status line is a live region that stands even when silent"
      (is (some #{[:div.status {:role "status"} nil]}
                (deep (page/result-section
                       (assoc state :result example-result))))))))

(deftest attribute-value-test
  (testing "a title is a cited work"
    (is (= [:cite "Hverdag"] (page/attribute-value :text_title "Hverdag"))))
  (testing "a four-digit year is a time"
    (is (= [:time "2023"] (page/attribute-value :text_year "2023"))))
  (testing "a non-year value under a _year key stays plain"
    (is (= "n/a" (page/attribute-value :text_year "n/a"))))
  (testing "anything else is plain text"
    (is (= "NCSI" (page/attribute-value :pos "NCSI")))))

(deftest sidebar-test
  (testing "nothing selected, no panel: it describes the cursor or nothing"
    (is (nil? (page/sidebar en nil))))
  (let [html (page/sidebar en {:token   {:word "hund" :pos "NCSI"}
                                 :structs {:text_title "Hverdag"}
                                 :corpus  "PROBE"})]
    (testing "it is a named complementary region, not a popover"
      (is (= :aside.sidebar (first html)))
      (is (= "Token details" (:aria-label (second html))))
      (is (not (contains? (second html) :popover))))
    (testing "it never takes focus, so the cursor can keep moving"
      (is (not (some #(and (map? %) (contains? % :autofocus)) (deep html)))))
    (testing "the token, its text and its corpus are named groups"
      (is (some #{[:h3 "Token"]} (deep html)))
      (is (some #{[:h3 "Text"]} (deep html)))
      (is (some #{[:h3 "Corpus"]} (deep html)))
      (is (some #{"NCSI"} (deep html)))
      (is (some #{"Hverdag"} (deep html))))
    (testing "closing is a plain button, since Escape is handled by the grid"
      (is (some #{[:button {:type "button" :on {:click [:close]}} "Close"]}
                (deep html))))))

(deftest match-control-test
  (let [options (fn [match]
                  (->> (deep (page/match-control en match false))
                       (filter #(and (map? %) (contains? % :selected)))
                       (map (juxt :value :selected))))]
    (testing "one select over how much of the form the query must cover,
              the whole form unless the URL says otherwise"
      (is (= [["" true] ["prefix" false] ["suffix" false] ["infix" false]]
             (options nil)))
      (is (= [["" false] ["prefix" false] ["suffix" false] ["infix" true]]
             (options "infix"))))
    (testing "named, and disabled while the query writes its own"
      (is (= [:label "match" " "] (subvec (page/match-control en nil false) 0 3)))
      (is (true? (:disabled (second (last (page/match-control en nil true)))))))
    (testing "in either language"
      (is (some #{"whole word" "part of word"} (deep (page/match-control en nil false))))
      (is (some #{"hele ordet" "en del af ordet"}
                (deep (page/match-control da "infix" false)))))))

(deftest query-field-test
  (testing "a simple or CQP query is a search box holding the query"
    (is (= [:input {:id           "q"
                    :name         "q"
                    :aria-label   "Query"
                    :placeholder  "one word, or several words in order"
                    :autocomplete "off"
                    :spellcheck   "false"
                    :required     true
                    :type         "search"
                    :value        "hund"}]
           (page/query-field en nil "hund" true))))
  (testing "a list is a text area, its words its content, and required too"
    (let [[tag attrs text] (page/query-field en "list" "hund\nkat" true)]
      (is (= :textarea tag))
      (is (= "q" (:name attrs)))
      (is (= "one word per line" (:placeholder attrs)))
      (is (true? (:required attrs)))
      (is (= "hund\nkat" text)))
    (is (= "ét ord pr. linje"
           (:placeholder (second (page/query-field da "list" nil true))))))
  (testing "a list submitted under another mode stands on one line in the
            box, which would otherwise drop its line breaks"
    (is (= "hund kat" (page/one-line "hund\r\nkat")))
    (is (= "hund kat" (page/one-line " hund \n\n kat ")))
    (is (= "" (page/one-line nil)))
    (is (= "hund kat"
           (:value (second (page/query-field en nil "hund\r\nkat" true)))))
    (is (= "hund\r\nkat"
           (last (page/query-field en "list" "hund\r\nkat" true))))))

(deftest from-input-test
  (testing "the form names the mode it was rendered in, so a submit whose
            radio was changed can read the query as it was typed"
    (let [from (fn [params]
                 (some #(and (map? %) (= "from" (:name %)) (:value %))
                       (deep (page/search-form {:ui en :folders []
                                                :params params}
                                               "/" nil))))]
      (is (= "simple" (from {})))
      (is (= "simple" (from {:mode "nonesuch"})))
      (is (= "list" (from {:mode "list" :q "a\nb"})))
      (is (= "extended" (from {:mode "extended"})))
      (is (= "cqp" (from {:mode "cqp"}))))))

(deftest query-phrase-test
  (is (= "hund" (page/query-phrase en {:q "hund"})))
  (is (= "2 words" (page/query-phrase en {:q "hund\n\nkat\n" :mode "list"})))
  (testing "a list counts its words however they are laid out"
    (is (= "3 words"
           (page/query-phrase en {:q "lille hund\nkat" :mode "list"}))))
  (is (= "1 ord" (page/query-phrase da {:q "hund" :mode "list"}))))

(deftest sidebar-text-link-test
  (let [selected {:token {:word "hund"} :corpus "PROBE" :cpos 9 :matchend 9}]
    (testing "the panel links to the whole text of the hit it describes"
      (is (some #(and (map? %) (= "/corpora/probe/text?cpos=9#hit" (:href %)))
                (deep (page/sidebar en selected))))
      (is (some #{"Læs hele teksten"} (deep (page/sidebar da selected)))))))
