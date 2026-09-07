(ns dk.cst.corpus-probe.views.search-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.hiccup :refer [deep]]
            [dk.cst.corpus-probe.test.hiccup :refer [da en text]]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.query.mode :as mode]
            [dk.cst.corpus-probe.views.corpus :as corpus-views]
            [dk.cst.corpus-probe.views.search :as search]))

(defn form
  "The search form of `state`, with the corpus chooser over its
  `:folders` handed in as the page hands it."
  [{:keys [ui lang folders params] :as state}]
  (search/search-form state "/" nil
                      (corpus-views/corpus-chooser
                       (or ui (i18n/->ui lang)) folders
                       {:selected (set (:corpus params))})))

(deftest search-form-test
  (let [state {:lang         "en"
               :folders      [{:label nil :folders []
                               :corpora [{:id "PROBE" :size 47}]}]
               :search-attrs [:word :pos :lemma]
               :params       {:corpus ["PROBE"] :q "hund" :sort "word"}}
        html  (form state)]
    (testing "the form is wrapped in a <search> landmark"
      (is (= :search (first html)))
      (is (= :form.search-form (first (second html)))))
    (testing "the form submits to the given action"
      (is (= "/" (get-in html [1 1 :action]))))
    (testing "the selected corpus is checked in the chooser"
      (is (some #(and (map? %) (= "corpus" (:name %)) (:checked %))
                (deep html))))
    (testing "the query field is a text area, one row for one line"
      (is (some #(and (map? %) (= "q" (:id %)) (= 1 (:rows %))) (deep html)))
      (is (some #{:textarea} (deep html))))
    (testing "the query is required, except from the frequency view, which
              counts every token of a blank one"
      (let [required (fn [state]
                       (->> (deep (form state))
                            (filter #(and (map? %) (= "q" (:id %))))
                            (first)
                            (:required)))]
        (is (true? (required state)))
        (is (false? (required (assoc state :view :frequencies))))))
    (testing "grouped controls have legends"
      (is (some #{:legend} (deep html))))
    (testing "the example says what the field takes"
      (is (some #(and (map? %) (= "q" (:id %))
                      (= "words, a list or CQP"
                         (:placeholder %)))
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
                (deep (search/search-form
                       (assoc state :ui en) "/"
                       [:input {:type  "hidden" :name "attr"
                                :value "word"}]
                       nil)))))
    (testing "the query options are one group, under the query row, the
              chooser where the page put it"
      (let [order (fn [x] (.indexOf (vec (deep html)) x))
            at    (fn [name]
                    (order (some #(when (and (map? %) (= name (:name %))) %)
                                 (deep html))))]
        (is (< (order :div.query) (order :textarea)))
        (is (< (order :textarea) (order :fieldset.modes.box)))
        (is (< (order :fieldset.modes.box) (order :fieldset.matching.box)))
        (is (< (order :fieldset.matching.box) (order :fieldset.chooser.box)))
        (testing "with the status line after the mode and above the rows"
          (is (< (order :fieldset.modes.box)
                 (order :div.status)))
          (is (< (order :div.status) (at "in"))))
        ;; the two boxes said the same thing twice; the group they named
        ;; is still named, without a box
        (is (not (some #{:fieldset.mode :fieldset.options} (deep html))))
        (is (some #{[:legend "Query type"] [:legend "Scope"]} (deep html)))
        (testing "and the box reads as a sentence: how much of which
                  attribute, within what, and the case box under them,
                  each row classed for the grid"
          (is (< (at "match") (at "in") (at "within") (at "ci")))
          (is (< (order :p.matching-find) (order :p.matching-within)
                 (order :p.matching-case))))))
    (testing "and the options are offered only for a query they can
              qualify, as the table of what each mode reads says, the mode
              being the shape of the text"
      (let [offered (fn [params]
                      (->> (deep (form (assoc state :params params)))
                           (keep #(when (map? %)
                                    (#{"in" "ci" "match" "within"}
                                     (:name %))))))
            texts   {"simple" {:q "hund"} "list" {:q "hund\nkat"}
                     "extended" {:mode "extended"} "cqp" {:q "[]"}}]
        (is (= ["match" "in" "within" "ci"] (offered (texts "simple"))))
        (is (= ["match" "in" "ci"] (offered (texts "list"))))
        (is (= ["within"] (offered (texts "extended"))))
        (is (empty? (offered (texts "cqp"))))
        (doseq [mode mode/modes]
          (is (= (filter #(mode/reads? mode (keyword %))
                         ["match" "in" "within" "ci"])
                 (offered (texts mode)))
              mode))))
    (testing "a simple search matches one of the corpora's attributes,
              the surface form unless the URL says otherwise"
      (let [options (fn [state]
                      (->> (deep (form state))
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
    (testing "no matching box at all for a query that writes its own, so
              no simple option rides along with a CQP one"
      ;; what the reader ticked is held in the params and comes back with
      ;; the mode that reads it, so taking the control away loses nothing
      (let [html (deep (form (-> state
                                 (assoc-in [:params :q] "[]")
                                 (assoc-in [:params :ci] "on"))))]
        (is (not (some #{:fieldset.matching.box} html)))
        (is (not (some #(and (map? %) (= "ci" (:name %))) html)))))
    (testing "one status region in the form, for what a change of mode
              could not keep, empty until then; the navigation's only with
              a client to put anything in it"
      (is (= [[:div.status {:role "status"} nil]]
             (filter #(and (vector? %) (= :div.status (first %)))
                     (deep html)))))
    (testing "with one, the navigation's follows the form, inside the same
              landmark, placed by its own class"
      (let [live (form (assoc state :client? true :pending? true))]
        (is (= :div.status (first (last live))))
        (is (= "navigation-status" (:class (second (last live)))))
        (is (some #{"Loading …"} (deep live)))))
    (testing "the query row says when it holds the tokens, for the layout"
      (is (= [:div.query {}]
             (subvec (get-in html [1 3]) 0 2)))
      (is (= [:div.query {:class "query-extended"}]
             (subvec (get-in (form (assoc state :params {:mode "extended"}))
                             [1 3])
                     0 2))))
    (testing "the same form in Danish"
      (let [da (deep (form (assoc state :ui da)))]
        (is (some #{"Søg"} da))
        (is (some #{"Søgeudtryk"} da))
        (is (not (some #(and (map? %) (= "lang" (:name %))) da)))))))

(deftest change-mode-button-test
  (let [button (fn [state]
                 (some #(when (and (vector? %) (= :button (first %))
                                   (= "Change mode" (last %)))
                          (second %))
                       (deep (form state))))]
    (testing "without the client, a button submits the form without its
              checks, so a reader can leave an empty form for another mode"
      (let [attrs (button {:ui en :folders [] :params {}})]
        (is (= "submit" (:type attrs)))
        (is (true? (:formnovalidate attrs))))
      (is (some #{:noscript} (deep (form {:ui en :folders [] :params {}})))))
    (testing "with it, none: the radios change the form themselves"
      (is (nil? (button {:ui en :folders [] :params {} :client? true}))))))

(deftest cqp-line-test
  (testing "what the tokens run as, under them, as the form holds them
            rather than as the params say"
    (is (= [:p.cqp "As CQP" ": " [:code "[word = \"x\"] []"]]
           (search/cqp-line en {:mode "extended" :t1.v "y"}
                            [{:id 1 :conditions [{:id 1 :v "x"}]}
                             {:id 2 :conditions [{:id 1 :op "any"}]}])))
    (is (= "Som CQP: [word = \"x\"]"
           (text (search/cqp-line da {:mode "extended"}
                                  [{:id 1 :conditions [{:id 1 :v "x"}]}])))))
  (testing "nothing for no query"
    (is (nil? (search/cqp-line en {:mode "extended"}
                               [{:id 1 :conditions [{:id 1}]}]))))
  (testing "and under the field, how its text is read (see
            `reading-line-test`), where that wants saying"
    (let [outputs (fn [state]
                    (filter #(and (vector? %) (= :p.cqp (first %)))
                            (deep (form (assoc state :ui en :folders [])))))]
      (is (empty? (outputs {:params {:q "hund"}})))
      (is (= 1 (count (outputs {:params {:q "a\nb"}}))))
      (is (= 1 (count (outputs {:params {:mode "extended"}
                                :tokens [{:id 1 :conditions
                                          [{:id 1 :v "hund"}]}]})))))))

(deftest reading-line-test
  (testing "words in order and a list say what they are, with the CQP they
            run as, and CQP that it is CQP"
    (is (= "2 words in order · As CQP: [word = \"lille\"] [word = \"hund\"]"
           (text (search/reading-line en {:q "lille hund"}))))
    (is (= "Any one of 3 words · As CQP: [lemma = \"(a|b|c)\"]"
           (text (search/reading-line en {:q "a\nb\nc" :in "lemma"}))))
    (is (= [:p.cqp {:id "reading"} "Read as CQP"]
           (search/reading-line en {:q "[] []"})))
    (is (= "2 ord i rækkefølge · Som CQP: [word = \"a\"] [word = \"b\"]"
           (text (search/reading-line da {:q "a b"})))))
  (testing "nothing for a blank field or one word, which read as they look"
    (is (nil? (search/reading-line en {})))
    (is (nil? (search/reading-line en {:q "  "})))
    (is (nil? (search/reading-line en {:q "hund"})))
    (is (nil? (search/reading-line en {:q "hund\n"}))))
  (testing "the field is described by the line, where there is one"
    (let [described (fn [params]
                      (->> (deep (form {:ui en :folders [] :params params}))
                           (some #(when (and (map? %) (= "q" (:id %)))
                                    (:aria-describedby %)))))]
      (is (= "reading" (described {:q "lille hund"})))
      (is (= "reading" (described {:q "[]"})))
      (is (nil? (described {:q "hund"}))))))

(deftest switch-notice-test
  (testing "nothing to say is nothing, the line's empty state"
    (is (nil? (search/switch-notice en "simple" [] #{}))))
  (testing "CQP the extended form cannot read is quoted as code"
    (let [notice (search/switch-notice en "extended" [[:cqp "[] []"]] #{})]
      (is (= "Extended cannot read CQP. The query is not kept: [] []"
             (text notice)))
      (is (some #{[:code "[] []"]} (deep notice))))
    (is (= "Udvidet kan ikke læse CQP. Søgeudtrykket bevares ikke: [] []"
           (text (search/switch-notice da "extended" [[:cqp "[] []"]] #{})))))
  (testing "a list past the cap, its length written as the language does"
    (is (= "A list of 1,200 words is not kept in Extended."
           (text (search/switch-notice en "extended" [[:list 1200]] #{}))))
    (is (= "En liste på 1.200 ord bevares ikke i Udvidet."
           (text (search/switch-notice da "extended" [[:list 1200]] #{})))))
  (testing "the params of a hand-written URL the mode did not read, each
            named once, in the URL's order"
    (is (= "Not used in CQP: the tokens, attribute, ignore case."
           (text (search/switch-notice en "cqp" []
                                       #{:in :ci :t1.v :t2.op})))))
  (testing "in the form's status line, which is empty otherwise"
    (let [status (fn [state]
                   (some #(when (and (vector? %) (= :div.status (first %)))
                            %)
                         (deep (form state))))]
      (is (= [:div.status {:role "status"} nil]
             (status {:ui en :folders [] :params {}})))
      (is (= "A list of 60 words is not kept in Extended."
             (text (drop 2 (status {:ui     en
                                    :folders []
                                    :params  {:mode "extended"}
                                    :switch  {:loss   [[:list 60]]
                                              :unread #{}}}))))))))

(deftest help-test
  (let [blocks [[:dl [:dt "Simple"] [:dd "Finds the words in order."]]]
        html   (search/help en blocks)]
    (testing "a region named by the interface, holding the document"
      (is (= [:section.help {:aria-label "Help"} blocks] html)))
    (testing "named in the reader's language"
      (is (= "Hjælp" (:aria-label (second (search/help da blocks))))))
    (testing "no help, no section"
      (is (nil? (search/help en nil)))
      (is (nil? (search/help en []))))
    (testing "it is not in the form: it stands where the results will"
      (is (not (some #{:section.help}
                     (deep (form {:lang "en" :folders [] :params {}}))))))))

(deftest navigation-status-test
  (testing "the region is rendered before it has anything to announce,
            placed by its own class"
    (is (= [:div.status {:class "navigation-status" :role "status"} nil]
           (search/navigation-status en false))))
  (testing "and reports a navigation in flight in either language"
    (is (some #{"Loading …"} (deep (search/navigation-status en true))))
    (is (some #{"Henter …"} (deep (search/navigation-status da true))))))

(deftest query-mode-test
  (let [radios (fn [params]
                 (->> (deep (form {:ui en :folders [] :params params}))
                      (filter #(and (map? %) (= "mode" (:name %))))))]
    (testing "the field comes first, since it is what most searches want"
      (is (= ["simple" "extended"] (map :value (radios {})))))
    (testing "and it is the default, whatever the shape of its text"
      (is (= [true false] (map (comp boolean :checked) (radios {}))))
      (is (= [true false]
             (map (comp boolean :checked) (radios {:mode "simple"}))))
      (is (= [true false]
             (map (comp boolean :checked) (radios {:q "[] []"})))))
    (testing "the extended form is opt-in and stays selected once chosen"
      (is (= [false true]
             (map (comp boolean :checked) (radios {:mode "extended"}))))
      (is (= [false true]
             (map (comp boolean :checked) (radios {:t1.v "x"})))))
    (testing "each radio dispatches the form it selects, so the client can
              swap the field for the tokens without a round trip"
      (is (= [[:set-mode "simple"] [:set-mode "extended"]]
             (map (comp :change :on) (radios {})))))))

(deftest match-control-test
  (let [options (fn [match]
                  (->> (deep (search/match-control en match))
                       (filter #(and (map? %) (contains? % :selected)))
                       (map (juxt :value :selected))))]
    (testing "one select over how much of the form the query must cover,
              the whole form unless the URL says otherwise"
      (is (= [["" true] ["prefix" false] ["suffix" false] ["infix" false]]
             (options nil)))
      (is (= [["" false] ["prefix" false] ["suffix" false] ["infix" true]]
             (options "infix"))))
    (testing "named for a screen reader, standing in a sentence"
      (is (= "match" (:aria-label (second (search/match-control en nil))))))
    (testing "in either language, as read before the attribute"
      (is (some #{"whole" "part of"} (deep (search/match-control en nil))))
      (is (some #{"hele" "del af"} (deep (search/match-control da "infix")))))))

(deftest within-control-test
  (let [chosen (fn [within]
                 (map :value (filter #(and (map? %) (:selected %))
                                     (deep (search/within-control en within)))))]
    (testing "the sentence unless the URL says otherwise, every unit named"
      (is (= ["sentence"] (chosen nil)))
      (is (= ["paragraph"] (chosen "paragraph")))
      (is (some #{"sentence" "paragraph" "text"}
                (deep (search/within-control en nil))))
      (is (some #{"sætning"} (deep (search/within-control da nil)))))))

(deftest query-field-test
  (testing "the field is a text area holding the query as its content, one
            row for one line, Enter labelled for a search, required"
    (is (= [:textarea {:id           "q"
                       :name         "q"
                       :rows         1
                       :aria-label   "Query"
                       :placeholder  "words, a list or CQP"
                       :autocomplete "off"
                       :spellcheck   "false"
                       :enterkeyhint "search"
                       :required     true
                       :on           {:input   [:set-query]
                                      :keydown [:submit-on-enter]}
                       :replicant/on-render [:set-validity nil]}
            "hund"]
           (search/query-field en "hund" true nil)))
    (testing "the text is rendered as typed, so the render never moves it"
      (is (= "hund " (last (search/query-field en "hund " true nil))))
      (is (= "hund\r\nkat" (last (search/query-field en "hund\r\nkat" true nil))))))
  (testing "a row, and one more for every line break, the empty line a
            Shift+Enter has just opened included, up to eight"
    (let [rows (fn [text] (:rows (second (search/query-field en text true nil))))]
      (is (= 1 (rows nil)))
      (is (= 1 (rows "")))
      (is (= 2 (rows "hund\n")))
      (is (= 2 (rows "hund\nkat")))
      (is (= 3 (rows "hund\r\nkat\n")))
      (is (= 8 (rows (str/join "\n" (range 20)))))))
  (testing "not required from the frequency view, which counts every token
            of a blank one"
    (is (false? (:required (second (search/query-field en nil false nil))))))
  (testing "a blank of any length is refused by the field itself, in the
            interface's words, where a query is required"
    (let [validity (fn [text required?]
                     (:replicant/on-render
                      (second (search/query-field en text required? nil))))]
      (is (= [:set-validity "Type a query"] (validity "" true)))
      (is (= [:set-validity "Type a query"] (validity " \n\r\n " true)))
      (is (= [:set-validity nil] (validity "hund" true)))
      (is (= [:set-validity nil] (validity "" false)))
      (is (= [:set-validity "Skriv en forespørgsel"]
             (:replicant/on-render (second (search/query-field da "" true nil)))))))
  (testing "described by the line under it, when told its id"
    (is (= "reading"
           (:aria-describedby (second (search/query-field en "a b" true
                                                          "reading")))))
    (is (nil? (:aria-describedby (second (search/query-field en "a b" true nil))))))
  (is (= "ord, en liste eller CQP"
         (:placeholder (second (search/query-field da nil true nil))))))
