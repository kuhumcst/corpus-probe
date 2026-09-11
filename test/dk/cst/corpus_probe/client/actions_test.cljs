(ns dk.cst.corpus-probe.client.actions-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.client.actions :as actions]
            [dk.cst.corpus-probe.hiccup :refer [deep]]
            [dk.cst.corpus-probe.client.lists-test :refer [filters folders]]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.query.tokens :as tokens]
            [dk.cst.corpus-probe.storage.settings :as settings]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views :as views]
            [dk.cst.corpus-probe.views.concordance :as concordance]))

(def hit
  {:corpus  "PROBE" :cpos 9
   :left    [{:word "en"} {:word "lille"}]
   :match   [{:word "hund"}]
   :right   [{:word "gør"}]
   :anchors {:match 9 :matchend 9}})

(def wider
  {:corpus "PROBE" :cpos 9
   :left   [{:word "der"} {:word "var"} {:word "en"} {:word "lille"}]
   :match  [{:word "hund"}]
   :right  [{:word "gør"} {:word "."}]})

(def other
  {:corpus  "PROBE" :cpos 20
   :left    []
   :match   [{:word "kat"}]
   :right   [{:word "sover"}]
   :anchors {:match 20 :matchend 20}})

(def data
  "A page as the server sends it."
  {:lang            "en"
   :title           "hund"
   :folders         folders
   :filter-controls filters
   :params          {:q "hund" :corpus ["PROBE"]}
   :tokens          (query/form-rows nil)
   :result          {:hits [hit other] :remaining ["PROBE"]}})

(def state
  (actions/data->state data "http://localhost/search?q=hund&corpus=PROBE"))

(def view-actions
  "Every action keyword the views' :on maps dispatch, read off the views
  by hand, so a rename on one side is caught here rather than by a
  control that quietly does nothing."
  #{:set-mode :add-token :remove-token :add-condition :remove-condition
    :set-query :submit-on-enter :set-condition :set-token :apply-view
    :toggle-corpora :toggle-filter-values :clear-filter :engage
    :toggle-open :filter :leave :swallow-enter :inspect :close
    :move-cursor :leave-concordance :forget-searches})

(deftest act-covers-every-view-action-test
  (let [examples [[:set-mode "extended" {:q "hund" :mode "extended"}]
                  [:add-token]
                  [:remove-token 1]
                  [:add-condition 1]
                  [:remove-condition [1 1]]
                  [:set-query "hund"]
                  [:submit-on-enter "Enter" false false]
                  [:set-condition [1 1 :v] "hund"]
                  [:set-token [1 :min] "2"]
                  [:apply-view "sort" "word"]
                  [:toggle-corpora ["PROBE"]]
                  [:toggle-filter-values [:text_year ["1591"]]]
                  [:clear-filter]
                  [:engage :corpora]
                  [:toggle-open :corpora :root true]
                  [:filter :corpora "x"]
                  [:leave :corpora true]
                  [:swallow-enter "Enter"]
                  [:inspect {:token {:word "hund"}} [["PROBE" 9] 0]]
                  [:close]
                  [:move-cursor [["PROBE" 9] 0] "ArrowRight" false]
                  [:leave-concordance]
                  [:forget-searches]]]
    (testing "one example per action the views emit, and each is answered"
      (is (= view-actions (set (map first examples))))
      (doseq [[kind :as action] examples]
        (is (map? (:state (actions/act state action))) (str kind))))
    (testing "the render hooks are effects, never actions"
      (is (nil? (actions/act state [:set-validity "x"])))
      (is (nil? (actions/act state [:set-checkbox-state {}]))))))

(deftest data->state-test
  (testing "marked as the client's, the lists at rest, the filters
            remembered by their corpora, and the trailing blank token gone"
    (is (true? (:client? state)))
    (is (nil? (:fragment state)))
    (is (= ["PROBE"] (:filters-for state)))
    (is (= {:choosing? false :unticked #{}}
           (dissoc (get-in state [:lists :corpora]) :open)))
    (is (= 1 (count (:tokens state))))
    (is (= ["hund"]
           (map #(get-in % [:conditions 0 :v])
                (:tokens (actions/data->state
                          (assoc data :tokens
                                 (query/form-rows (query/of {:q "hund"})))
                          "http://localhost/search"))))))
  (testing "the fragment the URL names"
    (is (= "top" (:fragment (actions/data->state
                             data "http://localhost/search#top"))))))

(deftest page-arrived-test
  (let [href "http://localhost/search?q=hund&corpus=PROBE#results"
        {state' :state :keys [effects]} (actions/page-arrived {} data href true)]
    (testing "the state of the page, with the results fragment dropped"
      (is (true? (:client? state')))
      (is (nil? (:fragment state'))))
    (testing "the address pushed without that fragment, then the title, the
              language, the URL, the fetches and the landing"
      (is (= [[:push-url "http://localhost/search?q=hund&corpus=PROBE"]
              [:set-title "hund"]
              [:set-lang "en"]
              [:sync-url]
              [:fetch-counts]
              [:land]
              [:select-query]]
             effects)))
    (testing "a popstate pushes nothing"
      (is (= [:set-title "hund"]
             (first (:effects (actions/page-arrived {} data href false)))))))
  (testing "a page that is not a search keeps the link back to the one
            before it, having no search of its own to build one from"
    (let [came-from {:nav {:search "/search?q=hund#results"}}
          arrive    (fn [page href]
                      (get-in (:state (actions/page-arrived came-from page
                                                            href true))
                              [:nav :search]))]
      (is (= "/search?q=hund#results"
             (arrive (assoc data :route :corpora :nav {:search url/search})
                     "http://localhost/corpora")))
      (testing "and a search page brings its own, which replaces it"
        (is (= "/search?q=kat#results"
               (arrive (assoc data :route :search
                              :nav {:search "/search?q=kat#results"})
                       "http://localhost/search?q=kat")))))))

(deftest cursor-test
  (testing "along a row the cursor steps and stops at the ends"
    (let [hits (actions/hits state)]
      (is (= [["PROBE" 9] 1] (actions/step-cursor hits [["PROBE" 9] 0] [0 1])))
      (is (= [["PROBE" 9] 3] (actions/step-cursor hits [["PROBE" 9] 3] [0 1])))
      (is (= [["PROBE" 9] 0] (actions/step-cursor hits [["PROBE" 9] 0] [0 -1])))
      (testing "and between rows it keeps its distance from the match"
        (is (= [["PROBE" 20] 0]
               (actions/step-cursor hits [["PROBE" 9] 2] [1 0])))
        (is (= [["PROBE" 9] 2]
               (actions/step-cursor hits [["PROBE" 20] 0] [-1 0]))))))
  (testing "a key moves the cursor, focus with it, and consumes the key"
    (let [{state' :state :keys [effects]}
          (actions/move-cursor state [["PROBE" 9] 0] "ArrowRight" false)]
      (is (= [["PROBE" 9] 1] (:cursor state')))
      ;; the view of the concordance is the concordance's own to move
      (is (= [[:prevent-default] [:focus "t-PROBE-9-1" true]] effects)))
    (is (= [["PROBE" 9] 3]
           (:cursor (:state (actions/move-cursor state [["PROBE" 9] 0]
                                                 "End" false)))))
    (is (= [["PROBE" 9] 0]
           (:cursor (:state (actions/move-cursor state [["PROBE" 9] 3]
                                                 "Home" false))))))
  (testing "and the Ctrl chords a text field takes for the ends of a line
            go to the same ends, whatever the shift key did to the letter"
    (is (= [["PROBE" 9] 3]
           (:cursor (:state (actions/move-cursor state [["PROBE" 9] 0]
                                                 "e" true)))))
    (is (= [["PROBE" 9] 0]
           (:cursor (:state (actions/move-cursor state [["PROBE" 9] 3]
                                                 "A" true)))))
    (testing "and without Ctrl they are letters the browser can have"
      (is (= {:state state}
             (actions/move-cursor state [["PROBE" 9] 0] "a" false)))
      (is (= {:state state}
             (actions/move-cursor state [["PROBE" 9] 0] "e" false)))))
  (testing "Escape closes the panel and nothing else moves"
    (let [{state' :state :keys [effects]}
          (actions/move-cursor (assoc state :selected {:token {}})
                               [["PROBE" 9] 0] "Escape" false)]
      (is (not (contains? state' :selected)))
      (is (= [[:prevent-default]] effects))))
  (testing "any other key is left to the browser"
    (is (= {:state state}
           (actions/move-cursor state [["PROBE" 9] 0] "x" false)))
    (is (= {:state state}
           (actions/move-cursor state [["PROBE" 9] 0] "x" true)))))

(deftest widen-test
  (let [reached (assoc-in state [:result :reach] 30)]
    (testing "coming to the end of a line asks for twice as much of it"
      (let [{state' :state :keys [effects]} (actions/widen reached 1 hit 1)]
        (is (= [[:fetch-wider 60]] effects))
        (is (= 1 (get-in state' [:result :widening])))))
    (testing "but not at the end its text already reaches, and not at the
              other end of a hit bounded at this one"
      (let [ended (assoc hit :bounds #{:end})]
        (is (nil? (:effects (actions/widen reached 1 ended 1))))
        (is (= [[:fetch-wider 60]]
               (:effects (actions/widen reached -1 ended -1))))))
    (testing "and nothing is asked for between rows, while a fetch is out,
              once no page comes back wider, or where the context is a
              unit of text, which bounds itself"
      (doseq [[what state* tokens]
              [["between rows" reached 0]
               ["already out"  (assoc-in reached [:result :widening] 1) 1]
               ["none wider"   (assoc-in reached [:result :widest?] true) 1]
               ["a unit"       (assoc-in state [:result :reach] :sentence) 1]]]
        (is (nil? (:effects (actions/widen state* tokens hit tokens))) what)))))

(deftest running-out-test
  ;; nine tokens, the match in the middle
  (let [row {:left (vec (repeat 4 {})) :match [{}] :right (vec (repeat 4 {}))}]
    (testing "the line gives out within the window the reader can see
              ahead of the cursor: the context asked for, and the fade"
      ;; context 1 + 3 fade steps: four tokens ahead is near enough
      (is (actions/running-out? row [nil 4] 1 1))
      (is (not (actions/running-out? row [nil 3] 1 1)))
      (testing "and the same counting back the other way"
        (is (actions/running-out? row [nil 4] -1 1))
        (is (not (actions/running-out? row [nil 5] -1 1)))))
    (testing "nothing runs out between rows, or where the context is a
              unit of text"
      (is (not (actions/running-out? row [nil 8] 0 1)))
      (is (not (actions/running-out? row [nil 8] 1 :sentence))))))

(deftest carried-cursor-test
  ;; hit: en lille [hund] gør, wider: der var en lille [hund] gør .
  (testing "the cursor keeps its word where more context to the left has
            moved it along the row, and takes the step it was refused"
    (is (= [["PROBE" 9] 4]
           (actions/carried-cursor [["PROBE" 9] 2] [hit] [wider] 0)))
    (is (= [["PROBE" 9] 6]
           (actions/carried-cursor [["PROBE" 9] 3] [hit] [wider] 1))))
  (testing "a cursor on no hit of this page is left where it is"
    (is (= [["VISER" 1] 0]
           (actions/carried-cursor [["VISER" 1] 0] [hit] [wider] 1)))))

(deftest wider-arrived-test
  (let [reached (-> state
                    (assoc-in [:result :reach] 30)
                    (assoc-in [:result :widening] 1)
                    (assoc :cursor [["PROBE" 9] 3]))]
    (testing "a wider page comes with the cursor's word and the step the
              end of the line refused"
      (let [{state' :state :keys [effects]}
            (actions/wider-arrived reached 60
                                   (assoc-in data [:result :hits]
                                             [wider other]))]
        (is (= [wider other] (get-in state' [:result :hits])))
        (is (= 60 (get-in state' [:result :reach])))
        (is (= [["PROBE" 9] 6] (:cursor state')))
        (is (= [[:focus "t-PROBE-9-6" true]] effects))
        (is (not (contains? (:result state') :widening)))))
    (testing "and one that came back no wider is all the line there is"
      (let [{state' :state} (actions/wider-arrived reached 60 data)]
        (is (true? (get-in state' [:result :widest?])))
        (is (not (contains? (:result state') :widening)))
        (is (= [["PROBE" 9] 3] (:cursor state')))))))

(deftest inspect-and-close-test
  (let [selected {:corpus "PROBE" :token {:word "hund"}}
        cursor   [["PROBE" 9] 3]]
    (is (= selected (:selected (actions/inspect state selected cursor))))
    (testing "the cursor follows what is inspected, so the tabbable token
              is the one the reader is on however they got there"
      (is (= cursor (:cursor (actions/inspect state selected cursor)))))
    (testing "and dismissing takes the cursor with it, so the concordance
              rests on its matches again rather than where the reader
              stopped reading"
      (let [dismissed (actions/inspect (assoc state
                                              :selected selected
                                              :cursor cursor)
                                       nil nil)]
        (is (not (contains? dismissed :selected)))
        (is (not (contains? dismissed :cursor)))))
    (let [{state' :state :keys [effects]}
          (actions/close (assoc state :selected selected))]
      (is (not (contains? state' :selected)))
      (is (= [[:focus concordance/region-id]] effects)))))

(deftest tokens-test
  (let [state (assoc state :tokens [{:id 1 :conditions [{:id 1 :v "a"}]}
                                    {:id 3 :conditions [{:id 1}]}])]
    (testing "a token is added after the last and its attribute focused"
      (let [{state' :state :keys [effects]} (actions/add-token state)]
        (is (= [1 3 4] (map :id (:tokens state'))))
        (is (= [[:focus-field "t3.attr"]] effects))))
    (testing "a token removed focuses the one in its place, or the last"
      (is (= {:state   (assoc state :tokens [{:id 3 :conditions [{:id 1}]}])
              :effects [[:focus-field "t1.attr"]]}
             (actions/remove-token state 1)))
      (is (= [[:focus-field "t1.attr"]]
             (:effects (actions/remove-token state 3))))
      (testing "and the last token is replaced by a blank rather than gone"
        (is (= [(tokens/blank-token 2)]
               (:tokens (:state (actions/remove-token
                                 (assoc state :tokens
                                        [{:id 1 :conditions [{:id 1}]}])
                                 1)))))))
    (testing "a condition is added to its token and its join focused"
      (let [{state' :state :keys [effects]} (actions/add-condition state 1)]
        (is (= [{:id 1 :v "a"} {:id 2}]
               (get-in state' [:tokens 0 :conditions])))
        (is (= [[:focus-field "t1.2.join"]] effects))))
    (testing "a condition removed focuses its successor's join, or a first
              condition's attribute"
      (let [state (assoc-in state [:tokens 0 :conditions]
                            [{:id 1} {:id 2} {:id 3}])]
        (is (= [[:focus-field "t1.2.join"]]
               (:effects (actions/remove-condition state 1 2))))
        (is (= [[:focus-field "t1.attr"]]
               (:effects (actions/remove-condition state 1 1))))
        (is (= [{:id 1} {:id 2}]
               (get-in (:state (actions/remove-condition state 1 3))
                       [:tokens 0 :conditions])))))
    (testing "a field is recorded as typed, a checkbox as on or nothing"
      (is (= "hund" (get-in (actions/set-condition state [1 1 :v] "hund")
                            [:tokens 0 :conditions 0 :v])))
      (is (= "on" (get-in (actions/set-condition state [1 1 :ci] true)
                          [:tokens 0 :conditions 0 :ci])))
      (is (= {:id 1 :v "a"}
             (get-in (actions/set-condition
                      (assoc-in state [:tokens 0 :conditions 0 :ci] "on")
                      [1 1 :ci] false)
                     [:tokens 0 :conditions 0])))
      (is (= "2" (get-in (actions/set-token state [2 :min] "2")
                         [:tokens 1 :min])))
      (is (= "on" (get-in (actions/set-token state [2 :start] true)
                          [:tokens 1 :start]))))))

(deftest switch-mode-test
  (let [simple (assoc state :params {:q "lille hund" :in "lemma"
                                     :corpus ["PROBE"]})
        live   (assoc (:params simple) :mode "extended")
        ext    (actions/switch-mode simple "extended" live)]
    (testing "the words seed the tokens, each with the field's options,
              and nothing is lost"
      (is (= "extended" (get-in ext [:params :mode])))
      (is (= ["lille" "hund"]
             (map #(get-in % [:conditions 0 :v]) (:tokens ext))))
      (is (= ["lemma" "lemma"]
             (map #(get-in % [:conditions 0 :attr]) (:tokens ext))))
      (is (= ["PROBE"] (get-in ext [:params :corpus])))
      (is (= {:loss [] :unread #{}} (:switch ext))))
    (testing "switching back with nothing edited gives the words back"
      (let [back (actions/switch-mode ext "simple"
                                      (assoc (:params ext) :mode "simple"))]
        (is (= "lille hund" (get-in back [:params :q])))
        (is (= "lemma" (get-in back [:params :in])))
        (is (= (:tokens simple) (:tokens back)))
        (is (= [] (get-in back [:switch :loss])))))
    (testing "an edit in between is carried across as CQP instead"
      (let [edited (assoc-in ext [:params :t1.v] "store")
            back   (actions/switch-mode edited "simple"
                                        (assoc (:params edited)
                                               :mode "simple"))]
        (is (re-find #"store" (get-in back [:params :q])))))
    (testing "CQP the extended form cannot hold is reported as lost"
      (let [cqp (assoc state :params {:q "[lemma = \"hund\"]"})
            ext (actions/switch-mode cqp "extended"
                                     (assoc (:params cqp) :mode "extended"))]
        (is (= :cqp (ffirst (get-in ext [:switch :loss]))))))))

(deftest enter-test
  (testing "Enter in the field submits at once, not through the wait a view
            control is applied after: the reader has asked for this one"
    (is (= [[:prevent-default] [:resubmit url/form-id]]
           (:effects (actions/submit-on-enter state "Enter" false false))))
    (is (nil? (:effects (actions/submit-on-enter state "Enter" true false))))
    (is (nil? (:effects (actions/submit-on-enter state "Enter" false true))))
    (is (nil? (:effects (actions/submit-on-enter state "a" false false)))))
  (testing "Enter in a filter box is swallowed"
    (is (= [[:prevent-default]]
           (:effects (actions/swallow-enter state "Enter"))))
    (is (nil? (:effects (actions/swallow-enter state "a"))))))

(deftest selection-test
  (testing "a corpus toggled joins the sorted selection, is noted for the
            chooser, and asks the filters to refresh"
    (let [{state' :state :keys [effects]}
          (actions/toggle-corpora state ["ANDEN"])]
      (is (= ["ANDEN" "PROBE"] (get-in state' [:params :corpus])))
      (is (= [[:refresh-filters]] effects))
      (is (= ["PROBE"]
             (get-in (:state (actions/toggle-corpora state' ["ANDEN"]))
                     [:params :corpus])))))
  (testing "a value toggled and the filter cleared are noted for its list"
    (let [chosen (actions/toggle-filter-values state :text_year ["1583"])]
      (is (= #{"1583" "1591"} (get-in chosen [:filter-controls :selected
                                              :text_year])))
      (is (= {} (get-in (actions/clear-filter chosen)
                        [:filter-controls :selected])))
      (is (= #{[:text_year "1583"] [:text_year "1591"]}
             (get-in (actions/clear-filter chosen)
                     [:lists :values :unticked])))))
  (testing "an attribute a pattern or a range narrows is emptied by its
            own control, fields and boxes together: the boxes cannot show
            that narrowing, so nothing else takes it back"
    (doseq [[what state] [["a pattern"
                           (assoc-in state [:filter-controls :patterns
                                            :text_year] "15..")]
                          ["a range"
                           (assoc-in state [:filter-controls :ranges
                                            :text_year] ["1583" nil])]]]
      (testing what
        (let [cleared (actions/toggle-filter-values state :text_year ["1591"])]
          (is (empty? (get-in cleared [:filter-controls :patterns])))
          (is (empty? (get-in cleared [:filter-controls :ranges])))
          (is (empty? (get-in cleared [:filter-controls :selected])))))))
  (testing "and the whole filter is cleared the same way, the patterns and
            the ranges with the values"
    (let [state   (-> state
                      (assoc-in [:filter-controls :patterns :text_year] "15..")
                      (assoc-in [:filter-controls :ranges :s_id] ["1" "9"]))
          cleared (actions/clear-filter state)]
      (is (empty? (get-in cleared [:filter-controls :patterns])))
      (is (empty? (get-in cleared [:filter-controls :ranges])))
      (is (empty? (get-in cleared [:filter-controls :selected])))))
  (testing "the list actions answer with a refresh only for the metadata
            filter"
    (is (= [[:refresh-filters]]
           (:effects (actions/act state [:engage :values]))))
    (is (nil? (:effects (actions/act state [:engage :corpora]))))
    (is (identical? state
                    (:state (actions/act state [:leave :corpora false]))))))

(deftest filters-test
  (let [looking (update-in state [:lists :values :open] conj :root)
        moved   (assoc-in looking [:params :corpus] ["ANDEN"])]
    (testing "the debounce elapsing fetches only when the filters are stale"
      (is (= {:state state} (actions/filters-due state)))
      (is (= {:state   (assoc moved :filters-pending? true)
              :effects [[:fetch-filters ["ANDEN"]]]}
             (actions/filters-due moved))))
    (testing "filters arriving for the selection replace the attributes and
              remember it"
      (let [arrived (actions/filters-arrived
                     (assoc moved :filters-pending? true) ["ANDEN"]
                     {:attrs [{:name :text_party :rows []}] :unlisted []
                      :selected {}})]
        (is (false? (:filters-pending? arrived)))
        (is (= ["ANDEN"] (:filters-for arrived)))
        (is (= [:text_party] (map :name (get-in arrived [:filter-controls
                                                         :attrs]))))
        (is (= {:text_year #{"1591"}}
               (get-in arrived [:filter-controls :selected])))))
    (testing "a slow answer to a selection the reader left is dropped"
      (is (= (assoc moved :filters-pending? false)
             (actions/filters-arrived moved ["PROBE"] {:attrs []}))))))

(deftest counts-arrived-test
  (let [{state' :state :keys [effects]}
        (actions/counts-arrived state {:title    "hund (2)"
                                       :counts   {"PROBE" 2}
                                       :size     2
                                       :pages    1
                                       :next-href nil})]
    (is (= {"PROBE" 2} (get-in state' [:result :counts])))
    (is (not (contains? (:result state') :remaining)))
    ;; the count is what says whether the search found anything, so the
    ;; field is offered again only once it has
    (is (= [[:set-title "hund (2)"] [:select-query]] effects))))

(def answered
  "A search page as the server sends one that found something."
  (assoc data
         :route  :search
         :asked  {:q "hund" :corpus ["PROBE"] :sort "word"}
         :result {:hits   [hit other]
                  :size   2
                  :counts [{:corpus "PROBE" :size 2}]
                  :filter {:text_year #{"1591"}}}))

(deftest remember-test
  (let [{state' :state :keys [effects]}
        (actions/remember (actions/data->state answered "http://localhost/"))]
    (testing "the question asked, what it was narrowed by and what it
              found, stored"
      (is (= [{:params "q=hund&corpus=PROBE"
               :hits   2
               :filter "text_year 1591"}]
             (:recent state')))
      (is (= [[:store-recent]] effects)))
    (testing "a search still being counted is remembered without a count,
              and takes one when the count arrives"
      (let [counting (assoc-in (actions/data->state answered "http://localhost/")
                               [:result :remaining] ["PROBE"])
            state''  (:state (actions/remember counting))]
        (is (= [{:params "q=hund&corpus=PROBE" :filter "text_year 1591"}]
               (:recent state'')))
        (is (= [{:params "q=hund&corpus=PROBE"
                 :hits   47
                 :filter "text_year 1591"}]
               (:recent (:state (actions/counts-arrived
                                 state'' {:title  "hund (47)"
                                          :counts [{:corpus "PROBE" :size 47}]
                                          :size   47
                                          :pages  1}))))))))
  (testing "a sample of the hits is the same question, and counts a part
            of what it found: the entry keeps the count it had"
    (let [sampled (-> (actions/data->state answered "http://localhost/")
                      (assoc-in [:asked :sample] "1")
                      (assoc-in [:result :size] 1))]
      (is (= [{:params "q=hund&corpus=PROBE" :hits 2 :filter "text_year 1591"}]
             (:recent (:state (actions/remember
                               (assoc sampled :recent
                                      [{:params "q=hund&corpus=PROBE"
                                        :hits   2}]))))))))
  (testing "nothing to remember on another page, before an answer, or
            where no corpus could be searched"
    (doseq [[what state] {"a corpus page" (assoc answered :route :corpora)
                          "a bare form"   (dissoc answered :result)
                          "a failure"     (assoc answered :result
                                                 {:counts [{:corpus "PROBE"
                                                            :error {}}]})}]
      (let [{state' :state :keys [effects]} (actions/remember state)]
        (is (nil? (:recent state')) what)
        (is (nil? effects) what))))
  (testing "forgetting is storing nothing, and the box takes the focus
            the button it quietens cannot hold, with what happened said"
    (let [{state' :state :keys [effects]}
          (actions/forget-searches {:recent [{:params "q=hund"}]})]
      (is (= [] (:recent state')))
      (is (= :cleared (:announcement state')))
      (is (= [[:store-recent] [:focus "recent"]] effects))))
  (testing "and the saying of it belongs to the act, so the next one
            takes it away"
    (is (nil? (:announcement (:state (actions/act {:announcement :cleared}
                                                  [:pending])))))))

(deftest set-query-test
  (let [answered (actions/data->state answered "http://localhost/search?q=hund")]
    (testing "every keystroke goes into the state, the answer standing
              while the field still asks something"
      (let [{state' :state :keys [effects]} (actions/set-query answered "hun")]
        (is (= "hun" (get-in state' [:params :q])))
        (is (some? (:result state')))
        (is (nil? effects))))
    (testing "emptying the field is starting over: the answer goes, and
              the address that cited it goes with it, onto the history"
      (doseq [blank ["" "  " "\n"]]
        (let [{state' :state :keys [effects]} (actions/set-query answered blank)]
          (is (= blank (get-in state' [:params :q])) blank)
          (is (not-any? state' actions/answer-keys) blank)
          ;; the title named the answer too, and no server titled this
          (is (= [[:set-title "Search · corpus-probe"]
                  [:push-url "/search"]
                  [:sync-url]]
                 effects)
              blank))))
    (testing "and the form is left as it stands, so the corpora a reader
              chose are still chosen"
      (is (= ["PROBE"] (get-in (:state (actions/set-query answered ""))
                               [:params :corpus]))))
    (testing "a field emptied where nothing was answered does nothing but
              hold what was typed"
      (let [{state' :state :keys [effects]}
            (actions/set-query (dissoc answered :result :error) "")]
        (is (nil? effects))
        (is (= "" (get-in state' [:params :q])))))))

(deftest cleared-page-test
  (testing "what a cleared field leaves on the page: the guide and the
            searches made lately, where the answer stood"
    (let [state (-> (assoc answered :help [[:p "Type a word."]])
                    (actions/data->state "http://localhost/search?q=hund")
                    (assoc :recent [{:params "q=kat"}])
                    (actions/set-query "")
                    (:state))
          html  (deep (views/search-page state))]
      (is (not (some #{:section.result} html)))
      (is (not (some #{:nav.tabs} html)))
      (is (some #{:section.help} html))
      (is (some #{"Type a word."} html))
      (is (some #{:nav.recent.box} html)))))

(deftest recent-carried-test
  (testing "the searches remembered are the client's own, and are carried
            across a page arriving, which brings none of its own"
    (is (= [{:params "q=kat"}]
           (:recent (:state (actions/page-arrived {:recent [{:params "q=kat"}]}
                                                  data "http://localhost/search"
                                                  false)))))))

(deftest pass-through-test
  (testing "the world's answers and the listeners' asks"
    (is (= "top"
           (:fragment (:state (actions/act state [:set-fragment "top"])))))
    (is (true? (:pending? (:state (actions/act state [:pending])))))
    (is (= [[:navigate "http://localhost/" true]]
           (:effects (actions/act state [:navigate "http://localhost/" true]))))
    (is (= [[:set-cookie :lang "da"] [:navigate "/search?q=hund" false]]
           (:effects (actions/act state [:set-preference "lang" "da"
                                         "/search?q=hund"]))))
    (testing "a reset goes to the bare form, and onto the history only
              when that is somewhere else"
      (is (= [[:set-cookie :settings ""] [:navigate "/search" true]]
             (:effects (actions/act state [:set-preference "settings" ""
                                           "/search?q=hund"]))))
      (is (= [[:set-cookie :settings ""] [:navigate "/search" false]]
             (:effects (actions/act state [:set-preference "settings" ""
                                           "/search"])))))
    (testing "storing the settings asks the server for nothing: the page
              stays as it is and only what the box measures against moves"
      (let [{state' :state :keys [effects]}
            (actions/act state [:set-preference "settings" "sort=word" "/search"])]
        (is (= [[:set-cookie :settings "sort=word"]
                ;; the button that had focus goes quiet as it is pressed
                [:focus settings/box-id]]
               effects))
        (is (= "sort=word" (:stored state')))
        (testing "and says so where it is heard and not seen"
          (is (= :saved (:announcement state'))))))
    (testing "an announcement belongs to the act that made it, so the
              reader's next move takes it away and the same thing said
              twice is heard twice"
      (let [said (:state (actions/act state [:set-preference "settings"
                                             "sort=word" "/search"]))]
        (is (nil? (:announcement (:state (actions/act said [:pending])))))))
    ;; the control's value is taken into the form's own params before the
    ;; search is asked for: the submit waits for the control to hold still,
    ;; and a render landing in between draws every control from the state
    (let [applied (actions/act state [:apply-view "sort" "word"])]
      (is (= "word" (get-in applied [:state :params :sort])))
      (is (= [[:apply-view url/form-id]] (:effects applied))))
    (is (= false (get-in (actions/act state [:apply-view "docs" false])
                         [:state :params :docs])))
    (is (= [[:leave-concordance]]
           (:effects (actions/act state [:leave-concordance]))))
    ;; a resize moves the strip the concordance is read in, not the state
    (is (= {:state state :effects [[:recentre]]}
           (actions/act state [:recentre])))
    (is (false? (:filters-pending? (:state (actions/act state
                                                        [:filters-failed])))))))

(deftest form-changed-test
  (let [state   (assoc state :selectable #{"PROBE" "VISER"} :stored ""
                       :params {} :autosave? true)
        changed (fn [state params] (actions/act state [:form-changed params]))]
    (testing "the form's own controls reach the state, matching options
              among them, which this client answers none of itself"
      (is (= {:in "lemma" :corpus ["PROBE"]}
             (:params (:state (changed state {:q      "hund"
                                              :in     "lemma"
                                              :corpus ["PROBE"]}))))))
    (testing "one corpus arrives as a name and several as a vector, and
              the chooser reads a selection of names either way"
      (is (= ["PROBE"] (:corpus (:params (:state (changed state
                                                          {:corpus "probe"}))))))
      (is (= ["PROBE" "VISER"]
             (:corpus (:params (:state (changed state
                                                {:corpus ["PROBE" "VISER"]})))))))
    (testing "and are stored as they change, so a change the reader did
              not search on survives their going somewhere else"
      (let [{state' :state :keys [effects]} (changed state {:in "lemma"})]
        (is (= [[:set-cookie :settings "in=lemma"]] effects))
        (is (= "in=lemma" (:stored state')))))
    (testing "every corpus chosen is stored as a scope, so the form comes
              back with them ticked rather than empty"
      (is (= [[:set-cookie :settings "scope=all"]]
             (:effects (changed state {:corpus ["PROBE" "VISER"]})))))
    (testing "a form at the app's own defaults is stored as nothing,
              which is what having stored nothing means"
      (is (= [[:set-cookie :settings ""]]
             (:effects (changed (assoc state :stored "in=lemma")
                                {:in "word"})))))
    (testing "nothing is stored for a change the settings do not carry"
      (is (nil? (:effects (changed state {:q "hund"})))))
    (testing "nor while the reader has turned storing off"
      (is (nil? (:effects (changed (assoc state :autosave? false)
                                   {:in "lemma"})))))))

(deftest set-autosave-test
  (let [state (assoc state :autosave? true :selectable #{"PROBE" "VISER"}
                     :stored "corpus=PROBE" :params {:corpus ["PROBE" "VISER"]})]
    (testing "turning storing off stores that choice at once, which is the
              one thing it cannot leave to the button beside it"
      (let [{state' :state :keys [effects]} (actions/act state
                                                         [:set-autosave false])]
        (is (false? (:autosave? state')))
        (is (= [[:set-cookie :settings "corpus=PROBE&autosave=off"]] effects))))
    (testing "and only that choice: a form the reader has not stored is
              not stored by their turning storing off"
      (is (= [[:set-cookie :settings "corpus=PROBE&autosave=off"]]
             (:effects (actions/act state [:set-autosave false])))))
    (testing "the state keeps up with what is stored, so the button beside
              it is not offered a change that has already been made"
      (is (= "corpus=PROBE&autosave=off"
             (:stored (:state (actions/act state [:set-autosave false]))))))
    (testing "turning it back on takes that choice out of the settings
              and stores the form in front of the reader, which is what
              asking for it to be stored from now on means"
      (let [off (:state (actions/act state [:set-autosave false]))]
        (is (= [[:set-cookie :settings "scope=all"]]
               (:effects (actions/act off [:set-autosave true]))))))))
