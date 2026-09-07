(ns dk.cst.corpus-probe.views.concordance-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.hiccup :refer [deep]]
            [dk.cst.corpus-probe.test.hiccup :refer [da en text]]
            [dk.cst.corpus-probe.cwb.command :as command]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.concordance :as concordance]))

(deftest token-title-test
  (testing "non-word attributes join into the tooltip"
    (is (= "NCSI · hund" (concordance/token-title {:word "hund" :pos "NCSI"
                                                   :lemma "hund"}))))
  (testing "structure tags and blank values are excluded"
    (is (= "NCSI" (concordance/token-title {:word "hund" :pos "NCSI" :lemma ""
                                            :open [:s]})))))

(deftest token-data-test
  (testing "every annotation but the surface word becomes a data-* attribute"
    (is (= {:data-pos "NCSI" :data-lemma "hund"}
           (concordance/token-data {:word "hund" :pos "NCSI" :lemma "hund"}))))
  (testing "structure tags are not emitted as data"
    (is (= {:data-pos "NCSI"}
           (concordance/token-data {:word "x" :pos "NCSI" :open [:s]})))))

(deftest token-count-test
  (is (= 3 (concordance/token-count {:left [{}] :match [{}] :right [{}]})))
  (is (= 1 (concordance/token-count {:match [{}]}))))

(deftest offsets-test
  (let [hit {:left [{} {}] :match [{}] :right [{} {} {}]}]
    (testing "an offset counts from the match, negative before it"
      (is (= -2 (concordance/token->offset hit 0)))
      (is (= 0 (concordance/token->offset hit 2)))
      (is (= 3 (concordance/token->offset hit 5))))
    (testing "and back to the nearest token a row has"
      (is (= 2 (concordance/offset->token hit 0)))
      (is (= 0 (concordance/offset->token hit -9)))
      (is (= 5 (concordance/offset->token hit 9))))))

(deftest default-cursor-test
  (testing "exactly one token is tabbable, the first, so the concordance is
            one tab stop rather than hundreds"
    (is (= [["PROBE" 9] 0] (concordance/default-cursor [{:corpus "PROBE" :cpos 9}])))
    (is (nil? (concordance/default-cursor [])))))

(deftest token-test
  (let [m         {:word "hund" :pos "NCSI"}
        hit       {:corpus "PROBE" :cpos 9}
        source    {:corpus "PROBE" :structs {:text_title "Hverdag"}}
        [tag attrs] (concordance/token {} hit source 0 m)]
    (testing "the surface form is the text content, annotations are data"
      (is (= "hund" (last (concordance/token {} hit source 0 m))))
      (is (= "NCSI" (:data-pos attrs))))
    (testing "without a client nothing answers a click, so it is not a control"
      (is (= :span.token tag))
      (testing "and it carries no handler no renderer would read"
        (is (not (contains? attrs :on)))))
    (testing "with one it is a button the cursor can rest on"
      (let [[tag attrs] (concordance/token {:client? true :cursor [["PROBE" 9] 0]}
                                           hit source 0 m)]
        (is (= :button.token tag))
        (is (= "button" (:type attrs)))
        (is (= "t-PROBE-9-0" (:id attrs)))
        (is (= "0" (:tabindex attrs)))
        (testing "and inspecting follows focus, not only a press"
          (is (= [:inspect (assoc source :token m)]
                 (get-in attrs [:on :focus])))
          (is (= [:move-cursor [["PROBE" 9] 0] :event/key]
                 (get-in attrs [:on :keydown]))))))
    (testing "every other token is out of the tab order"
      (is (= "-1" (:tabindex (second (concordance/token
                                      {:client? true :cursor [["PROBE" 9] 0]}
                                      hit source 1 m))))))
    (testing "a corpus that annotates nothing gets no empty tooltip"
      (is (not (contains? (second (concordance/token {} hit source 0 {:word "hund"}))
                          :title))))
    (testing "the token an anchor falls on is marked and named as such"
      (let [[_ attrs] (concordance/token {:anchored {0 :target}} hit source 0 m)]
        (is (= "target" (:class attrs)))
        (is (= "target · NCSI" (:title attrs))))
      (is (= "keyword" (:title (second (concordance/token {:anchored {0 :keyword}}
                                                          hit source 0
                                                          {:word "hund"})))))
      (is (= "keyword" (concordance/anchor-class :keyword))))))

(deftest source-label-test
  (testing "a text title is a cited work"
    (is (= [:cite "Hverdag"]
           (concordance/source-label {:text_id "t1" :text_title "Hverdag"}))))
  (testing "falls back to text_id, then any value"
    (is (= "t1" (concordance/source-label {:text_id "t1"})))
    (is (= "x" (concordance/source-label {:other "x"})))))

(deftest position-data-test
  (is (= {:data-cpos "9" :data-matchend "10"}
         (concordance/position-data 9 {:matchend 10 :target nil :keyword nil})))
  (testing "target and keyword anchors appear only when set"
    (is (= {:data-cpos "9" :data-matchend "9" :data-target "9"}
           (concordance/position-data 9 {:matchend 9 :target 9 :keyword nil})))))

(def sample-hit
  {:corpus  "PROBE"
   :cpos    9
   :anchors {:matchend 9 :target nil :keyword nil}
   :left    [{:word "lille" :pos "AN" :lemma "lille"}]
   :match   [{:word "hund" :pos "NCSI" :lemma "hund"}]
   :right   [{:word "i" :pos "PP" :lemma "i"}]
   :structs {:text_title "Hverdag"}})

(def client
  "Concordance options as the client renders them."
  {:ui en :client? true})

(deftest anchored-tokens-test
  (let [hit (assoc sample-hit :anchors {:matchend 9 :target 8 :keyword 10})]
    (testing "an anchor is found by its distance from the match"
      (is (= {0 :target 2 :keyword} (concordance/anchored-tokens hit))))
    (testing "an anchor beyond the row, or unset, marks nothing"
      (is (= {} (concordance/anchored-tokens sample-hit)))
      (is (= {} (concordance/anchored-tokens
                 (assoc sample-hit :anchors {:target 20 :keyword nil})))))))

(deftest hit-row-test
  (let [row (concordance/hit-row client sample-hit false)]
    (testing "the row carries its corpus position as data"
      (is (= :tr.kwic-hit (first row)))
      (is (= "9" (:data-cpos (second row)))))
    (testing "the position is the row's header and its first cell"
      (is (= :th.kwic-cpos (first (nth row 2))))
      (is (= "row" (:scope (second (nth row 2)))))
      (is (= :td.kwic-left (first (nth row 3)))))
    (testing "the source comes last, out of the line a reader is reading"
      (is (= :td.kwic-structs (first (nth row 6)))))
    (testing "the position cell is a disclosure button showing the cpos"
      (let [button (nth (nth row 2) 2)]
        (is (= :button (first button)))
        (is (= "9" (last button)))
        (is (= "false" (:aria-expanded (second button))))
        (is (= [:toggle-context {:corpus "PROBE" :cpos 9 :matchend 9}]
               (get-in button [1 :on :click])))))
    (testing "without a client it is the bare position, not a dead control"
      (is (= "9" (nth (nth (concordance/hit-row {:ui en} sample-hit false) 2) 2))))
    (testing "aria-expanded tracks the flag, and names the row it revealed"
      (let [button (nth (nth (concordance/hit-row client sample-hit true) 2) 2)]
        (is (= "true" (:aria-expanded (second button))))
        (is (= "context-PROBE-9" (:aria-controls (second button)))))
      (testing "and names nothing while there is no such row"
        (is (not (contains? (second (nth (nth row 2) 2)) :aria-controls)))))
    (testing "the control opens its name with the position a reader can see"
      (is (= "9 · Show or hide more context"
             (get-in (nth (nth row 2) 2) [1 :aria-label])))
      (is (= "9 · Vis eller skjul mere kontekst"
             (get-in (nth (nth (concordance/hit-row {:ui da :client? true}
                                                    sample-hit false)
                               2) 2)
                     [1 :aria-label]))))
    (testing "the match is wrapped in a mark element"
      (is (= :td.kwic-match (first (nth row 4))))
      (is (= :mark (get-in row [4 1 0]))))
    (testing "the anchors mark their tokens"
      (let [row (concordance/hit-row client
                                     (assoc sample-hit
                                            :anchors {:matchend 9 :target 8})
                                     false)]
        (is (some #(and (map? %) (= "target" (:class %)))
                  (deep (nth row 3))))))))

(deftest hit-rows-test
  (testing "a hit is always two children, the second nil without an expansion"
    (let [rows (concordance/hit-rows {:ui en} sample-hit)]
      (is (= 2 (count rows)))
      (is (nil? (second rows)))))
  (testing "a failed fetch says so, in a live region: it had no page load"
    (let [rows (concordance/hit-rows {:ui en :expanded {["PROBE" 9] concordance/failed}}
                                     sample-hit)]
      (is (= 2 (count rows)))
      (is (= [:span {:role "alert"} "The context did not load."]
             (get-in (second rows) [1 2])))))
  (testing "an expanded hit adds a full-width context row after it"
    (let [ex   {:left  [{:word "en"}]
                :match [{:word "hund"}]
                :right [{:word "i"}]}
          rows (concordance/hit-rows {:ui en :client? true
                                      :expanded {["PROBE" 9] ex}}
                                     sample-hit)]
      (is (= 2 (count rows)))
      (is (= :tr.kwic-expanded (first (second rows))))
      (is (= concordance/column-count (get-in (second rows) [2 1 :colspan])))
      (testing "its tokens are inspected with the hit's corpus and structs"
        (is (some #(and (map? %) (= "PROBE" (:corpus %))
                        (= {:text_title "Hverdag"} (:structs %)))
                  (deep (second rows)))))))
  (testing "the same position in another corpus is not expanded"
    (is (nil? (second (concordance/hit-rows {:lang     "en"
                                             :expanded {["VISER" 9] {}}}
                                            sample-hit)))))
  (testing "an expanded row numbers its tokens past the row it expands, so
            no two elements of one hit share an id or the cursor"
    (let [ex   {:left [{:word "en"}] :match [{:word "hund"}]
                :right [{:word "i"}]}
          rows (concordance/hit-rows {:lang     "en" :client? true
                                      :cursor   [["PROBE" 9] 0]
                                      :expanded {["PROBE" 9] ex}}
                                     sample-hit)
          ids  (keep #(when (map? %) (:id %)) (deep rows))
          cur  (filter #(and (map? %) (= "0" (:tabindex %))) (deep rows))]
      (is (= (count ids) (count (distinct ids))))
      (is (= 1 (count cur)))
      (is (= ["t-PROBE-9-0" "t-PROBE-9-1" "t-PROBE-9-2"
              "t-PROBE-9-3" "t-PROBE-9-4" "t-PROBE-9-5"]
             (filter #(re-matches #"t-.*" %) ids)))))
  (testing "a pending placeholder shows a loading row, also a live region"
    (let [rows (concordance/hit-rows {:ui en :expanded {["PROBE" 9] concordance/loading}}
                                     sample-hit)]
      (is (= 2 (count rows)))
      (is (= [:span {:role "status"} "Loading …"]
             (get-in (second rows) [1 2])))))
  (testing "the context row is named by the disclosure that revealed it"
    (let [rows (concordance/hit-rows {:lang     "en"
                                      :expanded {["PROBE" 9] {:left [] :match []
                                                              :right []}}}
                                     sample-hit)]
      (is (= "context-PROBE-9" (:id (second (second rows))))))))

(deftest concordance-test
  (let [hits   [sample-hit (assoc sample-hit :corpus "VISER" :cpos 3)]
        html   (concordance/concordance hits {:lang    "en"
                                              :caption "Concordance"
                                              :langs   {"VISER" "da"}})
        table  (nth html 2)
        groups (nth table 3)]
    (testing "the table scrolls inside a named, focusable region of its own"
      (is (= :div.scroll (first html)))
      (is (= {:id              concordance/region-id
              :role            "region"
              :tabindex        "0"
              :aria-labelledby concordance/caption-id
              :on              {:focusout [:leave-concordance]}}
             (second html))))
    (testing "leaving it closes the panel, which describes what it holds"
      (is (= [:leave-concordance] (get-in html [1 :on :focusout]))))
    (testing "the caption names the region as well as the table"
      (is (= :table.kwic (first table)))
      (is (= concordance/caption-id (:id (second (nth table 1))))))
    (testing "every column is headed, so a data cell resolves both headers,
              and no heading is a link: the glossary is linked from the
              prose, not from the machinery"
      (is (= [:thead
              [:tr
               [:th.kwic-cpos {:scope "col"}
                [:abbr {:title "corpus position"} "cpos"]]
               [:th.kwic-left {:scope "col"} "left context"]
               [:th.kwic-match {:scope "col"} "match"]
               [:th.kwic-right {:scope "col"} "right context"]
               [:th.kwic-structs {:scope "col"} "source"]]]
             (nth table 2))))
    (testing "a heading carries its column's class, so a rule about the
              column reaches the heading too"
      (is (= [:th.kwic-structs {:scope "col"} "source"]
             (get-in table [2 1 5]))))
    (testing "hits are grouped by corpus, each group headed by its name"
      (is (= 2 (count groups)))
      ;; the count is an explicit nil where the search has none, the
      ;; header keeping its shape whether or not one arrives
      (is (= [:th {:scope "rowgroup" :colspan 5}
              [:a {:href "/corpora/probe"} [:code "PROBE"]]
              nil]
             (get-in (first groups) [2 1])))
      (is (= :tr.kwic-corpus (get-in (first groups) [2 0]))))
    (testing "and by how many hits its corpus holds in all, beside the
              name and styled as the counts in the chooser are; a corpus
              whose query failed has none to show"
      (let [groups (-> (concordance/concordance
                        hits
                        {:ui     en
                         :counts [{:corpus "PROBE" :size 1113}
                                  {:corpus "VISER" :error {:type :cqp}}]})
                       (nth 2)
                       (nth 3))]
        (is (= [" " [:small.count "(1,113)"]]
               (get-in (first groups) [2 1 3])))
        (is (nil? (get-in (second groups) [2 1 3])))))
    (testing "a group carries its corpus's language when known"
      (is (nil? (:lang (second (first groups)))))
      (is (= "da" (:lang (second (second groups))))))
    (testing "hits without a corpus form one plain group without a header"
      (let [group (first (nth (nth (concordance/concordance
                                    [(dissoc sample-hit :corpus)]
                                    {:ui en})
                                   2)
                              3))]
        (is (= :tbody (first group)))
        (is (nil? (nth group 2)))))))

(deftest source-cell-test
  (let [hit {:corpus  "PROBE"
             :cpos    9
             :anchors {:matchend 10}
             :structs {:text_title "Hverdag"}}]
    (testing "the source links to the reading page of its text, hit marked"
      (is (= [:td.kwic-structs {:title "text_title: Hverdag"}
              [:a {:href "/corpora/probe/text?cpos=9&matchend=10#hit"}
               [:cite "Hverdag"]]]
             (concordance/source-cell hit))))
    (testing "a hit that knows no corpus has nowhere to link"
      (is (= [:cite "Hverdag"] (last (concordance/source-cell (dissoc hit :corpus))))))
    (testing "and one without annotations has no source at all"
      (is (nil? (last (concordance/source-cell {:corpus "PROBE" :cpos 9})))))))

(deftest sort-label-test
  (testing "every sort mode the command namespace offers is named here"
    ;; in Danish, where no label can coincide with the param value
    (is (= ["korpusrækkefølge" "match" "match bagfra" "venstre kontekst"
            "højre kontekst" "tilfældig"]
           (map (comp (partial concordance/sort-label da) first) command/sort-modes)))
    (doseq [[value] command/sort-modes]
      (is (not (str/blank? (concordance/sort-label en value)))
          (str "sort mode " value " has no label"))))
  (testing "a mode naming an attribute is the match by that attribute"
    (is (= "match lemma" (concordance/sort-label en "lemma")))
    (is (= "match lemma" (concordance/sort-label da "lemma")))))

(deftest sort-control-test
  (let [html (concordance/sort-control en ["corpus" "word"] "word")]
    (testing "a labelled select over the modes, the chosen one marked,
              submitting the query form and applying itself"
      (is (some #{[:label {:for "sort"} "Sort"]} (deep html)))
      (is (some #(and (map? %) (= "word" (:value %)) (:selected %)) (deep html)))
      (is (some #{"corpus order"} (deep html)))
      (is (some #(and (map? %) (= "sort" (:name %)) (= url/form-id (:form %))
                      (= [:apply-view] (get-in % [:on :change])))
                (deep html))))))

(deftest sample-control-test
  (testing "no sample is the whole result, and it is what is chosen"
    (let [html (concordance/sample-control en nil)]
      (is (some #{"all hits"} (deep html)))
      (is (some #(and (map? %) (= "" (:value %)) (:selected %)) (deep html)))))
  (testing "the offered sizes are the ones the reader can choose between"
    (is (= concordance/sample-sizes
           (keep #(when (number? (:value %)) (:value %))
                 (deep (concordance/sample-control en nil))))))
  (testing "the chosen size is the one marked, and it submits the query
            form as the sort control does"
    (let [html (concordance/sample-control en 100)]
      (is (some #(and (map? %) (= 100 (:value %)) (:selected %)) (deep html)))
      (is (some #(and (map? %) (= "sample" (:name %))
                      (= url/form-id (:form %))
                      (= [:apply-view] (get-in % [:on :change])))
                (deep html)))))
  (testing "a size the list does not hold is offered beside them, in
            order, so a hand-written URL shows as the sample it is"
    (is (= [50 77 100 500 1000]
           (keep #(when (number? (:value %)) (:value %))
                 (deep (concordance/sample-control en 77)))))))

(deftest context-control-test
  (let [values (fn [html] (->> (deep html) (filter map?) (keep :value)
                               (map str)))]
    (testing "the widths offered, the chosen one marked, units by name"
      (let [html (concordance/context-control en :sentence)]
        (is (= ["5" "10" "20" "sentence" "paragraph"] (values html)))
        (is (some #(and (map? %) (= "sentence" (:value %)) (:selected %))
                  (deep html)))
        (is (some #{"5 words" "sentence" "paragraph"} (deep html)))))
    (testing "a number of words the list lacks is offered among the
              numbers, in order"
      (is (= ["5" "7" "10" "20" "sentence" "paragraph"]
             (values (concordance/context-control en 7)))))
    (testing "it submits the query form as the sort control does"
      (is (some #(and (map? %) (= "context" (:name %))
                      (= url/form-id (:form %))
                      (= [:apply-view] (get-in % [:on :change])))
                (deep (concordance/context-control en 5)))))
    (testing "in Danish"
      (is (some #{"sætning" "5 ord" "Kontekst"}
                (deep (concordance/context-control da 5)))))))

(deftest near-control-test
  (testing "no word in force: an empty field and the default distance"
    (let [html (concordance/near-control en nil)]
      (is (some #(and (map? %) (= "near" (:name %)) (= "" (:value %))
                      (= url/form-id (:form %)))
                (deep html)))
      (is (some #(and (map? %)
                      (= (parse-long (:distance url/defaults)) (:value %))
                      (:selected %))
                (deep html)))))
  (testing "the word and distance in force, the distance applying itself"
    (let [html (concordance/near-control en {:word "kat" :distance 3})]
      (is (some #(and (map? %) (= "near" (:name %)) (= "kat" (:value %)))
                (deep html)))
      (is (some #(and (map? %) (= 3 (:value %)) (:selected %)) (deep html)))
      (is (some #(and (map? %) (= "distance" (:name %))
                      (= [:apply-view] (get-in % [:on :change])))
                (deep html))))
    (testing "and so does the word, once the reader is done typing it"
      (is (some #(and (map? %) (= "near" (:name %))
                      (= [:apply-view] (get-in % [:on :change])))
                (deep (concordance/near-control en {:word "kat" :distance 3}))))))
  (testing "a distance the list lacks is offered beside them, in order"
    (is (= [1 2 3 4 5 10]
           (keep #(when (number? (:value %)) (:value %))
                 (deep (concordance/near-control en {:word "kat" :distance 4}))))))
  (testing "in Danish"
    (is (some #{"Sammen med"} (deep (concordance/near-control da nil))))
    (is (some #{"1 ord"} (deep (concordance/near-control da nil))))))

(def example-result
  {:size 6 :page 0 :page-size 25 :pages 1
   :counts [{:corpus "PROBE" :size 5}
            {:corpus "VISER" :size 1}
            {:corpus "TALER" :error {:type :cqp :message "no lemma"}}
            {:corpus "GONE" :error {:type :cqp :message "no lemma"}}]
   :hits []})

(deftest concordance-section-test
  (let [state {:ui           en
               :view         :kwic
               :view-hrefs   [[:kwic "/?v=k"]
                              [:frequencies "/?v=f"]]
               :sort-modes   ["corpus" "word"]
               :asked        {:q "hund"}
               :params       {:q "hund"}
               :result       example-result
               :next-href    "/?page=1"
               :export-hrefs {:tsv "/e?format=tsv"}
               :export-limit 5}
        html  (concordance/concordance-section state)]
    (testing "what to do next with the hits follows them, not precedes them"
      (let [order (fn [x] (.indexOf (vec (deep html)) x))]
        (is (< (order :table.kwic) (order :p.downloads)))))
    (testing "the other view of the same hits is offered above them"
      (let [order (fn [x] (.indexOf (vec (deep html)) x))]
        (is (some #{:nav.views.menu} (deep html)))
        (is (< (order :nav.views.menu) (order :table.kwic)))))
    (testing "a cut export is announced"
      (is (some #{" the first 5 hits"} (deep html))))
    (testing "the region names itself and can be landed on"
      (is (= {:id              "results"
              :tabindex        "-1"
              :aria-labelledby "results-heading"}
             (second html))))
    (testing "and is busy while the answer to the next question is coming"
      (is (= "true" (:aria-busy (second (concordance/concordance-section
                                         (assoc state :pending? true)))))))
    (testing "its head holds the heading, the page's own h1, with the rest
              of the question under it as a subheading, and the views"
      (let [[tag [group h1 sub] views] (nth html 2)]
        (is (= :header.result-head tag))
        (is (= :hgroup group))
        (is (= [:h1 {:id "results-heading"}] (subvec h1 0 2)))
        (is (= "6 hits" (text (drop 2 h1))))
        (is (= :p (first sub)))
        (is (= "in 2 corpora" (text sub)))
        (is (= :nav.views.menu (first views)))))
    (testing "errors are headed sections before the concordance"
      (is (some #{[:h2 "CQP error"]} (deep html)))
      (is (some #{:table.kwic} (deep html))))
    (testing "the sort travels with the result, not with the query form"
      (is (some #{"corpus order"} (deep html)))
      (is (some #(and (map? %) (= "sort" (:id %)) (= url/form-id (:form %)))
                (deep html))))
    (testing "and so does the sample, which is a question the reader has
              on seeing how many hits there are"
      (is (some #(and (map? %) (= "sample" (:id %))
                      (= url/form-id (:form %)))
                (deep html))))
    (testing "the page links are rendered above the table as well as below"
      (is (= 2 (count (filter #(and (vector? %) (= :ul.row.pager (first %)))
                              (deep html))))))
    (testing "but only the first is a landmark, since both would share a name"
      (is (= 1 (count (filter #(and (vector? %) (= :nav.pagination.menu (first %)))
                              (deep html))))))
    (testing "the errors come before the concordance"
      (let [order (fn [x] (.indexOf (vec (deep html)) x))]
        (is (< (order [:h2 "CQP error"]) (order :table.kwic)))))
    (testing "the per-corpus counts head the concordance's row groups
              rather than standing in a table of their own"
      (let [html (deep (concordance/concordance-section
                        {:lang   "en"
                         :result (assoc example-result
                                        :hits [{:corpus "PROBE" :cpos 9
                                                :anchors {:matchend 9}
                                                :match   [{:word "hund"}]}])}))]
        (is (some #{[:small.count "(5)"]} html))
        (is (not (some #{:table.counts} html))))))
  (testing "nothing searchable means only the errors, and the heading names one"
    (let [html (concordance/concordance-section
                {:lang   "en"
                 :result {:page   0 :pages 1 :hits []
                          :counts [{:corpus "X" :error {:type :timeout}}]}})]
      (is (not (some #{:table.kwic} (deep html))))
      (is (some #{[:h1 {:id "results-heading"}
                   "The search did not finish in time"]}
                (deep html)))))
  (testing "a search that failed outright is a results region too"
    (let [html (concordance/concordance-section {:lang  "en"
                                                 :error {:type :no-corpus}})]
      (is (some #{[:h1 {:id "results-heading"} "No corpus selected"]}
                (deep html)))
      (is (some #{"Select at least one corpus to search."} (deep html)))))
  (testing "a search that found nothing offers no table and no downloads"
    (let [html (concordance/concordance-section
                {:lang         "en"
                 :result       {:size   0 :page 0 :pages 1 :hits []
                                :counts [{:corpus "PROBE" :size 0}]}
                 :export-hrefs {:tsv "/e?format=tsv"}})]
      (is (some #{"No hits."} (deep html)))
      (is (not (some #{:table.kwic} (deep html))))
      (is (not (some #{"/e?format=tsv"} (deep html))))
      (is (not (some #{"/frequencies?q=x"} (deep html))))))
  (testing "a result still being counted says at least, what is still
            being counted, and where the reader is without a last page"
    (let [html (concordance/concordance-section
                {:ui        en
                 :view      :kwic
                 :asked     {:q "hund"}
                 :params    {:q "hund"}
                 :next-href "/?page=2"
                 :result    (assoc example-result
                                   :pages     nil
                                   :remaining ["X" "Y"])})]
      (is (= "at least 6 hits"
             (text (drop 2 (second (second (nth html 2)))))))
      (is (some #{[:p "Counting hits in 2 corpora …"]} (deep html)))
      (is (some #{[:li "page 1"]} (deep html))))
    (testing "the status line is a live region that stands even when silent"
      (is (some #{[:div.status {:role "status"} nil]}
                (deep (concordance/concordance-section
                       {:ui en :view :kwic :asked {:q "hund"}
                        :params {:q "hund"} :result example-result})))))))

(deftest inspector-test
  (testing "nothing selected, no panel: it describes the cursor or nothing"
    (is (nil? (concordance/inspector en nil))))
  (let [html (concordance/inspector en {:token   {:word "hund" :pos "NCSI"}
                                        :structs {:text_title "Hverdag"}
                                        :corpus  "PROBE"})]
    (testing "it is a named complementary region, not a popover, found by
              the client by its id"
      (is (= :aside.inspector (first html)))
      (is (= concordance/inspector-id (:id (second html))))
      (is (= "Token details" (:aria-label (second html))))
      (is (not (contains? (second html) :popover))))
    (testing "it never takes focus, so the cursor can keep moving"
      (is (not (some #(and (map? %) (contains? % :autofocus)) (deep html)))))
    (testing "the token, its text and its corpus are named groups, boxes
              like the fieldsets, the facts of each a definition list"
      (is (some #{[:h3 "Token"]} (deep html)))
      (is (some #{[:h3 "Text"]} (deep html)))
      (is (some #{[:h3 "Corpus"]} (deep html)))
      (is (= 3 (count (filter #{:section.box} (deep html)))))
      (is (some #{:dl.facts} (deep html)))
      (is (some #{"NCSI"} (deep html)))
      (is (some #{[:cite "Hverdag"]} (deep html))))
    (testing "closing is a plain button, since Escape is handled by the grid"
      (is (some #{[:button.inspector-close {:type "button" :on {:click [:close]}}
                   "Close"]}
                (deep html))))))

(deftest inspector-text-link-test
  (let [selected {:token {:word "hund"} :corpus "PROBE" :cpos 9 :matchend 9}]
    (testing "the panel links to the whole text of the hit it describes"
      (is (some #(and (map? %) (= "/corpora/probe/text?cpos=9#hit" (:href %)))
                (deep (concordance/inspector en selected))))
      (is (some #{"Læs hele teksten"} (deep (concordance/inspector da selected)))))))
