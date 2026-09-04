(ns dk.cst.corpus-probe.views.kwic-test
  (:require [dk.cst.corpus-probe.views.hiccup :refer [da en]]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.views.kwic :as kwic]))

(deftest token-title-test
  (testing "non-word attributes join into the tooltip"
    (is (= "NCSI · hund" (kwic/token-title {:word "hund" :pos "NCSI"
                                            :lemma "hund"}))))
  (testing "structure tags and blank values are excluded"
    (is (= "NCSI" (kwic/token-title {:word "hund" :pos "NCSI" :lemma ""
                                     :open [:s]})))))

(deftest token-data-test
  (testing "every annotation but the surface word becomes a data-* attribute"
    (is (= {:data-pos "NCSI" :data-lemma "hund"}
           (kwic/token-data {:word "hund" :pos "NCSI" :lemma "hund"}))))
  (testing "structure tags are not emitted as data"
    (is (= {:data-pos "NCSI"}
           (kwic/token-data {:word "x" :pos "NCSI" :open [:s]})))))

(deftest token-count-test
  (is (= 3 (kwic/token-count {:left [{}] :match [{}] :right [{}]})))
  (is (= 1 (kwic/token-count {:match [{}]}))))

(deftest default-cursor-test
  (testing "exactly one token is tabbable, the first, so the concordance is
            one tab stop rather than hundreds"
    (is (= [["PROBE" 9] 0] (kwic/default-cursor [{:corpus "PROBE" :cpos 9}])))
    (is (nil? (kwic/default-cursor [])))))

(deftest token-test
  (let [m         {:word "hund" :pos "NCSI"}
        hit       {:corpus "PROBE" :cpos 9}
        source    {:corpus "PROBE" :structs {:text_title "Hverdag"}}
        [tag attrs] (kwic/token {} hit source 0 m)]
    (testing "the surface form is the text content, annotations are data"
      (is (= "hund" (last (kwic/token {} hit source 0 m))))
      (is (= "NCSI" (:data-pos attrs))))
    (testing "without a client nothing answers a click, so it is not a control"
      (is (= :span.token tag))
      (testing "and it carries no handler no renderer would read"
        (is (not (contains? attrs :on)))))
    (testing "with one it is a button the cursor can rest on"
      (let [[tag attrs] (kwic/token {:client? true :cursor [["PROBE" 9] 0]}
                                    hit source 0 m)]
        (is (= :button.token tag))
        (is (= "button" (:type attrs)))
        (is (= "t-PROBE-9-0" (:id attrs)))
        (is (= "0" (:tabindex attrs)))
        (testing "and inspecting follows focus, not only a press"
          (is (= [:inspect (assoc source :token m)]
                 (get-in attrs [:on :focus])))
          (is (= [:move-cursor [["PROBE" 9] 0]]
                 (get-in attrs [:on :keydown]))))))
    (testing "every other token is out of the tab order"
      (is (= "-1" (:tabindex (second (kwic/token
                                      {:client? true :cursor [["PROBE" 9] 0]}
                                      hit source 1 m))))))
    (testing "a corpus that annotates nothing gets no empty tooltip"
      (is (not (contains? (second (kwic/token {} hit source 0 {:word "hund"}))
                          :title))))))

(deftest source-label-test
  (testing "a text title is a cited work"
    (is (= [:cite "Hverdag"]
           (kwic/source-label {:text_id "t1" :text_title "Hverdag"}))))
  (testing "falls back to text_id, then any value"
    (is (= "t1" (kwic/source-label {:text_id "t1"})))
    (is (= "x" (kwic/source-label {:other "x"})))))

(deftest position-data-test
  (is (= {:data-cpos "9" :data-matchend "10"}
         (kwic/position-data 9 {:matchend 10 :target nil :keyword nil})))
  (testing "target and keyword anchors appear only when set"
    (is (= {:data-cpos "9" :data-matchend "9" :data-target "9"}
           (kwic/position-data 9 {:matchend 9 :target 9 :keyword nil})))))

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

(deftest hit-row-test
  (let [row (kwic/hit-row client sample-hit false)]
    (testing "the row carries its corpus position as data"
      (is (= "9" (:data-cpos (second row)))))
    (testing "the position is the row's header and its first cell"
      (is (= :th.cpos (first (nth row 2))))
      (is (= "row" (:scope (second (nth row 2)))))
      (is (= :td.structs (first (nth row 3)))))
    (testing "the position cell is a disclosure button showing the cpos"
      (let [button (nth (nth row 2) 2)]
        (is (= :button (first button)))
        (is (= "9" (last button)))
        (is (= "false" (:aria-expanded (second button))))
        (is (= [:toggle-context {:corpus "PROBE" :cpos 9 :matchend 9}]
               (get-in button [1 :on :click])))))
    (testing "without a client it is the bare position, not a dead control"
      (is (= "9" (nth (nth (kwic/hit-row {:ui en} sample-hit false) 2) 2))))
    (testing "aria-expanded tracks the flag, and names the row it revealed"
      (let [button (nth (nth (kwic/hit-row client sample-hit true) 2) 2)]
        (is (= "true" (:aria-expanded (second button))))
        (is (= "context-PROBE-9" (:aria-controls (second button)))))
      (testing "and names nothing while there is no such row"
        (is (not (contains? (second (nth (nth row 2) 2)) :aria-controls)))))
    (testing "the control opens its name with the position a reader can see"
      (is (= "9 · Toggle wider context"
             (get-in (nth (nth row 2) 2) [1 :aria-label])))
      (is (= "9 · Vis eller skjul bredere kontekst"
             (get-in (nth (nth (kwic/hit-row {:ui da :client? true}
                                             sample-hit false)
                               2) 2)
                     [1 :aria-label]))))
    (testing "the match is wrapped in a mark element"
      (is (= :mark (get-in row [5 1 0]))))))

(deftest hit-rows-test
  (testing "a hit is always two children, the second nil without an expansion"
    (let [rows (kwic/hit-rows {:ui en} sample-hit)]
      (is (= 2 (count rows)))
      (is (nil? (second rows)))))
  (testing "a failed fetch says so, in a live region: it had no page load"
    (let [rows (kwic/hit-rows {:ui en :expanded {["PROBE" 9] kwic/failed}}
                              sample-hit)]
      (is (= 2 (count rows)))
      (is (= [:span {:role "alert"} "Could not load the context."]
             (get-in (second rows) [1 2])))))
  (testing "an expanded hit adds a full-width context row after it"
    (let [ex   {:left  [{:word "en"}]
                :match [{:word "hund"}]
                :right [{:word "i"}]}
          rows (kwic/hit-rows {:ui en :client? true
                               :expanded {["PROBE" 9] ex}}
                              sample-hit)]
      (is (= 2 (count rows)))
      (is (= :tr.expanded (first (second rows))))
      (is (= kwic/column-count (get-in (second rows) [2 1 :colspan])))
      (testing "its tokens are inspected with the hit's corpus and structs"
        (is (some #(and (map? %) (= "PROBE" (:corpus %))
                        (= {:text_title "Hverdag"} (:structs %)))
                  (tree-seq coll? seq (second rows)))))))
  (testing "the same position in another corpus is not expanded"
    (is (nil? (second (kwic/hit-rows {:lang     "en"
                                      :expanded {["VISER" 9] {}}}
                                     sample-hit)))))
  (testing "an expanded row numbers its tokens past the row it expands, so
            no two elements of one hit share an id or the cursor"
    (let [ex   {:left [{:word "en"}] :match [{:word "hund"}]
                :right [{:word "i"}]}
          rows (kwic/hit-rows {:lang     "en" :client? true
                               :cursor   [["PROBE" 9] 0]
                               :expanded {["PROBE" 9] ex}}
                              sample-hit)
          ids  (keep #(when (map? %) (:id %)) (tree-seq coll? seq rows))
          cur  (filter #(and (map? %) (= "0" (:tabindex %)))
                       (tree-seq coll? seq rows))]
      (is (= (count ids) (count (distinct ids))))
      (is (= 1 (count cur)))
      (is (= ["t-PROBE-9-0" "t-PROBE-9-1" "t-PROBE-9-2"
              "t-PROBE-9-3" "t-PROBE-9-4" "t-PROBE-9-5"]
             (filter #(re-matches #"t-.*" %) ids)))))
  (testing "a pending placeholder shows a loading row, also a live region"
    (let [rows (kwic/hit-rows {:ui en :expanded {["PROBE" 9] :loading}}
                              sample-hit)]
      (is (= 2 (count rows)))
      (is (= [:span {:role "status"} "Loading …"]
             (get-in (second rows) [1 2])))))
  (testing "the context row is named by the disclosure that revealed it"
    (let [rows (kwic/hit-rows {:lang     "en"
                               :expanded {["PROBE" 9] {:left [] :match []
                                                       :right []}}}
                              sample-hit)]
      (is (= "context-PROBE-9" (:id (second (second rows))))))))

(deftest concordance-test
  (let [hits   [sample-hit (assoc sample-hit :corpus "VISER" :cpos 3)]
        html   (kwic/concordance hits {:lang    "en"
                                       :caption "Concordance"
                                       :langs   {"VISER" "da"}})
        table  (nth html 2)
        groups (nth table 3)]
    (testing "the table scrolls inside a named, focusable region of its own"
      (is (= :div.scroll (first html)))
      (is (= {:id              kwic/region-id
              :role            "region"
              :tabindex        "0"
              :aria-labelledby kwic/caption-id
              :on              {:focusout [:leave-concordance]}}
             (second html))))
    (testing "leaving it closes the panel, which describes what it holds"
      (is (= [:leave-concordance] (get-in html [1 :on :focusout]))))
    (testing "the caption names the region as well as the table"
      (is (= kwic/caption-id (:id (second (nth table 1))))))
    (testing "every column is headed, so a data cell resolves both headers"
      (is (= [:thead
              [:tr
               [:th {:scope "col"} "position"]
               [:th {:scope "col"} "source"]
               [:th {:scope "col"} "left context"]
               [:th {:scope "col"} "match"]
               [:th {:scope "col"} "right context"]]]
             (nth table 2))))
    (testing "hits are grouped by corpus, each group headed by its name"
      (is (= 2 (count groups)))
      (is (= [:th {:scope "rowgroup" :colspan 5}
              [:a {:href "/corpus/probe"} [:code "PROBE"]]]
             (get-in (first groups) [2 1]))))
    (testing "a group carries its corpus's language when known"
      (is (nil? (:lang (second (first groups)))))
      (is (= "da" (:lang (second (second groups))))))
    (testing "hits without a corpus form one plain group without a header"
      (let [group (first (nth (nth (kwic/concordance
                                    [(dissoc sample-hit :corpus)]
                                    {:ui en})
                                   2)
                              3))]
        (is (= :tbody (first group)))
        (is (nil? (nth group 2)))))))
