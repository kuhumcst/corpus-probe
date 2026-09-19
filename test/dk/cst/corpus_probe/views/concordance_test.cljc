(ns dk.cst.corpus-probe.views.concordance-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.hiccup :refer [deep]]
            [dk.cst.corpus-probe.test.hiccup :refer [da en text]]
            [dk.cst.corpus-probe.cwb.command :as command]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.concordance :as concordance]))

(defn region
  "The region that scrolls inside the box `concordance` frames it with:
  what these tests read, the box itself being only the frame."
  [& args]
  (second (apply concordance/concordance args)))

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
  (testing "exactly one token is tabbable, so the concordance is one tab
            stop rather than hundreds, and it is the match: the far end of
            the context is out of sight, and the window follows the cursor"
    (is (= [["PROBE" 9] 3]
           (concordance/default-cursor [{:corpus "PROBE" :cpos 9
                                         :left [{} {} {}] :match [{}]
                                         :right [{} {}]}])))
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
        (testing "and inspecting follows focus, not only a press, taking
                  the cursor with it"
          (is (= [:inspect (assoc source :token m) [["PROBE" 9] 0]]
                 (get-in attrs [:on :focus])))
          (is (= [:move-cursor [["PROBE" 9] 0] :event/key :event/ctrl?]
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
        (is (= ["target"] (:class attrs)))
        (is (= "target · NCSI" (:title attrs))))
      (is (= "keyword" (:title (second (concordance/token {:anchored {0 :keyword}}
                                                          hit source 0
                                                          {:word "hund"})))))
      (is (= "keyword" (concordance/anchor-class :keyword))))
    (testing "a token outside the reader's window carries its fade step, the
              classes a collection: Replicant warns on a space separated
              string, once for every token of the page"
      (is (= ["faded" "fade-3"]
             (:class (second (concordance/token {:faded {0 3}}
                                                hit source 0 m))))))))

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

(deftest faded-tokens-test
  ;; indices 0-4 are the left context, 5 the match, 6-10 the right
  (let [hit {:left [{} {} {} {} {}] :match [{}] :right [{} {} {} {} {}]}]
    (testing "the words either side of the match that were asked for stay,
              and the ones the page fetched past them fall away a step at
              a time, out of sight by the last"
      (is (= {0 3 1 2 2 1, 8 1 9 2 10 3}
             (concordance/faded-tokens hit 2 0))))
    (testing "the window travels with the cursor, uncovering the word
              ahead and letting the one behind go"
      (is (= {0 3 1 3 2 2 3 1, 9 1 10 2}
             (concordance/faded-tokens hit 2 1)))
      (is (= {0 1, 6 1 7 2 8 3 9 3 10 3}
             (concordance/faded-tokens hit 2 -2))))
    (testing "the match stays, however far the window has travelled from it"
      (is (nil? (get (concordance/faded-tokens hit 2 40) 5)))
      (is (nil? (get (concordance/faded-tokens hit 2 -40) 5))))
    (testing "the steps stop at the last one, however far out a word is"
      (is (= {0 3 1 3 2 3 3 2 4 1, 6 1 7 2 8 3 9 3 10 3}
             (concordance/faded-tokens hit 0 0))))
    (testing "a width the line does not reach leaves nothing to fade"
      (is (= {} (concordance/faded-tokens hit 5 0)))
      (is (= {} (concordance/faded-tokens hit 40 0))))
    (testing "a unit of text bounds itself, so none of it is faded"
      (is (= {} (concordance/faded-tokens hit :sentence 0))))))

(deftest travel-offset-test
  (let [hits [{:corpus "PROBE" :cpos 9
               :left [{} {}] :match [{}] :right [{} {}]}]]
    (testing "how far the cursor has moved from the match, which every
              row's window follows"
      (is (= 0 (concordance/travel-offset hits [["PROBE" 9] 2])))
      (is (= -2 (concordance/travel-offset hits [["PROBE" 9] 0])))
      (is (= 2 (concordance/travel-offset hits [["PROBE" 9] 4]))))
    (testing "a cursor on no hit here moves nothing"
      (is (= 0 (concordance/travel-offset hits nil)))
      (is (= 0 (concordance/travel-offset hits [["VISER" 1] 0]))))))

(deftest resolved-cursor-test
  (let [hits [{:corpus "PROBE" :cpos 9
               :left [{} {}] :match [{}] :right [{} {}]}]]
    (testing "a cursor naming a token of a hit here is kept"
      (is (= [["PROBE" 9] 4] (concordance/resolved-cursor hits
                                                          [["PROBE" 9] 4]))))
    (testing "one on no hit here, or past the tokens of a page that has
              since narrowed, falls back to the match"
      (is (= [["PROBE" 9] 2] (concordance/resolved-cursor hits nil)))
      (is (= [["PROBE" 9] 2] (concordance/resolved-cursor hits
                                                          [["VISER" 1] 0])))
      (is (= [["PROBE" 9] 2] (concordance/resolved-cursor hits
                                                          [["PROBE" 9] 99]))))
    (testing "and with no hits at all there is no cursor"
      (is (nil? (concordance/resolved-cursor [] [["PROBE" 9] 0]))))))

(deftest anchored-tokens-test
  (let [hit (assoc sample-hit :anchors {:matchend 9 :target 8 :keyword 10})]
    (testing "an anchor is found by its distance from the match"
      (is (= {0 :target 2 :keyword} (concordance/anchored-tokens hit))))
    (testing "an anchor beyond the row, or unset, marks nothing"
      (is (= {} (concordance/anchored-tokens sample-hit)))
      (is (= {} (concordance/anchored-tokens
                 (assoc sample-hit :anchors {:target 20 :keyword nil})))))))

(deftest hit-row-test
  (let [row (concordance/hit-row client sample-hit)]
    (testing "the row carries its corpus position as data"
      (is (= :tr.kwic-hit (first row)))
      (is (= "9" (:data-cpos (second row)))))
    (testing "the position is the row's header and its first cell, and the
              row is the four columns it ends with"
      (is (= :th.kwic-cpos (first (nth row 2))))
      (is (= "row" (:scope (second (nth row 2)))))
      (is (= :td.kwic-left (first (nth row 3))))
      (is (= concordance/column-count (- (count row) 2))))
    (testing "the match is wrapped in a mark element"
      (is (= :td.kwic-match (first (nth row 4))))
      (is (= :mark (get-in row [4 1 0]))))
    (testing "the anchors mark their tokens"
      (let [row (concordance/hit-row client
                                     (assoc sample-hit
                                            :anchors {:matchend 9 :target 8}))]
        (is (some #(and (map? %) (= ["target"] (:class %)))
                  (deep (nth row 3))))))))

(deftest caption-test
  (testing "a result of one page is named by the term alone"
    (is (= [:abbr {:title "key word in context"} "KWIC"]
           (concordance/caption en {:page 0 :pages 1} false))))
  (testing "one of several says which page it holds: the pager stands under
            the table, so a reader who hears it meets the rows first"
    (is (= (list [:abbr {:title "key word in context"} "KWIC"]
                 " · " "page 1 of 2")
           (concordance/caption en {:page 0 :pages 2} true)))
    (is (= "side 2 af 2"
           (last (concordance/caption da {:page 1 :pages 2} true))))))

(deftest concordance-test
  (let [hits   [sample-hit (assoc sample-hit :corpus "VISER" :cpos 3)]
        box    (concordance/concordance hits {:lang    "en"
                                              :caption "Concordance"
                                              :langs   {"VISER" "da"}})
        html   (second box)
        table  (nth html 3)
        groups (nth table 3)]
    (testing "the table stands in a box and scrolls inside a named region
              of its own, focusable but not a tab stop: nothing there is
              scrolled by hand"
      (is (= :div.table-box (first box)))
      (is (= :div.scroll (first html)))
      (is (= {:id              concordance/region-id
              :role            "region"
              :tabindex        "-1"
              :aria-labelledby concordance/caption-id
              :on              {:focusout [:leave-concordance]}}
             (second html))))
    (testing "leaving it closes the panel, which describes what it holds"
      (is (= [:leave-concordance] (get-in html [1 :on :focusout]))))
    (testing "without a script there is no cursor, so nothing is said about
              the keys that move one"
      (is (nil? (nth html 2)))
      (is (not (contains? (second html) :aria-describedby))))
    (testing "the region says how wide a line was asked for, which is how
              wide a page it takes to read it; a unit of text by name"
      (is (not (contains? (second html) :data-context)))
      (is (= 20 (:data-context (second (region hits {:ui en :context 20})))))
      (is (= "sentence" (:data-context (second (region hits
                                                       {:ui en
                                                        :context :sentence}))))))
    (testing "the caption names the region as well as the table, spoken
              but not seen: the view controls name it on screen"
      (is (= :table.kwic (first table)))
      (is (= [:caption.spoken {:id concordance/caption-id} "Concordance"]
             (nth table 1))))
    (testing "every column is headed, so a data cell resolves both headers,
              and no heading is a link: the glossary is linked from the
              prose, not from the machinery"
      (is (= [:thead
              [:tr
               [:th.kwic-cpos {:scope "col"}
                [:abbr {:title "corpus position"} "cpos"]]
               ;; an arrow heads each context, its name spoken: the words
               ;; take more room than a heading here has
               [:th.kwic-left {:scope "col"}
                (list [:span.spoken "left context"]
                      [:span.kwic-arrow {:aria-hidden "true"
                                         :title     "left context"} "←"])]
               [:th.kwic-match {:scope "col"} "match"]
               [:th.kwic-right {:scope "col"}
                (list [:span.spoken "right context"]
                      [:span.kwic-arrow {:aria-hidden "true"
                                         :title     "right context"} "→"])]]]
             (nth table 2))))
    (testing "a heading carries its column's class, so a rule about the
              column reaches the heading too"
      (is (= :th.kwic-right (first (get-in table [2 1 4])))))
    (testing "hits are grouped by corpus, each group headed by its name"
      (is (= 2 (count groups)))
      ;; the count is an explicit nil where the search has none, the
      ;; header keeping its shape whether or not one arrives
      (is (= [:th {:scope "rowgroup" :colspan concordance/column-count}
              [:span.pinned
               [:a {:href "/corpora/probe"} [:code "PROBE"]]
               nil]]
             (get-in (first groups) [2 1])))
      (is (= :tr.kwic-corpus (get-in (first groups) [2 0]))))
    (testing "and by how many hits its corpus holds in all, beside the
              name and styled as the counts in the chooser are; a corpus
              whose query failed has none to show"
      (let [groups (-> (region
                        hits
                        {:ui     en
                         :counts [{:corpus "PROBE" :size 1113}
                                  {:corpus "VISER" :error {:type :cqp}}]})
                       (nth 3)
                       (nth 3))]
        (is (= [" " [:small.note "(1,113)"]]
               (get-in (first groups) [2 1 2 2])))
        (is (nil? (get-in (second groups) [2 1 2 2])))))
    (testing "a group carries its corpus's language when known"
      (is (nil? (:lang (second (first groups)))))
      (is (= "da" (:lang (second (second groups))))))
    (testing "hits without a corpus form one plain group without a header"
      (let [group (first (nth (nth (region
                                    [(dissoc sample-hit :corpus)]
                                    {:ui en})
                                   3)
                              3))]
        (is (= :tbody (first group)))
        (is (nil? (nth group 2)))))))

(deftest position-cell-test
  (let [source (concordance/hit-source
                {:corpus  "PROBE"
                 :cpos    9
                 :anchors {:matchend 10}
                 :structs {:text_title "Hverdag" :text_year "1591"}})]
    (testing "the position heads the row and links to the reading page of
              its text, where the whole of the context is, with the hit
              marked; every annotation names it under the pointer"
      (is (= [:th.kwic-cpos {:scope "row"
                             :title "text_title: Hverdag\ntext_year: 1591"}
              [:a {:href "/corpora/probe/text?cpos=9&matchend=10#hit"} "9"]]
             (concordance/position-cell source))))
    (testing "a hit that knows no corpus has no text page to link to"
      (is (= "9" (last (concordance/position-cell (dissoc source :corpus))))))
    (testing "and one from a corpus that marks nothing gets no empty tooltip"
      (is (not (contains? (second (concordance/position-cell
                                   (dissoc source :structs)))
                          :title))))))

(deftest key-help-test
  (let [html (region [sample-hit] client)]
    (testing "where the script runs, the region says in words what only a
              reader who can see the cursor move would know: it is spoken,
              never seen, and describes the region rather than the token,
              which would say it again at every word"
      (is (= concordance/keys-id (get-in html [1 :aria-describedby])))
      (is (= :p.spoken (first (nth html 2))))
      (is (= concordance/keys-id (:id (second (nth html 2)))))
      (is (str/includes? (last (nth html 2)) "arrow keys")))))

(deftest sort-label-test
  (testing "every sort mode the command namespace offers is named here"
    ;; in Danish, where no label can coincide with the param value
    (is (= ["forekomst" "match" "match bagfra" "ordet før" "ordet efter"
            "tilfældig"]
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
      (is (= :label (first html)))
      (is (= "sort by" (second html)))
      (is (some #(and (map? %) (= "word" (:value %)) (:selected %)) (deep html)))
      (is (some #{"occurrence"} (deep html)))
      (is (some #(and (map? %) (= "sort" (:name %)) (= url/form-id (:form %))
                      (= [:apply-view "sort" :event.target/value]
                         (get-in % [:on :change])))
                (deep html))))))

(deftest sample-control-test
  (let [field (fn [shown]
                (some #(when (and (map? %) (= "sample" (:name %))) %)
                      (deep (concordance/sample-control en shown))))]
    (testing "a number field in its label, blank for the whole result,
              which its placeholder says"
      (is (= :label (first (concordance/sample-control en nil))))
      (is (= ["Sample" "per corpus"]
             (filter string? (concordance/sample-control en nil))))
      (is (= ["Udtræk" "pr. korpus"]
             (filter string? (concordance/sample-control da nil))))
      (is (= "" (:value (field nil))))
      (is (= "all" (:placeholder (field nil))))
      (is (= "alle" (:placeholder
                     (some #(when (and (map? %) (= "sample" (:name %))) %)
                           (deep (concordance/sample-control da nil)))))))
    (testing "holding the size the result has, or the one the form holds"
      (is (= 100 (:value (field 100))))
      (is (= "48" (:value (field "48")))))
    (testing "it submits the query form as the sort control does, once
              the reader is done with it"
      (is (= url/form-id (:form (field 100))))
      (is (= [:apply-view "sample" :event.target/value]
             (get-in (field 100) [:on :change]))))))

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
    (testing "the form holds a width and a unit alike, as strings, and
              each is made into what it means: a string sorted among the
              numbers throws, and a unit that stays one is offered twice"
      (is (= ["5" "10" "20" "sentence" "paragraph"]
             (values (concordance/context-control en "5"))))
      (is (some #(and (map? %) (= 5 (:value %)) (:selected %))
                (deep (concordance/context-control en "5"))))
      (is (= ["5" "10" "20" "sentence" "paragraph"]
             (values (concordance/context-control en "sentence"))))
      (is (some #(and (map? %) (= "sentence" (:value %)) (:selected %))
                (deep (concordance/context-control en "sentence")))))
    (testing "it submits the query form as the sort control does"
      (is (some #(and (map? %) (= "context" (:name %))
                      (= url/form-id (:form %))
                      (= [:apply-view "context" :event.target/value]
                         (get-in % [:on :change])))
                (deep (concordance/context-control en 5)))))
    (testing "in Danish"
      (is (some #{"sætning" "5 ord" "kontekst"}
                (deep (concordance/context-control da 5)))))))

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
    (testing "the other view of the same hits is offered by the page, on
              the query line, not by the section (see views-test)"
      (is (not (some #{:nav.tabs} (deep html)))))
    (testing "a cut export is announced"
      (is (some #{" the first 5 hits"} (deep html))))
    (testing "the region names itself, can be landed on and arrives with
              motion, keyed by the question"
      (is (= {:id                 "results"
              :tabindex           "-1"
              :data-arrival       ""
              :replicant/mounting {:class "arriving"}
              :replicant/key      {:q "hund"}
              :aria-labelledby    "results-heading"}
             (second html))))
    (testing "and is busy while the answer to the next question is coming"
      (is (= "true" (:aria-busy (second (concordance/concordance-section
                                         (assoc state :pending? true)))))))
    (testing "its head holds the answer, the heading and how far the
              search reached, and beside it the controls over that answer"
      (let [[tag [_ h1 reach] controls] (nth (nth html 2) 1)]
        (is (= :header.result-head tag))
        (is (= [:h1 {:id "results-heading"}] (subvec h1 0 2)))
        (is (= "6 hits" (text (drop 2 h1))))
        (is (= :details.caveats (first reach)))
        (is (= "in 2 corpora" (text (nth reach 3))))
        (is (= :div.view-controls (first controls)))
        (is (some #{"sort by"} (deep controls)))))
    (testing "the corpora that failed fold into that reach rather than
              standing as errors over the concordance"
      (is (not (some #{[:h2 "CQP error"]} (deep html))))
      (is (some #{:details.caveats} (deep html)))
      (is (some #{:table.kwic} (deep html))))
    (testing "the sort travels with the result, not with the query form"
      (is (some #{"occurrence"} (deep html)))
      (is (some #(and (map? %) (= "sort" (:id %)) (= url/form-id (:form %)))
                (deep html))))
    (testing "and so does the sample, which is a question the reader has
              on seeing how many hits there are"
      (is (some #(and (map? %) (= "sample" (:id %))
                      (= url/form-id (:form %)))
                (deep html))))
    (testing "the pager stands under the table, where a reader who has read
              it is, and is a landmark, being the only one"
      (is (= 1 (count (filter #(and (vector? %) (= :ul.row.pager (first %)))
                              (deep html)))))
      (is (= 1 (count (filter #(and (vector? %) (= :nav.pagination (first %)))
                              (deep html)))))
      (let [order (fn [x] (.indexOf (vec (deep html)) x))]
        (is (< (order :table.kwic) (order :ul.row.pager)))))
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
        (is (some #{[:small.note "(5)"]} html))
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
      (is (not (some #{"/frequencies?q=x"} (deep html)))))
    (testing "and no controls either, having nothing to decide about; the
              word the hits had to be near stays in the form, where it
              was asked"
      (is (nil? (concordance/concordance-controls
                 {:ui en :result {:size 0 :counts [{:corpus "PROBE" :size 0}]}})))
      (is (nil? (concordance/concordance-controls
                 {:ui     en
                  :result {:size 0 :near {:word "kat"}
                           :counts [{:corpus "PROBE" :size 0}]}})))))
  (testing "a result still being counted gives the hits so far, and where
            the reader is without a last page"
    (let [html (concordance/concordance-section
                {:ui        en
                 :view      :kwic
                 :asked     {:q "hund"}
                 :params    {:q "hund"}
                 :next-href "/?page=2"
                 :result    (assoc example-result
                                   :pages     nil
                                   :remaining ["X" "Y"])})]
      (is (= "6 hits" (text (drop 2 (second (second (nth (nth html 2) 1)))))))
      (is (some #{[:li "page 1"]} (deep html))))))

(defn disclosure
  "The text's disclosure in the card `html`; nil for none."
  [html]
  (first (filter #(and (vector? %) (= :details.inspector-text (first %)))
                 (deep html))))

(deftest inspector-test
  (testing "nothing selected, no panel: it describes the cursor or nothing"
    (is (nil? (concordance/inspector en nil nil false))))
  (let [cursor   [["PROBE" 9] 0]
        selected {:token   {:word "hund" :pos "NCSI"}
                  :structs {:text_title "Hverdag"}
                  :corpus  "PROBE"}
        html     (concordance/inspector en selected cursor false)
        flat     (deep html)]
    (testing "it is a named complementary region, not a popover, found by
              the client by its id"
      (is (= :aside.inspector (first html)))
      (is (= concordance/inspector-id (:id (second html))))
      (is (= "Token details" (:aria-label (second html))))
      (is (not (contains? (second html) :popover))))
    (testing "it never takes focus, so the cursor can keep moving, and the
              cursor's keys reach the cursor from inside it"
      (is (not (some #(and (map? %) (contains? % :autofocus)) flat)))
      (is (= [:key-in-card cursor :event/key :event/ctrl?]
             (get-in (second html) [:on :keydown]))))
    (testing "the client says on render which side of the token it landed"
      (is (= [:mark-side] (:replicant/on-render (second html)))))
    (testing "it arrives under one class, for the stylesheet, and leaves
              under another while motion is on"
      (is (= {:class "arriving"} (:replicant/mounting (second html))))
      (is (nil? (:replicant/unmounting (second html))))
      (is (= {:class "leaving"}
             (:replicant/unmounting
              (second (concordance/inspector en selected cursor false true))))))
    (testing "the word heads the card, and its other attributes follow as
              a definition list of their own, named as the form names them"
      (is (some #{[:h2 "hund"]} flat))
      (is (some #{:dl.facts} flat))
      (is (some #{[:dt "POS"]} flat))
      (is (some #{[:dt "ordklasse"]}
                (deep (concordance/inspector da selected cursor false))))
      (is (some #{[:dd "NCSI"]} flat)))
    (testing "the corpus is the last of them"
      (is (some #{[:dt "Source"]} flat))
      (is (some #{[:a {:href "/corpora/probe"} [:code "PROBE"]]} flat)))
    (testing "the text's attributes are under a disclosure, shut or open as
              the reader left it, which tells the client when it is worked"
      (is (false? (:open (second (disclosure html)))))
      (is (true? (:open (second (disclosure (concordance/inspector
                                             en selected cursor true))))))
      (is (= [:toggle-text :event.target/open]
             (get-in (second (disclosure html)) [:on :toggle])))
      (is (some #{[:summary "More"]} flat))
      (is (some #{[:summary "Less"]}
                (deep (concordance/inspector en selected cursor true))))
      (is (some #{[:cite "Hverdag"]} flat))
      (testing "and a corpus that marks no structure has no disclosure"
        (is (nil? (disclosure (concordance/inspector
                               en (dissoc selected :structs) cursor false))))))
    (testing "closing is a plain button, since Escape is handled by the
              grid: a cross the stylesheet draws, named for the reader
              who hears it"
      (is (some #{[:button.inspector-close {:type       "button"
                                            :aria-label "Close"
                                            :on         {:click [:close]}}]}
                flat)))))

(deftest inspector-placement-test
  (testing "the card stands right after the table, so a card anchored to a
            token follows it in the document and the tab order runs from
            the tokens to the card and on to the pager; only where the
            client runs"
    (let [state {:ui en :view :kwic :asked {:q "hund"} :params {:q "hund"}
                 :result example-result :export-hrefs {:csv "/csv"}
                 :client? true
                 :selected {:token {:word "hund"} :corpus "PROBE"}
                 :cursor [["PROBE" 9] 0]}
          flat  (deep (concordance/concordance-section state))]
      (is (< (.indexOf flat :table.kwic)
             (.indexOf flat :aside.inspector)
             (.indexOf flat :p.downloads)))
      (is (not (some #{:aside.inspector}
                     (deep (concordance/concordance-section
                            (dissoc state :client?)))))))))

(deftest inspector-text-link-test
  (let [selected {:token {:word "hund"} :corpus "PROBE" :cpos 9 :matchend 9}
        cursor   [["PROBE" 9] 0]]
    (testing "the panel names the hit's source corpus, and links to the
              text itself"
      (is (some #(and (map? %) (= "/corpora/probe/text?cpos=9#hit" (:href %)))
                (deep (concordance/inspector en selected cursor false))))
      (is (some #{"see text"}
                (deep (concordance/inspector en selected cursor false))))
      (is (some #{"se tekst"}
                (deep (concordance/inspector da selected cursor false)))))))
