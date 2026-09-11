(ns dk.cst.corpus-probe.views.widgets-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.hiccup :refer [deep]]
            [dk.cst.corpus-probe.test.hiccup :refer [da en]]
            [dk.cst.corpus-probe.views.widgets :as widgets]))

(deftest option-test
  (is (= [:option {:value "a" :selected true} "A"] (widgets/option "a" "a" "A")))
  (is (= [:option {:value "b" :selected false} "B"] (widgets/option "a" "b" "B"))))

(deftest select-test
  (let [html (widgets/select "search-form" "sort" "Sort"
                             [(widgets/option "word" "word" "match")])]
    (testing "a label for the control, then the control, named for the
              form it submits with and applying itself on change"
      (is (some #{[:label {:for "sort"} "Sort"]} (deep html)))
      (is (some #(and (map? %) (= "sort" (:id %)) (= "sort" (:name %))
                      (= "search-form" (:form %))
                      (= [:apply-view] (get-in % [:on :change])))
                (deep html))))))

(deftest status-test
  (testing "a live region, rendered whether or not it has anything to say"
    (is (= [:div.status {:role "status"} nil] (widgets/status nil)))
    (is (= [:div.status {:role "status"} [:p "x"]] (widgets/status [:p "x"]))))
  (testing "placed by a class of its own where a layout wants it"
    (is (= [:div.status {:class "navigation-status" :role "status"} nil]
           (widgets/status "navigation-status" nil)))))

(deftest link-row-test
  (let [html (widgets/link-row [[:a "/a" "A"] [:b "/b" "B"]] :b)]
    (testing "a row of links, each carrying its key, the current one
              marked"
      (is (= :ul.row (first html)))
      (is (= [[:li [:a {:href "/a" :data-key "a"} "A"]]
              [:li [:a {:href "/b" :data-key "b" :aria-current "page"} "B"]]]
             (second html))))
    (testing "and nothing marked when nothing is current"
      (is (not (some #(and (map? %) (:aria-current %))
                     (deep (widgets/link-row [[:a "/a" "A"]] nil))))))))

(deftest attribute-value-test
  (testing "a title is a cited work"
    (is (= [:cite "Hverdag"] (widgets/attribute-value :text_title "Hverdag"))))
  (testing "a four-digit year is a time"
    (is (= [:time "2023"] (widgets/attribute-value :text_year "2023"))))
  (testing "a non-year value under a _year key stays plain"
    (is (= "n/a" (widgets/attribute-value :text_year "n/a"))))
  (testing "anything else is as it is"
    (is (= "NCSI" (widgets/attribute-value :pos "NCSI")))
    (is (= [:data "x"] (widgets/attribute-value :size [:data "x"])))))

(deftest facts-test
  (let [html (widgets/facts {:text_title "Hverdag" :text_year "2023"})]
    (testing "a definition list of the pairs, the values rendered
              semantically"
      (is (= :dl.facts (first html)))
      (is (some #{[:dt "text_title"]} (deep html)))
      (is (some #{[:dd [:cite "Hverdag"]]} (deep html)))
      (is (some #{[:dd [:time "2023"]]} (deep html)))))
  (testing "a string key names itself"
    (is (some #{[:dt "size"]} (deep (widgets/facts [["size" "47"]]))))))

(deftest note-test
  (testing "a sign the layout has no room to explain: bare, said in words
            under the pointer, or linked to the page that says it at
            length, where the words name the link"
    (is (= [:small.note "?"] (widgets/note "?")))
    (is (= [:small.note {:title "a regular expression"} "?"]
           (widgets/note "?" "a regular expression")))
    (is (= [:small.note [:a.help-link {:href       "/glossary#regex"
                                       :title      "a regular expression"
                                       :aria-label "a regular expression"}
                          "?"]]
           (widgets/help "a regular expression" "regex")))))

(deftest count-badge-test
  (is (= [:small.note "(5)"] (widgets/count-badge 5)))
  (is (= [:small.note "(2/5)"] (widgets/count-badge 2 5)))
  (testing "with what the figures count in the title, and a mark where
            they are not the whole story"
    (is (= [:small.note {:title "2 of 5 chosen"} "(2/5)"]
           (widgets/count-badge 2 5 "2 of 5 chosen")))
    (is (= [:small.note {:title "2 of 5 chosen"} "(2/5)"
            [:span {:aria-hidden "true"} "*"]]
           (widgets/count-badge 2 5 "2 of 5 chosen" "*")))))

(deftest count-cell-test
  (is (= [:td.num "1,000"] (widgets/count-cell en 1000)))
  (is (= [:td.num "1.000"] (widgets/count-cell da 1000)))
  (testing "what more the cell says of the count follows it"
    (is (= [:td.num "3" " (1.0)"] (widgets/count-cell en 3 " (1.0)")))
    (is (= [:td.num "3" nil] (widgets/count-cell en 3 nil)))))

(deftest size-data-test
  (testing "the digits are grouped as the language groups them"
    (is (= [:data.size {:value "64600000"} "64,600,000 tokens"]
           (widgets/size-data en 64600000)))
    (is (= [:data.size {:value "64600000"} "64.600.000 tokens"]
           (widgets/size-data da 64600000)))))

(deftest select-all-test
  (testing "nothing to take is no control"
    (is (nil? (widgets/select-all "All" [] #{} [:take]))))
  (let [attrs (fn [items chosen & [opts]]
                (second (widgets/select-all "All" items chosen [:take] opts)))]
    (testing "checked when every item is chosen, partly when some are, the
              states no attribute carries set on every render"
      (is (true? (:checked (attrs ["a" "b"] #{"a" "b"}))))
      (is (false? (:checked (attrs ["a" "b"] #{"a"}))))
      (is (= [:set-checkbox-state {:indeterminate true :invalid nil}]
             (:replicant/on-render (attrs ["a" "b"] #{"a"}))))
      (is (= [:set-checkbox-state {:indeterminate false :invalid "Pick one"}]
             (:replicant/on-render (attrs ["a"] #{} {:invalid "Pick one"})))))
    (testing "named for a screen reader, dispatching the action given"
      (is (= "All" (:aria-label (attrs ["a"] #{}))))
      (is (= [:take] (get-in (attrs ["a"] #{}) [:on :change]))))
    (testing "a clear-only control is disabled while nothing is chosen,
              as a boolean rather than a string"
      (is (true? (:disabled (attrs ["a"] #{} {:clear-only? true}))))
      (is (false? (:disabled (attrs ["a"] #{"a"} {:clear-only? true}))))
      (is (false? (:disabled (attrs ["a"] #{})))))))

(deftest error-section-test
  (is (= [:section.error [:h2 "Oops"] [:p "why"]]
         (widgets/error-section "Oops" [:p "why"]))))

(deftest term-test
  (testing "an abbreviation carries its expansion and links to its entry"
    (is (= [:a {:href "/glossary#kwic"}
            [:abbr {:title "key word in context"} "KWIC"]]
           (widgets/term en :kwic)))
    (is (= [:a {:href "/glossary#cpos"} [:abbr {:title "korpusposition"} "cpos"]]
           (widgets/term da :cpos))))
  (testing "a word that is no abbreviation is the word, linked"
    (is (= [:a {:href "/glossary#metadata"} "Metadata"] (widgets/term en :metadata)))
    (is (= [:a {:href "/glossary#per-million"} "pr. million"]
           (widgets/term da :per-million))))
  (testing "where a link cannot go, the term stands unlinked"
    (is (= [:abbr {:title "Corpus Query Processor"} "CQP"]
           (widgets/term en :cqp false)))
    (is (= "Metadata" (widgets/term en :metadata false)))))

(deftest attrs-test
  (testing "the landmark every page shares"
    (is (= {:id "main" :tabindex "-1"} widgets/main-attrs))
    (is (= "main" widgets/main-id)))
  (testing "a language marks an element only when known"
    (is (= {:lang "da"} (widgets/lang-attrs "da")))
    (is (= {} (widgets/lang-attrs nil))))
  (testing "hidden keeps the element in the document"
    (is (= {:hidden true} (widgets/hidden-attrs true)))
    (is (= {} (widgets/hidden-attrs false)))))
