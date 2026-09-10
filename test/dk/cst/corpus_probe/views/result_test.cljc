(ns dk.cst.corpus-probe.views.result-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.hiccup :refer [deep]]
            [dk.cst.corpus-probe.test.hiccup :refer [da en text]]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.concordance :as concordance]
            [dk.cst.corpus-probe.views.result :as result]))

(def example-result
  {:size 6 :page 0 :page-size 25 :pages 1
   :counts [{:corpus "PROBE" :size 5}
            {:corpus "VISER" :size 1}
            {:corpus "TALER" :error {:type :cqp :message "no lemma"}}
            {:corpus "GONE" :error {:type :cqp :message "no lemma"}}]
   :hits []})

(deftest query-phrase-test
  (is (= "hund" (result/query-phrase en {:q "hund"})))
  (is (= "2 words" (result/query-phrase en {:q "hund\n\nkat\n"})))
  (testing "a list counts its words however they are laid out"
    (is (= "3 words"
           (result/query-phrase en {:q "lille hund\nkat"}))))
  (is (= "1 ord" (result/query-phrase da {:q "hund\n"})))
  (testing "CQP is the text as typed"
    (is (= "[] []" (result/query-phrase en {:q "[] []"}))))
  (testing "an extended search is the CQP its tokens compile to"
    (is (= "[lemma = \"hund\"]"
           (result/query-phrase en {:mode "extended" :t1.attr "lemma"
                                    :t1.v "hund"})))))

(deftest hits-phrase-test
  (is (= "1 hit" (result/hits-phrase en 1)))
  (is (= "0 hits" (result/hits-phrase en 0)))
  (testing "Danish, with its own digit grouping"
    (is (= "1 hit" (result/hits-phrase da 1)))
    (is (= "1.000 hits" (result/hits-phrase da 1000)))))

(deftest hits-heading-test
  (testing "the heading is the answer alone, how many: the field above
            holds the query"
    (is (= "6 hits" (result/hits-heading en {:q "hund"} 6)))
    (is (= "6 hits" (result/hits-heading da {:q "hund"} 6))))
  (testing "an extended search is answered like any other"
    (is (= "6 hits"
           (text (result/hits-heading en {:mode    "extended"
                                          :t1.attr "lemma"
                                          :t1.v    "hund"}
                                      6))))
    (testing "and every token when no token asked for anything"
      (is (= "All tokens" (result/hits-heading en {:mode "extended"} 47)))
      (is (result/asked? {:q "hund"}))
      (is (not (result/asked? {:q "hund" :mode "extended"})))))
  (testing "a blank query counts every token, which only a table asks for"
    (is (= "All tokens" (result/hits-heading en {:q ""} 47)))))

(deftest page-phrase-test
  (is (= "page 3 of 6" (result/page-phrase en {:page 2 :pages 6})))
  (is (= "side 3 af 6" (result/page-phrase da {:page 2 :pages 6}))))

(deftest sample-phrase-test
  (testing "no sample, nothing said"
    (is (nil? (result/sample-phrase en nil ["PROBE"]))))
  (testing "the size named is the one asked for, a corpus with fewer
            matches than that contributing all it has"
    (is (= "a random sample of at most 100"
           (result/sample-phrase en 100 ["PROBE"]))))
  (testing "over several corpora it says that each was sampled, one
            sample being drawn in every corpus"
    (is (= "a random sample of at most 100 per corpus"
           (result/sample-phrase en 100 ["PROBE" "VISER"])))
    (is (= "en tilfældig stikprøve på højst 100 pr. korpus"
           (result/sample-phrase da 100 ["PROBE" "VISER"])))))

(deftest position-label-test
  (is (= "before the match" (result/position-label en "match[-1]")))
  (is (= "over hele matchet" (result/position-label da "match..matchend")))
  (testing "a position nothing names is shown as CQP names it"
    (is (= "target" (result/position-label en "target")))))

(deftest subset-inputs-test
  (is (= (list [:input {:type "hidden" :name "subset" :value "kat"}]
               [:input {:type "hidden" :name "subset-at" :value "match[-1]"}]
               [:input {:type "hidden" :name "subset-attr" :value "lemma"}])
         (result/subset-inputs {:anchor "match[-1]" :attr :lemma
                                :value  "kat"})))
  (is (nil? (result/subset-inputs nil))))

(deftest filter-phrase-test
  (is (= "" (result/filter-phrase {})))
  (is (= "text_author ukendt; text_year 1583, 1591"
         (result/filter-phrase {:filter {:text_year   #{"1591" "1583"}
                                         :text_author #{"ukendt"}}})))
  (testing "patterns follow the values, between slashes"
    (is (= "text_title /Hav.*/; text_year 1583, 1591, /16../"
           (result/filter-phrase {:filter   {:text_year #{"1591" "1583"}}
                                  :patterns {:text_title ["Hav.*"]
                                             :text_year  ["16.."]}}))))
  (testing "and a range is the two numbers asked for, last of all: the
            values it stands for are the corpus's own, and there may be
            hundreds of them"
    (is (= "text_year 1900–2024"
           (result/filter-phrase {:ranges {:text_year [1900 2024]}})))
    (is (= "text_year 1583, /16../, 1900–2024"
           (result/filter-phrase {:filter   {:text_year #{"1583"}}
                                  :patterns {:text_year ["16.."]}
                                  :ranges   {:text_year [1900 2024]}})))))

(deftest found-in-phrase-test
  (testing "where the hits are, not where they were looked for: the
            chooser above shows what was searched"
    (is (= "in 2 corpora" (result/found-in-phrase en example-result)))
    (is (= "i 2 korpusser" (result/found-in-phrase da example-result))))
  (testing "one corpus is named rather than counted"
    (is (= "in PROBE"
           (result/found-in-phrase en {:counts [{:corpus "PROBE" :size 5}
                                                {:corpus "VISER" :size 0}]}))))
  (testing "a search that found nothing says nothing, a count of none
            being no news, and a corpus that failed has no size to read"
    (is (nil? (result/found-in-phrase en {:counts [{:corpus "PROBE" :size 0}]})))
    (is (nil? (result/found-in-phrase
               en {:counts [{:corpus "X" :error {:type :cqp}}]})))
    (is (nil? (result/found-in-phrase en nil)))))

(deftest reach-test
  (testing "every corpus answering, the reach is a line and no more"
    (is (= [:p "in PROBE"]
           (result/reach en {:counts [{:corpus "PROBE" :size 5}]}))))
  (testing "a corpus left out folds away under a count of them"
    (let [[tag summary [_ items]] (result/reach en example-result)]
      (is (= :details.caveats tag))
      (is (= "in 2 corpora" (text (drop 2 summary))))
      (testing "labelled in words, the mark that says there is something
                here being the stylesheet's, which reaches nobody
                listening"
        (is (= "in 2 corpora, 2 corpora left out of the search"
               (:aria-label (second summary)))))
      (testing "one item for each way of failing, naming what it took and
                carrying CQP's own words where they are the reason"
        (is (= 1 (count items)))
        (is (= "TALER, GONE: CQP errorno lemma" (text (first items)))))))
  (testing "and it is said even where no corpus found anything to place"
    (is (= :details.caveats
           (first (result/reach en {:counts [{:corpus "PROBE" :size 0}
                                             {:corpus "X"
                                              :error {:type :timeout}}]})))))
  (testing "a search that found nothing anywhere and failed nowhere has
            no reach to report"
    (is (nil? (result/reach en {:counts [{:corpus "PROBE" :size 0}]}))))
  (testing "and one no corpus could run has none either: its errors are
            the answer, headed by the region itself"
    (is (nil? (result/reach en {:counts [{:corpus "X"
                                          :error {:type :timeout}}]})))))

(deftest counting-test
  (testing "a result is being counted while corpora remain"
    (is (result/counting? {:remaining ["X"]}))
    (is (not (result/counting? {:remaining []})))
    (is (not (result/counting? {}))))
  (testing "the heading gives the hits counted so far as a floor"
    (is (= "at least 6 hits" (result/hits-heading en {:q "hund"} 6 true)))
    (is (= "mindst 6 hits" (result/hits-heading da {:q "hund"} 6 true))))
  (testing "the page is placed without a last page"
    (is (= "page 3" (result/page-phrase en {:page 2})))
    (is (= "side 3" (result/page-phrase da {:page 2}))))
  (testing "corpora still being counted count as searched"
    (is (result/searched? {:counts [] :remaining ["X"]}))
    (testing "but not yet as corpora the hits are in: the line grows as
              their counts arrive, beside a heading reading at least"
      (is (= "in A" (result/found-in-phrase
                     en {:counts [{:corpus "A" :size 1}] :remaining ["B" "C"]}))))))

(deftest searched?-test
  (testing "a corpus that answered makes the counts an answer"
    (is (result/searched? {:counts [{:corpus "PROBE" :size 5}]}))
    (testing "including an answer of none"
      (is (result/searched? {:counts [{:corpus "PROBE" :size 0}]}))))
  (testing "a search every corpus refused is not an answer"
    (is (not (result/searched?
              {:counts [{:corpus "X" :error {:type :timeout}}]})))
    (is (not (result/searched? nil)))))

(deftest view-label-test
  (testing "the jargon stands bare, so that the tab's link keeps a name
            of its own, and untranslated in either language"
    (is (= "KWIC" (result/view-label en :kwic)))
    (is (= "KWIC" (result/view-label da :kwic))))
  (is (= "Frekvenser" (result/view-label da :frequencies))))

(deftest view-switch-test
  (let [html (result/view-switch en :kwic [[:kwic "/?v=k"] [:frequencies "/?v=f"]])]
    (testing "a named navigation over a row of links, read as a tab
              strip, the view shown marked and so drawn as the tab"
      (is (= :nav.tabs (first html)))
      (is (= "Result view" (:aria-label (second html))))
      (is (some #{:ul.row} (deep html)))
      (is (some #(and (map? %) (= "/?v=k" (:href %)) (= "page" (:aria-current %)))
                (deep html)))
      (is (some #(and (map? %) (= "/?v=f" (:href %)) (not (:aria-current %)))
                (deep html)))))
  (testing "nothing without hrefs"
    (is (nil? (result/view-switch en :kwic nil)))))

(deftest view-controls-test
  (let [sort* (fn [lang] (concordance/sort-control lang [["word" :sort-word]] "word"))]
    (testing "no controls, nothing rendered"
      (is (nil? (result/view-controls en false nil nil false))))
    (testing "a control that acts on a result submits the form that made it"
      (let [html (result/view-controls en false (sort* "en") nil false)]
        (is (some #(and (map? %) (= url/form-id (:form %))) (deep html)))))
    (testing "without a client, a button is what applies it, one a browser
              with a script never shows"
      (is (some #{"Apply"} (deep (result/view-controls en false (sort* "en")
                                                       nil false))))
      (is (some #(and (vector? %) (= :noscript (first %)))
                (deep (result/view-controls en false (sort* "en") nil false))))
      (is (some #{"Anvend"} (deep (result/view-controls da false
                                                        (sort* "da")
                                                        nil false)))))
    (testing "with one, choosing an order is asking for it: no button"
      (let [html (result/view-controls en true (sort* "en") nil false)]
        (is (not (some #{"Apply"} (deep html))))
        (is (not (some #(and (vector? %) (= :button (first %))) (deep html))))))
    (testing "and the control itself is what applies it"
      (is (some #(and (map? %) (= [:apply-view] (get-in % [:on :change])))
                (deep (sort* "en")))))
    (testing "what narrows a result sits behind a disclosure, closed until
              a narrowing is in force, open while one is"
      (let [narrowing (concordance/sample-control en nil)
            closed    (result/view-controls en true (sort* "en") narrowing false)
            open      (result/view-controls en true (sort* "en") narrowing true)
            details   (fn [html] (some #(when (and (vector? %)
                                                   (= :details (first %)))
                                          %)
                                       (deep html)))]
        (is (= :div.view-controls (first closed)))
        (is (false? (:open (second (details closed)))))
        (is (true? (:open (second (details open)))))
        (is (some #{"Narrow the result"} (deep closed)))
        (is (some #{"Afgræns resultatet"}
                  (deep (result/view-controls da true (sort* "da")
                                              narrowing false))))
        (testing "each row gets its own button without a client"
          (is (= 2 (count (filter #{"Apply"}
                                  (deep (result/view-controls en false
                                                              (sort* "en")
                                                              narrowing
                                                              false)))))))
        (testing "and a narrowing alone, with nothing to read differently,
                  is the disclosure alone"
          (let [html (result/view-controls en true nil narrowing true)]
            (is (nil? (second html)))
            (is (details html))))))))

(deftest pager-links-test
  (testing "no links renders nothing"
    (is (nil? (result/pager-links en nil nil "page 1 of 1"))))
  (testing "links carry the rel values browsers use for a sequence"
    (let [html (result/pager-links en "/?page=0" "/?page=2" "page 2 of 3")]
      (is (some #{"prev"} (deep html)))
      (is (some #{"next"} (deep html)))
      (is (some #{"next →"} (deep html)))))
  (testing "the position rides between the two directions"
    (let [html (result/pager-links en "/?page=0" "/?page=2" "page 2 of 3")]
      (is (= [:li "page 2 of 3"]
             (second (filter #(and (vector? %)
                                   (#{:li :li.pager-prev :li.pager-next}
                                    (first %)))
                             (deep html)))))))
  (testing "a direction that is out of range is left out, not held open"
    (let [html (result/pager-links en nil "/?page=1" "page 1 of 3")]
      (is (= 2 (count (filter #(and (vector? %)
                                    (#{:li :li.pager-prev :li.pager-next}
                                     (first %)))
                              (deep html)))))
      (is (not (some #{"prev"} (deep html))))))
  (testing "in Danish"
    (let [html (result/pager-links da "/?page=0" "/?page=2" "side 2 af 3")]
      (is (some #{"← forrige"} (deep html)))
      (is (some #{"næste →"} (deep html))))))

(deftest pagination-test
  (testing "nothing to page is no landmark at all"
    (is (nil? (result/pagination en nil nil "page 1 of 1"))))
  (testing "the links are wrapped in a navigation landmark, named, chrome
            rather than text"
    (let [html (result/pagination en "/?page=0" "/?page=2" "page 2 of 3")]
      (is (= :nav.pagination.menu (first html)))
      (is (= "Pagination" (:aria-label (second html))))
      (is (= "Sidenavigation"
             (:aria-label (second (result/pagination da "/?page=0" nil "x")))))
      (is (some #{:ul.row.pager} (deep html))))))

(deftest download-links-test
  (is (nil? (result/download-links en nil nil)))
  (let [html (result/download-links en
                                    {:tsv "/x?format=tsv" :csv "/x?format=csv"}
                                    "the first 10 hits")]
    (testing "one download link per format, in a fixed order"
      (is (= [[:a {:href "/x?format=csv"} "CSV"]
              [:a {:href "/x?format=tsv"} "TSV"]]
             (filter #(and (vector? %) (= :a (first %))) (deep html)))))
    (testing "the note qualifies the download"
      (is (some #{" the first 10 hits"} (deep html))))))

(deftest error-groups-test
  (testing "identical errors are reported once, naming every corpus"
    (is (= [[{:type :cqp :message "no lemma"} ["TALER" "GONE"]]]
           (result/error-groups (:counts example-result)))))
  (is (empty? (result/error-groups [{:corpus "PROBE" :size 1}]))))

(deftest error-heading-test
  (is (= "CQP error" (result/error-heading en {:type :cqp})))
  (is (= "The search did not finish in time" (result/error-heading en {:type :timeout})))
  (is (= "Søgningen tog for lang tid"
         (result/error-heading da {:type :timeout}))))

(deftest rejection-explanation-test
  (testing "a guard's reason and attribute, agreeing with the corpora"
    (is (= "The corpus cannot be counted by lemma."
           (result/rejection-explanation
            en {:type :rejected :reason :not-groupable :attr :lemma} 1)))
    (is (= "The corpora cannot be counted by lemma."
           (result/rejection-explanation
            en {:type :rejected :reason :not-groupable :attr :lemma} 2)))
    (is (= "Korpusset kan ikke tælles efter lemma."
           (result/rejection-explanation
            da {:type :rejected :reason :not-groupable :attr :lemma} 1))))
  (testing "the one guard naming several attributes lists them"
    (is (= "The corpus has no metadata field of that name: text_year, text_x."
           (result/rejection-explanation
            en {:type :rejected :reason :no-filter-attr
                :attrs [:text_year :text_x]} 1))))
  (testing "a guard with no reason says nothing: its message is ours"
    (is (nil? (result/rejection-explanation
               en {:type :rejected :message "Not a groupable attribute"} 1)))
    (is (nil? (result/rejection-explanation
               en {:type :rejected :reason :not-groupable} 1)))))

(deftest rejected-attrs-test
  (is (= ["lemma"] (result/rejected-attrs {:attr :lemma})))
  (is (= ["a" "b"] (result/rejected-attrs {:attrs [:a :b]})))
  (is (empty? (result/rejected-attrs {}))))

(deftest rejected-body-test
  (testing "our own guard is worded, not boxed as another program's output"
    (let [body (result/error-body da {:type    :rejected
                                      :reason  :not-groupable
                                      :attr    :lemma
                                      :message "Not a groupable attribute"}
                                  ["TALER"])]
      (is (some #{"Korpusset kan ikke tælles efter lemma."} (deep body)))
      (is (not-any? #(and (vector? %) (= :pre (first %))) (deep body)))))
  (testing "CQP's own words keep their box"
    (is (some #(and (vector? %) (= :pre (first %)))
              (deep (result/error-body en {:type    :cqp
                                           :message "CQP Error:"}
                                       ["PROBE"]))))))

(deftest near-control-test
  (testing "no word in force: an empty field and the default distance"
    (let [html (result/near-control en nil)]
      (is (some #(and (map? %) (= "near" (:name %)) (= "" (:value %))
                      (= url/form-id (:form %)))
                (deep html)))
      (is (some #(and (map? %) (= url/default-distance (:value %))
                      (:selected %))
                (deep html)))))
  (testing "the word and distance in force, the distance applying itself"
    (let [html (result/near-control en {:word "kat" :distance 3})]
      (is (some #(and (map? %) (= "near" (:name %)) (= "kat" (:value %)))
                (deep html)))
      (is (some #(and (map? %) (= 3 (:value %)) (:selected %)) (deep html)))
      (is (some #(and (map? %) (= "distance" (:name %))
                      (= [:apply-view] (get-in % [:on :change])))
                (deep html))))
    (testing "and so does the word, once the reader is done typing it"
      (is (some #(and (map? %) (= "near" (:name %))
                      (= [:apply-view] (get-in % [:on :change])))
                (deep (result/near-control en {:word "kat" :distance 3}))))))
  (testing "a distance the list lacks is offered beside them, in order"
    (is (= [1 2 3 4 5 10]
           (keep #(when (number? (:value %)) (:value %))
                 (deep (result/near-control en {:word "kat" :distance 4}))))))
  (testing "in Danish"
    (is (some #{"Sammen med"} (deep (result/near-control da nil))))
    (is (some #{"1 ord"} (deep (result/near-control da nil))))))

(deftest bare-word-error-test
  (testing "a bare word in a CQP query is refused as a corpus, which the
            reader is told in their own terms, above CQP's own words"
    (let [message "CQP Error:\n\tCorpus ``hund'' is undefined"
          body    (result/error-body en {:type :cqp :message message} ["PROBE"])]
      (is (result/bare-word-error? message))
      (is (some #{(str "CQP reads a bare word as the name of a query result. "
                       "To match a word, put it in quotation marks.")}
                (deep body)))
      (is (some #{[:samp message]} (deep body))))
    (is (not (result/bare-word-error? "CQP Error:\n\tSyntax error")))
    (is (not (some #(and (string? %) (str/starts-with? % "This is not"))
                   (deep (result/error-body en {:type :cqp :message "x"} [])))))))

(deftest cqp-error-section-test
  (let [html (result/cqp-error-section en {:type :cqp :message "boom"} ["TALER"])]
    (testing "no live region: it is in the document before the page is parsed"
      (is (not (some #{"alert"} (deep html)))))
    (testing "it heads itself below the region's own h1"
      (is (= :section.error (first html)))
      (is (some #{[:h2 "CQP error"]} (deep html))))
    (testing "cqp's message is the sample output of another program"
      (is (some #{[:samp "boom"]} (deep html))))
    (is (some #{"boom"} (deep html)))
    (testing "an error CQP itself reported is headed as such"
      (is (some #{"CQP error"} (deep html))))
    (testing "the corpora concerned are named"
      (is (some #{[:code "TALER"]} (deep html)))))
  (testing "no corpus selected is explained without a CQP message"
    (let [html (result/cqp-error-section en {:type :no-corpus} nil)]
      (is (some #{"No corpus selected"} (deep html)))
      (is (some #{"Select at least one corpus to search."} (deep html)))
      (is (not (some #{:pre} (deep html))))))
  (testing "our own rejections and internal failures are not CQP errors"
    (is (some #{"Left out of the search"}
              (deep (result/cqp-error-section en {:type    :rejected
                                                  :message "x"} nil))))
    (is (some #{"Unexpected error"}
              (deep (result/cqp-error-section en {:type :internal} nil))))
    (is (some #{"Unknown corpus"}
              (deep (result/cqp-error-section en {:type :unknown-corpus} ["X"])))))
  (testing "the headings and explanations are translated, CQP's message not"
    (let [html (result/cqp-error-section da {:type :no-corpus} nil)]
      (is (some #{"Intet korpus valgt"} (deep html)))
      (is (some #{"Vælg mindst ét korpus at søge i."} (deep html))))
    (let [html (result/cqp-error-section da {:type :cqp :message "boom"} ["X"])]
      (is (some #{"CQP-fejl"} (deep html)))
      (is (some #{"boom"} (deep html))))))

(deftest results-region-test
  (let [state {:ui         en
               :view       :kwic
               :view-hrefs [[:kwic "/?v=k"] [:frequencies "/?v=f"]]
               :asked      {:q "hund"}
               :params     {:q "hund"}
               :result     example-result}
        html  (result/results-region state "6 hits" [:div.view-controls]
                                     [:p "body"])]
    (testing "the region names itself and can be landed on"
      (is (= :section.result (first html)))
      (is (= {:id              "results"
              :tabindex        "-1"
              :aria-labelledby "results-heading"}
             (second html))))
    (testing "and is busy while the answer to the next question is coming"
      (is (= "true" (:aria-busy (second (result/results-region
                                         (assoc state :pending? true)
                                         "6 hits" nil nil))))))
    (testing "its head holds the answer, the heading and how far the
              search reached, and beside it the controls over that answer"
      (let [[tag answer controls] (nth html 2)
            [_ h1 reach] answer]
        (is (= :header.result-head tag))
        (is (= :div.answer (first answer)))
        (is (= [:h1 {:id "results-heading"} "6 hits"] h1))
        (is (= "in 2 corpora" (text (second reach))))
        (is (= [:div.view-controls] controls))))
    (testing "the switch between the views is not one of its parts: it
              stands on the query line (see views-test)"
      (is (not (some #{:nav.tabs} (deep html)))))
    (testing "the status line is a live region that stands even when
              silent, before anything whose kind can change"
      (is (= [:div.status {:role "status"} nil] (nth html 3))))
    (testing "the corpora that failed are folded into that reach, not
              headed as errors of their own"
      (is (not (some #{[:h2 "CQP error"]} (deep html))))
      (is (some #{:details.caveats} (deep html)))
      (is (= [:p "body"] (last html))))
    (testing "the heading says at least, and a status line says what is
              still being counted"
      (let [counting (assoc example-result :pages nil :remaining ["X" "Y"])]
        (is (= "at least 6 hits"
               (result/result-heading en {:q "hund"} counting nil)))
        (is (some #{[:p "Counting hits in 2 corpora …"]}
                  (deep (result/results-region (assoc state :result counting)
                                               "x" nil nil))))))
    (testing "a search that failed outright is headed by its error"
      (is (= "No corpus selected"
             (result/result-heading en {:q "hund"} nil {:type :no-corpus})))
      (is (some #{"Select at least one corpus to search."}
                (deep (result/results-region {:ui en :error {:type :no-corpus}}
                                             "x" nil nil)))))))
