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
    (is (= "1 forekomst" (result/hits-phrase da 1)))
    (is (= "1.000 forekomster" (result/hits-phrase da 1000)))))

(deftest hits-heading-test
  (testing "the heading is the answer alone, how many: the field above
            holds the query"
    (is (= "6 hits" (result/hits-heading en {:q "hund"} 6)))
    (is (= "6 forekomster" (result/hits-heading da {:q "hund"} 6))))
  (testing "which the line under it names only once the form has moved
            on from it"
    (let [asked {:q "hund" :mode "simple"}]
      (is (nil? (result/question en {:asked asked :params asked})))
      (is (nil? (result/question en {:asked  asked
                                     :params {:q "hund " :mode "simple"}})))
      (is (= [:q "hund"]
             (result/question en {:asked  asked
                                  :params {:q "hunde" :mode "simple"}})))
      (testing "a switch that kept the query whole is no move, one that
                dropped part of it is"
        (is (nil? (result/question en {:asked  asked
                                       :params {:mode "extended"}
                                       :tokens [{:id 1 :conditions
                                                 [{:id 1 :v "hund"}]}]})))
        (is (= [:q "hund"]
               (result/question en {:asked  asked
                                    :params {:mode "extended"}
                                    :tokens [{:id 1 :conditions
                                              [{:id 1 :v "kat"}]}]}))))
      (testing "and a page with no answer yet asks nothing"
        (is (nil? (result/question en {:params {:q ""}}))))))
  (testing "a CQP query is code, a list is its length, a word is quoted"
    (is (= [:code "[lemma = \"hund\"]"]
           (result/query-mark en {:q "[lemma = \"hund\"]"})))
    (is (= "2 words" (result/query-mark en {:q "hund\nkat"})))
    (is (= [:q "hund"] (result/query-mark en {:q "hund" :mode "simple"}))))
  (testing "an extended search names the CQP its tokens compile to"
    (is (= [:code "[lemma = \"hund\"]"]
           (result/query-mark en {:mode "extended" :t1.attr "lemma"
                                  :t1.v "hund"})))
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

(deftest subset-phrase-test
  (is (= (list [:code "lemma"] " " "before the match" " = " [:code "kat"])
         (result/subset-phrase en {:anchor "match[-1]" :attr :lemma
                                   :value  "kat"})))
  (is (= "lemma før matchet = kat"
         (text (result/subset-phrase da {:anchor "match[-1]" :attr :lemma
                                         :value  "kat"}))))
  (is (nil? (result/subset-phrase en nil)))
  (testing "a position nothing names is shown as CQP names it"
    (is (= "target" (result/position-label en "target"))))
  (is (= "over hele matchet" (result/position-label da "match..matchend"))))

(deftest near-phrase-test
  (is (= (list "near" " " [:code "kat"])
         (result/near-phrase en {:word "kat" :distance 5})))
  (is (= "sammen med kat"
         (text (result/near-phrase da {:word "kat" :distance 5}))))
  (is (nil? (result/near-phrase en nil))))

(deftest filter-phrase-test
  (is (= "" (result/filter-phrase {} nil)))
  (is (= "text_author ukendt; text_year 1583, 1591"
         (result/filter-phrase {:text_year   #{"1591" "1583"}
                                :text_author #{"ukendt"}}
                               nil)))
  (is (nil? (result/within-phrase en nil nil)))
  (is (= "within text_year 1591"
         (result/within-phrase en {:text_year #{"1591"}} nil)))
  (is (= "inden for text_year 1591"
         (result/within-phrase da {:text_year #{"1591"}} nil)))
  (testing "patterns follow the values, between slashes"
    (is (= "text_title /Hav.*/; text_year 1583, 1591, /16../"
           (result/filter-phrase {:text_year #{"1591" "1583"}}
                                 {:text_title ["Hav.*"] :text_year ["16.."]})))
    (is (= "within text_title /Hav.*/"
           (result/within-phrase en nil {:text_title ["Hav.*"]})))))

(deftest qualifiers-test
  (let [phrases (fn [ui params result]
                  (map text (result/qualifiers ui params result)))]
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
               (phrases en {:q "[]" :in "lemma" :match "infix"}
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

(deftest counting-test
  (testing "a result is being counted while corpora remain"
    (is (result/counting? {:remaining ["X"]}))
    (is (not (result/counting? {:remaining []})))
    (is (not (result/counting? {}))))
  (testing "the heading gives the hits counted so far as a floor"
    (is (= "at least 6 hits" (result/hits-heading en {:q "hund"} 6 true)))
    (is (= "mindst 6 forekomster" (result/hits-heading da {:q "hund"} 6 true))))
  (testing "the page is placed without a last page"
    (is (= "page 3" (result/page-phrase en {:page 2})))
    (is (= "side 3" (result/page-phrase da {:page 2}))))
  (testing "corpora still being counted count as searched"
    (is (result/searched? {:counts [] :remaining ["X"]}))
    (is (= ["in 3 corpora"]
           (map text (result/qualifiers en {} {:counts    [{:corpus "A" :size 1}]
                                               :remaining ["B" "C"]}))))))

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
  (is (= [:abbr {:title "key word in context"} "KWIC"]
         (result/view-label en :kwic)))
  (is (= [:abbr {:title "søgeord i kontekst"} "KWIC"]
         (result/view-label da :kwic)))
  (is (= "Frekvenser" (result/view-label da :frequencies))))

(deftest view-switch-test
  (let [html (result/view-switch en :kwic [[:kwic "/?v=k"] [:frequencies "/?v=f"]])]
    (testing "a named navigation over a row of links, chrome rather than
              text, the view shown marked"
      (is (= :nav.views.menu (first html)))
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
    (is (some #{"Request rejected"}
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
        html  (result/results-region state "6 hits" ["in 2 corpora"] [:p "body"])]
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
    (testing "its head holds the heading, the page's own h1, with the rest
              of the question under it as a subheading, and the views"
      (let [[tag [group h1 sub] views] (nth html 2)]
        (is (= :header.result-head tag))
        (is (= :hgroup group))
        (is (= [:h1 {:id "results-heading"} "6 hits"] h1))
        (is (= :p (first sub)))
        (is (= "in 2 corpora" (text sub)))
        (is (= :nav.views.menu (first views)))))
    (testing "the status line is a live region that stands even when
              silent, before anything whose kind can change"
      (is (= [:div.status {:role "status"} nil] (nth html 3))))
    (testing "the errors of individual corpora are headed sections before
              the body"
      (is (some #{[:h2 "CQP error"]} (deep html)))
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
