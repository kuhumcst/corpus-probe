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
    (is (= "6 hits" (result/hits-heading en 6)))
    (is (= "6 hits" (result/hits-heading da 6)))))

(deftest asked?-test
  (testing "a search asks something or it does not run at all, and an
            extended form reads its tokens rather than the field"
    (is (result/asked? {:q "hund"}))
    (is (not (result/asked? {:q "hund" :mode "extended"})))
    (is (not (result/asked? {:q ""})))))

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
  (is (= "hele matchet" (result/position-label da "match..matchend")))
  (is (= "først i matchet" (result/position-label da "match")))
  (testing "a position nothing names is shown as CQP names it"
    (is (= "target" (result/position-label en "target")))))

(deftest subset-inputs-test
  (is (= (list [:input {:type "hidden" :name "subset" :value "kat"}]
               [:input {:type "hidden" :name "subset-at" :value "match[-1]"}]
               [:input {:type "hidden" :name "subset-attr" :value "lemma"}])
         (result/subset-inputs {:anchor "match[-1]" :attr :lemma
                                :value  "kat"})))
  (is (nil? (result/subset-inputs nil))))

(deftest subset-note-test
  (let [subset {:anchor "match" :attr :lemma :value "hund"}
        href   "/search?q=%5Bpos+%3D+%22N.*%22%5D&sort=word#results"]
    (testing "a slice of an answer is said as a sentence, in the words the
              table that made it used"
      (is (= (str "Showing only the hits where the lemma is \"hund\""
                  " first in the match")
             (result/subset-phrase en subset)))
      (is (= "Viser kun de hits hvor ordet er \"kat\" før matchet"
             (result/subset-phrase da {:anchor "match[-1]" :attr :word
                                       :value  "kat"})))
      (testing "an attribute the phrase has no word for is named as is"
        (is (= "Showing only the hits where s_id is \"3\" after the match"
               (result/subset-phrase en {:anchor "matchend[1]" :attr :s_id
                                         :value  "3"}))))
      (is (nil? (result/subset-phrase en nil))))
    (testing "and over the hits, with the way out in parentheses: all the
              hits, read as these were, from the first page"
      (let [note (result/subset-note en href subset)]
        (is (= :p.subset (first note)))
        (is (some #{(str "Showing only the hits where the lemma is \"hund\""
                         " first in the match")}
                  (deep note)))
        (is (some #(and (map? %) (= href (:href %))) (deep note)))
        (is (some #{"show all"} (deep note))))
      (is (nil? (result/subset-note en href nil))))))

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
  (testing "every corpus answering, the reach folds away under a sign, the
            corpora the hits are in inside, and nothing left out"
    (let [[tag attrs summary found more]
          (result/reach en {:counts [{:corpus "PROBE" :size 5}]})]
      (is (= :details.caveats tag))
      (is (nil? (:class attrs)))
      (is (= [:summary {:title "in PROBE"} [:span.spoken "in PROBE"]] summary))
      (is (= [:p "in PROBE"] found))
      (is (nil? more))))
  (testing "a corpus left out is counted in the sign's name and listed
            under the corpora found, the sign classed for it"
    (let [[tag attrs summary found [_ items]] (result/reach en example-result)]
      (is (= :details.caveats tag))
      (is (= "left-out" (:class attrs)))
      (is (= [:p "in 2 corpora"] found))
      (testing "labelled in words, the sign that says there is something
                here being the stylesheet's, which reaches nobody
                listening"
        (is (= "in 2 corpora, 2 corpora left out of the search"
               (:title (second summary))))
        (is (= [:span.spoken "in 2 corpora, 2 corpora left out of the search"]
               (nth summary 2))))
      (testing "one item for each way of failing, naming what it took and
                carrying CQP's own words where they are the reason"
        (is (= 1 (count items)))
        (is (= "TALER, GONE: CQP errorno lemma" (text (first items)))))))
  (testing "and it is said even where no corpus found anything to place"
    (let [counts [{:corpus "PROBE" :size 0}
                  {:corpus "X" :error {:type :timeout}}]
          [tag _ summary found] (result/reach en {:counts counts})]
      (is (= :details.caveats tag))
      (is (= "1 corpus left out of the search" (:title (second summary))))
      (is (nil? found))))
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
    (is (= "at least 6 hits" (result/hits-heading en 6 true)))
    (is (= "mindst 6 hits" (result/hits-heading da 6 true))))
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

(deftest found?-test
  (testing "a corpus holding a hit is something to read"
    (is (result/found? example-result))
    (is (result/found? {:counts [{:corpus "PROBE" :size 5}
                                 {:corpus "VISER" :size 0}]})))
  (testing "an answer of none is not, nor is a search that failed"
    (is (not (result/found? {:counts [{:corpus "PROBE" :size 0}]})))
    (is (not (result/found? {:counts [{:corpus "X" :error {:type :timeout}}]})))
    (is (not (result/found? nil)))))

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
      (is (nil? (result/view-controls en false nil))))
    (testing "a control that acts on a result submits the form that made it"
      (let [html (result/view-controls en false (sort* "en"))]
        (is (= :div.view-controls (first html)))
        (is (some #(and (map? %) (= url/form-id (:form %))) (deep html)))))
    (testing "without a client, a button is what applies it, one a browser
              with a script never shows"
      (is (some #{"Apply"} (deep (result/view-controls en false (sort* "en")))))
      (is (some #(and (vector? %) (= :noscript (first %)))
                (deep (result/view-controls en false (sort* "en")))))
      (is (some #{"Anvend"} (deep (result/view-controls da false
                                                        (sort* "da"))))))
    (testing "with one, choosing an order is asking for it: no button"
      (let [html (result/view-controls en true (sort* "en"))]
        (is (not (some #{"Apply"} (deep html))))
        (is (not (some #(and (vector? %) (= :button (first %))) (deep html))))))
    (testing "and the control itself is what applies it"
      (is (some #(and (map? %) (= [:apply-view "sort" :event.target/value]
                                  (get-in % [:on :change])))
                (deep (sort* "en")))))))

(deftest pager-test
  (testing "no links renders nothing"
    (is (nil? (result/pager en nil nil "page 1 of 1"))))
  (testing "the links are a named navigation landmark"
    (let [html (result/pager en "/?page=0" "/?page=2" "page 2 of 3")]
      (is (= :nav.pagination (first html)))
      (is (= "Pagination" (:aria-label (second html))))
      (is (= "Sidenavigation"
             (:aria-label (second (result/pager da "/?page=0" nil "x")))))))
  (testing "links carry the rel values browsers use for a sequence, each
            in a column of its own"
    (let [html (result/pager en "/?page=0" "/?page=2" "page 2 of 3")]
      (is (some #{:ul.row.pager} (deep html)))
      (is (some #{[:li.pager-prev
                   [:a {:href "/?page=0" :rel "prev"} "← previous"]]}
                (deep html)))
      (is (some #{[:li.pager-next
                   [:a {:href "/?page=2" :rel "next"} "next →"]]}
                (deep html)))))
  (testing "the position rides between the two directions"
    (let [html (result/pager en "/?page=0" "/?page=2" "page 2 of 3")]
      (is (= [:li "page 2 of 3"]
             (second (filter #(and (vector? %)
                                   (#{:li :li.pager-prev :li.pager-next}
                                    (first %)))
                             (deep html)))))))
  (testing "a direction that is out of range is left out, not held open"
    (let [html (result/pager en nil "/?page=1" "page 1 of 3")]
      (is (= 2 (count (filter #(and (vector? %)
                                    (#{:li :li.pager-prev :li.pager-next}
                                     (first %)))
                              (deep html)))))
      (is (not (some #{"prev"} (deep html))))))
  (testing "in Danish"
    (let [html (result/pager da "/?page=0" "/?page=2" "side 2 af 3")]
      (is (some #{"← forrige"} (deep html)))
      (is (some #{"næste →"} (deep html))))))

(deftest page-control-test
  (testing "a result still being counted knows of no pages to offer, so it
            says where the reader is and no more"
    (is (= "page 2" (result/page-control en true {:page 1})))
    (is (= "side 2" (result/page-control da true {:page 1}))))
  (testing "without a client there is nothing to follow the select, the
            pager's own links being what remains"
    (is (= "page 2 of 6" (result/page-control en false {:page 1 :pages 6}))))
  (testing "a page each, the one being read chosen, every option reading as
            the phrase it replaces so the closed select says the same"
    (let [options (nth (result/page-control en true {:page 1 :pages 3}) 2)]
      (is (= [[:option {:value 1 :selected false} "1 of 3"]
              [:option {:value 2 :selected true} "2 of 3"]
              [:option {:value 3 :selected false} "3 of 3"]]
             options))
      (is (= "1 af 6"
             (last (first (nth (result/page-control da true {:page 0 :pages 6})
                               2)))))))
  (testing "the select is named by what choosing does, nothing beside it
            being left to name it, and is followed rather than submitted:
            no name to submit under and no form to submit with"
    (let [attrs (second (result/page-control en true {:page 0 :pages 2}))]
      (is (= "Go to page" (:aria-label attrs)))
      (is (= "Gå til side"
             (:aria-label (second (result/page-control da true {:page 0
                                                                :pages 2})))))
      (is (= {:change [:go-to-page :event.target/value]} (:on attrs)))
      (is (nil? (:name attrs)))
      (is (nil? (:form attrs))))))

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

(deftest held-test
  (testing "the form's value where it holds one, the result's where it
            holds none, and none at all for the blank a control says none
            with"
    (is (= "word" (result/held {:sort "word"} :sort "corpus")))
    (is (= "corpus" (result/held {} :sort "corpus")))
    (is (nil? (result/held {:sort ""} :sort "corpus")))
    (is (= 5 (result/held {} :context 5)))))

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
        (is (= :details.caveats (first reach)))
        (is (= "in 2 corpora" (text (nth reach 3))))
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
               (result/result-heading en counting nil)))
        (is (some #{[:p "Counting hits in 2 corpora …"]}
                  (deep (result/results-region (assoc state :result counting)
                                               "x" nil nil))))))
    (testing "a search that failed outright is headed by its error"
      (is (= "No corpus selected"
             (result/result-heading en nil {:type :no-corpus})))
      (is (some #{"Select at least one corpus to search."}
                (deep (result/results-region {:ui en :error {:type :no-corpus}}
                                             "x" nil nil)))))))
