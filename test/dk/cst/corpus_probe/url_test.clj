(ns dk.cst.corpus-probe.url-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.api :as api]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.page :as page]))

(deftest paths-test
  (is (= "/" url/home))
  (is (= "/search" url/search))
  (is (= "/corpora" url/corpora))
  (testing "a corpus is a page under the index, by its lower-cased ID"
    (is (= "/corpora/viser" (url/corpus "VISER"))))
  (testing "a glossary entry is a place in the glossary"
    (is (= "/glossary" url/glossary))
    (is (= "/glossary#kwic" (url/glossary-entry "kwic"))))
  (testing "an export is the view of a result as a file under the search"
    (is (= "/search/kwic.tsv" (url/export :kwic "tsv")))
    (is (= "/search/frequencies.csv" (url/export "frequencies" :csv))))
  (testing "the fragment names the results region"
    (is (= (str "#" url/results-id) url/results-fragment))))

(deftest canonical-test
  (testing "a default says nothing, so a URL does not say it"
    (is (= {:q "hund"}
           (url/canonical {:q "hund" :mode "simple" :in "word" :sort "corpus"
                           :context "5" :view "kwic" :attr "word" :at "match"
                           :page "1"}))))
  (testing "nor does a blank, nor a param the app does not read"
    (is (= {:q "hund"}
           (url/canonical {:q "hund" :sample "" :fp.text_title " "
                           :lang "da" :format "tsv" nil "x"}))))
  (testing "what departs from the default stays, as a string"
    (is (= {:q "hund" :mode "cqp" :sort "word" :page "2"}
           (url/canonical {:q "hund" :mode "cqp" :sort "word" :page 2}))))
  (testing "the corpora are one param, uppercased, deduplicated, in order"
    (is (= {:q "hund" :corpus "PROBE,VISER"}
           (url/canonical {:q "hund" :corpus ["probe" "VISER" "PROBE"]})))
    (is (= {:corpus "PROBE,VISER"}
           (url/canonical {:corpus ["PROBE,VISER" ""]}))))
  (testing "every corpus that can be searched is no corpus named"
    (is (= {:q "hund"}
           (url/canonical {:q "hund" :corpus ["PROBE" "VISER"] :scope "chosen"}
                          #{"VISER" "PROBE"})))
    (testing "but not without knowing which those are"
      (is (= {:q "hund" :corpus "PROBE,VISER"}
             (url/canonical {:q "hund" :corpus ["PROBE" "VISER"]
                             :scope "chosen"})))))
  (testing "the scope marker survives only an emptied selection"
    (is (= {:q "hund" :scope "chosen"}
           (url/canonical {:q "hund" :scope "chosen"} #{"PROBE"})))
    (is (= {:q "hund"} (url/canonical {:q "hund"} #{"PROBE"})))
    (is (= {:q "hund" :corpus "PROBE"}
           (url/canonical {:q "hund" :corpus "PROBE" :scope "chosen"}
                          #{"PROBE" "VISER"}))))
  (testing "a param qualifying an absent one goes with it"
    (is (= {:q "hund"}
           (url/canonical {:q "hund" :distance "3" :subset-at "matchend"
                           :subset-attr "lemma"})))
    (is (= {:q "hund" :near "kat" :distance "3"}
           (url/canonical {:q "hund" :near "kat" :distance "3"})))
    (is (= {:q "hund" :subset "kat" :subset-at "matchend"}
           (url/canonical {:q "hund" :subset "kat" :subset-at "matchend"
                           :subset-attr "word"}))))
  (testing "the metadata filter keeps its values, blanks aside"
    (is (= {:f.text_year ["1591" "1583"] :fp.text_title "Hav.*"
            :ff.text_year "1590" :ft.text_year "1592"}
           (url/canonical {:f.text_year ["1591" "" "1583"]
                           :fp.text_title "Hav.*" :ff.text_year "1590"
                           :ft.text_year "1592" :f.text_author [""]}))))
  (testing "canonical params are their own canonical form"
    (let [params (url/canonical {:q "hund" :corpus ["PROBE" "VISER"]
                                 :f.text_year ["1591"] :near "kat"
                                 :distance "3" :view "frequencies"})]
      (is (= params (url/canonical params))))))

(deftest defaults-test
  (testing "each default is the one its reader applies to a URL without it"
    (is (= (:context url/defaults) (str (:context query/kwic-defaults))))
    (is (= (:sort url/defaults) (ffirst query/sort-modes)))
    (is (= (:distance url/defaults) (str page/near-distance)))
    (is (= (:view url/defaults) (second (first api/result-views))))
    (is (= (:attr url/defaults) (api/attr-param nil)))
    (is (= (:in url/defaults) (api/attr-param nil)))
    (is (= (:subset-attr url/defaults) (api/attr-param nil)))
    (is (= (:at url/defaults) (api/position-param nil)))
    (is (= (:subset-at url/defaults) (api/position-param nil)))
    (is (= 0 (api/page-param (:page url/defaults))))
    (is (= (api/->cqp {:q "hund"})
           (api/->cqp {:q "hund" :mode (:mode url/defaults)})))))

(deftest query-string-test
  (testing "what was asked, where, which hits, how shown, where in them"
    (is (= (str "q=hund&mode=cqp&corpus=PROBE&f.text_year=1591&near=kat"
                "&distance=3&sample=100&view=frequencies&attr=lemma&page=2")
           (url/query-string {:page "2" :attr "lemma" :view "frequencies"
                              :sample "100" :distance "3" :near "kat"
                              :f.text_year "1591" :corpus "PROBE"
                              :mode "cqp" :q "hund"}))))
  (testing "the filter's params sort by name among themselves"
    (is (= "f.text_author=x&f.text_year=1591&ff.text_year=1590&fp.text_title=H"
           (url/query-string {:fp.text_title "H" :ff.text_year "1590"
                              :f.text_year "1591" :f.text_author "x"}))))
  (testing "a vector value repeats its key"
    (is (= "f.text_year=1591&f.text_year=1583"
           (url/query-string {:f.text_year ["1591" "1583"]}))))
  (testing "form encoding, as a browser submits a GET form"
    (is (= "q=%5Blemma+%3D+%22hund%22%5D&mode=cqp"
           (url/query-string {:q "[lemma = \"hund\"]" :mode "cqp"})))
    (is (= "q=h%C3%B8ne" (url/query-string {:q "høne"}))))
  (testing "except that a comma and a colon stay readable"
    (is (= "q=a:%5B%5D+::+b&mode=cqp&corpus=PROBE,VISER&expand=PROBE:9,PROBE:12"
           (url/query-string {:q "a:[] :: b" :mode "cqp"
                              :corpus ["PROBE" "VISER"]
                              :expand "PROBE:9,PROBE:12"}))))
  (testing "nothing to say is an empty string"
    (is (= "" (url/query-string {:mode "simple"})))))

(deftest hrefs-test
  (testing "the search page, bare when the params say nothing"
    (is (= "/search" (url/search-href {})))
    (is (= "/search" (url/search-href {:mode "simple"})))
    (is (= "/search?corpus=PROBE" (url/search-href {:corpus "PROBE"}))))
  (testing "a result, which a link lands on"
    (is (= "/search?q=hund&page=2#results"
           (url/results-href {:q "hund" :page 2}))))
  (testing "an export of a view of it"
    (is (= "/search/kwic.tsv?q=hund" (url/export-href :kwic "tsv" {:q "hund"})))
    (is (= "/search/frequencies.csv?q=hund&attr=lemma"
           (url/export-href :frequencies "csv" {:q "hund" :attr "lemma"})))))

(deftest corpora-param-test
  (testing "one, repeated and comma-joined values all select corpora"
    (is (= ["PROBE"] (url/corpora-param "PROBE")))
    (is (= ["PROBE" "VISER"] (url/corpora-param ["PROBE" "VISER"])))
    (is (= ["PROBE" "VISER" "TALER"]
           (url/corpora-param ["PROBE,VISER" "TALER"]))))
  (testing "names are uppercased and deduplicated, blanks dropped"
    (is (= ["PROBE"] (url/corpora-param ["probe" "PROBE" ""]))))
  (is (= [] (url/corpora-param nil))))

(deftest metadata-key?-test
  (is (url/metadata-key? :f.text_year))
  (is (url/metadata-key? :fp.text_title))
  (is (url/metadata-key? :ff.text_year))
  (is (url/metadata-key? :ft.text_year))
  (testing "a prefix alone names no attribute"
    (is (not (url/metadata-key? :f.)))
    (is (not (url/metadata-key? :fx.text_year)))
    (is (not (url/metadata-key? :format)))
    (is (not (url/metadata-key? nil)))))
