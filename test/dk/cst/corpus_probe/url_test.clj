(ns dk.cst.corpus-probe.url-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.api :as api]
            [dk.cst.corpus-probe.cwb.command :as command]
            [dk.cst.corpus-probe.query.params :as params]
            [dk.cst.corpus-probe.search.batch :as batch]
            [dk.cst.corpus-probe.url :as url]))

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
  (testing "the data the client fetches is under one prefix, apart from
            the pages"
    (is (every? #(str/starts-with? % "/api/")
                [url/context-api url/filters-api url/counts-api])))
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
    (is (= {:q "hund" :sort "word" :page "2"}
           (url/canonical {:q "hund" :sort "word" :page 2}))))
  (testing "no URL names a mode: the text says it"
    (is (= {:q "[]"} (url/canonical {:q "[]" :mode "simple"})))
    (is (= {:q "hund"} (url/canonical {:q "hund" :mode "simple"})))
    (testing "and the field's line breaks are one character each"
      (is (= {:q "a\nb"} (url/canonical {:q "a\r\nb"})))))
  (testing "what the mode does not read is not carried"
    (is (= {:q "[]"}
           (url/canonical {:q "[]" :in "lemma" :ci "on" :match "prefix"})))
    (is (= {:t1.v "kat"}
           (url/canonical {:q "hund" :mode "extended" :t1.v "kat"})))
    (is (= {:t1.v "kat" :t1.attr "lemma"}
           (url/canonical {:q "hund" :t1.v "kat" :t1.attr "lemma"})))
    (is (= {:q "hund"}
           (url/canonical {:q "hund" :t1.v "kat" :mode "simple"})))
    (is (= {:q "a\nb"}
           (url/canonical {:q "a\nb" :within "text"})))
    (testing "while what it reads stays, and what every mode shares"
      (is (= {:q "a b" :within "text" :sort "word"}
             (url/canonical {:q "a b" :within "text" :sort "word"})))))
  (testing "the URL drops a token's field at its default, a token asking
            nothing and a condition asking nothing"
    (is (= {:t1.v "hund" :t1.3.v "kat" :t2.op "any" :t2.max "3"}
           (url/canonical {:mode "extended"
                           :t1.attr "word" :t1.op "is" :t1.v "hund"
                           :t1.min "1" :t1.max "1"
                           :t1.2.attr "pos" :t1.2.join "or"
                           :t1.3.v "kat" :t1.3.join "and"
                           :t2.op "any" :t2.min "1" :t2.max "3"
                           :t3.attr "lemma" :t3.op "prefix" :t3.v ""
                           :t3.ci "on"}))))
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
    (is (= (:context url/defaults) (str (:context batch/kwic-defaults))))
    (is (= (:sort url/defaults) (ffirst command/sort-modes)))
    (is (= (:distance url/defaults)
           (str (:distance (api/near-param "kat" nil)))))
    (is (= (:view url/defaults) (second (first url/result-views))))
    (is (= (api/view-param nil) (api/view-param (:view url/defaults))))
    (is (= (:attr url/defaults) (api/attr-param nil)))
    (is (= (:in url/defaults) (api/attr-param nil)))
    (is (= (:within url/defaults) (name (params/within-param nil))))
    (is (= (:subset-attr url/defaults) (api/attr-param nil)))
    (is (= (:at url/defaults) (api/position-param nil)))
    (is (= (:subset-at url/defaults) (api/position-param nil)))
    (is (= 0 (api/page-param (:page url/defaults))))))

(deftest pairs-test
  (testing "a token's fields are known, and sort together after the mode;
            the filter's params sort by name among themselves"
    (is (url/known? :t1.attr))
    (is (not (url/known? :lang)))
    (is (= ["t1.attr" "t1.v" "t2.op" "within" "corpus" "f.text_author"
            "f.text_year" "ff.text_year" "page"]
           (map first (url/pairs {:corpus "A" :t2.op "any" :t1.v "x"
                                  :within "paragraph" :page "2"
                                  :ff.text_year "1590" :f.text_year "1591"
                                  :f.text_author "x"
                                  :t1.attr "lemma"}))))))

(deftest query-string-test
  (testing "what was asked, where, which hits, how shown, where in them"
    (is (= (str "q=hund&corpus=PROBE&f.text_year=1591&near=kat"
                "&distance=3&sample=100&view=frequencies&attr=lemma&page=2")
           (url/query-string {:page "2" :attr "lemma" :view "frequencies"
                              :sample "100" :distance "3" :near "kat"
                              :f.text_year "1591" :corpus "PROBE"
                              :q "hund"}))))
  (testing "the filter's params sort by name among themselves"
    (is (= "f.text_author=x&f.text_year=1591&ff.text_year=1590&fp.text_title=H"
           (url/query-string {:fp.text_title "H" :ff.text_year "1590"
                              :f.text_year "1591" :f.text_author "x"}))))
  (testing "a vector value repeats its key"
    (is (= "f.text_year=1591&f.text_year=1583"
           (url/query-string {:f.text_year ["1591" "1583"]}))))
  (testing "form encoding, as a browser submits a GET form"
    (is (= "q=%5Blemma+%3D+%22hund%22%5D"
           (url/query-string {:q "[lemma = \"hund\"]"})))
    (is (= "q=h%C3%B8ne" (url/query-string {:q "høne"}))))
  (testing "except that a comma and a colon stay readable"
    (is (= "q=a:%5B%5D+::+b&corpus=PROBE,VISER&expand=PROBE:9,PROBE:12"
           (url/query-string {:q "a:[] :: b"
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

(deftest search-params-test
  (testing "the filter params identify a search along with the query"
    (is (= {:q "hund" :corpus ["A"] :f.text_year ["1591"]}
           (url/search-params {:q "hund" :corpus ["A"] :page "2" :sort "word"
                               :f.text_year ["1591"]})))
    (testing "and so do its patterns and ranges"
      (is (= {:q "hund" :fp.text_title "Hav.*" :ff.text_year "1590"
              :ft.text_year "1592"}
             (url/search-params {:q "hund" :fp.text_title "Hav.*"
                                 :ff.text_year "1590" :ft.text_year "1592"
                                 :page "2"})))))
  (testing "so does the sample, which decides which hits there are; the
            sort, which only decides their order, still does not"
    (is (= {:q "hund" :sample "100"}
           (url/search-params {:q "hund" :sample "100" :sort "word"}))))
  (testing "and so does the unit the words are kept within"
    (is (= {:q "lille hund" :within "text"}
           (url/search-params {:q "lille hund" :within "text" :sort "word"}))))
  (testing "the tokens identify the search, as the query does"
    (is (= {:mode "extended" :t1.v "a" :t2.op "any"}
           (url/search-params {:mode "extended" :t1.v "a" :t2.op "any"
                               :page "2"})))))

(deftest page-href-test
  (let [href (url/page-href {:corpus "PROBE" :q "hund" :page "1"} 2)]
    (is (str/starts-with? href "/search?"))
    (is (str/includes? href "corpus=PROBE"))
    (testing "a page turn lands on the results, not the top of the form"
      (is (str/ends-with? href "#results")))
    (testing "the URL counts pages from one, as the page does"
      (is (str/includes? href "page=3"))
      (is (not (str/includes? (url/page-href {:q "hund"} 0) "page="))))
    (testing "the query is URL-encoded"
      (is (str/includes? (url/page-href {:q "[lemma=\"a\"]"} 1) "%5B")))
    (testing "the per-page expand parameter is dropped"
      (is (not (str/includes? (url/page-href {:corpus "PROBE" :expand "9"} 1)
                              "expand"))))))

(deftest page-hrefs-test
  (let [params {:q "hund"}]
    (testing "the first page links onward only when the hits reach past it"
      (is (= {:prev-href nil :next-href nil}
             (url/page-hrefs params 0 {:size 25 :page-size 25})))
      (is (str/ends-with? (:next-href (url/page-hrefs params 0 {:size      26
                                                                :page-size 25}))
                          "page=2#results")))
    (testing "a result still being counted links onward on what it has so far"
      (is (some? (:next-href (url/page-hrefs params 0 {:size      26
                                                       :page-size 25
                                                       :remaining ["X"]}))))
      (is (nil? (:next-href (url/page-hrefs params 0 {:size      10
                                                      :page-size 25
                                                      :remaining ["X"]})))))
    (testing "and back from any page but the first, result or no result"
      (is (str/ends-with? (:prev-href (url/page-hrefs params 2 nil))
                          "page=2#results"))
      (is (nil? (:next-href (url/page-hrefs params 2 nil)))))
    (testing "a result of no hits is one page"
      (is (= 1 (url/page-count {:size 0 :page-size 25})))
      (is (= 2 (url/page-count {:size 26 :page-size 25}))))))

(deftest view-hrefs-test
  (let [hrefs (url/view-hrefs {:q "hund" :corpus ["PROBE"] :lang "da"})]
    (testing "one entry per view, in display order"
      (is (= [:kwic :frequencies] (map first hrefs))))
    (testing "every view of one search shares its URL but for the view param"
      (doseq [[_ href] hrefs]
        (is (str/starts-with? href "/search?q=hund&corpus=PROBE"))
        (is (str/ends-with? href "#results")))
      (testing "and the concordance, being the default, goes unnamed"
        (is (not (str/includes? (last (first hrefs)) "view=")))
        (is (str/includes? (last (second hrefs)) "view=frequencies"))))
    (testing "no URL names a language: that is the reader's own preference"
      (is (not (some #(str/includes? (last %) "lang") hrefs)))))
  (testing "the second attribute of a table travels between the views"
    (is (every? #(str/includes? (second %) "by=text_year")
                (url/view-hrefs {:q "hund" :attr "lemma" :by "text_year"})))))

(deftest subset-href-test
  (let [href (url/subset-href {:q "hund" :corpus ["PROBE"] :attr "lemma"
                               :at "match[-1]" :sort "word"}
                              :lemma "match[-1]" "en kat")]
    (testing "the concordance of the same search, kept to the row's hits"
      (is (str/starts-with? href "/search?"))
      (is (str/includes? href "subset=en+kat"))
      (is (str/includes? href "subset-at=match%5B-1%5D"))
      (is (str/includes? href "subset-attr=lemma"))
      (is (str/ends-with? href "#results")))
    (testing "the concordance is the default view, so it goes unnamed"
      (is (not (str/includes? href "view="))))
    (testing "the grouping and the order are the table's, not the hits'"
      (is (not (str/includes? href "attr=lemma&")))
      (is (not (str/includes? href "sort="))))))

(deftest export-hrefs-test
  (testing "the view of the search as a file, one URL per format"
    (is (= {:csv "/search/kwic.csv?q=hund&corpus=PROBE"
            :tsv "/search/kwic.tsv?q=hund&corpus=PROBE"}
           (url/export-hrefs :kwic url/export-formats
                             {:corpus ["PROBE"] :q "hund"})))
    (is (= "/search/frequencies.tsv?q=hund&attr=lemma"
           (:tsv (url/export-hrefs :frequencies ["tsv"]
                                   {:q "hund" :attr "lemma"}))))))

(deftest nav-hrefs-test
  (testing "the search keeps the current query, the rest are their pages"
    (is (= {:search          "/search?q=hund&corpus=PROBE#results"
            :corpora-heading "/corpora"
            :glossary        "/glossary"}
           (url/nav-hrefs {:q "hund" :corpus ["PROBE"] :sort "word"}))))
  (testing "and is the bare page when nothing was asked"
    (is (= "/search" (:search (url/nav-hrefs {:sort "word"}))))))

(deftest corpora-param-test
  (testing "one, repeated and comma-joined values all select corpora"
    (is (= ["PROBE"] (url/corpora-param "PROBE")))
    (is (= ["PROBE" "VISER"] (url/corpora-param ["PROBE" "VISER"])))
    (is (= ["PROBE" "VISER" "TALER"]
           (url/corpora-param ["PROBE,VISER" "TALER"]))))
  (testing "names are uppercased and deduplicated, blanks dropped"
    (is (= ["PROBE"] (url/corpora-param ["probe" "PROBE" ""]))))
  (is (= [] (url/corpora-param nil))))

(deftest expand-test
  (testing "the expanded hits as the URL names them"
    (is (= #{["PROBE" 9] ["VISER" 12]} (url/expand-param "PROBE:9,VISER:12")))
    (is (= #{["PROBE" 9]} (url/expand-param ["PROBE:9" "x" "PROBE:"])))
    (is (nil? (url/expand-param nil)))
    (is (nil? (url/expand-param ""))))
  (testing "and back, in order, or not at all"
    (let [hits #{["PROBE" 12] ["PROBE" 9]}]
      (is (= {:q "hund" :expand "PROBE:9,PROBE:12"}
             (url/with-expanded {:q "hund"} hits)))
      (is (= hits (url/expand-param (:expand (url/with-expanded {} hits)))))
      (is (= {:q "hund"}
             (url/with-expanded {:q "hund" :expand "PROBE:9"} nil))))))

(deftest metadata-key?-test
  (is (url/metadata-key? :f.text_year))
  (is (url/metadata-key? :fp.text_title))
  (is (url/metadata-key? :ff.text_year))
  (is (url/metadata-key? :ft.text_year))
  (testing "a prefix alone names no attribute"
    (is (not (url/metadata-key? :f.)))
    (is (not (url/metadata-key? :fx.text_year)))
    (is (not (url/metadata-key? :format)))
    (is (not (url/metadata-key? nil))))
  (testing "every prefix of the table is one, followed by an attribute"
    (is (every? #(url/metadata-key? (keyword (str % "text_year")))
                (vals url/filter-prefixes)))))

(deftest text-test
  (is (= "/corpora/probe/text?cpos=9#hit" (url/text "PROBE" 9 9)))
  (testing "a hit of several tokens names its end"
    (is (= "/corpora/probe/text?cpos=9&matchend=11#hit" (url/text "PROBE" 9 11))))
  (is (= "/corpora/probe/text?cpos=9#hit" (url/text "PROBE" 9 nil))))
