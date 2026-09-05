(ns dk.cst.corpus-probe.url-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.api :as api]
            [dk.cst.corpus-probe.commands :as commands]
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
    (is (= (:context url/defaults) (str (:context commands/kwic-defaults))))
    (is (= (:sort url/defaults) (ffirst commands/sort-modes)))
    (is (= (:distance url/defaults) (str page/near-distance)))
    (is (= (:view url/defaults) (second (first api/result-views))))
    (is (= (:attr url/defaults) (api/attr-param nil)))
    (is (= (:in url/defaults) (api/attr-param nil)))
    (is (= (:within url/defaults) (name (query/within-param nil))))
    (is (= (:subset-attr url/defaults) (api/attr-param nil)))
    (is (= (:at url/defaults) (api/position-param nil)))
    (is (= (:subset-at url/defaults) (api/position-param nil)))
    (is (= 0 (api/page-param (:page url/defaults))))
    (is (= (query/->cqp {:q "hund"})
           (query/->cqp {:q "hund" :mode (:mode url/defaults)})))
    (is (= (:mode url/defaults) (first url/modes)))))

(deftest modes-test
  (testing "every mode has its row of fields, and no row lacks its mode"
    (is (= (set url/modes) (set (keys url/fields)))))
  (testing "a URL naming no mode, or one the app does not know, is simple"
    (is (= "simple" (url/mode {})))
    (is (= "simple" (url/mode {:mode "nonesuch"})))
    (is (= "cqp" (url/mode {:mode "cqp"}))))
  (testing "a mode reads the mode, its own keys and, given tokens, their
            fields, nothing else"
    (is (url/reads? "simple" :within))
    (is (url/reads? "extended" :t2.3.v))
    (is (url/reads? "cqp" :mode))
    (is (not (url/reads? "cqp" :in)))
    (is (not (url/reads? "list" :within)))
    (is (not (url/reads? "simple" :t1.v)))
    (testing "the marker standing for the tokens is no key"
      (is (not (url/reads? "extended" ::url/tokens)))
      (is (not (url/query-key? ::url/tokens)))))
  (testing "a query key is one some mode reads; the rest say where and how"
    (is (every? url/query-key? [:q :mode :in :ci :match :within :t1.v
                                :t2.3.join]))
    (is (not-any? url/query-key? [:corpus :sort :f.text_year :page :from nil])))
  (testing "what the mode does not read is unread, and no URL carries it"
    (is (= #{:in :ci :match}
           (url/unread {:q "x" :mode "cqp" :in "lemma" :ci "on"
                        :match "prefix"})))
    (is (= {:q "x" :mode "cqp"}
           (url/canonical {:q "x" :mode "cqp" :in "lemma" :ci "on"
                           :match "prefix"})))
    (is (= {:mode "extended" :t1.v "kat"}
           (url/canonical {:q "hund" :mode "extended" :t1.v "kat"})))
    (is (= {:q "hund"}
           (url/canonical {:q "hund" :t1.v "kat" :t1.attr "lemma"})))
    (is (= {:q "a\nb" :mode "list"}
           (url/canonical {:q "a\nb" :mode "list" :within "text"})))
    (testing "while what it reads stays, and what every mode shares"
      (is (= {:q "a b" :within "text" :sort "word"}
             (url/canonical {:q "a b" :within "text" :sort "word"}))))))

(deftest unread-query?-test
  (testing "a query the mode does not read is the form submitted with its
            radio changed, or a hand-written URL"
    (is (url/unread-query? {:q "hund" :mode "extended"}))
    (is (url/unread-query? {:t1.v "hund" :mode "simple"}))
    (is (url/unread-query? {:t1.v "hund" :mode "cqp" :corpus "PROBE"})))
  (testing "not a query in its own mode, nor an option carried along"
    (is (not (url/unread-query? {:q "hund"})))
    (is (not (url/unread-query? {:q "hund" :mode "cqp" :in "lemma"})))
    (is (not (url/unread-query? {:mode "extended"}))))
  (testing "nor the blank field or the blank trailing token every form
            submits"
    (is (not (url/unread-query? {:q "" :mode "extended" :in "word"})))
    (is (not (url/unread-query? {:t1.attr "word" :t1.op "is" :t1.v ""
                                 :t1.min "1" :t1.max "1" :mode "simple"})))))

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

(deftest tokens-test
  (testing "a token's fields are known, and sort together after the mode"
    (is (= [2 1 :v] (url/token-field :t2.v)))
    (is (= [2 3 :join] (url/token-field :t2.3.join)))
    (is (nil? (url/token-field :t2.x)))
    (is (nil? (url/token-field :tv)))
    (is (url/known? :t1.attr))
    (is (= ["mode" "t1.attr" "t1.v" "t2.op" "within" "corpus"]
           (map first (url/pairs {:corpus "A" :t2.op "any" :t1.v "x"
                                  :within "paragraph"
                                  :t1.attr "lemma" :mode "extended"})))))
  (testing "a key is built as it is read"
    (is (= "t2.v" (url/token-key 2 1 :v)))
    (is (= "t2.3.join" (url/token-key 2 3 :join)))
    (is (= [2 3 :join] (url/token-field (keyword (url/token-key 2 3 :join)))))
    (is (= {:id 4 :conditions [{:id 1}]} (url/blank-token 4))))
  (testing "the tokens in order, each with its own fields and its
            conditions in order"
    (is (= [{:n 1 :conditions [{:c 1 :v "hund" :ci "on"}]}
            {:n 3 :max "2" :start "on" :conditions [{:c 1 :op "any"}]}]
           (url/token-rows {:t3.op "any" :t1.v "hund" :t3.max "2" :t3.start "on"
                            :t1.ci "on" :q "x"})))
    (is (= [{:n 1 :conditions [{:c 1 :v "a"} {:c 2 :v "b" :join "or"}]}]
           (url/token-rows {:t1.2.join "or" :t1.v "a" :t1.2.v "b"}))))
  (testing "a token asks for something when one of its conditions does:
            with a value, or as any word"
    (is (url/asks? {:conditions [{:v "hund"}]}))
    (is (url/asks? {:conditions [{:op "any"}]}))
    (is (url/asks? {:conditions [{:v ""} {:v "b" :join "or"}]}))
    (is (not (url/asks? {:conditions [{:op "prefix" :ci "on"}]})))
    (is (not (url/asks? {:conditions [{:v ""}]}))))
  (testing "the URL drops a field at its default, a token asking nothing
            and a condition asking nothing"
    (is (= {:mode "extended" :t1.v "hund" :t1.3.v "kat"
            :t2.op "any" :t2.max "3"}
           (url/canonical {:mode "extended"
                           :t1.attr "word" :t1.op "is" :t1.v "hund"
                           :t1.min "1" :t1.max "1"
                           :t1.2.attr "pos" :t1.2.join "or"
                           :t1.3.v "kat" :t1.3.join "and"
                           :t2.op "any" :t2.min "1" :t2.max "3"
                           :t3.attr "lemma" :t3.op "prefix" :t3.v ""
                           :t3.ci "on"}))))
  (testing "the defaults are the compiler's"
    (is (= "[word = \"x\"]"
           (query/extended->cqp
            [{:conditions [{:attr  (keyword (:attr url/token-defaults))
                            :op    (:op url/token-defaults)
                            :value "x"}]
              :min        (parse-long (:min url/token-defaults))
              :max        (parse-long (:max url/token-defaults))}])))
    (is (= "[a = \"x\" & b = \"y\"]"
           (query/token->cqp
            {:conditions [{:attr :a :value "x"}
                          {:attr :b :value "y"
                           :join (:join url/token-defaults)}]})))))

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

(deftest text-test
  (is (= "/corpora/probe/text?cpos=9#hit" (url/text "PROBE" 9 9)))
  (testing "a hit of several tokens names its end"
    (is (= "/corpora/probe/text?cpos=9&matchend=11#hit" (url/text "PROBE" 9 11))))
  (is (= "/corpora/probe/text?cpos=9#hit" (url/text "PROBE" 9 nil))))
