(ns dk.cst.corpus-probe.query-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.query :as query]))

(deftest escape-literal-test
  (testing "PCRE metacharacters are backslash-escaped"
    (is (= "a\\.b\\*c" (query/escape-literal "a.b*c")))
    (is (= "\\[\\]\\{\\}\\(\\)\\^\\$\\|\\?\\+\\\\"
           (query/escape-literal "[]{}()^$|?+\\"))))
  (testing "quotes are doubled, CQP-style"
    (is (= "12\"\"-screen" (query/escape-literal "12\"-screen"))))
  (testing "ordinary text passes through"
    (is (= "København" (query/escape-literal "København")))))

(deftest extended->cqp-test
  (testing "each operator against the attribute, the literal escaped"
    (is (= "word = \"hund\"" (query/condition->cqp {:value "hund"})))
    (is (= "lemma != \"hund\""
           (query/condition->cqp {:attr :lemma :op "not" :value "hund"})))
    (is (= "word = \"hu\\.\\*.*\""
           (query/condition->cqp {:op "prefix" :value "hu.*"})))
    (is (= "word = \".*nd\"" (query/condition->cqp {:op "suffix" :value "nd"})))
    (is (= "word = \".*un.*\"" (query/condition->cqp {:op "infix" :value "un"})))
    (is (= "pos = \"N\" %c"
           (query/condition->cqp {:attr :pos :op "is" :value "N" :ci? true}))))
  (testing "a regex is kept as written, but for the quotes it cannot hold"
    (is (= "pos = \"N.*|V.*\""
           (query/condition->cqp {:attr :pos :op "regex" :value "N.*|V.*"})))
    (is (= "word != \"a\"\"b\""
           (query/condition->cqp {:op "not-regex" :value "a\"b"})))
    (is (= "word = \"a b\"" (query/condition->cqp {:op "regex" :value "a\nb"}))))
  (testing "a token's conditions, grouped as KORP reads them: ors within
            a group, ands between groups"
    (is (= "[word = \"hund\"]" (query/token->cqp {:conditions [{:value "hund"}]})))
    (is (= "[lemma = \"hund\" | lemma = \"kat\"]"
           (query/token->cqp {:conditions [{:attr :lemma :value "hund"}
                                           {:attr :lemma :value "kat"
                                            :join "or"}]})))
    (is (= "[lemma = \"hund\" & pos = \"N.*\"]"
           (query/token->cqp {:conditions [{:attr :lemma :value "hund"}
                                           {:attr :pos :op "prefix" :value "N"
                                            :join "and"}]})))
    (is (= "[(lemma = \"hund\" | lemma = \"kat\") & pos = \"N.*\"]"
           (query/token->cqp {:conditions [{:attr :lemma :value "hund"}
                                           {:attr :lemma :value "kat"
                                            :join "or"}
                                           {:attr :pos :op "prefix" :value "N"
                                            :join "and"}]}))))
  (testing "an any token, repeats that are not once, and the sentence edges"
    (is (= "[]" (query/token->cqp {:conditions [{:op "any"}]})))
    (is (= "[]{0,2}" (query/token->cqp {:conditions [{:op "any"}] :min 0 :max 2})))
    (is (= "[word = \"x\"]{2,2}"
           (query/token->cqp {:conditions [{:value "x"}] :min 2 :max 2})))
    (is (= "<s> [word = \"x\"]{1,3} </s>"
           (query/token->cqp {:conditions [{:value "x"}] :max 3
                              :start? true :end? true}))))
  (testing "the tokens in order, none being no query"
    (is (= "[pos = \"N.*\"] []{1,2} [word = \"hund\"]"
           (query/extended->cqp
            [{:conditions [{:attr :pos :op "prefix" :value "N"}]}
             {:conditions [{:op "any"}] :max 2}
             {:conditions [{:attr :word :op "is" :value "hund"}]}])))
    (is (nil? (query/extended->cqp [])))))

(deftest simple->cqp-test
  (is (= "[word = \"hund\"]" (query/simple->cqp "hund")))
  (is (= "[word = \"lille\" %c] [word = \"hund\" %c]"
         (query/simple->cqp " lille  hund " {:case-insensitive? true})))
  (is (= "[word = \"hund.*\"]" (query/simple->cqp "hund" {:prefix? true})))
  (is (= "[word = \".*hund.*\"]"
         (query/simple->cqp "hund" {:prefix? true :suffix? true})))
  (testing "any positional attribute can be the one matched"
    (is (= "[lemma = \"hund\"]" (query/simple->cqp "hund" {:attr :lemma})))
    (is (= "[lemma = \"hund.*\" %c]"
           (query/simple->cqp "hund" {:attr :lemma :prefix? true
                                      :case-insensitive? true}))))
  (testing "regex metacharacters in input are matched literally"
    (is (= "[word = \"hund\\.\"]" (query/simple->cqp "hund."))))
  (testing "blank input yields nil, not a match-everything query"
    (is (nil? (query/simple->cqp "   ")))
    (is (nil? (query/simple->cqp "" {:prefix? true})))))

(deftest escape-value-test
  (testing "the control characters a command line cannot carry become
            regex escapes"
    (is (= "a\\tb\\nc\\rd" (query/escape-value "a\tb\nc\rd"))))
  (testing "over the literal escaping"
    (is (= "a\\.b\"\"" (query/escape-value "a.b\"")))))

(deftest list-query-test
  (testing "a list of words is one token pattern matching any of them"
    (is (= "[word = \"(hund|kat)\"]"
           (query/simple->cqp "hund\nkat" {:list? true})))
    (is (= "[lemma = \"(hund|kat).*\" %c]"
           (query/simple->cqp "hund\r\n\n kat \nhund"
                              {:list?             true
                               :prefix?           true
                               :case-insensitive? true
                               :attr              :lemma}))))
  (testing "the words are matched literally"
    (is (= "[word = \"(a\\.b|c\\|d)\"]"
           (query/simple->cqp "a.b\nc|d" {:list? true}))))
  (testing "several words to a line are several words: a token holds no
            space, and a box without line breaks lays a list out so"
    (is (= "[word = \"(hund|kat)\"]"
           (query/simple->cqp "hund kat" {:list? true}))))
  (is (nil? (query/simple->cqp "\n \n" {:list? true}))))

(deftest ->cqp-test
  (testing "CQP mode passes the query through verbatim"
    (is (= "[lemma = \"hund\"]"
           (query/->cqp {:q "[lemma = \"hund\"]" :mode "cqp"}))))
  (testing "simple mode compiles the query"
    (is (= "[word = \"hund.*\" %c]"
           (query/->cqp {:q "hund" :mode "simple" :match "prefix" :ci "on"}))))
  (testing "the in param names the attribute matched, word when blank"
    (is (= "[lemma = \"hund\"]" (query/->cqp {:q "hund" :in "lemma"})))
    (is (= "[word = \"hund\"]" (query/->cqp {:q "hund" :in ""}))))
  (testing "blank input yields nil"
    (is (nil? (query/->cqp {:q "   " :mode "cqp"})))
    (is (nil? (query/->cqp {:mode "simple"}))))
  (testing "simple is the default: a bare word is a word search, not CQP"
    (is (= "[word = \"hund\"]" (query/->cqp {:q "hund"})))
    (is (= "[word = \"hund\"]" (query/->cqp {:q "hund" :mode ""})))
    (testing "and its options still apply"
      (is (= "[word = \"hund.*\" %c]"
             (query/->cqp {:q "hund" :match "prefix" :ci "on"}))))))

(deftest match-param-test
  (testing "one param says how much of the form the query must cover"
    (is (= {} (query/match-param nil)))
    (is (= {} (query/match-param "")))
    (is (= {:prefix? true} (query/match-param "prefix")))
    (is (= {:suffix? true} (query/match-param "suffix")))
    (is (= {:prefix? true :suffix? true} (query/match-param "infix"))))
  (testing "so a query may fall anywhere in the form without two params
            both set having to mean that"
    (is (= "[word = \".*hund.*\"]" (query/->cqp {:q "hund" :match "infix"})))
    (is (= "[word = \".*hund\"]" (query/->cqp {:q "hund" :match "suffix"})))))

(deftest within-unit-test
  (testing "a simple search of several words is kept within a sentence"
    (is (= :sentence (query/within-unit {:q "lille hund"})))
    (is (= :sentence (query/within-unit {:q " lille  hund " :mode "simple"}))))
  (testing "one word cannot straddle a boundary"
    (is (nil? (query/within-unit {:q "hund"})))
    (is (nil? (query/within-unit {:q "  "})))
    (is (nil? (query/within-unit {}))))
  (testing "CQP says so itself"
    (is (nil? (query/within-unit {:q "[] []" :mode "cqp"})))))

(deftest token-params-test
  (testing "the tokens compile to CQP, defaults applied and bad values read
            as them"
    (is (= [{:conditions [{:attr :lemma :op "is" :value "hund" :ci? true}]
             :min 1 :max 1 :start? false :end? false}
            {:conditions [{:attr :word :op "any" :value "" :ci? false}]
             :min 0 :max 2 :start? false :end? true}]
           (query/token-params {:t1.attr "lemma" :t1.v "hund" :t1.ci "on"
                              :t2.op   "any" :t2.min "0" :t2.max "2"
                              :t2.end  "on"
                              :t3.attr "pos" :t3.op "prefix"})))
    (is (= [{:conditions [{:attr :word :op "is" :value "x" :ci? false}
                          {:attr :pos :op "is" :value "N" :ci? false
                           :join "and"}]
             :min 2 :max 2 :start? false :end? false}]
           (query/token-params {:t1.v "x" :t1.attr "no such" :t1.op "nope"
                              :t1.min "2" :t1.max "1"
                              :t1.2.attr "pos" :t1.2.v "N" :t1.2.join "nor"
                              :t1.3.attr "pos" :t1.3.join "or"})))
    (is (= "[lemma = \"hund\" %c] []{0,2} </s>"
           (query/->cqp {:mode "extended" :t1.attr "lemma" :t1.v "hund"
                       :t1.ci "on" :t2.op "any" :t2.min "0" :t2.max "2"
                       :t2.end "on"})))
    (is (nil? (query/->cqp {:mode "extended" :q "hund"}))))
  (testing "several tokens are kept within the unit the params name, the
            sentence by default; one is not, unless it opens or closes a
            sentence"
    (is (= :sentence
           (query/within-unit {:mode "extended" :t1.v "a" :t2.v "b"})))
    (is (= :paragraph (query/within-unit {:mode "extended" :t1.v "a" :t2.v "b"
                                        :within "paragraph"})))
    (is (= :paragraph (query/within-unit {:q "a b" :within "paragraph"})))
    (is (= :sentence (query/within-unit {:q "a b" :within "nonesuch"})))
    (is (nil? (query/within-unit {:mode "extended" :t1.v "a"})))
    (is (= :sentence (query/within-unit {:mode "extended" :t1.v "a"
                                       :t1.start "on"})))))

(deftest compiled-golden-test
  (testing "the CQP and the unit each query compiles to today, pinned so
            that one compiler can take the place of two without a change
            of text"
    (doseq [[params cqp within]
            [[{:q "hund"} "[word = \"hund\"]" nil]
             [{:q "lille hund"}
              "[word = \"lille\"] [word = \"hund\"]" :sentence]
             [{:q "lille hund" :within "paragraph"}
              "[word = \"lille\"] [word = \"hund\"]" :paragraph]
             [{:q "hund" :in "lemma" :ci "on"} "[lemma = \"hund\" %c]" nil]
             [{:q "hund" :match "prefix"} "[word = \"hund.*\"]" nil]
             [{:q "hund" :match "suffix"} "[word = \".*hund\"]" nil]
             [{:q "hund" :match "infix" :ci "on" :in "lemma"}
              "[lemma = \".*hund.*\" %c]" nil]
             [{:q "a.b \"c\""}
              "[word = \"a\\.b\"] [word = \"\"\"c\"\"\"]" :sentence]
             [{:q "hund\nkat" :mode "list"} "[word = \"(hund|kat)\"]" nil]
             [{:q "hund\nkat" :mode "list" :match "prefix" :ci "on" :in "lemma"}
              "[lemma = \"(hund|kat).*\" %c]" nil]
             ;; TODO: the one compiler drops the parentheses of a list of
             ;; one word; this row pins today's text so that the change is
             ;; made on purpose
             [{:q "hund" :mode "list"} "[word = \"(hund)\"]" nil]
             [{:mode "extended" :t1.attr "lemma" :t1.v "hund"}
              "[lemma = \"hund\"]" nil]
             [{:mode "extended" :t1.v "lille" :t2.v "hund" :within "text"}
              "[word = \"lille\"] [word = \"hund\"]" :text]
             [{:mode "extended" :t1.v "hund" :t1.2.v "kat" :t1.2.join "or"}
              "[word = \"hund\" | word = \"kat\"]" nil]
             [{:mode "extended" :t1.v "hund" :t1.2.v "kat" :t1.2.join "or"
               :t1.3.attr "pos" :t1.3.op "prefix" :t1.3.v "N"
               :t1.3.join "and"}
              "[(word = \"hund\" | word = \"kat\") & pos = \"N.*\"]" nil]
             [{:mode "extended" :t1.op "any" :t1.min "0" :t1.max "2"
               :t1.end "on"}
              "[]{0,2} </s>" :sentence]
             [{:mode "extended" :t1.attr "pos" :t1.op "regex" :t1.v "N.*|V.*"
               :t1.ci "on"}
              "[pos = \"N.*|V.*\" %c]" nil]
             [{:mode "extended" :t1.op "not" :t1.v "hund" :t2.op "any"
               :t2.start "on"}
              "[word != \"hund\"] <s> []" :sentence]
             [{:q "[lemma = \"hund\"] within s" :mode "cqp"}
              "[lemma = \"hund\"] within s" nil]
             ;; a bare word under CQP is what CQP is given, and refuses
             [{:q "hund" :mode "cqp" :in "lemma"} "hund" nil]]]
      (is (= cqp (query/->cqp params)) (pr-str params))
      (is (= within (query/within-unit params)) (pr-str params)))))
