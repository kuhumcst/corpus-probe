(ns dk.cst.corpus-probe.query-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.query :as query]))

(defn cqp-of
  "The CQP the search `params` compile to."
  [params]
  (query/->cqp (query/of params)))

(defn within-of
  "The unit of text the search `params` are kept within."
  [params]
  (query/within (query/of params)))

(deftest escape-literal-test
  (testing "PCRE metacharacters are backslash-escaped"
    (is (= "a\\.b\\*c" (query/escape-literal "a.b*c")))
    (is (= "\\[\\]\\{\\}\\(\\)\\^\\$\\|\\?\\+\\\\"
           (query/escape-literal "[]{}()^$|?+\\"))))
  (testing "quotes are doubled, CQP-style"
    (is (= "12\"\"-screen" (query/escape-literal "12\"-screen"))))
  (testing "ordinary text passes through"
    (is (= "København" (query/escape-literal "København")))))

(deftest escape-value-test
  (testing "the control characters a command line cannot carry become
            regex escapes"
    (is (= "a\\tb\\nc\\rd" (query/escape-value "a\tb\nc\rd"))))
  (testing "over the literal escaping"
    (is (= "a\\.b\"\"" (query/escape-value "a.b\"")))))

(deftest extended->cqp-test
  (testing "each operator against the attribute, the literal escaped"
    (is (= "word = \"hund\"" (query/condition->cqp {:value "hund"})))
    (is (= "lemma != \"hund\""
           (query/condition->cqp {:attr :lemma :op "not" :value "hund"})))
    (is (= "word = \"hu\\.\\*.*\""
           (query/condition->cqp {:op "prefix" :value "hu.*"})))
    (is (= "word = \".*nd\""
           (query/condition->cqp {:op "suffix" :value "nd"})))
    (is (= "word = \".*un.*\""
           (query/condition->cqp {:op "infix" :value "un"})))
    (is (= "pos = \"N\" %c"
           (query/condition->cqp {:attr :pos :op "is" :value "N" :ci? true}))))
  (testing "a regex is kept as written, but for the quotes it cannot hold"
    (is (= "pos = \"N.*|V.*\""
           (query/condition->cqp {:attr :pos :op "regex" :value "N.*|V.*"})))
    (is (= "word != \"a\"\"b\""
           (query/condition->cqp {:op "not-regex" :value "a\"b"})))
    (is (= "word = \"a b\""
           (query/condition->cqp {:op "regex" :value "a\nb"}))))
  (testing "a token's conditions, grouped as KORP reads them: ors within
            a group, ands between groups"
    (is (= "[word = \"hund\"]"
           (query/token->cqp {:conditions [{:value "hund"}]})))
    (is (= "[lemma = \"hund\" & pos = \"N.*\"]"
           (query/token->cqp {:conditions [{:attr :lemma :value "hund"}
                                           {:attr :pos :op "prefix" :value "N"
                                            :join "and"}]})))
    (testing "alternatives of one literal value are one alternation"
      (is (= "[lemma = \"(hund|kat)\"]"
             (query/token->cqp {:conditions [{:attr :lemma :value "hund"}
                                             {:attr :lemma :value "kat"
                                              :join "or"}]})))
      (is (= "[lemma = \"(hund|kat)\" & pos = \"N.*\"]"
             (query/token->cqp {:conditions [{:attr :lemma :value "hund"}
                                             {:attr :lemma :value "kat"
                                              :join "or"}
                                             {:attr :pos :op "prefix" :value "N"
                                              :join "and"}]})))
      (is (= "[lemma = \"(hund|kat).*\" %c]"
             (query/token->cqp {:conditions [{:attr :lemma :op "prefix"
                                              :value "hund" :ci? true}
                                             {:attr :lemma :op "prefix"
                                              :value "kat" :ci? true
                                              :join "or"}]}))))
    (testing "not so alternatives of two shapes, or of a negation, whose
              alternation would mean something else"
      (is (= "[(word = \"a\" | lemma = \"b\") & word = \"c\"]"
             (query/token->cqp {:conditions [{:value "a"}
                                             {:attr :lemma :value "b"
                                              :join "or"}
                                             {:value "c" :join "and"}]})))
      (is (= "[word != \"a\" | word != \"b\"]"
             (query/token->cqp {:conditions [{:op "not" :value "a"}
                                             {:op "not" :value "b"
                                              :join "or"}]})))))
  (testing "an any token, repeats that are not once, and the sentence edges"
    (is (= "[]" (query/token->cqp {:conditions [{:op "any"}]})))
    (is (= "[]{0,2}"
           (query/token->cqp {:conditions [{:op "any"}] :min 0 :max 2})))
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

(deftest match-op-test
  (testing "one param says how much of the form the query must cover"
    (is (= "is" (query/match-op nil)))
    (is (= "is" (query/match-op "")))
    (is (= "prefix" (query/match-op "prefix")))
    (is (= "suffix" (query/match-op "suffix")))
    (is (= "infix" (query/match-op "infix"))))
  (testing "so a query may fall anywhere in the form without two params
            both set having to mean that"
    (is (= "[word = \".*hund.*\"]" (cqp-of {:q "hund" :match "infix"})))
    (is (= "[word = \".*hund\"]" (cqp-of {:q "hund" :match "suffix"})))))

(deftest of-test
  (testing "words in order, one token each, with the options every word
            shares, kept within the unit named"
    (is (= {:tokens [{:conditions [{:attr :lemma :op "prefix" :value "lille"
                                    :ci? true}]
                      :min 1 :max 1 :start? false :end? false}
                     {:conditions [{:attr :lemma :op "prefix" :value "hund"
                                    :ci? true}]
                      :min 1 :max 1 :start? false :end? false}]
            :within :text}
           (query/of {:q " lille  hund " :in "lemma" :ci "on" :match "prefix"
                      :within "text"}))))
  (testing "a list is one token of alternatives, however it is laid out,
            each word once, within nothing the mode reads"
    (is (= {:tokens [{:conditions [{:attr :word :op "is" :value "hund"
                                    :ci? false}
                                   {:attr :word :op "is" :value "kat"
                                    :ci? false :join "or"}]
                      :min 1 :max 1 :start? false :end? false}]
            :within :sentence}
           (query/of {:q "hund\nkat hund" :mode "list" :within "text"}))))
  (testing "the tokens of an extended search, as its params say"
    (is (= {:tokens [{:conditions [{:attr :lemma :op "is" :value "hund"
                                    :ci? true}]
                      :min 1 :max 1 :start? false :end? false}
                     {:conditions [{:attr :word :op "any" :value ""
                                    :ci? false}]
                      :min 0 :max 2 :start? false :end? true}]
            :within :paragraph}
           (query/of {:mode "extended" :t1.attr "lemma" :t1.v "hund"
                      :t1.ci "on" :t2.op "any" :t2.min "0" :t2.max "2"
                      :t2.end "on" :within "paragraph"}))))
  (testing "CQP as typed"
    (is (= {:cqp "[lemma = \"hund\"] within s"}
           (query/of {:q "[lemma = \"hund\"] within s" :mode "cqp"
                      :in "lemma"}))))
  (testing "nothing asked is no query: a blank, or the query of another
            mode, which the mode does not read"
    (is (nil? (query/of {})))
    (is (nil? (query/of {:q "  " :mode "cqp"})))
    (is (nil? (query/of {:q "hund" :mode "extended"})))
    (is (nil? (query/of {:t1.v "hund" :mode "simple"}))))
  (testing "simple is the default: a bare word is a word search, not CQP,
            and so is a mode the app does not know"
    (is (= (query/of {:q "hund"})
           (query/of {:q "hund" :mode ""})
           (query/of {:q "hund" :mode "nonesuch"})))))

(deftest cqp-test
  (is (= "[word = \"hund\"]" (cqp-of {:q "hund"})))
  (is (= "[word = \"lille\" %c] [word = \"hund\" %c]"
         (cqp-of {:q " lille  hund " :ci "on"})))
  (is (= "[word = \"hund.*\"]" (cqp-of {:q "hund" :match "prefix"})))
  (is (= "[word = \".*hund.*\"]" (cqp-of {:q "hund" :match "infix"})))
  (testing "any positional attribute can be the one matched, word when blank"
    (is (= "[lemma = \"hund\"]" (cqp-of {:q "hund" :in "lemma"})))
    (is (= "[word = \"hund\"]" (cqp-of {:q "hund" :in ""})))
    (is (= "[lemma = \"hund.*\" %c]"
           (cqp-of {:q "hund" :in "lemma" :match "prefix" :ci "on"}))))
  (testing "regex metacharacters in input are matched literally"
    (is (= "[word = \"hund\\.\"]" (cqp-of {:q "hund."}))))
  (testing "CQP mode passes the query through verbatim"
    (is (= "[lemma = \"hund\"]"
           (cqp-of {:q "[lemma = \"hund\"]" :mode "cqp"}))))
  (testing "blank input yields nil, not a match-everything query"
    (is (nil? (cqp-of {:q "   "})))
    (is (nil? (cqp-of {:q "" :match "prefix"})))
    (is (nil? (cqp-of {:q "   " :mode "cqp"})))
    (is (nil? (cqp-of {:mode "simple"}))))
  (testing "a list of words is one token pattern matching any of them"
    (is (= "[word = \"(hund|kat)\"]" (cqp-of {:q "hund\nkat" :mode "list"})))
    (is (= "[lemma = \"(hund|kat).*\" %c]"
           (cqp-of {:q "hund\r\n\n kat \nhund" :mode "list" :match "prefix"
                    :ci "on" :in "lemma"})))
    (testing "the words are matched literally"
      (is (= "[word = \"(a\\.b|c\\|d)\"]"
             (cqp-of {:q "a.b\nc|d" :mode "list"}))))
    (testing "several words to a line are several words: a token holds no
              space, and a box without line breaks lays a list out so"
      (is (= "[word = \"(hund|kat)\"]" (cqp-of {:q "hund kat" :mode "list"}))))
    (testing "a list of one word is that word"
      (is (= "[word = \"hund\"]" (cqp-of {:q "hund" :mode "list"}))))
    (is (nil? (cqp-of {:q "\n \n" :mode "list"})))))

(deftest within-test
  (testing "a simple search of several words is kept within a sentence,
            or the unit named"
    (is (= :sentence (within-of {:q "lille hund"})))
    (is (= :sentence (within-of {:q " lille  hund " :mode "simple"})))
    (is (= :paragraph (within-of {:q "a b" :within "paragraph"})))
    (is (= :sentence (within-of {:q "a b" :within "nonesuch"}))))
  (testing "one word cannot straddle a boundary, nor can a list, which is
            one token"
    (is (nil? (within-of {:q "hund"})))
    (is (nil? (within-of {:q "  "})))
    (is (nil? (within-of {})))
    (is (nil? (within-of {:q "hund\nkat" :mode "list" :within "text"}))))
  (testing "several tokens are kept within the unit the params name, the
            sentence by default; one is not, unless it opens or closes a
            sentence"
    (is (= :sentence (within-of {:mode "extended" :t1.v "a" :t2.v "b"})))
    (is (= :paragraph (within-of {:mode "extended" :t1.v "a" :t2.v "b"
                                  :within "paragraph"})))
    (is (nil? (within-of {:mode "extended" :t1.v "a"})))
    (is (= :sentence (within-of {:mode "extended" :t1.v "a"
                                 :t1.start "on"}))))
  (testing "CQP says so itself"
    (is (nil? (within-of {:q "[] []" :mode "cqp"})))))

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
           (cqp-of {:mode "extended" :t1.attr "lemma" :t1.v "hund"
                    :t1.ci "on" :t2.op "any" :t2.min "0" :t2.max "2"
                    :t2.end "on"})))
    (is (nil? (cqp-of {:mode "extended" :q "hund"})))))

(deftest compiled-golden-test
  (testing "the CQP and the unit each query compiles to, pinned"
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
             [{:q "hund" :mode "list"} "[word = \"hund\"]" nil]
             [{:mode "extended" :t1.attr "lemma" :t1.v "hund"}
              "[lemma = \"hund\"]" nil]
             [{:mode "extended" :t1.v "lille" :t2.v "hund" :within "text"}
              "[word = \"lille\"] [word = \"hund\"]" :text]
             [{:mode "extended" :t1.v "hund" :t1.2.v "kat" :t1.2.join "or"}
              "[word = \"(hund|kat)\"]" nil]
             [{:mode "extended" :t1.v "hund" :t1.2.v "kat" :t1.2.join "or"
               :t1.3.attr "pos" :t1.3.op "prefix" :t1.3.v "N"
               :t1.3.join "and"}
              "[word = \"(hund|kat)\" & pos = \"N.*\"]" nil]
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
      (is (= cqp (cqp-of params)) (pr-str params))
      (is (= within (within-of params)) (pr-str params)))))

(deftest params-test
  (testing "the params of a mode read back as the query they carry, and
            print again as themselves, nothing at its default"
    (doseq [[mode params]
            [["simple" {:q "lille hund" :in "lemma" :ci "on" :match "prefix"
                        :within "text"}]
             ["simple" {:q "hund"}]
             ["list" {:q "hund\nkat" :mode "list" :match "suffix"}]
             ["extended" {:mode "extended" :t1.v "hund" :t1.2.v "kat"
                          :t1.2.join "or" :t1.3.attr "pos" :t1.3.op "prefix"
                          :t1.3.v "N" :t2.op "any" :t2.min "0" :t2.max "2"
                          :t2.end "on" :within "paragraph"}]
             ["extended" {:mode "extended" :t1.v "a" :t1.2.v "b" :t1.min "2"
                          :t1.max "2" :t1.ci "on"}]
             ["cqp" {:q "[] []" :mode "cqp"}]]]
      (let [query (query/of params)]
        (is (= params (query/params mode query)) mode)
        (is (= query (query/of (query/params mode query))) mode))))
  (testing "no query prints as no params, the mode aside"
    (is (= {} (query/params "simple" nil)))
    (is (= {:mode "list"} (query/params "list" nil)))))

(deftest project-test
  (let [extended (query/of {:mode "extended" :t1.attr "lemma" :t1.v "hund"
                            :t1.2.attr "pos" :t1.2.v "N" :t1.2.join "and"
                            :t2.op "any" :t2.max "3" :t3.v "kat" :t3.min "2"
                            :t3.max "2" :t3.end "on" :within "paragraph"})
        simple   (query/of {:q "lille hund" :in "lemma" :ci "on"})
        list     (query/of {:q "hund\nkat" :mode "list" :in "lemma"})
        cqp      (query/of {:q "[lemma = \"hund\"] within s" :mode "cqp"})
        held-as  (fn [mode query]
                   (query/params mode (query/project mode query)))]
    (testing "a form holds its own query whole, and the extended form every
              query of tokens"
      (doseq [[mode query] [["simple" simple] ["list" list]
                            ["extended" extended] ["cqp" cqp]
                            ["extended" simple] ["extended" list]]]
        (is (empty? (query/loss mode query)) mode)
        (is (= query (query/project mode query)) mode)))
    (testing "the CQP form holds every query of tokens as its CQP, kept
              within its unit"
      (is (= {:cqp (str "[lemma = \"hund\" & pos = \"N\"] []{1,3} "
                        "[word = \"kat\"]{2,2} </s> within p")}
             (query/project "cqp" extended)))
      (is (= {:cqp "[lemma = \"lille\" %c] [lemma = \"hund\" %c] within s"}
             (query/project "cqp" simple)))
      (is (= {:cqp "[lemma = \"(hund|kat)\"]"} (query/project "cqp" list))))
    (testing "the simple form and the list form hold each other's words,
              the one in order, the other as any of them"
      (is (= [[:order]] (query/loss "list" simple)))
      (is (= {:q "lille\nhund" :mode "list" :in "lemma" :ci "on"}
             (held-as "list" simple)))
      (is (= [[:any]] (query/loss "simple" list)))
      (is (= {:q "hund kat" :in "lemma"} (held-as "simple" list)))
      (testing "a word being a list of one, and a list of one a word"
        (is (empty? (query/loss "list" (query/of {:q "hund"}))))
        (is (empty? (query/loss "simple"
                                (query/of {:q "hund" :mode "list"}))))))
    (testing "of an extended search they hold the first word of each token,
              matched as the first word is, and say what they dropped"
      (is (= [[:condition 1 2] [:any-word 2] [:repeat 2]
              [:options 3] [:repeat 3] [:edge 3]]
             (query/loss "simple" extended)))
      (is (= {:q "hund kat" :in "lemma" :within "paragraph"}
             (held-as "simple" extended)))
      (is (= [[:order] [:condition 1 2] [:any-word 2] [:repeat 2]
              [:options 3] [:repeat 3] [:edge 3]]
             (query/loss "list" extended)))
      (is (= {:q "hund\nkat" :mode "list" :in "lemma"}
             (held-as "list" extended)))
      (testing "an operator no word has is an option they lack, and a value
                with a space in it no word"
        (let [regex (query/of {:mode "extended" :t1.op "regex" :t1.v "h.nd"})
              space (query/of {:mode "extended" :t1.v "lille hund"
                               :t2.v "kat"})]
          (is (= [[:options 1]] (query/loss "simple" regex)))
          (is (nil? (query/project "simple" regex)))
          (is (= [[:value 1]] (query/loss "simple" space)))
          (is (= {:q "kat"} (held-as "simple" space))))))
    (testing "they read CQP as words, which the reader is told"
      (is (= [[:reading]] (query/loss "simple" cqp)))
      (is (= {:q "[lemma = \"hund\"] within s"} (held-as "simple" cqp)))
      (is (= [[:reading]] (query/loss "list" cqp)))
      (is (= {:q "[lemma\n=\n\"hund\"]\nwithin\ns" :mode "list"}
             (held-as "list" cqp))))
    (testing "the extended form reads no CQP, and starts blank; nor a list
              past the cap"
      (is (= [[:cqp "[lemma = \"hund\"] within s"]]
             (query/loss "extended" cqp)))
      (is (nil? (query/project "extended" cqp)))
      (let [n    (inc query/max-alternatives)
            long {:tokens [(query/token
                            (map #(hash-map :attr :word :op "is"
                                            :value (str %) :ci? false)
                                 (range n)))]
                  :within :sentence}]
        (is (= [[:list n]] (query/loss "extended" long)))
        (is (nil? (query/project "extended" long)))))
    (testing "what a form holds reads back as itself"
      (doseq [mode  ["simple" "list" "extended" "cqp"]
              query [extended simple list cqp]]
        (let [held (query/project mode query)]
          (is (= held (query/of (query/params mode held)))
              [mode (query/->cqp query)]))))
    (testing "no query is held by no form, and loses nothing"
      (is (nil? (query/project "simple" nil)))
      (is (empty? (query/loss "cqp" nil))))))

(deftest form-rows-test
  (testing "the tokens as the form shows them, then one blank, numbered
            afresh"
    (is (= [{:id 1 :conditions [{:id 1 :v "a"} {:id 2 :v "c" :join "or"}]}
            {:id 2 :max "2" :conditions [{:id 1 :op "any"}]}
            {:id 3 :conditions [{:id 1}]}]
           (query/form-rows
            (query/of {:mode "extended" :t1.v "a" :t1.2.attr "pos"
                       :t1.2.join "and" :t1.3.v "c" :t1.3.join "or"
                       :t2.ci "on" :t5.op "any" :t5.max "2"})))))
  (testing "a query of words as tokens, each with its options"
    (is (= [{:id 1 :conditions [{:id 1 :attr "lemma" :v "lille" :ci "on"}]}
            {:id 2 :conditions [{:id 1 :attr "lemma" :v "hund" :ci "on"}]}
            {:id 3 :conditions [{:id 1}]}]
           (query/form-rows (query/of {:q "lille hund" :in "lemma"
                                       :ci "on"})))))
  (testing "the blank alone for no query"
    (is (= [{:id 1 :conditions [{:id 1}]}] (query/form-rows nil)))))

(deftest arrived-test
  (let [arrived (fn [params]
                  (select-keys (query/arrived params) [:form :loss :unread]))
        runs?   (fn [params] (some? (:query (query/arrived params))))
        held-as (fn [params]
                  (let [{:keys [form held]} (query/arrived params)]
                    (query/params form held)))]
    (testing "no switch: the form holds what its mode reads and runs it,
              naming what a hand-written URL carried that it does not read"
      (is (= {:form "simple" :loss [] :unread #{}} (arrived {:q "hund"})))
      (is (runs? {:q "hund"}))
      (is (= {:form "cqp" :loss [] :unread #{:in :ci}}
             (arrived {:q "[]" :mode "cqp" :in "lemma" :ci "on"})))
      (is (= {:form "simple" :loss [] :unread #{:t1.v}}
             (arrived {:q "hund" :t1.v "kat"})))
      (testing "and nothing when the field is blank"
        (is (= {:form "simple" :loss [] :unread #{}}
               (arrived {:q "" :from "simple"})))
        (is (not (runs? {:q "" :from "simple"})))))
    (testing "into the tokens: the field read as the mode it was typed in
              seeds them and runs as them, unless it was CQP"
      (is (= {:mode "extended" :t1.attr "lemma" :t1.v "lille"
              :t2.attr "lemma" :t2.v "hund"}
             (held-as {:q "lille hund" :in "lemma" :mode "extended"
                       :from "simple"})))
      (is (runs? {:q "lille hund" :in "lemma" :mode "extended"
                  :from "simple"}))
      (is (= {:mode "extended" :t1.v "hund" :t1.2.v "kat" :t1.2.join "or"}
             (held-as {:q "hund\nkat" :mode "extended" :from "list"})))
      (is (= {:mode "extended" :t1.v "hund" :t2.v "kat"}
             (held-as {:q "hund kat" :mode "extended"})))
      (let [params {:q "[lemma = \"x\"]" :mode "extended" :from "cqp"}]
        (is (= {:form "extended" :loss [[:cqp "[lemma = \"x\"]"]] :unread #{}}
               (arrived params)))
        (is (nil? (:held (query/arrived params))))
        (is (not (runs? params)))))
    (testing "out of the tokens: projected into the field, run when the
              field holds them whole, held back when part of them was lost"
      (let [tokens {:t1.attr "lemma" :t1.v "hund" :t2.op "any" :t2.max "3"
                    :from "extended"}]
        (is (= {:q "hund" :in "lemma"} (held-as (assoc tokens :mode "simple"))))
        (is (= [[:any-word 2] [:repeat 2]]
               (:loss (query/arrived (assoc tokens :mode "simple")))))
        (is (not (runs? (assoc tokens :mode "simple"))))
        (is (= {:mode "cqp" :q "[lemma = \"hund\"] []{1,3} within s"}
               (held-as (assoc tokens :mode "cqp"))))
        (is (runs? (assoc tokens :mode "cqp"))))
      (is (runs? {:t1.v "hund" :from "extended" :mode "simple"}))
      (is (= {:q "hund"} (held-as {:t1.v "hund" :from "extended"
                                   :mode "simple"}))))
    (testing "between the modes that share the field: the ticked one reads
              it and runs, and the change of reading is said"
      (is (= [[:order]]
             (:loss (query/arrived {:q "lille hund" :mode "list"
                                    :from "simple"}))))
      (is (= {:q "lille\nhund" :mode "list"}
             (held-as {:q "lille hund" :mode "list" :from "simple"})))
      (is (runs? {:q "lille hund" :mode "list" :from "simple"}))
      (is (= [[:any]]
             (:loss (query/arrived {:q "hund\nkat" :mode "simple"
                                    :from "list"}))))
      (is (= {:q "hund kat"}
             (held-as {:q "hund\nkat" :mode "simple" :from "list"})))
      (is (= [[:reading]]
             (:loss (query/arrived {:q "[] []" :mode "simple" :from "cqp"}))))
      (is (runs? {:q "[] []" :mode "simple" :from "cqp"}))
      (testing "and CQP reads the text as typed, whatever it was"
        (let [params {:q "hund" :mode "cqp" :from "simple" :in "lemma"}]
          (is (= {:form "cqp" :loss [] :unread #{}} (arrived params)))
          (is (= {:mode "cqp" :q "hund"} (held-as params))))))
    (testing "a from naming the mode ticked, or none the app knows, is no
              switch"
      (is (= {:form "simple" :loss [] :unread #{:t1.v}}
             (arrived {:q "hund" :t1.v "kat" :from "simple"})))
      (is (= {:form "simple" :loss [] :unread #{}}
             (arrived {:q "hund" :from "nonesuch"}))))))
