(ns dk.cst.corpus-probe.query.params-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.query.params :as params]))

(deftest match-op-test
  (testing "one param says how much of the form the query must cover,
            the whole of it unless it names a part"
    (is (= "is" (params/match-op nil)))
    (is (= "is" (params/match-op "")))
    (is (= "is" (params/match-op "nonesuch")))
    (is (= "prefix" (params/match-op "prefix")))
    (is (= "suffix" (params/match-op "suffix")))
    (is (= "infix" (params/match-op "infix")))))

(deftest repeat-param-test
  (is (= 0 (params/repeat-param "0" 1)))
  (is (= 99 (params/repeat-param "99" 1)))
  (testing "anything else is the default"
    (is (= 1 (params/repeat-param "100" 1)))
    (is (= 2 (params/repeat-param "-1" 2)))
    (is (= 1 (params/repeat-param nil 1)))
    (is (= 1 (params/repeat-param "x" 1)))))

(deftest within-param-test
  (testing "the unit named; the sentence for none, or one the app lacks"
    (is (= :paragraph (params/within-param "paragraph")))
    (is (= :text (params/within-param "text")))
    (is (= :sentence (params/within-param nil)))
    (is (= :sentence (params/within-param "nonesuch")))))

(deftest token-params-test
  (testing "the tokens compile to CQP, defaults applied and bad values read
            as them"
    (is (= [{:conditions [{:attr :lemma :op "is" :value "hund" :ci? true}]
             :min 1 :max 1 :start? false :end? false}
            {:conditions [{:attr :word :op "any" :value "" :ci? false}]
             :min 0 :max 2 :start? false :end? true}]
           (params/token-params {:t1.attr "lemma" :t1.v "hund" :t1.ci "on"
                                 :t2.op   "any" :t2.min "0" :t2.max "2"
                                 :t2.end  "on"
                                 :t3.attr "pos" :t3.op "prefix"})))
    (is (= [{:conditions [{:attr :word :op "is" :value "x" :ci? false}
                          {:attr :pos :op "is" :value "N" :ci? false
                           :join "and"}]
             :min 2 :max 2 :start? false :end? false}]
           (params/token-params {:t1.v "x" :t1.attr "no such" :t1.op "nope"
                                 :t1.min "2" :t1.max "1"
                                 :t1.2.attr "pos" :t1.2.v "N" :t1.2.join "nor"
                                 :t1.3.attr "pos" :t1.3.join "or"})))))

(deftest token->params-test
  (testing "a token prints as the fields its form submits, nothing at its
            default, and reads back as itself"
    (let [token {:conditions [{:attr :lemma :op "is" :value "hund" :ci? true}
                              {:attr :word :op "prefix" :value "kat"
                               :ci? false :join "or"}]
                 :min 1 :max 3 :start? false :end? true}]
      (is (= {:t2.attr "lemma" :t2.v "hund" :t2.ci "on"
              :t2.2.op "prefix" :t2.2.v "kat" :t2.2.join "or"
              :t2.max "3" :t2.end "on"}
             (params/token->params 2 token)))
      (is (= [token] (params/token-params (params/token->params 1 token)))))))

(deftest word-params-test
  (testing "the words in the field with the options they share, one line
            each for a list, which is kept within nothing"
    (is (= {:q "lille hund" :in "lemma" :match "prefix" :ci "on"
            :within "text"}
           (params/word-params
            "simple"
            {:tokens [{:conditions [{:attr :lemma :op "prefix" :value "lille"
                                     :ci? true}]}
                      {:conditions [{:attr :lemma :op "prefix" :value "hund"
                                     :ci? true}]}]
             :within :text})))
    (is (= {:q "hund\nkat"}
           (params/word-params
            "list"
            {:tokens [{:conditions [{:attr :word :op "is" :value "hund"}
                                    {:attr :word :op "is" :value "kat"
                                     :join "or"}]}]
             :within :text})))))
