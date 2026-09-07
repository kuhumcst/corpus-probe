(ns dk.cst.corpus-probe.query.mode-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.query.mode :as mode]))

(deftest modes-test
  (testing "every mode has its row of fields, and no row lacks its mode"
    (is (= (set mode/modes) (set (keys mode/fields)))))
  (testing "the first mode is the default, and every query key's default
            is a key some mode reads"
    (is (= (first mode/modes) (mode/mode {})))
    (is (every? mode/query-key? (keys mode/defaults))))
  (testing "a submitted form's radio names the form; the text says its
            mode by its shape, tokens before text; and nothing is simple"
    (is (= "simple" (mode/mode {})))
    (is (= "simple" (mode/mode {:mode "nonesuch"})))
    (is (= "simple" (mode/mode {:mode "cqp"})))
    (is (= "cqp" (mode/mode {:mode "simple" :q "[]"})))
    (is (= "simple" (mode/mode {:q "x"})))
    (is (= "list" (mode/mode {:q "x\ny"})))
    (is (= "cqp" (mode/mode {:q "\"x\""})))
    (is (= "extended" (mode/mode {:t1.v "x"})))
    (is (= "extended" (mode/mode {:t1.v "x" :q "y"})))
    (is (= "extended" (mode/mode {:mode "extended" :q "y"})))
    (is (= "simple" (mode/mode {:mode "simple" :t1.v "x"})))
    (is (nil? (mode/typed {:in "lemma"})))
    (is (= "cqp" (mode/typed {:q "[]" :mode "extended"})))
    (is (= "extended" (mode/typed {:t1.v "x" :mode "simple"}))))
  (testing "the shape of the field's text: CQP by its first character, a
            list by a line break, words otherwise"
    (is (= "simple" (mode/shape nil)))
    (is (= "simple" (mode/shape "")))
    (is (= "simple" (mode/shape " \n\r\n ")))
    (is (= "simple" (mode/shape "lille hund")))
    (is (= "list" (mode/shape "hund\nkat")))
    (is (= "list" (mode/shape "hund\r\nkat")))
    (is (= "cqp" (mode/shape "[lemma = \"hund\"]")))
    (is (= "cqp" (mode/shape "  \"hund\" \"kat\"")))
    (is (= "cqp" (mode/shape "'hund'")))
    (is (= "cqp" (mode/shape "<s> []")))
    (is (= "cqp" (mode/shape "(\"a\" \"b\")+")))
    (is (= "cqp" (mode/shape "@[pos = \"N\"]")))
    (is (= "cqp" (mode/shape "MU(meet \"a\" \"b\" 1 2)")))
    (is (= "cqp" (mode/shape "[lemma = \"hund\"]\n[]")))
    (is (= "simple" (mode/shape "hund.*")))
    (is (= "simple" (mode/shape "MU"))))
  (testing "the form: the radio when it names one, else the form of the
            mode the query was typed in"
    (is (= "simple" (mode/form {})))
    (is (= "simple" (mode/form {:q "[]"})))
    (is (= "extended" (mode/form {:t1.v "x"})))
    (is (= "extended" (mode/form {:mode "extended"})))
    (is (= "simple" (mode/form {:mode "simple" :t1.v "x"})))
    (is (= "extended" (mode/form {:mode "nonesuch" :t1.v "x"})))
    (is (= "simple" (mode/form-of "cqp")))
    (is (= "extended" (mode/form-of "extended"))))
  (testing "a mode reads the mode, its own keys and, given tokens, their
            fields, nothing else"
    (is (mode/reads? "simple" :within))
    (is (mode/reads? "extended" :t2.3.v))
    (is (mode/reads? "cqp" :mode))
    (is (not (mode/reads? "cqp" :in)))
    (is (not (mode/reads? "list" :within)))
    (is (not (mode/reads? "simple" :t1.v)))
    (testing "the marker standing for the tokens is no key"
      (is (not (mode/reads? "extended" ::mode/tokens)))
      (is (not (mode/query-key? ::mode/tokens)))))
  (testing "a query key is one some mode reads; the rest say where and how"
    (is (every? mode/query-key? [:q :mode :in :ci :match :within
                                 :t1.v :t2.3.join]))
    (is (not-any? mode/query-key? [:corpus :sort :f.text_year :page :from
                                   :cqp nil])))
  (testing "what the mode does not read is unread"
    (is (= #{:in :ci :match}
           (mode/unread {:q "[]" :in "lemma" :ci "on" :match "prefix"})))
    (is (= {:q "[]"}
           (mode/without-unread {:q "[]" :in "lemma" :ci "on"} "cqp")))))

(deftest read-keys-test
  (testing "what a form holds of the query params: the keys its mode reads,
            the mode itself aside"
    (is (= #{:q :in :within}
           (set (mode/read-keys "simple" {:q "x" :in "lemma" :within "text"
                                          :t1.v "y" :mode "simple"
                                          :corpus "A"}))))
    (is (= #{:t1.v :within}
           (set (mode/read-keys "extended" {:q "x" :in "lemma" :within "text"
                                            :t1.v "y" :corpus "A"}))))
    (is (= #{:q} (set (mode/read-keys "cqp" {:q "x" :in "lemma"}))))))

(deftest unread-query?-test
  (testing "a query the mode does not read is the form submitted with its
            radio changed, or a hand-written URL"
    (is (mode/unread-query? {:q "hund" :mode "extended"}))
    (is (mode/unread-query? {:q "[]" :mode "extended"}))
    (is (mode/unread-query? {:t1.v "hund" :mode "simple"}))
    (is (mode/unread-query? {:t1.v "hund" :mode "simple" :corpus "PROBE"})))
  (testing "not a query in its own form, nor an option carried along"
    (is (not (mode/unread-query? {:q "hund"})))
    (is (not (mode/unread-query? {:q "[]" :in "lemma"})))
    (is (not (mode/unread-query? {:mode "extended"}))))
  (testing "nor the blank field or the blank trailing token every form
            submits"
    (is (not (mode/unread-query? {:q "" :mode "extended" :in "word"})))
    (is (not (mode/unread-query? {:t1.attr "word" :t1.op "is" :t1.v ""
                                  :t1.min "1" :t1.max "1" :mode "simple"})))))
