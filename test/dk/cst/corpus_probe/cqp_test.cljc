(ns dk.cst.corpus-probe.cqp-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.cqp :as cqp]))

(deftest escape-literal-test
  (testing "PCRE metacharacters are backslash-escaped"
    (is (= "a\\.b\\*c" (cqp/escape-literal "a.b*c")))
    (is (= "\\[\\]\\{\\}\\(\\)\\^\\$\\|\\?\\+\\\\"
           (cqp/escape-literal "[]{}()^$|?+\\"))))
  (testing "quotes are doubled, CQP-style"
    (is (= "12\"\"-screen" (cqp/escape-literal "12\"-screen"))))
  (testing "ordinary text passes through"
    (is (= "København" (cqp/escape-literal "København")))))

(deftest escape-value-test
  (testing "the control characters a command line cannot carry become
            regex escapes"
    (is (= "a\\tb\\nc\\rd" (cqp/escape-value "a\tb\nc\rd"))))
  (testing "over the literal escaping"
    (is (= "a\\.b\"\"" (cqp/escape-value "a.b\"")))))

(deftest flatten-whitespace-test
  (is (= "a b c" (cqp/flatten-whitespace "a\nb\tc")))
  (testing "a run of them is one space"
    (is (= "a b" (cqp/flatten-whitespace "a\r\n\tb")))))

(deftest regex-value-test
  (testing "the metacharacters are kept, being the point, and quotes doubled"
    (is (= "hund.*\"\"" (cqp/regex-value "hund.*\""))))
  (testing "the line breaks and TABs a command line cannot carry are
            flattened"
    (is (= "a b" (cqp/regex-value "a\n\tb")))))

(deftest name?-test
  (is (cqp/name? "lemma"))
  (is (cqp/name? "_q-1"))
  (testing "CQP cannot parse a name beginning with a digit"
    (is (not (cqp/name? "1abc"))))
  (testing "nothing outside CQP's own identifier rule"
    (is (not (cqp/name? "q abc")))
    (is (not (cqp/name? "q;drop")))
    (is (not (cqp/name? "")))
    (is (not (cqp/name? nil)))))

(deftest corpus-name?-test
  (is (cqp/corpus-name? "PROBE"))
  (is (cqp/corpus-name? "MEMO_1880"))
  (is (not (cqp/corpus-name? "probe")))
  (is (not (cqp/corpus-name? "PROBE; exit")))
  (is (not (cqp/corpus-name? ""))))

(deftest units-test
  (testing "the sentence comes first, being the default, and each unit
            has the CQP name of its usual attribute"
    (is (= [:sentence :paragraph :text] (map first cqp/units)))
    (is (= {:sentence "s" :paragraph "p" :text "text"} (into {} cqp/units)))))
