(ns dk.cst.corpus-probe.query-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
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

(deftest simple->cqp-test
  (is (= "[word = \"hund\"]" (query/simple->cqp "hund")))
  (is (= "[word = \"lille\" %c] [word = \"hund\" %c]"
         (query/simple->cqp " lille  hund " {:case-insensitive? true})))
  (is (= "[word = \"hund.*\"]" (query/simple->cqp "hund" {:prefix? true})))
  (is (= "[word = \".*hund.*\"]"
         (query/simple->cqp "hund" {:prefix? true :suffix? true})))
  (is (= "[word = \"hund\"] within s" (query/simple->cqp "hund" {:within :s})))
  (testing "regex metacharacters in input are matched literally"
    (is (= "[word = \"hund\\.\"]" (query/simple->cqp "hund."))))
  (testing "blank input yields nil, not a match-everything query"
    (is (nil? (query/simple->cqp "   ")))
    (is (nil? (query/simple->cqp "" {:prefix? true})))))

(deftest corpus-name?-test
  (is (query/corpus-name? "PROBE"))
  (is (query/corpus-name? "MEMO_1880"))
  (is (not (query/corpus-name? "probe")))
  (is (not (query/corpus-name? "PROBE; exit")))
  (is (not (query/corpus-name? ""))))

(deftest locked-query-test
  (let [locked (query/locked-query "\"hund\";")]
    (testing "the query is wrapped in a QueryLock sandbox"
      (is (re-matches #"(?s)set QueryLock \d+;\n\"hund\"\n;\nunlock \d+;"
                      locked)))
    (testing "lock and unlock use the same key"
      (let [[_ k1 k2] (re-matches #"(?s)set QueryLock (\d+);.*unlock (\d+);"
                                  locked)]
        (is (= k1 k2)))))
  (testing "newlines and TABs in the query are flattened"
    (is (not (re-find #"\t" (query/locked-query "\"a\"\t[]\n\"b\"")))))
  (testing "the terminator and unlock survive a # comment in the query"
    (is (re-find #"# comment\n;\nunlock \d+;$"
                 (query/locked-query "\"hund\" # comment")))))

(deftest kwic-commands-test
  (let [opts     {:p-attrs      [:word :pos :lemma]
                  :struct-attrs [:text_id :text_title]
                  :page         2
                  :page-size    10}
        commands (query/kwic-commands "PROBE" "\"hund\"" opts)]
    (is (= 8 (count commands)))
    (is (= "PROBE;" (nth commands 1)))
    (testing "paging arithmetic"
      (is (str/includes? (nth commands 4) "cat Last 20 29;"))
      (is (= "dump Last 20 29;" (nth commands 5))))
    (testing "p-attributes beyond word are shown"
      (is (str/includes? (nth commands 4) "show +pos +lemma; ")))
    (testing "each struct attribute gets its own single-column tabulate"
      (is (= "tabulate Last 20 29 match text_id;" (nth commands 6)))
      (is (= "tabulate Last 20 29 match text_title;" (nth commands 7)))))
  (testing "no tabulate commands without struct attributes"
    (is (= 6 (count (query/kwic-commands "PROBE" "\"hund\""
                                         {:p-attrs [:word]})))))
  (testing "negative or zero paging values are clamped, not passed to CQP"
    (let [commands (query/kwic-commands "PROBE" "\"hund\""
                                        {:p-attrs   [:word]
                                         :page      -1
                                         :page-size 0})]
      (is (= "dump Last 0 0;" (nth commands 5))))))
