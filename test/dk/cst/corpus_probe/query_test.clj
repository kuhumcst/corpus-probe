(ns dk.cst.corpus-probe.query-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.i18n :as i18n]
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

(deftest filter-query-test
  (testing "one attribute anchors and expands to its own region"
    (is (= "<text_year = \"1591\"> [] expand to text_year"
           (query/filter-query [[:text_year #{"1591"}]]))))
  (testing "several values are an alternation, sorted, matched literally"
    (is (= "<text_title = \"a\\.b|c\"> [] expand to text_title"
           (query/filter-query [[:text_title #{"c" "a.b"}]]))))
  (testing "a TAB in a value becomes the regex escape"
    (is (= "<text_title = \"a\\tb\"> [] expand to text_title"
           (query/filter-query [[:text_title #{"a\tb"}]]))))
  (testing "several attributes must all hold, anchored on the first"
    (is (= (str "<s_id = \"2\"> [_.text_year = \"1583|1591\"]"
                " expand to s_id")
           (query/filter-query [[:s_id #{"2"}]
                                [:text_year #{"1591" "1583"}]])))))

(deftest restricted-query-test
  (testing "no filter is the plain locked query"
    (is (re-matches #"(?s)set QueryLock \d+;\n\"hund\"\n;\nunlock \d+;"
                    (query/restricted-query "\"hund\"" nil))))
  (testing "a filter runs under its own lock, then is activated"
    (let [restricted (query/restricted-query "\"hund\""
                                             [[:text_year #{"1591"}]])]
      (is (re-matches
           (re-pattern (str "(?s)set QueryLock (\\d+);\n"
                            "<text_year = \"1591\"> \\[\\] expand to text_year"
                            "\n;\nunlock \\1;\nFilter = Last;\nFilter;\n"
                            "set QueryLock (\\d+);\n\"hund\"\n;\nunlock \\2;"))
           restricted)))))

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

(deftest position-query-test
  (testing "a single-token span anchors just the position"
    (is (= "[word=\".*\" & _ = 9]" (query/position-query 9 9))))
  (testing "a multi-token span adds a trailing matchall count"
    (is (= "[word=\".*\" & _ = 9] []{4}" (query/position-query 9 13)))))

(deftest sort-command-test
  (testing "the default is corpus order"
    (is (= "sort Last;" (query/sort-command "corpus")))
    (is (= "sort Last;" (query/sort-command nil)))
    (is (= "sort Last;" (query/sort-command "bogus"))))
  (testing "word sort uses ExternalSort for locale collation"
    (is (str/includes? (query/sort-command "word") "ExternalSort")))
  (testing "random sort uses a fixed seed"
    (is (str/includes? (query/sort-command "random") "randomize 1"))))

(deftest sort-modes-test
  (testing "every sort mode is labelled by a key the dictionary defines"
    ;; `tr` renders an unknown key as its own name rather than failing
    (is (empty? (remove i18n/dictionary (map second query/sort-modes))))))

(deftest page-rows-test
  (is (= [0 24] (query/page-rows 0 25)))
  (is (= [20 29] (query/page-rows 2 10)))
  (testing "negative or zero paging values are clamped, not passed to CQP"
    (is (= [0 0] (query/page-rows -1 0))))
  (testing "a page beyond CQP's int range is clamped below it"
    (let [[from to] (query/page-rows 100000000 25)]
      (is (<= to query/max-row))
      (is (= 24 (- to from))))
    (is (<= (second (query/page-rows Long/MAX_VALUE 25)) query/max-row))))

(deftest kwic-commands-test
  (let [opts     {:p-attrs      [:word :pos :lemma]
                  :struct-attrs [:text_id :text_title]
                  :rows         [20 29]
                  :sort         "word"}
        commands (query/kwic-commands "PROBE" "\"hund\"" opts)]
    (is (= 9 (count commands)))
    (is (= "PROBE;" (nth commands 1)))
    (testing "the sort slot precedes the page commands"
      (is (str/includes? (nth commands 4) "ExternalSort")))
    (testing "the rows select the cat and dump range"
      (is (str/includes? (nth commands 5) "cat Last 20 29;"))
      (is (= "dump Last 20 29;" (nth commands 6))))
    (testing "p-attributes beyond word are shown"
      (is (str/includes? (nth commands 5) "show +pos +lemma; ")))
    (testing "each struct attribute gets its own single-column tabulate"
      (is (= "tabulate Last 20 29 match text_id;" (nth commands 7)))
      (is (= "tabulate Last 20 29 match text_title;" (nth commands 8)))))
  (testing "no tabulate commands without struct attributes"
    (is (= 7 (count (query/kwic-commands "PROBE" "\"hund\""
                                         {:p-attrs [:word]})))))
  (testing "the default rows are the first page"
    (is (= "dump Last 0 24;"
           (nth (query/kwic-commands "PROBE" "\"hund\"" {:p-attrs [:word]})
                6)))))
