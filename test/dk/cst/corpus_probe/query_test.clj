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
  (testing "each mode is a param value and the command it runs, no more"
    ;; what a mode is called is the interface's business rather than this
    ;; namespace's (see dk.cst.corpus-probe.views.page/sort-label)
    (is (every? (fn [[value command]]
                  (and (string? value) (string? command) ))
                query/sort-modes))
    (is (every? #(= 2 (count %)) query/sort-modes))))

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

(defn batch-commands
  "The commands of `batch` by section, so that a batch reads by name
  rather than by position."
  [batch]
  (query/batch-sections batch (mapv second batch)))

(deftest valid-result-name-test
  (is (= "q_ab12" (query/valid-result-name "q_ab12")))
  (testing "CQP cannot parse a name beginning with a digit"
    (is (thrown? Exception (query/valid-result-name "1abc"))))
  (testing "nothing outside CQP's own identifier rule"
    (is (thrown? Exception (query/valid-result-name "q abc")))
    (is (thrown? Exception (query/valid-result-name "q;drop")))
    (is (thrown? Exception (query/valid-result-name "")))))

(deftest valid-data-directory-test
  (is (= "/var/cache/probe" (query/valid-data-directory "/var/cache/probe")))
  (testing "a quote or backslash would end the quoted CQP string early"
    (is (thrown? Exception (query/valid-data-directory "/tmp/a\"b")))
    (is (thrown? Exception (query/valid-data-directory "/tmp/a\\b"))))
  (testing "a newline would end it and leave CQP reading commands"
    (is (thrown? Exception (query/valid-data-directory "/tmp/a\nPROBE;")))
    (is (thrown? Exception (query/valid-data-directory "/tmp/a\tb")))))

(deftest batch-sections-test
  (is (= {:size [["5"]] :cat [["a"] ["b"]]}
         (query/batch-sections [[:size "size Last;"]
                                [:cat "cat Last 0 0;"]
                                [:cat "cat Last 1 1;"]]
                               [["5"] ["a"] ["b"]])))
  (testing "a batch nothing ran has no sections"
    (is (= {} (query/batch-sections [] [])))))

(deftest kwic-batch-test
  (let [opts {:p-attrs      [:word :pos :lemma]
              :struct-attrs [:text_id :text_title]
              :rows         [20 29]
              :sort         "word"}
        b    (batch-commands (query/kwic-batch "PROBE" "\"hund\"" opts))]
    (is (= ["PROBE;"] (:corpus b)))
    (is (= ["size Last;"] (:size b)))
    (is (str/includes? (first (:sort b)) "ExternalSort"))
    (testing "the rows select the cat and dump range"
      (is (str/includes? (first (:cat b)) "cat Last 20 29;"))
      (is (= ["dump Last 20 29;"] (:dump b))))
    (testing "p-attributes beyond word are shown"
      (is (str/includes? (first (:cat b)) "show +pos +lemma; ")))
    (testing "each struct attribute gets its own single-column tabulate"
      (is (= ["tabulate Last 20 29 match text_id;"
              "tabulate Last 20 29 match text_title;"]
             (:tabulate b)))))
  (testing "no tabulate commands without struct attributes"
    (is (nil? (:tabulate (batch-commands
                          (query/kwic-batch "PROBE" "\"hund\""
                                            {:p-attrs [:word]}))))))
  (testing "the default rows are the first page"
    (is (= ["dump Last 0 24;"]
           (:dump (batch-commands (query/kwic-batch "PROBE" "\"hund\""
                                                    {:p-attrs [:word]})))))))

(deftest kwic-batch-cache-test
  (testing "without a cache the batch neither saves nor sets DataDirectory"
    (let [b (batch-commands (query/kwic-batch "PROBE" "\"hund\""
                                              {:p-attrs [:word]}))]
      (is (nil? (:save b)))
      (is (not (str/includes? (first (:setup b)) "DataDirectory")))))
  (testing "with a cache the sorted result is saved under the given name"
    (let [b (batch-commands
             (query/kwic-batch "PROBE" "\"hund\""
                               {:p-attrs   [:word]
                                :cache-dir "/var/cache/probe"
                                :nqr       "q_abc"}))]
      (is (str/includes? (first (:setup b))
                         "set DataDirectory \"/var/cache/probe\"; "))
      (is (= ["q_abc = Last; save q_abc;"] (:save b)))
      (testing "the page still comes from Last, which is what was sorted"
        (is (= ["dump Last 0 24;"] (:dump b))))))
  (testing "the name is guarded like every other value spliced in"
    (is (thrown? Exception (query/kwic-batch "PROBE" "\"hund\""
                                             {:p-attrs [:word] :nqr "1bad"})))))

(deftest kwic-batch-order-test
  (testing "DataDirectory precedes activation; the name comes after the sort"
    (is (= [:setup :corpus :query :size :sort :save :cat :dump]
           (mapv first (query/kwic-batch "PROBE" "\"hund\""
                                         {:p-attrs   [:word]
                                          :cache-dir "/var/cache/probe"
                                          :nqr       "q_abc"}))))))

(deftest stored-kwic-batch-test
  (let [batch (query/stored-kwic-batch "PROBE" "q_abc"
                                       {:p-attrs      [:word :pos]
                                        :struct-attrs [:text_id]
                                        :rows         [25 49]
                                        :cache-dir    "/var/cache/probe"})
        b     (batch-commands batch)]
    (testing "nothing is queried, sorted or saved"
      (is (= [:setup :corpus :size :cat :dump :tabulate] (mapv first batch))))
    (testing "every row command reads the stored result"
      (is (= ["size q_abc;"] (:size b)))
      (is (str/includes? (first (:cat b)) "cat q_abc 25 49;"))
      (is (= ["dump q_abc 25 49;"] (:dump b)))
      (is (= ["tabulate q_abc 25 49 match text_id;"] (:tabulate b))))
    (testing "the display profile and context are still set per request"
      (is (str/includes? (first (:setup b)) "set Context 5 words;"))))
  (testing "the name is guarded here too"
    (is (thrown? Exception (query/stored-kwic-batch "PROBE" "1abc"
                                                    {:p-attrs [:word]})))))
