(ns dk.cst.corpus-probe.cwb.command-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.cwb.command :as command]))

(deftest sentence-tags-test
  (testing "the sentence tags take each corpus's own name for a sentence"
    (is (= "<sentence> [word = \"x\"] </sentence>"
           (command/sentence-tags "<s> [word = \"x\"] </s>" :sentence)))
    (is (= "<s> [word = \"x\"] </s>"
           (command/sentence-tags "<s> [word = \"x\"] </s>" :s)))
    (is (= "<s> [word = \"x\"]"
           (command/sentence-tags "<s> [word = \"x\"]" nil)))
    (testing "but leave a literal alone"
      (is (= "[word = \"<s>\"] </sentence>"
             (command/sentence-tags "[word = \"<s>\"] </s>" :sentence))))))

(deftest within-clause-test
  (testing "a within clause naming a unit by CWB's usual name takes the
            corpus's own, and goes where the corpus marks no such unit"
    (is (= "[] [] within sentence"
           (command/within-clause "[] [] within s" {:sentence :sentence})))
    (is (= "[] [] within s"
           (command/within-clause "[] [] within s;" {:sentence :s})))
    (is (= "[] [] within text"
           (command/within-clause "[] [] within text" {:text :text})))
    (is (= "[] []" (command/within-clause "[] [] within p" {:sentence :s}))))
  (testing "a query without one, or one naming an attribute outright, is
            left as it is"
    (is (= "[] []" (command/within-clause "[] []" {:sentence :sentence})))
    (is (= "[] [] within sentence"
           (command/within-clause "[] [] within sentence" {})))
    (is (= "[word = \"within s\"]"
           (command/within-clause "[word = \"within s\"]" {})))))

(deftest within-query-test
  (is (= "[] [] within s" (command/within-query "[] []" :s)))
  (testing "no attribute, no clause: the query is left as it was"
    (is (= "[] []" (command/within-query "[] []" nil)))))

(deftest near-command-test
  (is (= (str "set Last keyword nearest [word = \"kat\" %c] within 5 words"
              " from match; delete Last without keyword;")
         (command/near-command {:word "kat" :distance 5})))
  (testing "the word is escaped like every spliced value"
    (is (str/includes? (command/near-command {:word "a.b\n" :distance 2})
                       "[word = \"a\\.b\\n\" %c]")))
  (testing "no word, no command"
    (is (nil? (command/near-command nil)))
    (is (nil? (command/near-command {:word " " :distance 5})))))

(deftest filter-query-test
  (testing "one attribute anchors and expands to its own region"
    (is (= "<text_year = \"1591\"> [] expand to text_year"
           (command/filter-query [[:text_year #{"1591"}]]))))
  (testing "several values are an alternation, sorted, matched literally"
    (is (= "<text_title = \"a\\.b|c\"> [] expand to text_title"
           (command/filter-query [[:text_title #{"c" "a.b"}]]))))
  (testing "a TAB in a value becomes the regex escape"
    (is (= "<text_title = \"a\\tb\"> [] expand to text_title"
           (command/filter-query [[:text_title #{"a\tb"}]]))))
  (testing "a pattern is matched as the regex it is, in a group of its
            own, beside the values"
    (is (= "<text_year = \"1591|(15..)|(16[0-4].)\"> [] expand to text_year"
           (command/filter-query
            [[:text_year #{"1591"} ["15.." "16[0-4]."]]])))
    (is (= "<text_title = \"(Hav.*)\"> [] expand to text_title"
           (command/filter-query [[:text_title #{} ["Hav.*"]]])))
    (testing "its quotes doubled, which is all it needs"
      (is (= "<text_title = \"(\"\"a\"\")\"> [] expand to text_title"
             (command/filter-query [[:text_title #{} ["\"a\""]]])))))
  (testing "several attributes must all hold, anchored on the first"
    (is (= (str "<s_id = \"2\"> [_.text_year = \"1583|1591\"]"
                " expand to s_id")
           (command/filter-query [[:s_id #{"2"}]
                                  [:text_year #{"1591" "1583"}]])))))

(deftest restricted-query-test
  (testing "no filter is the plain locked query"
    (is (re-matches #"(?s)set QueryLock \d+;\n\"hund\"\n;\nunlock \d+;"
                    (command/restricted-query "\"hund\"" nil))))
  (testing "a filter runs under its own lock, then is activated"
    (let [restricted (command/restricted-query "\"hund\""
                                               [[:text_year #{"1591"}]])]
      (is (re-matches
           (re-pattern (str "(?s)set QueryLock (\\d+);\n"
                            "<text_year = \"1591\"> \\[\\] expand to text_year"
                            "\n;\nunlock \\1;\nFilter = Last;\nFilter;\n"
                            "set QueryLock (\\d+);\n\"hund\"\n;\nunlock \\2;"))
           restricted)))))

(deftest locked-query-test
  (let [locked (command/locked-query "\"hund\";")]
    (testing "the query is wrapped in a QueryLock sandbox"
      (is (re-matches #"(?s)set QueryLock \d+;\n\"hund\"\n;\nunlock \d+;"
                      locked)))
    (testing "lock and unlock use the same key"
      (let [[_ k1 k2] (re-matches #"(?s)set QueryLock (\d+);.*unlock (\d+);"
                                  locked)]
        (is (= k1 k2)))))
  (testing "newlines and TABs in the query are flattened"
    (is (not (re-find #"\t" (command/locked-query "\"a\"\t[]\n\"b\"")))))
  (testing "the terminator and unlock survive a # comment in the query"
    (is (re-find #"# comment\n;\nunlock \d+;$"
                 (command/locked-query "\"hund\" # comment")))))

(deftest position-query-test
  (testing "a single-token span anchors just the position"
    (is (= "[word=\".*\" & _ = 9]" (command/position-query 9 9))))
  (testing "a multi-token span adds a trailing matchall count"
    (is (= "[word=\".*\" & _ = 9] []{4}" (command/position-query 9 13)))))

(deftest sort-command-test
  (testing "the default is corpus order"
    (is (= "sort Last;" (command/sort-command "corpus")))
    (is (= "sort Last;" (command/sort-command nil)))
    (is (= "sort Last;" (command/sort-command "no such mode"))))
  (testing "word sort uses ExternalSort for locale collation"
    (is (str/includes? (command/sort-command "word") "ExternalSort")))
  (testing "random sort uses a fixed seed"
    (is (str/includes? (command/sort-command "random") "randomize 1"))))

(deftest sort-modes-test
  (testing "each mode is a param value and the command it runs, no more"
    ;; what a mode is called is the interface's business rather than this
    ;; namespace's (see dk.cst.corpus-probe.views.concordance/sort-label)
    (is (every? (fn [[value command]]
                  (and (string? value) (string? command)))
                command/sort-modes))
    (is (every? #(= 2 (count %)) command/sort-modes))))

(deftest sample-command-test
  (testing "a sample is seeded, so one URL always names the same hits"
    (is (= "randomize 1; reduce Last to 100;" (command/sample-command 100))))
  (testing "no sample where none is asked for"
    (is (nil? (command/sample-command nil)))
    (testing "including a sample of none of the hits, which CQP ignores
              silently and would otherwise leave the whole result reported
              as a sample of it"
      (is (nil? (command/sample-command 0)))
      (is (nil? (command/sample-command -1))))))

(deftest valid-corpus-name-test
  (is (= "PROBE" (command/valid-corpus-name "PROBE")))
  (testing "anything but an uppercase CQP corpus name is refused"
    (is (thrown? Exception (command/valid-corpus-name "probe")))
    (is (thrown? Exception (command/valid-corpus-name "PROBE; exit")))
    (is (thrown? Exception (command/valid-corpus-name nil)))))

(deftest valid-result-name-test
  (is (= "q_ab12" (command/valid-result-name "q_ab12")))
  (testing "CQP cannot parse a name beginning with a digit"
    (is (thrown? Exception (command/valid-result-name "1abc"))))
  (testing "nothing outside CQP's own identifier rule"
    (is (thrown? Exception (command/valid-result-name "q abc")))
    (is (thrown? Exception (command/valid-result-name "q;drop")))
    (is (thrown? Exception (command/valid-result-name "")))))

(deftest valid-data-directory-test
  (is (= "/var/cache/probe" (command/valid-data-directory "/var/cache/probe")))
  (testing "a quote or backslash would end the quoted CQP string early"
    (is (thrown? Exception (command/valid-data-directory "/tmp/a\"b")))
    (is (thrown? Exception (command/valid-data-directory "/tmp/a\\b"))))
  (testing "a newline would end it and leave CQP reading commands"
    (is (thrown? Exception (command/valid-data-directory "/tmp/a\nPROBE;")))
    (is (thrown? Exception (command/valid-data-directory "/tmp/a\tb")))))

(deftest count-command-test
  (is (= "group Last match[-1] lemma;"
         (command/count-command "match[-1]" :lemma)))
  (testing "the whole match is counted with count, whose output differs"
    (is (= "count Last by lemma;"
           (command/count-command "match..matchend" :lemma)))
    (is (command/whole-match? "match..matchend"))
    (is (not (command/whole-match? "match"))))
  (testing "within a region attribute, the regions each value occurs in"
    (is (= "group Last match lemma within text;"
           (command/count-command "match" :lemma {:within :text}))))
  (testing "only CQP's own positions are spliced in"
    (is (thrown? Exception (command/count-command "match[-2]" :lemma)))
    (is (thrown? Exception (command/count-command "match; exit" :lemma)))))

(deftest subset-command-test
  (let [at (fn [anchor] (command/subset-command {:anchor anchor :attr :lemma
                                                 :value  "a.b"}))]
    (testing "at the ends of the match, CQP's own subset, the value escaped
              and read through the this label"
      (is (= "Last = subset Last where match: [_.lemma = \"a\\.b\"];"
             (at "match")))
      (is (= "Last = subset Last where matchend: [_.lemma = \"a\\.b\"];"
             (at "matchend"))))
    (testing "beside it, the keyword anchor set on the one token there"
      (is (= (str "set Last keyword nearest [_.lemma = \"a\\.b\"] within left"
                  " 1 words from match; delete Last without keyword;")
             (at "match[-1]")))
      (is (= (str "set Last keyword nearest [_.lemma = \"a\\.b\"] within"
                  " right 1 words from matchend; delete Last without keyword;")
             (at "matchend[1]"))))
    (testing "over the whole match, the result intersected with the exact
              sequence, one locked token pattern per space"
      (let [cmd (command/subset-command {:anchor "match..matchend" :attr :lemma
                                         :value  "en hund"})]
        (is (str/starts-with? cmd "Q = Last;\nset QueryLock "))
        (is (str/includes? cmd "\n[lemma = \"en\"] [lemma = \"hund\"]\n;\n"))
        (is (str/ends-with? cmd ";\nLast = intersection Q Last;"))))
    (is (thrown? Exception (at "target")))))

(deftest narrowing-test
  (let [subset {:anchor "match" :attr :lemma :value "hund"}
        near   {:word "kat" :distance 5}]
    (testing "the subset comes first, so the word is looked for beside the
              hits that are kept"
      (is (= [:subset :near]
             (mapv first (command/narrowing {:subset subset :near near})))))
    (testing "nothing asked for, nothing to run"
      (is (= [] (command/narrowing {})))
      (is (= [] (command/narrowing {:near {:word " " :distance 5}}))))))

(deftest sort-attr-test
  (is (= :lemma (command/sort-attr "lemma")))
  (testing "the fixed modes name no attribute, word included"
    (is (nil? (command/sort-attr "word")))
    (is (nil? (command/sort-attr "corpus")))
    (is (nil? (command/sort-attr "reverse"))))
  (testing "nothing that is not a name reaches a command"
    (is (nil? (command/sort-attr "lemma; exit")))
    (is (nil? (command/sort-attr "")))
    (is (nil? (command/sort-attr nil)))))

(deftest sort-attribute-command-test
  (testing "a mode naming an attribute sorts by it under the same collation"
    (is (= "set ExternalSort on; sort Last by lemma;"
           (command/sort-command "lemma"))))
  (testing "the reverse sort reads the word from its end"
    (is (= "set ExternalSort on; sort Last by word reverse;"
           (command/sort-command "reverse")))))

(deftest count-by-command-test
  (testing "a second attribute is counted against at the match"
    (is (= "group Last match lemma by match text_year;"
           (command/count-command "match" :lemma {:by :text_year})))
    (is (= "group Last matchend[1] word by match pos within text;"
           (command/count-command "matchend[1]" :word {:by     :pos
                                                       :within :text}))))
  (testing "count has no by, so the whole match ignores it"
    (is (= "count Last by lemma;"
           (command/count-command "match..matchend" :lemma {:by :pos})))))

(deftest load-command-test
  (is (= "set DataDirectory \"/cache/PROBE\"; PROBE; Last = q_1;"
         (command/load-command "PROBE" "q_1" "/cache/PROBE")))
  (testing "the name and the directory are guarded"
    (is (thrown? Exception
                 (command/load-command "PROBE" "q_1; exit" "/cache")))
    (is (thrown? Exception
                 (command/load-command "PROBE" "q_1" "/c\"; exit")))))
