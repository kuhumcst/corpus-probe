(ns dk.cst.corpus-probe.commands-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.commands :as commands]))

(deftest sentence-tags-test
  (testing "the sentence tags take each corpus's own name for a sentence"
    (is (= "<sentence> [word = \"x\"] </sentence>"
           (commands/sentence-tags "<s> [word = \"x\"] </s>" :sentence)))
    (is (= "<s> [word = \"x\"] </s>"
           (commands/sentence-tags "<s> [word = \"x\"] </s>" :s)))
    (is (= "<s> [word = \"x\"]"
           (commands/sentence-tags "<s> [word = \"x\"]" nil)))
    (testing "but leave a literal alone"
      (is (= "[word = \"<s>\"] </sentence>"
             (commands/sentence-tags "[word = \"<s>\"] </s>" :sentence))))))

(deftest within-clause-test
  (testing "a within clause naming a unit by CWB's usual name takes the
            corpus's own, and goes where the corpus marks no such unit"
    (is (= "[] [] within sentence"
           (commands/within-clause "[] [] within s" {:sentence :sentence})))
    (is (= "[] [] within s"
           (commands/within-clause "[] [] within s;" {:sentence :s})))
    (is (= "[] [] within text"
           (commands/within-clause "[] [] within text" {:text :text})))
    (is (= "[] []" (commands/within-clause "[] [] within p" {:sentence :s}))))
  (testing "a query without one, or one naming an attribute outright, is
            left as it is"
    (is (= "[] []" (commands/within-clause "[] []" {:sentence :sentence})))
    (is (= "[] [] within sentence"
           (commands/within-clause "[] [] within sentence" {})))
    (is (= "[word = \"within s\"]"
           (commands/within-clause "[word = \"within s\"]" {})))))

(deftest within-query-test
  (is (= "[] [] within s" (commands/within-query "[] []" :s)))
  (testing "no attribute, no clause: the query is left as it was"
    (is (= "[] []" (commands/within-query "[] []" nil)))))

(deftest near-command-test
  (is (= (str "set Last keyword nearest [word = \"kat\" %c] within 5 words"
              " from match; delete Last without keyword;")
         (commands/near-command {:word "kat" :distance 5})))
  (testing "the word is escaped like every spliced value"
    (is (str/includes? (commands/near-command {:word "a.b\n" :distance 2})
                       "[word = \"a\\.b\\n\" %c]")))
  (testing "no word, no command"
    (is (nil? (commands/near-command nil)))
    (is (nil? (commands/near-command {:word " " :distance 5})))))

(deftest corpus-name?-test
  (is (commands/corpus-name? "PROBE"))
  (is (commands/corpus-name? "MEMO_1880"))
  (is (not (commands/corpus-name? "probe")))
  (is (not (commands/corpus-name? "PROBE; exit")))
  (is (not (commands/corpus-name? ""))))

(deftest filter-query-test
  (testing "one attribute anchors and expands to its own region"
    (is (= "<text_year = \"1591\"> [] expand to text_year"
           (commands/filter-query [[:text_year #{"1591"}]]))))
  (testing "several values are an alternation, sorted, matched literally"
    (is (= "<text_title = \"a\\.b|c\"> [] expand to text_title"
           (commands/filter-query [[:text_title #{"c" "a.b"}]]))))
  (testing "a TAB in a value becomes the regex escape"
    (is (= "<text_title = \"a\\tb\"> [] expand to text_title"
           (commands/filter-query [[:text_title #{"a\tb"}]]))))
  (testing "a pattern is matched as the regex it is, in a group of its
            own, beside the values"
    (is (= "<text_year = \"1591|(15..)|(16[0-4].)\"> [] expand to text_year"
           (commands/filter-query
            [[:text_year #{"1591"} ["15.." "16[0-4]."]]])))
    (is (= "<text_title = \"(Hav.*)\"> [] expand to text_title"
           (commands/filter-query [[:text_title #{} ["Hav.*"]]])))
    (testing "its quotes doubled, which is all it needs"
      (is (= "<text_title = \"(\"\"a\"\")\"> [] expand to text_title"
             (commands/filter-query [[:text_title #{} ["\"a\""]]])))))
  (testing "several attributes must all hold, anchored on the first"
    (is (= (str "<s_id = \"2\"> [_.text_year = \"1583|1591\"]"
                " expand to s_id")
           (commands/filter-query [[:s_id #{"2"}]
                                [:text_year #{"1591" "1583"}]])))))

(deftest restricted-query-test
  (testing "no filter is the plain locked query"
    (is (re-matches #"(?s)set QueryLock \d+;\n\"hund\"\n;\nunlock \d+;"
                    (commands/restricted-query "\"hund\"" nil))))
  (testing "a filter runs under its own lock, then is activated"
    (let [restricted (commands/restricted-query "\"hund\""
                                             [[:text_year #{"1591"}]])]
      (is (re-matches
           (re-pattern (str "(?s)set QueryLock (\\d+);\n"
                            "<text_year = \"1591\"> \\[\\] expand to text_year"
                            "\n;\nunlock \\1;\nFilter = Last;\nFilter;\n"
                            "set QueryLock (\\d+);\n\"hund\"\n;\nunlock \\2;"))
           restricted)))))

(deftest locked-query-test
  (let [locked (commands/locked-query "\"hund\";")]
    (testing "the query is wrapped in a QueryLock sandbox"
      (is (re-matches #"(?s)set QueryLock \d+;\n\"hund\"\n;\nunlock \d+;"
                      locked)))
    (testing "lock and unlock use the same key"
      (let [[_ k1 k2] (re-matches #"(?s)set QueryLock (\d+);.*unlock (\d+);"
                                  locked)]
        (is (= k1 k2)))))
  (testing "newlines and TABs in the query are flattened"
    (is (not (re-find #"\t" (commands/locked-query "\"a\"\t[]\n\"b\"")))))
  (testing "the terminator and unlock survive a # comment in the query"
    (is (re-find #"# comment\n;\nunlock \d+;$"
                 (commands/locked-query "\"hund\" # comment")))))

(deftest position-query-test
  (testing "a single-token span anchors just the position"
    (is (= "[word=\".*\" & _ = 9]" (commands/position-query 9 9))))
  (testing "a multi-token span adds a trailing matchall count"
    (is (= "[word=\".*\" & _ = 9] []{4}" (commands/position-query 9 13)))))

(deftest sort-command-test
  (testing "the default is corpus order"
    (is (= "sort Last;" (commands/sort-command "corpus")))
    (is (= "sort Last;" (commands/sort-command nil)))
    (is (= "sort Last;" (commands/sort-command "no such mode"))))
  (testing "word sort uses ExternalSort for locale collation"
    (is (str/includes? (commands/sort-command "word") "ExternalSort")))
  (testing "random sort uses a fixed seed"
    (is (str/includes? (commands/sort-command "random") "randomize 1"))))

(deftest sort-modes-test
  (testing "each mode is a param value and the command it runs, no more"
    ;; what a mode is called is the interface's business rather than this
    ;; namespace's (see dk.cst.corpus-probe.views.page/sort-label)
    (is (every? (fn [[value command]]
                  (and (string? value) (string? command) ))
                commands/sort-modes))
    (is (every? #(= 2 (count %)) commands/sort-modes))))

(deftest sample-command-test
  (testing "a sample is seeded, so one URL always names the same hits"
    (is (= "randomize 1; reduce Last to 100;" (commands/sample-command 100))))
  (testing "no sample where none is asked for"
    (is (nil? (commands/sample-command nil)))
    (testing "including a sample of none of the hits, which CQP ignores
              silently and would otherwise leave the whole result reported
              as a sample of it"
      (is (nil? (commands/sample-command 0)))
      (is (nil? (commands/sample-command -1))))))

(deftest page-rows-test
  (is (= [0 24] (commands/page-rows 0 25)))
  (is (= [20 29] (commands/page-rows 2 10)))
  (testing "negative or zero paging values are clamped, not passed to CQP"
    (is (= [0 0] (commands/page-rows -1 0))))
  (testing "a page beyond CQP's int range is clamped below it"
    (let [[from to] (commands/page-rows 100000000 25)]
      (is (<= to commands/max-row))
      (is (= 24 (- to from))))
    (is (<= (second (commands/page-rows Long/MAX_VALUE 25)) commands/max-row))))

(defn batch-commands
  "The commands of `batch` by section, so that a batch reads by name
  rather than by position."
  [batch]
  (commands/batch-sections batch (mapv second batch)))

(deftest valid-result-name-test
  (is (= "q_ab12" (commands/valid-result-name "q_ab12")))
  (testing "CQP cannot parse a name beginning with a digit"
    (is (thrown? Exception (commands/valid-result-name "1abc"))))
  (testing "nothing outside CQP's own identifier rule"
    (is (thrown? Exception (commands/valid-result-name "q abc")))
    (is (thrown? Exception (commands/valid-result-name "q;drop")))
    (is (thrown? Exception (commands/valid-result-name "")))))

(deftest valid-data-directory-test
  (is (= "/var/cache/probe" (commands/valid-data-directory "/var/cache/probe")))
  (testing "a quote or backslash would end the quoted CQP string early"
    (is (thrown? Exception (commands/valid-data-directory "/tmp/a\"b")))
    (is (thrown? Exception (commands/valid-data-directory "/tmp/a\\b"))))
  (testing "a newline would end it and leave CQP reading commands"
    (is (thrown? Exception (commands/valid-data-directory "/tmp/a\nPROBE;")))
    (is (thrown? Exception (commands/valid-data-directory "/tmp/a\tb")))))

(deftest batch-sections-test
  (is (= {:size [["5"]] :cat [["a"] ["b"]]}
         (commands/batch-sections [[:size "size Last;"]
                                [:cat "cat Last 0 0;"]
                                [:cat "cat Last 1 1;"]]
                               [["5"] ["a"] ["b"]])))
  (testing "a batch nothing ran has no sections"
    (is (= {} (commands/batch-sections [] [])))))

(deftest kwic-batch-test
  (let [opts {:p-attrs      [:word :pos :lemma]
              :struct-attrs [:text_id :text_title]
              :rows         [20 29]
              :sort         "word"}
        b    (batch-commands (commands/kwic-batch "PROBE" "\"hund\"" opts))]
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
                          (commands/kwic-batch "PROBE" "\"hund\""
                                            {:p-attrs [:word]}))))))
  (testing "the default rows are the first page"
    (is (= ["dump Last 0 24;"]
           (:dump (batch-commands (commands/kwic-batch "PROBE" "\"hund\""
                                                    {:p-attrs [:word]})))))))

(deftest kwic-batch-cache-test
  (testing "without a cache the batch neither saves nor sets DataDirectory"
    (let [b (batch-commands (commands/kwic-batch "PROBE" "\"hund\""
                                              {:p-attrs [:word]}))]
      (is (nil? (:save b)))
      (is (not (str/includes? (first (:setup b)) "DataDirectory")))))
  (testing "with a cache the sorted result is saved under the given name"
    (let [b (batch-commands
             (commands/kwic-batch "PROBE" "\"hund\""
                               {:p-attrs   [:word]
                                :cache-dir "/var/cache/probe"
                                :nqr       "q_abc"}))]
      (is (str/includes? (first (:setup b))
                         "set DataDirectory \"/var/cache/probe\"; "))
      (is (= ["q_abc = Last; save q_abc;"] (:save b)))
      (testing "the page still comes from Last, which is what was sorted"
        (is (= ["dump Last 0 24;"] (:dump b))))))
  (testing "the name is guarded like every other value spliced in"
    (is (thrown? Exception (commands/kwic-batch "PROBE" "\"hund\""
                                             {:p-attrs [:word] :nqr "1bad"})))))

(deftest kwic-batch-order-test
  (testing "DataDirectory precedes activation; the name comes after the sort"
    (is (= [:setup :corpus :query :size :sort :save :cat :dump]
           (mapv first (commands/kwic-batch "PROBE" "\"hund\""
                                         {:p-attrs   [:word]
                                          :cache-dir "/var/cache/probe"
                                          :nqr       "q_abc"}))))))

(deftest kwic-batch-sample-test
  (testing "the sample is drawn before the result is counted or ordered"
    ;; the size to report is the sample's, and CQP's reduce discards the
    ;; sort order of the result it reduces
    (is (= [:setup :corpus :query :sample :size :sort :cat :dump]
           (mapv first (commands/kwic-batch "PROBE" "\"hund\""
                                         {:p-attrs [:word] :sample 100})))))
  (testing "and what is saved is the sample, sorted"
    (is (= [:setup :corpus :query :sample :size :sort :save :cat :dump]
           (mapv first (commands/kwic-batch "PROBE" "\"hund\""
                                         {:p-attrs   [:word]
                                          :sample    100
                                          :cache-dir "/var/cache/probe"
                                          :nqr       "q_abc"})))))
  (let [b (batch-commands (commands/kwic-batch "PROBE" "\"hund\""
                                            {:p-attrs [:word] :sample 100}))]
    (is (= ["randomize 1; reduce Last to 100;"] (:sample b))))
  (testing "a batch that asks for no sample has no such command at all"
    (is (nil? (:sample (batch-commands
                        (commands/kwic-batch "PROBE" "\"hund\""
                                          {:p-attrs [:word]})))))))

(deftest context-spec-test
  (is (= "5 words" (commands/context-spec 5)))
  (testing "a unit of text is one region of it either side"
    (is (= "1 s" (commands/context-spec :s)))
    (is (str/includes? (first (:setup (batch-commands
                                       (commands/kwic-batch "PROBE" "\"hund\""
                                                         {:p-attrs [:word]
                                                          :context :s}))))
                       "set Context 1 s;"))))

(deftest count-command-test
  (is (= "group Last match[-1] lemma;"
         (commands/count-command "match[-1]" :lemma)))
  (testing "the whole match is counted with count, whose output differs"
    (is (= "count Last by lemma;"
           (commands/count-command "match..matchend" :lemma)))
    (is (commands/whole-match? "match..matchend"))
    (is (not (commands/whole-match? "match"))))
  (testing "within a region attribute, the regions each value occurs in"
    (is (= "group Last match lemma within text;"
           (commands/count-command "match" :lemma {:within :text}))))
  (testing "only CQP's own positions are spliced in"
    (is (thrown? Exception (commands/count-command "match[-2]" :lemma)))
    (is (thrown? Exception (commands/count-command "match; exit" :lemma)))))

(deftest subset-command-test
  (let [at (fn [anchor] (commands/subset-command {:anchor anchor :attr :lemma
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
      (let [cmd (commands/subset-command {:anchor "match..matchend" :attr :lemma
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
             (mapv first (commands/narrowing {:subset subset :near near})))))
    (testing "nothing asked for, nothing to run"
      (is (= [] (commands/narrowing {})))
      (is (= [] (commands/narrowing {:near {:word " " :distance 5}}))))
    (is (= [:setup :corpus :query :subset :near :sample :size :sort :cat :dump]
           (mapv first (commands/kwic-batch "PROBE" "\"hund\""
                                         {:p-attrs [:word]
                                          :subset  subset
                                          :near    near
                                          :sample  100}))))))

(deftest kwic-batch-near-test
  (testing "the nearby word narrows the result before the sample is drawn
            from it, and before it is counted or ordered"
    (is (= [:setup :corpus :query :near :sample :size :sort :cat :dump]
           (mapv first (commands/kwic-batch "PROBE" "\"hund\""
                                         {:p-attrs [:word]
                                          :near    {:word "kat" :distance 5}
                                          :sample  100})))))
  (let [near {:word "kat" :distance 5}
        b    (batch-commands (commands/kwic-batch "PROBE" "\"hund\""
                                               {:p-attrs [:word] :near near}))]
    (is (= [(commands/near-command near)] (:near b)))))

(deftest stored-kwic-batch-test
  (let [batch (commands/stored-kwic-batch "PROBE" "q_abc"
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
    (is (thrown? Exception (commands/stored-kwic-batch "PROBE" "1abc"
                                                    {:p-attrs [:word]})))))

(deftest sort-attr-test
  (is (= :lemma (commands/sort-attr "lemma")))
  (testing "the fixed modes name no attribute, word included"
    (is (nil? (commands/sort-attr "word")))
    (is (nil? (commands/sort-attr "corpus")))
    (is (nil? (commands/sort-attr "reverse"))))
  (testing "nothing that is not a name reaches a command"
    (is (nil? (commands/sort-attr "lemma; exit")))
    (is (nil? (commands/sort-attr "")))
    (is (nil? (commands/sort-attr nil)))))

(deftest sort-attribute-command-test
  (testing "a mode naming an attribute sorts by it under the same collation"
    (is (= "set ExternalSort on; sort Last by lemma;"
           (commands/sort-command "lemma"))))
  (testing "the reverse sort reads the word from its end"
    (is (= "set ExternalSort on; sort Last by word reverse;"
           (commands/sort-command "reverse")))))

(deftest count-by-command-test
  (testing "a second attribute is counted against at the match"
    (is (= "group Last match lemma by match text_year;"
           (commands/count-command "match" :lemma {:by :text_year})))
    (is (= "group Last matchend[1] word by match pos within text;"
           (commands/count-command "matchend[1]" :word {:by     :pos
                                                     :within :text}))))
  (testing "count has no by, so the whole match ignores it"
    (is (= "count Last by lemma;"
           (commands/count-command "match..matchend" :lemma {:by :pos})))))

(deftest load-command-test
  (is (= "set DataDirectory \"/cache/PROBE\"; PROBE; Last = q_1;"
         (commands/load-command "PROBE" "q_1" "/cache/PROBE")))
  (testing "the name and the directory are guarded"
    (is (thrown? Exception
                 (commands/load-command "PROBE" "q_1; exit" "/cache")))
    (is (thrown? Exception
                 (commands/load-command "PROBE" "q_1" "/c\"; exit")))))

(deftest tabulate-commands-test
  (testing "the TAB-free columns in one command, each annotation in its own"
    (is (= [[:tabulate (str "tabulate Last 0 9 match, matchend, "
                            "match[-5]..match[-1] word, match..matchend word, "
                            "matchend[1]..matchend[5] word, "
                            "match..matchend pos, match..matchend lemma;")]
            [:tabulate "tabulate Last 0 9 match text_id;"]
            [:tabulate "tabulate Last 0 9 match text_title;"]]
           (commands/tabulate-commands "Last" [0 9] 5 [:word :pos :lemma]
                                    [:text_id :text_title])))))

(deftest export-batch-test
  (testing "the result is produced as for a page, then tabulated"
    (is (= [:setup :corpus :query :size :sort :tabulate :tabulate]
           (map first (commands/export-batch "PROBE" "[]"
                                          {:p-attrs      [:word]
                                           :struct-attrs [:text_id]
                                           :context      5
                                           :limit        100}))))
    (is (= [:setup :corpus :query :sample :size :sort :save :tabulate]
           (map first (commands/export-batch "PROBE" "[]"
                                          {:p-attrs      [:word]
                                           :struct-attrs []
                                           :context      5
                                           :limit        100
                                           :sample       10
                                           :nqr          "q_1"
                                           :cache-dir    "/c"})))))
  (testing "a stored result is only read"
    (let [batch (commands/stored-export-batch "PROBE" "q_1"
                                           {:p-attrs      [:word]
                                            :struct-attrs []
                                            :context      5
                                            :limit        100
                                            :cache-dir    "/c"})]
      (is (= [:setup :corpus :size :tabulate] (map first batch)))
      (is (= (str "tabulate q_1 0 99 match, matchend, match[-5]..match[-1] "
                  "word, match..matchend word, matchend[1]..matchend[5] word;")
             (second (last batch)))))))
