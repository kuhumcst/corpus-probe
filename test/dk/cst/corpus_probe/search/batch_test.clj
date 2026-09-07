(ns dk.cst.corpus-probe.search.batch-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.cwb.command :as command]
            [dk.cst.corpus-probe.search.batch :as batch]))

(defn batch-commands
  "The commands of `b` (a batch) by section, so that a batch reads by name
  rather than by position."
  [b]
  (batch/batch-sections b (mapv second b)))

(deftest hardened-profile-test
  (testing "every separator is a TAB-framed marker letter"
    (is (str/includes? batch/hardened-profile
                       "set AttributeSeparator \"\tA\t\";"))
    (is (str/includes? batch/hardened-profile
                       "set StructureDelimiter \"\tS\t\";"))
    (is (str/ends-with? batch/hardened-profile "set ShowTagAttributes off;"))))

(deftest page-rows-test
  (is (= [0 24] (batch/page-rows 0 25)))
  (is (= [20 29] (batch/page-rows 2 10)))
  (testing "negative or zero paging values are clamped, not passed to CQP"
    (is (= [0 0] (batch/page-rows -1 0))))
  (testing "a page beyond CQP's int range is clamped below it"
    (let [[from to] (batch/page-rows 100000000 25)]
      (is (<= to batch/max-row))
      (is (= 24 (- to from))))
    (is (<= (second (batch/page-rows Long/MAX_VALUE 25)) batch/max-row))))

(deftest batch-sections-test
  (is (= {:size [["5"]] :cat [["a"] ["b"]]}
         (batch/batch-sections [[:size "size Last;"]
                                [:cat "cat Last 0 0;"]
                                [:cat "cat Last 1 1;"]]
                               [["5"] ["a"] ["b"]])))
  (testing "a batch nothing ran has no sections"
    (is (= {} (batch/batch-sections [] [])))))

(deftest kwic-batch-test
  (let [opts {:p-attrs      [:word :pos :lemma]
              :struct-attrs [:text_id :text_title]
              :rows         [20 29]
              :sort         "word"}
        b    (batch-commands (batch/kwic-batch "PROBE" "\"hund\"" opts))]
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
                          (batch/kwic-batch "PROBE" "\"hund\""
                                            {:p-attrs [:word]}))))))
  (testing "the default rows are the first page"
    (is (= ["dump Last 0 24;"]
           (:dump (batch-commands (batch/kwic-batch "PROBE" "\"hund\""
                                                    {:p-attrs [:word]})))))))

(deftest kwic-batch-cache-test
  (testing "without a cache the batch neither saves nor sets DataDirectory"
    (let [b (batch-commands (batch/kwic-batch "PROBE" "\"hund\""
                                              {:p-attrs [:word]}))]
      (is (nil? (:save b)))
      (is (not (str/includes? (first (:setup b)) "DataDirectory")))))
  (testing "with a cache the sorted result is saved under the given name"
    (let [b (batch-commands
             (batch/kwic-batch "PROBE" "\"hund\""
                               {:p-attrs   [:word]
                                :cache-dir "/var/cache/probe"
                                :nqr       "q_abc"}))]
      (is (str/includes? (first (:setup b))
                         "set DataDirectory \"/var/cache/probe\"; "))
      (is (= ["q_abc = Last; save q_abc;"] (:save b)))
      (testing "the page still comes from Last, which is what was sorted"
        (is (= ["dump Last 0 24;"] (:dump b))))))
  (testing "the name is guarded like every other value spliced in"
    (is (thrown? Exception (batch/kwic-batch "PROBE" "\"hund\""
                                             {:p-attrs [:word] :nqr "1bad"})))))

(deftest kwic-batch-order-test
  (testing "DataDirectory precedes activation; the name comes after the sort"
    (is (= [:setup :corpus :query :size :sort :save :cat :dump]
           (mapv first (batch/kwic-batch "PROBE" "\"hund\""
                                         {:p-attrs   [:word]
                                          :cache-dir "/var/cache/probe"
                                          :nqr       "q_abc"}))))))

(deftest kwic-batch-sample-test
  (testing "the sample is drawn before the result is counted or ordered"
    ;; the size to report is the sample's, and CQP's reduce discards the
    ;; sort order of the result it reduces
    (is (= [:setup :corpus :query :sample :size :sort :cat :dump]
           (mapv first (batch/kwic-batch "PROBE" "\"hund\""
                                         {:p-attrs [:word] :sample 100})))))
  (testing "and what is saved is the sample, sorted"
    (is (= [:setup :corpus :query :sample :size :sort :save :cat :dump]
           (mapv first (batch/kwic-batch "PROBE" "\"hund\""
                                         {:p-attrs   [:word]
                                          :sample    100
                                          :cache-dir "/var/cache/probe"
                                          :nqr       "q_abc"})))))
  (let [b (batch-commands (batch/kwic-batch "PROBE" "\"hund\""
                                            {:p-attrs [:word] :sample 100}))]
    (is (= ["randomize 1; reduce Last to 100;"] (:sample b))))
  (testing "a batch that asks for no sample has no such command at all"
    (is (nil? (:sample (batch-commands
                        (batch/kwic-batch "PROBE" "\"hund\""
                                          {:p-attrs [:word]})))))))

(deftest context-spec-test
  (is (= "5 words" (batch/context-spec 5)))
  (testing "a unit of text is one region of it either side"
    (is (= "1 s" (batch/context-spec :s)))
    (is (str/includes? (first (:setup (batch-commands
                                       (batch/kwic-batch "PROBE" "\"hund\""
                                                         {:p-attrs [:word]
                                                          :context :s}))))
                       "set Context 1 s;"))))

(deftest kwic-batch-narrowing-test
  (testing "the subset, the word nearby and the sample narrow the result
            in that order, before it is counted or ordered"
    (is (= [:setup :corpus :query :subset :near :sample :size :sort :cat :dump]
           (mapv first (batch/kwic-batch "PROBE" "\"hund\""
                                         {:p-attrs [:word]
                                          :subset  {:anchor "match"
                                                    :attr   :lemma
                                                    :value  "hund"}
                                          :near    {:word "kat" :distance 5}
                                          :sample  100}))))))

(deftest kwic-batch-near-test
  (testing "the nearby word narrows the result before the sample is drawn
            from it, and before it is counted or ordered"
    (is (= [:setup :corpus :query :near :sample :size :sort :cat :dump]
           (mapv first (batch/kwic-batch "PROBE" "\"hund\""
                                         {:p-attrs [:word]
                                          :near    {:word "kat" :distance 5}
                                          :sample  100})))))
  (let [near {:word "kat" :distance 5}
        b    (batch-commands (batch/kwic-batch "PROBE" "\"hund\""
                                               {:p-attrs [:word] :near near}))]
    (is (= [(command/near-command near)] (:near b)))))

(deftest size-batch-test
  (testing "the activation, the query, the narrowings, the sample and the
            size, in that order"
    (is (= [:corpus :query :subset :near :sample :size]
           (mapv first (batch/size-batch "PROBE" "\"hund\""
                                         {:subset {:anchor "match"
                                                   :attr   :lemma
                                                   :value  "hund"}
                                          :near   {:word "kat" :distance 5}
                                          :sample 10})))))
  (testing "which is what every batch over a result opens with"
    (is (= [:corpus :query :size]
           (mapv first (batch/size-batch "PROBE" "\"hund\"" {}))))
    (is (= (mapv first (batch/size-batch "PROBE" "[]" {:sample 5}))
           (subvec (mapv first (batch/kwic-batch "PROBE" "[]"
                                                 {:p-attrs [:word] :sample 5}))
                   1 5)))))

(deftest count-batch-test
  (let [counting [(command/count-command "match" :lemma)
                  (command/count-command "match" :lemma {:within :text})]
        b        (batch/count-batch "PROBE" "\"hund\""
                                    {:sample 10 :near {:word "kat" :distance 5}}
                                    counting)
        sections (batch-commands b)]
    (testing "the query and its narrowings, then a count section a command"
      (is (= [:corpus :query :near :size :count :count] (mapv first b)))
      (is (= counting (:count sections))))
    (testing "never the sample, a count of a sample being no count"
      (is (nil? (:sample sections))))))

(deftest stored-count-batch-test
  (let [counting [(command/count-command "match" :lemma)]
        b        (batch/stored-count-batch "PROBE" "q_1" {:cache-dir "/c"}
                                           counting)
        sections (batch-commands b)]
    (testing "the result loaded, sized and put back into corpus order, then
              counted"
      (is (= [:load :size :sort :count] (mapv first b)))
      (is (= ["set DataDirectory \"/c\"; PROBE; Last = q_1;"]
             (:load sections)))
      (is (= ["size Last;"] (:size sections)))
      (is (= ["sort Last;"] (:sort sections)))
      (is (= counting (:count sections))))
    (testing "the name is guarded"
      (is (thrown? Exception
                   (batch/stored-count-batch "PROBE" "1bad" {:cache-dir "/c"}
                                             counting))))))

(deftest stored-kwic-batch-test
  (let [b        (batch/stored-kwic-batch "PROBE" "q_abc"
                                          {:p-attrs      [:word :pos]
                                           :struct-attrs [:text_id]
                                           :rows         [25 49]
                                           :cache-dir    "/var/cache/probe"})
        sections (batch-commands b)]
    (testing "nothing is queried, sorted or saved"
      (is (= [:setup :corpus :size :cat :dump :tabulate] (mapv first b))))
    (testing "every row command reads the stored result"
      (is (= ["size q_abc;"] (:size sections)))
      (is (str/includes? (first (:cat sections)) "cat q_abc 25 49;"))
      (is (= ["dump q_abc 25 49;"] (:dump sections)))
      (is (= ["tabulate q_abc 25 49 match text_id;"] (:tabulate sections))))
    (testing "the display profile and context are still set per request"
      (is (str/includes? (first (:setup sections)) "set Context 5 words;"))))
  (testing "the name is guarded here too"
    (is (thrown? Exception (batch/stored-kwic-batch "PROBE" "1abc"
                                                    {:p-attrs [:word]})))))

(deftest tabulate-commands-test
  (testing "the TAB-free columns in one command, each annotation in its own"
    (is (= [[:tabulate (str "tabulate Last 0 9 match, matchend, "
                            "match[-5]..match[-1] word, match..matchend word, "
                            "matchend[1]..matchend[5] word, "
                            "match..matchend pos, match..matchend lemma;")]
            [:tabulate "tabulate Last 0 9 match text_id;"]
            [:tabulate "tabulate Last 0 9 match text_title;"]]
           (batch/tabulate-commands "Last" [0 9] 5 [:word :pos :lemma]
                                    [:text_id :text_title])))))

(deftest export-batch-test
  (testing "the result is produced as for a page, then tabulated"
    (is (= [:setup :corpus :query :size :sort :tabulate :tabulate]
           (map first (batch/export-batch "PROBE" "[]"
                                          {:p-attrs      [:word]
                                           :struct-attrs [:text_id]
                                           :context      5
                                           :limit        100}))))
    (is (= [:setup :corpus :query :sample :size :sort :save :tabulate]
           (map first (batch/export-batch "PROBE" "[]"
                                          {:p-attrs      [:word]
                                           :struct-attrs []
                                           :context      5
                                           :limit        100
                                           :sample       10
                                           :nqr          "q_1"
                                           :cache-dir    "/c"})))))
  (testing "a stored result is only read"
    (let [b (batch/stored-export-batch "PROBE" "q_1"
                                       {:p-attrs      [:word]
                                        :struct-attrs []
                                        :context      5
                                        :limit        100
                                        :cache-dir    "/c"})]
      (is (= [:setup :corpus :size :tabulate] (map first b)))
      (is (= (str "tabulate q_1 0 99 match, matchend, match[-5]..match[-1] "
                  "word, match..matchend word, matchend[1]..matchend[5] word;")
             (second (last b)))))))
