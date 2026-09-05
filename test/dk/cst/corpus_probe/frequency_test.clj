(ns dk.cst.corpus-probe.frequency-test
  "Frequency breakdowns and metadata filter value lists; skipped when CWB
  or the dev corpora are missing."
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.cache :as cache]
            [dk.cst.corpus-probe.cqp-test :refer [ctx when-cwb]]
            [dk.cst.corpus-probe.frequency :as frequency]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.search :as search]
            [dk.cst.corpus-probe.search-test :refer [da-collator]]
            [dk.cst.corpus-probe.tools-test :refer [with-value-limit]]
            [taoensso.telemere :as t]))

(deftest filter-options-test
  (when-cwb
   (let [{:keys [attrs unlisted]} (frequency/filter-options! ctx ["VISER"
                                                               "TALER"])]
     (testing "attributes keep registry order, values merge over corpora"
       (is (= [:s_id :text_id :text_title :text_year :text_author :text_speaker
               :text_party]
              (map :name attrs)))
       (is (= [{:value "1583" :freqs {"VISER" 1} :total 1}
               {:value "1591" :freqs {"VISER" 1} :total 1}
               {:value "2014" :freqs {"TALER" 1} :total 1}
               {:value "2015" :freqs {"TALER" 1} :total 1}
               {:value "2016" :freqs {"TALER" 1} :total 1}]
              (:rows (nth attrs 3)))))
     (is (= [] unlisted)))
   (testing "an attribute with too many values in one corpus is unlisted"
     ;; text_year has two values in VISER but three in TALER
     (with-value-limit 2
       (let [{:keys [attrs unlisted]} (frequency/filter-options! ctx ["VISER"
                                                                   "TALER"])]
         (is (= [:text_title :text_author :text_speaker :text_party]
                (map :name attrs)))
         (is (= [:s_id :text_id :text_year] unlisted)))))
   (testing "a corpus that cannot be read offers nothing"
     (is (= {:attrs [] :unlisted []}
            ;; the corpus is deliberately unreadable; its warning, and
            ;; the stack trace with it, would only look like a failure
            (t/with-min-level :fatal
              (frequency/filter-options! ctx ["NOSUCH"])))))))

(deftest frequencies-test
  (when-cwb
   (let [freqs (frequency/frequencies! ctx "PROBE" "[pos = \"N.*\"]" :lemma)]
     (is (= {:values ["hund"] :freq 5} (first freqs)))
     (is (= 10 (count freqs))))
   (testing "asked to, each value also counts the texts it occurs in"
     (let [freqs (frequency/frequencies! ctx "PROBE" "[pos = \"N.*\"]" :lemma
                                         {:docs true})]
       (is (= {:values ["hund"] :freq 5 :docs 3} (first freqs)))
       (is (every? #(= 1 (:docs %)) (rest freqs))))
     (testing "except over the whole match, which count cannot"
       (is (= {:values ["hund"] :freq 3}
              (first (frequency/frequencies! ctx "PROBE" "[pos = \"N.*\"]"
                                             :word {:docs true
                                                    :at   "match..matchend"}))))))))

(deftest with-docs-test
  (is (= [{:values ["a"] :freq 3 :docs 2} {:values ["b"] :freq 1 :docs 0}]
         (frequency/with-docs [{:values ["a"] :freq 3} {:values ["b"] :freq 1}]
                              [{:values ["a"] :freq 2}]))))

(deftest frequency-rows-docs-test
  (testing "the texts counted travel with the rows, per corpus"
    (is (= [{:value "hund" :freqs {"A" 5 "B" 1} :docs {"A" 3 "B" 1} :total 6}]
           (frequency/frequency-rows
            [{:corpus "A" :freqs [{:values ["hund"] :freq 5 :docs 3}]}
             {:corpus "B" :freqs [{:values ["hund"] :freq 1 :docs 1}]}]))))
  (testing "and rows that counted none carry none"
    (is (= [{:value "hund" :freqs {"A" 5} :total 5}]
           (frequency/frequency-rows
            [{:corpus "A" :freqs [{:values ["hund"] :freq 5}]}])))))

(deftest groupable-attrs-test
  (when-cwb
   (testing "a word-only corpus offers word and its annotated s-attributes"
     (is (= [:word :s_id :text_id :text_speaker :text_party :text_year]
            (map :name (frequency/groupable-attrs! ctx "TALER")))))))

(deftest merge-frequencies-test
  (testing "values are merged across corpora and sorted by total, then value"
    (is (= [{:value "hund" :freqs {"A" 5 "B" 1} :total 6}
            {:value "borg" :freqs {"B" 2} :total 2}
            {:value "kat" :freqs {"A" 2} :total 2}]
           (frequency/merge-frequencies
            @da-collator
            [{:corpus "A" :freqs [{:values ["hund"] :freq 5}
                                  {:values ["kat"] :freq 2}]}
             {:corpus "B" :freqs [{:values ["borg"] :freq 2}
                                  {:values ["hund"] :freq 1}]}]))))
  (testing "ties in the total are broken by the collation, not by code point"
    (is (= ["and" "ægte" "øl"]
           (map :value
                (frequency/merge-frequencies
                 @da-collator
                 [{:corpus "A" :freqs [{:values ["øl"] :freq 1}
                                       {:values ["ægte"] :freq 1}
                                       {:values ["and"] :freq 1}]}])))))
  (is (= [] (frequency/merge-frequencies @da-collator []))))

(deftest frequency-table-test
  (when-cwb
   (let [table (frequency/frequency-table! ctx ["PROBE" "VISER" "TALER"]
                                        "[pos = \"N.*\"]" "lemma")]
     (testing "per-corpus counts carry the corpus size for relative rates"
       (is (= [{:corpus "PROBE" :tokens 47 :size 15}
               {:corpus "VISER" :tokens 48 :size 16}]
              (take 2 (:counts table)))))
     (testing "a corpus without the attribute fails alone"
       (is (re-find #"groupable" (-> table :counts last :error :message))))
     (testing "rows merge the corpora"
       (is (= {:value "hund" :freqs {"PROBE" 5 "VISER" 1} :total 6}
              (first (:rows table))))))
   (testing "a blank query tables the whole corpus from its lexicon"
     (let [table (frequency/frequency-table! ctx ["PROBE"] "" :lemma)]
       (is (= [{:corpus "PROBE" :tokens 47 :size 47}] (:counts table)))
       (is (= {:value "." :freqs {"PROBE" 6} :total 6} (first (:rows table))))))
   (testing "a whole corpus is tabled by a structural attribute from the
             size of its regions"
     (let [table (frequency/frequency-table! ctx ["PROBE"] "" :text_year)]
       (is (= [{:corpus "PROBE" :tokens 47 :size 47}] (:counts table)))
       (is (:sized table))
       (is (= [{:value "2024" :freqs {"PROBE" 27} :total 27 :tokens {"PROBE" 27}}
               {:value "2023" :freqs {"PROBE" 20} :total 20 :tokens {"PROBE" 20}}]
              (:rows table)))))))

(deftest sized-frequencies-test
  (when-cwb
   (testing "a breakdown by a structural attribute measures each value's text"
     (let [table (frequency/frequency-table! ctx ["PROBE" "VISER"]
                                             "[pos = \"N.*\"]" :text_year)]
       (is (:sized table))
       (is (= {:value "2023" :freqs {"PROBE" 8} :total 8 :tokens {"PROBE" 20}}
              (some #(when (= "2023" (:value %)) %) (:rows table))))))
   (testing "and a filtered one measures the regions kept"
     (let [table (frequency/frequency-table! ctx ["PROBE"] "[pos = \"N.*\"]"
                                             :text_year
                                             {:filter {:text_year #{"2024"}}})]
       (is (= [{:value "2024" :freqs {"PROBE" 7} :total 7 :tokens {"PROBE" 27}}]
              (:rows table)))))
   (testing "a positional attribute measures nothing"
     (is (not (:sized (frequency/frequency-table! ctx ["PROBE"]
                                                  "[pos = \"N.*\"]" :lemma)))))
   (testing "nor does the whole match, which count cannot size"
     (is (not (:sized (frequency/frequency-table!
                       ctx ["PROBE"] "[pos = \"N.*\"]" :text_year
                       {:at "match..matchend"})))))))

(deftest cross-tabulation-test
  (when-cwb
   (testing "each value is counted against the second attribute"
     (is (= {:values ["hund" "2023"] :freq 3}
            (first (frequency/frequencies! ctx "PROBE" "[pos = \"N.*\"]" :lemma
                                           {:by :text_year})))))
   (testing "the second attribute must be groupable too"
     (is (thrown-with-msg? Exception #"groupable"
                           (frequency/frequencies! ctx "PROBE" "[]" :lemma
                                                   {:by "text; exit"}))))
   (let [table (frequency/frequency-table! ctx ["PROBE" "VISER"]
                                           "[pos = \"N.*\"]" "lemma"
                                           {:by "text_year" :docs true})]
     (testing "the corpora are summed into one row per value"
       (is (= {:value "hund" :cells {"2023" 3 "2024" 2 "1591" 1} :total 6}
              (first (:rows table)))))
     (testing "the columns are the values of the second attribute, collated,
               each with its text"
       (is (= [{:value "1583" :total 8 :tokens 29}
               {:value "1591" :total 8 :tokens 19}
               {:value "2023" :total 8 :tokens 20}
               {:value "2024" :total 7 :tokens 27}]
              (:columns table)))
       (is (= 4 (:column-count table)))
       (is (:sized table)))
     (testing "no texts are counted beside a cross-tabulation"
       (is (not (:docs table))))
     (is (= :text_year (:by table))))
   (testing "over the whole match there is no second attribute"
     (is (nil? (:by (frequency/frequency-table!
                     ctx ["PROBE"] "[pos = \"N.*\"]" "word"
                     {:by "text_year" :at "match..matchend"})))))))

(deftest pair-rows-test
  (is (= [{:value "hund" :cells {"2023" 4 "2024" 1} :total 5}]
         (frequency/pair-rows
          [{:corpus "A" :freqs [{:values ["hund" "2023"] :freq 3}]}
           {:corpus "B" :freqs [{:values ["hund" "2023"] :freq 1}
                                {:values ["hund" "2024"] :freq 1}]}]))))

(deftest columns-test
  (let [rows [{:value "a" :cells {"y" 5 "x" 1 "z" 1}}
              {:value "b" :cells {"x" 3}}]]
    (testing "columns are collated, with their totals and their text summed"
      (is (= [{:value "x" :total 4 :tokens 30}
              {:value "y" :total 5 :tokens 10}
              {:value "z" :total 1 :tokens 0}]
             (frequency/columns @da-collator
                                [{:sizes {"x" 10 "y" 10}} {:sizes {"x" 20}}]
                                rows))))
    (testing "without sizes they measure nothing"
      (is (= [{:value "x" :total 4} {:value "y" :total 5} {:value "z" :total 1}]
             (frequency/columns @da-collator [{}] rows))))
    (testing "the most frequent are kept"
      (with-redefs [frequency/column-limit 2]
        (is (= ["x" "y"]
               (map :value (frequency/columns @da-collator [{}] rows))))))))

(deftest stored-breakdown-test
  (when-cwb
   (let [ctx       (assoc ctx :cache-dir (str (fs/create-temp-dir)))
         q         "[pos = \"N.*\"]"
         hunde     "\"hund.*\" %c"
         breakdown #(frequency/frequencies! ctx "PROBE" q :lemma
                                            {:docs true :sort "word"})
         fresh     (breakdown)
         opts      (search/cache-opts ctx "PROBE" q {:sort "word"})
         counting  [(query/count-command "match" :lemma)]
         file      (cache/result-file ctx "PROBE" (:nqr opts))]
     (testing "until a concordance saves the result there is nothing to read"
       (is (nil? (frequency/stored-breakdown! ctx "PROBE" q opts counting))))
     (search/kwic! ctx "PROBE" q {:sort "word"})
     (testing "once one has, the breakdown reads it and agrees with a fresh run"
       (is (= [["hund\t5" "kat\t2" "København\t1" "bord\t1" "dag\t1" "hav\t1"
                "have\t1" "sol\t1" "strand\t1" "ven\t1"]]
              (frequency/stored-breakdown! ctx "PROBE" q opts counting)))
       (is (= fresh (breakdown))))
     (testing "and it is the file that is counted, not the query"
       ;; give the query another query's stored result: if the breakdown
       ;; reads the file rather than running the query, it counts that
       (search/kwic! ctx "PROBE" hunde {:sort "word"})
       (fs/copy (cache/result-file
                 ctx "PROBE"
                 (:nqr (search/cache-opts ctx "PROBE" hunde {:sort "word"})))
                file
                {:replace-existing true})
       (is (= [{:values ["hund"] :freq 5 :docs 3}] (breakdown))))
     (testing "a truncated file is discarded and the query run instead"
       (with-open [f (java.io.RandomAccessFile. file "rw")]
         (.setLength f 12))
       (is (= fresh (t/with-min-level :fatal (breakdown))))
       (is (not (cache/stored? ctx "PROBE" (:nqr opts)))))
     (testing "a sampled concordance is never counted"
       (search/kwic! ctx "PROBE" q {:sort "word" :sample 3})
       (is (cache/stored? ctx "PROBE"
                          (:nqr (search/cache-opts ctx "PROBE" q
                                                   {:sort "word" :sample 3}))))
       (is (= fresh (breakdown)))))))
