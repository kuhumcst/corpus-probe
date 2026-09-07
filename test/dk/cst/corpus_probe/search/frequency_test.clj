(ns dk.cst.corpus-probe.search.frequency-test
  "Frequency breakdowns and metadata filter value lists, and that a
  breakdown counts the matches the concordance shows; skipped when CWB
  or the dev corpora are missing."
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.search :as search]
            [dk.cst.corpus-probe.search.frequency :as frequency]
            [dk.cst.corpus-probe.test.cwb
             :refer [ctx da-collator when-cwb with-value-limit]]
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

(deftest concordance-agreement-test
  (when-cwb
   (let [nouns  "[pos = \"N.*\"]"
         phrase "[pos = \"D\"] [pos = \"A.*\"]? [pos = \"N.*\"]"
         near   {:word "katten" :distance 5}]
     (testing "a breakdown kept within a sentence is of the matches the
               concordance keeps"
       ;; a full stop ends one sentence and Hunde opens the next
       (is (empty? (frequency/frequencies! ctx "PROBE"
                                           "[word = \"\\.\"] [word = \"Hunde\"]"
                                           :word {:within :sentence}))))
     (testing "the whole match, counted as the strings it matched"
       (is (= [{:values ["en hund"] :freq 1}
               {:values ["en lille hund"] :freq 1}]
              (frequency/frequencies! ctx "PROBE" phrase :word
                                      {:at "match..matchend"}))))
     (testing "a breakdown at a position, and of the narrowed hits"
       (is (= {:values ["PP"] :freq 6}
              (first (frequency/frequencies! ctx "PROBE" nouns :pos
                                             {:at "match[-1]"}))))
       (is (= [{:values ["hund"] :freq 5}]
              (frequency/frequencies! ctx "PROBE" nouns :lemma
                                      {:subset {:anchor "match"
                                                :attr   :lemma
                                                :value  "hund"}}))))
     (testing "a breakdown counts the hits with the word nearby"
       ;; two noun phrases, only one of them within five words of Katten
       (is (= [{:values ["en"] :freq 1}]
              (frequency/frequencies! ctx "PROBE" phrase :word {:near near}))))
     (testing "a narrowing of nothing is nothing, not CQP's refusal to
               narrow an empty result"
       (is (= [] (frequency/frequencies! ctx "PROBE" "[word = \"nonesuch\"]"
                                         :word {:near near})))
       (is (= [] (frequency/frequencies! ctx "PROBE" nouns :lemma
                                         {:filter {:text_year #{"1591"}}
                                          :near   near}))))
     (testing "a frequency table counts within the filter, tokens included"
       (let [table (frequency/frequency-table!
                    ctx ["VISER"] "[lemma = \"hund\"]" :text_year
                    {:filter {:text_year #{"1591"}}})]
         (is (= [{:corpus "VISER" :tokens 19 :size 1}] (:counts table)))
         (is (= [{:value "1591" :freqs {"VISER" 1} :total 1
                  :tokens {"VISER" 19}}]
                (:rows table)))))
     (testing "a blank query under a filter tables the filtered tokens"
       (let [table (frequency/frequency-table!
                    ctx ["VISER"] "" :lemma {:filter {:text_year #{"1591"}}})]
         (is (= [{:corpus "VISER" :tokens 19 :size 19}] (:counts table)))))
     (testing "a breakdown of the whole corpus under a pattern counts the
               filtered regions"
       (is (= (search/size! ctx "VISER" "[word = \".*\"]"
                            {:patterns {:text_year ["158."]}})
              (:tokens (frequency/corpus-frequencies!
                        ctx "VISER" "" :word
                        {:patterns {:text_year ["158."]}}))))))))

(deftest interpolation-guard-test
  (when-cwb
   (testing "attribute names outside the corpus inventory are rejected"
     (let [canary "/tmp/corpus-probe-pwned-attr"]
       (fs/delete-if-exists canary)
       (is (thrown? Exception
                    (frequency/frequencies!
                     ctx "PROBE" "\"hund\""
                     (str "lemma > \"| touch " canary "\""))))
       (is (not (fs/exists? canary)))))))
