(ns dk.cst.corpus-probe.frequency-test
  "Frequency breakdowns and metadata filter value lists; skipped when CWB
  or the dev corpora are missing."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.cqp-test :refer [ctx when-cwb]]
            [dk.cst.corpus-probe.frequency :as frequency]
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
   (testing "a whole corpus cannot be tabled by a structural attribute"
     (is (-> (frequency/frequency-table! ctx ["VISER"] "" :text_author)
             :counts first :error)))))
