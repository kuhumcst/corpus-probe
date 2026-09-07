(ns dk.cst.corpus-probe.cwb.tools-test
  "Golden-file tests of the tool parsers, and integration tests of the
  cwb-* tool wrappers, skipped when CWB or the dev corpora are missing."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.cwb.corpus :as corpus]
            [dk.cst.corpus-probe.cwb.tools :as tools]
            [dk.cst.corpus-probe.test.cwb
             :refer [ctx golden-lines when-cwb with-value-limit]]))

(deftest describe->stats-test
  (let [stats (tools/describe->stats (golden-lines "describe.txt"))]
    (is (= "PROBE" (:name stats)))
    (is (= 47 (:size stats)))
    (is (= "utf8" (:charset stats)))
    (testing "an empty description is absent, not blank"
      (is (not (contains? stats :description))))
    (testing "per-attribute statistics keep registry order"
      (is (= [{:name :word :tokens 47 :types 36}
              {:name :pos :tokens 47 :types 15}
              {:name :lemma :tokens 47 :types 32}]
             (:p-attrs stats)))
      (is (= {:name :s :regions 6 :values? false}
             (first (:s-attrs stats))))
      (is (= [:s_id :text_id :text_title :text_year]
             (->> (:s-attrs stats) (filter :values?) (map :name)))))
    (is (= [] (:a-attrs stats))))
  (testing "an attribute without data keeps its name and no counts"
    (is (= [{:name :lemma}]
           (:p-attrs (tools/describe->stats
                      ["p-ATT lemma                       NO DATA"])))))
  (testing "a description and alignment attributes are captured"
    (let [stats (tools/describe->stats
                 ["description:    Danske folkeviser (dev)"
                  "a-ATT viser_probe               3 alignment blocks"])]
      (is (= "Danske folkeviser (dev)" (:description stats)))
      (is (= [{:name :viser_probe :blocks 3}] (:a-attrs stats))))))

(deftest lexdecode->freqs-test
  (let [freqs (tools/lexdecode->freqs (golden-lines "lexdecode.tsv"))]
    (testing "entries come out sorted by frequency, in the group shape"
      (is (= [{:values ["."] :freq 6} {:values ["hund"] :freq 5}]
             (take 2 freqs)))
      (is (= 32 (count freqs)))
      (is (apply >= (map :freq freqs))))))

(deftest s-decode->freqs-test
  (testing "values come out sorted by value with their region counts"
    (is (= [{:values ["Hverdag"] :freq 1}
            {:values ["Samtale"] :freq 1}
            {:values ["Vejret"] :freq 1}]
           (tools/s-decode->freqs (golden-lines "s-decode.txt")))))
  (testing "repeated values are counted and blank lines skipped"
    (is (= [{:values ["S"] :freq 2} {:values ["V"] :freq 1}]
           (tools/s-decode->freqs ["S" "V" "S" ""])))))

(deftest s-decode->sizes-test
  (testing "each value gets the tokens of its regions"
    (is (= {"Hverdag" 20 "Vejret" 19 "Samtale" 8}
           (tools/s-decode->sizes (golden-lines "s-decode-regions.txt")))))
  (testing "repeated values add up, blank values and lines are skipped"
    (is (= {"S" 5 "V" 1}
           (tools/s-decode->sizes ["0\t2\tS" "3\t3\tV" "4\t5\tS" "6\t7\t"
                                   ""])))))

(deftest describe-corpus-test
  (testing "a hostile corpus name is rejected before any command is built"
    (is (thrown-with-msg? Exception #"Invalid corpus name"
                          (tools/describe-corpus! ctx "PROBE; exit"))))
  (when-cwb
   (let [stats (tools/describe-corpus! ctx "TALER")]
     (is (= 42 (:size stats)))
     (is (= "Folketingstaler (dev)" (:description stats)))
     (testing "the word-only corpus reports a single p-attribute"
       (is (= [:word] (map :name (:p-attrs stats)))))
     (is (= 7 (count (:s-attrs stats)))))
   (testing "an unknown corpus throws instead of returning empty stats"
     (is (thrown? Exception (tools/describe-corpus! ctx "NOSUCH"))))
   (testing "an entry whose data are gone says so the way CQP's error does"
     ;; the tool exits 0 for it, printing ERROR in place of the size, so
     ;; the throw has to carry what a corpus with no entry at all does not
     (let [phantom (fn [corpus reg]
                     (corpus/phantom?
                      (try (tools/describe-corpus! {:registry reg} corpus)
                           (catch Exception e e))))]
       (is (phantom "REGISTRY-PROBE" "test/resources"))
       (is (not (phantom "NOSUCH" (:registry ctx))))))))

(deftest describe-broken-attribute-test
  ;; one attribute whose data files are gone prints NO DATA for that row
  ;; only; the corpus stays describable
  (when-cwb
   (let [reg  (fs/create-temp-dir)
         home (fs/create-temp-dir)]
     (fs/copy-tree (fs/file (:registry ctx) ".." "data" "probe") home)
     (doseq [f (fs/glob home "lemma.*")] (fs/delete f))
     (spit (fs/file reg "probe")
           (-> (slurp (fs/file (:registry ctx) "probe"))
               (str/replace #"(?m)^HOME .*" (str "HOME " home))
               (str/replace #"(?m)^INFO .*" (str "INFO " home "/.info"))))
     (let [stats (tools/describe-corpus! {:registry (str reg)} "PROBE")]
       (is (= 47 (:size stats)))
       (is (= {:name :lemma} (last (:p-attrs stats))))))))

(deftest describe-missing-data-test
  ;; the tool exits 0 for a registry entry whose data files are gone,
  ;; printing ERROR/NO DATA placeholders; the wrapper must throw anyway
  (when-cwb
   (let [reg  (fs/create-temp-dir)
         home (fs/create-temp-dir)]
     (spit (fs/file reg "phantom")
           (-> (slurp (fs/file (:registry ctx) "probe"))
               (str/replace #"(?m)^ID .*" "ID   phantom")
               (str/replace #"(?m)^HOME .*" (str "HOME " home))
               (str/replace #"(?m)^INFO .*" (str "INFO " home "/.info"))))
     (is (thrown? Exception
                  (tools/describe-corpus! {:registry (str reg)} "PHANTOM"))))))

(deftest lexicon-test
  (testing "a hostile corpus name is rejected before any command is built"
    (is (thrown-with-msg? Exception #"Invalid corpus name"
                          (tools/lexicon! ctx "PROBE; exit" :word))))
  (when-cwb
   (testing "attribute names outside the corpus inventory are rejected"
     (is (thrown-with-msg? Exception #"Not a positional attribute"
                           (tools/lexicon! ctx "TALER" "lemma; exit"))))
   (testing "the lexicon comes sorted by frequency in the group shape"
     (is (= {:values ["."] :freq 6} (first (tools/lexicon! ctx "TALER" :word))))
     (is (= 33 (count (tools/lexicon! ctx "TALER" :word)))))))

(deftest annotation-values-test
  (testing "a hostile corpus name is rejected before any command is built"
    (is (thrown-with-msg? Exception #"Invalid corpus name"
                          (tools/annotation-values! ctx "PROBE; exit"
                                                    :text_year))))
  (when-cwb
   (testing "only annotated s-attributes of the corpus can be decoded"
     (is (thrown-with-msg? Exception #"Not an annotated structural attribute"
                           (tools/annotation-values! ctx "TALER" :text_author)))
     (is (thrown-with-msg? Exception #"Not an annotated structural attribute"
                           (tools/annotation-values! ctx "TALER" "text")))
     (is (thrown-with-msg? Exception #"Not an annotated structural attribute"
                           (tools/annotation-values! ctx "TALER" "word"))))
   (testing "the values come sorted with their region counts"
     (is (= [{:values ["2014"] :freq 1}
             {:values ["2015"] :freq 1}
             {:values ["2016"] :freq 1}]
            (tools/annotation-values! ctx "TALER" :text_year)))
     (is (= [{:values ["S"] :freq 2} {:values ["V"] :freq 1}]
            (tools/annotation-values! ctx "TALER" "text_party"))))
   (testing "an attribute with too many values to list is nil"
     (with-value-limit 1
       (is (nil? (tools/annotation-values! ctx "VISER" :text_year)))))))

(deftest annotation-sizes-test
  (testing "a hostile corpus name is rejected before any command is built"
    (is (thrown-with-msg? Exception #"Invalid corpus name"
                          (tools/annotation-sizes! ctx "PROBE; exit"
                                                   :text_year))))
  (when-cwb
   (testing "only annotated s-attributes of the corpus can be decoded"
     (is (thrown-with-msg? Exception #"Not an annotated structural attribute"
                           (tools/annotation-sizes! ctx "TALER" "word"))))
   (testing "each value gets the tokens of its regions, the corpus in all"
     (is (= {"2023" 20 "2024" 27}
            (tools/annotation-sizes! ctx "PROBE" :text_year)))
     (is (= {"S" 28 "V" 14}
            (tools/annotation-sizes! ctx "TALER" "text_party")))
     (is (= 42 (reduce + (vals (tools/annotation-sizes! ctx "TALER"
                                                        :text_party))))))))
