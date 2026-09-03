(ns dk.cst.corpus-probe.tools-test
  "Integration tests for the cwb-* tool wrappers; skipped when CWB or the
  dev corpora are missing."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.corpus :as corpus]
            [dk.cst.corpus-probe.cqp-test :refer [ctx when-cwb]]
            [dk.cst.corpus-probe.tools :as tools]))

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

(defmacro with-value-limit
  "Run `body` with dk.cst.corpus-probe.tools/value-limit bound to `n`
  and the facts cache emptied before and after: the value lists decoded
  under one limit are cached, and would be served under another."
  [n & body]
  `(with-redefs [tools/value-limit ~n]
     (reset! corpus/facts-cache {})
     (try ~@body (finally (reset! corpus/facts-cache {})))))

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
