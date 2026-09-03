(ns dk.cst.corpus-probe.corpus-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.corpus :as corpus]
            [dk.cst.corpus-probe.cqp-test
             :refer [ctx mismatched-entry temp-registry when-cwb]]))

(deftest read-registry-test
  (let [corpus (corpus/read-registry "test/resources/registry-probe")]
    (is (= "probe" (:id corpus)))
    (is (= "" (:name corpus)))
    (is (= "/corpora/data/probe" (:home corpus)))
    (is (= "utf8" (:charset corpus)))
    (is (= "??" (:language corpus)))
    (testing "p-attributes keep declaration (= display) order"
      (is (= [:word :pos :lemma] (:p-attrs corpus))))
    (testing "s-attributes include the split-off annotation attributes"
      (is (some #{:text_title} (:s-attrs corpus))))
    (is (= [] (:aligned corpus)))))

(def fixture
  "The registry entry fixture that needs no encoded corpus."
  "test/resources/registry-probe")

(deftest corpora-test
  (let [reg (temp-registry fixture {"probe2" (mismatched-entry fixture)
                                    "readme" "not a registry entry\n"})]
    (fs/create-dir (fs/file reg "old"))
    (let [corpora (corpus/corpora {:registry reg})]
      (testing "the ID is the filename, which is the name CQP resolves"
        (is (= ["probe" "probe2"] (map :id corpora))))
      (testing "stray files and subdirectories are not corpora"
        (is (= 2 (count corpora)))))))

(deftest language-test
  (is (= "da" (corpus/language {:language "da"})))
  (testing "the ?? placeholder and other junk are not a language"
    (is (nil? (corpus/language {:language "??"})))
    (is (nil? (corpus/language {})))))

(deftest charset-test
  (testing "the registry's charset property maps to a Java charset name"
    ;; registry-file lowercases the corpus name, finding the test fixture
    (is (= "UTF-8" (corpus/charset {:registry "test/resources"}
                                   "REGISTRY-PROBE"))))
  (testing "missing registry entries fall back to UTF-8"
    (is (= "UTF-8" (corpus/charset {:registry "test/resources"} "NOSUCH"))))
  (is (= "ISO-8859-1" (corpus/cwb->charset "latin1"))))

(deftest info-test
  (when-cwb
   (let [info (corpus/info! ctx "VISER")]
     (is (= 48 (:size info)))
     (is (= "da" (-> info :properties :language)))
     (testing "the .info file text comes through verbatim"
       (is (re-find #"folkeviser" (:info info)))))))

(deftest overview-test
  (when-cwb
   (testing "a registry corpus summarizes to an index entry"
     (is (= {:id       "TALER"
             :title    "Folketingstaler (dev)"
             :language "da"
             :size     42}
            (corpus/overview! ctx (corpus/read-registry
                                   (corpus/registry-file ctx "TALER"))))))
   (testing "an unreadable corpus keeps its entry with a nil size"
     (is (nil? (:size (corpus/overview! ctx {:id "nosuch"})))))))

(deftest facts-cache-test
  (let [ctx   {:registry "test/resources"}
        calls (atom 0)
        ;; a label unique to this run, since the cache outlives a test run
        ;; in a long-lived REPL
        label (str "test " (System/nanoTime))
        facts (fn []
                (corpus/with-facts-cache! ctx "REGISTRY-PROBE" label
                                          #(do (swap! calls inc) :facts)))]
    (testing "a value is computed once and shared by concurrent callers"
      (is (every? #{:facts} (pmap (fn [_] (facts)) (range 8))))
      (is (= 1 @calls)))
    (testing "a failure is not cached"
      (let [boom    (fn [] (throw (ex-info "x" {})))
            failing #(corpus/with-facts-cache! ctx "REGISTRY-PROBE" "fail" boom)]
        (is (thrown? Exception (failing)))
        (is (thrown? Exception (failing)))))
    (testing "a changed registry file supersedes the cached entry"
      (let [reg   (temp-registry fixture {})
            ctx   {:registry reg}
            f     (fs/file reg "probe")
            label (str "evict " (System/nanoTime))
            entries (fn []
                      (filter (fn [[[r c l]]]
                                (and (= r reg) (= c "PROBE") (= l label)))
                              @corpus/facts-cache))]
        (is (= :old (corpus/with-facts-cache! ctx "PROBE" label
                                              (constantly :old))))
        (.setLastModified f (+ (.lastModified f) 5000))
        (is (= :new (corpus/with-facts-cache! ctx "PROBE" label
                                              (constantly :new))))
        (is (= 1 (count (entries))))))))

(deftest charset-of-directory-test
  (testing "a subdirectory named like a corpus is not read as an entry"
    (let [reg (temp-registry fixture {})]
      (fs/create-dir (fs/file reg "old"))
      (is (= "UTF-8" (corpus/charset {:registry reg} "OLD"))))))

(deftest overview-failure-test
  (testing "a phantom corpus is summarized without a size"
    (let [e (ex-info "x" {:error {:type :cqp
                                  :message "CQP Error:\n\tCorpus ``X'' is undefined"}})]
      (is (corpus/phantom? e))
      (is (not (corpus/phantom? (ex-info "x" {:error {:type :timeout}}))))))
  (testing "any other failure to read the size propagates"
    (is (thrown? Exception
                 (corpus/overview! {:registry (temp-registry fixture {})
                                    :cqp      "no-such-cqp"}
                                   {:id "probe"})))))
