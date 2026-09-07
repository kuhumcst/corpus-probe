(ns dk.cst.corpus-probe.cwb.corpus-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.cwb.corpus :as corpus]
            [dk.cst.corpus-probe.cwb.registry :as registry]
            [dk.cst.corpus-probe.test.cwb
             :refer [ctx encode! fixture phantom-ctx! temp-registry! when-cwb]]
            [taoensso.telemere :as t]))

(deftest corpus-ctx-test
  (testing "hostile corpus names are rejected before any command is built"
    (is (thrown-with-msg? Exception #"Invalid corpus name"
                          (corpus/corpus-ctx {} "PROBE; exit")))
    (is (thrown-with-msg? Exception #"Invalid corpus name"
                          (corpus/corpus-ctx {} "probe"))))
  (testing "the corpus's own charset joins the context"
    (is (= "UTF-8" (:charset (corpus/corpus-ctx {:registry "test/resources"}
                                                "REGISTRY-PROBE"))))))

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
            (corpus/overview! ctx (registry/entry
                                   (registry/entry-file ctx "TALER"))))))
   (testing "an unreadable corpus keeps its entry with a nil size"
     (is (nil? (:size (corpus/overview! ctx {:id "nosuch"})))))))

(deftest facts-cache-data-test
  (testing "re-encoding the data in place supersedes the cached entry,
            though the registry entry is byte-identical"
    (let [ctx   (encode! "aaa")
          label (str "encode " (System/nanoTime))]
      (is (= :old (corpus/facts! ctx "PROBE" label (constantly :old))))
      (spit (fs/file (:home ctx) "word.corpus") "bbbb")
      (is (= :new (corpus/facts! ctx "PROBE" label (constantly :new)))))))

(deftest facts-cache-test
  (let [ctx   {:registry "test/resources"}
        calls (atom 0)
        ;; a label unique to this run, since the cache outlives a test run
        ;; in a long-lived REPL
        label (str "test " (System/nanoTime))
        facts (fn []
                (corpus/facts! ctx "REGISTRY-PROBE" label
                               #(do (swap! calls inc) :facts)))]
    (testing "a value is computed once and shared by concurrent callers"
      (is (every? #{:facts} (pmap (fn [_] (facts)) (range 8))))
      (is (= 1 @calls)))
    (testing "a failure is not cached"
      (let [boom    (fn [] (throw (ex-info "x" {})))
            failing #(corpus/facts! ctx "REGISTRY-PROBE" "fail" boom)]
        (is (thrown? Exception (failing)))
        (is (thrown? Exception (failing)))))
    (testing "a changed registry file supersedes the cached entry"
      (let [reg   (temp-registry! fixture {})
            ctx   {:registry reg}
            f     (fs/file reg "probe")
            label (str "evict " (System/nanoTime))
            entries (fn []
                      (filter (fn [[[r c l]]]
                                (and (= r reg) (= c "PROBE") (= l label)))
                              @corpus/facts-cache))]
        (is (= :old (corpus/facts! ctx "PROBE" label (constantly :old))))
        (.setLastModified f (+ (.lastModified f) 5000))
        (is (= :new (corpus/facts! ctx "PROBE" label (constantly :new))))
        (is (= 1 (count (entries))))))))

(deftest overview-failure-test
  (testing "a phantom corpus is summarized without a size"
    (let [e (ex-info "x" {:error {:type :cqp
                                  :message "CQP Error:\n\tCorpus ``X'' is undefined"}})]
      (is (corpus/phantom? e))
      (is (not (corpus/phantom? (ex-info "x" {:error {:type :timeout}}))))))
  (testing "a cwb-* tool says it with a flag instead of CQP's wording"
    (is (corpus/phantom? (ex-info "x" {:phantom? true})))
    (is (not (corpus/phantom? (ex-info "x" {:corpus "X"})))))
  (testing "any other failure to read the size propagates"
    (is (thrown? Exception
                 (corpus/overview! {:registry (temp-registry! fixture {})
                                    :cqp      "no-such-cqp"}
                                   {:id "probe"})))))

(deftest attribute-test
  (let [attributes [{:type :positional :name :word :values? false}
                    {:type :structural :name :s :values? false}
                    {:type :structural :name :text_year :values? true}]]
    (testing "an attribute is found by name, string or keyword"
      (is (= {:type :positional :name :word :values? false}
             (corpus/attribute attributes "word")))
      (is (= :text_year (:name (corpus/attribute attributes :text_year))))
      (is (nil? (corpus/attribute attributes "nonesuch")))
      (is (nil? (corpus/attribute attributes nil))))
    (testing "the predicates over a description"
      (is (corpus/positional? (corpus/attribute attributes :word)))
      (is (not (corpus/positional? (corpus/attribute attributes :s))))
      (is (corpus/annotated-s-attr? (corpus/attribute attributes :text_year)))
      (is (not (corpus/annotated-s-attr? (corpus/attribute attributes :s))))
      (is (= [true false true] (map corpus/countable-attr? attributes))))
    (testing "the names matching one"
      (is (= [:s :text_year]
             (corpus/attr-names #(= :structural (:type %)) attributes))))))

(deftest unit-attr-test
  (let [attrs (fn [& names]
                (mapv (fn [n] {:type :structural :name n}) names))]
    (testing "a sentence is s in CWB's own corpora, sentence at KU"
      (is (= :s (corpus/unit-attr (attrs :text :s) :sentence)))
      (is (= :sentence (corpus/unit-attr (attrs :sentence :text) :sentence))))
    (testing "a corpus marking no sentences restricts nothing"
      (is (nil? (corpus/unit-attr (attrs :text) :sentence))))
    (testing "a positional attribute by the same name does not count"
      (is (nil? (corpus/unit-attr [{:type :positional :name :s}] :sentence))))))

(deftest corpus-lang-test
  (let [entries [{:id "probe" :language "??"}
                 {:id "dan1" :language "da"}]]
    (testing "a plausible language code is returned"
      (is (= "da" (corpus/corpus-lang entries "DAN1"))))
    (testing "a placeholder language is ignored"
      (is (nil? (corpus/corpus-lang entries "PROBE"))))
    (testing "an unknown corpus yields nil"
      (is (nil? (corpus/corpus-lang entries "NOPE"))))))

(deftest split-known-test
  (is (= [["PROBE"] ["NOPE"]]
         (corpus/split-known [{:id "probe"}] ["PROBE" "NOPE"]))))

(deftest readable-corpora-test
  (when-cwb
   (testing "every dev corpus reads, in registry order, named as CQP names it"
     (is (= ["PROBE" "TALER" "VISER"]
            (corpus/readable-corpora! ctx (registry/entries ctx)))))
   (testing "an entry CWB has no data for is not readable, and no error"
     (let [ctx (phantom-ctx!)]
       (is (= [] (corpus/readable-corpora! ctx (registry/entries ctx))))))))

(deftest corpus-tree-test
  (when-cwb
   (testing "a phantom still appears in the tree, sizeless"
     (let [ctx (phantom-ctx!)]
       (is (= [{:label   nil
                :corpora [{:id "PROBE" :title nil :language nil :size nil}]
                :folders []}]
              (corpus/corpus-tree! ctx (registry/entries ctx))))))
   (testing "and so does an entry that cannot be read right now"
     (let [ctx (assoc (phantom-ctx!) :cqp "no-such-cqp")]
       ;; the corpus is deliberately unreadable; its error, and the stack
       ;; trace with it, would only look like a failing test
       (is (= [{:label   nil
                :corpora [{:id "PROBE" :title nil :language nil}]
                :folders []}]
              (t/with-min-level :fatal
                (corpus/corpus-tree! ctx (registry/entries ctx)))))))))
