(ns dk.cst.corpus-probe.search.opts-test
  "The options of a search as one corpus runs them: the query, the
  context and the metadata filter resolved for the corpus, and what the
  cache adds; skipped where CWB is needed and missing."
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.search.opts :as opts]
            [dk.cst.corpus-probe.test.cwb :refer [cache-ctx! ctx when-cwb]]))

(deftest corpus-query-test
  (when-cwb
   (testing "a query's own within clause is named for the corpus, or dropped
             where the corpus marks no such unit, as the tags are"
     (is (= "<s> [] </s> within s"
            (opts/corpus-query! ctx "PROBE" "<s> [] </s> within s" nil)))
     (is (= "[] []" (opts/corpus-query! ctx "PROBE" "[] [] within p" nil)))
     (is (= "[] [] within text"
            (opts/corpus-query! ctx "PROBE" "[] []" :text))))))

(deftest corpus-context-test
  (let [attrs [{:type :structural :name :s} {:type :structural :name :text}]]
    (testing "a number of words is what it is"
      (is (= 10 (opts/corpus-context attrs 10))))
    (testing "a unit is the corpus's own attribute for it"
      (is (= :s (opts/corpus-context attrs :sentence))))
    (testing "or the usual width where the corpus marks none"
      (is (= 5 (opts/corpus-context attrs :paragraph))))))

(deftest corpus-filter-test
  (when-cwb
   (testing "the triples the filter query takes, patterns beside values"
     (is (= [[:text_title #{} ["Hav.*"]] [:text_year #{"1583"} ["1591"]]]
            (opts/corpus-filter! ctx "VISER"
                                 {:filter   {:text_year #{"1583"}}
                                  :patterns {:text_year  ["1591"]
                                             :text_title ["Hav.*"]}}))))
   (testing "attributes from two levels anchor on the innermost"
     (is (= [[:s_id #{"2"} nil] [:text_year #{"1591"} nil]]
            (opts/corpus-filter! ctx "VISER"
                                 {:filter {:text_year #{"1591"}
                                           :s_id      #{"2"}}}))))
   (testing "a range is the corpus's own values in it, and no more: the
             numbers between that nothing carries are not asked for"
     (is (= [[:text_year #{"1583" "1591"} nil]]
            (opts/corpus-filter! ctx "VISER" {:ranges {:text_year [1000 1600]}})))
     (is (= [[:text_year #{"1591"} nil]]
            (opts/corpus-filter! ctx "VISER" {:ranges {:text_year [1584 1600]}})))
     (testing "beside the values chosen, which it joins"
       (is (= [[:text_year #{"1583" "1591"} nil]]
              (opts/corpus-filter! ctx "VISER"
                                   {:filter {:text_year #{"1583"}}
                                    :ranges {:text_year [1584 1600]}}))))
     (testing "and nothing where the corpus carries none of it"
       (is (= [[:text_year #{} nil]]
              (opts/corpus-filter! ctx "VISER"
                                   {:ranges {:text_year [1700 1800]}})))))
   (testing "nothing restricts nothing"
     (is (nil? (opts/corpus-filter! ctx "VISER" {})))
     (is (nil? (opts/corpus-filter! ctx "VISER" {:filter {} :ranges {}}))))
   (testing "an attribute the corpus lacks is rejected before any command"
     (is (thrown-with-msg? Exception #"Not an annotated structural attribute"
                           (opts/corpus-filter! ctx "TALER"
                                                {:filter {:text_author #{"x"}}})))
     (is (thrown-with-msg? Exception #"Not an annotated structural attribute"
                           (opts/corpus-filter!
                            ctx "TALER" {:ranges {:text_author [1 2]}}))))))

(deftest cache-opts!-test
  (let [ctx (cache-ctx!)]
    (testing "a context keeping a cache names the result and makes the
              corpus's directory"
      (let [{:keys [cache-dir nqr]} (opts/cache-opts! ctx "PROBE" "[]"
                                                      {:sort "word"})]
        (is (= "PROBE" (fs/file-name cache-dir)))
        (is (fs/directory? cache-dir))
        (is (re-matches #"q_[0-9a-f]{32}" nqr))))
    (testing "a false :cache? keeps a one-off query out of it"
      (is (= {:cache? false}
             (opts/cache-opts! ctx "VISER" "[]" {:cache? false})))
      (is (not (fs/exists? (fs/file (:cache-dir ctx) "VISER")))))
    (testing "a context keeping no cache adds nothing"
      (is (= {:sort "word"}
             (opts/cache-opts! (dissoc ctx :cache-dir) "PROBE" "[]"
                               {:sort "word"}))))))
