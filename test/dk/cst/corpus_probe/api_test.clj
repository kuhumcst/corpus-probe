(ns dk.cst.corpus-probe.api-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.api :as api]))

(deftest ->cqp-test
  (testing "CQP mode passes the query through verbatim"
    (is (= "[lemma = \"hund\"]"
           (api/->cqp {:q "[lemma = \"hund\"]" :mode "cqp"}))))
  (testing "simple mode compiles the query"
    (is (= "[word = \"hund.*\" %c]"
           (api/->cqp {:q "hund" :mode "simple" :prefix "on" :ci "on"}))))
  (testing "blank input yields nil"
    (is (nil? (api/->cqp {:q "   " :mode "cqp"})))
    (is (nil? (api/->cqp {:mode "simple"})))))

(deftest page-href-test
  (let [href (api/page-href {:corpus "PROBE" :q "hund" :page "0"} 2)]
    (is (str/starts-with? href "/?"))
    (is (str/includes? href "corpus=PROBE"))
    (is (str/includes? href "page=2"))
    (testing "the query is URL-encoded"
      (is (str/includes? (api/page-href {:q "[lemma=\"a\"]"} 1) "%5B")))))

(deftest query-string-test
  (is (= "a=1&b=2" (api/query-string {:a 1 :b 2})))
  (testing "nil values are dropped"
    (is (= "a=1" (api/query-string {:a 1 :b nil})))))
