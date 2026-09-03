(ns dk.cst.corpus-probe.corpus-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.corpus :as corpus]))

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

(deftest charset-test
  (testing "the registry's charset property maps to a Java charset name"
    ;; registry-file lowercases the corpus name, finding the test fixture
    (is (= "UTF-8" (corpus/charset {:registry "test/resources"}
                                   "REGISTRY-PROBE"))))
  (testing "missing registry entries fall back to UTF-8"
    (is (= "UTF-8" (corpus/charset {:registry "test/resources"} "NOSUCH"))))
  (is (= "ISO-8859-1" (corpus/cwb->charset "latin1"))))
