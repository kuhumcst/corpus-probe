(ns dk.cst.corpus-probe.i18n-scan-test
  "Guards that the committed template and the bundled translations still
  describe the strings the source actually shows."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.i18n-scan :as scan]
            [dk.cst.corpus-probe.translations :as translations]))

(def extracted
  "Every UI string the source names, scanned once: reading and parsing
  the whole source tree is not free."
  (delay (scan/msgids)))

(deftest extractor-test
  (let [extract scan/extractor]
    (testing "a literal, qualified or not"
      (is (= "a" (extract '(tr ui "a"))))
      (is (= "a" (extract '(i18n/tr ui "a")))))
    (testing "a (str ...) of literals is one string, so a long msgid fits"
      (is (= "a b" (extract '(i18n/tr ui (str "a " "b"))))))
    (testing "a context becomes part of the msgid"
      (is (= "button|a" (extract '(i18n/trx ui "button" "a")))))
    (testing "a plural pair becomes one entry"
      (is (= ["a" "as"] (extract '(i18n/trn ui "a" "as" n)))))
    (testing "a string unrelated to a lookup does not extract"
      (is (nil? (extract '(str "a" "b")))))))

(deftest template-drift-test
  (is (= @extracted
         (set (keys (translations/read-po "i18n/template.pot"))))
      "the template is stale; regenerate it with: clojure -M:i18n"))

(deftest translations-complete-test
  (doseq [[lang table] i18n/tables]
    (testing (str "the bundled " lang " translation")
      (is (empty? (remove (set (keys table)) @extracted))
          "these strings the source shows have no translation")
      (is (empty? (remove @extracted (keys table)))
          "these translations are stale: nothing in the source shows them"))))
