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
    (testing "a string with placeholders filled from a map is the string"
      (is (= "token {n}" (extract '(i18n/tr ui "token {n}" {:n 2}))))
      (is (= ["{n} word" "{n} words"]
             (extract '(i18n/trn ui "{n} word" "{n} words" n {:n n})))))
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

(defn placeholders
  "The placeholder keys of UI string `s` (see dk.cst.corpus-probe.i18n/fill)."
  [s]
  (set (map second (re-seq #"\{(\w+)\}" (str s)))))

(deftest placeholders-kept-test
  (doseq [[lang table] i18n/tables
          [msgid msgstr] table]
    (testing (str "the " lang " translation of " (pr-str msgid))
      ;; a plural entry is a pair of forms, each against its own
      (doseq [[id str] (if (vector? msgid)
                         (map vector msgid msgstr)
                         [[msgid msgstr]])]
        (is (= (placeholders id) (placeholders str))
            "the translation must keep the placeholders of its msgid")))))

