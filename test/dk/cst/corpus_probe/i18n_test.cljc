(ns dk.cst.corpus-probe.i18n-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.views.layout :as layout]
            [dk.cst.corpus-probe.views.page :as page]))

(deftest dictionary-test
  (testing "every entry says the same thing in exactly the languages served"
    (let [wanted (set (map keyword i18n/languages))]
      (is (empty? (for [[k entry] i18n/dictionary
                        :when     (not= wanted (set (keys entry)))]
                    k))
          "these keys have the wrong languages")
      (is (empty? (for [[k entry] i18n/dictionary
                        [lang s]  entry
                        :when     (or (not (string? s)) (str/blank? s))]
                    [k lang]))
          "these translations are blank"))))

(deftest key-coverage-test
  (testing "every key a lookup table names is in the dictionary"
    ;; `tr` renders an unknown key as its own name rather than failing, so
    ;; a table naming a key nothing defines would reach the page unnoticed
    (is (empty? (remove i18n/dictionary (map first layout/nav-items))))
    (is (empty? (remove i18n/dictionary page/error-types)))
    (is (empty? (remove i18n/dictionary (vals page/error-explanations))))))

(deftest tr-test
  (is (= "Søg" (i18n/tr "da" :submit)))
  (is (= "Search" (i18n/tr "en" :submit)))
  (testing "a key no translation covers renders as itself, not as nothing"
    (is (= "nonesuch" (i18n/tr "da" :nonesuch))))
  (testing "so does a language the dictionary does not have"
    (is (= "submit" (i18n/tr "de" :submit)))))

(deftest supported?-test
  (is (i18n/supported? "da"))
  (is (i18n/supported? "en"))
  (is (not (i18n/supported? "de")))
  (is (not (i18n/supported? nil)))
  (testing "the default is one of them"
    (is (i18n/supported? i18n/default-language))))

(deftest group-digits-test
  (testing "English groups with a comma and points its decimals"
    (is (= "0" (i18n/group-digits "en" 0)))
    (is (= "999" (i18n/group-digits "en" 999)))
    (is (= "1,000" (i18n/group-digits "en" 1000)))
    (is (= "64,600,000" (i18n/group-digits "en" 64600000)))
    (is (= "1,234.5" (i18n/group-digits "en" 1234.5))))
  (testing "Danish swaps both separators"
    (is (= "999" (i18n/group-digits "da" 999)))
    (is (= "1.000" (i18n/group-digits "da" 1000)))
    (is (= "64.600.000" (i18n/group-digits "da" 64600000)))
    (is (= "1.234,5" (i18n/group-digits "da" 1234.5)))
    (testing "so a rate cannot be mistaken for a grouped count"
      (is (= "125.000,0" (i18n/group-digits "da" 125000.0)))))
  (testing "a statistic that could not be computed stays nothing"
    (is (nil? (i18n/group-digits "da" nil)))))
