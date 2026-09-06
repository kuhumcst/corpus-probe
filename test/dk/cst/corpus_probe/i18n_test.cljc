(ns dk.cst.corpus-probe.i18n-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.i18n :as i18n]
            #?(:clj [dk.cst.corpus-probe.translations :as translations])))

(def en
  "The lookup context of the source language, in which every string is
  its own msgid."
  (i18n/->ui "en"))

(def da
  "The lookup context of the bundled Danish translation."
  (i18n/->ui "da"))

(deftest tr-test
  (testing "the source language is the msgid, so it needs no table"
    (is (= "Search" (i18n/tr en "Search"))))
  (is (= "Søgning" (i18n/tr da "Search")))
  (testing "a string no translation covers falls back to its own English"
    (is (= "Nonesuch" (i18n/tr da "Nonesuch"))))
  (testing "so does a language we have no table for"
    (is (= "Search" (i18n/tr (i18n/->ui "de") "Search")))))

(deftest fill-test
  (testing "a placeholder takes the value under its key, wherever it stands"
    (is (= "token 2, condition 1"
           (i18n/fill "token {n}, condition {c}" {:n 2 :c 1})))
    (is (= "Betingelse 1 i token 2"
           (i18n/fill "Betingelse {c} i token {n}" {:n 2 :c 1}))))
  (testing "a key the map lacks stands as it is, and a value goes in as
            it is"
    (is (= "token {n}" (i18n/fill "token {n}" {})))
    (is (= "1.200 words" (i18n/fill "{n} words" {:n "1.200"}))))
  (testing "filled after the lookup, so the translation places the value"
    (is (= "token 2" (i18n/tr en "token {n}" {:n 2})))
    (is (= "2 regions" (i18n/trn en "{n} region" "{n} regions" 2 {:n 2})))))

(deftest trx-test
  (testing "one English word several languages split takes a context"
    (is (= "Søg" (i18n/trx da "button" "Search")))
    (is (= "Søgning" (i18n/tr da "Search"))))
  (testing "the context never reaches the page: the fallback is the string"
    (is (= "Search" (i18n/trx en "button" "Search")))
    (is (= "Nonesuch" (i18n/trx da "button" "Nonesuch")))))

(deftest trn-test
  (testing "which form a count takes is the translation's to say"
    (is (= "region" (i18n/trn da "region" "regions" 1)))
    (is (= "regioner" (i18n/trn da "region" "regions" 2)))
    (is (= "forekomst" (i18n/trn da "hit" "hits" 1)))
    (is (= "forekomster" (i18n/trn da "hit" "hits" 9))))
  (testing "an untranslated pair falls back to its own English"
    (is (= "ox" (i18n/trn da "ox" "oxen" 1)))
    (is (= "oxen" (i18n/trn da "ox" "oxen" 3))))
  (testing "zero is plural, as both languages have it"
    (is (= "regioner" (i18n/trn da "region" "regions" 0)))))

(deftest languages-test
  (testing "every language is the source one or a bundled translation"
    (is (= #{"da" "en"} (set i18n/languages))))
  (is (i18n/supported? "da"))
  (is (i18n/supported? "en"))
  (is (not (i18n/supported? "de")))
  (is (not (i18n/supported? nil)))
  (testing "the default is one of them"
    (is (i18n/supported? i18n/default-language))))

(deftest tables-test
  (testing "a table maps English onto the language, plurals as pairs"
    (let [da (get i18n/tables "da")]
      (is (= "Korpusser" (get da "Corpora")))
      (is (= ["værdi" "værdier"] (get da ["value" "values"])))))
  (testing "no translation is blank, which would render as nothing"
    (doseq [[lang table] i18n/tables
            [msgid msgstr] table
            s (if (vector? msgstr) msgstr [msgstr])]
      (is (not= "" s) (str lang " leaves " (pr-str msgid) " empty"))))
  #?(:clj
     (testing "a msgid holding a quote survives the PO round trip, which
               the reader unescapes (see
               dk.cst.corpus-probe.translations/unescape)"
       (is (= "\"x\" eller [lemma = \"x\"]"
              (get (translations/read-po "dk/cst/corpus_probe/quoted.po")
                   "\"x\" or [lemma = \"x\"]"))))))

(deftest group-digits-test
  (testing "English groups with a comma and points its decimals"
    (is (= "0" (i18n/group-digits en 0)))
    (is (= "999" (i18n/group-digits en 999)))
    (is (= "1,000" (i18n/group-digits en 1000)))
    (is (= "64,600,000" (i18n/group-digits en 64600000)))
    (is (= "1,234.5" (i18n/group-digits en 1234.5))))
  (testing "Danish swaps both separators"
    (is (= "999" (i18n/group-digits da 999)))
    (is (= "1.000" (i18n/group-digits da 1000)))
    (is (= "64.600.000" (i18n/group-digits da 64600000)))
    (is (= "1.234,5" (i18n/group-digits da 1234.5)))
    (testing "so a rate cannot be mistaken for a grouped count"
      (is (= "125.000,0" (i18n/group-digits da 125000.0)))))
  (testing "a language we have no format for is written as English"
    (is (= "1,000" (i18n/group-digits (i18n/->ui "de") 1000))))
  (testing "a statistic that could not be computed stays nothing"
    (is (nil? (i18n/group-digits da nil)))))

(deftest fixed-digits-test
  (testing "a rate keeps its one decimal on either platform"
    (is (= "150,000.0" (i18n/group-digits en 150000.0 1)))
    (is (= "150,000.0" (i18n/group-digits en 150000 1)))
    (is (= "1.234,0" (i18n/group-digits da 1234 1)))
    (is (= "106,383.0" (i18n/group-digits en 106382.96 1)))
    (is (nil? (i18n/group-digits en nil 1)))))
