(ns dk.cst.corpus-probe.i18n.po-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.i18n.po :as po]))

(deftest unescape-test
  (testing "a backslash quotes the character after it, whatever it is"
    (is (= "\"x\"" (po/unescape "\\\"x\\\"")))
    (is (= "a\\b" (po/unescape "a\\\\b")))
    (is (= "plain" (po/unescape "plain")))))

(deftest read-po-test
  (testing "a msgid holding a quote survives the PO round trip, which
            the reader unescapes (see `unescape`)"
    (is (= "\"x\" eller [lemma = \"x\"]"
           (get (po/read-po "dk/cst/corpus_probe/quoted.po")
                "\"x\" or [lemma = \"x\"]"))))
  (testing "the bundled tables are keyed by language, plurals as pairs"
    (let [da (get (po/tables) "da")]
      (is (= "Korpusser" (get da "Corpora")))
      (is (= ["værdi" "værdier"] (get da ["value" "values"]))))))
