(ns dk.cst.corpus-probe.settings-test
  "The settings a reader stores: which params they hold, how they are
  written into one value and read back, and where a preference leaves
  the reader."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.settings :as settings]
            [dk.cst.corpus-probe.url :as url]))

(deftest string-test
  (testing "the settings of a search, and nothing of the search itself"
    (is (= {:corpus "PROBE,VISER" :mode "extended"}
           (settings/params
            (settings/string {:q           "hund"
                              :corpus      ["PROBE" "VISER"]
                              :mode        "extended"
                              :view        "frequencies"
                              :page        "2"
                              :f.text_year "1591"})))))
  (testing "which view a result is shown in belongs to the result, not to
            the reader, so a form seeded from the settings still asks for
            a query"
    (is (= "" (settings/string {:view "frequencies"}))))
  (testing "a cookie value may hold no comma, so the corpora keep the
            encoding a form gives them"
    (is (= "corpus=PROBE%2CVISER"
           (settings/string {:corpus ["PROBE" "VISER"]}))))
  (testing "a box left unticked is stored as the absence a form submits
            it as, so storing again forgets what was unticked"
    (is (= {:in "lemma"}
           (settings/params (settings/string {:in "lemma" :ci nil})))))
  (testing "only what departs from the app's own defaults is stored, so
            a form nobody changed is stored as nothing"
    (is (= "" (settings/string {:mode "simple" :in "word"
                                :sort "corpus" :context "5" :attr "word"
                                :at   "match" :within "sentence"})))
    (is (= "sort=word" (settings/string {:sort "word" :context "5"}))))
  (testing "nothing stored under another name is read back"
    (is (= {:mode "extended"}
           (settings/params "mode=extended&view=frequencies&q=hund&lang=en"))))
  (testing "the settings are stored while they fit in a cookie"
    (is (settings/storable? ""))
    (is (settings/storable? (settings/string {:corpus ["PROBE"]})))
    (is (not (settings/storable?
              (apply str (repeat (inc settings/max-length) "x")))))))

(deftest corpora-test
  (testing "a form is not a search: every corpus ticked and none ticked
            are different states, and only one of them can be searched,
            so each selection is stored as its own thing"
    (let [all    #{"PROBE" "VISER"}
          stored #(settings/string % all)]
      (is (= ""             (stored {:corpus []})))
      (is (= "scope=all"    (stored {:corpus ["PROBE" "VISER"]})))
      (is (= "corpus=PROBE" (stored {:corpus ["PROBE"]})))
      (is (= "scope=chosen" (stored {:corpus [] :scope "chosen"})))
      (testing "and every corpus is stored as a scope rather than by
                name, since a registry of many would not fit in a cookie"
        (let [many (mapv #(str "CORPUS" %) (range 500))]
          (is (= "scope=all" (settings/string {:corpus many} (set many))))))
      (testing "which is written out again for the form that reads it"
        (is (= {:scope "chosen" :corpus ["PROBE" "VISER"]}
               (-> (stored {:corpus ["PROBE" "VISER"]})
                   (settings/params)
                   (url/with-every-corpus ["PROBE" "VISER"]))))))))

(deftest autosave-test
  (testing "storing as you change them is what a reader has until they
            say otherwise, so only turning it off is ever stored"
    (is (true? (settings/autosave? "")))
    (is (true? (settings/autosave? "corpus=PROBE")))
    (is (false? (settings/autosave? "corpus=PROBE&autosave=off"))))
  (testing "it rides with the settings but seeds no form"
    (is (= {:corpus "PROBE"} (settings/params "corpus=PROBE&autosave=off"))))
  (testing "an unticked checkbox submits nothing, which is how the reader
            without a script turns it off"
    (is (= {:settings "corpus=PROBE&autosave=off"}
           (settings/with-autosave {:settings "corpus=PROBE"})))
    (is (= {:settings "corpus=PROBE" :autosave "on"}
           (settings/with-autosave {:settings "corpus=PROBE" :autosave "on"}))))
  (testing "a reset puts it back on with everything else, and a choice
            that names no settings is left alone"
    (is (= {:settings ""} (settings/with-autosave {:settings ""})))
    (is (= {:lang "en"} (settings/with-autosave {:lang "en"})))))

(deftest return-test
  (testing "a choice sends the reader back where they were"
    (is (= "/search?q=hund"
           (settings/return {:lang "en" :return "/search?q=hund"})))
    (is (= "/search?q=hund"
           (settings/return {:settings "sort=word"
                             :return   "/search?q=hund"}))))
  (testing "but a reset goes to the bare form, which is what it did"
    (is (= "/search" (settings/return {:settings ""
                                       :return   "/search?q=hund"})))))
