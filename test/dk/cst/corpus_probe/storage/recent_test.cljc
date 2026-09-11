(ns dk.cst.corpus-probe.storage.recent-test
  "The searches a reader has made lately: which params name the question
  one asked, what remembering one does to the list, and how the list is
  written and read back."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.storage.recent :as recent]))

(deftest asked-test
  (testing "the question: the query, the corpora it was asked of and the
            metadata filter"
    (is (= {:q "hund" :corpus ["PROBE"] :f.text_year "1591"}
           (recent/asked {:q "hund" :corpus ["PROBE"] :f.text_year "1591"}))))
  (testing "not how the answer is read, which is the reader's own and
            belongs to their settings"
    (is (= {:q "hund"}
           (recent/asked {:q    "hund" :sort "word" :context "10"
                          :view "frequencies" :page "2" :attr "lemma"}))))
  (testing "nor the narrowings worked from beside an answer, which refine
            a question the history holds already"
    (is (= {:q "hund"}
           (recent/asked {:q      "hund" :sample "50" :near "kat"
                          :distance "3" :subset "NOUN"
                          :subset-at "match" :subset-attr "pos"})))))

(deftest refined?-test
  (testing "a narrowed answer counts a part of what the question found"
    (is (true? (recent/refined? {:q "hund" :sample "50"})))
    (is (true? (recent/refined? {:q "hund" :near "kat"})))
    (is (true? (recent/refined? {:q "hund" :subset "NOUN"}))))
  (testing "and a qualifier narrows nothing on its own, as an empty
            field narrows nothing"
    (is (false? (recent/refined? {:q "hund" :distance "3"})))
    (is (false? (recent/refined? {:q "hund" :sample "" :near nil})))
    (is (false? (recent/refined? {:q "hund"})))))

(deftest entry-test
  (testing "the question as a URL cites it, with what the answer reported
            beside it"
    (is (= {:params "q=hund&corpus=PROBE,VISER"
            :hits   12
            :filter "text_year 1591"}
           (recent/entry {:q "hund" :corpus ["PROBE" "VISER"] :sort "word"}
                         {:hits 12 :filter "text_year 1591"}))))
  (testing "a fact the answer did not report is left out rather than
            written as nothing"
    (is (= {:params "q=hund"}
           (recent/entry {:q "hund"} {:hits nil :filter nil}))))
  (testing "params that ask nothing are no search to remember"
    (is (nil? (recent/entry {:corpus ["PROBE"]} {:hits 12})))))

(deftest href-test
  (testing "the result of the search remembered, from the string as it
            was written: a filter names one param per value, and reading
            it back would keep the last of them alone"
    (is (= "/search?q=hund&f.text_year=1591&f.text_year=1592#results"
           (recent/href
            {:params "q=hund&f.text_year=1591&f.text_year=1592"})))))

(deftest remember-test
  (let [entry (fn [q] {:params (str "q=" q)})]
    (testing "the newest first"
      (is (= [(entry "kat") (entry "hund")]
             (-> []
                 (recent/remember (entry "hund"))
                 (recent/remember (entry "kat"))))))
    (testing "the same question moves rather than repeats, and takes what
              the answer said this time"
      (is (= [{:params "q=hund" :hits 12} (entry "kat")]
             (recent/remember [(entry "kat") (entry "hund")]
                              {:params "q=hund" :hits 12}))))
    (testing "and keeps what it said before where it says nothing now,
              which is how a narrowed answer leaves the count alone"
      (is (= [{:params "q=hund" :hits 12 :filter "text_year 1591"}]
             (recent/remember [{:params "q=hund" :hits 12
                                :filter "text_year 1591"}]
                              (entry "hund")))))
    (testing "the oldest go once there are more than the list holds"
      (let [full (reduce recent/remember [] (map entry (range 20)))]
        (is (= recent/max-entries (count full)))
        (is (= (entry 19) (first full)))))
    (testing "and a pasted list of thousands of words is a search like
              any other, kept whole"
      (let [long-entry (fn [q] {:params (str "q=" (str/join (repeat 6000 q)))})
            kept       (reduce recent/remember [] (map long-entry ["a" "b"]))]
        (is (= 2 (count kept)))
        (is (= (long-entry "b") (first kept)))))))

(deftest entries-test
  (testing "what was written, read back"
    (let [kept (recent/remember [] {:params "q=hund" :hits 12})]
      (is (= kept (recent/entries (recent/string kept))))))
  (testing "nothing stored is no history"
    (is (= [] (recent/entries nil)))
    (is (= [] (recent/entries ""))))
  (testing "and what another version of this wrote is left behind rather
            than read"
    (is (= [] (recent/entries "not edn [")))
    (is (= [] (recent/entries (pr-str {:params "q=hund"}))))
    (is (= [] (recent/entries (pr-str [{:query "q=hund"}]))))
    (is (= [] (recent/entries (pr-str [{:params "" :hits 1}]))))
    (is (= [] (recent/entries (pr-str [{:params "q=hund" :hits "many"}]))))))
