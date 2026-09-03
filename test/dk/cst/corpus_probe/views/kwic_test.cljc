(ns dk.cst.corpus-probe.views.kwic-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.views.kwic :as kwic]))

(deftest token-title-test
  (testing "non-word attributes join into the tooltip"
    (is (= "NCSI · hund" (kwic/token-title {:word "hund" :pos "NCSI"
                                            :lemma "hund"}))))
  (testing "structure tags and blank values are excluded"
    (is (= "NCSI" (kwic/token-title {:word "hund" :pos "NCSI" :lemma ""
                                     :open [:s]})))))

(deftest token-data-test
  (testing "every annotation but the surface word becomes a data-* attribute"
    (is (= {:data-pos "NCSI" :data-lemma "hund"}
           (kwic/token-data {:word "hund" :pos "NCSI" :lemma "hund"}))))
  (testing "structure tags are not emitted as data"
    (is (= {:data-pos "NCSI"}
           (kwic/token-data {:word "x" :pos "NCSI" :open [:s]})))))

(deftest token-test
  (let [m       {:word "hund" :pos "NCSI"}
        structs {:text_title "Hverdag"}
        [_ attrs] (kwic/token structs m)]
    (testing "the surface form is the text content, annotations are data"
      (is (= "hund" (last (kwic/token structs m))))
      (is (= "NCSI" (:data-pos attrs))))
    (testing "a click dispatches :inspect with the token and its structs"
      (is (= [:inspect {:token m :structs structs}]
             (get-in attrs [:on :click]))))))

(deftest source-label-test
  (testing "a text title is a cited work"
    (is (= [:cite "Hverdag"]
           (kwic/source-label {:text_id "t1" :text_title "Hverdag"}))))
  (testing "falls back to text_id, then any value"
    (is (= "t1" (kwic/source-label {:text_id "t1"})))
    (is (= "x" (kwic/source-label {:other "x"})))))

(deftest position-data-test
  (is (= {:data-cpos "9" :data-matchend "10"}
         (kwic/position-data 9 {:matchend 10 :target nil :keyword nil})))
  (testing "target and keyword anchors appear only when set"
    (is (= {:data-cpos "9" :data-matchend "9" :data-target "9"}
           (kwic/position-data 9 {:matchend 9 :target 9 :keyword nil})))))

(def sample-hit
  {:cpos    9
   :anchors {:matchend 9 :target nil :keyword nil}
   :left    [{:word "lille" :pos "AN" :lemma "lille"}]
   :match   [{:word "hund" :pos "NCSI" :lemma "hund"}]
   :right   [{:word "i" :pos "PP" :lemma "i"}]
   :structs {:text_title "Hverdag"}})

(deftest hit-row-test
  (let [row (kwic/hit-row sample-hit false)]
    (testing "the row carries its corpus position as data"
      (is (= "9" (:data-cpos (second row)))))
    (testing "the corpus-position cell is a disclosure button showing the cpos"
      (let [button (get-in row [2 1])]
        (is (= :button (first button)))
        (is (= "9" (last button)))
        (is (= "false" (:aria-expanded (second button))))
        (is (= [:toggle-context {:cpos 9 :matchend 9}]
               (get-in button [1 :on :click])))))
    (testing "aria-expanded tracks the expanded flag"
      (is (= "true" (get-in (kwic/hit-row sample-hit true) [2 1 1 :aria-expanded]))))
    (testing "the match is wrapped in a mark element"
      (is (= :mark (get-in row [5 1 0]))))))

(deftest hit-rows-test
  (testing "a hit with no expansion is a single row"
    (is (= 1 (count (kwic/hit-rows {} sample-hit)))))
  (testing "an expanded hit adds a full-width context row after it"
    (let [ex   {:left [{:word "en"}] :match [{:word "hund"}] :right [{:word "i"}]}
          rows (kwic/hit-rows {9 ex} sample-hit)]
      (is (= 2 (count rows)))
      (is (= :tr.expanded (first (second rows))))
      (is (= 5 (get-in (second rows) [1 1 :colspan])))))
  (testing "a pending (non-map) placeholder shows a loading row"
    (let [rows (kwic/hit-rows {9 :loading} sample-hit)]
      (is (= 2 (count rows)))
      (is (= "…" (get-in (second rows) [1 2]))))))
