(ns dk.cst.corpus-probe.cwb.parse-test
  "Golden-file tests: every parser runs against byte-exact CQP output
  captured by dev/capture-golden.sh."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.cwb.parse :as parse]
            [dk.cst.corpus-probe.test.cwb :refer [golden-lines]]))

(deftest markers-test
  (testing "the letters the golden files were captured with"
    (is (= {:attribute "A" :token "T" :left "L" :right "R" :structure "S"}
           parse/markers))))

(deftest kwic-line->hit-test
  (testing "hardened-profile KWIC lines parse into complete hits"
    (let [hits (parse/kwic->hits [:word :pos :lemma]
                                 (golden-lines "kwic-hardened.txt"))]
      (is (= 5 (count hits)))
      (is (= {:cpos  9
              :left  [{:word "." :pos "PUN" :lemma "."}
                      {:word "Katten" :pos "NCSD" :lemma "kat"}
                      {:word "jagter" :pos "VPRA" :lemma "jagte"}
                      {:word "en" :pos "D" :lemma "en"}
                      {:word "lille" :pos "AN" :lemma "lille"}]
              :match [{:word "hund" :pos "NCSI" :lemma "hund"}]
              :right [{:word "i" :pos "PP" :lemma "i"}
                      {:word "haven" :pos "NCSD" :lemma "have"}
                      {:word "." :pos "PUN" :lemma "."}
                      {:word "Hunde" :pos "NCPI" :lemma "hund"}
                      {:word "og" :pos "CC" :lemma "og"}]}
             (second hits)))
      (testing "match at corpus start has an empty left context"
        (is (= [] (:left (first hits)))))
      (testing "UTF-8 values survive"
        (is (= "være" (-> hits (nth 2) :right (nth 2) :lemma)))))))

(deftest kwic-structure-tags-test
  (testing "structure tags attach to the neighbouring token"
    (let [hits (parse/kwic->hits [:word :pos :lemma]
                                 (golden-lines "kwic-hardened-structs.txt"))
          hit  (first hits)]
      (is (= [:s] (-> hit :match first :open)))
      (is (= [:s] (->> hit :right (keep :close) first))))))

(deftest clip-context-test
  (let [word  (fn [w & tags] (into {:word w} tags))
        ;; a context wide enough to run out of the text at both ends
        hit   {:left  [(word "før") (word "." {:close [:text]})
                       (word "Her" {:open [:text]}) (word "står")]
               :match [(word "det")]
               :right [(word "skrevet") (word "." {:close [:text]})
                       (word "Så" {:open [:text]}) (word "videre")]}
        words (fn [hit k] (mapv :word (get hit k)))]
    (testing "the context is cut back to the text the match is in, the
              tokens bounding it kept"
      (let [clipped (parse/clip-context :text hit)]
        (is (= ["Her" "står"] (words clipped :left)))
        (is (= ["skrevet" "."] (words clipped :right)))))
    (testing "and says which ends of its text it has reached, so that a
              reader travelling along the line knows where to stop asking
              for more of it"
      (is (= #{:start :end} (:bounds (parse/clip-context :text hit))))
      (let [open-ended (update hit :right
                               #(vec (take-while (fn [t] (not (:close t))) %)))]
        (is (= #{:start} (:bounds (parse/clip-context :text open-ended))))))
    (testing "no attribute to clip at, or no tag on any token, leaves the
              hit as it is"
      (is (= hit (parse/clip-context nil hit)))
      (is (= hit (parse/clip-context :p hit))))))

(deftest show-cd->attributes-test
  (let [attrs (parse/show-cd->attributes (golden-lines "show-cd.txt"))]
    (is (= 9 (count attrs)))
    (is (= {:type :positional :name :word :values? false :shown? true}
           (first attrs)))
    (testing "annotated s-attributes are marked"
      (is (= [:s_id :text_id :text_title :text_year]
             (->> attrs (filter :values?) (map :name)))))))

(deftest dump->anchors-test
  (let [anchors (parse/dump->anchors (golden-lines "dump.tsv"))]
    (is (= 5 (count anchors)))
    (is (= {:match 9 :matchend 9 :target nil :keyword nil} (second anchors)))))

(deftest size->n-test
  (is (= 47 (parse/size->n ["47"]))))

(deftest tabulate->rows-test
  (testing "one section per attribute, one line per hit, into a row per hit"
    (is (= [["t1" "2023"] ["t2" "2024"]]
           (parse/tabulate->rows [["t1" "t2"] ["2023" "2024"]]))))
  (testing "a TAB inside a value survives, since nothing is split"
    (is (= [["bad\ttitle"]] (parse/tabulate->rows [["bad\ttitle"]]))))
  (testing "no sections, no rows"
    (is (= [] (parse/tabulate->rows [])))
    (is (= [] (parse/tabulate->rows nil)))))

(deftest group->freqs-test
  (is (= {:values ["hund"] :freq 5}
         (first (parse/group->freqs (golden-lines "group.txt")))))
  (testing "a TAB inside a grouped annotation value stays intact"
    (is (= [{:values ["bad\ttitle"] :freq 7}]
           (parse/group->freqs ["bad\ttitle\t7"])))))

(deftest count->freqs-test
  (is (= [{:values ["en hund"] :freq 3} {:values ["en lille hund"] :freq 1}]
         (parse/count->freqs ["3\t0\ten hund" "1\t3\ten lille hund"]))))

(deftest info->map-test
  (let [info (parse/info->map (golden-lines "info.txt"))]
    (is (= "PROBE" (:name info)))
    (is (= 47 (:size info)))
    (is (= "utf8" (:charset info)))
    (is (= {:language "??" :charset "utf8"} (:properties info)))
    (is (= "No further information available about PROBE" (:info info))))
  (testing "colon-shaped lines in the .info body stay verbatim in :info"
    (let [info (parse/info->map ["Name:    PROBE" "Size:    47" ""
                                 "A Danish corpus." "Name: not a header"
                                 "Contact: someone@example.org"])]
      (is (= "PROBE" (:name info)))
      (is (= (str "A Danish corpus.\nName: not a header\n"
                  "Contact: someone@example.org")
             (:info info))))))

(deftest kwic-line-hostile-values-test
  (testing "a CR inside a token value does not break the line parse"
    (let [line (str "        0: \tT\t\tL\ta\rb\tA\tP\tA\tl\tR\t")
          hit  (parse/kwic-line->hit [:word :pos :lemma] line)]
      (is (= [{:word "a\rb" :pos "P" :lemma "l"}] (:match hit)))))
  (testing "a non-KWIC line throws instead of returning garbage"
    (is (thrown? Exception (parse/kwic-line->hit [:word] "not a kwic line")))))

(deftest group-pairs->freqs-test
  (testing "the counted value comes first, though CQP prints it second"
    (is (= [{:values ["hund" "2023"] :freq 3}]
           (parse/group-pairs->freqs ["2023\thund\t3"]))))
  (testing "a TAB inside the value counted against stays intact"
    (is (= [{:values ["hund" "bad\ttitle"] :freq 7}]
           (parse/group-pairs->freqs ["bad\ttitle\thund\t7"])))))
