(ns dk.cst.corpus-probe.parse-test
  "Golden-file tests: every parser runs against byte-exact CQP output
  captured by dev/capture-golden.sh."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.parse :as parse]))

(defn golden-lines
  "The lines of golden file `filename` under test/resources/golden/."
  [filename]
  (str/split (slurp (str "test/resources/golden/" filename)) #"\n"))

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

(deftest tsv->rows-test
  (is (= ["Hunden" "hund" "t1" "Hverdag"]
         (first (parse/tsv->rows (golden-lines "tabulate.tsv"))))))

(deftest count->freqs-test
  (let [freqs (parse/count->freqs (golden-lines "count.txt"))]
    (is (= {:freq 5 :row 5 :value "hund"} (first freqs)))
    (is (= {:freq 1 :row 0 :value "København"} (nth freqs 2)))))

(deftest group->freqs-test
  (testing "unary grouping"
    (is (= {:values ["hund"] :freq 5}
           (first (parse/group->freqs (golden-lines "group.txt"))))))
  (testing "pairwise grouping"
    (is (= {:values ["NCSI" "hund"] :freq 3}
           (first (parse/group->freqs 2 (golden-lines "group-pairwise.txt"))))))
  (testing "a TAB inside a grouped annotation value stays intact (unary)"
    (is (= [{:values ["bad\ttitle"] :freq 7}]
           (parse/group->freqs ["bad\ttitle\t7"])))))

(deftest lexicon->freqs-test
  (let [freqs (parse/lexicon->freqs (golden-lines "lexdecode.tsv"))]
    (testing "entries come out sorted by frequency, in the group shape"
      (is (= [{:values ["."] :freq 6} {:values ["hund"] :freq 5}]
             (take 2 freqs)))
      (is (= 32 (count freqs)))
      (is (apply >= (map :freq freqs))))))

(deftest describe->map-test
  (let [stats (parse/describe->map (golden-lines "describe.txt"))]
    (is (= "PROBE" (:name stats)))
    (is (= 47 (:size stats)))
    (is (= "utf8" (:charset stats)))
    (testing "an empty description is absent, not blank"
      (is (not (contains? stats :description))))
    (testing "per-attribute statistics keep registry order"
      (is (= [{:name :word :tokens 47 :types 36}
              {:name :pos :tokens 47 :types 15}
              {:name :lemma :tokens 47 :types 32}]
             (:p-attrs stats)))
      (is (= {:name :s :regions 6 :values? false}
             (first (:s-attrs stats))))
      (is (= [:s_id :text_id :text_title :text_year]
             (->> (:s-attrs stats) (filter :values?) (map :name)))))
    (is (= [] (:a-attrs stats))))
  (testing "an attribute without data keeps its name and no counts"
    (is (= [{:name :lemma}]
           (:p-attrs (parse/describe->map
                      ["p-ATT lemma                       NO DATA"])))))
  (testing "a description and alignment attributes are captured"
    (let [stats (parse/describe->map
                 ["description:    Danske folkeviser (dev)"
                  "a-ATT viser_probe               3 alignment blocks"])]
      (is (= "Danske folkeviser (dev)" (:description stats)))
      (is (= [{:name :viser_probe :blocks 3}] (:a-attrs stats))))))

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
