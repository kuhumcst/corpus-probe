(ns dk.cst.corpus-probe.search-test
  "Integration tests for the full search round trip (milestone 1's exit
  criterion); skipped when CWB or the dev corpus is missing."
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.cqp-test :refer [ctx when-cwb]]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.search :as search]
            [taoensso.telemere :as t]
            [dk.cst.corpus-probe.tools-test :refer [with-value-limit]]))

(deftest kwic-test
  (when-cwb
   (let [{:keys [size hits] :as page}
         (search/kwic! ctx "PROBE" "\"hund.*\" %c" {:rows [0 2]})]
     (is (= 5 size))
     (is (= 3 (count hits)))
     (testing "hits carry tokens, anchors and structural metadata"
       (let [{:keys [cpos match anchors structs]} (second hits)]
         (is (= 9 cpos))
         (is (= [{:word "hund" :pos "NCSI" :lemma "hund"}] match))
         (is (= {:match 9 :matchend 9 :target nil :keyword nil} anchors))
         (is (= {:s_id "2" :text_id "t1" :text_title "Hverdag"
                 :text_year "2023"}
                structs))))
     (testing "a later row range"
       (let [page2 (search/kwic! ctx "PROBE" "\"hund.*\" %c" {:rows [3 5]})]
         (is (= 2 (count (:hits page2))))
         (is (= 34 (-> page2 :hits first :cpos))))))))

(deftest simple-search-round-trip-test
  (when-cwb
   (is (= 5 (:size (search/kwic! ctx "PROBE"
                                 (query/simple->cqp "hund"
                                                    {:prefix?           true
                                                     :case-insensitive? true})))))))

(deftest sort-test
  (when-cwb
   (let [order   (fn [opts]
                   (->> (merge {:rows [0 49]} opts)
                        (search/kwic! ctx "PROBE" "[]")
                        :hits
                        (mapv :cpos)))
         natural (order {})
         sorted  (order {:sort "word"})]
     (is (= 47 (count sorted)))
     (testing "word sort reorders the hits away from corpus order"
       (is (not= natural sorted)))
     (testing "context sorts also run and cover the whole result"
       (is (= 47 (count (order {:sort "left"}))))
       (is (= 47 (count (order {:sort "right"})))))
     (testing "an unknown sort mode is corpus order"
       (is (= natural (order {:sort "bogus"})))))))

(deftest danish-collation-test
  ;; requires gawk + the da_DK.UTF-8 locale for CQP's ExternalSort
  (when-cwb
   (let [words (->> (search/kwic! (assoc ctx :sort-locale "da_DK.UTF-8")
                                  "PROBE" "[]" {:sort "word" :rows [0 49]})
                    :hits
                    (mapv (comp :word first :match)))]
     (testing "collation is case-folded Danish, not byte order"
       ;; byte order would sort uppercase Det before lowercase dag
       (is (< (.indexOf words "dag") (.indexOf words "Det"))))
     (testing "o-slash sorts after regular letters within a word"
       (is (< (.indexOf words "Katten") (.indexOf words "København")))))))

(deftest context-expansion-test
  (when-cwb
   (testing "a hit re-fetched by position returns wider context"
     (let [q (query/position-query 9 9)
           {:keys [hits size]} (search/kwic! ctx "PROBE" q
                                             {:context      50
                                              :rows         [0 0]
                                              :struct-attrs []})]
       (is (= 1 size))
       (is (= ["hund"] (map :word (:match (first hits)))))
       (is (pos? (count (:left (first hits)))))))))

(deftest size-test
  (when-cwb
   (is (= 5 (search/size! ctx "PROBE" "\"hund.*\" %c")))
   (testing "corpus-size! reports a failing corpus instead of throwing"
     (is (= 5 (:size (search/corpus-size! ctx "PROBE" "\"hund.*\" %c" {}))))
     (is (= :cqp (-> (search/corpus-size! ctx "TALER" "[lemma = \"x\"]" {})
                     :error :type))))
   (testing "corpus-sizes! keeps the order and stops at the deadline"
     (is (= ["VISER" "PROBE"]
            (map :corpus (search/corpus-sizes! ctx ["VISER" "PROBE"] "[]"
                                               (search/deadline ctx) {}))))
     (is (= [:timeout :timeout]
            (map (comp :type :error)
                 (search/corpus-sizes! ctx ["VISER" "PROBE"] "[]" 0 {})))))))

(deftest filter-test
  (when-cwb
   (let [q "[lemma = \"hund\"]"]
     (testing "a filter restricts the hits to the matching regions"
       (is (= 1 (search/size! ctx "VISER" q {:filter {:text_year #{"1591"}}})))
       (is (= 0 (search/size! ctx "VISER" q {:filter {:text_year #{"1583"}}})))
       (is (= [13] (->> (search/kwic! ctx "VISER" q
                                      {:filter {:text_year #{"1591"}}})
                        :hits
                        (map :cpos)))))
     (testing "several attributes must all hold"
       (is (= 1 (search/size! ctx "VISER" q
                              {:filter {:text_year   #{"1591" "1583"}
                                        :text_author #{"ukendt"}}})))
       (is (= 0 (search/size! ctx "VISER" q
                              {:filter {:text_year   #{"1591"}
                                        :text_author #{"nobody"}}}))))
     (testing "attributes from two levels anchor on the innermost"
       (is (= [[:s_id #{"2"}] [:text_year #{"1591"}]]
              (search/corpus-filter! ctx "VISER" {:text_year #{"1591"}
                                                  :s_id      #{"2"}})))
       (is (nil? (search/corpus-filter! ctx "VISER" {})))
       (is (= 6 (search/size! ctx "VISER" "[]"
                              {:filter {:text_year #{"1591"} :s_id #{"2"}}}))))
     (testing "a corpus lacking the attribute is rejected before any command"
       (let [{:keys [error]} (search/corpus-size! ctx "TALER" "[]"
                                                  {:filter {:text_author
                                                            #{"x"}}})]
         (is (= :rejected (:type error)))
         (is (re-find #"Not an annotated structural attribute"
                      (:message error))))
       (is (thrown-with-msg? Exception #"Not an annotated structural attribute"
                             (search/kwic! ctx "VISER" "[]"
                                           {:filter {:text #{"x"}}}))))
     (testing "a concordance filters every corpus, counted ones included"
       (let [page (search/concordance! ctx ["VISER" "PROBE"] "[]"
                                       {:page-size 1
                                        :filter    {:text_year #{"1591"
                                                                 "2023"}}})]
         (is (= [{:corpus "VISER" :size 19} {:corpus "PROBE" :size 20}]
                (:counts page)))))
     (testing "a frequency table counts within the filter, tokens included"
       (let [table (search/frequency-table! ctx ["VISER"] q :text_year
                                            {:filter {:text_year #{"1591"}}})]
         (is (= [{:corpus "VISER" :tokens 19 :size 1}] (:counts table)))
         (is (= [{:value "1591" :freqs {"VISER" 1} :total 1}] (:rows table)))))
     (testing "a blank query under a filter tables the filtered tokens"
       (let [table (search/frequency-table! ctx ["VISER"] "" :lemma
                                            {:filter {:text_year #{"1591"}}})]
         (is (= [{:corpus "VISER" :tokens 19 :size 19}] (:counts table))))))))

(deftest filter-options-test
  (when-cwb
   (let [{:keys [attrs unlisted]} (search/filter-options! ctx ["VISER"
                                                               "TALER"])]
     (testing "attributes keep registry order, values merge over corpora"
       (is (= [:s_id :text_id :text_title :text_year :text_author :text_speaker
               :text_party]
              (map :name attrs)))
       (is (= [{:value "1583" :freqs {"VISER" 1} :total 1}
               {:value "1591" :freqs {"VISER" 1} :total 1}
               {:value "2014" :freqs {"TALER" 1} :total 1}
               {:value "2015" :freqs {"TALER" 1} :total 1}
               {:value "2016" :freqs {"TALER" 1} :total 1}]
              (:rows (nth attrs 3)))))
     (is (= [] unlisted)))
   (testing "an attribute with too many values in one corpus is unlisted"
     ;; text_year has two values in VISER but three in TALER
     (with-value-limit 2
       (let [{:keys [attrs unlisted]} (search/filter-options! ctx ["VISER"
                                                                   "TALER"])]
         (is (= [:text_title :text_author :text_speaker :text_party]
                (map :name attrs)))
         (is (= [:s_id :text_id :text_year] unlisted)))))
   (testing "a corpus that cannot be read offers nothing"
     (is (= {:attrs [] :unlisted []}
            ;; the corpus is deliberately unreadable; its warning, and
            ;; the stack trace with it, would only look like a failure
            (t/with-min-level :fatal
              (search/filter-options! ctx ["NOSUCH"])))))))

(deftest error-map-test
  (testing "a CQP error travels as it is"
    (is (= {:type :cqp :message "x"}
           (search/error-map (ex-info "failed" {:error {:type :cqp
                                                        :message "x"}})))))
  (testing "one of our own guards is a rejection with its message"
    (is (= {:type :rejected :message "Invalid corpus name"}
           (search/error-map (ex-info "Invalid corpus name" {})))))
  (testing "any other exception is internal, its message withheld"
    (is (= {:type :internal}
           (t/with-min-level :fatal
             (search/error-map (java.io.IOException. "/srv/secret")))))))

(deftest pmap-n-test
  (is (= [1 2 3 4 5] (search/pmap-n 2 inc (range 5))))
  (testing "at most n calls run at once"
    (let [running (atom 0) peak (atom 0)]
      (dorun (search/pmap-n 3 (fn [_]
                                (swap! peak max (swap! running inc))
                                (Thread/sleep 20)
                                (swap! running dec))
                            (range 40)))
      (is (<= @peak 3)))))

(deftest concordance-test
  (when-cwb
   (let [q      "[word = \".*en\" %c]"
         sizes  (fn [corpora] (mapv #(search/size! ctx % q) corpora))
         result (search/concordance! ctx ["PROBE" "VISER" "TALER"] q
                                     {:page-size 5})]
     (testing "the counts cover every corpus in order and sum to the size"
       (is (= ["PROBE" "VISER" "TALER"] (mapv :corpus (:counts result))))
       (is (= (sizes ["PROBE" "VISER" "TALER"])
              (mapv :size (:counts result))))
       (is (= (reduce + (sizes ["PROBE" "VISER" "TALER"])) (:size result))))
     (testing "the first page fills from the first corpus"
       (is (= 5 (count (:hits result))))
       (is (= ["PROBE"] (distinct (map :corpus (:hits result))))))
     (testing "a page straddling two corpora continues into the next"
       (let [[n1] (sizes ["PROBE"])
             page (search/concordance! ctx ["PROBE" "VISER"] q
                                       {:page 1 :page-size (dec n1)})]
         (is (= (dec n1) (count (:hits page))))
         (is (= ["PROBE" "VISER"] (distinct (map :corpus (:hits page)))))
         (is (= (first (mapv :cpos (:hits (search/kwic! ctx "VISER" q))))
                (:cpos (second (:hits page)))))))
     (testing "a page past every corpus has no hits but full counts"
       (let [page (search/concordance! ctx ["PROBE" "VISER"] q {:page 99})]
         (is (empty? (:hits page)))
         (is (= (sizes ["PROBE" "VISER"]) (mapv :size (:counts page))))))
     (testing "a corpus lacking a queried attribute fails alone"
       (let [page (search/concordance! ctx ["TALER" "PROBE"]
                                       "[lemma = \"hund\"]")]
         (is (= :cqp (-> page :counts first :error :type)))
         (is (= 5 (-> page :counts second :size)))
         (is (= 5 (:size page)))
         (is (= ["PROBE"] (distinct (map :corpus (:hits page)))))))
     (testing "an exhausted budget stops querying and reports timeouts"
       (let [page (search/concordance! (assoc ctx :search-budget-ms -1)
                                       ["PROBE" "VISER"] q)]
         (is (= [:timeout :timeout] (map (comp :type :error) (:counts page))))
         (is (empty? (:hits page))))))))

(deftest frequencies-test
  (when-cwb
   (let [freqs (search/frequencies! ctx "PROBE" "[pos = \"N.*\"]" :lemma)]
     (is (= {:values ["hund"] :freq 5} (first freqs)))
     (is (= 10 (count freqs))))))

(deftest groupable-attrs-test
  (when-cwb
   (testing "a word-only corpus offers word and its annotated s-attributes"
     (is (= [:word :s_id :text_id :text_speaker :text_party :text_year]
            (map :name (search/groupable-attrs! ctx "TALER")))))))

(def da-collator
  "The collator of a Danish installation, as the handlers build it."
  (delay (search/->collator {:sort-locale "da_DK.UTF-8"})))

(deftest locale-test
  (testing "an LC_ALL value names its language and territory"
    (is (= "da" (.getLanguage (search/locale "da_DK.UTF-8"))))
    (is (= "DK" (.getCountry (search/locale "da_DK.UTF-8"))))
    (is (= "en" (.getLanguage (search/locale "en_US")))))
  (testing "a value naming no locale is the root one"
    (is (= java.util.Locale/ROOT (search/locale "C")))
    (is (= java.util.Locale/ROOT (search/locale "")))
    (is (= java.util.Locale/ROOT (search/locale nil)))))

(deftest collator-test
  (testing "Danish sorts æ, ø and å after z, not among the vowels"
    (is (= ["and" "brød" "zoo" "ægte" "øl" "århus"]
           (sort @da-collator
                 ["øl" "ægte" "zoo" "århus" "and" "brød"]))))
  (testing "an installation with no sort locale still sorts"
    (is (= ["a" "z"] (sort (search/->collator {}) ["z" "a"])))))

(deftest merge-frequencies-test
  (testing "values are merged across corpora and sorted by total, then value"
    (is (= [{:value "hund" :freqs {"A" 5 "B" 1} :total 6}
            {:value "borg" :freqs {"B" 2} :total 2}
            {:value "kat" :freqs {"A" 2} :total 2}]
           (search/merge-frequencies
            @da-collator
            [{:corpus "A" :freqs [{:values ["hund"] :freq 5}
                                  {:values ["kat"] :freq 2}]}
             {:corpus "B" :freqs [{:values ["borg"] :freq 2}
                                  {:values ["hund"] :freq 1}]}]))))
  (testing "ties in the total are broken by the collation, not by code point"
    (is (= ["and" "ægte" "øl"]
           (map :value
                (search/merge-frequencies
                 @da-collator
                 [{:corpus "A" :freqs [{:values ["øl"] :freq 1}
                                       {:values ["ægte"] :freq 1}
                                       {:values ["and"] :freq 1}]}])))))
  (is (= [] (search/merge-frequencies @da-collator []))))

(deftest frequency-table-test
  (when-cwb
   (let [table (search/frequency-table! ctx ["PROBE" "VISER" "TALER"]
                                        "[pos = \"N.*\"]" "lemma")]
     (testing "per-corpus counts carry the corpus size for relative rates"
       (is (= [{:corpus "PROBE" :tokens 47 :size 15}
               {:corpus "VISER" :tokens 48 :size 16}]
              (take 2 (:counts table)))))
     (testing "a corpus without the attribute fails alone"
       (is (re-find #"groupable" (-> table :counts last :error :message))))
     (testing "rows merge the corpora"
       (is (= {:value "hund" :freqs {"PROBE" 5 "VISER" 1} :total 6}
              (first (:rows table))))))
   (testing "a blank query tables the whole corpus from its lexicon"
     (let [table (search/frequency-table! ctx ["PROBE"] "" :lemma)]
       (is (= [{:corpus "PROBE" :tokens 47 :size 47}] (:counts table)))
       (is (= {:value "." :freqs {"PROBE" 6} :total 6} (first (:rows table))))))
   (testing "a whole corpus cannot be tabled by a structural attribute"
     (is (-> (search/frequency-table! ctx ["VISER"] "" :text_author)
             :counts first :error)))))

(deftest error-reporting-test
  (when-cwb
   (testing "a bad query throws with the CQP error attached"
     (let [e (try (search/kwic! ctx "PROBE" "[pos = ")
                  (catch Exception e (ex-data e)))]
       (is (= :cqp (-> e :error :type)))))))

(deftest query-lock-test
  (when-cwb
   (testing "redirection smuggled after the query is rejected, not executed"
     (let [canary "/tmp/corpus-probe-pwned"]
       (fs/delete-if-exists canary)
       (is (thrown? Exception
                    (search/kwic! ctx "PROBE"
                                  (str "\"hund\"; cat Last > \"| touch "
                                       canary "\""))))
       (is (not (fs/exists? canary)))))))

(deftest interpolation-guard-test
  (testing "hostile corpus names are rejected before any command is built"
    (is (thrown-with-msg? Exception #"Invalid corpus name"
                          (search/corpus-ctx {} "PROBE; exit")))
    (is (thrown-with-msg? Exception #"Invalid corpus name"
                          (search/corpus-ctx {} "probe"))))
  (when-cwb
   (testing "attribute names outside the corpus inventory are rejected"
     (let [canary "/tmp/corpus-probe-pwned-attr"]
       (fs/delete-if-exists canary)
       (is (thrown? Exception
                    (search/frequencies!
                     ctx "PROBE" "\"hund\""
                     (str "lemma > \"| touch " canary "\""))))
       (is (not (fs/exists? canary)))))
   (testing "struct-attrs outside the corpus inventory are rejected"
     (is (thrown? Exception
                  (search/kwic! ctx "PROBE" "\"hund\""
                                {:struct-attrs [:bogus_attr]}))))))
