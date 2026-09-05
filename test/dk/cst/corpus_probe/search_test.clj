(ns dk.cst.corpus-probe.search-test
  "Integration tests for the full search round trip (milestone 1's exit
  criterion); skipped when CWB or the dev corpus is missing."
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [dk.cst.corpus-probe.cache :as cache]
            [dk.cst.corpus-probe.cqp :as cqp]
            [dk.cst.corpus-probe.cqp-test :refer [ctx when-cwb]]
            [dk.cst.corpus-probe.frequency :as frequency]
            [dk.cst.corpus-probe.commands :as commands]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.search :as search]
            [taoensso.telemere :as t]
            [dk.cst.corpus-probe.tools-test :refer [with-value-limit]]))

(use-fixtures :each
  ;; the count memo and the reaping timestamp are both process-wide: one
  ;; test's count would otherwise answer the next one's question, and a
  ;; test that reaps would silence the reaping of every test after it
  (fn [f]
    (cache/forget-counts!)
    (reset! cache/last-reap 0)
    (reset! cache/in-flight {})
    (f)
    (cache/forget-counts!)
    (reset! cache/last-reap 0)
    (reset! cache/in-flight {})))

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

(deftest unit-attr-test
  (let [attrs (fn [& names]
                (mapv (fn [n] {:type :structural :name n}) names))]
    (testing "a sentence is s in CWB's own corpora, sentence at KU"
      (is (= :s (search/unit-attr (attrs :text :s) :sentence)))
      (is (= :sentence (search/unit-attr (attrs :sentence :text) :sentence))))
    (testing "a corpus marking no sentences restricts nothing"
      (is (nil? (search/unit-attr (attrs :text) :sentence))))
    (testing "a positional attribute by the same name does not count"
      (is (nil? (search/unit-attr [{:type :positional :name :s}] :sentence))))))

(deftest within-test
  (when-cwb
   ;; a full stop ends one sentence and Hunde opens the next
   (let [q "[word = \"\\.\"] [word = \"Hunde\"]"]
     (testing "the two words match across the boundary unless kept within
               a sentence, in the count as in the concordance"
       (is (= 1 (search/size! ctx "PROBE" q)))
       (is (= 0 (search/size! ctx "PROBE" q {:within :sentence})))
       (is (= 0 (:size (search/kwic! ctx "PROBE" q {:within :sentence})))))
     (testing "the clause names the corpus's own sentence attribute"
       (is (= (str q " within s")
              (:query (search/kwic! ctx "PROBE" q {:within :sentence})))))
     (testing "and a breakdown is of the same matches"
       (is (empty? (frequency/frequencies! ctx "PROBE" q :word
                                           {:within :sentence})))))))

(deftest pattern-filter-test
  (when-cwb
   (let [q "[word = \".*\"]"]
     (testing "a pattern accepts every value it matches, beside the values"
       (is (= (search/size! ctx "VISER" q {:filter {:text_year #{"1583"}}})
              (search/size! ctx "VISER" q {:patterns {:text_year ["158."]}})))
       (is (= (search/size! ctx "VISER" q)
              (search/size! ctx "VISER" q {:patterns {:text_year ["15.."]}})
              (search/size! ctx "VISER" q {:filter   {:text_year #{"1583"}}
                                           :patterns {:text_year ["1591"]}}))))
     (testing "the triples the filter query takes, patterns beside values"
       (is (= [[:text_title #{} ["Hav.*"]] [:text_year #{"1583"} ["1591"]]]
              (search/corpus-filter! ctx "VISER" {:text_year #{"1583"}}
                                     {:text_year  ["1591"]
                                      :text_title ["Hav.*"]}))))
     (testing "and a breakdown of the whole corpus under a pattern counts
               the filtered regions"
       (is (= (search/size! ctx "VISER" q {:patterns {:text_year ["158."]}})
              (:tokens (frequency/corpus-frequencies!
                        ctx "VISER" "" :word
                        {:patterns {:text_year ["158."]}}))))))))

(deftest corpus-context-test
  (let [attrs [{:type :structural :name :s} {:type :structural :name :text}]]
    (testing "a number of words is what it is"
      (is (= 10 (search/corpus-context attrs 10))))
    (testing "a unit is the corpus's own attribute for it"
      (is (= :s (search/corpus-context attrs :sentence))))
    (testing "or the usual width where the corpus marks none"
      (is (= 5 (search/corpus-context attrs :paragraph))))))

(deftest context-unit-test
  (when-cwb
   (let [hit (fn [context]
               (first (:hits (search/kwic! ctx "PROBE" "[word = \"hund\"]"
                                           {:rows [0 0] :context context}))))]
     (testing "a hit shown with its sentence: the whole of it, however long"
       (let [{:keys [left right]} (hit :sentence)]
         (is (= ["Katten" "jagter" "en" "lille"] (mapv :word left)))
         (is (= ["i" "haven" "."] (mapv :word right)))))
     (testing "a unit the corpus lacks shows the usual width instead"
       (is (= 5 (count (:left (hit :paragraph)))))))))

(deftest subset-test
  (when-cwb
   (let [q    "[pos = \"N.*\"]"
         size (fn [subset] (search/size! ctx "PROBE" q {:subset subset}))
         page (fn [subset] (search/kwic! ctx "PROBE" q {:subset subset}))]
     (is (= 15 (search/size! ctx "PROBE" q)))
     (testing "kept to a value at the start or the end of the match, of a
               positional or a structural attribute"
       (is (= 5 (size {:anchor "match" :attr :lemma :value "hund"})))
       (is (= 7 (size {:anchor "matchend" :attr :text_year :value "2024"}))))
     (testing "kept to a value on the token beside the match, which is
               then marked as the keyword"
       (let [{:keys [size hits]}
             (page {:anchor "match[-1]" :attr :pos :value "D"})]
         (is (= 1 size))
         (is (= 33 (-> hits first :anchors :keyword))))
       (is (= 2 (size {:anchor "matchend[1]" :attr :lemma :value "i"}))))
     (testing "an attribute the corpus lacks is refused before any command"
       (is (thrown? Exception
                    (size {:anchor "match" :attr :nonesuch :value "x"}))))
     (testing "the whole match, counted as the strings it matched and
               narrowed to one of them"
       (let [q "[pos = \"D\"] [pos = \"A.*\"]? [pos = \"N.*\"]"]
         (is (= [{:values ["en hund"] :freq 1}
                 {:values ["en lille hund"] :freq 1}]
                (frequency/frequencies! ctx "PROBE" q :word
                                        {:at "match..matchend"})))
         (is (= [33] (->> (search/kwic! ctx "PROBE" q
                                        {:subset {:anchor "match..matchend"
                                                  :attr   :word
                                                  :value  "en hund"}})
                          :hits
                          (mapv :cpos))))))
     (testing "a breakdown at a position, and of the narrowed hits"
       (is (= {:values ["PP"] :freq 6}
              (first (frequency/frequencies! ctx "PROBE" q :pos
                                             {:at "match[-1]"}))))
       (is (= [{:values ["hund"] :freq 5}]
              (frequency/frequencies! ctx "PROBE" q :lemma
                                      {:subset {:anchor "match"
                                                :attr   :lemma
                                                :value  "hund"}})))))))

(deftest near-test
  (when-cwb
   ;; two noun phrases, only one of them within five words of Katten
   (let [q    "[pos = \"D\"] [pos = \"A.*\"]? [pos = \"N.*\"]"
         near {:word "katten" :distance 5}
         page (search/kwic! ctx "PROBE" q {:near near})]
     (testing "only the hits with the word nearby remain, the word marked
               as their keyword, in the count as in the concordance"
       (is (= 2 (search/size! ctx "PROBE" q)))
       (is (= 1 (search/size! ctx "PROBE" q {:near near})))
       (is (= 1 (:size page)))
       (is (= 5 (-> page :hits first :anchors :keyword))))
     (testing "a breakdown counts the same hits"
       (is (= [{:values ["en"] :freq 1}]
              (frequency/frequencies! ctx "PROBE" q :word {:near near}))))
     (testing "and the word is reported back with the result"
       (is (= near (:near (search/concordance! ctx ["PROBE"] q
                                               {:near near}))))))))

(deftest narrowing-nothing-test
  (when-cwb
   (let [q      "[pos = \"N.*\"]"
         none   "[word = \"nonesuch\"]"
         filter {:text_year #{"1591"}}
         near   {:word "katten" :distance 5}
         subset {:anchor "match" :attr :lemma :value "hund"}]
     (testing "a narrowing of nothing is nothing, not CQP's refusal to
               narrow an empty result"
       (is (= 0 (search/size! ctx "PROBE" none {:near near})))
       (is (= 0 (search/size! ctx "PROBE" none {:subset subset})))
       (is (= {:size 0 :hits []}
              (select-keys (search/kwic! ctx "PROBE" none {:near near})
                           [:size :hits])))
       (is (= 0 (:size (search/kwic! ctx "PROBE" none {:subset subset}))))
       (is (= [] (frequency/frequencies! ctx "PROBE" none :word {:near near})))
       (is (= [] (:rows (search/export! ctx "PROBE" none
                                       {:near near :limit 10})))))
     (testing "which a metadata filter that leaves a corpus no region makes
               of any query"
       (is (= 0 (search/size! ctx "PROBE" q {:filter filter})))
       (is (= 0 (search/size! ctx "PROBE" q {:filter filter :near near})))
       (is (= 0 (:size (search/kwic! ctx "PROBE" q {:filter filter
                                                    :subset subset}))))
       (is (= [] (frequency/frequencies! ctx "PROBE" q :lemma
                                         {:filter filter :near near}))))
     (testing "and of a chain of narrowings, the first of which empties
               the result for the second"
       (let [none {:anchor "match" :attr :lemma :value "nonesuch"}]
         (is (= 0 (search/size! ctx "PROBE" q {:subset none :near near})))
         (is (= 0 (:size (search/kwic! ctx "PROBE" q {:subset none
                                                      :near   near}))))))
     (testing "while a narrowing of something still narrows"
       (is (= 5 (search/size! ctx "PROBE" q {:subset subset})))
       ;; two of the five nouns with the lemma hund have katten nearby
       (is (= 2 (search/size! ctx "PROBE" q {:subset subset :near near})))))))

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
       (is (= natural (order {:sort "no such mode"})))))))

(deftest sample-test
  (when-cwb
   (let [all    (search/size! ctx "PROBE" "[]")
         drawn  (fn [n opts]
                  (mapv :cpos (:hits (search/kwic! ctx "PROBE" "[]"
                                                   (merge {:sample n
                                                           :rows   [0 49]}
                                                          opts)))))
         sample (drawn 5 {})]
     (is (= 47 all))
     (testing "a sample holds that many of the matches, and the size the
               search reports is the sample's"
       (is (= 5 (count sample)))
       (is (= 5 (search/size! ctx "PROBE" "[]" {:sample 5}))))
     (testing "the same hits every time, so one URL names one sample"
       (is (= sample (drawn 5 {}))))
     (testing "the sample is of the matches rather than of an order of
               them, so sorting shows the same hits in another order"
       (is (= (set sample) (set (drawn 5 {:sort "word"})))))
     (testing "a sample of more hits than there are is the whole result"
       (is (= all (search/size! ctx "PROBE" "[]" {:sample 1000}))))
     (testing "and asking for no sample leaves the result whole"
       (is (= all (search/size! ctx "PROBE" "[]" {:sample nil})))
       (is (= all (search/size! ctx "PROBE" "[]" {:sample 0})))))))

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
     (let [q (commands/position-query 9 9)
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
       (is (= [[:s_id #{"2"} nil] [:text_year #{"1591"} nil]]
              (search/corpus-filter! ctx "VISER" {:text_year #{"1591"}
                                                  :s_id      #{"2"}}
                                     nil)))
       (is (nil? (search/corpus-filter! ctx "VISER" {} nil)))
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
       (let [table (frequency/frequency-table! ctx ["VISER"] q :text_year
                                            {:filter {:text_year #{"1591"}}})]
         (is (= [{:corpus "VISER" :tokens 19 :size 1}] (:counts table)))
         (is (= [{:value "1591" :freqs {"VISER" 1} :total 1
                  :tokens {"VISER" 19}}]
                (:rows table)))))
     (testing "a blank query under a filter tables the filtered tokens"
       (let [table (frequency/frequency-table! ctx ["VISER"] "" :lemma
                                            {:filter {:text_year #{"1591"}}})]
         (is (= [{:corpus "VISER" :tokens 19 :size 19}] (:counts table))))))))

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

(deftest incremental-concordance-test
  (when-cwb
   (let [q       "[]"
         corpora ["PROBE" "VISER" "TALER"]
         page    #(select-keys (search/concordance! ctx corpora q
                                                    {:incremental? true})
                               [:counts :size :remaining])]
     (testing "the corpora past the page are left uncounted, and named"
       (is (= {:counts    [{:corpus "PROBE" :size 47}]
               :size      47
               :remaining ["VISER" "TALER"]}
              (page))))
     (testing "a count made before is reported from memory"
       (search/size! ctx "VISER" q)
       (is (= {:counts    [{:corpus "PROBE" :size 47} {:corpus "VISER" :size 48}]
               :size      95
               :remaining ["TALER"]}
              (page))))
     (testing "and once every corpus is counted nothing remains"
       (search/size! ctx "TALER" q)
       (is (= {:counts [{:corpus "PROBE" :size 47} {:corpus "VISER" :size 48}
                        {:corpus "TALER" :size 42}]
               :size   137}
              (page)))))))

(deftest remember-size-test
  (when-cwb
   (let [q "\"hund.*\" %c"]
     (is (nil? (search/known-size ctx "PROBE" q {})))
     (testing "a page remembers the count its batch made"
       (search/concordance! ctx ["PROBE"] q {:incremental? true})
       (is (= 5 (search/known-size ctx "PROBE" q {})))
       (with-redefs [search/run-size!
                     (fn [& _] (throw (ex-info "counted again" {})))]
         (is (= 5 (search/size! ctx "PROBE" q)))))
     (testing "a narrowing of nothing is remembered like any other count"
       (let [opts {:near {:word "x" :distance 5}}]
         (is (= 0 (search/size! ctx "PROBE" "\"nonesuch\"" opts)))
         (is (= 0 (search/known-size ctx "PROBE" "\"nonesuch\"" opts))))))))

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
                    (frequency/frequencies!
                     ctx "PROBE" "\"hund\""
                     (str "lemma > \"| touch " canary "\""))))
       (is (not (fs/exists? canary)))))
   (testing "struct-attrs outside the corpus inventory are rejected"
     (is (thrown? Exception
                  (search/kwic! ctx "PROBE" "\"hund\""
                                {:struct-attrs [:bogus_attr]}))))))

(defn cache-ctx
  "`ctx` with a cache directory of its own, so that one test's stored
  results are never another's."
  []
  (assoc ctx :cache-dir (str (fs/create-temp-dir))))

(defn stored-name
  "The name `corpus` stores the result of `query` under `opts` beneath, as
  `search/kwic!` computes it."
  [ctx corpus query opts]
  (:nqr (search/kwic-opts! ctx corpus query opts)))

(deftest kwic-cache-test
  (when-cwb
   (let [ctx  (cache-ctx)
         q    "\"hund.*\" %c"
         page (search/kwic! ctx "PROBE" q {:rows [0 2]})]
     (testing "the search stores its result and answers as it always did"
       (is (= 5 (:size page)))
       (is (= 3 (count (:hits page))))
       (is (cache/stored? ctx "PROBE" (stored-name ctx "PROBE" q {}))))
     (testing "a second request for it gives the same answers"
       (is (= page (search/kwic! ctx "PROBE" q {:rows [0 2]}))))
     (testing "a different page comes out of the same stored result"
       (is (seq (:hits page)))
       (is (= (drop 3 (:hits (search/kwic! ctx "PROBE" q {:rows [0 4]})))
              (:hits (search/kwic! ctx "PROBE" q {:rows [3 4]}))))
       (is (= 1 (count (fs/list-dir (cache/corpus-directory ctx "PROBE"))))))
     (testing "nothing is left behind under a pending name"
       (is (= 1 (count (fs/list-dir (cache/corpus-directory ctx "PROBE")))))))))

(deftest kwic-cache-touch-test
  (when-cwb
   (let [ctx  (cache-ctx)
         q    "\"hund.*\" %c"
         _    (search/kwic! ctx "PROBE" q {})
         file (cache/result-file ctx "PROBE" (stored-name ctx "PROBE" q {}))]
     (.setLastModified file 1000000)
     (search/kwic! ctx "PROBE" q {:rows [1 2]})
     (testing "reading a stored result keeps it from being reaped"
       (is (> (.lastModified file) 1000000))))))

(deftest kwic-cache-failure-test
  (when-cwb
   (let [ctx (cache-ctx)]
     (testing "a query CQP rejects stores nothing, not even a pending file"
       (is (thrown? Exception (search/kwic! ctx "PROBE" "[bogus = " {})))
       (is (empty? (fs/list-dir (cache/corpus-directory ctx "PROBE")))))
     (testing "a cache that cannot be written answers the search anyway"
       (let [broken (assoc ctx :cache-dir "/dev/null/nope")]
         ;; /dev/null is not a directory, so cqp cannot save into it
         (is (= 5 (:size (search/kwic! broken "PROBE"
                                       "\"hund.*\" %c" {})))))))))

(deftest running-ctx-test
  (testing "a batch that runs the query gets the longer timeout"
    (is (= 900000
           (:timeout-ms (search/running-ctx {:timeout-ms       60000
                                             :query-timeout-ms 900000})))))
  (testing "with none configured, every batch keeps the ordinary timeout"
    (is (= 60000 (:timeout-ms (search/running-ctx {:timeout-ms 60000}))))
    (is (nil? (:timeout-ms (search/running-ctx {}))))))

(deftest within-deadline-test
  (let [soon (+ (System/currentTimeMillis) 5000)
        ctx  (search/within-deadline {:timeout-ms       60000
                                      :query-timeout-ms 300000}
                                     soon)]
    (testing "no batch may outlive the budget the search was given"
      (is (<= (:timeout-ms ctx) 5000))
      (is (<= (:query-timeout-ms ctx) 5000))))
  (testing "a deadline further off than the timeouts leaves them alone"
    (let [far (+ (System/currentTimeMillis) 600000)
          ctx (search/within-deadline {:timeout-ms       60000
                                       :query-timeout-ms 300000} far)]
      (is (= 60000 (:timeout-ms ctx)))
      (is (= 300000 (:query-timeout-ms ctx)))))
  (testing "a deadline already past still leaves a floor to fail in"
    (let [ctx (search/within-deadline {:timeout-ms 60000}
                                      (- (System/currentTimeMillis) 10000))]
      (is (= 1000 (:timeout-ms ctx)))))
  (testing "timeouts that were never configured are not invented"
    (let [soon (+ (System/currentTimeMillis) 5000)]
      (is (= {} (search/within-deadline {} soon))))))

(deftest size-memo-test
  (when-cwb
   (let [q "\"hund.*\" %c"]
     (is (= 5 (search/size! ctx "PROBE" q)))
     (testing "counting again does not run the query again"
       (with-redefs [search/run-size!
                     (fn [& _] (throw (ex-info "counted again" {})))]
         (is (= 5 (search/size! ctx "PROBE" q)))))
     (testing "a different filter is still counted on its own"
       (is (= 19 (search/size! ctx "VISER" "[]"
                               {:filter {:text_year #{"1591"}}})))
       (is (= 48 (search/size! ctx "VISER" "[]")))))))

(deftest kwic-single-flight-test
  (when-cwb
   (let [ctx  (cache-ctx)
         q    "\"hund.*\" %c"
         runs (atom 0)]
     (with-redefs [search/fresh-sections!
                   (let [f search/fresh-sections!]
                     (fn [& args] (swap! runs inc) (Thread/sleep 150)
                       (apply f args)))]
       (testing "eight readers asking at once run the query once between them"
         (is (= [5 5 5 5 5 5 5 5]
                (mapv (comp :size deref)
                      (mapv (fn [_] (future (search/kwic! ctx "PROBE" q {})))
                            (range 8)))))
         (is (= 1 @runs))))
     (testing "and the result they shared is the one that got stored"
       (is (cache/stored? ctx "PROBE" (stored-name ctx "PROBE" q {})))))))

(deftest size-single-flight-test
  (when-cwb
   (let [runs (atom 0)]
     (with-redefs [search/run-size!
                   (let [f search/run-size!]
                     (fn [& args] (swap! runs inc) (Thread/sleep 150)
                       (apply f args)))]
       (testing "eight readers counting at once run the query once"
         (is (= [5 5 5 5 5 5 5 5]
                (mapv deref
                      (mapv (fn [_] (future (search/size! ctx "PROBE"
                                                          "\"hund.*\" %c")))
                            (range 8)))))
         (is (= 1 @runs)))))))

(deftest kwic-shared-page-test
  (when-cwb
   (let [ctx (cache-ctx)
         run (fn [rows] (future (search/kwic! ctx "VISER" "[]"
                                              {:rows rows :sort "word"})))
         [a b] (mapv deref [(run [0 4]) (run [5 9])])]
     (testing "two readers wanting different pages do not share one run"
       (is (= [0 4] (:rows a)))
       (is (= [5 9] (:rows b)))
       (is (seq (:hits a)))
       (is (not= (mapv :cpos (:hits a)) (mapv :cpos (:hits b))))))))

(deftest kwic-timeout-path-test
  (when-cwb
   (let [seen (atom [])
         ctx  (assoc (cache-ctx) :timeout-ms 60000 :query-timeout-ms 300000)
         q    "\"hund.*\" %c"]
     (with-redefs [cqp/run-batch!
                   (let [f cqp/run-batch!]
                     (fn [c cmds] (swap! seen conj (:timeout-ms c))
                       (f c cmds)))]
       (search/kwic! ctx "PROBE" q {})
       (testing "running the query gets the long budget"
         (is (every? #{300000} @seen)))
       (reset! seen [])
       (search/kwic! ctx "PROBE" q {})
       (testing "reading the result back again gets the ordinary one"
         (is (every? #{60000} @seen)))
       (reset! seen [])
       (search/size! ctx "PROBE" q)
       (testing "counting runs the query, so it gets the long budget too"
         (is (every? #{300000} @seen)))))))

(deftest kwic-cache-budget-test
  (when-cwb
   (let [ctx (assoc (cache-ctx) :cache-max-bytes 1)
         q   "\"hund.*\" %c"]
     (testing "a result too big for the disk budget is saved, then reaped"
       (is (= 5 (:size (search/kwic! ctx "PROBE" q {}))))
       (is (empty? (fs/list-dir (cache/corpus-directory ctx "PROBE"))))))))

(deftest kwic-no-cache-test
  (when-cwb
   (let [ctx (cache-ctx)]
     (testing "a false :cache? keeps a one-off query out of the cache"
       (is (seq (:hits (search/kwic! ctx "PROBE" "\"hund.*\" %c"
                                     {:cache? false}))))
       ;; not even the corpus directory is made for it
       (is (not (fs/exists? (cache/corpus-directory ctx "PROBE"))))))))

(deftest kwic-cache-hit-test
  (when-cwb
   (let [ctx   (cache-ctx)
         hunde "\"hund.*\" %c"
         andet "\"den\" %c"]
     (search/kwic! ctx "PROBE" hunde {})
     (search/kwic! ctx "PROBE" andet {})
     ;; give one query the other's stored result: if the second search
     ;; reads the file rather than running its query, it answers with it
     (fs/copy (cache/result-file ctx "PROBE" (stored-name ctx "PROBE" hunde {}))
              (cache/result-file ctx "PROBE" (stored-name ctx "PROBE" andet {}))
              {:replace-existing true})
     (testing "a stored result is read rather than the query run again"
       (is (seq (:hits (search/kwic! ctx "PROBE" hunde {}))))
       (is (= (:hits (search/kwic! ctx "PROBE" hunde {}))
              (:hits (search/kwic! ctx "PROBE" andet {}))))))))

(deftest intact?-test
  (testing "a page of distinct positions could be a real result"
    (is (search/intact? {:dump [["0\t0\t-1\t-1" "5\t7\t-1\t-1"]]})))
  (testing "repeated positions are the zero-filled rows past a truncation"
    (is (not (search/intact? {:dump [["0\t0\t-1\t-1" "0\t0\t-1\t-1"]]}))))
  (testing "a page with no rows at all is fine"
    (is (search/intact? {:dump [[]]}))))

(defn overstate-matches!
  "Rewrite the match count in the save `file` of `corpus` under `ctx` to
  `n`, leaving the matches themselves alone.

  This is what a full disk leaves behind: CQP writes the header, runs out
  of room part way through the matches and reports nothing, so the file
  claims every match while holding only the ones that fit. Reading it back
  raises no error either; the rows past the cut come out zero-filled.

  The count sits after the magic number, the registry path and the corpus
  name, each NUL-terminated, padded to a four-byte boundary."
  [ctx corpus file n]
  (let [before (+ 4 (inc (count (:registry ctx))) (inc (count corpus)))
        at     (+ before (mod (- 4 (mod before 4)) 4))]
    (with-open [raf (java.io.RandomAccessFile. (fs/file file) "rw")]
      (.seek raf at)
      (let [was (Integer/reverseBytes (.readInt raf))]
        (.seek raf at)
        (.writeInt raf (Integer/reverseBytes (int n)))
        was))))

(deftest kwic-overstated-cache-test
  (when-cwb
   (let [ctx  (cache-ctx)
         q    "[]"
         rows {:rows [100 124]}
         _    (search/kwic! ctx "VISER" q {})
         file (cache/result-file ctx "VISER" (stored-name ctx "VISER" q {}))]
     (testing "the save file says what we think it says"
       (is (= 48 (overstate-matches! ctx "VISER" file 5000))))
     (testing "a result claiming more matches than it holds is discarded"
       (let [fresh  (search/kwic! (dissoc ctx :cache-dir) "VISER" q rows)
             cached (search/kwic! ctx "VISER" q rows)]
         (is (= 48 (:size cached)))
         (is (= (mapv :cpos (:hits fresh)) (mapv :cpos (:hits cached))))))
     (testing "and it is stored again, so the page after it is a hit"
       (is (cache/stored? ctx "VISER" (stored-name ctx "VISER" q {})))))))

(deftest kwic-truncated-cache-test
  (when-cwb
   (let [ctx  (cache-ctx)
         q    "[]"
         page (search/kwic! ctx "VISER" q {})
         file (cache/result-file ctx "VISER" (stored-name ctx "VISER" q {}))]
     ;; a save file cut short is what a full disk leaves behind: CQP reads
     ;; it back without a word and zero-fills everything past the cut
     (with-open [f (java.io.RandomAccessFile. file "rw")]
       (.setLength f 100))
     (testing "a truncated stored result is discarded rather than served"
       (is (= (mapv :cpos (:hits page))
              (mapv :cpos (:hits (search/kwic! ctx "VISER" q {}))))))
     (testing "and the query is stored again, so the page after it is a hit"
       (is (cache/stored? ctx "VISER" (stored-name ctx "VISER" q {})))))))

(deftest kwic-cache-poison-test
  (when-cwb
   (let [ctx  (cache-ctx)
         q    "\"hund.*\" %c"
         page (search/kwic! ctx "PROBE" q {})
         nqr  (stored-name ctx "PROBE" q {})]
     (spit (cache/result-file ctx "PROBE" nqr) "not a save file at all")
     (testing "a stored result CQP cannot read is discarded and re-run"
       (is (= page (search/kwic! ctx "PROBE" q {}))))
     (testing "and stored again, so the page after it is a hit"
       (is (cache/stored? ctx "PROBE" nqr))))))

(deftest kwic-cache-sort-test
  (when-cwb
   (let [ctx    (assoc (cache-ctx) :sort-locale "da_DK.UTF-8")
         order  (fn [sort]
                  (mapv :cpos (:hits (search/kwic! ctx "VISER" "[]"
                                                   {:sort sort :rows [0 9]}))))
         sorted (order "word")]
     (testing "each sort mode stores a result of its own"
       (is (not= sorted (order "corpus")))
       (is (= 2 (count (fs/list-dir (cache/corpus-directory ctx "VISER"))))))
     (testing "a stored sorted result pages in the order it was saved in"
       (is (seq sorted))
       (is (= sorted (order "word"))))
     (testing "the stored order is read, not recomputed on each request"
       ;; hand the corpus-order result the sorted one's name: a request
       ;; that reads the file answers with the sorted order
       (let [name-of (fn [sort] (stored-name ctx "VISER" "[]" {:sort sort}))]
         (fs/copy (cache/result-file ctx "VISER" (name-of "word"))
                  (cache/result-file ctx "VISER" (name-of "corpus"))
                  {:replace-existing true})
         (is (= sorted (order "corpus"))))))))

(deftest kwic-cache-sample-test
  (when-cwb
   (let [ctx  (cache-ctx)
         hits (fn [n] (mapv :cpos (:hits (search/kwic! ctx "VISER" "[]"
                                                       {:sample n}))))
         five (hits 5)]
     (testing "a sample is part of the search, so it is part of the name"
       (is (not= five (hits nil)))
       (is (= 2 (count (fs/list-dir (cache/corpus-directory ctx "VISER"))))))
     (testing "a sampled result is stored and read like any other"
       (is (= 5 (count five)))
       (is (= five (hits 5))))
     (testing "the sample really is read back from its file"
       (let [name-of (fn [n] (stored-name ctx "VISER" "[]" {:sample n}))]
         (fs/copy (cache/result-file ctx "VISER" (name-of 5))
                  (cache/result-file ctx "VISER" (name-of nil))
                  {:replace-existing true}))
       (is (= five (hits nil)))))))

(deftest kwic-cache-filter-test
  (when-cwb
   (let [ctx    (cache-ctx)
         filter {:text_year #{"1591"}}
         hits   (fn [f] (mapv :cpos (:hits (search/kwic! ctx "VISER" "[]"
                                                         {:filter f}))))
         within (hits filter)]
     (testing "the filter is part of the search, so it is part of the name"
       (is (not= within (hits nil)))
       (is (= 2 (count (fs/list-dir (cache/corpus-directory ctx "VISER"))))))
     (testing "a filtered result is stored and read like any other"
       (is (seq within))
       (is (= within (hits filter))))
     (testing "the filtered result really is read back from its file"
       (let [name-of (fn [f] (stored-name ctx "VISER" "[]" {:filter f}))]
         (fs/copy (cache/result-file ctx "VISER" (name-of filter))
                  (cache/result-file ctx "VISER" (name-of nil))
                  {:replace-existing true}))
       (is (= within (hits nil)))))))

(deftest attribute-sort-test
  ;; requires gawk + the da_DK.UTF-8 locale, like danish-collation-test
  (when-cwb
   (let [danish (assoc ctx :sort-locale "da_DK.UTF-8")]
     (testing "the hits sort by any positional attribute, under the collation"
       (is (= ["bord" "dag" "hav" "have" "hund"]
              (->> (search/kwic! danish "PROBE" "[pos = \"N.*\"]"
                                 {:sort "lemma" :rows [0 4]})
                   :hits
                   (map (comp :lemma first :match))))))
     (testing "and by the word read from its end"
       (is (= ["hund" "hund" "hund" "Hunde" "katte"]
              (->> (search/kwic! danish "PROBE" "[pos = \"N.*\"]"
                                 {:sort "reverse" :rows [0 4]})
                   :hits
                   (map (comp :word first :match)))))))
   (testing "an attribute the corpus lacks is rejected, not sorted around"
     (is (thrown-with-msg? Exception #"Not a positional attribute"
                           (search/kwic! ctx "TALER" "[]" {:sort "lemma"}))))))

(deftest blocks-test
  (let [tokens [{:word "a" :open [:s]} {:word "b"} {:word "c" :open [:s]}]]
    (is (= [[{:word "a" :open [:s]} {:word "b"}] [{:word "c" :open [:s]}]]
           (search/blocks :s tokens)))
    (testing "without a unit the text is one block"
      (is (= [tokens] (search/blocks nil tokens))))
    (testing "a text starting inside a region still starts a block"
      (is (= [[{:word "b"}]] (search/blocks :s [{:word "b"}]))))))

(deftest text-test
  (when-cwb
   (let [text (search/text! ctx "PROBE" 9)]
     (testing "the whole text holding the position, sentence by sentence"
       (is (= "PROBE" (:corpus text)))
       (is (= [0 19] [(:from text) (:to text)]))
       (is (= {:s_id "1" :text_id "t1" :text_title "Hverdag" :text_year "2023"}
              (:structs text)))
       (is (= [["Hunden" "sover" "under" "bordet" "."]
               ["Katten" "jagter" "en" "lille" "hund" "i" "haven" "."]
               ["Hunde" "og" "katte" "er" "gode" "venner" "."]]
              (:blocks text)))))
   (testing "a position no text holds is nothing"
     (is (nil? (search/text! ctx "PROBE" 999))))
   (testing "a hostile corpus name is rejected before any command is built"
     (is (thrown-with-msg? Exception #"Invalid corpus name"
                           (search/text! ctx "PROBE; exit" 9))))))

(deftest export-test
  (when-cwb
   (let [export (search/export! ctx "PROBE" "\"hund.*\" %c"
                                {:limit 100 :sort "corpus"})]
     (testing "every hit as a row of strings: positions, contexts, match,
               then the annotations"
       (is (= 5 (:size export)))
       (is (= [:pos :lemma :s_id :text_id :text_title :text_year]
              (:annotations export)))
       (is (= ["9" "9" ". Katten jagter en lille" "hund" "i haven . Hunde og"
               "NCSI" "hund" "2" "t1" "Hverdag" "2023"]
              (second (:rows export)))))
     (testing "a hit at the corpus edge has a short context, not blanks"
       (is (= ["0" "0" "" "Hunden" "sover under bordet . Katten"
               "NCSD" "hund" "1" "t1" "Hverdag" "2023"]
              (first (:rows export))))))
   (testing "the limit cuts the rows and not the size"
     (let [export (search/export! ctx "PROBE" "\"hund.*\" %c" {:limit 2})]
       (is (= 5 (:size export)))
       (is (= 2 (count (:rows export))))))
   (testing "a unit of context becomes a number of words"
     (is (= "Hunden sover under bordet . Katten jagter en lille"
            (nth (second (:rows (search/export! ctx "PROBE" "\"hund.*\" %c"
                                                {:limit   5
                                                 :context :sentence})))
                 2))))))

(deftest export-cache-test
  (when-cwb
   (let [ctx   (cache-ctx)
         hunde "\"hund.*\" %c"
         andet "\"den\" %c"
         words (fn [q] (map #(nth % 3)
                            (:rows (search/export! ctx "PROBE" q {:limit 10}))))]
     (search/kwic! ctx "PROBE" hunde {})
     (search/kwic! ctx "PROBE" andet {})
     ;; give one query the other's stored result: if the export reads the
     ;; file rather than running its query, it prints that
     (fs/copy (cache/result-file ctx "PROBE" (stored-name ctx "PROBE" hunde {}))
              (cache/result-file ctx "PROBE" (stored-name ctx "PROBE" andet {}))
              {:replace-existing true})
     (testing "an export reads the result the concordance saved"
       (is (seq (words hunde)))
       (is (= (words hunde) (words andet))))
     (testing "and saves its own, so the concordance after it reads a file"
       (search/export! ctx "PROBE" "\"kat.*\" %c" {:limit 10 :sort "word"})
       (is (cache/stored? ctx "PROBE"
                          (stored-name ctx "PROBE" "\"kat.*\" %c"
                                       {:sort "word"})))))))

(deftest export-corpora-test
  (when-cwb
   (let [exports (search/export-corpora! ctx ["PROBE" "NOSUCH" "VISER"]
                                         "\"hund.*\" %c" (search/deadline ctx)
                                         6 {})]
     (testing "corpus by corpus, a failing one carrying its error"
       (is (= ["PROBE" "NOSUCH" "VISER"] (map :corpus exports)))
       (is (= 5 (count (:rows (first exports)))))
       (is (:error (second exports))))
     (testing "each within what is left of the limit"
       (is (= 1 (count (:rows (nth exports 2)))))
       (is (= 1 (count (search/export-corpora! ctx ["PROBE" "VISER"]
                                               "\"hund.*\" %c"
                                               (search/deadline ctx) 5 {})))))
     (testing "and none once the deadline has passed"
       (is (= [{:type :timeout}]
              (map :error (search/export-corpora! ctx ["PROBE"] "[]" 0 5
                                                  {}))))))))
