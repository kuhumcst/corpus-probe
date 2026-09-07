(ns dk.cst.corpus-probe.search.result-test
  "The stored-or-fresh engine through the searches built on it: a saved
  query result read back, discarded when damaged, shared while in flight
  and saved again after a miss, for pages, exports and breakdowns alike;
  skipped when CWB or the dev corpus is missing."
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [dk.cst.corpus-probe.cwb :as cwb]
            [dk.cst.corpus-probe.cwb.command :as command]
            [dk.cst.corpus-probe.search :as search]
            [dk.cst.corpus-probe.search.batch :as batch]
            [dk.cst.corpus-probe.search.cache :as cache]
            [dk.cst.corpus-probe.search.frequency :as frequency]
            [dk.cst.corpus-probe.search.opts :as opts]
            [dk.cst.corpus-probe.search.result :as result]
            [dk.cst.corpus-probe.test.cwb
             :refer [ctx reset-cache! when-cwb]]
            [taoensso.telemere :as t]))

(use-fixtures :each reset-cache!)

(defn caching-ctx!
  "`ctx` with a cache directory of its own, so that one test's stored
  results are never another's."
  []
  (assoc ctx :cache-dir (str (fs/create-temp-dir))))

(defn stored-name
  "The name `corpus` stores the result of `query` under `opts` beneath, as
  dk.cst.corpus-probe.search.opts/kwic-opts! computes it."
  [ctx corpus query opts]
  (:nqr (opts/kwic-opts! ctx corpus query opts)))

(deftest kwic-cache-test
  (when-cwb
   (let [ctx  (caching-ctx!)
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
   (let [ctx  (caching-ctx!)
         q    "\"hund.*\" %c"
         _    (search/kwic! ctx "PROBE" q {})
         file (cache/result-file ctx "PROBE" (stored-name ctx "PROBE" q {}))]
     (.setLastModified file 1000000)
     (search/kwic! ctx "PROBE" q {:rows [1 2]})
     (testing "reading a stored result keeps it from being reaped"
       (is (> (.lastModified file) 1000000))))))

(deftest kwic-cache-failure-test
  (when-cwb
   (let [ctx (caching-ctx!)]
     (testing "a query CQP rejects stores nothing, not even a pending file"
       (is (thrown? Exception (search/kwic! ctx "PROBE" "[bogus = " {})))
       (is (empty? (fs/list-dir (cache/corpus-directory ctx "PROBE")))))
     (testing "a cache that cannot be written answers the search anyway"
       (let [broken (assoc ctx :cache-dir "/dev/null/nope")]
         ;; /dev/null is not a directory, so cqp cannot save into it
         (is (= 5 (:size (search/kwic! broken "PROBE"
                                       "\"hund.*\" %c" {})))))))))

(deftest size-memo-test
  (when-cwb
   (let [q "\"hund.*\" %c"]
     (is (= 5 (search/size! ctx "PROBE" q)))
     (testing "counting again does not run the query again"
       (with-redefs [result/run-result!
                     (fn [& _] (throw (ex-info "counted again" {})))]
         (is (= 5 (search/size! ctx "PROBE" q)))))
     (testing "a different filter is still counted on its own"
       (is (= 19 (search/size! ctx "VISER" "[]"
                               {:filter {:text_year #{"1591"}}})))
       (is (= 48 (search/size! ctx "VISER" "[]")))))))

(deftest kwic-single-flight-test
  (when-cwb
   (let [ctx  (caching-ctx!)
         q    "\"hund.*\" %c"
         runs (atom 0)]
     (with-redefs [result/run-fresh!
                   (let [f result/run-fresh!]
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
     (with-redefs [result/run-result!
                   (let [f result/run-result!]
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
   (let [ctx (caching-ctx!)
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
         ctx  (assoc (caching-ctx!) :timeout-ms 60000 :query-timeout-ms 300000)
         q    "\"hund.*\" %c"]
     (with-redefs [cwb/run-batch!
                   (let [f cwb/run-batch!]
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
   (let [ctx (assoc (caching-ctx!) :cache-max-bytes 1)
         q   "\"hund.*\" %c"]
     (testing "a result too big for the disk budget is saved, then reaped"
       (is (= 5 (:size (search/kwic! ctx "PROBE" q {}))))
       (is (empty? (fs/list-dir (cache/corpus-directory ctx "PROBE"))))))))

(deftest kwic-no-cache-test
  (when-cwb
   (let [ctx (caching-ctx!)]
     (testing "a false :cache? keeps a one-off query out of the cache"
       (is (seq (:hits (search/kwic! ctx "PROBE" "\"hund.*\" %c"
                                     {:cache? false}))))
       ;; not even the corpus directory is made for it
       (is (not (fs/exists? (cache/corpus-directory ctx "PROBE"))))))))

(deftest kwic-cache-hit-test
  (when-cwb
   (let [ctx   (caching-ctx!)
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
    (is (result/intact? {:dump [["0\t0\t-1\t-1" "5\t7\t-1\t-1"]]})))
  (testing "repeated positions are the zero-filled rows past a truncation"
    (is (not (result/intact? {:dump [["0\t0\t-1\t-1" "0\t0\t-1\t-1"]]}))))
  (testing "a page with no rows at all is fine"
    (is (result/intact? {:dump [[]]}))))

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
   (let [ctx  (caching-ctx!)
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
   (let [ctx  (caching-ctx!)
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
   (let [ctx  (caching-ctx!)
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
   (let [ctx    (assoc (caching-ctx!) :sort-locale "da_DK.UTF-8")
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
   (let [ctx  (caching-ctx!)
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
   (let [ctx    (caching-ctx!)
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

(deftest export-cache-test
  (when-cwb
   (let [ctx   (caching-ctx!)
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

(deftest stored-breakdown-test
  (when-cwb
   (let [ctx       (caching-ctx!)
         q         "[pos = \"N.*\"]"
         hunde     "\"hund.*\" %c"
         breakdown #(frequency/frequencies! ctx "PROBE" q :lemma
                                            {:docs true :sort "word"})
         fresh     (breakdown)
         opts      (opts/cache-opts! ctx "PROBE" q {:sort "word"})
         counting  [(command/count-command "match" :lemma)]
         stored    #(result/read-stored!
                     ctx "PROBE" q opts
                     (fn [corpus nqr opts]
                       (batch/stored-count-batch corpus nqr opts counting))
                     (fn [sections]
                       (cache/holds? ctx "PROBE" (:nqr opts)
                                     (result/match-count sections))))
         file      (cache/result-file ctx "PROBE" (:nqr opts))]
     (testing "until a concordance saves the result there is nothing to read"
       (is (nil? (stored))))
     (search/kwic! ctx "PROBE" q {:sort "word"})
     (testing "once one has, the breakdown reads it and agrees with a fresh run"
       (is (= [["hund\t5" "kat\t2" "København\t1" "bord\t1" "dag\t1" "hav\t1"
                "have\t1" "sol\t1" "strand\t1" "ven\t1"]]
              (:count (stored))))
       (is (= fresh (breakdown))))
     (testing "and it is the file that is counted, not the query"
       ;; give the query another query's stored result: if the breakdown
       ;; reads the file rather than running the query, it counts that
       (search/kwic! ctx "PROBE" hunde {:sort "word"})
       (fs/copy (cache/result-file
                 ctx "PROBE"
                 (:nqr (opts/cache-opts! ctx "PROBE" hunde {:sort "word"})))
                file
                {:replace-existing true})
       (is (= [{:values ["hund"] :freq 5 :docs 3}] (breakdown))))
     (testing "a truncated file is discarded and the query run instead"
       (with-open [f (java.io.RandomAccessFile. file "rw")]
         (.setLength f 12))
       (is (= fresh (t/with-min-level :fatal (breakdown))))
       (is (not (cache/stored? ctx "PROBE" (:nqr opts)))))
     (testing "a sampled concordance is never counted"
       (search/kwic! ctx "PROBE" q {:sort "word" :sample 3})
       (is (cache/stored? ctx "PROBE"
                          (:nqr (opts/cache-opts! ctx "PROBE" q
                                                  {:sort "word" :sample 3}))))
       (is (= fresh (breakdown)))))))
