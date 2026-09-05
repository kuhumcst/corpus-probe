(ns dk.cst.corpus-probe.cache-test
  "Unit tests for the saved query result cache: naming, invalidation and
  reaping. Nothing here runs CQP; the round trip through a real save file
  is exercised in dk.cst.corpus-probe.search-test."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [dk.cst.corpus-probe.cache :as cache]))

(defn temp-ctx
  "A context over a fresh cache directory and a registry holding an entry
  for PROBE and one for VISER, so that `build-stamp` has something to read
  for each and two corpora differ by more than a missing entry."
  []
  (let [registry (fs/create-temp-dir)]
    (doseq [id ["probe" "viser"]]
      (spit (fs/file registry id)
            (str "NAME \"\"\nID " id "\nHOME /nowhere\nATTRIBUTE word\n")))
    {:registry  (str registry)
     :cache-dir (str (fs/create-temp-dir))}))

(use-fixtures :each
  ;; all three are process-wide: a test that moves the reaping timestamp
  ;; would otherwise silence the reaping of every test after it, and a
  ;; remembered count would outlive the corpus it was counted from
  (fn [f]
    (reset! cache/last-reap 0)
    (reset! cache/in-flight {})
    (cache/forget-counts!)
    (f)
    (reset! cache/last-reap 0)
    (reset! cache/in-flight {})
    (cache/forget-counts!)))

(deftest directory-test
  (testing "no cache directory configured means no cache"
    (is (nil? (cache/directory {})))
    (is (nil? (cache/directory {:cache-dir ""})))
    (is (nil? (cache/directory {:cache-dir "   "}))))
  (is (= "/var/cache/probe"
         (str (cache/directory {:cache-dir "/var/cache/probe"})))))

(deftest result-name-test
  (let [ctx  (temp-ctx)
        nqr  (fn [query opts] (cache/result-name ctx "PROBE" query opts))]
    (testing "a name CQP can parse: a letter first, and hex after"
      (is (re-matches #"q_[0-9a-f]{32}" (nqr "[]" {}))))
    (testing "the same search twice is the same name"
      (is (= (nqr "[]" {:sort "word"}) (nqr "[]" {:sort "word"}))))
    (testing "every part of the search that changes the result changes it"
      (is (not= (nqr "[]" {}) (nqr "\"hund\"" {})))
      (is (not= (nqr "[]" {:sort "word"}) (nqr "[]" {:sort "left"})))
      (is (not= (nqr "[]" {:filter [[:text_year #{"1591"}]]})
                (nqr "[]" {:filter [[:text_year #{"1583"}]]})))
      (is (not= (nqr "[]" {}) (nqr "[]" {:sample 100})))
      (is (not= (nqr "[]" {:sample 100}) (nqr "[]" {:sample 500})))
      (is (not= (nqr "[]" {}) (cache/result-name ctx "VISER" "[]" {}))))
    (testing "what the display does with the matches is not part of it"
      (is (= (nqr "[]" {})
             (nqr "[]" {:rows [25 49] :context 20 :struct-attrs [:text_id]}))))
    (testing "the modes that all mean corpus order share one result"
      (is (= (nqr "[]" {:sort "corpus"}) (nqr "[]" {:sort nil})))
      (is (= (nqr "[]" {:sort "corpus"}) (nqr "[]" {:sort "no such mode"})))
      (is (not= (nqr "[]" {:sort "corpus"}) (nqr "[]" {:sort "word"}))))
    (testing "two registries can define one corpus name"
      (is (not= (nqr "[]" {})
                (cache/result-name (assoc ctx :registry "/elsewhere")
                                   "PROBE" "[]" {}))))
    (testing "the collation a sorted result was saved in is part of it"
      (is (not= (cache/result-name ctx "PROBE" "[]" {:sort "word"})
                (cache/result-name (assoc ctx :sort-locale "C") "PROBE" "[]"
                                   {:sort "word"}))))
    (testing "how the filter's values are held is not part of the search"
      (is (= (nqr "[]" {:filter [[:y #{"a" "b"}]]})
             (nqr "[]" {:filter [[:y ["b" "a"]]]}))))
    (testing "re-encoding the corpus invalidates every name under it"
      (let [before (nqr "[]" {})]
        (.setLastModified (fs/file (:registry ctx) "probe") 1000000)
        (is (not= before (nqr "[]" {})))))))

(deftest build-stamp-test
  (let [ctx  (temp-ctx)
        home (fs/create-temp-dir)]
    (spit (fs/file (:registry ctx) "probe")
          (str "NAME \"\"\nID probe\nHOME " home "\nATTRIBUTE word\n"))
    (spit (fs/file home "word.corpus") "aaa")
    (let [before (cache/build-stamp ctx "PROBE")]
      (testing "the stamp follows the corpus data, not the registry entry"
        ;; cwb-encode rewrites the entry only when passed -R, so encoding
        ;; a corpus in place leaves it byte-identical while the data change
        (spit (fs/file home "word.corpus") "bbbb")
        (is (not= before (cache/build-stamp ctx "PROBE")))))
    (testing "an entry whose data cannot be found still stamps"
      (is (some? (cache/build-stamp (temp-ctx) "PROBE"))))))

(deftest build-stamp-charset-test
  (let [ctx  (temp-ctx)
        home (fs/create-temp-dir)
        write (fn [charset]
                (spit (fs/file (:registry ctx) "probe")
                      (str "NAME \"\"\nID probe\nHOME " home "\n"
                           "ATTRIBUTE word\n"
                           "##:: charset = \"" charset "\"\n")))]
    (spit (fs/file home "word.corpus") "aaa")
    (write "utf8")
    (let [before (cache/build-stamp ctx "PROBE")]
      (testing "a charset correction changes which matches exist, so it
                must change the stamp, though it touches no data"
        (write "latin1")
        (is (not= before (cache/build-stamp ctx "PROBE")))))))

(deftest corpus-directory-test
  (let [ctx (temp-ctx)]
    (testing "each corpus gets a directory of its own, created on demand"
      (is (= "PROBE" (str (fs/file-name (cache/corpus-directory ctx "PROBE")))))
      (is (not (fs/exists? (cache/corpus-directory ctx "PROBE"))))
      (is (fs/directory? (cache/corpus-directory! ctx "PROBE"))))
    (testing "the result file lives inside it"
      (is (= "PROBE:q_abc"
             (str (fs/file-name (cache/result-file ctx "PROBE" "q_abc"))))))
    (testing "a corpus name is validated, since it becomes a path"
      (is (thrown? Exception (cache/corpus-directory ctx "../../etc"))))
    (testing "no cache directory means no corpus directory"
      (is (nil? (cache/corpus-directory {} "PROBE"))))))

(deftest match-key-test
  (let [ctx (temp-ctx)
        k   (fn [query opts] (cache/match-key ctx "PROBE" query opts))]
    (testing "what changes which matches there are changes the key"
      (is (not= (k "[]" {}) (k "\"hund\"" {})))
      (is (not= (k "[]" {}) (cache/match-key ctx "VISER" "[]" {})))
      (is (not= (k "[]" {:filter [[:y #{"a"}]]})
                (k "[]" {:filter [[:y #{"b"}]]})))
      (testing "a sample among them: it decides both which matches are
                kept and how many, so a count of one is not a count of
                the whole result"
        (is (not= (k "[]" {}) (k "[]" {:sample 100})))
        (is (not= (k "[]" {:sample 100}) (k "[]" {:sample 500}))))
      (testing "and a narrowing to a value at an anchor"
        (is (not= (k "[]" {})
                  (k "[]" {:subset {:anchor "match" :attr :lemma
                                    :value  "hund"}}))))
      (testing "and a nearby word, which decides which matches are kept"
        (is (not= (k "[]" {}) (k "[]" {:near {:word "kat" :distance 5}})))
        (is (not= (k "[]" {:near {:word "kat" :distance 5}})
                  (k "[]" {:near {:word "kat" :distance 2}})))))
    (testing "how they are ordered or displayed does not, since the count
              is the same either way"
      (is (= (k "[]" {}) (k "[]" {:sort "word"})))
      (is (= (k "[]" {}) (k "[]" {:rows [25 49] :context 20})))
      (testing "a sample being the same hits however they are then sorted"
        (is (= (k "[]" {:sample 100})
               (k "[]" {:sample 100 :sort "word"})))))
    (testing "but the saved result's key does take the ordering in"
      (is (not= (cache/result-key ctx "PROBE" "[]" {:sort "word"})
                (cache/result-key ctx "PROBE" "[]" {:sort "left"}))))))

(deftest count!-test
  (let [ctx  (temp-ctx)
        runs (atom 0)
        n    (fn [query opts]
               (cache/count! ctx "PROBE" query opts
                             (fn [] (swap! runs inc) 42)))]
    (testing "the first ask counts, the next does not"
      (is (= 42 (n "[]" {})))
      (is (= 42 (n "[]" {})))
      (is (= 1 @runs)))
    (testing "how the matches are ordered is not part of it"
      (is (= 42 (n "[]" {:sort "word"})))
      (is (= 1 @runs)))
    (testing "a different query or filter is counted on its own"
      (n "\"hund\"" {})
      (n "[]" {:filter [[:y #{"a"}]]})
      (is (= 3 @runs)))
    (testing "a failed count is not remembered as an answer"
      (is (thrown? Exception
                   (cache/count! ctx "PROBE" "[bad]" {}
                                 (fn [] (throw (ex-info "nope" {}))))))
      (is (= 7 (cache/count! ctx "PROBE" "[bad]" {} (fn [] 7)))))
    (testing "forgetting makes the next ask count again"
      (cache/forget-counts!)
      (n "[]" {})
      (is (= 4 @runs)))))

(deftest share!-test
  (let [runs (atom 0)
        go   (promise)
        slow (fn [] (swap! runs inc) @go :done)
        fs   (mapv (fn [_] (future (cache/share! :k slow))) (range 8))]
    (testing "callers arriving while it runs share the one run"
      ;; the run is held open until `go`, so the eight really do overlap
      ;; however slow or fast the machine is
      (while (zero? @runs) (Thread/sleep 5))
      (is (= 1 @runs))
      (deliver go true)
      (is (= (repeat 8 :done) (mapv deref fs)))
      (is (= 1 @runs)))
    (testing "a caller arriving after it finished runs it again"
      (cache/share! :k (fn [] (swap! runs inc) :done))
      (is (= 2 @runs)))
    (testing "a different key is a different run"
      (cache/share! :other (fn [] (swap! runs inc) :done))
      (is (= 3 @runs))))
  (testing "a run that throws is shared too, and leaves nothing behind"
    (let [boom (fn [] (throw (ex-info "boom" {})))]
      (is (thrown? Exception (cache/share! :bad boom)))
      (is (thrown? Exception (cache/share! :bad boom)))
      (is (empty? @cache/in-flight)))))

(deftest count!-invalidation-test
  (let [ctx  (temp-ctx)
        home (fs/create-temp-dir)]
    (spit (fs/file (:registry ctx) "probe")
          (str "NAME \"\"\nID probe\nHOME " home "\nATTRIBUTE word\n"))
    (spit (fs/file home "word.corpus") "aaa")
    (let [runs (atom 0)
          n    #(cache/count! ctx "PROBE" "[]" {} (fn [] (swap! runs inc) 1))]
      (n) (n)
      (is (= 1 @runs))
      (testing "re-encoding the corpus is the memo's only invalidation"
        (spit (fs/file home "word.corpus") "bbbb")
        (n)
        (is (= 2 @runs))))))

(deftest pending-name-test
  (let [nqr "q_abc"]
    (testing "a pending name is unique, and still a name CQP can parse"
      (is (not= (cache/pending-name nqr) (cache/pending-name nqr)))
      (is (re-matches #"q_abc_[0-9a-f]{32}" (cache/pending-name nqr))))
    (testing "no lookup can find it, since it is not the name asked for"
      (is (not= nqr (cache/pending-name nqr))))))

(deftest result-file?-test
  (testing "the cache's own files, which reaping may delete"
    (is (cache/result-file? (fs/file "PROBE:q_0f3a")))
    (is (cache/result-file? (fs/file "FT_KORPUS-2:q_0f3a_9b2c"))))
  (testing "a corpus name is uppercase, so a lowercase one is not ours"
    (is (not (cache/result-file? (fs/file "probe:q_0f3a")))))
  (testing "anything else in the directory is left alone"
    (is (not (cache/result-file? (fs/file "notes.txt"))))
    (is (not (cache/result-file? (fs/file "PROBE:Filter"))))
    (is (not (cache/result-file? (fs/file "q_0f3a"))))))

(deftest stored?-test
  (let [ctx (temp-ctx)]
    (cache/corpus-directory! ctx "PROBE")
    (is (not (cache/stored? ctx "PROBE" "q_abc")))
    (spit (cache/result-file ctx "PROBE" "q_abc") "x")
    (is (cache/stored? ctx "PROBE" "q_abc"))
    (testing "nothing is stored when no cache is configured"
      (is (not (cache/stored? {} "PROBE" "q_abc"))))))

(deftest commit!-test
  (let [ctx (temp-ctx)]
    (cache/corpus-directory! ctx "PROBE")
    (spit (cache/result-file ctx "PROBE" "q_abc_pending") "saved")
    (cache/commit! ctx "PROBE" "q_abc_pending" "q_abc" 0)
    (testing "the pending file becomes the one lookups find"
      (is (cache/stored? ctx "PROBE" "q_abc"))
      (is (= "saved" (slurp (cache/result-file ctx "PROBE" "q_abc"))))
      (is (not (cache/stored? ctx "PROBE" "q_abc_pending"))))
    (testing "a later result replaces the one stored"
      (spit (cache/result-file ctx "PROBE" "q_abc_two") "newer")
      (cache/commit! ctx "PROBE" "q_abc_two" "q_abc" 0)
      (is (= "newer" (slurp (cache/result-file ctx "PROBE" "q_abc")))))
    (testing "a cqp that saved nothing leaves the stored result alone"
      (cache/commit! ctx "PROBE" "q_abc_missing" "q_abc" 0)
      (is (= "newer" (slurp (cache/result-file ctx "PROBE" "q_abc")))))))

(deftest commit!-truncated-test
  (let [ctx (temp-ctx)]
    (cache/corpus-directory! ctx "PROBE")
    (spit (cache/result-file ctx "PROBE" "q_short_pending") "far too small")
    (cache/commit! ctx "PROBE" "q_short_pending" "q_short" 1000)
    (testing "a file too small to hold its matches is never given the name"
      (is (not (cache/stored? ctx "PROBE" "q_short"))))
    (testing "and it is thrown away rather than left to be reaped"
      (is (not (cache/stored? ctx "PROBE" "q_short_pending"))))))

(deftest discard!-test
  (let [ctx (temp-ctx)]
    (cache/corpus-directory! ctx "PROBE")
    (spit (cache/result-file ctx "PROBE" "q_abc") "x")
    (cache/discard! ctx "PROBE" "q_abc")
    (is (not (cache/stored? ctx "PROBE" "q_abc")))))

(deftest reap!-test
  (let [ctx     (assoc (temp-ctx) :cache-ttl-ms 1000)
        _       (cache/corpus-directory! ctx "PROBE")
        stale   (cache/result-file ctx "PROBE" "q_stale")
        fresh   (cache/result-file ctx "VISER" "q_fresh")
        foreign (fs/file (cache/corpus-directory ctx "PROBE") "notes.txt")]
    (cache/corpus-directory! ctx "VISER")
    (doseq [f [stale fresh foreign]]
      (spit f "x")
      (.setLastModified f 1000000))
    (cache/touch! ctx "VISER" "q_fresh")
    (is (= 1 (cache/reap! ctx)))
    (testing "reaping reaches every corpus, and spares what was read"
      (is (not (.exists stale)))
      (is (.exists fresh)))
    (testing "a file the cache did not write is never deleted"
      (is (.exists foreign)))
    (testing "no cache directory means nothing to reap"
      (is (= 0 (cache/reap! {}))))))

(defn result-of
  "Create a saved result of `corpus` named `nqr` under `ctx`, `bytes` long
  and last read at `read-at`, and return its file."
  [ctx corpus nqr bytes read-at]
  (cache/corpus-directory! ctx corpus)
  (doto (cache/result-file ctx corpus nqr)
    (spit (apply str (repeat bytes "x")))
    (.setLastModified read-at)))

(deftest excess-files-test
  (let [ctx (temp-ctx)
        a   (result-of ctx "PROBE" "q_a" 100 3000000)
        b   (result-of ctx "PROBE" "q_b" 100 2000000)
        c   (result-of ctx "PROBE" "q_c" 100 1000000)]
    (testing "results that already fit are all kept"
      (is (empty? (cache/excess-files 300 [a b c])))
      (is (empty? (cache/excess-files 1000 [a b c]))))
    (testing "the least recently read go first, and only enough of them"
      (is (= [c] (cache/excess-files 200 [a b c])))
      (is (= [b c] (cache/excess-files 100 [a b c]))))
    (testing "a budget nothing fits inside deletes everything"
      (is (= [a b c] (cache/excess-files 0 [a b c]))))
    (testing "nothing to weigh"
      (is (empty? (cache/excess-files 100 []))))))

(deftest reap-budget-test
  (let [now  (System/currentTimeMillis)
        ctx  (assoc (temp-ctx) :cache-ttl-ms 600000 :cache-max-bytes 250)
        old  (result-of ctx "PROBE" "q_old" 100 (- now 3000))
        mid  (result-of ctx "VISER" "q_mid" 100 (- now 2000))
        new* (result-of ctx "PROBE" "q_new" 100 (- now 1000))]
    (testing "nothing is stale, so only the disk budget bites"
      (is (= 1 (cache/reap! ctx)))
      (is (not (.exists old)))
      (is (.exists mid))
      (is (.exists new*)))
    (testing "a budget the whole cache fits inside deletes nothing"
      (is (= 0 (cache/reap! (assoc ctx :cache-max-bytes 1000)))))
    (testing "the default budget is generous enough never to bite in dev"
      (is (= 0 (cache/reap! (dissoc ctx :cache-max-bytes)))))))

(deftest reap-due!-test
  (let [ctx (assoc (temp-ctx) :cache-ttl-ms 1000)]
    (cache/corpus-directory! ctx "PROBE")
    (doto (cache/result-file ctx "PROBE" "q_stale")
      (spit "x")
      (.setLastModified 1000000))
    (is (= 1 (cache/reap-due! ctx)))
    (testing "the save straight after does not walk the directory again"
      (is (nil? (cache/reap-due! ctx))))))

(deftest holds?-test
  (let [ctx {:registry "r" :cache-dir (str (fs/create-temp-dir))}
        nqr "q_1"]
    (.mkdirs (cache/corpus-directory ctx "PROBE"))
    (spit (cache/result-file ctx "PROBE" nqr) (apply str (repeat 40 "x")))
    (testing "a file is held to eight bytes a match, as when it was saved"
      (is (cache/holds? ctx "PROBE" nqr 5))
      (is (not (cache/holds? ctx "PROBE" nqr 6))))
    (testing "a missing file holds nothing"
      (is (not (cache/holds? ctx "PROBE" "q_2" 1))))))
