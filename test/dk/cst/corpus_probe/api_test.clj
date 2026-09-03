(ns dk.cst.corpus-probe.api-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [cognitect.transit :as transit]
            [dk.cst.corpus-probe.api :as api])
  (:import [java.io ByteArrayInputStream]))

(defn transit->
  "Decode transit-JSON string `s` (test helper, mirroring api/->transit)."
  [s]
  (let [in (ByteArrayInputStream. (.getBytes s "UTF-8"))]
    (transit/read (transit/reader in :json))))

(deftest ->cqp-test
  (testing "CQP mode passes the query through verbatim"
    (is (= "[lemma = \"hund\"]"
           (api/->cqp {:q "[lemma = \"hund\"]" :mode "cqp"}))))
  (testing "simple mode compiles the query"
    (is (= "[word = \"hund.*\" %c]"
           (api/->cqp {:q "hund" :mode "simple" :prefix "on" :ci "on"}))))
  (testing "blank input yields nil"
    (is (nil? (api/->cqp {:q "   " :mode "cqp"})))
    (is (nil? (api/->cqp {:mode "simple"})))))

(deftest page-href-test
  (let [href (api/page-href {:corpus "PROBE" :q "hund" :page "0"} 2)]
    (is (str/starts-with? href "/?"))
    (is (str/includes? href "corpus=PROBE"))
    (is (str/includes? href "page=2"))
    (testing "the query is URL-encoded"
      (is (str/includes? (api/page-href {:q "[lemma=\"a\"]"} 1) "%5B")))
    (testing "the per-page expand parameter is dropped"
      (is (not (str/includes? (api/page-href {:corpus "PROBE" :expand "9"} 1)
                              "expand"))))))

(deftest query-string-test
  (is (= "a=1&b=2" (api/query-string {:a 1 :b 2})))
  (testing "nil values are dropped"
    (is (= "a=1" (api/query-string {:a 1 :b nil})))))

(deftest context-page-validation-test
  (testing "a hostile corpus name is rejected before touching cqp"
    (is (= 400 (:status (api/context-page {} {:query-params {:corpus   "bad; exit"
                                                             :cpos     "9"
                                                             :matchend "9"}})))))
  (testing "a non-integer position is rejected"
    (is (= 400 (:status (api/context-page {} {:query-params {:corpus   "PROBE"
                                                             :cpos     "x"
                                                             :matchend "9"}}))))))

(deftest page-title-test
  (testing "no query is just the app name"
    (is (= "corpus-probe" (api/page-title {}))))
  (testing "a search names the query and corpus"
    (is (= "hund · PROBE — corpus-probe"
           (api/page-title {:q "hund" :corpus "PROBE"})))))

(deftest content-lang-test
  (let [corpora [{:id "probe" :language "??"}
                 {:id "dan1" :language "da"}]]
    (testing "a plausible language code is returned"
      (is (= "da" (api/content-lang corpora "DAN1"))))
    (testing "a placeholder language is ignored"
      (is (nil? (api/content-lang corpora "PROBE"))))
    (testing "an unknown corpus yields nil"
      (is (nil? (api/content-lang corpora "NOPE"))))))

(deftest correct-quote-escaping-test
  (testing "corrupted double quotes (&#39;) are restored to &#34;"
    (is (= "[lemma=&#34;hund&#34;]"
           (api/correct-quote-escaping "[lemma=&#39;hund&#39;]"))))
  (testing "real apostrophes (&apos;) are left untouched"
    (is (= "it&apos;s" (api/correct-quote-escaping "it&apos;s")))))

(deftest script-safe-test
  (testing "< is neutralised so corpus content cannot terminate the script"
    (is (= "a\\u003c/script>b" (api/script-safe "a</script>b"))))
  (testing "a hostile value survives embedding and decoding"
    (let [data    {:hits [{:word "12\"" :tag "</script>"}]}
          payload (api/script-safe (api/->transit data))]
      (is (not (re-find #"</script" payload)))
      ;; the browser reads the script text verbatim; the JSON reader decodes
      ;; the < escapes, which we emulate here before decoding.
      (is (= data (transit-> (str/replace payload "\\u003c" "<")))))))
