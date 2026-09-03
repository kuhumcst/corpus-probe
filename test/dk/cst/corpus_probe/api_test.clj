(ns dk.cst.corpus-probe.api-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [cognitect.transit :as transit]
            [dk.cst.corpus-probe.api :as api]
            [dk.cst.corpus-probe.cqp-test :refer [ctx when-cwb]])
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
    (is (= "a=1" (api/query-string {:a 1 :b nil}))))
  (testing "a vector value repeats its key"
    (is (= "corpus=A&corpus=B&q=x"
           (api/query-string {:corpus ["A" "B"] :q "x"})))))

(deftest corpora-param-test
  (testing "one, repeated and comma-joined values all select corpora"
    (is (= ["PROBE"] (api/corpora-param "PROBE")))
    (is (= ["PROBE" "VISER"] (api/corpora-param ["PROBE" "VISER"])))
    (is (= ["PROBE" "VISER" "TALER"]
           (api/corpora-param ["PROBE,VISER" "TALER"]))))
  (testing "names are uppercased and deduplicated, blanks dropped"
    (is (= ["PROBE"] (api/corpora-param ["probe" "PROBE" ""]))))
  (is (= [] (api/corpora-param nil))))

(deftest context-page-validation-test
  (let [context (fn [params] (api/context-page {} {:query-params params}))]
    (testing "a hostile corpus name is rejected before touching cqp"
      (is (= 400 (:status (context {:corpus   "bad; exit"
                                    :cpos     "9"
                                    :matchend "9"})))))
    (testing "a non-integer position is rejected"
      (is (= 400 (:status (context {:corpus   "PROBE"
                                    :cpos     "x"
                                    :matchend "9"})))))))

(deftest public-error-test
  (testing "CL warning lines (which may name server paths) are dropped"
    (is (= {:type :cqp :message "CQP Error: bad query\n  [pos = <--"}
           (api/public-error
            {:type    :cqp
             :message (str "CL warning: ID field 'x' does not match name of "
                           "registry file /srv/registry/y\n"
                           "CQP Error: bad query\n  [pos = <--")}))))
  (testing "a message of nothing but warnings empties out"
    (is (nil? (:message (api/public-error {:message "CL warning: x"})))))
  (testing "an error without a message passes through"
    (is (= {:type :timeout} (api/public-error {:type :timeout}))))
  (testing "follow-on errors after the failing command's own are dropped"
    (is (= "CQP Error:\n\tCorpus ``NOSUCH'' is undefined"
           (:message (api/public-error
                      {:message (str "CQP Error:\n\tCorpus ``NOSUCH'' is "
                                     "undefined\n"
                                     (str/join "\n" api/follow-on-errors))})))))
  (testing "a message of nothing but follow-on errors is kept"
    (let [message (first api/follow-on-errors)]
      (is (= message (:message (api/public-error {:message message})))))))

(deftest grouped-corpora-test
  (let [probe     {:id "PROBE" :title nil :size 47}
        viser     {:id "VISER" :title "Folkeviser" :size 48}
        taler     {:id "TALER" :title "Taler" :size 42}
        overviews [probe taler viser]
        folders   [{:label   "Litteratur"
                    :folders [{:label "Folkeviser" :corpora ["VISER"]}]}
                   {:label   "Folketinget"
                    :corpora ["TALER" "GONE"]}]
        grouped   (api/grouped-corpora folders overviews)]
    (testing "nested folders resolve their corpus IDs to overviews"
      (is (= [viser] (-> grouped first :folders first :corpora))))
    (testing "IDs the registry does not know are dropped"
      (is (= [taler] (:corpora (second grouped)))))
    (testing "unclaimed corpora follow as a label-less folder"
      (is (= {:label nil :corpora [probe] :folders []} (last grouped))))
    (testing "no configuration means one label-less folder of everything"
      (is (= [{:label nil :corpora overviews :folders []}]
             (api/grouped-corpora nil overviews))))
    (testing "everything claimed means no trailing folder"
      (is (= 1 (count (api/grouped-corpora [{:label   "All"
                                             :corpora ["PROBE" "TALER"
                                                       "VISER"]}]
                                           overviews)))))
    (testing "folders the registry leaves empty are dropped, at any depth"
      (is (= [{:label "Litteratur" :corpora []
               :folders [{:label "Folkeviser" :corpora [viser] :folders []}]}
              {:label nil :corpora [probe taler] :folders []}]
             (api/grouped-corpora [{:label   "Litteratur"
                                    :corpora ["GONE"]
                                    :folders [{:label "Empty" :corpora ["X"]}
                                              {:label   "Folkeviser"
                                               :corpora ["VISER"]}]}
                                   {:label "Nothing" :corpora []}]
                                  overviews))))))

(deftest corpus-page-test
  (let [ctx  {:registry "test/resources"}
        page (fn [id] (api/corpus-page ctx {:path-params {:id id}}))]
    (testing "a hostile corpus name is rejected before touching anything"
      (is (= 404 (:status (page "bad; exit"))))
      (is (= 404 (:status (page "../resources/registry-probe")))))
    (testing "a name with no registry entry is not found"
      (is (= 404 (:status (page "nope")))))
    (testing "a directory is not a corpus"
      (is (= 404 (:status (page "golden")))))
    (testing "an entry is found case-insensitively and its paths stay private"
      ;; the fixture's data files do not exist, so the page is the alert
      (let [{:keys [status body]} (page "Registry-Probe")]
        (is (= 200 status))
        (is (str/includes? body "Could not read corpus"))
        (is (not (str/includes? body "/corpora/data/probe")))))))

(deftest page-title-test
  (is (= "corpus-probe" (api/page-title)))
  (is (= "VISER · corpus-probe" (api/page-title "VISER")))
  (testing "blank parts are skipped"
    (is (= "corpus-probe" (api/page-title nil "")))))

(deftest search-title-test
  (testing "no query is just the app name"
    (is (= "corpus-probe" (api/search-title {}))))
  (testing "a search names the query and corpus"
    (is (= "hund · PROBE · corpus-probe"
           (api/search-title {:q "hund" :corpus ["PROBE"]}))))
  (testing "several corpora are counted"
    (is (= "hund · 2 corpora · corpus-probe"
           (api/search-title {:q "hund" :corpus ["PROBE" "VISER"]}))))
  (testing "no corpora are not counted"
    (is (= "hund · corpus-probe" (api/search-title {:q "hund" :corpus []})))))

(deftest scalar-params-test
  (testing "a repeated scalar param keeps its first value, corpus its vector"
    (is (= {:q "a" :page "1" :corpus ["A" "B"]}
           (api/scalar-params {:q ["a" "b"] :page "1" :corpus ["A" "B"]})))))

(deftest page-param-test
  (is (= 0 (api/page-param nil)))
  (is (= 3 (api/page-param "3")))
  (testing "negative, non-numeric and absurd values are the first page"
    (is (= 0 (api/page-param "-3")))
    (is (= 0 (api/page-param "x")))
    (is (= 0 (api/page-param "99999999999999999999")))))

(deftest split-known-test
  (is (= [["PROBE"] ["NOPE"]]
         (api/split-known [{:id "probe"}] ["PROBE" "NOPE"]))))

(deftest search-outcome-test
  (testing "nothing selected at all is the no-corpus error"
    (is (= {:error {:type :no-corpus}} (api/search-outcome! {} [] [] "x" 0 nil))))
  (testing "only unknown corpora yields an empty result reporting them"
    (let [{:keys [result]} (api/search-outcome! {} [] ["NOPE"] "x" 0 nil)]
      (is (= [{:corpus "NOPE" :error {:type :unknown-corpus}}]
             (:counts result)))
      (is (= 0 (:size result)))
      (is (= 1 (:pages result))))))

(deftest public-counts-test
  (testing "per-corpus errors are prepared for display"
    (is (= [{:corpus "X" :error {:type :cqp :message "CQP Error: bad"}}]
           (:counts (api/public-counts
                     {:counts [{:corpus "X"
                                :error  {:type    :cqp
                                         :message (str "CL warning: /srv/x\n"
                                                       "CQP Error: bad")}}]}))))))

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

(deftest attr-param-test
  (is (= "word" (api/attr-param nil)))
  (is (= "word" (api/attr-param "")))
  (is (= "lemma" (api/attr-param "lemma"))))

(deftest frequencies-page-test
  (let [ctx  {:registry "test/resources"}
        page (fn [params] (api/frequencies-page ctx {:query-params params}))]
    (testing "a fresh visit is just the form"
      (let [{:keys [status body]} (page {})]
        (is (= 200 status))
        (is (str/includes? body "Group by"))
        (is (not (str/includes? body "No corpus selected")))))
    (testing "a submission without a corpus is refused"
      (is (str/includes? (:body (page {:attr "word" :q "hund"}))
                         "No corpus selected")))))

(deftest export-validation-test
  (let [ctx    {:registry "test/resources"}
        export (fn [params] (api/export-kwic ctx {:query-params params}))]
    (testing "an export needs a query, known corpora and a known format"
      (is (= 400 (:status (export {:corpus "REGISTRY-PROBE" :format "tsv"}))))
      (is (= 400 (:status (export {:q "hund" :format "tsv"}))))
      (is (= 400 (:status (export {:corpus "NOPE" :q "hund" :format "tsv"}))))
      (is (= 400 (:status (export {:corpus "REGISTRY-PROBE" :q "hund"
                                   :format "xls"})))))
    (testing "a frequency export needs corpora and a known format"
      (is (= 400 (:status (api/export-frequencies
                           ctx {:query-params {:format "csv"}})))))))

(deftest export-hrefs-test
  (is (= {:csv "/export/kwic?corpus=PROBE&q=hund&format=csv"
          :tsv "/export/kwic?corpus=PROBE&q=hund&format=tsv"}
         (api/export-hrefs "/export/kwic" {:corpus ["PROBE"] :q "hund"}))))

(deftest attr-options-test
  (testing "an unreadable corpus contributes nothing, word remains"
    (is (= [{:type :positional :name :word}]
           (api/attr-options! {:registry "test/resources" :cqp "no-such-cqp"}
                              ["REGISTRY-PROBE"]))))
  (when-cwb
   (testing "the union over corpora, positional first, keeps registry order"
     (is (= [:word :pos :lemma :s_id :text_id :text_title :text_year
             :text_speaker :text_party]
            (map :name (api/attr-options! ctx ["PROBE" "TALER"])))))))

(deftest export-failure-test
  (testing "a search that fails everywhere is a 400 with the reasons"
    (let [{:keys [status headers body]}
          (api/export-kwic {:registry "test/resources" :cqp "no-such-cqp"}
                           {:query-params {:corpus "REGISTRY-PROBE"
                                           :q      "hund"
                                           :format "tsv"}})]
      (is (= 400 status))
      (is (str/starts-with? (get headers "Content-Type") "text/plain"))
      (is (str/starts-with? body "REGISTRY-PROBE: internal")))))
