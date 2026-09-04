(ns dk.cst.corpus-probe.api-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [cognitect.transit :as transit]
            [dk.cst.corpus-probe.api :as api]
            [dk.cst.corpus-probe.corpus :as corpus]
            [dk.cst.corpus-probe.cqp-test :refer [ctx when-cwb]]
            [dk.cst.corpus-probe.frequency :as frequency]
            [taoensso.telemere :as t])
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
    (is (nil? (api/->cqp {:mode "simple"}))))
  (testing "simple is the default: a bare word is a word search, not CQP"
    (is (= "[word = \"hund\"]" (api/->cqp {:q "hund"})))
    (is (= "[word = \"hund\"]" (api/->cqp {:q "hund" :mode ""})))
    (testing "and its options still apply"
      (is (= "[word = \"hund.*\" %c]"
             (api/->cqp {:q "hund" :prefix "on" :ci "on"}))))))

(deftest selected-corpora-test
  (let [corpora [{:id "probe"} {:id "viser"}]]
    (testing "the corpora the request names win"
      (is (= ["PROBE"] (api/selected-corpora nil corpora {:corpus "probe"})))
      (is (= ["PROBE" "VISER"]
             (api/selected-corpora nil corpora {:corpus ["probe" "viser"]
                                                :scope  "chosen"}))))
    (testing "a reader who unticked every corpus gets no corpus, not all"
      (is (= [] (api/selected-corpora nil corpora {:scope "chosen"}))))
    (when-cwb
     (testing "naming none searches every corpus CWB can read"
       (is (= ["PROBE" "TALER" "VISER"]
              (sort (api/selected-corpora ctx (corpus/corpora ctx) {}))))))))

(deftest results-fragment-hrefs-test
  (testing "a page turn lands on the results, not the top of the form"
    (is (str/ends-with? (api/page-href {:q "hund"} 1) "#results")))
  (testing "so does every view of the same search"
    (is (every? #(str/ends-with? (last %) "#results")
                (:view-hrefs (api/search-view-data
                              {:registry "nonesuch"}
                              {:query-params {:q "hund"}}))))))

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

(deftest filters-page-test
  (let [asked (atom nil)
        call  (fn [params]
                (with-redefs [corpus/corpora
                              (fn [_] [{:id "probe"} {:id "viser"}])
                              frequency/filter-options!
                              (fn [_ corpora]
                                (reset! asked corpora)
                                {:attrs [{:name :text_year :rows []}]
                                 :unlisted []})]
                  (api/filters-page {} {:query-params params})))]
    (testing "only names the registry has reach the filters"
      (let [response (call {:corpus ["PROBE" "NOSUCH"]})]
        (is (= ["PROBE"] @asked))
        (is (= 200 (:status response)))
        (is (str/starts-with? (get-in response [:headers "Content-Type"])
                              "application/transit+json"))))
    (testing "the attributes come back for the client to render"
      (is (= [{:name :text_year :rows []}]
             (:attrs (transit-> (:body (call {:corpus "VISER"})))))))
    (testing "a hostile name is filtered out rather than reaching a command"
      (call {:corpus "bad; exit"})
      (is (= [] @asked)))
    (testing "the values a reader chose are the reader's, not answered here"
      (is (not (contains? (transit-> (:body (call {:corpus "PROBE"})))
                          :selected))))))

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
  (testing "a failed filter leaves only its own error"
    (is (= "CQP Error:\n\tStructural attribute X.text_author does not exist."
           (:message (api/public-error
                      {:message (str "CQP Error:\n\tStructural attribute "
                                     "X.text_author does not exist.\n"
                                     "CQP Error:\n\tCorpus ``Last'' is "
                                     "undefined\n"
                                     "CQP Error:\n\tCorpus ``Filter'' is "
                                     "undefined")})))))
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
        page (fn [id] (api/corpus-page ctx {:path-params {:id id}
                                            :headers {"cookie" "lang=en"}}))]
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

(deftest document-test
  (let [switch {"da" "/?lang=da" "en" "/?lang=en"}
        base   {:lang "en" :switch switch :title "T"
                :body [:main {:id "main"} "body"]}
        plain  (api/document base)
        client (api/document (assoc base :payload "[]"))
        ;; nil rather than -1 for absent, so an order assertion cannot
        ;; pass on a part that is not there at all
        at     (fn [doc s] (let [i (.indexOf ^String doc ^String s)]
                             (when-not (neg? i) i)))]
    (testing "the bypass link is the document's first focusable element"
      (is (some? (at plain "href=\"#main\"")))
      (is (< (at plain "href=\"#main\"") (at plain "<header"))))
    (testing "the footer follows the body, so it is the document's own"
      (is (some? (at plain "<footer")))
      (is (< (at plain "<main") (at plain "<footer"))))
    (testing "every page mounts the client, so every page can route"
      (is (str/includes? plain "<div id=\"app\">"))
      (is (str/includes? plain "/js/main.js"))
      (is (str/includes? client "<div id=\"app\">")))
    (testing "only a page given one carries a bootstrap payload"
      (is (not (str/includes? plain "id=\"bootstrap\"")))
      (is (str/includes? client "id=\"bootstrap\"")))
    (testing "a page with no payload still gets its <main>"
      (is (str/includes? plain "<main")))))

(deftest shell-data-test
  (let [request {:uri "/" :query-params {:q "hund" :corpus "PROBE"}}
        data    (api/shell-data request {:q "hund" :corpus ["PROBE"]})]
    (testing "the masthead travels in the view data, so a routed navigation
              re-renders it rather than leaving last render's links"
      (is (= "/" (:path data)))
      (is (contains? data :nav)))
    (testing "returning to the search keeps the query"
      (is (str/includes? (:search (:nav data)) "q=hund")))
    (testing "no URL names a language: that is the reader's own preference"
      (is (= "/corpora" (:corpora-heading (:nav data))))
      (is (not (str/includes? (:search (:nav data)) "lang="))))
    (testing "the frequency table is not a place: it is a view of a result"
      (is (not (contains? (:nav data) :frequencies))))))

(deftest result-title-test
  (let [params {:q "hund" :corpus ["PROBE"] :attr "lemma"}
        result {:size 6 :page 0 :counts [{:corpus "PROBE" :size 6}]}]
    (testing "the concordance names its hit count"
      (is (= "hund · 6 hits · PROBE · corpus-probe"
             (api/result-title "en" :kwic params result))))
    (testing "a frequency table counts values, so it names what it grouped"
      (is (= "hund · PROBE · by lemma · Frequencies · corpus-probe"
             (api/result-title "en" :frequencies params result))))
    (testing "a whole-corpus table says so rather than naming a query"
      (is (= "All tokens · PROBE · by lemma · Frequencies · corpus-probe"
             (api/result-title "en" :frequencies (assoc params :q "")
                               result))))))

(deftest view-hrefs-test
  (let [hrefs (api/view-hrefs {:q "hund" :corpus ["PROBE"] :lang "da"})]
    (testing "one entry per view, in display order"
      (is (= [:kwic :frequencies] (map first hrefs)))
      (is (= [:concordance :frequencies] (map second hrefs))))
    (testing "every view of one search shares its URL but for the view param"
      (doseq [[_ _ href] hrefs]
        (is (str/includes? href "q=hund"))
        (is (str/includes? href "corpus=PROBE"))
        (is (str/ends-with? href "#results")))
      (is (str/includes? (last (first hrefs)) "view=kwic"))
      (is (str/includes? (last (second hrefs)) "view=frequencies")))))

(deftest view-param-test
  (is (= :kwic (api/view-param nil)))
  (is (= :kwic (api/view-param "kwic")))
  (is (= :kwic (api/view-param "nonesuch")))
  (is (= :frequencies (api/view-param "frequencies"))))

(deftest search-title-test
  (testing "no query is just the app name"
    (is (= "corpus-probe" (api/search-title "en" {}))))
  (testing "a search names the query and corpus"
    (is (= "hund · PROBE · corpus-probe"
           (api/search-title "en" {:q "hund" :corpus ["PROBE"]}))))
  (testing "several corpora are counted"
    (is (= "hund · 2 corpora · corpus-probe"
           (api/search-title "en" {:q "hund" :corpus ["PROBE" "VISER"]})))
    (is (= "hund · 2 korpusser · corpus-probe"
           (api/search-title "da" {:q "hund" :corpus ["PROBE" "VISER"]}))))
  (testing "no corpora are not counted"
    (is (= "hund · corpus-probe"
           (api/search-title "en" {:q "hund" :corpus []}))))
  (testing "the outcome rides in the title, which is all a reload announces"
    (is (= "hund · 6 hits · PROBE · corpus-probe"
           (api/search-title "en" {:q "hund" :corpus ["PROBE"]}
                             {:size 6 :page 0
                              :counts [{:corpus "PROBE" :size 6}]})))
    (testing "with the page number once past the first"
      (is (= "hund · 6 hits · PROBE · page 3 · corpus-probe"
             (api/search-title "en" {:q "hund" :corpus ["PROBE"]}
                               {:size 6 :page 2
                                :counts [{:corpus "PROBE" :size 6}]}))))
    (testing "a search no corpus answered reports no count"
      (is (= "hund · PROBE · corpus-probe"
             (api/search-title "en" {:q "hund" :corpus ["PROBE"]}
                               {:size 0 :page 0
                                :counts [{:corpus "PROBE"
                                          :error {:type :timeout}}]})))))
  (testing "a metadata filter is named"
    (is (= "hund · PROBE · text_year 1591 · corpus-probe"
           (api/search-title "en" {:q "hund" :corpus ["PROBE"]
                                   :f.text_year ["1591"]})))))

(deftest accept-language-test
  (testing "the first language we have wins, by quality"
    (is (= "da" (api/accept-language "da-DK,da;q=0.9,en;q=0.8")))
    (is (= "en" (api/accept-language "en-GB,en;q=0.9")))
    (is (= "en" (api/accept-language "de,en;q=0.7,fr;q=0.9"))))
  (testing "the quality parameter is case-insensitive and may carry spaces"
    (is (= "da" (api/accept-language "da;Q=1")))
    (is (= "en" (api/accept-language "de , en ; q=0.9"))))
  (testing "a language offered at quality 0 is refused, not preferred"
    (is (nil? (api/accept-language "en;q=0")))
    (is (= "da" (api/accept-language "en;q=0,da;q=0.1"))))
  (testing "a language we do not have yields nothing"
    (is (nil? (api/accept-language "de-DE,fr;q=0.8")))
    (is (nil? (api/accept-language "")))
    (is (nil? (api/accept-language nil)))))

(deftest request-language-test
  (testing "the language the reader chose wins over what their browser asks"
    (is (= "en" (api/request-language
                 {:headers {"cookie" "lang=en" "accept-language" "da"}}))))
  (testing "a stored language we do not have falls back to the header"
    (is (= "en" (api/request-language
                 {:headers {"cookie" "lang=de" "accept-language" "en"}}))))
  (testing "without either, Danish"
    (is (= "da" (api/request-language {})))
    (is (= "da" (api/request-language {:headers {"accept-language" "de"}}))))
  (testing "the URL has no say: a shared link imposes no language"
    (is (= "da" (api/request-language {:query-params {:lang "en"}})))
    (is (= "en" (api/request-language
                 {:query-params {:lang "da"}
                  :headers      {"cookie" "lang=en"}})))))

(deftest valueless-param-test
  (testing "a query param written without a value names nothing"
    ;; Pedestal parses `?foo` into {nil "foo"}, and a nil key names no
    ;; param and no metadata filter
    (is (not (api/filter-key? nil)))
    (is (= "a=1" (api/query-string {nil "foo" :a 1})))
    (is (= {} (api/filter-params {nil "foo"}))))
  (testing "so it does not fail the page it was appended to"
    (let [ctx {:registry "test/resources"}]
      (is (= 200 (:status (api/corpora-page ctx {:uri "/corpora"
                                                 :query-params {nil "foo"}}))))
      (is (= 200 (:status (api/search-page ctx {:uri "/"
                                                :query-params
                                                {nil "foo"}})))))))

(deftest cookie-value-test
  (testing "a stored setting is read back by its name"
    (is (= "en" (api/cookie-value "lang=en" :lang)))
    (is (= "da" (api/cookie-value "other=1; lang=da; more=2" :lang))))
  (testing "a value the setting does not accept is not read back"
    (is (nil? (api/cookie-value "lang=xx" :lang)))
    (is (nil? (api/cookie-value "" :lang)))
    (is (nil? (api/cookie-value nil :lang)))))

(deftest preference-cookies-test
  (testing "a named setting with a value it accepts is stored for a year"
    (is (= ["lang=en;Path=/;Max-Age=31536000;SameSite=Lax"]
           (api/preference-cookies {:lang "en"}))))
  (testing "a value the setting refuses stores nothing, not a fallback"
    (is (= [] (api/preference-cookies {:lang "xx"}))))
  (testing "a name the allowlist does not carry cannot be stored at all"
    (is (= [] (api/preference-cookies {:session "stolen" :evil "x"})))
    (is (= ["lang=en;Path=/;Max-Age=31536000;SameSite=Lax"]
           (api/preference-cookies {:lang "en" :session "stolen"}))))
  (testing "so a caller can never choose both a cookie's name and its value"
    (is (every? #(str/starts-with? % "lang=")
                (api/preference-cookies {:lang    "da"
                                         :return  "/"
                                         "lang"   "xx"
                                         :Path    "/evil"})))))

(deftest preferences-page-test
  (let [post (fn [params] (api/preferences-page nil {:form-params params}))]
    (testing "the choice is stored and the reader sent back where they were"
      (let [{:keys [status headers]} (post {:lang "en" :return "/?q=hund"})]
        (is (= 303 status))
        (is (= "/?q=hund" (get headers "Location")))
        (is (= ["lang=en;Path=/;Max-Age=31536000;SameSite=Lax"]
               (get headers "Set-Cookie")))))
    (testing "a return that names anywhere but this app is not followed"
      (is (= "/" (get-in (post {:lang "en" :return "https://evil.example/"})
                         [:headers "Location"])))
      (is (= "/" (get-in (post {:lang "en" :return "//evil.example/"})
                         [:headers "Location"]))))
    (testing "a request that stores nothing sets no cookie header at all"
      (is (not (contains? (:headers (post {:lang "xx" :return "/"}))
                          "Set-Cookie"))))))

(deftest safe-return-test
  (is (= "/?q=x" (api/safe-return "/?q=x")))
  (is (= "/" (api/safe-return "//evil.example")))
  (is (= "/" (api/safe-return "https://evil.example")))
  (is (= "/" (api/safe-return nil))))

(deftest scalar-params-test
  (testing "a repeated scalar param keeps its first value, corpus its vector"
    (is (= {:q "a" :page "1" :corpus ["A" "B"]}
           (api/scalar-params {:q ["a" "b"] :page "1" :corpus ["A" "B"]}))))
  (testing "metadata filter params keep their vectors too"
    (is (= {:f.text_year ["1591" "1583"]}
           (api/scalar-params {:f.text_year ["1591" "1583"]})))))

(deftest filter-params-test
  (testing "f. params become the filter map, one value or several"
    (is (= {:text_year #{"1591" "1583"} :text_author #{"ukendt"}}
           (api/filter-params {:q             "hund"
                               :f.text_year   ["1591" "1583"]
                               :f.text_author "ukendt"}))))
  (testing "blank values are dropped, and with them empty attributes"
    (is (= {} (api/filter-params {:f.text_year ["" " "]}))))
  (testing "a param naming no attribute is dropped"
    (is (= {} (api/filter-params {:f. "x"}))))
  (is (= {} (api/filter-params {:q "hund" :corpus ["A"]}))))

(deftest search-params-test
  (testing "the filter params identify a search along with the query"
    (is (= {:q "hund" :corpus ["A"] :f.text_year ["1591"]}
           (api/search-params {:q "hund" :corpus ["A"] :page "2" :sort "word"
                               :f.text_year ["1591"]})))))

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
    (is (= {:error {:type :no-corpus}}
           (api/search-outcome! {} [] [] "x" {:page 0}))))
  (testing "only unknown corpora yields an empty result reporting them"
    (let [{:keys [result]} (api/search-outcome! {} [] ["NOPE"] "x" {:page 0})]
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
        page (fn [params]
               (api/frequencies-page ctx
                                     {:query-params (assoc params
                                                           :lang "en")}))]
    (testing "the old frequency page is now a view of a search, and says so"
      (let [{:keys [status headers]} (page {:q "hund" :corpus "PROBE"})]
        (is (= 301 status))
        (let [location (get headers "Location")]
          (is (str/starts-with? location "/?"))
          (is (str/includes? location "view=frequencies"))
          (is (str/includes? location "q=hund"))
          (is (str/includes? location "corpus=PROBE"))
          (is (str/ends-with? location "#results")))))
    (testing "a grouping is named, so the redirect lands on a table"
      (is (str/includes? (get-in (page {:q "hund"}) [:headers "Location"])
                         "attr=word"))
      (is (str/includes? (get-in (page {:q "hund" :attr "lemma"})
                                 [:headers "Location"])
                         "attr=lemma")))))

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
    ;; the corpus is deliberately unreadable; its error, and the stack
    ;; trace with it, would only look like a failing test
    (is (= [{:type :positional :name :word}]
           (t/with-min-level :fatal
             (api/attr-options! {:registry "test/resources"
                                 :cqp      "no-such-cqp"}
                                ["REGISTRY-PROBE"])))))
  (when-cwb
   (testing "the union over corpora, positional first, keeps registry order"
     (is (= [:word :pos :lemma :s_id :text_id :text_title :text_year
             :text_speaker :text_party]
            (map :name (api/attr-options! ctx ["PROBE" "TALER"])))))))

(deftest export-failure-test
  (testing "a search that fails everywhere is a 400 with the reasons"
    (let [{:keys [status headers body]}
          (t/with-min-level :fatal
            (api/export-kwic {:registry "test/resources" :cqp "no-such-cqp"}
                             {:query-params {:corpus "REGISTRY-PROBE"
                                             :q      "hund"
                                             :format "tsv"}}))]
      (is (= 400 status))
      (is (str/starts-with? (get headers "Content-Type") "text/plain"))
      (is (str/starts-with? body "REGISTRY-PROBE: internal")))))
