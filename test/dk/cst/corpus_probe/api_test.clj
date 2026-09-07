(ns dk.cst.corpus-probe.api-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [cognitect.transit :as transit]
            [dk.cst.corpus-probe.api :as api]
            [dk.cst.corpus-probe.search.cache :as cache]
            [dk.cst.corpus-probe.cwb.registry :as registry]
            [dk.cst.corpus-probe.test.cwb :refer [ctx when-cwb]]
            [dk.cst.corpus-probe.test.hiccup :refer [da en]]
            [dk.cst.corpus-probe.search.frequency :as frequency]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.url :as url]
            [taoensso.telemere :as t])
  (:import [java.io ByteArrayInputStream]))

(defn transit->
  "Decode transit-JSON string `s` (test helper, mirroring api/->transit)."
  [s]
  (let [in (ByteArrayInputStream. (.getBytes s "UTF-8"))]
    (transit/read (transit/reader in :json))))

(deftest position-param-test
  (is (= "match[-1]" (api/position-param "match[-1]")))
  (testing "anything but CQP's four positions is the start of the match"
    (is (= "match" (api/position-param nil)))
    (is (= "match" (api/position-param "target")))))

(deftest subset-param-test
  (is (= {:anchor "matchend[1]" :attr :lemma :value "kat"}
         (api/subset-param {:subset      "kat"
                            :subset-at   "matchend[1]"
                            :subset-attr "lemma"})))
  (testing "the anchor and attribute fall back as their params do"
    (is (= {:anchor "match" :attr :word :value "kat"}
           (api/subset-param {:subset "kat"}))))
  (testing "no value, no narrowing"
    (is (nil? (api/subset-param {:subset "" :subset-attr "lemma"})))
    (is (nil? (api/subset-param {})))))

(deftest context-param-test
  (is (= 10 (api/context-param "10")))
  (is (= :sentence (api/context-param "sentence")))
  (testing "anything else is the usual width"
    (is (= 5 (api/context-param nil)))
    (is (= 5 (api/context-param "0")))
    (is (= 5 (api/context-param "chapter")))))

(deftest near-param-test
  (testing "a word and how far away it may be"
    (is (= {:word "kat" :distance 3} (api/near-param " kat " "3"))))
  (testing "a distance that is not a positive integer is the default"
    (let [default (parse-long (:distance url/defaults))]
      (is (= {:word "kat" :distance default} (api/near-param "kat" nil)))
      (is (= {:word "kat" :distance default} (api/near-param "kat" "0")))
      (is (= {:word "kat" :distance default} (api/near-param "kat" "x")))))
  (testing "no word, nothing to be near"
    (is (nil? (api/near-param "" "5")))
    (is (nil? (api/near-param nil nil)))))

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
              (sort (api/selected-corpora ctx (registry/entries ctx) {}))))))))

(deftest form-corpora-test
  (when-cwb
   (let [form (fn [params]
                (get-in (api/search-view-data ctx {:query-params params})
                        [:params :corpus]))]
     (testing "the form starts with no corpus selected: choosing them is
               the reader's first move, and the chooser refuses a search
               without one"
       (is (= [] (form {}))))
     (testing "but shows what a search searched, which for a URL naming no
               corpus is every readable one"
       (is (= ["PROBE" "TALER" "VISER"] (sort (form {:q "hund"})))))
     (testing "and what a URL named, searched or not"
       (is (= ["VISER"] (form {:corpus "viser"})))
       (is (= ["VISER"] (form {:corpus "viser" :q "hund"})))))))

(deftest results-fragment-hrefs-test
  (testing "every view of the same search lands on the results"
    (is (every? #(str/ends-with? (last %) "#results")
                (:view-hrefs (api/search-view-data
                              {:registry "nonesuch"}
                              {:query-params {:q "hund"}}))))))

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
                (with-redefs [registry/entries
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

(deftest document-page-test
  (let [page (fn [name lang]
               (api/document-page {} name {:headers {"cookie" (str "lang=" lang)}}))]
    (testing "a document names its page by its own heading, in the language read"
      (is (= "Corpus search · corpus-probe"
             (second (re-find #"<title>([^<]*)" (:body (page "frontpage" "en"))))))
      (is (= "Ordliste · corpus-probe"
             (second (re-find #"<title>([^<]*)" (:body (page "glossary" "da")))))))
    (testing "and is a document page, headed by the document"
      (let [body (:body (page "glossary" "en"))]
        (is (str/includes? body "<main id=\"main\" tabindex=\"-1\" class=\"document\">"))
        (is (str/includes? body "<h1 id=\"glossary\">Glossary</h1>"))
        (is (str/includes? body "<dt id=\"kwic\">KWIC</dt>"))))))

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
        (is (str/includes? body "Unreadable corpus"))
        (is (not (str/includes? body "/corpora/data/probe")))))))

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
    (testing "the masthead and the footer mount too: a routed language
              switch re-words both without a reload"
      (is (str/includes? plain "<div id=\"masthead\"><header"))
      (is (str/includes? plain "<div id=\"footer\"><footer")))
    (testing "only a page given one carries a bootstrap payload"
      (is (not (str/includes? plain "id=\"bootstrap\"")))
      (is (str/includes? client "id=\"bootstrap\"")))
    (testing "a page with no payload still gets its <main>"
      (is (str/includes? plain "<main")))))

(deftest shell-data-test
  (let [request {:uri "/search" :query-params {:q "hund" :corpus "PROBE"}}
        data    (api/shell-data request {:q "hund" :corpus ["PROBE"]})]
    (testing "the masthead travels in the view data, so a routed navigation
              re-renders it rather than leaving last render's links"
      (is (= "/search" (:path data)))
      (is (contains? data :nav)))
    (testing "returning to the search keeps the query"
      (is (= "/search?q=hund&corpus=PROBE#results" (:search (:nav data)))))
    (testing "and without one is the bare search page"
      (is (= "/search" (:search (:nav (api/shell-data request {}))))))
    (testing "no URL names a language: that is the reader's own preference"
      (is (= "/corpora" (:corpora-heading (:nav data))))
      (is (= "/glossary" (:glossary (:nav data))))
      (is (not (str/includes? (:search (:nav data)) "lang="))))
    (testing "the frequency table is not a place: it is a view of a result"
      (is (not (contains? (:nav data) :frequencies))))))

(deftest view-param-test
  (is (= :kwic (api/view-param nil)))
  (is (= :kwic (api/view-param "kwic")))
  (is (= :kwic (api/view-param "nonesuch")))
  (is (= :frequencies (api/view-param "frequencies"))))

(deftest accepted-languages-test
  (testing "every language offered, most preferred first, primary subtags"
    (is (= ["da" "en" "de"]
           (api/accepted-languages "de;q=0.5,da-DK,en;q=0.8"))))
  (testing "a language refused at quality 0 is left out, a blank offers none"
    (is (= ["da"] (api/accepted-languages "en;q=0,da")))
    (is (= [] (api/accepted-languages nil)))
    (is (= [] (api/accepted-languages "")))))

(deftest request-languages-test
  (let [languages (fn [headers] (api/request-languages {:headers headers}))]
    (testing "what the reader chose, then what they accept by quality, then
              the app's own Danish and English"
      (is (= ["en" "de" "da"] (languages {"cookie"          "lang=en"
                                          "accept-language" "de,da;q=0.8"})))
      (is (= ["de" "da" "en"] (languages {"accept-language" "de,da;q=0.8"})))
      (is (= ["de" "fr" "en" "da"]
             (languages {"accept-language" "de,en;q=0.7,fr;q=0.9"}))))
    (testing "a stored language we do not have is no choice at all"
      (is (= ["en" "da"] (languages {"cookie"          "lang=de"
                                     "accept-language" "en"}))))
    (testing "without either, the app's own"
      (is (= ["da" "en"] (languages {})))
      (is (= ["xx" "da" "en"] (languages {"accept-language" "xx"}))))
    (testing "the quality parameter is case-insensitive and may carry spaces"
      (is (= ["da" "en"] (languages {"accept-language" "da;Q=1"})))
      (is (= ["de" "en" "da"] (languages {"accept-language" "de , en ; q=0.9"}))))
    (testing "the URL has no say: a shared link imposes no language"
      (is (= ["da" "en"] (api/request-languages {:query-params {:lang "en"}}))))))

(deftest request-language-test
  (testing "the interface takes the first language it has"
    (is (= "en" (api/request-language
                 {:headers {"cookie" "lang=en" "accept-language" "da"}})))
    (is (= "en" (api/request-language
                 {:headers {"cookie" "lang=de" "accept-language" "en"}})))
    (is (= "en" (api/request-language
                 {:headers {"accept-language" "de,en;q=0.7,fr;q=0.9"}}))))
  (testing "without one it has, Danish"
    (is (= "da" (api/request-language {})))
    (is (= "da" (api/request-language {:headers {"accept-language" "de"}})))
    (is (= "da" (api/request-language
                 {:headers {"accept-language" "en;q=0,da;q=0.1"}}))))
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
    (is (= "q=x" (url/query-string {nil "foo" :q "x"})))
    (is (= {} (api/filter-params {nil "foo"}))))
  (testing "so it does not fail the page it was appended to"
    (let [ctx {:registry "test/resources"}]
      (is (= 200 (:status (api/corpora-page ctx {:uri "/corpora"
                                                 :query-params {nil "foo"}}))))
      (is (= 200 (:status (api/search-page ctx {:uri "/search"
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

(deftest pattern-params-test
  (testing "a pattern param is kept as the reader wrote it"
    (is (= {:text_title ["Hav.*"]}
           (api/pattern-params {:q "x" :fp.text_title "Hav.*"}))))
  (testing "a range of integers is spelt out"
    (is (= {:text_year ["1590|1591|1592"]}
           (api/pattern-params {:ff.text_year "1590" :ft.text_year "1592"})))
    (is (= "1583" (api/range-pattern "1583" "1583")))
    (testing "and not at all when either end is missing, or out of order"
      (is (= {} (api/pattern-params {:ff.text_year "1590"})))
      (is (nil? (api/range-pattern "1592" "1590")))
      (is (nil? (api/range-pattern "1590" "many"))))
    (testing "only so far"
      (is (= api/range-limit
             (count (str/split (api/range-pattern "0" "5000") #"\|"))))))
  (testing "both together, blanks and nameless params dropped"
    (is (= {:text_year ["15.." "1590|1591"]}
           (api/pattern-params {:fp.text_year "15.." :ff.text_year "1590"
                                :ft.text_year "1591" :fp.text_title " "
                                :fp. "x"})))))

(deftest pattern-fields-test
  (is (= {:patterns {:text_title "Hav.*"}
          :ranges   {:text_year ["1590" nil] :text_pages [nil "5"]}}
         (api/pattern-fields {:fp.text_title "Hav.*" :ff.text_year "1590"
                              :ft.text_pages "5" :q "x"}))))

(deftest switched-frequency-test
  (when-cwb
   (testing "a form submitted from the frequency view with its mode changed
             counts the query it carried, never every token of the corpora"
     (let [{:keys [result error tokens]}
           (api/search-view-data ctx {:query-params {:q      "hund"
                                                     :mode   "extended"
                                                     :view   "frequencies"
                                                     :corpus "PROBE"}})]
       (is (= 3 (get-in result [:counts 0 :size])))
       (is (nil? error))
       (is (= [{:id 1 :conditions [{:id 1 :v "hund"}]}
               {:id 2 :conditions [{:id 1}]}]
              tokens))))
   (testing "while a blank query still counts them all, from a form whose
             radio was changed too"
     (is (some? (:result (api/search-view-data
                          ctx {:query-params {:view "frequencies"
                                              :corpus "PROBE"}}))))
     (is (some? (:result (api/search-view-data
                          ctx {:query-params {:t1.attr "word" :t1.op "is"
                                              :t1.v "" :t1.min "1"
                                              :t1.max "1" :mode "simple"
                                              :view "frequencies"
                                              :corpus "PROBE"}})))))))

(deftest sample-param-test
  (is (= 100 (api/sample-param "100")))
  (testing "no sample is the whole result"
    (is (nil? (api/sample-param nil)))
    (is (nil? (api/sample-param ""))))
  (testing "a sample of none of the hits is no sample rather than an
            empty result, and neither is anything that is not a number"
    (is (nil? (api/sample-param "0")))
    (is (nil? (api/sample-param "-5")))
    (is (nil? (api/sample-param "many")))))

(deftest page-param-test
  (testing "the URL counts from one, the result from nought"
    (is (= 0 (api/page-param "1")))
    (is (= 2 (api/page-param "3"))))
  (testing "anything that is not a positive integer is the first page"
    (is (= 0 (api/page-param nil)))
    (is (= 0 (api/page-param "0")))
    (is (= 0 (api/page-param "-3")))
    (is (= 0 (api/page-param "x")))
    (is (= 0 (api/page-param "99999999999999999999")))))

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

(deftest counts-page-test
  (when-cwb
   (cache/forget-counts!)
   (let [request {:headers      {"accept" "application/transit+json"}
                  :query-params {:q "[]"}}
         view    #(api/search-view-data ctx request)]
     (testing "a page the client renders arrives before the corpora past it
               are counted, linking onward on the hits it has"
       (let [{:keys [result next-href]} (view)]
         (is (= [{:corpus "PROBE" :size 47}] (:counts result)))
         (is (= ["TALER" "VISER"] (:remaining result)))
         (is (nil? (:pages result)))
         (is (some? next-href))))
     (testing "the count of the whole search follows, with what depends on it"
       (let [{:keys [status body]} (api/counts-page ctx request)
             counted (transit-> body)]
         (is (= 200 status))
         (is (= ["PROBE" "TALER" "VISER"] (map :corpus (:counts counted))))
         (is (= 137 (:size counted)))
         (is (= 6 (:pages counted)))
         (is (str/ends-with? (:next-href counted) "page=2#results"))
         (is (str/includes? (:title counted) "137"))))
     (testing "after which the page is counted in full, from memory"
       (let [{:keys [result]} (view)]
         (is (nil? (:remaining result)))
         (is (= 6 (:pages result)))))
     (testing "a document waits for the count, having no script to ask with"
       (cache/forget-counts!)
       (let [{:keys [result]} (api/search-view-data ctx (dissoc request :headers))]
         (is (nil? (:remaining result)))
         (is (= 6 (:pages result)))))
     (testing "a request describing no search is refused"
       (is (= 400 (:status (api/counts-page ctx {:query-params {}})))))
     (cache/forget-counts!))))

(deftest public-counts-test
  (testing "per-corpus errors are prepared for display"
    (is (= [{:corpus "X" :error {:type :cqp :message "CQP Error: bad"}}]
           (:counts (api/public-counts
                     {:counts [{:corpus "X"
                                :error  {:type    :cqp
                                         :message (str "CL warning: /srv/x\n"
                                                       "CQP Error: bad")}}]}))))))

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

(deftest export-validation-test
  (let [ctx    {:registry "test/resources"}
        export (fn [file params]
                 (api/export-page ctx {:path-params  {:file file}
                                       :query-params params}))]
    (testing "an export needs a query and known corpora"
      (is (= 400 (:status (export "kwic.tsv" {:corpus "REGISTRY-PROBE"}))))
      (is (= 400 (:status (export "kwic.tsv" {:q "hund"}))))
      (is (= 400 (:status (export "kwic.tsv" {:corpus "NOPE" :q "hund"})))))
    (testing "a frequency export needs corpora"
      (is (= 400 (:status (export "frequencies.csv" {})))))
    (testing "a file that is not a view of a result in a format is not found"
      (is (= 404 (:status (export "kwic.xls" {:corpus "REGISTRY-PROBE"
                                               :q "hund"}))))
      (is (= 404 (:status (export "nonesuch.tsv" {:corpus "REGISTRY-PROBE"
                                                   :q "hund"}))))
      (is (= 404 (:status (export nil {:corpus "REGISTRY-PROBE" :q "hund"})))))))

(deftest extended-mode-test
  (when-cwb
   (testing "an extended search runs, and the page knows its CQP, its
             tokens and the values its fields suggest"
     (let [{:keys [result params tokens value-lists]}
           (api/search-view-data ctx {:query-params {:mode    "extended"
                                                     :corpus  "PROBE"
                                                     :t1.attr "lemma"
                                                     :t1.v    "hund"}})]
       (is (= 5 (:size result)))
       (is (= "[lemma = \"hund\"]" (query/->cqp (query/of params))))
       (is (= [{:id 1 :conditions [{:id 1 :attr "lemma" :v "hund"}]}
               {:id 2 :conditions [{:id 1}]}]
              tokens))
       (is (= 15 (count (:pos value-lists))))
       (is (some #{"NCSD"} (:pos value-lists)))))
   (testing "a sentence edge runs, its tags named for the corpus"
     (let [size (fn [cqp]
                  (get-in (api/search-outcome! ctx ["PROBE"] [] cqp
                                               {:page 0 :within :sentence})
                          [:result :size]))]
       (is (= 1 (size "<s> [word = \"Hunden\"]")))
       (is (= 6 (size "<s> []")))
       (is (= 6 (size "[] </s>")))))
   (testing "an attribute one corpus lacks, or cannot list, has no value list"
     (is (= [:lemma :pos :word]
            (sort (keys (api/value-lists! ctx ["PROBE" "VISER"]
                                          [:word :pos :lemma])))))
     (is (= [:word]
            (sort (keys (api/value-lists! ctx ["PROBE" "TALER"]
                                          [:word :pos :lemma]))))))))

(deftest switch-page-test
  (when-cwb
   (let [page (fn [params]
                (api/search-view-data ctx {:query-params (assoc params
                                                                :corpus "PROBE")}))]
     (testing "a form submitted into the extended mode runs the words it
               was typed as, as tokens, and cites them so"
       (let [{:keys [result params tokens switch cited]}
             (page {:q "hund" :in "lemma" :mode "extended"})]
         (is (= 5 (:size result)))
         (is (= "[lemma = \"hund\"]" (query/->cqp (query/of params))))
         (is (= [{:id 1 :conditions [{:id 1 :attr "lemma" :v "hund"}]}
                 {:id 2 :conditions [{:id 1}]}]
                tokens))
         (is (= {:loss [] :unread #{}} switch))
         (is (= {:t1.attr "lemma" :t1.v "hund" :corpus "PROBE"} cited))
         (testing "with the field it came from kept as memory, uncited"
           (is (= "hund" (:q params))))))
     (testing "a form submitted out of the extended mode hands the field
               the tokens' CQP, kept within their unit, and runs it"
       (let [{:keys [result error params switch tokens cited]}
             (page {:t1.attr "lemma" :t1.v "hund" :t2.op "any" :t2.max "3"
                    :mode "simple"})]
         (is (some? result))
         (is (nil? error))
         (is (= "[lemma = \"hund\"] []{1,3} within s" (:q params)))
         (is (= {:loss [] :unread #{}} switch))
         (is (= [{:id 1 :conditions [{:id 1}]}] tokens))
         (is (= {:q "[lemma = \"hund\"] []{1,3} within s" :corpus "PROBE"}
                cited))
         (testing "in the frequency view too"
           (is (some? (:result (page {:t1.attr "lemma" :t1.v "hund"
                                      :t2.op "any" :mode "simple"
                                      :view "frequencies"}))))))
       (let [{:keys [result params]}
             (page {:t1.v "lille" :t2.v "hund" :mode "simple"})]
         (is (= "[word = \"lille\"] [word = \"hund\"] within s"
                (:q params)))
         (is (= 1 (:size result)))))
     (testing "CQP submitted into the extended mode runs nothing, and the
               line says why"
       (let [{:keys [result params switch tokens]}
             (page {:q "[lemma = \"hund\"]" :mode "extended"})]
         (is (nil? result))
         (is (= [[:cqp "[lemma = \"hund\"]"]] (:loss switch)))
         (is (= [{:id 1 :conditions [{:id 1}]}] tokens))
         (testing "with the text kept as memory, uncited"
           (is (= "[lemma = \"hund\"]" (:q params)))))))))

(deftest citation-redirect-test
  (when-cwb
   (let [pairs (fn [query-string]
                 (into {}
                       (map (fn [pair]
                              (let [[k v] (str/split pair #"=" 2)]
                                [(keyword k) (or v "")])))
                       (remove str/blank? (str/split query-string #"&"))))
         fetch (fn [query-string]
                 (api/search-page ctx {:query-params (pairs query-string)
                                       :query-string query-string
                                       :headers      {}}))]
     (testing "a document asked for by a query string that is not the
               search's citation is sent to it"
       (let [{:keys [status headers]}
             (fetch "q=hund&match=&scope=chosen&corpus=PROBE&mode=simple")]
         (is (= 303 status))
         (is (= "/search?q=hund&corpus=PROBE#results"
                (get headers "Location")))))
     (testing "the citation itself, and the bare page, are answered"
       (is (= 200 (:status (fetch "q=hund&corpus=PROBE"))))
       (is (= 200 (:status (fetch "")))))
     (testing "not a form submitted with its mode changed: what it holds
               is not what it was given, and it runs nothing until sent"
       (is (= 200 (:status (fetch "t1.v=hund&mode=simple&corpus=PROBE")))))
     (testing "nor the client's own request for the data, which cites for
               itself"
       (is (= 200 (:status (api/search-page
                            ctx {:query-params {:q "hund" :match ""
                                                :corpus "PROBE"}
                                 :query-string "q=hund&match=&corpus=PROBE"
                                 :headers      {"accept"
                                                url/transit-type}}))))))))

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
            (api/export-page {:registry "test/resources" :cqp "no-such-cqp"}
                             {:path-params  {:file "kwic.tsv"}
                              :query-params {:corpus "REGISTRY-PROBE"
                                             :q      "hund"}}))]
      (is (= 400 status))
      (is (str/starts-with? (get headers "Content-Type") "text/plain"))
      (is (str/starts-with? body "REGISTRY-PROBE: internal")))))

(deftest sort-options-test
  (testing "the fixed modes, then a sort by each attribute but word"
    (is (= ["corpus" "word" "reverse" "left" "right" "random" "lemma" "pos"]
           (api/sort-options [:word :lemma :pos])))
    (is (= ["corpus" "word" "reverse" "left" "right" "random"]
           (api/sort-options [:word])))))

(deftest by-param-test
  (is (= :text_year (api/by-param "text_year")))
  (testing "no attribute, no second attribute"
    (is (nil? (api/by-param "")))
    (is (nil? (api/by-param nil)))))

(deftest list-mode-test
  (testing "a list compiles to one token pattern"
    (is (= "[lemma = \"(hund|kat)\"]"
           (query/->cqp (query/of {:q "hund\nkat" :in "lemma"})))))
  (testing "a list is one token, so it is kept within nothing"
    (is (nil? (query/within (query/of {:q "hund\nkat"}))))))

(deftest text-page-test
  (let [page (fn [id params]
               (api/text-page {:registry "test/resources"}
                              {:path-params  {:id id}
                               :query-params params
                               :headers      {"cookie" "lang=en"}}))]
    (testing "a hostile or unknown corpus, or no position, is not found"
      (is (= 404 (:status (page "bad; exit" {:cpos "9"}))))
      (is (= 404 (:status (page "nope" {:cpos "9"}))))
      (is (= 404 (:status (page "registry-probe" {:cpos "nine"})))))
    (testing "without a position the reader is sent to the corpus page"
      (is (= 303 (:status (page "registry-probe" {}))))
      (is (= "/corpora/registry-probe"
             (get-in (page "registry-probe" {:cpos ""}) [:headers "Location"]))))
    (testing "a corpus whose data are gone is a page saying so, and its
              paths stay private"
      (let [{:keys [status body]} (page "registry-probe" {:cpos "9"})]
        (is (= 200 status))
        (is (str/includes? body "CQP error"))
        (is (not (str/includes? body "/corpora/data/probe")))))))

(deftest export-stream-test
  (when-cwb
   (let [download (fn [file params]
                    (let [{:keys [status headers body]}
                          (api/export-page ctx {:path-params  {:file file}
                                                :query-params params})
                          out (java.io.ByteArrayOutputStream.)]
                      (body out)
                      {:status  status
                       :type    (get headers "Content-Type")
                       :text    (String. (.toByteArray out) "UTF-8")}))
         {:keys [status type text]} (download "kwic.tsv" {:corpus "PROBE,TALER"
                                                          :q      "hund"})
         lines (str/split-lines text)]
     (testing "the download is written as the corpora answer, under the
               columns of them all"
       (is (= 200 status))
       (is (str/starts-with? type "text/tab-separated-values"))
       (is (= (str "corpus\tcpos\tmatchend\tleft\tmatch\tright\tmatch pos\t"
                   "match lemma\ts_id\ttext_id\ttext_title\ttext_year\t"
                   "text_speaker\ttext_party")
              (first lines)))
       (is (some #{(str "PROBE\t9\t9\t. Katten jagter en lille\thund\t"
                        "i haven . Hunde og\tNCSI\thund\t2\tt1\tHverdag\t"
                        "2023\t\t")}
                 lines)))
     (testing "as CSV, with the byte order mark and CRLF"
       (let [{:keys [text]} (download "kwic.csv" {:corpus "PROBE" :q "hund"})]
         (is (str/starts-with? text "﻿corpus,cpos,matchend,"))
         (is (str/includes? text "\r\n")))))))
