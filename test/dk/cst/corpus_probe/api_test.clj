(ns dk.cst.corpus-probe.api-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [cognitect.transit :as transit]
            [dk.cst.corpus-probe.api :as api]
            [dk.cst.corpus-probe.cache :as cache]
            [dk.cst.corpus-probe.corpus :as corpus]
            [dk.cst.corpus-probe.cqp-test :refer [ctx when-cwb]]
            [dk.cst.corpus-probe.frequency :as frequency]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.hiccup :refer [da en]]
            [dk.cst.corpus-probe.views.page :as page]
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

(deftest subset-href-test
  (let [href (api/subset-href {:q "hund" :corpus ["PROBE"] :attr "lemma"
                               :at "match[-1]" :sort "word"}
                              :lemma "match[-1]" "en kat")]
    (testing "the concordance of the same search, kept to the row's hits"
      (is (str/starts-with? href "/search?"))
      (is (str/includes? href "subset=en+kat"))
      (is (str/includes? href "subset-at=match%5B-1%5D"))
      (is (str/includes? href "subset-attr=lemma"))
      (is (str/ends-with? href "#results")))
    (testing "the concordance is the default view, so it goes unnamed"
      (is (not (str/includes? href "view="))))
    (testing "the grouping and the order are the table's, not the hits'"
      (is (not (str/includes? href "attr=lemma&")))
      (is (not (str/includes? href "sort="))))))

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
    (is (= {:word "kat" :distance page/near-distance}
           (api/near-param "kat" nil)))
    (is (= {:word "kat" :distance page/near-distance}
           (api/near-param "kat" "0")))
    (is (= {:word "kat" :distance page/near-distance}
           (api/near-param "kat" "x"))))
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
              (sort (api/selected-corpora ctx (corpus/corpora ctx) {}))))))))

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
  (testing "a page turn lands on the results, not the top of the form"
    (is (str/ends-with? (api/page-href {:q "hund"} 1) "#results")))
  (testing "so does every view of the same search"
    (is (every? #(str/ends-with? (last %) "#results")
                (:view-hrefs (api/search-view-data
                              {:registry "nonesuch"}
                              {:query-params {:q "hund"}}))))))

(deftest page-href-test
  (let [href (api/page-href {:corpus "PROBE" :q "hund" :page "1"} 2)]
    (is (str/starts-with? href "/search?"))
    (is (str/includes? href "corpus=PROBE"))
    (testing "the URL counts pages from one, as the page does"
      (is (str/includes? href "page=3"))
      (is (not (str/includes? (api/page-href {:q "hund"} 0) "page="))))
    (testing "the query is URL-encoded"
      (is (str/includes? (api/page-href {:q "[lemma=\"a\"]"} 1) "%5B")))
    (testing "the per-page expand parameter is dropped"
      (is (not (str/includes? (api/page-href {:corpus "PROBE" :expand "9"} 1)
                              "expand"))))))

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

(deftest result-title-test
  (let [params {:q "hund" :corpus ["PROBE"] :attr "lemma"}
        result {:size 6 :page 0 :counts [{:corpus "PROBE" :size 6}]}]
    (testing "the concordance names its hit count"
      (is (= "hund · 6 hits · PROBE · corpus-probe"
             (api/result-title en :kwic params result))))
    (testing "a frequency table counts values, so it names what it grouped"
      (is (= "hund · PROBE · by lemma · Frequencies · corpus-probe"
             (api/result-title en :frequencies params result))))
    (testing "a whole-corpus table says so rather than naming a query"
      (is (= "All tokens · PROBE · by lemma · Frequencies · corpus-probe"
             (api/result-title en :frequencies (assoc params :q "")
                               result))))))

(deftest view-hrefs-test
  (let [hrefs (api/view-hrefs {:q "hund" :corpus ["PROBE"] :lang "da"})]
    (testing "one entry per view, in display order"
      (is (= [:kwic :frequencies] (map first hrefs))))
    (testing "every view of one search shares its URL but for the view param"
      (doseq [[_ href] hrefs]
        (is (str/starts-with? href "/search?q=hund&corpus=PROBE"))
        (is (str/ends-with? href "#results")))
      (testing "and the concordance, being the default, goes unnamed"
        (is (not (str/includes? (last (first hrefs)) "view=")))
        (is (str/includes? (last (second hrefs)) "view=frequencies"))))
    (testing "no URL names a language: that is the reader's own preference"
      (is (not (some #(str/includes? (last %) "lang") hrefs))))))

(deftest view-param-test
  (is (= :kwic (api/view-param nil)))
  (is (= :kwic (api/view-param "kwic")))
  (is (= :kwic (api/view-param "nonesuch")))
  (is (= :frequencies (api/view-param "frequencies"))))

(deftest search-title-test
  (testing "no query names the page, which the frontpage's title does not"
    (is (= "Search · corpus-probe" (api/search-title en {})))
    (is (= "Søgning · corpus-probe" (api/search-title da {}))))
  (testing "a search names the query and corpus"
    (is (= "hund · PROBE · corpus-probe"
           (api/search-title en {:q "hund" :corpus ["PROBE"]}))))
  (testing "several corpora are counted"
    (is (= "hund · 2 corpora · corpus-probe"
           (api/search-title en {:q "hund" :corpus ["PROBE" "VISER"]})))
    (is (= "hund · 2 korpusser · corpus-probe"
           (api/search-title da {:q "hund" :corpus ["PROBE" "VISER"]}))))
  (testing "no corpora are not counted"
    (is (= "hund · corpus-probe"
           (api/search-title en {:q "hund" :corpus []}))))
  (testing "the outcome rides in the title, which is all a reload announces"
    (is (= "hund · 6 hits · PROBE · corpus-probe"
           (api/search-title en {:q "hund" :corpus ["PROBE"]}
                             {:size 6 :page 0
                              :counts [{:corpus "PROBE" :size 6}]})))
    (testing "with the page number once past the first"
      (is (= "hund · 6 hits · PROBE · page 3 · corpus-probe"
             (api/search-title en {:q "hund" :corpus ["PROBE"]}
                               {:size 6 :page 2
                                :counts [{:corpus "PROBE" :size 6}]}))))
    (testing "a search no corpus answered reports no count"
      (is (= "hund · PROBE · corpus-probe"
             (api/search-title en {:q "hund" :corpus ["PROBE"]}
                               {:size 0 :page 0
                                :counts [{:corpus "PROBE"
                                          :error {:type :timeout}}]})))))
  (testing "a metadata filter is named"
    (is (= "hund · PROBE · text_year 1591 · corpus-probe"
           (api/search-title en {:q "hund" :corpus ["PROBE"]
                                   :f.text_year ["1591"]}))))
  (testing "a sample says so beside the count it drew"
    (let [title (fn [size]
                  (api/search-title en {:q "hund" :corpus ["PROBE"]}
                                    {:size   size :page 0 :sample 100
                                     :counts [{:corpus "PROBE" :size size}]}))]
      (is (= (str "hund · 6 hits · a random sample of at most 100"
                  " · PROBE · corpus-probe")
             (title 6)))
      (testing "and a search that found nothing drew nothing, so it does not"
        (is (= "hund · 0 hits · PROBE · corpus-probe" (title 0)))))))

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

(deftest search-params-test
  (testing "the filter params identify a search along with the query"
    (is (= {:q "hund" :corpus ["A"] :f.text_year ["1591"]}
           (api/search-params {:q "hund" :corpus ["A"] :page "2" :sort "word"
                               :f.text_year ["1591"]})))
    (testing "and so do its patterns and ranges"
      (is (= {:q "hund" :fp.text_title "Hav.*" :ff.text_year "1590"
              :ft.text_year "1592"}
             (api/search-params {:q "hund" :fp.text_title "Hav.*"
                                 :ff.text_year "1590" :ft.text_year "1592"
                                 :page "2"})))))
  (testing "so does the sample, which decides which hits there are; the
            sort, which only decides their order, still does not"
    (is (= {:q "hund" :sample "100"}
           (api/search-params {:q "hund" :sample "100" :sort "word"}))))
  (testing "and so does the unit the words are kept within"
    (is (= {:q "lille hund" :within "text"}
           (api/search-params {:q "lille hund" :within "text" :sort "word"})))))

(deftest switched-frequency-test
  (when-cwb
   (testing "a form submitted from the frequency view with its mode changed
             counts nothing, rather than every token of the corpora"
     (let [{:keys [result error tokens]}
           (api/search-view-data ctx {:query-params {:q      "hund"
                                                     :mode   "extended"
                                                     :view   "frequencies"
                                                     :corpus "PROBE"}})]
       (is (nil? result))
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

(deftest page-hrefs-test
  (let [params {:q "hund"}]
    (testing "the first page links onward only when the hits reach past it"
      (is (= {:prev-href nil :next-href nil}
             (api/page-hrefs params 0 {:size 25 :page-size 25})))
      (is (str/ends-with? (:next-href (api/page-hrefs params 0 {:size      26
                                                                :page-size 25}))
                          "page=2#results")))
    (testing "a result still being counted links onward on what it has so far"
      (is (some? (:next-href (api/page-hrefs params 0 {:size      26
                                                       :page-size 25
                                                       :remaining ["X"]}))))
      (is (nil? (:next-href (api/page-hrefs params 0 {:size      10
                                                      :page-size 25
                                                      :remaining ["X"]})))))
    (testing "and back from any page but the first, result or no result"
      (is (str/ends-with? (:prev-href (api/page-hrefs params 2 nil))
                          "page=2#results"))
      (is (nil? (:next-href (api/page-hrefs params 2 nil)))))))

(deftest counts-page-test
  (when-cwb
   (cache/forget-counts!)
   (let [request {:headers      {"accept" "application/transit+json"}
                  :query-params {:q "[]" :mode "cqp"}}
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
  (testing "the titles name the CQP the rows compiled to"
    (let [params {:mode "extended" :t1.attr "lemma" :t1.v "hund"
                  :cqp "[lemma = \"hund\"]" :corpus ["PROBE"]}]
      (is (str/starts-with? (api/search-title en params {:size   5
                                                          :page   0
                                                          :counts [{:corpus "PROBE"
                                                                    :size   5}]})
                            "[lemma = \"hund\"] · 5 hits"))
      (is (str/starts-with? (api/frequency-title en (assoc params :attr "word"))
                            "[lemma = \"hund\"]"))
      (is (str/starts-with? (api/search-title en (dissoc params :cqp :t1.attr
                                                         :t1.v)
                                              nil)
                            "Search"))))
  (testing "the tokens identify the search, as the query does"
    (is (= {:mode "extended" :t1.v "a" :t2.op "any"}
           (api/search-params {:mode "extended" :t1.v "a" :t2.op "any"
                               :page "2"}))))
  (testing "the form's tokens are those asked for, their conditions
            likewise, and one blank, numbered afresh"
    (is (= [{:id 1 :conditions [{:id 1 :v "a"} {:id 2 :v "c" :join "or"}]}
            {:id 2 :max "2" :conditions [{:id 1 :op "any"}]}
            {:id 3 :conditions [{:id 1}]}]
           (api/token-fields {:mode "extended"
                              :t1.v "a" :t1.2.attr "pos" :t1.2.join "and"
                              :t1.3.v "c" :t1.3.join "or"
                              :t2.ci "on" :t5.op "any" :t5.max "2"})))
    (is (= [{:id 1 :conditions [{:id 1}]}] (api/token-fields {:q "x"})))
    (testing "and the blank alone under any other mode, whose form has no
              tokens, whatever the URL carries"
      (is (= [{:id 1 :conditions [{:id 1}]}]
             (api/token-fields {:t1.v "a" :mode "simple"}))))
    (testing "seeded from the query a reader typed before switching mode,
              read as the mode they typed it in"
      (is (= [{:id 1 :conditions [{:id 1 :v "hund" :ci "on"}]}
              {:id 2 :conditions [{:id 1}]}]
             (api/token-fields {:q "hund" :ci "on" :mode "extended"})))
      (is (= [{:id 1 :conditions [{:id 1 :v "hund"}
                                  {:id 2 :v "kat" :join "or"}]}
              {:id 2 :conditions [{:id 1}]}]
             (api/token-fields {:q "hund\nkat" :mode "extended"
                                :from "list"})))))
  (when-cwb
   (testing "an extended search runs, and the page knows its CQP, its
             tokens and the values its fields suggest"
     (let [{:keys [result params tokens value-lists]}
           (api/search-view-data ctx {:query-params {:mode    "extended"
                                                     :corpus  "PROBE"
                                                     :t1.attr "lemma"
                                                     :t1.v    "hund"}})]
       (is (= 5 (:size result)))
       (is (= "[lemma = \"hund\"]" (:cqp params)))
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

(deftest export-hrefs-test
  (testing "the view of the search as a file, one URL per format"
    (is (= {:csv "/search/kwic.csv?q=hund&corpus=PROBE"
            :tsv "/search/kwic.tsv?q=hund&corpus=PROBE"}
           (api/export-hrefs :kwic {:corpus ["PROBE"] :q "hund"})))
    (is (= "/search/frequencies.tsv?q=hund&attr=lemma"
           (:tsv (api/export-hrefs :frequencies {:q "hund" :attr "lemma"}))))))

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

(deftest view-hrefs-by-test
  (testing "the second attribute of a table travels between the views"
    (is (every? #(str/includes? (second %) "by=text_year")
                (api/view-hrefs {:q "hund" :attr "lemma" :by "text_year"})))))

(deftest list-mode-test
  (testing "a list compiles to one token pattern"
    (is (= "[lemma = \"(hund|kat)\"]"
           (query/->cqp (query/of {:q "hund\nkat" :mode "list" :in "lemma"})))))
  (testing "a list is one token, so it is kept within nothing"
    (is (nil? (query/within (query/of {:q "hund\nkat" :mode "list"})))))
  (testing "a list is titled by its length, a title being one line"
    (is (str/starts-with? (api/search-title en {:q "hund\nkat\n" :mode "list"})
                          "2 words"))))

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
