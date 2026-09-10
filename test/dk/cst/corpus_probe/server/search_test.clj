(ns dk.cst.corpus-probe.server.search-test
  "The search page and its endpoints: what a request asks, the view data
  a page renders from, the citation it redirects to, and the counts,
  filters and context the client fetches alone."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [cognitect.transit :as transit]
            [dk.cst.corpus-probe.cwb.registry :as registry]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.search.cache :as cache]
            [dk.cst.corpus-probe.search.frequency :as frequency]
            [dk.cst.corpus-probe.server.request :as request]
            [dk.cst.corpus-probe.server.search :as search]
            [dk.cst.corpus-probe.settings :as settings]
            [dk.cst.corpus-probe.test.cwb :refer [ctx when-cwb]]
            [dk.cst.corpus-probe.url :as url]
            [taoensso.telemere :as t])
  (:import [java.io ByteArrayInputStream]))

(defn transit->
  "Decode transit-JSON string `s` (test helper, mirroring
  dk.cst.corpus-probe.server.response/->transit)."
  [s]
  (let [in (ByteArrayInputStream. (.getBytes s "UTF-8"))]
    (transit/read (transit/reader in :json))))

(defn query-pairs
  "Query string `s` as the params Pedestal reads it into, a key that
  repeats collecting a vector as it does there (test helper)."
  [s]
  (reduce (fn [m pair]
            (let [[k v] (str/split pair #"=" 2)
                  k     (keyword k)
                  v     (or v "")]
              (if-let [held (get m k)]
                (assoc m k (conj (if (vector? held) held [held]) v))
                (assoc m k v))))
          {}
          (remove str/blank? (str/split s #"&"))))

(deftest selected-corpora-test
  (let [selectable ["PROBE" "VISER"]]
    (testing "the corpora the request names win"
      (is (= ["PROBE"] (search/selected-corpora selectable {:corpus "probe"})))
      (is (= ["PROBE" "VISER"]
             (search/selected-corpora selectable {:corpus ["probe" "viser"]
                                                  :scope  "chosen"}))))
    (testing "a reader who unticked every corpus gets no corpus, not all"
      (is (= [] (search/selected-corpora selectable {:scope "chosen"}))))
    (testing "naming none searches every corpus the chooser offers"
      (is (= ["PROBE" "VISER"] (search/selected-corpora selectable {}))))))

(deftest form-corpora-test
  (when-cwb
   (let [form (fn [params]
                (get-in (search/search-view-data ctx {:query-params params})
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
                (:view-hrefs (search/search-view-data
                              {:registry "nonesuch"}
                              {:query-params {:q "hund"}}))))))

(deftest valueless-param-test
  (testing "a query param written without a value does not fail the page"
    (is (= 200 (:status (search/serve-search {:registry "test/resources"}
                                             {:uri          "/search"
                                              :query-params {nil "foo"}}))))))

(deftest serve-filters-test
  (let [asked (atom nil)
        call  (fn [params]
                (with-redefs [registry/entries
                              (fn [_] [{:id "probe"} {:id "viser"}])
                              frequency/filter-options!
                              (fn [_ corpora]
                                (reset! asked corpora)
                                {:attrs [{:name :text_year :rows []}]
                                 :unlisted []})]
                  (search/serve-filters {} {:query-params params})))]
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

(deftest switched-frequency-test
  (when-cwb
   (testing "a form submitted from the frequency view with its mode changed
             counts the query it carried, never every token of the corpora"
     (let [{:keys [result error tokens]}
           (search/search-view-data ctx {:query-params {:q      "hund"
                                                        :mode   "extended"
                                                        :view   "frequencies"
                                                        :corpus "PROBE"}})]
       (is (= 3 (get-in result [:counts 0 :size])))
       (is (nil? error))
       (is (= [{:id 1 :conditions [{:id 1 :v "hund"}]}
               {:id 2 :conditions [{:id 1}]}]
              tokens))))
   (testing "while a blank query runs nothing at all, in either view: a
             form with no query is a form, not a count of every token"
     (is (nil? (:result (search/search-view-data
                         ctx {:query-params {:view "frequencies"
                                             :corpus "PROBE"}}))))
     (is (nil? (:result (search/search-view-data
                         ctx {:query-params {:t1.attr "word" :t1.op "is"
                                             :t1.v "" :t1.min "1"
                                             :t1.max "1" :mode "simple"
                                             :view "frequencies"
                                             :corpus "PROBE"}})))))))

(deftest search-outcome-test
  (testing "nothing selected at all is the no-corpus error"
    (is (= {:error {:type :no-corpus}}
           (search/search-outcome! {} [] [] "x" {:page 0}))))
  (testing "only unknown corpora yields an empty result reporting them"
    (let [{:keys [result]} (search/search-outcome! {} [] ["NOPE"] "x" {:page 0})]
      (is (= [{:corpus "NOPE" :error {:type :unknown-corpus}}]
             (:counts result)))
      (is (= 0 (:size result)))
      (is (= 1 (:pages result))))))

(deftest serve-counts-test
  (when-cwb
   (cache/forget-counts!)
   (let [request {:headers      {"accept" "application/transit+json"}
                  :query-params {:q "[]"}}
         view    #(search/search-view-data ctx request)]
     (testing "a page the client renders arrives before the corpora past it
               are counted, linking onward on the hits it has"
       (let [{:keys [result next-href]} (view)]
         (is (= [{:corpus "PROBE" :size 47}] (:counts result)))
         (is (= ["TALER" "VISER"] (:remaining result)))
         (is (nil? (:pages result)))
         (is (some? next-href))))
     (testing "the count of the whole search follows, with what depends on it"
       (let [{:keys [status body]} (search/serve-counts ctx request)
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
       (let [{:keys [result]} (search/search-view-data
                               ctx (dissoc request :headers))]
         (is (nil? (:remaining result)))
         (is (= 6 (:pages result)))))
     (testing "a request describing no search is refused"
       (is (= 400 (:status (search/serve-counts ctx {:query-params {}})))))
     (cache/forget-counts!))))

(deftest public-counts-test
  (testing "per-corpus errors are prepared for display"
    (is (= [{:corpus "X" :error {:type :cqp :message "CQP Error: bad"}}]
           (:counts (search/public-counts
                     {:counts [{:corpus "X"
                                :error  {:type    :cqp
                                         :message (str "CL warning: /srv/x\n"
                                                       "CQP Error: bad")}}]}))))))

(deftest extended-mode-test
  (when-cwb
   (testing "an extended search runs, and the page knows its CQP, its
             tokens and the values its fields suggest"
     (let [{:keys [result params tokens value-lists]}
           (search/search-view-data ctx {:query-params {:mode    "extended"
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
                  (get-in (search/search-outcome! ctx ["PROBE"] [] cqp
                                                  {:page 0 :within :sentence})
                          [:result :size]))]
       (is (= 1 (size "<s> [word = \"Hunden\"]")))
       (is (= 6 (size "<s> []")))
       (is (= 6 (size "[] </s>")))))
   (testing "an attribute one corpus lacks, or cannot list, has no value list"
     (is (= [:lemma :pos :word]
            (sort (keys (search/value-lists! ctx ["PROBE" "VISER"]
                                             [:word :pos :lemma])))))
     (is (= [:word]
            (sort (keys (search/value-lists! ctx ["PROBE" "TALER"]
                                             [:word :pos :lemma]))))))))

(deftest switch-page-test
  (when-cwb
   (let [page (fn [params]
                (search/search-view-data
                 ctx {:query-params (assoc params :corpus "PROBE")}))]
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
   (let [fetch (fn [query-string]
                 (search/serve-search ctx
                                      {:query-params (query-pairs query-string)
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
       (is (= 200 (:status (search/serve-search
                            ctx {:query-params {:q "hund" :match ""
                                                :corpus "PROBE"}
                                 :query-string "q=hund&match=&corpus=PROBE"
                                 :headers      {"accept"
                                                url/transit-type}}))))))))

(deftest seeded-form-test
  (when-cwb
   (let [stored {"cookie" "settings=corpus=VISER&view=frequencies&sort=word"}
         page   (fn [params headers]
                  (search/search-view-data ctx {:query-params params
                                                :headers      headers}))]
     (testing "a page asked for nothing shows the settings its reader stored"
       (let [{:keys [params view]} (page {} stored)]
         (is (= ["VISER"] (:corpus params)))
         (is (= "word" (:sort params)))
         ;; which view a result is shown in is not a stored setting, so a
         ;; seeded form starts at the concordance whatever was stored
         (is (= :kwic view))))
     (testing "and runs nothing, since the reader has asked for no search:
               a page they only arrived at spends no query"
       (let [{:keys [result error]} (page {} stored)]
         (is (nil? result))
         (is (nil? error))))
     (testing "how a query is matched rides on the query, and a seeded
               form has none, so it shows what was stored as it stands"
       (let [{:keys [params]} (page {} {"cookie" (str "settings=in=lemma&ci=on"
                                                      "&match=prefix"
                                                      "&within=text")})]
         (is (= {:in "lemma" :ci "on" :match "prefix" :within "text"}
                (select-keys params [:in :ci :match :within])))))
     (testing "a page asked for a search reads it from the URL alone, so a
               link to a result finds the same hits for everyone"
       (is (= ["PROBE"] (:corpus (:params (page {:q "hund" :corpus "PROBE"}
                                                stored))))))
     (testing "a reader who stored nothing starts with no corpus selected"
       (is (empty? (:corpus (:params (page {} {})))))
       (is (= "" (:stored (page {} {})))))
     (testing "and one who stored every corpus gets every corpus back,
               not the empty form that names none"
       (let [{:keys [params stored]} (page {} {"cookie" "settings=scope=all"})]
         (is (= ["PROBE" "TALER" "VISER"] (sort (:corpus params))))
         ;; or the form would read as departing from what it is
         (is (= "scope=all" stored))))
     (testing "the page carries what is stored, so the form can be told
               from it, and whether a search stores itself"
       (is (= "corpus=VISER&sort=word" (:stored (page {} stored))))
       (is (true? (:autosave? (page {} {}))))
       (is (false? (:autosave? (page {} {"cookie" "settings=autosave=off"}))))))))

(deftest stores-settings-test
  (when-cwb
   (let [fetch  (fn [query-string]
                  (search/serve-search ctx
                                       {:query-params (query-pairs query-string)
                                        :query-string query-string
                                        :headers      {}}))
         stored (fn [response]
                  (some-> (get-in response [:headers "Set-Cookie"])
                          (first)
                          (request/cookie-value :settings)
                          (settings/params)))]
     (testing "a submitted form stores what it departed from the app's
               own defaults by, and nothing it left alone"
       (is (= {:corpus "PROBE" :in "lemma" :ci "on"}
              (stored
               (fetch "q=hund&corpus=PROBE&scope=chosen&mode=simple&in=lemma&ci=on")))))
     (testing "a search over every corpus stores that it was every corpus,
               so the form comes back with them ticked rather than empty"
       (is (= {:scope "all"}
              (stored (fetch (str "q=hund&scope=chosen&mode=simple"
                                  "&corpus=PROBE&corpus=TALER&corpus=VISER"))))))
     (testing "not while the reader has turned storing off"
       (is (nil? (some-> (search/serve-search
                          ctx {:query-params (query-pairs "q=hund&corpus=PROBE")
                               :query-string "q=hund&corpus=PROBE&scope=chosen"
                               :headers      {"cookie" "settings=autosave=off"}})
                         (get-in [:headers "Set-Cookie"])))))
     (testing "a box left unticked is stored as the absence it is submitted
               as, so the next form starts unticked too"
       (is (nil? (:ci (stored (fetch "q=hund&corpus=PROBE&scope=chosen"))))))
     (testing "not the query, nor which page of the result was asked for"
       (is (nil? (:q (stored (fetch "q=hund&corpus=PROBE&scope=chosen&page=2"))))))
     (testing "a link to a result stores nothing: it says how the reader
               who shared it works, not how the reader who followed it does"
       (is (nil? (stored (fetch "q=hund&corpus=PROBE")))))
     (testing "nor does a bare page, which asked nothing"
       (is (nil? (stored (fetch ""))))))))

(deftest attr-options-test
  (testing "an unreadable corpus contributes nothing, word remains"
    ;; the corpus is deliberately unreadable; its error, and the stack
    ;; trace with it, would only look like a failing test
    (is (= [{:type :positional :name :word}]
           (t/with-min-level :fatal
             (search/attr-options! {:registry "test/resources"
                                    :cqp      "no-such-cqp"}
                                   ["REGISTRY-PROBE"])))))
  (when-cwb
   (testing "the union over corpora, positional first, keeps registry order"
     (is (= [:word :pos :lemma :s_id :text_id :text_title :text_year
             :text_speaker :text_party]
            (map :name (search/attr-options! ctx ["PROBE" "TALER"])))))))

(deftest sort-options-test
  (testing "the fixed modes, then a sort by each attribute but word"
    (is (= ["corpus" "word" "reverse" "left" "right" "random" "lemma" "pos"]
           (search/sort-options [:word :lemma :pos])))
    (is (= ["corpus" "word" "reverse" "left" "right" "random"]
           (search/sort-options [:word])))))

(deftest list-mode-test
  (testing "a list compiles to one token pattern"
    (is (= "[lemma = \"(hund|kat)\"]"
           (query/->cqp (query/of {:q "hund\nkat" :in "lemma"})))))
  (testing "a list is one token, so it is kept within nothing"
    (is (nil? (query/within (query/of {:q "hund\nkat"}))))))
