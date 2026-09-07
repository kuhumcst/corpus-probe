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
            [dk.cst.corpus-probe.server.search :as search]
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

(deftest selected-corpora-test
  (let [entries [{:id "probe"} {:id "viser"}]]
    (testing "the corpora the request names win"
      (is (= ["PROBE"] (search/selected-corpora! nil entries {:corpus "probe"})))
      (is (= ["PROBE" "VISER"]
             (search/selected-corpora! nil entries {:corpus ["probe" "viser"]
                                                    :scope  "chosen"}))))
    (testing "a reader who unticked every corpus gets no corpus, not all"
      (is (= [] (search/selected-corpora! nil entries {:scope "chosen"}))))
    (when-cwb
     (testing "naming none searches every corpus CWB can read"
       (is (= ["PROBE" "TALER" "VISER"]
              (sort (search/selected-corpora! ctx (registry/entries ctx)
                                              {}))))))))

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

(deftest serve-context-validation-test
  (let [context (fn [params] (search/serve-context {} {:query-params params}))]
    (testing "a hostile corpus name is rejected before touching cqp"
      (is (= 400 (:status (context {:corpus   "bad; exit"
                                    :cpos     "9"
                                    :matchend "9"})))))
    (testing "a non-integer position is rejected"
      (is (= 400 (:status (context {:corpus   "PROBE"
                                    :cpos     "x"
                                    :matchend "9"})))))))

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
   (testing "while a blank query still counts them all, from a form whose
             radio was changed too"
     (is (some? (:result (search/search-view-data
                          ctx {:query-params {:view "frequencies"
                                              :corpus "PROBE"}}))))
     (is (some? (:result (search/search-view-data
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
   (let [pairs (fn [query-string]
                 (into {}
                       (map (fn [pair]
                              (let [[k v] (str/split pair #"=" 2)]
                                [(keyword k) (or v "")])))
                       (remove str/blank? (str/split query-string #"&"))))
         fetch (fn [query-string]
                 (search/serve-search ctx {:query-params (pairs query-string)
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
