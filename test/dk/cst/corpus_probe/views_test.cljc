(ns dk.cst.corpus-probe.views-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.hiccup :refer [deep]]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.test.hiccup :refer [da en]]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views :as views]
            [dk.cst.corpus-probe.views.widgets :as widgets]))

(def help
  "A search help, as dk.cst.corpus-probe.server.search/serve-search puts
  one in the data."
  [[:p "Type a word."]])

(def base
  "A search page with nothing searched for yet."
  {:ui en :folders [] :params {} :help help})

(def view-hrefs
  "The two views of one result, as url/view-hrefs builds them."
  [[:kwic "/search?q=hund#results"]
   [:frequencies "/search?q=hund&view=frequencies#results"]])

(defn h1s
  "The h1 headings among hiccup `html`."
  [html]
  (filter #(and (vector? %) (= :h1 (first %))) (deep html)))

(deftest search-page-test
  (testing "the bypass link can reach the page, which is classed for the
            wide layout"
    (is (= :main.search-page (first (views/search-page base))))
    (is (= widgets/main-attrs (second (views/search-page base)))))
  (testing "nothing heads the page but what it shows: the answer, and
            nothing until there is one"
    (is (empty? (h1s (views/search-page base))))
    (is (= [[:h1 {:id "results-heading"} "The search did not finish in time"]]
           (h1s (views/search-page (assoc base :error {:type :timeout}))))))
  (testing "the form submits to the results, so a search lands on its answer"
    (is (= "/search#results"
           (get-in (views/search-page base) [2 1 1 :action])))
    (is (= (str url/search url/results-fragment)
           (get-in (views/search-page base) [2 1 1 :action]))))
  (testing "the corpus chooser stands in the form, over the selection"
    (let [html (deep (views/search-page
                      (assoc base
                             :folders [{:label nil :folders []
                                        :corpora [{:id "PROBE" :size 47}
                                                  {:id "VISER" :size 48}]}]
                             :params  {:corpus ["PROBE"]})))]
      (is (some #{:fieldset.chooser.box} html))
      (is (some #(and (map? %) (= "corpus" (:name %)) (= "PROBE" (:value %))
                      (:checked %))
                html))
      (is (some #(and (map? %) (= "corpus" (:name %)) (= "VISER" (:value %))
                      (not (:checked %)))
                html))))
  (testing "no query renders no results region at all, but the help
            where the results will be"
    (is (not (some #{"results"} (deep (views/search-page base)))))
    (is (= :section.help (first (last (views/search-page base)))))
    (is (some #{"Type a word."} (deep (views/search-page base)))))
  (testing "and the help gives way to an answer"
    (is (not (some #{:section.help}
                   (deep (views/search-page
                          (assoc base :error {:type :timeout})))))))
  (testing "a page without a help document simply has none"
    (is (nil? (last (views/search-page (dissoc base :help))))))
  (testing "an error is shown as the outcome of the search"
    (let [html (views/search-page (assoc base :error {:type :cqp
                                                      :message "boom"}))]
      (is (some #{"boom"} (deep html)))
      (is (some #{"results"} (deep html)))))
  (testing "the inspector stands before the answer while a token is
            selected, and only where the client runs, marking the page"
    (let [selected {:token {:word "hund"} :corpus "PROBE"}
          html     (views/search-page (assoc base :client? true
                                             :selected selected))]
      (is (= :aside.inspector (first (nth html 3))))
      (is (= "inspecting" (:class (second html))))
      (is (not (some #{:aside.inspector}
                     (deep (views/search-page (assoc base :selected selected))))))
      (is (not (contains? (second (views/search-page (assoc base :client? true)))
                          :class))))))

(deftest search-page-carries-the-view-test
  (let [views (fn [v] (->> (deep (views/search-page (assoc base :view v)))
                           (filter #(and (map? %) (= "view" (:name %))))))]
    (testing "a form submitted from the frequency view answers in it"
      ;; without this a regrouped result comes back as a concordance
      (is (= [{:type "hidden" :name "view" :value "frequencies"}]
             (views :frequencies))))
    (testing "the concordance is the default, so it names nothing"
      (is (empty? (views :kwic))))))

(deftest result-view-test
  (let [result {:size 1 :page 0 :pages 1 :hits []
                :counts [{:corpus "PROBE" :size 1}]}
        state  (assoc base :result result :view-hrefs view-hrefs)]
    (testing "the concordance is the default view of a result"
      (let [html (views/search-page state)]
        (is (some #{:table.kwic} (deep html)))
        (is (not (some #{:table.frequencies} (deep html))))))
    (testing "the same search counted rather than listed is the other view"
      (let [freq {:attr   :word
                  :query  "hund"
                  :counts [{:corpus "PROBE" :tokens 47 :size 1}]
                  :rows   [{:value "hund" :freqs {"PROBE" 1} :total 1}]}
            html (views/search-page (assoc state
                                           :view   :frequencies
                                           :result freq
                                           :attrs  [{:type :positional
                                                     :name :word}]))]
        (is (some #{:table.frequencies} (deep html)))
        (is (not (some #{:table.kwic} (deep html))))))
    (testing "both views are offered, whichever is being shown"
      (doseq [view [:kwic :frequencies]]
        (let [html (views/search-page (assoc state :view view))]
          (is (some #{"/search?q=hund#results"} (deep html)))
          (is (some #{"/search?q=hund&view=frequencies#results"}
                    (deep html))))))))

(deftest document-page-test
  (let [body [[:h1 {:id "corpus-search"} "Corpus search"] [:p "prose"]
              [:dl [:dt {:id "kwic"} "KWIC"] [:dd "key word " [:em "in"] " context"]]]
        html (views/page {:route :document :lang "en" :data {:body body}})]
    (testing "the document is the page's content, under its own heading"
      (is (= [:main.document widgets/main-attrs body] html)))
    (testing "the page claims no heading of its own: the document names it"
      (is (= 1 (count (filter #{:h1} (deep html))))))
    (testing "the element the location's fragment names is marked"
      (let [marked (views/page {:route :document :lang "en"
                                :data {:body body} :fragment "kwic"})]
        (is (some #{[:dt {:id "kwic"} [:mark "KWIC"]]} (deep marked)))
        (is (= 1 (count (filter #{:mark} (deep marked)))))))
    (testing "and nothing is, without a fragment or with one nothing carries"
      (is (not (some #{:mark} (deep html))))
      (is (not (some #{:mark}
                     (deep (views/page {:route :document :lang "en"
                                        :data {:body body}
                                        :fragment "nonesuch"}))))))))

(deftest page-test
  (testing "every route renders its main, in the language of the state"
    (is (= :main.search-page
           (first (views/page {:route :search :lang "en" :folders [] :params {}}))))
    (is (some #{[:h1 "Korpusser"]}
              (deep (views/page {:route :corpora :lang "da" :data {:folders []}}))))
    (is (some #{[:h1 "VISER"]}
              (deep (views/page {:route :corpus :lang "en"
                                 :data  {:corpus "VISER" :stats {} :info {}}}))))
    (is (= :main.document
           (first (views/page {:route :text :lang "en"
                               :data  {:corpus "PROBE" :structs {} :blocks []
                                       :from 0}})))))
  (testing "a route this app does not render is nothing"
    (is (nil? (views/page {:route :nonesuch :lang "en"})))))

(def nav
  "The site navigation of a search page, as url/nav-hrefs builds it."
  {:search          "/search?q=hund#results"
   :corpora-heading "/corpora"
   :glossary        "/glossary"})

(deftest language-names-test
  (testing "every language the app serves can name itself in the switch"
    (is (= (set i18n/languages) (set (keys views/language-names))))))

(deftest language-switch-test
  (let [html    (views/language-switch da "/?q=hund")
        buttons (filter #(and (map? %) (= "lang" (:name %))) (deep html))]
    (testing "a preference is set, not navigated to: no URL names a language"
      (is (= :form.languages (first html)))
      (is (= "post" (:method (second html))))
      (is (= url/preferences (:action (second html))))
      (is (not (some #(and (string? %) (.contains ^String % "lang="))
                     (deep html)))))
    (testing "the language in use is shown, so the reader can see it"
      (is (some #{[:span {:lang "da" :aria-current "true"} "Dansk"]}
                (deep html))))
    (testing "but it is not a control: choosing it would do nothing"
      (is (= ["en"] (map :value buttons)))
      (is (= ["en"] (map :lang buttons)))
      (testing "and the other way round"
        (let [en (deep (views/language-switch en "/"))]
          (is (some #{[:span {:lang "en" :aria-current "true"} "English"]} en))
          (is (= ["da"] (map :value (filter #(and (map? %) (= "lang" (:name %)))
                                            en)))))))
    (testing "so no language is ever offered as a change to itself"
      (doseq [lang i18n/languages]
        (is (not (some #{lang}
                       (map :value
                            (filter #(and (map? %) (= "lang" (:name %)))
                                    (deep (views/language-switch
                                           (i18n/->ui lang) "/")))))))))
    (testing "every language is named in itself, whichever is in use"
      (is (some #{"Dansk"} (deep html)))
      (is (some #{"English"} (deep html))))
    (testing "it returns the reader to the page they were reading"
      (is (some #{{:type "hidden" :name "return" :value "/?q=hund"}}
                (deep html))))
    (testing "the group says what it is about, in the page's own language,
              to a screen reader alone: the row itself needs no caption"
      (is (= "Sprog" (:aria-label (second html))))
      (is (= "Language" (:aria-label (second (views/language-switch en "/")))))
      (is (not (some #{"Sprog" "Language"} (deep (last html))))))))

(deftest skip-link-test
  (testing "the bypass link points at the page's own content"
    (is (= [:a.skip {:href "#main"} "Skip to content"]
           (views/skip-link en)))
    (is (= "Gå til indhold" (last (views/skip-link da))))))

(deftest site-footer-test
  (testing "the footer credits what the app is a front end for, and is
            classed for the stylesheet"
    (is (= :footer.footer (first (views/site-footer en))))
    (is (some #{"https://cwb.sourceforge.io/"}
              (deep (views/site-footer en))))
    (is (some #{"Powered by"} (deep (views/site-footer en))))
    (is (some #{"Bygget på"} (deep (views/site-footer da)))))
  (let [hrefs (fn [ui] (->> (deep (views/site-footer ui))
                            (filter #(and (map? %) (:href %)))
                            (map :href)))]
    (testing "and says where the manual, the source and the institution are"
      (is (some #{"https://cwb.sourceforge.io/files/CQP_Manual/"} (hrefs en)))
      (is (some #{"https://github.com/kuhumcst/corpus-probe"} (hrefs en)))
      (is (some #{"CQP manual"} (deep (views/site-footer en))))
      (is (some #{"Kildekode"} (deep (views/site-footer da))))
      (testing "the links close the row"
        (is (= :ul.row (first (last (views/site-footer en))))))
      (testing "the institution in the reader's language, where it has one"
        (is (some #{"https://cst.ku.dk/english/"} (hrefs en)))
        (is (some #{"https://cst.ku.dk/"} (hrefs da)))))
    (testing "and whose it is, this year"
      (is (some #{(views/year)} (deep (views/site-footer en))))
      (is (some #{"University of Copenhagen"} (deep (views/site-footer en))))
      (is (some #{"Københavns Universitet"} (deep (views/site-footer da)))))))

(deftest site-header-test
  (let [links (fn [path nav]
                (filter #(and (map? %) (:href %))
                        (deep (views/site-header en path nav))))]
    (testing "the masthead is classed for the stylesheet, its navigation
              chrome rather than text"
      (is (= :header.masthead (first (views/site-header en "/" nav))))
      (is (some #{:nav.menu} (deep (views/site-header en "/" nav)))))
    (testing "the navigation carries the search, the site name does not"
      (is (= ["/" "/search?q=hund#results" "/corpora" "/glossary"]
             (map :href (links "/" nav))))
      (testing "so the name is the way back to the frontpage"
        (is (= "/" (:href (first (links "/search" nav)))))))
    (testing "no link names a language"
      (is (not (some #(.contains ^String (:href %) "lang=") (links "/" nav)))))
    (testing "the nav marks the page being served, and only it"
      (is (= [nil "page" nil nil] (map :aria-current (links "/search" nav))))
      (is (= [nil nil "page" nil] (map :aria-current (links "/corpora" nav))))
      (is (= [nil nil nil "page"] (map :aria-current (links "/glossary" nav))))
      (testing "a page no nav item names marks nothing"
        (is (= [nil nil nil nil] (map :aria-current (links "/" nav))))
        (is (= [nil nil nil nil]
               (map :aria-current (links "/corpora/viser" nav))))))
    (testing "the masthead is in the page's own language"
      (let [da (deep (views/site-header da "/" nav))]
        (is (some #{"Søgning"} da))
        (is (some #{"Korpusser"} da))
        (is (some #{"Ordliste"} da))
        (testing "the frequency table is a view of a result, not a place"
          (is (not (some #{"Frekvenser"} da))))))
    (testing "the masthead claims no heading: each page names itself"
      (is (not (some #{:h1} (deep (views/site-header en "/" nav))))))
    (testing "the app's own name is not translated"
      (is (some #{"corpus-probe"} (deep (views/site-header da "/" nav)))))))

(deftest page-title-test
  (is (= "corpus-probe" (views/page-title)))
  (is (= "VISER · corpus-probe" (views/page-title "VISER")))
  (testing "blank parts are skipped"
    (is (= "corpus-probe" (views/page-title nil "")))))

(deftest search-title-test
  (testing "no query names the page, which the frontpage's title does not"
    (is (= "Search · corpus-probe" (views/search-title en {})))
    (is (= "Søgning · corpus-probe" (views/search-title da {}))))
  (testing "a search names the query and corpus"
    (is (= "hund · PROBE · corpus-probe"
           (views/search-title en {:q "hund" :corpus ["PROBE"]}))))
  (testing "several corpora are counted"
    (is (= "hund · 2 corpora · corpus-probe"
           (views/search-title en {:q "hund" :corpus ["PROBE" "VISER"]})))
    (is (= "hund · 2 korpusser · corpus-probe"
           (views/search-title da {:q "hund" :corpus ["PROBE" "VISER"]}))))
  (testing "no corpora are not counted"
    (is (= "hund · corpus-probe"
           (views/search-title en {:q "hund" :corpus []}))))
  (testing "the outcome rides in the title, which is all a reload announces"
    (is (= "hund · 6 hits · PROBE · corpus-probe"
           (views/search-title en {:q "hund" :corpus ["PROBE"]}
                               {:size 6 :page 0
                                :counts [{:corpus "PROBE" :size 6}]})))
    (testing "with the page number once past the first"
      (is (= "hund · 6 hits · PROBE · page 3 · corpus-probe"
             (views/search-title en {:q "hund" :corpus ["PROBE"]}
                                 {:size 6 :page 2
                                  :counts [{:corpus "PROBE" :size 6}]}))))
    (testing "a search no corpus answered reports no count"
      (is (= "hund · PROBE · corpus-probe"
             (views/search-title en {:q "hund" :corpus ["PROBE"]}
                                 {:size 0 :page 0
                                  :counts [{:corpus "PROBE"
                                            :error {:type :timeout}}]})))))
  (testing "the metadata filter the result was kept within is named"
    (is (= "hund · 6 hits · PROBE · text_year 1591 · corpus-probe"
           (views/search-title en {:q "hund" :corpus ["PROBE"]
                                   :f.text_year ["1591"]}
                               {:size   6 :page 0
                                :counts [{:corpus "PROBE" :size 6}]
                                :filter {:text_year #{"1591"}}}))))
  (testing "a sample says so beside the count it drew"
    (let [title (fn [size]
                  (views/search-title en {:q "hund" :corpus ["PROBE"]}
                                      {:size   size :page 0 :sample 100
                                       :counts [{:corpus "PROBE" :size size}]}))]
      (is (= (str "hund · 6 hits · a random sample of at most 100"
                  " · PROBE · corpus-probe")
             (title 6)))
      (testing "and a search that found nothing drew nothing, so it does not"
        (is (= "hund · 0 hits · PROBE · corpus-probe" (title 0))))))
  (testing "a list is titled by its length, a title being one line"
    (is (= "2 words · corpus-probe" (views/search-title en {:q "hund\nkat\n"}))))
  (testing "an extended search names the CQP its rows compiled to"
    (let [params {:mode "extended" :t1.attr "lemma" :t1.v "hund"
                  :corpus ["PROBE"]}]
      (is (= "[lemma = \"hund\"] · 5 hits · PROBE · corpus-probe"
             (views/search-title en params {:size   5
                                            :page   0
                                            :counts [{:corpus "PROBE"
                                                      :size   5}]})))
      (is (= "Search · corpus-probe"
             (views/search-title en (dissoc params :t1.attr :t1.v) nil))))))

(deftest result-title-test
  (let [params {:q "hund" :corpus ["PROBE"] :attr "lemma"}
        result {:size 6 :page 0 :counts [{:corpus "PROBE" :size 6}]}]
    (testing "the concordance names its hit count"
      (is (= "hund · 6 hits · PROBE · corpus-probe"
             (views/result-title en :kwic params result))))
    (testing "a frequency table counts values, so it names what it grouped"
      (is (= "hund · PROBE · by lemma · Frequencies · corpus-probe"
             (views/result-title en :frequencies params result)))
      (is (= "[lemma = \"hund\"] · PROBE · by word · Frequencies · corpus-probe"
             (views/frequency-title en {:mode "extended" :t1.attr "lemma"
                                        :t1.v "hund" :corpus ["PROBE"]
                                        :attr "word"}
                                    nil))))
    (testing "a whole-corpus table says so rather than naming a query"
      (is (= "All tokens · PROBE · by lemma · Frequencies · corpus-probe"
             (views/result-title en :frequencies (assoc params :q "")
                                 result))))
    (testing "a form seeded from stored settings has counted nothing, so
              it is the search page whatever view it was left in"
      (is (= "Search · corpus-probe"
             (views/title {:route :search :lang "en" :seeded? true
                           :view  :frequencies
                           :params (dissoc params :q)})))
      (is (= "All tokens · PROBE · by lemma · Frequencies · corpus-probe"
             (views/title {:route :search :lang "en"
                           :view  :frequencies
                           :params (dissoc params :q) :result result}))))))

(deftest document-title-test
  (is (= "Query help"
         (views/document-title [[:p "x"] [:h1 {:id "a"} "Query help"]])))
  (is (= "The cpos column"
         (views/document-title
          [[:h2 {:id "a"} "The " [:code "cpos"] " column"]])))
  (is (nil? (views/document-title [[:p "x"]]))))

(deftest title-test
  (testing "every route titles itself, in the language of the state"
    (is (= "hund · PROBE · corpus-probe"
           (views/title {:route :search :lang "en" :view :kwic
                         :params {:q "hund" :corpus ["PROBE"]}})))
    (is (= "Corpus search · corpus-probe"
           (views/title {:route :document :lang "en"
                         :data  {:body [[:h1 {:id "x"} "Corpus search"] [:p "p"]]}})))
    (is (= "Korpusser · corpus-probe"
           (views/title {:route :corpora :lang "da" :data {:folders []}})))
    (is (= "VISER · corpus-probe"
           (views/title {:route :corpus :lang "en" :data {:corpus "VISER"}})))
    (is (= "Hverdag · PROBE · corpus-probe"
           (views/title {:route :text :lang "en"
                         :data  {:corpus "PROBE"
                                 :structs {:text_title "Hverdag"}}})))
    (is (= "Tekst · PROBE · corpus-probe"
           (views/title {:route :text :lang "da" :data {:corpus "PROBE"}}))))
  (testing "a route this app does not title is the app name alone"
    (is (= "corpus-probe" (views/title {:route :nonesuch :lang "en"})))))
