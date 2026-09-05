(ns dk.cst.corpus-probe.views.layout-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.views.hiccup :refer [da deep en]]
            [dk.cst.corpus-probe.views.layout :as layout]))

(def nav
  "The site navigation of a search page, as api/nav-hrefs builds it."
  {:search          "/search?q=hund#results"
   :corpora-heading "/corpora"
   :glossary        "/glossary"})

(deftest language-names-test
  (testing "every language the app serves can name itself in the switch"
    (is (= (set i18n/languages) (set (keys layout/language-names))))))

(deftest language-switch-test
  (let [html    (layout/language-switch da "/?q=hund")
        buttons (filter #(and (map? %) (= "lang" (:name %))) (deep html))]
    (testing "a preference is set, not navigated to: no URL names a language"
      (is (= :form.languages (first html)))
      (is (= "post" (:method (second html))))
      (is (= layout/preferences-path (:action (second html))))
      (is (not (some #(and (string? %) (.contains ^String % "lang="))
                     (deep html)))))
    (testing "the language in use is shown, so the reader can see it"
      (is (some #{[:span {:lang "da" :aria-current "true"} "Dansk"]}
                (deep html))))
    (testing "but it is not a control: choosing it would do nothing"
      (is (= ["en"] (map :value buttons)))
      (is (= ["en"] (map :lang buttons)))
      (testing "and the other way round"
        (let [en (deep (layout/language-switch en "/"))]
          (is (some #{[:span {:lang "en" :aria-current "true"} "English"]} en))
          (is (= ["da"] (map :value (filter #(and (map? %) (= "lang" (:name %)))
                                            en)))))))
    (testing "so no language is ever offered as a change to itself"
      (doseq [lang i18n/languages]
        (is (not (some #{lang}
                       (map :value
                            (filter #(and (map? %) (= "lang" (:name %)))
                                    (deep (layout/language-switch
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
      (is (= "Language" (:aria-label (second (layout/language-switch en "/")))))
      (is (not (some #{"Sprog" "Language"} (deep (last html))))))))

(deftest skip-link-test
  (testing "the bypass link points at the page's own content"
    (is (= [:a.skip {:href "#main"} "Skip to content"]
           (layout/skip-link en)))
    (is (= "Gå til indhold" (last (layout/skip-link da))))))

(deftest term-test
  (testing "an abbreviation carries its expansion and links to its entry"
    (is (= [:a {:href "/glossary#kwic"}
            [:abbr {:title "key word in context"} "KWIC"]]
           (layout/term en :kwic)))
    (is (= [:a {:href "/glossary#cpos"} [:abbr {:title "korpusposition"} "cpos"]]
           (layout/term da :cpos))))
  (testing "a word that is no abbreviation is the word, linked"
    (is (= [:a {:href "/glossary#metadata"} "Metadata"] (layout/term en :metadata)))
    (is (= [:a {:href "/glossary#per-million"} "pr. million"]
           (layout/term da :per-million))))
  (testing "where a link cannot go, the term stands unlinked"
    (is (= [:abbr {:title "Corpus Query Processor"} "CQP"]
           (layout/term en :cqp false)))
    (is (= "Metadata" (layout/term en :metadata false)))))

(deftest site-footer-test
  (testing "the app says what it is, once, where a reader needs it once"
    (is (some #{"CWB corpus search"} (deep (layout/site-footer en))))
    (is (some #{"CWB-korpussøgning"} (deep (layout/site-footer da)))))
  (testing "and credits what it is a front end for"
    (is (some #{"https://cwb.sourceforge.io/"}
              (deep (layout/site-footer en))))
    (is (some #{"Powered by"} (deep (layout/site-footer en))))
    (is (some #{"Drevet af"} (deep (layout/site-footer da)))))
  (let [hrefs (fn [ui] (->> (deep (layout/site-footer ui))
                            (filter #(and (map? %) (:href %)))
                            (map :href)))]
    (testing "and says where the manual, the source and the institution are"
      (is (some #{"https://cwb.sourceforge.io/files/CQP_Manual/"} (hrefs en)))
      (is (some #{"https://github.com/kuhumcst/corpus-probe"} (hrefs en)))
      (is (some #{"CQP manual"} (deep (layout/site-footer en))))
      (is (some #{"Kildekode"} (deep (layout/site-footer da))))
      (testing "the institution in the reader's language, where it has one"
        (is (some #{"https://cst.ku.dk/english/"} (hrefs en)))
        (is (some #{"https://cst.ku.dk/"} (hrefs da)))))
    (testing "and whose it is, this year"
      (is (some #{(layout/year)} (deep (layout/site-footer en))))
      (is (some #{"University of Copenhagen"} (deep (layout/site-footer en))))
      (is (some #{"Københavns Universitet"} (deep (layout/site-footer da)))))))

(deftest site-header-test
  (let [links (fn [path nav]
                (filter #(and (map? %) (:href %))
                        (deep (layout/site-header en path nav))))]
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
      (let [da (deep (layout/site-header da "/" nav))]
        (is (some #{"Søgning"} da))
        (is (some #{"Korpusser"} da))
        (is (some #{"Ordliste"} da))
        (testing "what the app is belongs in the footer, not over every page"
          (is (not (some #{"CWB-korpussøgning"} da))))
        (testing "the frequency table is a view of a result, not a place"
          (is (not (some #{"Frekvenser"} da))))))
    (testing "the masthead claims no heading: each page names itself"
      (is (not (some #{:h1} (deep (layout/site-header en "/" nav))))))
    (testing "the app's own name is not translated"
      (is (some #{"corpus-probe"} (deep (layout/site-header da "/" nav)))))))
