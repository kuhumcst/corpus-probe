(ns dk.cst.corpus-probe.views.layout-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.views.hiccup :refer [da deep en]]
            [dk.cst.corpus-probe.views.layout :as layout]))

(def nav
  "The site navigation of a search page, as api/nav-hrefs builds it."
  {:search          "/?q=hund#results"
   :corpora-heading "/corpora"})

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
    (testing "the group says what it is about, in the page's own language"
      (is (some #{"Sprog"} (deep html)))
      (is (some #{"Language"} (deep (layout/language-switch en "/")))))))

(deftest skip-link-test
  (testing "the bypass link points at the page's own content"
    (is (= [:a.skip {:href "#main"} "Skip to content"]
           (layout/skip-link en)))
    (is (= "Gå til indhold" (last (layout/skip-link da))))))

(deftest site-footer-test
  (testing "the app says what it is, once, where a reader needs it once"
    (is (some #{"CWB corpus search"} (deep (layout/site-footer en))))
    (is (some #{"CWB-korpussøgning"} (deep (layout/site-footer da)))))
  (testing "and credits what it is a front end for"
    (is (some #{"https://cwb.sourceforge.io/"}
              (deep (layout/site-footer en))))
    (is (some #{"Powered by"} (deep (layout/site-footer en))))
    (is (some #{"Drevet af"} (deep (layout/site-footer da))))))

(deftest site-header-test
  (let [links (fn [path nav]
                (filter #(and (map? %) (:href %))
                        (deep (layout/site-header en path nav))))]
    (testing "the navigation carries the search, the site name does not"
      (is (= ["/" "/?q=hund#results" "/corpora"]
             (map :href (links "/" nav))))
      (testing "so the name is the way back to a clean search"
        (is (= "/" (:href (first (links "/" nav)))))))
    (testing "no link names a language"
      (is (not (some #(.contains ^String (:href %) "lang=") (links "/" nav)))))
    (testing "the nav marks the page being served, and only it"
      (is (= [nil "page" nil] (map :aria-current (links "/" nav))))
      (is (= [nil nil "page"] (map :aria-current (links "/corpora" nav))))
      (testing "a page no nav item names marks nothing"
        (is (= [nil nil nil] (map :aria-current (links "/corpus/viser" nav))))))
    (testing "the masthead is in the page's own language"
      (let [da (deep (layout/site-header da "/" nav))]
        (is (some #{"Søgning"} da))
        (is (some #{"Korpusser"} da))
        (testing "what the app is belongs in the footer, not over every page"
          (is (not (some #{"CWB-korpussøgning"} da))))
        (testing "the frequency table is a view of a result, not a place"
          (is (not (some #{"Frekvenser"} da))))))
    (testing "the masthead claims no heading: each page names itself"
      (is (not (some #{:h1} (deep (layout/site-header en "/" nav))))))
    (testing "the app's own name is not translated"
      (is (some #{"corpus-probe"} (deep (layout/site-header da "/" nav)))))))
