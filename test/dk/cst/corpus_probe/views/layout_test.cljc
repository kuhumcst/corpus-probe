(ns dk.cst.corpus-probe.views.layout-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.views.hiccup :refer [deep]]
            [dk.cst.corpus-probe.views.layout :as layout]))

(def switch
  "The language switch of a search page, as api/language-hrefs builds it."
  {"da" "/?q=hund&lang=da"
   "en" "/?q=hund&lang=en"})

(deftest language-names-test
  (testing "every language the app serves can name itself in the switch"
    (is (= (set i18n/languages) (set (keys layout/language-names))))))

(deftest language-switch-test
  (let [links (fn [lang]
                (filter #(and (map? %) (:hreflang %))
                        (deep (layout/language-switch lang switch))))]
    (testing "one link per language, to this page in that language"
      (is (= ["/?q=hund&lang=da" "/?q=hund&lang=en"]
             (map :href (links "da")))))
    (testing "each link is labelled and marked in the language it leads to"
      (is (= ["da" "en"] (map :lang (links "da"))))
      (is (= ["da" "en"] (map :hreflang (links "da"))))
      (is (some #{"Dansk"} (deep (layout/language-switch "da" switch))))
      (is (some #{"English"} (deep (layout/language-switch "da" switch)))))
    (testing "exactly the current language is marked as the current page"
      (is (= ["page" nil] (map :aria-current (links "da"))))
      (is (= [nil "page"] (map :aria-current (links "en")))))
    (testing "the switch itself is labelled in the page's own language"
      (is (= "Sprog"
             (:aria-label (second (layout/language-switch "da" switch)))))
      (is (= "Language"
             (:aria-label (second (layout/language-switch "en" switch))))))))

(deftest site-header-test
  (let [hrefs (fn [lang]
                (keep #(when (and (map? %) (:href %)) (:href %))
                      (deep (layout/site-header lang switch))))]
    (testing "every link keeps the language, the site name included"
      (is (= ["/?lang=en" "/?lang=en" "/frequencies?lang=en"
              "/corpora?lang=en" "/?q=hund&lang=da" "/?q=hund&lang=en"]
             (hrefs "en"))))
    (testing "the navigation is in the page's own language"
      (let [da (deep (layout/site-header "da" switch))]
        (is (some #{"Søgning"} da))
        (is (some #{"Frekvenser"} da))
        (is (some #{"Korpusser"} da))
        (is (some #{"CWB-korpussøgning"} da))))
    (testing "the app's own name is not translated"
      (is (some #{"corpus-probe"} (deep (layout/site-header "da" switch)))))))
