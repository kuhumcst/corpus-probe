(ns dk.cst.corpus-probe.views.layout
  "Hiccup shared by every page: the site masthead with its navigation and
  the language switch."
  (:require [dk.cst.corpus-probe.i18n :as i18n]))

(def language-names
  "Each supported language named in itself, as a language switch should
  name it."
  {"da" "Dansk"
   "en" "English"})

(defn language-switch
  "The language switch: the current page in each supported language, from
  `switch` (a map of language code to that URL), the page's own `lang`
  marked as current.

  Each link is labelled in the language it leads to and carries that
  language, since its text is not in the language of the page around it."
  [lang switch]
  [:nav.languages {:aria-label (i18n/tr lang :language)}
   [:ul
    (for [code i18n/languages]
      [:li [:a (cond-> {:href (switch code) :lang code :hreflang code}
                 (= code lang) (assoc :aria-current "page"))
            (language-names code)]])]])

(def nav-items
  "The top-level navigation, in display order: the dictionary key naming
  each page and its path."
  [[:search "/"]
   [:frequencies "/frequencies"]
   [:corpora-heading "/corpora"]])

(defn site-header
  "The site masthead shared by every page, in language `lang`: the name
  linking home, the top-level navigation, and the language switch over
  `switch` (see `language-switch`).

  Every link here carries `lang`, the site name included, so following one
  keeps the language the reader chose."
  [lang switch]
  [:header
   [:h1 [:a {:href (str "/?lang=" lang)} "corpus-probe"]]
   [:p.subtitle (i18n/tr lang :subtitle)]
   [:nav {:aria-label (i18n/tr lang :site)}
    [:ul
     (for [[k path] nav-items]
       [:li [:a {:href (str path "?lang=" lang)} (i18n/tr lang k)]])]]
   (language-switch lang switch)])
