(ns dk.cst.corpus-probe.views.layout
  "Hiccup shared by every page: the bypass link, the site masthead with its
  navigation and the language switch, and the site footer."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.i18n :as i18n]))

(def main-id
  "The id of every page's <main>. Named once, so `skip-link` and the
  element it reaches cannot drift apart without the whole app noticing."
  "main")

(def main-attrs
  "The attributes every page's <main> carries: `main-id`, and the tabindex
  that lets the bypass link move focus into it rather than only scrolling
  to it."
  {:id main-id :tabindex "-1"})

(defn skip-link
  "The bypass link past the masthead to the page's own content, in
  language `lang`: the first focusable thing in the document, off screen
  until a keyboard reaches it.

  WCAG 2.4.1 asks for a mechanism past the blocks a page repeats, and the
  masthead is one on every page here."
  [lang]
  [:a.skip {:href (str "#" main-id)} (i18n/tr lang :skip-to-content)])

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

(defn current-path
  "The path of the page being served, read from the language `switch`:
  every entry there is this page's own URL in one language, so the path is
  known without threading the request through the document shell."
  [switch]
  (some-> (first (vals switch)) (str/split #"\?") first))

(defn site-header
  "The site masthead shared by every page, in language `lang`: the name
  linking home, the top-level navigation with the served page marked as
  the current one, and the language switch over `switch` (see
  `language-switch`).

  The site name is a paragraph, not a heading: it is the same string on
  every page, so it names the site rather than the page, and each page's
  own <h1> lives inside its <main>. Every link here carries `lang`, the
  site name included, so following one keeps the language the reader
  chose."
  [lang switch]
  (let [path (current-path switch)]
    [:header
     [:p.sitename [:a {:href (str "/?lang=" lang)} "corpus-probe"]]
     [:p.subtitle (i18n/tr lang :subtitle)]
     [:nav {:aria-label (i18n/tr lang :site)}
      [:ul
       (for [[k p] nav-items]
         [:li [:a (cond-> {:href (str p "?lang=" lang)}
                    (= p path) (assoc :aria-current "page"))
               (i18n/tr lang k)]])]]
     (language-switch lang switch)]))

(defn site-footer
  "The site's contentinfo in language `lang`: what this app is a front end
  for, credited where a reader can follow it.

  One line. Rendered as a direct child of <body>, so it is the document's
  contentinfo rather than a section footer inside the main content."
  [lang]
  [:footer
   [:p (i18n/tr lang :powered-by) " "
    [:a {:href "https://cwb.sourceforge.io/"} "IMS Open Corpus Workbench"]]])
