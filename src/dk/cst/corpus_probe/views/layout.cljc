(ns dk.cst.corpus-probe.views.layout
  "Hiccup shared by every page: the bypass link, the site masthead with its
  navigation and the language switch, and the site footer.

  No URL here names a language. Which language a reader wants is their own
  preference, remembered for them, so the same URL serves either one and
  a link can be shared without imposing the sharer's language on whoever
  opens it."
  (:require [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.url :as url]))

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
  `ui`: the first focusable thing in the document, off screen
  until a keyboard reaches it.

  WCAG 2.4.1 asks for a mechanism past the blocks a page repeats, and the
  masthead is one on every page here."
  [ui]
  [:a.skip {:href (str "#" main-id)} (i18n/tr ui "Skip to content")])

(def language-names
  "Each supported language named in itself, as a language switch should
  name it."
  {"da" "Dansk"
   "en" "English"})

(def preferences-path
  "Where a setting is stored: a preference is not a place, so choosing one
  changes state and sends the reader back where they were."
  "/preferences")

(defn language-switch
  "The language switch: every supported language named in itself, the one
  in use as plain text and each other as a button that stores it,
  submitting to `preferences-path` and returning to `path`.

  The language in use is shown but is not a control, because choosing it
  would do nothing and a control that can do nothing is one a reader has
  to reason about; the others are controls, because choosing them does
  something. So the switch says both what the page is in and what it could
  be in. The one in use is marked current, which the stylesheet shows in
  bold, as it shows the current page in the navigation beside it.

  No visible name: a row of language names at the end of the masthead is
  a language switch, and saying so would say what the row already says.
  The form carries the name instead, so it is a named landmark for a
  reader who cannot see the row.

  A form rather than links, because the language a reader wants is their
  preference rather than a property of the page they are on: the same URL
  serves either language, and the choice is remembered for them. Each name
  carries its own language, since none of them is in the language of the
  page around it."
  [ui path]
  [:form.languages {:method     "post"
                    :action     preferences-path
                    :aria-label (i18n/tr ui "Language")}
   [:input {:type "hidden" :name "return" :value path}]
   [:p (interpose
        " · "
        (for [code i18n/languages]
          (if (= code (:lang ui))
            [:span {:lang code :aria-current "true"} (language-names code)]
            [:button {:type  "submit"
                      :name  "lang"
                      :value code
                      :lang  code}
             (language-names code)])))]])

(def nav-items
  "The top-level navigation, in display order: the key naming each page
  (which is also the key its URL arrives under) and its path."
  ;; TODO: the CQP guide is linked from the search help and the glossary
  ;; only, and a reader who has searched has neither in view. Does it
  ;; belong here, as a fourth entry?
  [[:search url/search]
   [:corpora-heading url/corpora]
   [:glossary url/glossary]])

(defn nav-label
  "What the `nav-items` entry `k` is called, in `ui`."
  [ui k]
  (case k
    :search          (i18n/tr ui "Search")
    :corpora-heading (i18n/tr ui "Corpora")
    :glossary        (i18n/tr ui "Glossary")
    (name k)))

(defn term
  "The jargon `k` as the interface shows it, in `ui`: CWB's own word, as
  an <abbr> with its expansion where it is one, linked to its glossary
  entry (the key is the entry's id) unless `linked?` is false: inside
  another link, in a label whose click belongs to its control, or in
  the machinery of a form or a result (a legend, a table's head, a
  caption), which the glossary is linked from the prose instead of. Not
  only abbreviations: match and frequency are terms too."
  ([ui k]
   (term ui k true))
  ([ui k linked?]
   (let [[label expansion]
         (case k
           :kwic                  ["KWIC" (i18n/tr ui "key word in context")]
           :cqp                   ["CQP" "Corpus Query Processor"]
           :cpos                  ["cpos" (i18n/tr ui "corpus position")]
           :match                 [(i18n/tr ui "match")]
           :frequency             [(i18n/tr ui "frequency")]
           :metadata              [(i18n/tr ui "Metadata")]
           :positional-attributes [(i18n/tr ui "Positional attributes")]
           :structural-attributes [(i18n/tr ui "Structural attributes")]
           :alignment-attributes  [(i18n/tr ui "Alignment attributes")]
           :per-million           [(i18n/tr ui "per million")])
         shown (if expansion [:abbr {:title expansion} label] label)]
     (if linked?
       [:a {:href (url/glossary-entry (name k))} shown]
       shown))))

(defn site-header
  "The site masthead shared by every page, in `ui`: three
  things with one role each. Who this is (the name, linking to the
  frontpage), where a reader can go (the top-level navigation over `nav`,
  each `nav-items` key to its URL, with `path`, the page being served,
  marked as the current one), and how they want it (the language switch,
  which returns to `path`).

  What the app is, the masthead does not say: the frontpage does, and a
  reader of a tool needs it once.

  The nav's hrefs are given rather than built here, because the search
  keeps its query across the masthead and only the handler knows what that
  query is.

  The site name is a paragraph, not a heading: it is the same string on
  every page, so it names the site rather than the page, and each page's
  own <h1> lives inside its <main>. Every link here carries the
  site name included, so following one keeps the language the reader
  chose."
  [ui path nav]
  [:header
   ;; the site's name is a link home and nothing else: HTML has no element
   ;; for the name of a site, and what this actually is, is the way back to
   ;; the frontpage. It keeps no query: the navigation beside it is what
   ;; carries a search onward
   [:a.sitename {:href url/home} "corpus-probe"]
   [:nav {:aria-label (i18n/tr ui "Site")}
    [:ul.row
     (for [[k p] nav-items]
       [:li [:a (cond-> {:href (get nav k p)}
                  (= p path) (assoc :aria-current "page"))
             (nav-label ui k)]])]]
   (language-switch ui path)])

(defn year
  "The current year, as the copyright line states it: the server's clock,
  or the browser's."
  []
  #?(:clj  (.getValue (java.time.Year/now))
     :cljs (.getFullYear (js/Date.))))

(defn site-footer
  "The site's contentinfo in `ui`: what this is a front end for, whose it
  is, and where its manual and its source are, in the order the row
  shows them.

  All of it belongs here rather than in the masthead, where a credit and
  a row of links would compete with the site's name and its navigation on
  every page: a reader of a tool needs these once, and looks for them at
  the foot. What the app is, it does not say: the frontpage does.

  Rendered as a direct child of <body>, so it is the document's
  contentinfo rather than a section footer inside the main content."
  [ui]
  [:footer
   [:p (i18n/tr ui "Powered by") " "
    [:a {:href "https://cwb.sourceforge.io/"} "IMS Open Corpus Workbench"]
    "."]
   ;; TODO: no licence is named anywhere yet, and the holder is a guess:
   ;; the university whose organisation publishes the source
   [:p [:small "© " (year) " "
        ;; the institution's own site is Danish, with an English edition
        [:a {:href (if (= "da" (:lang ui))
                     "https://cst.ku.dk/"
                     "https://cst.ku.dk/english/")}
         (i18n/tr ui "Centre for Language Technology")]
        ", " (i18n/tr ui "University of Copenhagen")]]
   [:ul.row
    [:li [:a {:href "https://cwb.sourceforge.io/files/CQP_Manual/"}
          (i18n/tr ui "CQP manual")]]
    [:li [:a {:href "https://github.com/kuhumcst/corpus-probe"}
          (i18n/tr ui "Source code")]]]])
