(ns dk.cst.corpus-probe.views
  "Every page the app serves, by the route that names it, its document
  title by the same route, and the chrome every page shares: the bypass
  link, the site masthead with its navigation and the language switch,
  and the site footer. The pages are composed of the generic widgets
  (dk.cst.corpus-probe.views.widgets), the chooser over a tree of
  checkboxes (dk.cst.corpus-probe.views.chooser), the search form
  (dk.cst.corpus-probe.views.search, with its token rows and its
  metadata filter under it), the results region
  (dk.cst.corpus-probe.views.result), the concordance and its inspector
  (dk.cst.corpus-probe.views.concordance), the frequency tables
  (dk.cst.corpus-probe.views.frequency) and the corpus pages
  (dk.cst.corpus-probe.views.corpus).

  This is the one namespace that knows the whole set: the server renders
  a request through `page` and titles it through `title`, and the client
  renders its state through the same `page`, so a route cannot look one
  way to a reader who waited for the document and another to one whose
  client swapped it in.

  A frequency table is not another page: it is the same search counted
  rather than listed. So one page holds the query form and one results
  region, and the `:view` of the application state decides which view of
  the hits that region holds.

  The frontpage and the glossary are prose rather than interface: the
  hiccup of a Markdown document (see dk.cst.corpus-probe.docs), rendered
  as it arrives.

  No URL here names a language. Which language a reader wants is their own
  preference, remembered for them, so the same URL serves either one and
  a link can be shared without imposing the sharer's language on whoever
  opens it."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.hiccup :as hiccup]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.concordance :as concordance]
            [dk.cst.corpus-probe.views.corpus :as corpus-views]
            [dk.cst.corpus-probe.views.frequency :as frequency]
            [dk.cst.corpus-probe.views.result :as result]
            [dk.cst.corpus-probe.views.search :as search]
            [dk.cst.corpus-probe.views.widgets :as widgets]))

(defn skip-link
  "The bypass link past the masthead to the page's own content, in
  `ui`: the first focusable thing in the document, off screen
  until a keyboard reaches it.

  WCAG 2.4.1 asks for a mechanism past the blocks a page repeats, and the
  masthead is one on every page here."
  [ui]
  [:a.skip {:href (str "#" widgets/main-id)} (i18n/tr ui "Skip to content")])

(def language-names
  "Each supported language named in itself, as a language switch should
  name it."
  {"da" "Dansk"
   "en" "English"})

(defn language-switch
  "The language switch: every supported language named in itself, the one
  in use as plain text and each other as a button that stores it,
  submitting to dk.cst.corpus-probe.url/preferences and returning to
  `path`.

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
                    :action     url/preferences
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
  [:header.masthead
   ;; the site's name is a link home and nothing else: HTML has no element
   ;; for the name of a site, and what this actually is, is the way back to
   ;; the frontpage. It keeps no query: the navigation beside it is what
   ;; carries a search onward
   [:a.sitename {:href url/home} "corpus-probe"]
   [:nav.menu {:aria-label (i18n/tr ui "Site")}
    (widgets/link-row (for [[k p] nav-items] [p (get nav k p) (nav-label ui k)])
                      path)]
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
  [:footer.footer
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
   (widgets/link-row
    [[:manual "https://cwb.sourceforge.io/files/CQP_Manual/"
      (i18n/tr ui "CQP manual")]
     [:source "https://github.com/kuhumcst/corpus-probe"
      (i18n/tr ui "Source code")]]
    nil)])

(defn search-page
  "The search page's main content from application `state`, in its
  `:ui`: the query form (see dk.cst.corpus-probe.views.search/search-form,
  carrying the `:view` as a hidden input so that a control applied from
  one view answers in that view, and the corpus chooser over its
  `:folders`, see dk.cst.corpus-probe.views.corpus/corpus-chooser, showing
  what is chosen or everything there is to choose by what `:lists` holds
  of it), the inspection panel while a token is `:selected`, and the
  results region when the params described a search, the hits counted as
  a frequency table or listed as a concordance by its `:view`, or the
  `:help` (see dk.cst.corpus-probe.views.search/help) where they did not.

  No heading of its own: a search landmark with a search button says
  what it is, and a heading saying so again was one more thing between
  the reader and the field. The results region heads the page once there
  is an answer; until then the help stands there, and nothing heads the
  page.

  The form submits to the results fragment, so a search lands the reader
  on its own answer rather than at the top of the form that asked for it.
  The <main> is focusable so the bypass link can move the reader into it,
  and classed for the wide layout, which marks it while the inspector is
  open, since the panel takes the rail's column."
  [{:keys [ui view folders params lists result error selected client?]
    :as state}]
  (let [{:keys [corpora]} lists
        corpus  (set (:corpus params))
        ;; the list's state is named for the chooser's options, and what
        ;; it holds beyond them the chooser ignores
        chooser (corpus-views/corpus-chooser
                 ui folders (assoc corpora
                                   :selected corpus
                                   :held     (into corpus (:unticked corpora))
                                   :client?  client?))]
    [:main.search-page (cond-> widgets/main-attrs
                         selected (assoc :class "inspecting"))
     ;; the form has to say which view it is being submitted from, or a
     ;; result regrouped from the frequency table comes back as a
     ;; concordance: one page serves both, and only this says which
     (search/search-form state (str url/search url/results-fragment)
                         (when (= :frequencies view)
                           [:input {:type  "hidden" :name "view"
                                    :value "frequencies"}])
                         chooser)
     ;; the panel takes the form's column while it is open, so it sits next
     ;; to the hits it describes; it comes before them in the document so
     ;; reading order and visual order agree at every width
     (when client? (concordance/inspector ui selected))
     ;; the help stands where the answer will, until there is one: the
     ;; reader who has not searched yet is the one with room to read it
     (cond
       (not (or result error)) (search/help ui (:help state))
       (= :frequencies view)   (frequency/frequency-section state)
       :else                   (concordance/concordance-section state))]))

(defn document-page
  "The main content of a page that is a document, the frontpage or the
  glossary: the `:body` of `data`, the hiccup of the document, whose own
  first heading names the page, with the element the `fragment` of the
  location names marked (see dk.cst.corpus-probe.hiccup/mark-target)."
  [{:keys [body]} fragment]
  [:main.document widgets/main-attrs (hiccup/mark-target fragment body)])

(defn page
  "The main content of the page `state` describes, by its `:route`; nil
  for a route this app does not render.

  The lookup context every view translates through is derived here from
  the state's `:lang` and handed down as `:ui`, so the state itself
  carries only the language code: it travels to the client as transit,
  and the client already holds every table (see
  dk.cst.corpus-probe.i18n).

  The two corpus pages and the reading page take that context and their
  own data, whose `:lang` is the corpus's rather than the interface's;
  a document page needs neither."
  [{:keys [route lang] :as state}]
  (let [ui    (i18n/->ui lang)
        state (assoc state :ui ui)]
    (case route
      :document (document-page (:data state) (:fragment state))
      :search   (search-page state)
      :corpora  (corpus-views/index-page ui (:data state))
      :corpus   (corpus-views/info-page ui (:data state))
      :text     (corpus-views/reading-page ui (:data state))
      nil)))

(defn page-title
  "The document title: the page-specific `parts` (most specific first,
  blanks skipped) followed by the app name, so tabs and bookmarks are
  meaningful."
  [& parts]
  (str/join " · " (concat (remove str/blank? parts) ["corpus-probe"])))

(defn search-title
  "The document title of the search page for `params` in `ui`:
  the query, how many hits it found (from `result`, when given), the
  selected corpora (`:corpus`, a vector of names, when any), the metadata
  filter the result was kept within and the page number when past the
  first; just the app name when nothing was searched for.

  The hit count is in the title because a full page reload announces the
  title and nothing else, so the title is where the outcome of a search
  first reaches a screen reader. It is left out when no corpus could be
  searched, since then there is no count to report."
  ([ui params]
   (search-title ui params nil))
  ([ui {:keys [corpus] :as params} result]
   (if-not (result/asked? params)
     (page-title (i18n/tr ui "Search"))
     (let [page-n (:page result 0)
           ;; a search every corpus refused still has a result, of size
           ;; 0; titling that "0 hits" reports an answer the search never
           ;; got, and contradicts the results heading
           hits   (when (result/searched? result)
                    (result/hits-phrase ui (:size result)))]
       (page-title (result/query-phrase ui params)
                   hits
                   ;; only ever beside a count it could have drawn from:
                   ;; a search that found nothing sampled nothing
                   (when (and hits (pos? (:size result 0)))
                     (result/sample-phrase ui (:sample result) corpus))
                   (when (seq corpus) (result/corpora-phrase ui corpus))
                   (result/filter-phrase (:filter result) (:patterns result))
                   (when (pos? page-n)
                     (str (i18n/tr ui "page") " " (inc page-n))))))))

(defn frequency-title
  "The document title of the frequency view for `params` in `ui`: what
  was counted, in which corpora, within the metadata filter of `result`,
  and by what.

  A frequency result counts values rather than hits, so it cannot borrow
  the concordance's title: there is no hit count to report."
  [ui {:keys [corpus attr by] :as params} result]
  (page-title (if (result/asked? params)
                (result/query-phrase ui params)
                (i18n/tr ui "All tokens"))
              (when (seq corpus) (result/corpora-phrase ui corpus))
              (result/filter-phrase (:filter result) (:patterns result))
              (str (i18n/tr ui "by") " " attr
                   (when-not (str/blank? by)
                     (str " " (i18n/tr ui "and") " " by)))
              (i18n/tr ui "Frequencies")))

(defn result-title
  "The document title of the search described by `params` in `ui`, shown
  in `view` with `result`: each view names what it shows, since a full
  page load announces the title and nothing else."
  [ui view params result]
  (if (= :frequencies view)
    (frequency-title ui params result)
    (search-title ui params result)))

(defn document-title
  "The text of the first heading among the hiccup `blocks` of a
  document, which is what it calls itself; nil without one."
  [blocks]
  (some #(when (hiccup/heading? %) (hiccup/heading-text %)) blocks))

(defn title
  "The document title of the page `state` describes, by its `:route`, in
  the language of its `:lang`: the search page by its `:view`, `:params`
  and `:result` (see `result-title`), a document by its own first
  heading, the corpus index by name, a corpus page by its corpus and the
  reading page by its text and corpus, from its `:data`; the app name
  alone for a route this app does not title."
  [{:keys [route lang view params result data] :as state}]
  (let [ui (i18n/->ui lang)]
    (case route
      :search   (result-title ui view params result)
      :document (page-title (document-title (:body data)))
      :corpora  (page-title (i18n/tr ui "Corpora"))
      :corpus   (page-title (:corpus data))
      :text     (page-title (corpus-views/text-name ui (:structs data))
                            (:corpus data))
      (page-title))))
