(ns dk.cst.corpus-probe.views
  "Every page the app serves, by route, its document title by the same
  route, and the chrome every page shares: the bypass link, the masthead
  with its navigation and language switch, and the footer. The server
  renders a request through `page` and titles it through `title`; the
  client renders its state through the same `page`."
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
  `ui`: the first focusable thing in the document."
  [ui]
  ;; WCAG 2.4.1: a way past the blocks every page repeats
  [:a.skip {:href (str "#" widgets/main-id)} (i18n/tr ui "Skip to content")])

(def language-names
  "Each supported language named in itself, as a language switch should
  name it."
  {"da" "Dansk"
   "en" "English"})

(defn language-switch
  "The language switch in `ui`: every supported language named in
  itself, the one in use as plain text and each other as a button that
  stores it, posted to the preferences URL and returning to `path`."
  [ui path]
  ;; a form, not links: the language is a stored preference rather than
  ;; a property of the page, and the same URL serves either one
  [:form.languages {:method     "post"
                    :action     url/preferences
                    :aria-label (i18n/tr ui "Language")}
   [:input {:type "hidden" :name "return" :value path}]
   [:p (interpose
        " · "
        (for [code i18n/languages]
          ;; the one in use is text, not a control that could do nothing
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
  "The site masthead shared by every page, in `ui`: the site name
  linking home, the navigation over `nav` (each `nav-items` key to its
  href, the page at `path` marked current) and the language switch
  returning to `path`."
  [ui path nav]
  [:header.masthead
   ;; a link home, not a heading: HTML has no element for a site's name.
   ;; It keeps no query; the navigation beside it carries a search onward
   [:a.sitename {:href url/home} "corpus-probe"]
   ;; the hrefs come from the handler: the search keeps its query across
   ;; the masthead, and only the handler knows what that query is
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
  is, and where its manual and its source are.

  Belongs directly under <body>: inside <main> it would be a section
  footer rather than the document's contentinfo."
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
  "The search page's main content from application `state`: the query
  form with the corpus chooser over its `:folders`, the inspector while
  a token is `:selected`, and the results region by `:view` once the
  `:params` described a search, else the `:help`."
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
    ;; no h1 of its own: the results region heads the page once there is
    ;; an answer, and the search landmark says what the page is until then
    [:main.search-page (cond-> widgets/main-attrs
                         selected (assoc :class "inspecting"))
     ;; the form has to say which view it is being submitted from, or a
     ;; result regrouped from the frequency table comes back as a
     ;; concordance: one page serves both, and only this says which.
     ;; It submits to the results fragment, so a search lands on its answer
     (search/search-form state (str url/search url/results-fragment)
                         (list
                          (when (= :frequencies view)
                            [:input {:type  "hidden" :name "view"
                                     :value "frequencies"}])
                          (result/subset-inputs (:subset result)))
                         chooser)
     ;; before the hits in the document, so reading order and visual
     ;; order agree; the panel takes the form's column while it is open
     (when client? (concordance/inspector ui selected))
     ;; the help stands where the answer will, until there is one: the
     ;; reader who has not searched yet is the one with room to read it
     (cond
       (not (or result error)) (search/help ui (:help state))
       (= :frequencies view)   (frequency/frequency-section state)
       :else                   (concordance/concordance-section state))]))

(defn document-page
  "The main content of a document page, the frontpage or the glossary:
  the `:body` of `data`, with the element `fragment` names marked."
  [{:keys [body]} fragment]
  [:main.document widgets/main-attrs (hiccup/mark-target fragment body)])

(defn page
  "The main content of the page `state` describes, by its `:route`, the
  `:ui` every view translates through derived from its `:lang`; nil for
  a route this app does not render."
  [{:keys [route lang] :as state}]
  ;; derived here rather than carried in the state, which travels to the
  ;; client as transit; the client already holds every table
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
  blanks skipped) followed by the app name."
  [& parts]
  (str/join " · " (concat (remove str/blank? parts) ["corpus-probe"])))

(defn search-title
  "The document title of the search page for `params` in `ui`: the
  query, how many hits `result` found (when given), the corpora, the
  metadata filter and the page number when past the first; the app name
  alone when nothing was searched for."
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
                   (result/filter-phrase result)
                   (when (pos? page-n)
                     (str (i18n/tr ui "page") " " (inc page-n))))))))

(defn frequency-title
  "The document title of the frequency view for `params` in `ui`: what
  was counted, in which corpora, within the metadata filter of `result`,
  and by what."
  [ui {:keys [corpus attr by] :as params} result]
  (page-title (if (result/asked? params)
                (result/query-phrase ui params)
                (i18n/tr ui "All tokens"))
              (when (seq corpus) (result/corpora-phrase ui corpus))
              (result/filter-phrase result)
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
  "The document title of the page `state` describes, by its `:route` and
  in the language of its `:lang`; the app name alone for a route this
  app does not title."
  [{:keys [route lang view params result data seeded?] :as state}]
  (let [ui (i18n/->ui lang)]
    (case route
      ;; a seeded form has answered nothing, whatever view the settings
      ;; left it in: the frequency view names what it counted, and it
      ;; counted nothing
      :search   (if seeded?
                  (search-title ui nil)
                  (result-title ui view params result))
      :document (page-title (document-title (:body data)))
      :corpora  (page-title (i18n/tr ui "Corpora"))
      :corpus   (page-title (:corpus data))
      :text     (page-title (corpus-views/text-name ui (:structs data))
                            (:corpus data))
      (page-title))))
