(ns dk.cst.corpus-probe.views.app
  "Every page the app serves, by the route that names it.

  This is the one namespace that knows the whole set: the server renders
  a request through `page` and the client renders its state through the
  same `page`, so a route cannot look one way to a reader who waited for
  the document and another to one whose client swapped it in.

  A frequency table is not another page: it is the same search counted
  rather than listed. So one page holds the query form and one results
  region, and the `:view` of the application state decides which view of
  the hits that region holds.

  The frontpage and the glossary are prose rather than interface: the
  hiccup of a Markdown document (see dk.cst.corpus-probe.docs), rendered
  as it arrives."
  (:require [clojure.walk :as walk]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.corpus :as corpus-views]
            [dk.cst.corpus-probe.views.frequencies :as freq-views]
            [dk.cst.corpus-probe.views.layout :as layout]
            [dk.cst.corpus-probe.views.page :as page]))

(defn result-view
  "The outcome of the search in `state` in its `:view`: the hits counted
  as a frequency table, or listed as a concordance."
  [{:keys [view] :as state}]
  (if (= :frequencies view)
    (freq-views/frequency-section state)
    (page/result-section state)))

(defn search-view
  "The search page's main content from application `state`, in its
  `:ui`: the page heading, the query form (see
  dk.cst.corpus-probe.views.page/search-form, carrying the `:view` as a
  hidden input so that a control applied from one view answers in that
  view), the inspection panel while a token is `:selected`, and the
  results region when the params described a search, or the `:guide`
  (see dk.cst.corpus-probe.views.page/guide) where they did not.

  The form submits to the results fragment, so a search lands the reader
  on its own answer rather than at the top of the form that asked for it.
  The <main> is focusable so the bypass link can move the reader into it."
  [{:keys [ui view result error selected client?] :as state}]
  [:main layout/main-attrs
   [:h1 (i18n/tr ui "Search")]
   ;; the form has to say which view it is being submitted from, or a
   ;; result regrouped from the frequency table comes back as a
   ;; concordance: one page serves both, and only this says which
   (page/search-form state (str url/search url/results-fragment)
                     (when (= :frequencies view)
                       [:input {:type  "hidden" :name "view"
                                :value "frequencies"}]))
   ;; the panel takes the form's column while it is open, so it sits next
   ;; to the hits it describes; it comes before them in the document so
   ;; reading order and visual order agree at every width
   (when client? (page/sidebar ui selected))
   ;; the help stands where the answer will, until there is one: the
   ;; reader who has not searched yet is the one with room to read it
   (if (or result error)
     (result-view state)
     (page/guide (:guide state)))])

(defn mark-target
  "The hiccup `body` with the content of the element whose id is
  `fragment` in a <mark>: the part a link named, which a deep link near
  the foot of a page cannot scroll to the top. `body` as it is without a
  fragment or a match."
  [fragment body]
  (if fragment
    (walk/postwalk (fn [x]
                     (if (and (vector? x) (map? (second x))
                              (= fragment (:id (second x))))
                       [(first x) (second x) (into [:mark] (drop 2 x))]
                       x))
                   body)
    body))

(defn document-view
  "The main content of a page that is a document, the frontpage or the
  glossary: the `:body` of `data`, the hiccup of the document, whose own
  first heading names the page, with the element the `fragment` of the
  location names marked (see `mark-target`)."
  [{:keys [body]} fragment]
  [:main.document layout/main-attrs (mark-target fragment body)])

(defn page
  "The main content of the page `state` describes, by its `:route`; nil
  for a route this app does not render.

  The lookup context every view translates through is derived here from
  the state's `:lang` and handed down as `:ui`, so the state itself
  carries only the language code: it travels to the client as transit,
  and the client already holds every table (see
  dk.cst.corpus-probe.i18n).

  The two corpus pages take that context and their own data, whose
  `:lang` is the corpus's rather than the interface's; a document page
  needs neither."
  [{:keys [route lang] :as state}]
  (let [ui    (i18n/->ui lang)
        state (assoc state :ui ui)]
    (case route
      :document (document-view (:data state) (:fragment state))
      :search  (search-view state)
      :corpora (corpus-views/index-view ui (:data state))
      :corpus  (corpus-views/info-view ui (:data state))
      nil)))
