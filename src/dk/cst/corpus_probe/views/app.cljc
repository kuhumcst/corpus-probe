(ns dk.cst.corpus-probe.views.app
  "Every page the app serves, by the route that names it.

  This is the one namespace that knows the whole set: the server renders
  a request through `page` and the client renders its state through the
  same `page`, so a route cannot look one way to a reader who waited for
  the document and another to one whose client swapped it in.

  A frequency table is not another page: it is the same search counted
  rather than listed. So one page holds the query form and one results
  region, and the `:view` of the application state decides which view of
  the hits that region holds."
  (:require [dk.cst.corpus-probe.i18n :as i18n]
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
  `:lang`: the page heading, the query form (see
  dk.cst.corpus-probe.views.page/search-form, carrying the `:view` as a
  hidden input so that a control applied from one view answers in that
  view), the inspection panel while a token is `:selected`, and the
  results region when the params described a search.

  The form submits to the results fragment, so a search lands the reader
  on its own answer rather than at the top of the form that asked for it.
  The <main> is focusable so the bypass link can move the reader into it."
  [{:keys [lang view result error selected client?] :as state}]
  [:main layout/main-attrs
   [:h1 (i18n/tr lang :search)]
   ;; the form has to say which view it is being submitted from, or a
   ;; result regrouped from the frequency table comes back as a
   ;; concordance: one page serves both, and only this says which
   (page/search-form state (str "/" page/results-fragment)
                     (when (= :frequencies view)
                       [:input {:type  "hidden" :name "view"
                                :value "frequencies"}]))
   ;; the panel takes the form's column while it is open, so it sits next
   ;; to the hits it describes; it comes before them in the document so
   ;; reading order and visual order agree at every width
   (when client? (page/sidebar lang selected))
   (when (or result error) (result-view state))])

(defn page
  "The main content of the page `state` describes, by its `:route`; nil
  for a route this app does not render.

  The two read-only pages take the interface language and their own data,
  whose `:lang` is the corpus's rather than the interface's."
  [{:keys [route lang] :as state}]
  (case route
    :search  (search-view state)
    :corpora (corpus-views/index-view lang (:data state))
    :corpus  (corpus-views/info-view lang (:data state))
    nil))
