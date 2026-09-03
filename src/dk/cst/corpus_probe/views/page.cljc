(ns dk.cst.corpus-probe.views.page
  "Hiccup for the search page: layout, search form, result summary,
  pagination and the inspection sidebar.

  `app-view` is the whole page body. The server renders it for first paint
  with no selection; the client renders the same view from the same state,
  so clicking a token reveals the sidebar without a round trip. The markup
  uses the elements HTML provides for each part -- <search> for the query
  form, a table <caption> for the result summary, <nav> for pagination, an
  alert for errors, an <aside> for the inspector -- so the document is
  meaningful without the stylesheet."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.views.kwic :as kwic]))

(defn search-form
  "The search form over `corpora` (corpus maps), prefilled from `params`
  (:corpus :q :mode :ci :prefix :suffix).

  Wrapped in a <search> landmark and submitted as GET, so every search has a
  shareable URL and works without JavaScript. The mode radios and the
  simple-search option checkboxes are separate <fieldset> groups.
  `sort-modes` are the [value label] pairs offered by the sort control."
  [corpora sort-modes {:keys [corpus q mode ci prefix suffix sort]}]
  [:search
   [:form.search {:method "get" :action "/"}
    [:p
     [:label {:for "corpus"} "Corpus"]
     [:select {:id "corpus" :name "corpus"}
      (for [{:keys [id]} corpora
            :let [value (str/upper-case id)]]
        [:option {:value value :selected (= value corpus)} value])]]
    [:p
     [:label {:for "q"} "Query"]
     [:input {:id           "q"
              :name         "q"
              :type         "search"
              :value        (or q "")
              :placeholder  "[lemma = \"hund\"] or plain words"
              :autocomplete "off"
              :spellcheck   "false"}]]
    [:p
     [:label {:for "sort"} "Sort"]
     [:select {:id "sort" :name "sort"}
      (for [[value label] sort-modes]
        [:option {:value value :selected (= value sort)} label])]]
    [:fieldset.mode
     [:legend "Query mode"]
     [:label [:input {:type "radio" :name "mode" :value "cqp"
                      :checked (not= mode "simple")}] "CQP"]
     [:label [:input {:type "radio" :name "mode" :value "simple"
                      :checked (= mode "simple")}] "Simple"]]
    [:fieldset.options
     [:legend "Simple-search options"]
     [:label [:input {:type "checkbox" :name "ci" :value "on"
                      :checked (some? ci)}] "ignore case"]
     [:label [:input {:type "checkbox" :name "prefix" :value "on"
                      :checked (some? prefix)}] "starts with"]
     [:label [:input {:type "checkbox" :name "suffix" :value "on"
                      :checked (some? suffix)}] "ends with"]]
    [:button {:type "submit"} "Search"]]])

(defn result-summary
  "The summary text of a KWIC `result` page (`:size` hits over `:pages`),
  used as the concordance table's caption."
  [{:keys [size corpus page pages]}]
  (str size " " (if (= 1 size) "hit" "hits") " in " corpus
       " · page " (inc page) " of " pages))

(defn pagination
  "Prev/next navigation for a result page; `prev-href`/`next-href` are
  computed server-side and are nil when out of range. The links carry the
  `rel` values browsers and crawlers use for sequential pages."
  [prev-href next-href]
  (when (or prev-href next-href)
    [:nav.pagination {:aria-label "Pagination"}
     (if prev-href [:a {:href prev-href :rel "prev"} "← previous"] [:span])
     (if next-href [:a {:href next-href :rel "next"} "next →"] [:span])]))

(defn error-section
  "A CQP `error` map rendered as an alert. cqp's own messages, including the
  `<--` position pointer, are shown to the user unchanged in a <pre>."
  [{:keys [type message]}]
  [:section.error {:role "alert"}
   [:h2 (case type
          :timeout "The query timed out"
          "CQP error")]
   (when message [:pre message])])

(defn attribute-value
  "Render attribute value `v` semantically by its key `k`: a text title as
  `<cite>`, a four-digit year as `<time>`, otherwise plain text."
  [k v]
  (let [n (name k)]
    (cond
      (str/ends-with? n "_title")                             [:cite v]
      (and (str/ends-with? n "_year") (re-matches #"\d{4}" v)) [:time v]
      :else                                                   v)))

(defn attribute-list
  "A definition list of the attribute map `m`, each value rendered by
  `attribute-value`."
  [m]
  [:dl (mapcat (fn [[k v]] [[:dt (name k)] [:dd (attribute-value k v)]]) m)])

(defn detail-group
  "A titled group of attributes `m` in the sidebar, or nil when empty."
  [title m]
  (when (seq m)
    [:section
     [:h2 title]
     (attribute-list m)]))

(defn sidebar
  "The token inspection panel, an auto popover so it floats in the top layer
  with the browser's own light-dismiss and Escape handling.

  The `<aside>` is always present (the client toggles it via the Popover API
  from `:selected` state); it holds the `selected` token's own attributes and
  its hit's structural metadata as titled groups only while a token is
  selected. The `:toggle` handler lets a browser-driven dismiss clear the
  selection."
  [selected]
  [:aside.sidebar {:id         "token-details"
                   :popover    "auto"
                   :aria-label "Token details"
                   :on         {:toggle [:popover-toggle]}}
   (when-let [{:keys [token structs]} selected]
     (list
      [:button {:type "button" :on {:click [:close]}} "Close"]
      (detail-group "Token" (dissoc token :open :close))
      (detail-group "Text" structs)))])

(defn app-view
  "The whole page body from application `state`: the search form, then
  either the CQP `:error`, the KWIC `:result` (with pagination) or nothing,
  plus the inspection `:selected` sidebar. `:content-lang`, when present,
  marks the corpus text's language."
  [{:keys [corpora sort-modes params result error selected expanded prev-href
           next-href content-lang]}]
  [:main
   [:header
    [:h1 [:a {:href "/"} "corpus-probe"]]
    [:p.subtitle "CWB corpus search"]]
   (search-form corpora sort-modes params)
   (cond
     error  (error-section error)
     result [:section.result {:aria-label "Results"}
             (kwic/concordance (:hits result)
                               {:caption  (result-summary result)
                                :lang     content-lang
                                :expanded expanded})
             (pagination prev-href next-href)]
     :else  nil)
   (sidebar selected)])
