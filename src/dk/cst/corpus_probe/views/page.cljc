(ns dk.cst.corpus-probe.views.page
  "Hiccup for the search page: the result summary, per-corpus counts,
  pagination and the inspection sidebar, plus the pieces shared with the
  frequency page: the search form, the error alerts and the download
  links.

  `app-view` is the page's main content (the site header around it is the
  document's). The server renders it for first paint with no selection;
  the client renders the same view from the same state, so clicking a
  token reveals the sidebar without a round trip. The markup uses the
  elements HTML provides for each part -- <search> for the query form, a
  table <caption> for the result summary, <nav> for pagination, an alert
  for errors, an <aside> for the inspector -- so the document is
  meaningful without the stylesheet."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.views.corpus :as corpus-views]
            [dk.cst.corpus-probe.views.kwic :as kwic]))

(defn sort-control
  "The sort control of the concordance: a select over the `sort-modes`
  [value label] pairs with `sort` chosen."
  [sort-modes sort]
  [:p
   [:label {:for "sort"} "Sort"]
   [:select {:id "sort" :name "sort"}
    (for [[value label] sort-modes]
      [:option {:value value :selected (= value sort)} label])]])

(def filter-prefix
  "The query param prefix naming a metadata filter: the prefix followed by
  the attribute name, as in `f.text_year`, one param per chosen value."
  "f.")

(defn filter-phrase
  "The metadata `filter` (a map of attribute to the set of values
  accepted) in words: each attribute with its values, sorted; empty
  without a filter.

  (filter-phrase {:text_year #{\"1591\" \"1583\"} :text_author #{\"ukendt\"}})
  ;; => \"text_author ukendt; text_year 1583, 1591\""
  [filter]
  (str/join "; " (for [[attr values] (sort-by key filter)]
                   (str (name attr) " " (str/join ", " (sort values))))))

(defn within-phrase
  "The metadata `filter` as the qualifier of a result summary, or nil
  without a filter: \" within text_year 1591\"."
  [filter]
  (when (seq filter)
    (str " within " (filter-phrase filter))))

(declare attribute-value)

(defn filter-item
  "One value `m` ({:value :total}) of metadata attribute `attr` in the
  filter fieldset: a checkbox submitting the value under the attribute's
  filter param, checked when the value is in the set `chosen`.

  The label shows the value as the sidebar does (see `attribute-value`)
  and how many regions carry it, when known: a chosen value the corpora
  no longer offer has no count."
  [attr chosen {:keys [value total] :as m}]
  [:li
   [:label
    [:input {:type    "checkbox"
             :name    (str filter-prefix (name attr))
             :value   value
             :checked (contains? chosen value)}]
    " " (attribute-value attr value)
    (when total
      (list " " [:data {:value (str total)}
                 (str total " " (if (= 1 total) "region" "regions"))]))]])

(defn filter-details
  "The disclosure of metadata attribute `attr` in the filter fieldset:
  its value `rows` (see `filter-item`) plus the `chosen` values missing
  from them, so a selection is never lost on resubmit.

  Open, and counting the selection in its summary, when any value is
  chosen."
  [attr rows chosen]
  (let [listed (set (map :value rows))
        rows   (into rows (for [value (sort chosen) :when (not (listed value))]
                            {:value value}))]
    [:details {:open (boolean (seq chosen))}
     [:summary [:code (name attr)]
      (when (seq chosen) (str " · " (count chosen) " selected"))]
     [:ul (map (partial filter-item attr chosen) rows)]]))

(defn filter-fieldset
  "The metadata filter fieldset of the search form from `filters` (see
  dk.cst.corpus-probe.search/filter-options!); nil without metadata.

  A disclosure per listed attribute (`:attrs`) holds a checkbox per value
  (see `filter-details`), followed by one per `:selected` attribute the
  list lacks, then a note naming the `:unlisted` attributes. `:selected`
  maps each attribute to the set of chosen values."
  [{:keys [attrs unlisted selected] :as filters}]
  (when (or (seq attrs) (seq unlisted) (seq selected))
    (let [listed (set (map :name attrs))]
      [:fieldset.filters
       [:legend "Metadata"]
       (for [{attr :name :keys [rows]} attrs]
         (filter-details attr rows (get selected attr)))
       (for [[attr chosen] (sort-by key selected) :when (not (listed attr))]
         (filter-details attr [] chosen))
       (when (seq unlisted)
         [:p "Too many values to list: "
          (interpose ", " (map (fn [attr] [:code (name attr)]) unlisted))])])))

(defn search-form
  "The search form of `state`: over its `:folders` tree of corpus
  overviews and its metadata `:filter-controls`, prefilled from its
  `:params` (:corpus, a vector of selected names, :q :mode :ci :prefix
  :suffix), submitted as GET to `action`, with the page's own `controls`
  (hiccup) after the query.

  Wrapped in a <search> landmark; GET, so every search has a shareable URL
  and works without JavaScript. The controls are the sort of the
  concordance or the grouping attribute of the frequency table. The corpus
  chooser, the metadata filter, the mode radios and the simple-search
  option checkboxes are separate <fieldset> groups."
  [{:keys [folders filter-controls params] :as state} action controls]
  (let [{:keys [corpus q mode ci prefix suffix]} params]
    [:search
     [:form.search {:method "get" :action action}
      (corpus-views/chooser folders (set corpus))
      (filter-fieldset filter-controls)
      [:p
       [:label {:for "q"} "Query"]
       [:input {:id           "q"
                :name         "q"
                :type         "search"
                :value        (or q "")
                :placeholder  "[lemma = \"hund\"] or plain words"
                :autocomplete "off"
                :spellcheck   "false"}]]
      controls
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
      [:button {:type "submit"} "Search"]]]))

(defn corpora-phrase
  "The corpus `names` in words: the one name, or how many there were."
  [names]
  (if (= 1 (count names))
    (first names)
    (str (count names) " corpora")))

(defn hits-phrase
  "The number of hits `n` in words."
  [n]
  (str n " " (if (= 1 n) "hit" "hits")))

(defn result-summary
  "The summary text of a concordance `result` page (`:size` hits over
  `:pages`, in the corpora that could be searched, within its metadata
  `:filter`), used as the concordance table's caption."
  [{:keys [size counts page pages] :as result}]
  (str (hits-phrase size) " in "
       (corpora-phrase (map :corpus (filter :size counts)))
       (within-phrase (:filter result))
       " · page " (inc page) " of " pages))

(defn counts-table
  "The per-corpus hit `counts` of a search over several corpora as a table,
  each corpus linking to its info page; a corpus whose query failed shows
  no count (its error is reported separately)."
  [counts]
  [:table.counts
   [:caption "Hits per corpus"]
   [:thead
    [:tr [:th {:scope "col"} "corpus"] [:th {:scope "col"} "hits"]]]
   [:tbody
    (for [{:keys [corpus size error]} counts]
      [:tr
       [:th {:scope "row"}
        [:a {:href (corpus-views/corpus-href corpus)} [:code corpus]]]
       [:td.n (if error [:em "error"] (corpus-views/group-digits size))]])]])

(defn error-groups
  "The errors among the per-corpus `counts`, grouped by identical error:
  [[error [corpus ...]] ...] in first-seen order, so a query that fails the
  same way in every corpus is reported once."
  [counts]
  (let [failed (filter :error counts)]
    (for [error (distinct (map :error failed))]
      [error (map :corpus (filter #(= error (:error %)) failed))])))

(defn pagination
  "Prev/next navigation for a result page; `prev-href`/`next-href` are
  computed server-side and are nil when out of range. The links carry the
  `rel` values browsers and crawlers use for sequential pages."
  [prev-href next-href]
  (when (or prev-href next-href)
    [:nav.pagination {:aria-label "Pagination"}
     (if prev-href [:a {:href prev-href :rel "prev"} "← previous"] [:span])
     (if next-href [:a {:href next-href :rel "next"} "next →"] [:span])]))

(def error-headings
  "The alert heading for each error :type; CQP's own errors are the
  default."
  {:timeout        "The query timed out"
   :no-corpus      "No corpus selected"
   :unknown-corpus "Unknown corpus"
   :rejected       "Request rejected"
   :misaligned     "CQP output could not be read"
   :internal       "Unexpected error"})

(def error-explanations
  "What to tell the user for the error types that carry no message."
  {:no-corpus      "Select at least one corpus to search."
   :unknown-corpus "The registry has no corpus by that name."
   :misaligned     "CQP printed something other than the requested rows."
   :internal       "The search failed on the server; its log has the details."})

(defn error-section
  "An `error` map rendered as an alert, naming the `corpora` it concerns
  when given. cqp's own messages, including the `<--` position pointer, are
  shown to the user unchanged in a <pre>; errors without a message are
  explained instead."
  [{:keys [type message]} corpora]
  [:section.error {:role "alert"}
   [:h2 (get error-headings type "CQP error")]
   (when (seq corpora)
     [:p "in " (interpose ", " (map (fn [c] [:code c]) corpora))])
   (when-let [explanation (error-explanations type)]
     [:p explanation])
   (when message [:pre message])])

(defn download-links
  "Links downloading the current table in each format of `hrefs` (format
  keyword to URL), with `note` (when given) qualifying what the download
  holds; nil without hrefs. The response itself asks to be saved (its
  Content-Disposition), so the links carry no download attribute."
  [hrefs note]
  (when (seq hrefs)
    [:p.downloads "Download"
     (when note (str " " note))
     ": "
     (interpose " · "
                (for [[format href] (sort hrefs)]
                  [:a {:href href} (str/upper-case (name format))]))]))

(defn result-section
  "The results of the concordance `:result` in `state`: an alert per
  distinct error among its corpora, then (when any corpus could be
  searched) the per-corpus counts, a link to the frequencies of the same
  hits (`:freq-href`), the download links (`:export-hrefs`, exports holding
  at most `:export-limit` hits), the concordance with its `:expanded` hits
  and `:langs`, and the pagination links `:prev-href`/`:next-href`."
  [{:keys [result langs expanded freq-href export-hrefs export-limit
           prev-href next-href] :as state}]
  (let [{:keys [counts hits size]} result]
    [:section.result {:aria-label "Results"}
     (for [[error corpora] (error-groups counts)]
       (error-section error corpora))
     (when (some :size counts)
       (list
        (when (next counts) (counts-table counts))
        (when freq-href
          [:p [:a {:href freq-href} "Frequencies of these hits"]])
        (download-links export-hrefs
                        (when (and export-limit (> size export-limit))
                          (str "the first "
                               (corpus-views/group-digits export-limit)
                               " hits")))
        (kwic/concordance hits {:caption  (result-summary result)
                                :langs    langs
                                :expanded expanded})
        (pagination prev-href next-href)))]))

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
  from `:selected` state); it holds the `selected` token's own attributes,
  its hit's structural metadata and its corpus as titled groups only while
  a token is selected. The `:toggle` handler lets a browser-driven dismiss
  clear the selection."
  [selected]
  [:aside.sidebar {:id         "token-details"
                   :popover    "auto"
                   :aria-label "Token details"
                   :on         {:toggle [:popover-toggle]}}
   (when-let [{:keys [token structs corpus]} selected]
     (list
      [:button {:type "button" :on {:click [:close]}} "Close"]
      (detail-group "Token" (dissoc token :open :close))
      (detail-group "Text" structs)
      (when corpus
        [:section
         [:h2 "Corpus"]
         [:p [:a {:href (corpus-views/corpus-href corpus)} [:code corpus]]]])))])

(defn app-view
  "The search page's main content from application `state`: the search
  form (see `search-form`) with the `:sort-modes` control, then either
  the `:error`, the concordance `:result` (see `result-section`) or
  nothing, plus the inspection `:selected` sidebar."
  [{:keys [sort-modes params result error selected] :as state}]
  [:main
   (search-form state "/" (sort-control sort-modes (:sort params)))
   (cond
     error  (error-section error nil)
     result (result-section state)
     :else  nil)
   (sidebar selected)])
