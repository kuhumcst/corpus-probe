(ns dk.cst.corpus-probe.views.page
  "Hiccup for the search page: the result summary, per-corpus counts,
  pagination and the inspection sidebar, plus the pieces shared with the
  frequency page: the search form, the error sections and the download
  links.

  `app-view` is the page's main content (the site header around it is the
  document's). The server renders it for first paint with no selection;
  the client renders the same view from the same state, so clicking a
  token reveals the sidebar without a round trip. The markup uses the
  element HTML provides for each part: <search> for the query form, a
  named region for the outcome, <nav> for pagination, headings for errors
  and an <aside> for the inspector, so the document is meaningful without
  the stylesheet."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.views.corpus :as corpus-views]
            [dk.cst.corpus-probe.views.kwic :as kwic]
            [dk.cst.corpus-probe.views.layout :as layout]))

(def results-id
  "The id of the results region a search lands on."
  "results")

(def results-fragment
  "The fragment every form action and every link to a result ends in, so a
  submit or a page turn lands the reader on the answer rather than at the
  top of the form that asked for it. Named once, so the URLs and the
  region they name cannot drift apart."
  (str "#" results-id))

(defn sort-control
  "The sort control of the concordance in language `lang`: a select over
  the `sort-modes` [value label-key] pairs with `sort` chosen."
  [lang sort-modes sort]
  [:p
   [:label {:for "sort"} (i18n/tr lang :sort)]
   [:select {:id "sort" :name "sort"}
    (for [[value k] sort-modes]
      [:option {:value value :selected (= value sort)} (i18n/tr lang k)])]])

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
  "The metadata `filter` as the qualifier of a result summary in language
  `lang`, or nil without a filter: \" within text_year 1591\"."
  [lang filter]
  (when (seq filter)
    (str " " (i18n/tr lang :within) " " (filter-phrase filter))))

(declare attribute-value)

(defn filter-item
  "One value `m` ({:value :total}) of metadata attribute `attr` in the
  filter fieldset: a checkbox submitting the value under the attribute's
  filter param, checked when the value is in the set `chosen`.

  The label shows the value as the sidebar does (see `attribute-value`)
  and, in language `lang`, how many regions carry it, when known: a chosen
  value the corpora no longer offer has no count."
  [lang attr chosen {:keys [value total] :as m}]
  [:li
   [:label
    [:input {:type    "checkbox"
             :name    (str filter-prefix (name attr))
             :value   value
             :checked (contains? chosen value)}]
    " " (attribute-value attr value)
    (when total
      (list " " [:data {:value (str total)}
                 (str (i18n/group-digits lang total) " "
                      (i18n/tr lang (if (= 1 total) :region :regions)))]))]])

(defn filter-details
  "The disclosure of metadata attribute `attr` in the filter fieldset:
  its value `rows` (see `filter-item`) plus the `chosen` values missing
  from them, so a selection is never lost on resubmit.

  Open, and counting the selection in its summary, when any value is
  chosen. The wording is in language `lang`."
  [lang attr rows chosen]
  (let [listed (set (map :value rows))
        rows   (into rows (for [value (sort chosen) :when (not (listed value))]
                            {:value value}))]
    [:details {:open (boolean (seq chosen))}
     [:summary [:code (name attr)]
      (when (seq chosen)
        (str " · " (count chosen) " " (i18n/tr lang :selected)))]
     [:ul (map (partial filter-item lang attr chosen) rows)]]))

(defn filter-fieldset
  "The metadata filter fieldset of the search form from `filters` (see
  dk.cst.corpus-probe.frequency/filter-options!); nil without metadata.

  A disclosure per listed attribute (`:attrs`) holds a checkbox per value
  (see `filter-details`), followed by one per `:selected` attribute the
  list lacks, then a note naming the `:unlisted` attributes, all worded in
  language `lang`. `:selected` maps each attribute to the set of chosen
  values."
  [lang {:keys [attrs unlisted selected] :as filters}]
  (when (or (seq attrs) (seq unlisted) (seq selected))
    (let [listed (set (map :name attrs))]
      [:fieldset.filters
       [:legend (i18n/tr lang :metadata)]
       (for [{attr :name :keys [rows]} attrs]
         (filter-details lang attr rows (get selected attr)))
       (for [[attr chosen] (sort-by key selected) :when (not (listed attr))]
         (filter-details lang attr [] chosen))
       (when (seq unlisted)
         [:p (i18n/tr lang :too-many-values)
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
  option checkboxes are separate <fieldset> groups. The chosen `:lang` is
  submitted along with the search, so the results come back in the same
  language."
  [{:keys [lang folders filter-controls params] :as state} action controls]
  (let [{:keys [corpus q mode ci prefix suffix]} params]
    [:search
     [:form.search {:method "get" :action action}
      [:input {:type "hidden" :name "lang" :value lang}]
      (corpus-views/chooser lang folders (set corpus))
      (filter-fieldset lang filter-controls)
      [:p
       [:label {:for "q"} (i18n/tr lang :query)]
       [:input {:id           "q"
                :name         "q"
                :type         "search"
                :value        (or q "")
                :placeholder  (i18n/tr lang :query-example)
                :autocomplete "off"
                :spellcheck   "false"}]]
      controls
      [:fieldset.mode
       [:legend (i18n/tr lang :query-mode)]
       [:label [:input {:type "radio" :name "mode" :value "cqp"
                        :checked (not= mode "simple")}] "CQP"]
       [:label [:input {:type "radio" :name "mode" :value "simple"
                        :checked (= mode "simple")}] (i18n/tr lang :simple)]]
      [:fieldset.options
       [:legend (i18n/tr lang :simple-options)]
       [:label [:input {:type "checkbox" :name "ci" :value "on"
                        :checked (some? ci)}] (i18n/tr lang :ignore-case)]
       [:label [:input {:type "checkbox" :name "prefix" :value "on"
                        :checked (some? prefix)}] (i18n/tr lang :starts-with)]
       [:label [:input {:type "checkbox" :name "suffix" :value "on"
                        :checked (some? suffix)}] (i18n/tr lang :ends-with)]]
      [:button {:type "submit"} (i18n/tr lang :submit)]]]))

(defn corpora-phrase
  "The corpus `names` in words in language `lang`: the one name, or how
  many there were."
  [lang names]
  (if (= 1 (count names))
    (first names)
    (str (count names) " " (i18n/tr lang :corpora))))

(defn hits-phrase
  "The number of hits `n` in words in language `lang`."
  [lang n]
  (str (i18n/group-digits lang n) " "
       (i18n/tr lang (if (= 1 n) :hit :hits))))

(defn page-phrase
  "Where in a paged `result` the reader is, in language `lang`."
  [lang {:keys [page pages] :as result}]
  (str (i18n/tr lang :page) " " (inc page) " " (i18n/tr lang :of) " " pages))

(defn result-summary
  "The summary text of a concordance `result` page (`:size` hits over
  `:pages`, in the corpora that could be searched, within its metadata
  `:filter`), used as the heading naming the results region, in language
  `lang`."
  [lang {:keys [size counts] :as result}]
  (str (hits-phrase lang size) " " (i18n/tr lang :in) " "
       (corpora-phrase lang (map :corpus (filter :size counts)))
       (within-phrase lang (:filter result))
       " · " (page-phrase lang result)))

(defn counts-table
  "The per-corpus hit `counts` of a search over several corpora as a table,
  each corpus linking to its info page; a corpus whose query failed shows
  no count (its error is reported separately). The wording is in language
  `lang`."
  [lang counts]
  [:table.counts
   [:caption (i18n/tr lang :hits-per-corpus)]
   [:thead
    [:tr [:th {:scope "col"} (i18n/tr lang :corpus)]
     [:th {:scope "col"} (i18n/tr lang :hits)]]]
   [:tbody
    (for [{:keys [corpus size error]} counts]
      [:tr
       [:th {:scope "row"}
        [:a {:href (corpus-views/corpus-href lang corpus)} [:code corpus]]]
       [:td.n (if error
                [:em (i18n/tr lang :error)]
                (i18n/group-digits lang size))]])]])

(defn error-groups
  "The errors among the per-corpus `counts`, grouped by identical error:
  [[error [corpus ...]] ...] in first-seen order, so a query that fails the
  same way in every corpus is reported once."
  [counts]
  (let [failed (filter :error counts)]
    (for [error (distinct (map :error failed))]
      [error (map :corpus (filter #(= error (:error %)) failed))])))

(defn pager-links
  "The page links of a result around `position` (where in the sequence the
  reader is), labelled in language `lang`; nil when neither
  `prev-href` nor `next-href` is in range.

  A list, so assistive technology can say how many options there are, and
  an absent direction is left out rather than held open by an empty
  element, which nothing positioned. The links carry the `rel` values
  browsers and crawlers use for sequential pages."
  [lang prev-href next-href position]
  (when (or prev-href next-href)
    [:ul.pager
     (when prev-href
       [:li [:a {:href prev-href :rel "prev"}
             (str "← " (i18n/tr lang :previous))]])
     [:li position]
     (when next-href
       [:li [:a {:href next-href :rel "next"}
             (str (i18n/tr lang :next) " →")]])]))

(defn pagination
  "`pager-links` as a navigation landmark named in language `lang`.

  Only one of a result's two pagers is a landmark: the APG asks each
  landmark of a repeated role to carry a name of its own, and two named
  Pagination cannot be told apart. The repeat below the table is the bare
  list, whose links stay operable and stay in the tab order."
  [lang prev-href next-href position]
  (when-let [links (pager-links lang prev-href next-href position)]
    [:nav.pagination {:aria-label (i18n/tr lang :pagination)} links]))

(def error-types
  "The error :types this project reports itself. Each doubles as the
  dictionary key of its heading; anything else is CQP's own error, headed
  as such and carrying its message."
  #{:timeout :no-corpus :unknown-corpus :rejected :misaligned :internal})

(def error-explanations
  "The dictionary key explaining each error type that carries no message
  of its own."
  {:no-corpus      :no-corpus-why
   :unknown-corpus :unknown-corpus-why
   :misaligned     :misaligned-why
   :internal       :internal-why})

(defn error-body
  "The parts of an `error` under its heading in language `lang`: the
  `corpora` it concerns, the explanation of a type that carries no message,
  and cqp's own message verbatim, its `<--` position pointer included, as
  the sample output of another program."
  [lang {:keys [type message]} corpora]
  (list
   (when (seq corpora)
     [:p (str (i18n/tr lang :in) " ")
      (interpose ", " (map (fn [c] [:code c]) corpora))])
   (when-let [k (error-explanations type)]
     [:p (i18n/tr lang k)])
   ;; the stylesheet scrolls this rather than letting cqp's column-aligned
   ;; pointer reflow, and a scroll container a keyboard cannot reach is
   ;; unreadable in the browsers that do not focus scrollers themselves
   (when message [:pre {:tabindex "0"} [:samp message]])))

(defn error-name
  "The heading naming `error` in language `lang`: the type this project
  reports itself, or cqp's own error."
  [lang {:keys [type] :as error}]
  (i18n/tr lang (if (error-types type) type :cqp-error)))

(defn error-section
  "An `error` map under a heading of its own in language `lang`, naming
  the `corpora` it concerns when given.

  No live region: every error here arrives by a full page reload, where a
  region that is already populated announces nothing, while the alert role
  costs the section its own semantics and flattens its heading. The reader
  reaches the error because the search lands on it (see `result-section`).

  An h3, since it sits inside a results region already headed by an h2."
  [lang error corpora]
  [:section.error
   [:h3 (error-name lang error)]
   (error-body lang error corpora)])

(defn download-links
  "Links downloading the current table in each format of `hrefs` (format
  keyword to URL), with `note` (when given) qualifying what the download
  holds; nil without hrefs, worded in language `lang`. The response itself
  asks to be saved (its Content-Disposition), so the links carry no
  download attribute."
  [lang hrefs note]
  (when (seq hrefs)
    [:p.downloads (i18n/tr lang :download)
     (when note (str " " note))
     ": "
     (interpose " · "
                (for [[format href] (sort hrefs)]
                  [:a {:href href} (str/upper-case (name format))]))]))

(defn searched?
  "True when any corpus of `result` could be searched, so its counts are an
  answer rather than a report of failure."
  [{:keys [counts] :as result}]
  (boolean (some :size counts)))

(defn result-heading
  "The heading naming the results region in language `lang`: the summary
  of the concordance `result` when any corpus could be searched, else the
  name of the error that came instead, so a search that failed everywhere
  is not announced as a count of nothing."
  [lang {:keys [counts] :as result} error]
  (if (searched? result)
    (result-summary lang result)
    (error-name lang (or error (some :error counts)))))

(defn result-section
  "The outcome of a search in `state`, as a region named by its own
  visible heading and focusable, so a GET search can land on it.

  Holds either the concordance `:result` or the `:error` that came
  instead: the errors of individual corpora, then, when any corpus could
  be searched and found something, the pagination above and below the
  table, the per-corpus counts, a link to the frequencies of the same hits
  (`:freq-href`), the download links (`:export-hrefs`, exports holding at
  most `:export-limit` hits) and the concordance with its `:expanded` hits
  and `:langs`, all worded in the state's `:lang`."
  [{:keys [lang result error langs expanded freq-href export-hrefs
           export-limit prev-href next-href] :as state}]
  (let [{:keys [counts hits size]} result
        position (when result (page-phrase lang result))]
    [:section.result {:id              results-id
                      :tabindex        "-1"
                      :aria-labelledby "results-heading"}
     [:h2 {:id "results-heading"} (result-heading lang result error)]
     (when error (error-body lang error nil))
     (for [[e corpora] (error-groups counts)]
       (error-section lang e corpora))
     (when (searched? result)
       ;; a search that found nothing has nothing to page, download or
       ;; count: the table would be a header over no rows and the exports
       ;; header-only files
       (if (zero? size)
         [:p (i18n/tr lang :no-hits)]
         (list
          (pagination lang prev-href next-href position)
          (when (next counts) (counts-table lang counts))
          (when freq-href
            [:p [:a {:href freq-href} (i18n/tr lang :these-freqs)]])
          (download-links lang export-hrefs
                          (when (and export-limit (> size export-limit))
                            (str (i18n/tr lang :the-first) " "
                                 (i18n/group-digits lang export-limit) " "
                                 (i18n/tr lang :hits))))
          (kwic/concordance hits {:caption  (i18n/tr lang :concordance)
                                  :lang     lang
                                  :langs    langs
                                  :expanded expanded})
          (pager-links lang prev-href next-href position))))]))

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
  clear the selection. The group titles are in language `lang`; the
  attribute names inside them are the corpus's own."
  [lang selected]
  [:aside.sidebar {:id         "token-details"
                   :popover    "auto"
                   :aria-label (i18n/tr lang :token-details)
                   :on         {:toggle [:popover-toggle]}}
   (when-let [{:keys [token structs corpus]} selected]
     (list
      [:button {:type "button" :on {:click [:close]}} (i18n/tr lang :close)]
      (detail-group (i18n/tr lang :token) (dissoc token :open :close))
      (detail-group (i18n/tr lang :text) structs)
      (when corpus
        [:section
         [:h2 (i18n/tr lang :corpus-heading)]
         [:p [:a {:href (corpus-views/corpus-href lang corpus)}
              [:code corpus]]]])))])

(defn app-view
  "The search page's main content from application `state`, in its
  `:lang`: the search form (see `search-form`) with the `:sort-modes`
  control, the results region when the params described a search (see
  `result-section`, which holds the `:error` as well as the `:result`),
  and the inspection `:selected` sidebar.

  The form submits to the results fragment, so a search lands the reader
  on its own answer rather than at the top of the form that asked for it.
  The <main> is focusable so the bypass link can move the reader into it."
  [{:keys [lang sort-modes params result error selected] :as state}]
  [:main layout/main-attrs
   [:h1 (i18n/tr lang :search)]
   (search-form state (str "/" results-fragment)
                (sort-control lang sort-modes (:sort params)))
   (when (or result error) (result-section state))
   (sidebar lang selected)])
