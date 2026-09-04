(ns dk.cst.corpus-probe.views.page
  "Hiccup for the search page: the result summary, per-corpus counts,
  pagination and the inspection sidebar, plus the pieces shared with the
  frequency page: the search form, the error sections and the download
  links.

  The page these build is assembled by
  dk.cst.corpus-probe.views.search/search-view, which knows both views a
  result can be shown in. The server renders it for first paint with no
  selection; the client renders the same views from the same state, so
  clicking a token reveals the sidebar without a round trip. The markup uses the
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

(def form-id
  "The id of the search form, so a control that acts on a result can sit
  beside the result and still submit the search that produced it."
  "search-form")

(defn sort-control
  "The sort control of the concordance in language `lang`: a select over
  the `sort-modes` [value label-key] pairs with `sort` chosen.

  It names the form it submits with, so it can sit beside the table it
  reorders rather than inside the query form: ordering a result is a
  different task from writing the query that produced it."
  [lang sort-modes sort]
  (list
   [:label {:for "sort"} (i18n/tr lang :sort)]
   " "
   [:select {:id "sort" :name "sort" :form form-id}
    (for [[value k] sort-modes]
      [:option {:value value :selected (= value sort)} (i18n/tr lang k)])]))

(defn view-controls
  "The `controls` (hiccup) that reorder or regroup a result already
  fetched, with the submit that applies them, in language `lang`; nil
  without controls.

  They live with the result rather than in the query form, so re-ordering
  a concordance costs a click instead of a scroll back past the form."
  [lang controls]
  (when controls
    [:p.viewctl controls " "
     [:button {:type "submit" :form form-id} (i18n/tr lang :apply)]]))

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

  Closed whatever is chosen, its summary counting the selection: one
  attribute may carry hundreds of values, and reopening them on every
  resubmit grew the form exactly while the reader was refining it. The
  wording is in language `lang`."
  [lang attr rows chosen]
  (let [listed (set (map :value rows))
        rows   (into rows (for [value (sort chosen) :when (not (listed value))]
                            {:value value}))]
    [:details
     [:summary [:code (name attr)]
      (when (seq chosen)
        (str " · " (count chosen) " " (i18n/tr lang :selected)))]
     [:ul (map (partial filter-item lang attr chosen) rows)]]))

(defn filter-fieldset
  "The metadata filter fieldset of the search form from `filters` (see
  dk.cst.corpus-probe.frequency/filter-options!); nil without metadata.

  One disclosure over the whole filter, counting the values chosen across
  every attribute and open only while the filter is active, so a corpus
  with forty annotated attributes is one line rather than forty. Inside
  it, a disclosure per listed attribute (`:attrs`) holds a checkbox per
  value (see `filter-details`), followed by one per `:selected` attribute
  the list lacks, then a note naming the `:unlisted` attributes, all
  worded in language `lang`. `:selected` maps each attribute to the set of
  chosen values."
  [lang {:keys [attrs unlisted selected] :as filters}]
  (when (or (seq attrs) (seq unlisted) (seq selected))
    (let [listed (set (map :name attrs))
          n      (reduce + (map count (vals selected)))]
      [:fieldset.filters
       [:legend (i18n/tr lang :metadata)]
       [:details {:open (pos? n)}
        [:summary (str n " " (i18n/tr lang :selected))]
        (for [{attr :name :keys [rows]} attrs]
          (filter-details lang attr rows (get selected attr)))
        (for [[attr chosen] (sort-by key selected) :when (not (listed attr))]
          (filter-details lang attr [] chosen))
        (when (seq unlisted)
          [:p (i18n/tr lang :too-many-values)
           (interpose ", "
                      (map (fn [attr] [:code (name attr)]) unlisted))])]])))

(defn query-example
  "The dictionary key of the example query for `mode`: the two modes take
  different input, so one example cannot serve both."
  [mode]
  (if (= mode "cqp") :query-example-cqp :query-example-simple))

(defn search-form
  "The search form of `state`: over its `:folders` tree of corpus
  overviews and its metadata `:filter-controls`, prefilled from its
  `:params` (:corpus, a vector of selected names, :q :mode :ci :prefix
  :suffix), submitted as GET to `action`, with the page's own `extra`
  hidden inputs.

  The query input comes first, then the two settings that decide how it
  is read, then the scope of the search: the corpus chooser and the
  metadata filter, each behind one disclosure. So the field the reader
  reaches for is the first control in the form, whatever the registry
  holds.

  The query example is the placeholder of the mode in `:params`, and the
  mode radios dispatch `:set-mode`, so choosing a mode swaps the example
  without a round trip; without the client the example is the one the
  search was submitted with.

  Wrapped in a <search> landmark; GET, so every search has a shareable URL
  and works without JavaScript. The form carries an id, so a control
  rendered outside it (the sort of the concordance, the grouping of the
  frequency table) still submits with it. The corpus
  chooser, the metadata filter, the mode radios and the simple-search
  option checkboxes are separate <fieldset> groups. No language is
  submitted with the search: which language the answer is worded in is the
  reader's own stored preference, not part of what they asked."
  [{:keys [lang folders filter-controls params] :as state} action extra]
  (let [{:keys [corpus q mode ci prefix suffix]} params]
    [:search
     [:form.search {:id form-id :method "get" :action action}
      extra
      [:p
       [:label {:for "q"} (i18n/tr lang :query)]
       [:input {:id           "q"
                :name         "q"
                :type         "search"
                :value        (or q "")
                :placeholder  (i18n/tr lang (query-example mode))
                :autocomplete "off"
                :spellcheck   "false"}]]
      [:fieldset.mode
       [:legend (i18n/tr lang :query-mode)]
       [:label [:input {:type    "radio" :name "mode" :value "simple"
                        :checked (not= mode "cqp")
                        :on      {:change [:set-mode "simple"]}}]
        (i18n/tr lang :simple)]
       [:label [:input {:type    "radio" :name "mode" :value "cqp"
                        :checked (= mode "cqp")
                        :on      {:change [:set-mode "cqp"]}}]
        "CQP"]]
      ;; marks a selection the reader actually made: without it, unticking
      ;; every corpus and submitting is indistinguishable from arriving
      ;; with no corpus named, which searches them all
      [:input {:type "hidden" :name "scope" :value "chosen"}]
      (corpus-views/chooser lang folders (set corpus))
      (filter-fieldset lang filter-controls)
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

(defn view-switch
  "The switch between the views of one result in language `lang`: each of
  `hrefs` ([view label-key url], see
  dk.cst.corpus-probe.api/view-hrefs) as a link, `view` marked as the one
  being shown; nil without hrefs.

  Links rather than an ARIA tablist: each view is its own URL and its own
  question put to CQP, so following one is a navigation, which is what a
  link means. A tablist would promise a panel that is already loaded."
  [lang view hrefs]
  (when (seq hrefs)
    [:nav.views {:aria-label (i18n/tr lang :result-views)}
     [:ul
      (for [[k label href] hrefs]
        [:li [:a (cond-> {:href href}
                   (= k view) (assoc :aria-current "page"))
              (i18n/tr lang label)]])]]))

(defn result-heading
  "The heading naming the results region in language `lang`: the summary
  of the concordance `result` when any corpus could be searched, else the
  name of the error that came instead, so a search that failed everywhere
  is not announced as a count of nothing."
  [lang {:keys [counts] :as result} error]
  (if (searched? result)
    (result-summary lang result)
    (error-name lang (or error (some :error counts)))))

(defn results-region
  "The outcome of a search in `state` under `heading`, as a region named by
  that heading and focusable, so a GET search can land on it.

  Every view of a result shares this: the heading, the switch between the
  views, the error that replaced the result or the errors of individual
  corpora, and then `body`, the view's own content. The two views differ
  only in what they say about the same hits, so they differ only in what
  they pass here."
  [{:keys [lang view view-hrefs result error] :as state} heading body]
  [:section.result {:id              results-id
                    :tabindex        "-1"
                    :aria-labelledby "results-heading"}
   [:h2 {:id "results-heading"} heading]
   (view-switch lang view view-hrefs)
   (when error (error-body lang error nil))
   (for [[e corpora] (error-groups (:counts result))]
     (error-section lang e corpora))
   body])

(defn result-section
  "The concordance view of the search in `state`: when any corpus could be
  searched and found something, the sort control, the pagination above and
  below the table, the per-corpus counts and the concordance with its
  `:expanded` hits and `:langs`, then the download links (`:export-hrefs`,
  exports holding at most `:export-limit` hits), all worded in the state's
  `:lang` and wrapped in the shared `results-region`."
  [{:keys [lang sort-modes params result error langs expanded client?
           export-hrefs export-limit prev-href next-href]
    :as state}]
  (let [{:keys [counts hits size]} result
        position (when result (page-phrase lang result))]
    (results-region
     state
     (result-heading lang result error)
     (when (searched? result)
       ;; a search that found nothing has nothing to page, download or
       ;; count: the table would be a header over no rows and the exports
       ;; header-only files
       (if (zero? size)
         [:p (i18n/tr lang :no-hits)]
         (list
          (view-controls lang (sort-control lang sort-modes (:sort params)))
          (pagination lang prev-href next-href position)
          (when (next counts) (counts-table lang counts))
          (kwic/concordance hits {:caption  (i18n/tr lang :concordance)
                                  :lang     lang
                                  :langs    langs
                                  :expanded expanded
                                  :client?  client?
                                  :cursor   (:cursor state)})
          (pager-links lang prev-href next-href position)
          ;; what to do next with these hits, so it follows them: reading
          ;; the concordance is the task, taking it elsewhere is the one
          ;; after
          (download-links lang export-hrefs
                          (when (and export-limit (> size export-limit))
                            (str (i18n/tr lang :the-first) " "
                                 (i18n/group-digits lang export-limit) " "
                                 (i18n/tr lang :hits))))))))))

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
     [:h3 title]
     (attribute-list m)]))

(defn sidebar
  "The token inspection panel: what the concordance's cursor is on, in
  language `lang`, from `selected` (its :token, :structs and :corpus);
  nil while nothing is selected.

  Above the rail's breakpoint it takes the query column, so it sits beside
  the hits it describes without narrowing them; below it, it is a sheet at
  the foot of the viewport.

  It does not take focus: the cursor stays on the token so the arrow keys
  keep moving, and the panel describes whatever the cursor is on. That is
  why it is not a popover, which would put itself in the top layer, out of
  the grid, and want focus of its own. Escape closes it from the
  concordance.

  The group titles are in `lang`; the attribute names inside them are the
  corpus's own."
  [lang {:keys [token structs corpus] :as selected}]
  (when selected
    [:aside.sidebar {:aria-label (i18n/tr lang :token-details)}
     [:h2 (i18n/tr lang :token-details)]
     [:button {:type "button" :on {:click [:close]}} (i18n/tr lang :close)]
     (detail-group (i18n/tr lang :token) (dissoc token :open :close))
     (detail-group (i18n/tr lang :text) structs)
     (when corpus
       [:section
        [:h3 (i18n/tr lang :corpus-heading)]
        [:p [:a {:href (corpus-views/corpus-href lang corpus)}
             [:code corpus]]]])]))
