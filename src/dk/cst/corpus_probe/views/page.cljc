(ns dk.cst.corpus-probe.views.page
  "Hiccup for the search page: the result summary, per-corpus counts,
  pagination and the inspection sidebar, plus the pieces shared with the
  frequency page: the search form, the error sections and the download
  links.

  The page these build is assembled by
  dk.cst.corpus-probe.views.app/search-view, which knows both views a
  result can be shown in. The server renders it for first paint with no
  selection; the client renders the same views from the same state, so
  clicking a token reveals the sidebar without a round trip. The markup
  uses the element HTML provides for each part: <search> for the query
  form, a named region for the outcome, <nav> for pagination, headings for
  errors and an <aside> for the inspector, so the document is meaningful
  without the stylesheet."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.views.controls :as controls]
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

(defn sort-label
  "What the sort mode `value` (see dk.cst.corpus-probe.query/sort-modes)
  is called, in `ui`; the value itself for a mode nothing names.

  Naming them here rather than in the query namespace keeps the CQP
  command table free of anything the interface decides."
  [ui value]
  (case value
    "corpus" (i18n/tr ui "corpus order")
    "word"   (i18n/tr ui "match")
    "left"   (i18n/tr ui "left context")
    "right"  (i18n/tr ui "right context")
    "random" (i18n/tr ui "random")
    value))

(defn sort-control
  "The sort control of the concordance in `ui`: a select over
  the `sort-modes` values (see dk.cst.corpus-probe.query/sort-modes)
  with `sort` chosen and each named by `sort-label`.

  It names the form it submits with, so it can sit beside the table it
  reorders rather than inside the query form: ordering a result is a
  different task from writing the query that produced it."
  [ui sort-modes sort]
  (list
   [:label {:for "sort"} (i18n/tr ui "Sort")]
   " "
   [:select {:id   "sort" :name "sort" :form form-id
             :on   {:change [:apply-view]}}
    (for [value sort-modes]
      [:option {:value value :selected (= value sort)}
       (sort-label ui value)])]))

(def sample-sizes
  "The sample sizes the concordance offers, in display order: as many
  hits as a reader might work through by hand, a result larger than that
  being read by sampling it rather than by paging to the end.

  A hand-written URL may name any other size, which `sample-control`
  then shows beside these."
  [50 100 500 1000])

(defn sample-control
  "The sample control of the concordance in `ui`: a select over the
  `sample-sizes` with `sample` chosen, or the whole result when it names
  none.

  A size the list does not hold is offered beside them, so that a URL
  naming one shows as the sample it is rather than as the whole result.
  It names the form it submits with for the reason `sort-control` does."
  [ui sample]
  (list
   [:label {:for "sample"} (i18n/tr ui "Sample")]
   " "
   [:select {:id   "sample" :name "sample" :form form-id
             :on   {:change [:apply-view]}}
    [:option {:value "" :selected (nil? sample)} (i18n/tr ui "all hits")]
    (for [n (sort (cond-> (set sample-sizes) sample (conj sample)))]
      [:option {:value n :selected (= n sample)}
       (i18n/group-digits ui n)])]))

(defn view-controls
  "The `controls` (hiccup) deciding how a result is read rather than what
  was searched for, in `ui`; nil without controls.

  They live with the result rather than in the query form, so re-ordering
  a concordance costs a click instead of a scroll back past the form.
  Sampling one is here for the same reason though it runs the query
  again: how many of the hits to read is a question the reader has on
  seeing how many there are.

  Each applies itself on being changed, so where `client?` there is no
  button: choosing an order is asking for it, and a control that needs a
  second control to take effect is one the reader has to be told about.
  Without a client nothing can act on a change, so the button is what
  applies it there."
  [ui client? controls]
  (when controls
    [:p.viewctl controls
     (when-not client?
       (list " " [:button {:type "submit" :form form-id}
                  (i18n/tr ui "Apply")]))]))

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
  "The metadata `filter` as the qualifier of a result summary in `ui`,
  or nil without a filter: \" within text_year 1591\"."
  [ui filter]
  (when (seq filter)
    (str " " (i18n/tr ui "within") " " (filter-phrase filter))))

(defn attribute-value
  "Render attribute value `v` semantically by its key `k`: a text title as
  `<cite>`, a four-digit year as `<time>`, otherwise plain text."
  [k v]
  (let [n (name k)]
    (cond
      (str/ends-with? n "_title")                              [:cite v]
      (and (str/ends-with? n "_year") (re-matches #"\d{4}" v)) [:time v]
      :else                                                    v)))

(defn filter-item
  "One metadata value `m` of attribute `attr` as a filter entry: a checkbox
  named for the attribute's filter param, checked when `chosen` holds its
  value, and, in `ui`, how many regions carry it, when known: a
  chosen value the corpora no longer offer has no count.

  A value the filter box has hidden keeps its checkbox in the document,
  for the reason a filtered-out corpus does: a box the form cannot see is
  part of a filter dropped without anyone saying so."
  [ui attr chosen {:keys [value total hidden?] :as m}]
  [:li (cond-> {} hidden? (assoc :hidden true))
   [:label
    [:input {:type    "checkbox"
             :name    (str filter-prefix (name attr))
             :value   value
             :checked (contains? chosen value)
             :on      {:change [:toggle-filter-values [attr [value]]]}}]
    " " (attribute-value attr value)
    (when total
      (list " " [:data {:value (str total)}
                 (str (i18n/group-digits ui total) " "
                      (i18n/trn ui "region" "regions" total))]))]])

(defn attr-rows
  "The rows to offer for attribute `attr`: its listed `rows` followed by
  the `chosen` values missing from them, so a selection is never lost on
  resubmit."
  [rows chosen]
  (let [listed (set (map :value rows))]
    (into (vec rows)
          (for [value (sort chosen) :when (not (listed value))]
            {:value value}))))

(defn narrow-attrs
  "`attrs` (each with its rows already prepared) with every value that
  does not answer `q` marked `:hidden?`, and an attribute left with
  nothing showing marked too.

  An attribute whose own name answers `q` keeps all of its values, so
  naming an attribute is a way of asking for its values rather than a way
  of finding none. Marked rather than dropped, so that what a reader has
  chosen goes on being submitted while they look for something else."
  [q attrs]
  (mapv (fn [{:keys [name rows] :as attr}]
          (let [whole? (controls/answers? q name)
                rows   (mapv #(cond-> %
                                (not (or whole? (controls/answers? q (:value %))))
                                (assoc :hidden? true))
                             rows)]
            (cond-> (assoc attr :rows rows)
              (every? :hidden? rows) (assoc :hidden? true))))
        attrs))

(defn filter-details
  "The disclosure of one prepared metadata attribute (its `:name`, its
  `:rows` and whether the filter has left it `:hidden?`) in the filter
  fieldset, with the `chosen` values of that attribute and, where
  `client?`, a control taking every value showing.

  Closed whatever is chosen, its summary counting the selection: one
  attribute may carry hundreds of values, and reopening them on every
  resubmit grew the form exactly while the reader was refining it. The
  wording is in `ui`."
  [ui client? {attr :name :keys [rows hidden?]} chosen]
  ;; a set even when nothing is chosen, since it is read as a predicate
  (let [chosen  (set chosen)
        showing (mapv :value (remove :hidden? rows))]
    (controls/toggled
     (when client?
       (controls/select-all (str (i18n/tr ui "All values of") " " (name attr))
                            showing chosen
                            [:toggle-filter-values [attr showing]]))
     [:details (cond-> {} hidden? (assoc :hidden true))
      [:summary [:code (name attr)]
       (when (seq chosen)
         (str " · " (count chosen) " " (i18n/tr ui "selected")))]
      [:ul (map (partial filter-item ui attr chosen) rows)]])))

(defn value-count
  "How many metadata values are chosen across every attribute of
  `selected`, a map of attribute to the set chosen under it."
  [selected]
  (reduce + (map count (vals selected))))

(defn clear-toggle
  "The control emptying the whole metadata filter, over the prepared
  `attrs` and the `selected` values, in `ui`.

  The same control the corpus chooser carries in this position, with the
  one direction that has no meaning here taken away. Choosing every value
  of every attribute is not a filter at all: it accepts every region
  carrying the attribute, which is what choosing none already does, and it
  would put one query parameter per value into the URL, tens of thousands
  of them at the KU registry. So it is disabled while nothing is chosen,
  which is the only state it could do that from, and every state it is
  offered in clears.

  It empties the whole filter rather than the part the box is showing.
  A filter is not a thing to empty by halves: what survived would be a
  constraint the reader had just told the box to hide from them."
  [ui attrs selected]
  (let [items (for [{attr :name :keys [rows]} attrs
                    {:keys [value]} rows]
                [attr value])]
    (controls/select-all (i18n/tr ui "Clear filter")
                         items
                         (fn [[attr value]]
                           (contains? (get selected attr) value))
                         [:clear-filter]
                         {:clear-only? true})))

(defn filter-fieldset
  "The metadata filter fieldset of the search form from `filters` (see
  dk.cst.corpus-probe.frequency/filter-options!); nil without metadata.

  One disclosure over the whole filter, counting the values chosen across
  every attribute, so a corpus with forty annotated attributes is one line
  rather than forty. Inside it, a disclosure per listed attribute
  (`:attrs`) holds a checkbox per value (see `filter-details`), followed
  by one per `:selected` attribute the list lacks, then a note naming the
  `:unlisted` attributes, all worded in `ui`. `:selected` maps
  each attribute to the set of chosen values.

  Where `:client?`, it carries the same three controls the corpus chooser
  does, from the same namespace: a box narrowing it to the attributes and
  values answering `:filter`, a control per attribute taking every value
  showing, and one beside the whole fieldset (see `clear-toggle`).

  The count is of `:selected`, which is live: it follows the boxes as the
  reader ticks them, rather than saying what the last search was for.
  What is open follows `:served`, the selection the page arrived with,
  until `:open?` says the reader has opened or shut it themselves, for the
  reason the corpus chooser's does: a count that opened and shut the
  fieldset as it changed would be moving the controls while they were
  being used.

  Which attributes there are to filter by depends on the corpora
  selected, and only the server knows: `:pending?` marks the fieldset busy
  while the client is fetching them for a selection that has changed."
  [ui {:keys [attrs unlisted selected] :as filters}
   {:keys [served open? pending? client?] q :filter}]
  (when (or (seq attrs) (seq unlisted) (seq selected))
    (let [listed    (set (map :name attrs))
          n         (value-count selected)
          prepared  (concat
                     (for [{attr :name :keys [rows]} attrs]
                       {:name attr :rows (attr-rows rows (get selected attr))})
                     (for [[attr chosen] (sort-by key selected)
                           :when (not (listed attr))]
                       {:name attr :rows (attr-rows [] chosen)}))
          filtering (not (str/blank? q))
          shown     (cond->> prepared
                      filtering (narrow-attrs (str/lower-case q)))
          nothing-found? (and filtering (every? :hidden? shown))]
      [:fieldset.filters
       [:legend (i18n/tr ui "Metadata")]
       (when client?
         (controls/filter-box "value-filter" (i18n/tr ui "Filter values") q
                              [:filter-values]))
       (when client?
         (controls/filter-status (when nothing-found?
                                   (i18n/tr ui "No values found."))))
       (controls/toggled
        (when client? (clear-toggle ui prepared selected))
       ;; TODO: a visible in-flight treatment. aria-busy says it to a
       ;; screen reader and nothing says it to anyone else; the
       ;; role="status" pattern of `navigation-status` is the obvious one
       ;; to reach for when we decide what it should look like
       [:details (cond-> {:open (or filtering
                                    (if (some? open?)
                                      open?
                                      (pos? (value-count served))))
                          :on   {:toggle [:set-filters-open]}}
                   pending?       (assoc :aria-busy "true")
                   ;; nothing in it answers, so the message below stands
                   ;; where it was; hidden rather than dropped, because
                   ;; its checkboxes are what a search submits
                   nothing-found? (assoc :hidden true))
        [:summary (str n " " (i18n/tr ui "selected"))]
        (for [{attr :name :as m} shown]
          (filter-details ui client? m (get selected attr)))
        ;; a caveat about the control rather than part of it, which is
        ;; what <small> is for: these attributes are not on offer here
        (when (seq unlisted)
          [:p [:small (i18n/tr ui "Too many values to list: ")
               (interpose ", "
                          (map (fn [attr] [:code (name attr)]) unlisted))]])])])))

(defn query-example
  "The example query shown for `mode`, in `ui`: the two modes take
  different input, so one example cannot serve both."
  [ui mode]
  (if (= mode "cqp")
    (i18n/tr ui "[lemma = \"hund\"] or [pos = \"N.*\"]")
    (i18n/tr ui "hund, or several words in order")))

(defn navigation-status
  "The live region reporting a routed navigation in flight in `ui`,
  which says so while `pending?` and holds nothing otherwise.

  Rendered whether or not there is anything to say, because a live region
  announces a change to what it holds: one created already full has no
  change to announce, and several screen readers say nothing at all. A
  <div> rather than a <p>, so that the empty one costs no margins.

  It sits with the query controls rather than with the results, because
  the wait starts at the submit button and the results it is about may
  not exist yet. Above 64rem those controls are a sticky rail, so it
  stays in view while a page of hits is fetched too."
  [ui pending?]
  [:div.status {:role "status"}
   ;; TODO: design this. A line of text arriving under the form is what
   ;; it says, not a thing anyone drew, and three questions are open:
   ;; where a reader is actually looking when they are waiting (the foot
   ;; of the rail is where the button is, but not where the answer will
   ;; be); whether it wants a minimum time on screen, since an answer at
   ;; 450ms still flashes past `dk.cst.corpus-probe.ui/pending-delay-ms`;
   ;; and whether waiting should say more than that it is waiting. The
   ;; metadata filter has the same decision pending (see
   ;; `filter-fieldset`), and the two should be answered together.
   (when pending? [:p (i18n/tr ui "Loading …")])])

(defn search-form
  "The search form of `state`: over its `:folders` tree of corpus
  overviews and its metadata `:filter-controls`, prefilled from its
  `:params` (:corpus, a vector of selected names, :q :mode :ci :prefix
  :suffix), submitted as GET to `action`, with the page's own `extra`
  hidden inputs.

  The query input comes first, then everything that decides how it is
  read: the mode, and under it the options that only a simple query has.
  Then the scope of the search, the corpus chooser and the metadata
  filter, each behind one disclosure. So the field the reader reaches for
  is the first control in the form, whatever the registry holds, and what
  qualifies what they typed is under their hand rather than past two
  disclosures.

  The query example is the placeholder of the mode in `:params`, and the
  mode radios dispatch `:set-mode`, so choosing a mode swaps the example
  and disables the simple options without a round trip; without the
  client both are as the search was submitted.

  Wrapped in a <search> landmark; GET, so every search has a shareable URL
  and works without JavaScript. The form carries an id, so a control
  rendered outside it (the sort of the concordance, the grouping of the
  frequency table) still submits with it. The corpus chooser, the metadata
  filter and the query options are <fieldset> groups; inside the last, the
  mode and the options it governs are a named group each without a box of
  their own. No language is submitted with the search: which language the
  answer is worded in is the reader's own stored preference, not part of
  what they asked.

  Where the client runs, `navigation-status` follows the form inside the
  landmark, so a reader learns that their question is in flight beside
  the button they asked it with, and `:served-corpus`, the corpora this
  page was served for, decides which folders of the chooser start open
  while `:params` follows what the reader is choosing now."
  [{:keys [ui folders filter-controls params client? pending? served-corpus
           served-filter corpus-filter value-filter chooser-open?
           filters-open? filters-pending?]
    :as state}
   action extra]
  (let [{:keys [corpus q mode ci prefix suffix]} params]
    [:search
     [:form.search {:id form-id :method "get" :action action}
      extra
      ;; the button belongs against the field it submits, not at the foot
      ;; of every control that qualifies it. A field with a search button
      ;; beside it needs no visible label to say what it is, so the name
      ;; it keeps is the one only a screen reader reads
      [:p
       [:input {:id           "q"
                :name         "q"
                :type         "search"
                :aria-label   (i18n/tr ui "Query")
                :value        (or q "")
                :placeholder  (query-example ui mode)
                :autocomplete "off"
                :spellcheck   "false"}]
       " "
       [:button {:type "submit"} (i18n/trx ui "button" "Search")]]
      ;; one group: everything here qualifies the query above it, and two
      ;; boxes said that twice. A row each, so the mode a reader is in
      ;; does not run into the options it decides the meaning of.
      ;;
      ;; The options are disabled rather than taken away when they mean
      ;; nothing: a reader who looks at CQP and comes back finds what they
      ;; had ticked still ticked, the form does not change height under
      ;; them, and a disabled control is not submitted, so nothing about a
      ;; simple search rides along with a CQP one
      [:fieldset.query-options
       [:legend (i18n/tr ui "Query options")]
       ;; the radios are still a group of their own, and still named:
       ;; a fieldset is not the only thing that can say so
       [:p {:role "radiogroup" :aria-label (i18n/tr ui "Query mode")}
        [:label [:input {:type    "radio" :name "mode" :value "simple"
                         :checked (not= mode "cqp")
                         :on      {:change [:set-mode "simple"]}}]
         (i18n/tr ui "Simple")]
        " "
        [:label [:input {:type    "radio" :name "mode" :value "cqp"
                         :checked (= mode "cqp")
                         :on      {:change [:set-mode "cqp"]}}]
         "CQP"]]
       [:p {:role "group" :aria-label (i18n/tr ui "Simple-search options")}
        [:label [:input {:type "checkbox" :name "ci" :value "on"
                         :checked  (some? ci)
                         :disabled (= mode "cqp")}]
         (i18n/tr ui "ignore case")]
        " "
        [:label [:input {:type "checkbox" :name "prefix" :value "on"
                         :checked  (some? prefix)
                         :disabled (= mode "cqp")}]
         (i18n/tr ui "starts with")]
        " "
        [:label [:input {:type "checkbox" :name "suffix" :value "on"
                         :checked  (some? suffix)
                         :disabled (= mode "cqp")}]
         (i18n/tr ui "ends with")]]]
      ;; marks a selection the reader actually made: without it, unticking
      ;; every corpus and submitting is indistinguishable from arriving
      ;; with no corpus named, which searches them all
      [:input {:type "hidden" :name "scope" :value "chosen"}]
      (corpus-views/chooser ui folders
                            {:selected (set corpus)
                             :served   (set (or served-corpus corpus))
                             :client?  client?
                             :filter   corpus-filter
                             :open?    chooser-open?})
      (filter-fieldset ui filter-controls
                       {:served   served-filter
                        :open?    filters-open?
                        :pending? filters-pending?
                        :client?  client?
                        :filter   value-filter})]
     ;; only where the client runs: every other navigation is the
     ;; browser's own, and the browser reports those itself
     (when client? (navigation-status ui pending?))]))

(defn corpora-phrase
  "The corpus `names` in words in `ui`: the one name, or how
  many there were."
  [ui names]
  (if (= 1 (count names))
    (first names)
    (str (count names) " " (i18n/tr ui "corpora"))))

(defn hits-phrase
  "The number of hits `n` in words in `ui`."
  [ui n]
  (str (i18n/group-digits ui n) " "
       (i18n/trn ui "hit" "hits" n)))

(defn page-phrase
  "Where in a paged `result` the reader is, in `ui`."
  [ui {:keys [page pages] :as result}]
  (str (i18n/tr ui "page") " " (inc page) " " (i18n/tr ui "of") " " pages))

(defn sample-phrase
  "That a result holds a random `sample` of the matches rather than all
  of them, in `ui`, over `corpora`; nil when it holds them all.

  The number is the sample asked for rather than the hits it came back
  with, the two differing wherever a corpus had fewer matches than that,
  and it is named as being per corpus over several, one sample being
  drawn in each (see dk.cst.corpus-probe.search/concordance!)."
  [ui sample corpora]
  (when sample
    (str (i18n/tr ui "a random sample of at most") " "
         (i18n/group-digits ui sample)
         (when (next corpora) (str " " (i18n/tr ui "per corpus"))))))

(defn result-summary
  "The summary text of a concordance `result` page (`:size` hits over
  `:pages`, in the corpora that could be searched, within its metadata
  `:filter` and holding its random `:sample` of the matches), used as the
  heading naming the results region, in `ui`."
  [ui {:keys [size counts] :as result}]
  (let [searched (map :corpus (filter :size counts))]
    (str (hits-phrase ui size) " " (i18n/tr ui "in") " "
         (corpora-phrase ui searched)
         (within-phrase ui (:filter result))
         ;; a search that found nothing sampled nothing, and saying it
         ;; drew a sample of what it found reads as the reason it is empty
         (when (pos? size)
           (some->> (sample-phrase ui (:sample result) searched) (str ", ")))
         " · " (page-phrase ui result))))

(defn counts-table
  "The per-corpus hit `counts` of a search over several corpora as a table,
  each corpus linking to its info page; a corpus whose query failed shows
  no count (its error is reported separately). The wording is in `ui`."
  [ui counts]
  [:table.counts
   [:caption (i18n/tr ui "Hits per corpus")]
   [:thead
    [:tr [:th {:scope "col"} (i18n/tr ui "corpus")]
     ;; a column heading takes the plural form of what it counts
     [:th {:scope "col"} (i18n/trn ui "hit" "hits" 2)]]]
   [:tbody
    (for [{:keys [corpus size error]} counts]
      [:tr
       [:th {:scope "row"}
        [:a {:href (corpus-views/corpus-href ui corpus)} [:code corpus]]]
       [:td.n (if error
                [:em (i18n/tr ui "error")]
                (i18n/group-digits ui size))]])]])

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
  reader is), labelled in `ui`; nil when neither
  `prev-href` nor `next-href` is in range.

  A list, so assistive technology can say how many options there are, and
  an absent direction is left out rather than held open by an empty
  element, which nothing positioned. The links carry the `rel` values
  browsers and crawlers use for sequential pages."
  [ui prev-href next-href position]
  (when (or prev-href next-href)
    [:ul.row.pager
     (when prev-href
       [:li [:a {:href prev-href :rel "prev"}
             (str "← " (i18n/tr ui "previous"))]])
     [:li position]
     (when next-href
       [:li [:a {:href next-href :rel "next"}
             (str (i18n/tr ui "next") " →")]])]))

(defn pagination
  "`pager-links` as a navigation landmark named in `ui`.

  Only one of a result's two pagers is a landmark: the APG asks each
  landmark of a repeated role to carry a name of its own, and two named
  Pagination cannot be told apart. The repeat below the table is the bare
  list, whose links stay operable and stay in the tab order."
  [ui prev-href next-href position]
  (when-let [links (pager-links ui prev-href next-href position)]
    [:nav.pagination {:aria-label (i18n/tr ui "Pagination")} links]))

(defn error-heading
  "What the error of `type` is called, in `ui`: the types this project
  reports itself, and CQP's own error for anything else, which is headed
  as such and carries its message."
  [ui type]
  (case type
    :timeout        (i18n/tr ui "The query timed out")
    :no-corpus      (i18n/tr ui "No corpus selected")
    :unknown-corpus (i18n/tr ui "Unknown corpus")
    :rejected       (i18n/tr ui "Request rejected")
    :misaligned     (i18n/tr ui "CQP output could not be read")
    :internal       (i18n/tr ui "Unexpected error")
    (i18n/tr ui "CQP error")))

(defn error-explanation
  "What the error of `type` means, in `ui`, for the types that carry no
  message of their own; nil for the rest, whose message says it."
  [ui type]
  (case type
    :no-corpus      (i18n/tr ui "Select at least one corpus to search.")
    :unknown-corpus (i18n/tr ui "The registry has no corpus by that name.")
    :misaligned     (i18n/tr ui (str "CQP printed something other than the "
                                     "requested rows."))
    :internal       (i18n/tr ui (str "The search failed on the server; its "
                                     "log has the details."))
    nil))

(defn error-body
  "The parts of an `error` under its heading in `ui`: the
  `corpora` it concerns, the explanation of a type that carries no message,
  and cqp's own message verbatim, its `<--` position pointer included, as
  the sample output of another program."
  [ui {:keys [type message]} corpora]
  (list
   (when (seq corpora)
     [:p (str (i18n/tr ui "in") " ")
      (interpose ", " (map (fn [c] [:code c]) corpora))])
   (when-let [explanation (error-explanation ui type)]
     [:p explanation])
   ;; the stylesheet scrolls this rather than letting cqp's column-aligned
   ;; pointer reflow, and a scroll container a keyboard cannot reach is
   ;; unreadable in the browsers that do not focus scrollers themselves
   (when message [:pre {:tabindex "0"} [:samp message]])))

(defn error-name
  "The heading naming `error` in `ui`: the type this project reports
  itself, or cqp's own error."
  [ui {:keys [type] :as error}]
  (error-heading ui type))

(defn error-section
  "An `error` map under a heading of its own in `ui`, naming
  the `corpora` it concerns when given.

  No live region: every error here arrives by a full page reload, where a
  region that is already populated announces nothing, while the alert role
  costs the section its own semantics and flattens its heading. The reader
  reaches the error because the search lands on it (see `result-section`).

  An h3, since it sits inside a results region already headed by an h2."
  [ui error corpora]
  [:section.error
   [:h3 (error-name ui error)]
   (error-body ui error corpora)])

(defn download-links
  "Links downloading the current table in each format of `hrefs` (format
  keyword to URL), with `note` (when given) qualifying what the download
  holds; nil without hrefs, worded in `ui`. The response itself
  asks to be saved (its Content-Disposition), so the links carry no
  download attribute."
  [ui hrefs note]
  (when (seq hrefs)
    [:p.downloads (i18n/tr ui "Download")
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

(defn view-label
  "What the result view `k` is called, in `ui` (see
  dk.cst.corpus-probe.api/result-views)."
  [ui k]
  (case k
    :kwic        (i18n/tr ui "Concordance")
    :frequencies (i18n/tr ui "Frequencies")
    (name k)))

(defn view-switch
  "The switch between the views of one result in `ui`: each of
  `hrefs` ([view url], see dk.cst.corpus-probe.api/view-hrefs) as a
  link named by `view-label`, `view` marked as the one being shown; nil
  without hrefs.

  Links rather than an ARIA tablist: each view is its own URL and its own
  question put to CQP, so following one is a navigation, which is what a
  link means. A tablist would promise a panel that is already loaded."
  [ui view hrefs]
  (when (seq hrefs)
    [:nav.views {:aria-label (i18n/tr ui "Result view")}
     [:ul.row
      (for [[k href] hrefs]
        [:li [:a (cond-> {:href href}
                   (= k view) (assoc :aria-current "page"))
              (view-label ui k)]])]]))

(defn result-heading
  "The heading naming the results region in `ui`: the summary
  of the concordance `result` when any corpus could be searched, else the
  name of the error that came instead, so a search that failed everywhere
  is not announced as a count of nothing."
  [ui {:keys [counts] :as result} error]
  (if (searched? result)
    (result-summary ui result)
    (error-name ui (or error (some :error counts)))))

(defn results-region
  "The outcome of a search in `state` under `heading`, as a region named by
  that heading and focusable, so a GET search can land on it.

  Every view of a result shares this: the heading, the switch between the
  views, the error that replaced the result or the errors of individual
  corpora, and then `body`, the view's own content. The two views differ
  only in what they say about the same hits, so they differ only in what
  they pass here.

  Marked busy while a navigation is `pending?`, since until that one
  lands what this holds is the answer to the question before it."
  [{:keys [ui view view-hrefs result error pending?] :as state} heading body]
  [:section.result (cond-> {:id              results-id
                            :tabindex        "-1"
                            :aria-labelledby "results-heading"}
                     ;; while the next question is in flight these hits
                     ;; are still the previous one's answer, and nothing
                     ;; about them says so
                     pending? (assoc :aria-busy "true"))
   [:h2 {:id "results-heading"} heading]
   (view-switch ui view view-hrefs)
   (when error (error-body ui error nil))
   (for [[e corpora] (error-groups (:counts result))]
     (error-section ui e corpora))
   body])

(defn result-section
  "The concordance view of the search in `state`: when any corpus could be
  searched and found something, the sort and sample controls, the
  pagination above and below the table, the concordance with its
  `:expanded` hits and `:langs`, then the per-corpus counts as an aside
  and the download links (`:export-hrefs`, exports holding at most
  `:export-limit` hits), all worded in the state's `:ui` and wrapped in
  the shared `results-region`."
  [{:keys [ui sort-modes params result error langs expanded client?
           export-hrefs export-limit prev-href next-href]
    :as state}]
  (let [{:keys [counts hits size]} result
        position (when result (page-phrase ui result))]
    (results-region
     state
     (result-heading ui result error)
     (when (searched? result)
       ;; a search that found nothing has nothing to page, download or
       ;; count: the table would be a header over no rows and the exports
       ;; header-only files
       (if (zero? size)
         [:p (i18n/tr ui "No hits.")]
         (list
          (view-controls ui client?
                         (list (sort-control ui sort-modes (:sort params))
                               " "
                               (sample-control ui (:sample result))))
          (pagination ui prev-href next-href position)
          (kwic/concordance hits {:caption  (i18n/tr ui "Concordance")
                                  :ui       ui
                                  :langs    langs
                                  :expanded expanded
                                  :client?  client?
                                  :cursor   (:cursor state)})
          (pager-links ui prev-href next-href position)
          ;; an <aside>, after the hits rather than before them: it is
          ;; about the answer rather than part of it, and where the hits
          ;; came from is a question a reader has once they have read
          ;; them. After the page links too, which belong against the
          ;; foot of the table they turn, where a reader who has finished
          ;; reading reaches. Unnamed, so that inside the results
          ;; <section> it stays a container rather than becoming a second
          ;; complementary landmark: the table's own caption names it
          (when (next counts) [:aside (counts-table ui counts)])
          ;; what to do next with these hits, so it follows them: reading
          ;; the concordance is the task, taking it elsewhere is the one
          ;; after
          (download-links ui export-hrefs
                          (when (and export-limit (> size export-limit))
                            (str (i18n/tr ui "the first") " "
                                 (i18n/group-digits ui export-limit) " "
                                 (i18n/trn ui "hit" "hits"
                                           export-limit))))))))))

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
  `ui`, from `selected` (its :token, :structs and :corpus);
  nil while nothing is selected.

  Above the rail's breakpoint it takes the query column, so it sits beside
  the hits it describes without narrowing them; below it, it is a sheet at
  the foot of the viewport.

  It does not take focus: the cursor stays on the token so the arrow keys
  keep moving, and the panel describes whatever the cursor is on. That is
  why it is not a popover, which would put itself in the top layer, out of
  the grid, and want focus of its own. Escape closes it from the
  concordance.

  The group titles are in `ui`; the attribute names inside them are the
  corpus's own."
  [ui {:keys [token structs corpus] :as selected}]
  (when selected
    [:aside.sidebar {:aria-label (i18n/tr ui "Token details")}
     [:h2 (i18n/tr ui "Token details")]
     [:button {:type "button" :on {:click [:close]}} (i18n/tr ui "Close")]
     (detail-group (i18n/tr ui "Token") (dissoc token :open :close))
     (detail-group (i18n/tr ui "Text") structs)
     (when corpus
       [:section
        [:h3 (i18n/tr ui "Corpus")]
        [:p [:a {:href (corpus-views/corpus-href ui corpus)}
             [:code corpus]]]])]))
