(ns dk.cst.corpus-probe.views.frequencies
  "Hiccup for the frequency page: the search form with a grouping control,
  and the breakdown of the hits (or of the whole corpora) by one attribute,
  merged over the selected corpora into one table.

  The table keeps the columns CQP's `group` prints (value and frequency)
  per corpus and adds the computed relative frequencies and totals
  (PLAN.md §7), or, counted against a second attribute, has a column per
  value of it; the value cells render like the sidebar's attribute
  values, so a text title is a <cite> and a year a <time>."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.stats :as stats]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.layout :as layout]
            [dk.cst.corpus-probe.views.page :as page]))

(def row-limit
  "The most values a frequency table shows; a breakdown by word of a large
  corpus has hundreds of thousands, and the heading says how many were
  cut."
  500)

(defn attr-option
  "One attribute description `m` as an option, selected when its name is
  `selected` (a string)."
  [selected {attr :name :as m}]
  [:option {:value (name attr) :selected (= (name attr) selected)} (name attr)])

(defn attr-control
  "The grouping control in `ui`: a select over the attribute
  descriptions `attrs` with `selected` (a string) chosen, the positional
  and the structural attributes in their own option groups, under CWB's
  names for them.

  It names the form it submits with, so it can sit beside the table it
  regroups rather than inside the query form: regrouping a result is a
  different task from writing the query that produced it."
  [ui attrs selected]
  (let [{p :positional s :structural} (group-by :type attrs)]
    (list
     [:label {:for "attr"} (i18n/tr ui "Group by")]
     " "
     [:select {:id   "attr" :name "attr" :form page/form-id
               :on   {:change [:apply-view]}}
      (when (seq p)
        [:optgroup {:label (i18n/tr ui "positional attributes")}
         (map (partial attr-option selected) p)])
      (when (seq s)
        [:optgroup {:label (i18n/tr ui "structural attributes")}
         (map (partial attr-option selected) s)])])))

(defn position-control
  "The control choosing where in the match the table counts, in `ui`: a
  select over the `positions` (see dk.cst.corpus-probe.query/positions),
  each named by dk.cst.corpus-probe.views.page/position-label, with `at`
  chosen. It follows the attribute it qualifies and names the form it
  submits with, as `attr-control` does."
  [ui positions at]
  [:select {:id         "at" :name "at" :form page/form-id
            :aria-label (i18n/tr ui "Position")
            :on         {:change [:apply-view]}}
   (for [position positions]
     [:option {:value position :selected (= position at)}
      (page/position-label ui position)])])

(defn by-control
  "The control choosing what the columns of the table are, in `ui`: the
  corpora counted, one group of columns each, or the values of one of
  the attribute descriptions `attrs`, the corpora then summed, `by` (a
  keyword; nil for the corpora) chosen. The structural attributes come
  first, a breakdown over the years or the authors being what this is
  mostly for. It follows the position it qualifies and names the form it
  submits with, as `attr-control` does."
  [ui attrs by]
  (let [{p :positional s :structural} (group-by :type attrs)
        selected (some-> by name)]
    (list
     [:label {:for "by"} (i18n/tr ui "columns")]
     " "
     [:select {:id   "by" :name "by" :form page/form-id
               :on   {:change [:apply-view]}}
      [:option {:value "" :selected (nil? by)} (i18n/tr ui "corpora")]
      (when (seq s)
        [:optgroup {:label (i18n/tr ui "structural attributes")}
         (map (partial attr-option selected) s)])
      (when (seq p)
        [:optgroup {:label (i18n/tr ui "positional attributes")}
         (map (partial attr-option selected) p)])])))

(defn shown-phrase
  "That `shown` of `n` things, called `things` (the word for that many
  of them), are shown, in `ui`: \"120 values, the 100 most frequent
  shown\", or just the count when they all are."
  [ui things n shown]
  (str (i18n/group-digits ui n) " " things
       (when (< shown n)
         (str ", " (i18n/tr ui "the") " " (i18n/group-digits ui shown)
              " " (i18n/tr ui "most frequent shown")))))

(defn grouping-phrase
  "How the frequency `result` counted, as a phrase in `ui`: by which
  attribute, where in the match, and against which second attribute when
  it is a cross-tabulation. The first of the phrases under the heading
  (see dk.cst.corpus-probe.views.page/qualifiers), being what this view
  asks that the concordance does not."
  [ui {:keys [attr at by]}]
  (list (i18n/tr ui "by") " " [:code (name attr)]
        (when at (str " " (page/position-label ui at)))
        (when by (list " " (i18n/tr ui "and") " " [:code (name by)]))))

(defn table-caption
  "The caption of the table of frequency `result`, in `ui`: its name,
  how many values it holds and shows (see `shown-phrase`, `row-limit`),
  the columns likewise when it is counted `:by` a second attribute, and
  what the parentheses hold when those columns are `:sized`. The counts
  are here rather than in the heading: they are the table's size, which
  is what a caption says of a table."
  [ui {:keys [rows by columns column-count sized]}]
  (let [n (count rows)]
    [:caption (i18n/tr ui "Frequencies") " · "
     (shown-phrase ui (i18n/trn ui "value" "values" n) n (min row-limit n))
     (when by
       (str " · " (shown-phrase ui (i18n/trn ui "column" "columns"
                                             column-count)
                                column-count (count columns))))
     (when (and by sized)
       (str " · " (i18n/tr ui (str "the rate per million tokens of the "
                                   "column in parentheses"))))]))

(defn docs-control
  "The control asking for the texts each value occurs in to be counted
  beside its frequency, in `ui`, `docs?` saying whether they are. It
  names the form it submits with and applies itself, as the grouping
  does."
  [ui docs?]
  [:label
   [:input {:type    "checkbox" :name "docs" :value "on"
            :checked docs?
            :form    page/form-id
            :on      {:change [:apply-view]}}]
   " " (i18n/tr ui "count texts")])

(defn frequency-cells
  "The cells of a frequency `n` against `tokens` tokens, in `ui`: the
  count and its rate per million of them, then the tokens themselves
  where the table is `sized` (they are then the text of the value rather
  than the corpus, and differ from row to row), then the number of
  texts `docs` when the table counts them (a number, nil otherwise)."
  [ui n tokens sized docs]
  (list [:td.n (i18n/group-digits ui n)]
        [:td.n (i18n/group-digits ui (stats/per-million n tokens) 1)]
        (when sized [:td.n (i18n/group-digits ui tokens)])
        (when docs [:td.n (i18n/group-digits ui docs)])))

(defn value-cell
  "The value `value` of attribute `attr` as the header of its row,
  linking to the hits it counted where the row has an `href`."
  [attr value href]
  [:th {:scope "row"}
   (let [cell (page/attribute-value attr value)]
     (if href [:a {:href href} cell] cell))])

(defn frequency-table
  "The merged frequency `result` as a table: a row per value (the
  `row-limit` most frequent, each linking to the hits it counted where
  the row carries an `:href`), a column group per readable corpus (its
  frequency, the rate per million tokens, those tokens when the result
  is `:sized`, since they are then the text of the value rather than of
  the corpus, and, when it counts `:docs`, the texts it occurs in) and,
  over several corpora, a total group. The counts are headed frequency,
  CWB's own word for what `group` and cwb-lexdecode report; the headings
  are in `ui`."
  [ui {:keys [attr counts rows docs sized] :as result}]
  (let [readable (filter :tokens counts)
        total?   (boolean (next readable))
        tokens   (reduce + (map :tokens readable))
        shown    (take row-limit rows)
        groups   (cond-> readable total? (concat [:total]))
        span     (cond-> 2 sized inc docs inc)]
    [:table.frequencies
     (table-caption ui result)
     [:colgroup]
     (for [_ groups] [:colgroup {:span span}])
     [:thead
      [:tr
       [:th {:scope "col" :rowspan 2} [:code (name attr)]]
       (for [{:keys [corpus]} readable]
         [:th {:scope "colgroup" :colspan span}
          [:a {:href (url/corpus corpus)}
           [:code corpus]]])
       (when total?
         [:th {:scope "colgroup" :colspan span} (i18n/tr ui "total")])]
      [:tr
       (for [_ groups]
         (list [:th {:scope "col"} (layout/term ui :frequency)]
               [:th {:scope "col"} (layout/term ui :per-million)]
               (when sized [:th {:scope "col"} (i18n/tr ui "tokens")])
               (when docs [:th {:scope "col"} (i18n/tr ui "texts")])))]]
     [:tbody
      (for [{:keys [value freqs total href]
             doc-freqs :docs row-tokens :tokens} shown]
        [:tr
         (value-cell attr value href)
         (for [{:keys [corpus tokens]} readable]
           (frequency-cells ui (get freqs corpus 0)
                            (if sized (get row-tokens corpus 0) tokens)
                            sized
                            (when docs (get doc-freqs corpus 0))))
         (when total?
           (frequency-cells ui total
                            (if sized (reduce + (vals row-tokens)) tokens)
                            sized
                            (when docs (reduce + (vals doc-freqs)))))])]]))

(defn crosstab-table
  "The cross-tabulated frequency `result` (see
  dk.cst.corpus-probe.frequency/frequency-table! under :by) as a table:
  a row per value of the attribute counted (the `row-limit` most
  frequent, linked as `frequency-table` links them), a column per value
  of the attribute it was counted against (the `:columns`) and a total
  column, the corpora summed. Where the result is `:sized`, the tokens
  each column measures against head the rows, and each cell gives the
  rate per million of them after the count, in parentheses, as KORP's
  statistics do; a count of nothing has no rate. The headings are in
  `ui`.

  Inside the region that scrolls it: a column per year is wider than the
  page, which must not scroll sideways with it."
  [ui {:keys [attr by counts columns rows sized] :as result}]
  (let [tokens (reduce + (map :tokens (filter :tokens counts)))
        cell   (fn [n t]
                 (let [rate (when (and sized t (pos? n))
                              (stats/per-million n t))]
                   [:td.n (i18n/group-digits ui n)
                    (when rate
                      (str " (" (i18n/group-digits ui rate 1) ")"))]))]
    [:div.scroll
     [:table.frequencies.crosstab
      (table-caption ui result)
      [:thead
       [:tr
        [:th {:scope "col"} [:code (name attr)]]
        (for [{:keys [value]} columns]
          [:th {:scope "col"} (page/attribute-value by value)])
        [:th {:scope "col"} (i18n/tr ui "total")]]]
      [:tbody
       (when sized
         [:tr
          [:th {:scope "row"} (i18n/tr ui "tokens")]
          (for [{col-tokens :tokens} columns]
            [:td.n (i18n/group-digits ui col-tokens)])
          [:td.n (i18n/group-digits ui tokens)]])
       (for [{:keys [value cells total href]} (take row-limit rows)]
         [:tr
          (value-cell attr value href)
          (for [{col :value col-tokens :tokens} columns]
            (cell (get cells col 0) col-tokens))
          (cell total tokens)])]]]))

(defn tabled?
  "True when any corpus of frequency `result` could be counted, so its
  rows are an answer rather than a report of failure."
  [{:keys [counts] :as result}]
  (boolean (some :tokens counts)))

(defn frequency-heading
  "The heading naming the results region in `ui`: what the search
  `params` describe found (see dk.cst.corpus-probe.views.page/hits-heading),
  counted over the corpora of the frequency `result` that could be
  counted, else the name of the `error` that came instead."
  [ui params {:keys [counts] :as result} error]
  (if (tabled? result)
    (page/hits-heading ui params
                       (reduce + (keep :size (filter :tokens counts))))
    (page/error-name ui (or error (some :error counts)))))

(defn frequency-section
  "The frequency view of the search in `state`.

  Holds, when any corpus could be counted, the grouping, position and
  column controls, with the text count where the columns are the corpora
  (a cross-tabulation counts no texts), the near control behind their
  disclosure (see dk.cst.corpus-probe.views.page/view-controls) and the
  table, cross-tabulated when the result is counted `:by` a second
  attribute, then the download links (`:export-hrefs`, exports holding
  every row), in the state's `:ui`, wrapped in the shared
  dk.cst.corpus-probe.views.page/results-region."
  [{:keys [ui attrs positions params result error export-hrefs client?]
    :as   state}]
  (let [tabled (tabled? result)]
    (page/results-region
     state
     (frequency-heading ui params result error)
     ;; how the table counted comes first: it is what this view asks
     ;; that the concordance of the same hits does not
     (cond->> (page/qualifiers ui params result)
       tabled (cons (grouping-phrase ui result)))
     (when tabled
       (list
        (page/view-controls ui client?
                            (list (attr-control ui attrs (:attr params))
                                  " "
                                  (position-control ui positions (:at result))
                                  " "
                                  (by-control ui attrs (:by result))
                                  (when-not (:by result)
                                    (list " "
                                          (docs-control ui (:docs result)))))
                            (page/near-control ui (:near result))
                            (:near result))
        (if (:by result)
          (crosstab-table ui result)
          (frequency-table ui result))
        ;; what to do next with the table, so it follows the table
        (page/download-links ui export-hrefs
                             (when (< row-limit (count (:rows result)))
                               (i18n/tr ui "all values"))))))))
