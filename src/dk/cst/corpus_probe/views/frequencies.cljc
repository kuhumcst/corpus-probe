(ns dk.cst.corpus-probe.views.frequencies
  "Hiccup for the frequency page: the search form with a grouping control,
  and the breakdown of the hits (or of the whole corpora) by one attribute,
  merged over the selected corpora into one table.

  The table keeps the columns CQP's `group` prints (value and frequency)
  per corpus and adds the computed relative frequencies and totals
  (PLAN.md §7); the value cells render like the sidebar's attribute values,
  so a text title is a <cite> and a year a <time>."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.stats :as stats]
            [dk.cst.corpus-probe.views.corpus :as corpus-views]
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

(defn frequency-summary
  "The summary of frequency `result` showing `shown` of its rows, used as
  the heading naming the results region: what was counted, in the corpora
  that could be counted, within the metadata filter, narrowed and near a
  word, by what and where in the match, and how many values there are,
  in `ui`."
  [ui {:keys [query attr at counts rows] :as result} shown]
  (let [n        (count rows)
        readable (filter :tokens counts)]
    (str (if (str/blank? query)
           (i18n/tr ui "All tokens")
           (page/hits-phrase ui (reduce + (keep :size readable))))
         " " (i18n/tr ui "in") " "
         (page/corpora-phrase ui (map :corpus readable))
         (page/within-phrase ui (:filter result) (:patterns result))
         (page/subset-phrase ui (:subset result))
         (page/near-phrase ui (:near result))
         " " (i18n/tr ui "by") " " (name attr)
         (when at (str " " (page/position-label ui at)))
         " · " (i18n/group-digits ui n) " "
         (i18n/trn ui "value" "values" n)
         (when (< shown n)
           (str ", " (i18n/tr ui "the") " " (i18n/group-digits ui shown)
                " " (i18n/tr ui "most frequent shown"))))))

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
  "The cells of a frequency `n` in a corpus of `tokens`, in `ui`: the
  count and its rate per million tokens, then the number of texts `docs`
  when the table counts them (a number, nil otherwise)."
  [ui n tokens docs]
  (list [:td.n (i18n/group-digits ui n)]
        [:td.n (i18n/group-digits ui (stats/per-million n tokens))]
        (when docs [:td.n (i18n/group-digits ui docs)])))

(defn frequency-table
  "The merged frequency `result` as a table: a row per value (the
  `row-limit` most frequent, each linking to the hits it counted where
  the row carries an `:href`), a column group per readable corpus (its
  frequency, the rate per million tokens and, when the result counts
  `:docs`, the texts it occurs in) and, over several corpora, a total
  group. The counts are headed frequency, CWB's own word for what
  `group` and cwb-lexdecode report; the headings are in `ui`."
  [ui {:keys [attr counts rows docs] :as result}]
  (let [readable (filter :tokens counts)
        total?   (boolean (next readable))
        tokens   (reduce + (map :tokens readable))
        shown    (take row-limit rows)
        groups   (cond-> readable total? (concat [:total]))
        span     (if docs 3 2)]
    [:table.frequencies
     [:caption (i18n/tr ui "Frequencies")]
     [:colgroup]
     (for [_ groups] [:colgroup {:span span}])
     [:thead
      [:tr
       [:th {:scope "col" :rowspan 2} [:code (name attr)]]
       (for [{:keys [corpus]} readable]
         [:th {:scope "colgroup" :colspan span}
          [:a {:href (corpus-views/corpus-href ui corpus)}
           [:code corpus]]])
       (when total?
         [:th {:scope "colgroup" :colspan span} (i18n/tr ui "total")])]
      [:tr
       (for [_ groups]
         (list [:th {:scope "col"} (i18n/tr ui "frequency")]
               [:th {:scope "col"} (i18n/tr ui "per million")]
               (when docs [:th {:scope "col"} (i18n/tr ui "texts")])))]]
     [:tbody
      (for [{:keys [value freqs total href] doc-freqs :docs} shown]
        [:tr
         ;; the value links to the hits it counted, where the table has a
         ;; link for it
         [:th {:scope "row"}
          (let [cell (page/attribute-value attr value)]
            (if href [:a {:href href} cell] cell))]
         (for [{:keys [corpus tokens]} readable]
           (frequency-cells ui (get freqs corpus 0) tokens
                            (when docs (get doc-freqs corpus 0))))
         (when total?
           (frequency-cells ui total tokens
                            (when docs (reduce + (vals doc-freqs)))))])]]))

(defn tabled?
  "True when any corpus of frequency `result` could be counted, so its
  rows are an answer rather than a report of failure."
  [{:keys [counts] :as result}]
  (boolean (some :tokens counts)))

(defn frequency-heading
  "The heading naming the results region in `ui`: the summary
  of the frequency `result` showing `shown` of its rows when any corpus
  could be counted, else the name of the `error` that came instead."
  [ui {:keys [counts] :as result} error shown]
  (if (tabled? result)
    (frequency-summary ui result shown)
    (page/error-name ui (or error (some :error counts)))))

(defn frequency-section
  "The frequency view of the search in `state`.

  Holds, when any corpus could be counted, the grouping, position and
  text count controls with the near control behind their disclosure (see
  dk.cst.corpus-probe.views.page/view-controls) and the table, then the
  download links (`:export-hrefs`, exports holding every row), in the
  state's `:ui`, wrapped in the shared
  dk.cst.corpus-probe.views.page/results-region."
  [{:keys [ui attrs positions params result error export-hrefs client?]
    :as   state}]
  (let [shown (min row-limit (count (:rows result)))]
    (page/results-region
     state
     (frequency-heading ui result error shown)
     (when (tabled? result)
       (list
        (page/view-controls ui client?
                            (list (attr-control ui attrs (:attr params))
                                  " "
                                  (position-control ui positions (:at result))
                                  " "
                                  (docs-control ui (:docs result)))
                            (page/near-control ui (:near result))
                            (:near result))
        (frequency-table ui result)
        ;; what to do next with the table, so it follows the table
        (page/download-links ui export-hrefs
                             (when (< row-limit (count (:rows result)))
                               (i18n/tr ui "all values"))))))))
