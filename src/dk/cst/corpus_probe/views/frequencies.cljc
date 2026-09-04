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

(defn frequency-summary
  "The summary of frequency `result` showing `shown` of its rows, used as
  the heading naming the results region: what was counted, in the corpora
  that could be counted and within the metadata filter, by what, and how
  many values there are, in `ui`."
  [ui {:keys [query attr counts rows] :as result} shown]
  (let [n        (count rows)
        readable (filter :tokens counts)]
    (str (if (str/blank? query)
           (i18n/tr ui "All tokens")
           (page/hits-phrase ui (reduce + (keep :size readable))))
         " " (i18n/tr ui "in") " "
         (page/corpora-phrase ui (map :corpus readable))
         (page/within-phrase ui (:filter result))
         " " (i18n/tr ui "by") " " (name attr)
         " · " (i18n/group-digits ui n) " "
         (i18n/trn ui "value" "values" n)
         (when (< shown n)
           (str ", " (i18n/tr ui "the") " " (i18n/group-digits ui shown)
                " " (i18n/tr ui "most frequent shown"))))))

(defn frequency-cells
  "The two cells of a frequency `n` in a corpus of `tokens`, in `ui`:
  the count and its rate per million tokens."
  [ui n tokens]
  (list [:td.n (i18n/group-digits ui n)]
        [:td.n (i18n/group-digits ui (stats/per-million n tokens))]))

(defn frequency-table
  "The merged frequency `result` as a table: a row per value (the
  `row-limit` most frequent), a column group per readable corpus (its
  frequency and the rate per million tokens) and, over several corpora, a
  total group. The counts are headed frequency, CWB's own word for what
  `group` and cwb-lexdecode report; the headings are in `ui`."
  [ui {:keys [attr counts rows] :as result}]
  (let [readable (filter :tokens counts)
        total?   (boolean (next readable))
        tokens   (reduce + (map :tokens readable))
        shown    (take row-limit rows)
        groups   (cond-> readable total? (concat [:total]))]
    [:table.frequencies
     [:caption (i18n/tr ui "Frequencies")]
     [:colgroup]
     (for [_ groups] [:colgroup {:span 2}])
     [:thead
      [:tr
       [:th {:scope "col" :rowspan 2} [:code (name attr)]]
       (for [{:keys [corpus]} readable]
         [:th {:scope "colgroup" :colspan 2}
          [:a {:href (corpus-views/corpus-href ui corpus)}
           [:code corpus]]])
       (when total?
         [:th {:scope "colgroup" :colspan 2} (i18n/tr ui "total")])]
      [:tr
       (for [_ groups]
         (list [:th {:scope "col"} (i18n/tr ui "frequency")]
               [:th {:scope "col"} (i18n/tr ui "per million")]))]]
     [:tbody
      (for [{:keys [value freqs total]} shown]
        [:tr
         [:th {:scope "row"} (page/attribute-value attr value)]
         (for [{:keys [corpus tokens]} readable]
           (frequency-cells ui (get freqs corpus 0) tokens))
         (when total? (frequency-cells ui total tokens))])]]))

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

  Holds, when any corpus could be counted, the grouping control and the
  table, then the download links (`:export-hrefs`, exports holding every
  row), in the state's `:ui`, wrapped in the shared
  dk.cst.corpus-probe.views.page/results-region."
  [{:keys [ui attrs params result error export-hrefs client?] :as state}]
  (let [shown (min row-limit (count (:rows result)))]
    (page/results-region
     state
     (frequency-heading ui result error shown)
     (when (tabled? result)
       (list
        (page/view-controls ui client?
                            (attr-control ui attrs (:attr params)))
        (frequency-table ui result)
        ;; what to do next with the table, so it follows the table
        (page/download-links ui export-hrefs
                             (when (< row-limit (count (:rows result)))
                               (i18n/tr ui "all values"))))))))
