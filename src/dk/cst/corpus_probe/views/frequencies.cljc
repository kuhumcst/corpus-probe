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
  corpus has hundreds of thousands, and the caption says how many were
  cut."
  500)

(defn attr-option
  "One attribute description `m` as an option, selected when its name is
  `selected` (a string)."
  [selected {attr :name :as m}]
  [:option {:value (name attr) :selected (= (name attr) selected)} (name attr)])

(defn attr-control
  "The grouping control in language `lang`: a select over the attribute
  descriptions `attrs` with `selected` (a string) chosen, the positional
  and the structural attributes in their own option groups, under CWB's
  names for them."
  [lang attrs selected]
  (let [{p :positional s :structural} (group-by :type attrs)]
    [:p
     [:label {:for "attr"} (i18n/tr lang :group-by)]
     [:select {:id "attr" :name "attr"}
      (when (seq p)
        [:optgroup {:label (i18n/tr lang :p-attrs)}
         (map (partial attr-option selected) p)])
      (when (seq s)
        [:optgroup {:label (i18n/tr lang :s-attrs)}
         (map (partial attr-option selected) s)])]]))

(defn frequency-summary
  "The caption of the table of frequency `result` showing `shown` of its
  rows: what was counted, in the corpora that could be counted and within
  the metadata filter, by what, and how many values there are, in language
  `lang`."
  [lang {:keys [query attr counts rows] :as result} shown]
  (let [n        (count rows)
        readable (filter :tokens counts)]
    (str (if (str/blank? query)
           (i18n/tr lang :all-tokens)
           (page/hits-phrase lang (reduce + (keep :size readable))))
         " " (i18n/tr lang :in) " "
         (page/corpora-phrase lang (map :corpus readable))
         (page/within-phrase lang (:filter result))
         " " (i18n/tr lang :by) " " (name attr)
         " · " (i18n/group-digits lang n) " "
         (i18n/tr lang (if (= 1 n) :value :values))
         (when (< shown n)
           (str ", " (i18n/tr lang :the) " " (i18n/group-digits lang shown)
                " " (i18n/tr lang :most-frequent))))))

(defn frequency-cells
  "The two cells of a frequency `n` in a corpus of `tokens`, in language
  `lang`: the count and its rate per million tokens."
  [lang n tokens]
  (list [:td.n (i18n/group-digits lang n)]
        [:td.n (i18n/group-digits lang (stats/per-million n tokens))]))

(defn frequency-table
  "The merged frequency `result` as a table: a row per value (the
  `row-limit` most frequent), a column group per readable corpus (its
  frequency and the rate per million tokens) and, over several corpora, a
  total group. The counts are headed frequency, CWB's own word for what
  `group` and cwb-lexdecode report; the headings are in language `lang`."
  [lang {:keys [attr counts rows] :as result}]
  (let [readable (filter :tokens counts)
        total?   (boolean (next readable))
        tokens   (reduce + (map :tokens readable))
        shown    (take row-limit rows)
        groups   (cond-> readable total? (concat [:total]))]
    [:table.frequencies
     [:caption (frequency-summary lang result (count shown))]
     [:colgroup]
     (for [_ groups] [:colgroup {:span 2}])
     [:thead
      [:tr
       [:th {:scope "col" :rowspan 2} [:code (name attr)]]
       (for [{:keys [corpus]} readable]
         [:th {:scope "colgroup" :colspan 2}
          [:a {:href (corpus-views/corpus-href lang corpus)}
           [:code corpus]]])
       (when total?
         [:th {:scope "colgroup" :colspan 2} (i18n/tr lang :total)])]
      [:tr
       (for [_ groups]
         (list [:th {:scope "col"} (i18n/tr lang :frequency)]
               [:th {:scope "col"} (i18n/tr lang :per-million)]))]]
     [:tbody
      (for [{:keys [value freqs total]} shown]
        [:tr
         [:th {:scope "row"} (page/attribute-value attr value)]
         (for [{:keys [corpus tokens]} readable]
           (frequency-cells lang (get freqs corpus 0) tokens))
         (when total? (frequency-cells lang total tokens))])]]))

(defn frequency-section
  "The frequency `:result` in `state`: an alert per distinct error among
  its corpora, then, when any corpus could be counted, a link to the
  concordance of the same hits (`:kwic-href`, absent for a whole-corpus
  table), the download links (`:export-hrefs`, exports holding every row)
  and the table, in the state's `:lang`."
  [{:keys [lang result kwic-href export-hrefs] :as state}]
  [:section.result {:aria-label (i18n/tr lang :frequencies)}
   (for [[error corpora] (page/error-groups (:counts result))]
     (page/error-section lang error corpora))
   (when (some :tokens (:counts result))
     (list
      (when kwic-href
        [:p [:a {:href kwic-href} (i18n/tr lang :this-concordance)]])
      (page/download-links lang export-hrefs
                           (when (< row-limit (count (:rows result)))
                             (i18n/tr lang :all-values)))
      (frequency-table lang result)))])

(defn frequencies-view
  "The frequency page's main content from `state`: the search form (see
  dk.cst.corpus-probe.views.page/search-form) with the grouping control
  over `:attrs`, then either the `:error`, the frequency `:result` (see
  `frequency-section`) or nothing, all in the state's `:lang`."
  [{:keys [lang params attrs result error] :as state}]
  [:main
   (page/search-form state "/frequencies"
                     (attr-control lang attrs (:attr params)))
   (cond
     error  (page/error-section lang error nil)
     result (frequency-section state)
     :else  nil)])
