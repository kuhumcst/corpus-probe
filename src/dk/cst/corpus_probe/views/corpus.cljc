(ns dk.cst.corpus-probe.views.corpus
  "Hiccup for the corpus index, the corpus chooser of the search form and
  the per-corpus info pages.

  The index and the chooser share one folder-grouped tree over the
  registry. The chooser is a control, the same one the metadata filter
  is (see dk.cst.corpus-probe.views.tree): each folder is a <details>
  disclosure, so the tree collapses without any script, and each corpus
  a checkbox. The index is a document: a heading per folder and a list
  per folder's corpora, each a link to its info page. Both give the
  token count as a machine-readable <data>. The info page maps the facts
  CWB itself reports (the registry entry, `info;` and
  `cwb-describe-corpus -s`) onto a definition list and per-attribute
  statistics tables, following PLAN.md §7."
  (:require [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.controls :as controls]
            [dk.cst.corpus-probe.views.layout :as layout]
            [dk.cst.corpus-probe.views.tree :as tree]))

(defn count-cell
  "A statistics table cell for count `n` in `ui`, or for the
  tool's NO DATA when the count is nil because an attribute's data files
  cannot be read."
  [ui n]
  [:td.n (if n (i18n/group-digits ui n) [:em (i18n/tr ui "no data")])])

(defn token-count
  "The token count `n` as a <data> element, in `ui`: grouped
  digits for people, the plain number in `value` for machines."
  [ui n]
  [:data.size {:value (str n)}
   (str (i18n/group-digits ui n) " " (i18n/tr ui "tokens"))])

(defn corpus-details
  "The details following a corpus's name in a tree entry for overview map
  `m`: its ID when the name shown is a title, and its token count in
  `ui`, or a mark that the corpus cannot be read."
  [ui {:keys [id title size] :as m}]
  (list (when title (list [:code id] " "))
        (if size
          (token-count ui size)
          [:em (i18n/tr ui "unavailable")])))

(defn corpus-item
  "One corpus overview map `m` as an index entry: a link to its info page
  named by its title (falling back to its ID), then its details in
  `ui`."
  [ui {:keys [id title] :as m}]
  [:li [:a {:href (url/corpus id)} (or title id)] " "
   (corpus-details ui m)])

(defn chooser-item
  "One corpus overview map `m` as a chooser entry: a checkbox labelled by
  its title (falling back to its ID) and its details, checked when its ID
  is in the set `selected`, its details in `ui`. A corpus that
  cannot be read is disabled.

  The box reports every change, so that the selection the application
  state holds is the one the reader can see. A search still submits the
  form's own controls rather than that state, but the folder toggles and
  the counts in the summaries are computed from it, and a count that
  disagreed with the boxes under it would be worse than no count."
  [ui selected {:keys [id title size hidden?] :as m}]
  [:li (cond-> {} hidden? (assoc :hidden true))
   [:label
    [:input {:type     "checkbox"
             :name     "corpus"
             :value    id
             :checked  (contains? selected id)
             :disabled (nil? size)
             :on       {:change [:toggle-corpora [id]]}}]
    " " (or title id) " " (corpus-details ui m)]])

(defn folder-corpora
  "Every corpus overview in resolved `folder`, subfolders included."
  [{:keys [corpora folders] :as folder}]
  (concat corpora (mapcat folder-corpora folders)))

(defn labelled-folders
  "Label the label-less tail folder among `folders` \"Other\", in `ui`,
  when it has labelled siblings.

  Otherwise the ungrouped corpora could be mistaken for part of the
  disclosure above them; a lone label-less folder (no grouping configured
  at all) stays a bare list."
  [ui folders]
  (cond->> folders
    (next folders) (map (fn [f]
                          (update f :label #(or % (i18n/tr ui "Other")))))))

(defn corpus-toggle
  "dk.cst.corpus-probe.views.controls/select-all over the corpus `ids`,
  called `label`, with the set of `selected` IDs and the select-all's own
  `opts`.

  It precedes the disclosure rather than sitting in the <summary>, so a
  whole folder can be included or excluded without opening it, and so it
  is a control in its own right: a summary is a button, and a button need
  not expose the controls nested in it."
  ([label selected ids]
   (corpus-toggle label selected ids nil))
  ([label selected ids opts]
   (controls/select-all label ids selected [:toggle-corpora (vec ids)] opts)))

(defn all-toggle
  "`corpus-toggle` over every corpus on offer, the `ids` (see
  dk.cst.corpus-probe.views.tree/offered), named for the registry in
  `ui`: the one control that selects or clears it, which is otherwise a
  click per folder and, under a filter, the only way to take everything
  the filter found at once.

  It also carries the chooser's one constraint, that a search needs a
  corpus: it is invalid while nothing is `selected`, and says so in
  words, which the summary's own figures do not, so the browser refuses
  the search on the control that can put it right. Whatever the chooser
  shows: a selection out of sight is still a selection, and a corpus out
  of sight is still one this can choose."
  [ui selected ids]
  (corpus-toggle (i18n/tr ui "All corpora") selected ids
                 {:invalid (when (empty? selected)
                             (i18n/tr ui "Select at least one corpus"))}))

(defn heading
  "The heading tag `level` deep, h6 at the deepest: HTML has no h7."
  [level]
  (keyword (str "h" (min 6 level))))

(defn index-folder
  "One resolved `folder` of the corpus index in `ui`, headed at `level`
  (2 for a top-level folder, one more for each folder inside it): its
  label as a heading, when it has one, its corpora as a list (see
  `corpus-item`) and its subfolders after them, each a level down. A
  label-less folder is its list alone, and takes no level."
  [ui level {:keys [label corpora folders]}]
  (list
   (when label [(heading level) label])
   ;; classed for the stylesheet, which lines the entries' names, ids
   ;; and sizes up in columns across the list
   (when (seq corpora) [:ul.index (map (partial corpus-item ui) corpora)])
   (map (partial index-folder ui (cond-> level label inc)) folders)))

(defn index-view
  "The corpus index page body in `ui`: the `folders` tree of corpus
  overviews laid out as a document, a heading per folder and a list per
  folder's corpora (see `index-folder`), the ungrouped tail labelled by
  `labelled-folders`.

  A document rather than the chooser's tree of disclosures: a reader is
  here to read, not to work a control, so nothing is folded away and the
  headings give the page an outline. The tree is the page's own content
  rather than navigation inside it, so it sits directly in <main>."
  [ui {:keys [folders]}]
  [:main layout/main-attrs
   [:h1 (i18n/tr ui "Corpora")]
   (map (partial index-folder ui 2) (labelled-folders ui folders))])

(defn tree
  "The `folders` of the registry as the tree the chooser takes (see
  dk.cst.corpus-probe.views.tree), in `ui`: a node per folder, named by
  the path of labels down to it, which names its disclosure the same in
  either language, and its corpora its leaves, named by their IDs, read
  by their IDs and titles and disabled where they cannot be read. The
  label-less tail folder is labelled among labelled siblings (see
  `labelled-folders`), after its node has been named."
  [ui folders]
  (letfn [(node [path {:keys [label corpora folders]}]
            (let [id (conj path label)]
              {:id    id
               :label label
               :items (mapv (fn [{:keys [id title size] :as m}]
                              (assoc m
                                     :text      (str id " " title)
                                     :disabled? (nil? size)))
                            corpora)
               :nodes (mapv (partial node id) folders)}))]
    (mapv (fn [folder {:keys [label]}]
            (assoc (node [] folder) :label label))
          folders
          (labelled-folders ui folders))))

(defn chooser
  "The corpus selection of the search form: the `folders` tree as the
  chooser over it (see dk.cst.corpus-probe.views.tree/chooser, which
  the `opts` are for), the IDs in the set `:selected` checked, in `ui`.

  Each folder carries a `corpus-toggle` selecting or clearing the whole
  of it, and the registry `all-toggle`, which refuses a search until
  something is selected, in the words the count does not say. A corpus
  that cannot be read cannot be chosen, so it is disabled and not
  counted: a folder holding one would otherwise be partly chosen for
  ever, and stand open for ever with it."
  [ui folders {:keys [selected] :or {selected #{}} :as opts}]
  (tree/chooser
   ui :corpora (tree ui folders)
   (assoc opts
          :selected  selected
          :legend    (i18n/tr ui "Corpora")
          :not-found (i18n/tr ui "No corpora found.")
          :control   (partial all-toggle ui selected)
          :toggle    (fn [{:keys [label offered]}]
                       (corpus-toggle (str (i18n/tr ui "All corpora in")
                                           " " label)
                                      selected offered))
          :item      (partial chooser-item ui selected))))

(defn facts-list
  "The general facts of a corpus as a definition list: its token count and
  charset from describe `stats`, then the registry properties reported by
  `info` (minus the charset property, which would repeat the charset row).
  The row labels are in `ui`; the registry property names are
  CWB's own and stay as they are."
  [ui stats info]
  [:dl.facts
   [:dt (i18n/tr ui "size")] [:dd (token-count ui (:size stats))]
   [:dt (i18n/tr ui "charset")] [:dd (:charset stats)]
   (mapcat (fn [[k v]] [[:dt (name k)] [:dd v]])
           (sort-by key (dissoc (:properties info) :charset)))])

(defn p-attr-table
  "The positional attributes of describe `stats` as a statistics table in
  `ui`."
  [ui {:keys [p-attrs] :as stats}]
  (when (seq p-attrs)
    [:table.attributes
     [:caption (layout/term ui :positional-attributes)]
     [:thead
      [:tr [:th {:scope "col"} (i18n/tr ui "attribute")]
       [:th.n {:scope "col"} (i18n/tr ui "tokens")]
       [:th.n {:scope "col"} (i18n/tr ui "types")]]]
     [:tbody
      (for [{attr :name :keys [tokens types]} p-attrs]
        [:tr [:th {:scope "row"} [:code (name attr)]]
         (count-cell ui tokens)
         (count-cell ui types)])]]))

(defn s-attr-table
  "The structural attributes of describe `stats` as a statistics table,
  marking the annotation-carrying ones, in `ui`."
  [ui {:keys [s-attrs] :as stats}]
  (when (seq s-attrs)
    [:table.attributes
     [:caption (layout/term ui :structural-attributes)]
     [:thead
      [:tr [:th {:scope "col"} (i18n/tr ui "attribute")]
       ;; a column heading takes the plural form of what it counts
       [:th.n {:scope "col"} (i18n/trn ui "region" "regions" 2)]
       [:th {:scope "col"} (i18n/tr ui "annotations")]]]
     [:tbody
      (for [{attr :name :keys [regions values?]} s-attrs]
        [:tr [:th {:scope "row"} [:code (name attr)]]
         (count-cell ui regions)
         [:td (when values? (i18n/tr ui "with annotations"))]])]]))

(defn a-attr-table
  "The alignment attributes of describe `stats` as a statistics table; no
  KU corpus has any, but hiding one that exists would be unfaithful. The
  headings are in `ui`."
  [ui {:keys [a-attrs] :as stats}]
  (when (seq a-attrs)
    [:table.attributes
     [:caption (layout/term ui :alignment-attributes)]
     [:thead
      [:tr [:th {:scope "col"} (i18n/tr ui "attribute")]
       [:th.n {:scope "col"} (i18n/tr ui "blocks")]]]
     [:tbody
      (for [{attr :name :keys [blocks]} a-attrs]
        [:tr [:th {:scope "row"} [:code (name attr)]]
         (count-cell ui blocks)])]]))

(defn lang-attrs
  "The attribute map marking an element's text as being in `lang`, when
  the language is known."
  [lang]
  (cond-> {} lang (assoc :lang lang)))

(defn info-text
  "The free-text content of the corpus's .info file, verbatim from the
  :info key of `info` (the parsed `info;` output), in the corpus's own
  `corpus-lang`; its heading is in `ui`."
  [ui info corpus-lang]
  (when-let [text (:info info)]
    [:section.about
     [:h2 (i18n/tr ui "Info")]
     ;; the corpus author's own prose, not program output, so a bare <pre>
     [:pre (lang-attrs corpus-lang) text]]))

(defn unreadable-section
  "The section shown in `ui` in place of the corpus facts when
  CWB cannot read the corpus's data, saying whether CWB has no data for
  the registry entry at all (`phantom?`) or reading it failed this time.

  Deliberately detail-free otherwise: the underlying tool output can name
  server paths, which never reach a rendered page. No live region: the
  section is in the document before the page is parsed, where a live
  region announces nothing anyway."
  [ui phantom?]
  [:section.error
   [:h2 (i18n/tr ui "Unreadable corpus")]
   [:p (if phantom?
         (i18n/tr ui (str "The registry lists this corpus, but CWB has "
                          "no data for it."))
         (i18n/tr ui "CWB cannot read the data files of this corpus."))]])

(defn info-view
  "The corpus info page body for `data` in `ui`: the corpus
  title and ID, its facts, attribute statistics and .info text, and links
  searching it and listing its word frequencies, which a `:phantom?` entry
  cannot be and so does not get.

  `data` holds :corpus (the uppercase name), :title (its registry NAME, when
  set), :lang (its language code, when known: the title and the .info text
  are in the corpus's own language, the rest of the page is not), and
  either :stats (describe) + :info (`info;`) or an :error, which is
  replaced by a fixed section saying whether the entry is a `:phantom?`."
  [ui {:keys [corpus title stats info error phantom?]
         corpus-lang :lang :as data}]
  [:main.corpus-info layout/main-attrs
   ;; an <hgroup> groups a heading with its own subheading, so it earns its
   ;; place only when the registry NAME gives the ID one to be grouped with
   (if title
     [:hgroup
      [:h1 (lang-attrs corpus-lang) title]
      [:p [:code corpus]]]
     [:h1 corpus])
   (if error
     (unreadable-section ui phantom?)
     (list
      (facts-list ui stats info)
      (p-attr-table ui stats)
      (s-attr-table ui stats)
      (a-attr-table ui stats)
      (info-text ui info corpus-lang)))
   ;; a corpus CWB has no data for cannot be searched, so it is not
   ;; offered, as the chooser does not offer it either
   ;; where this page leads: the two things a reader does with a corpus
   ;; once they have read about it. A <nav> like the site's and the result
   ;; views', and named like them, since a page with two navigations owes
   ;; a reader a way of telling them apart
   (when-not phantom?
     [:nav {:aria-label (i18n/tr ui "This corpus")}
      [:ul.row
       [:li [:a {:href (url/search-href {:corpus corpus})}
             (str (i18n/tr ui "Search in") " " corpus)]]
       [:li [:a {:href (url/results-href {:corpus corpus
                                          :view   "frequencies"})}
             (str (i18n/tr ui "Word frequencies of") " " corpus)]]]])])
