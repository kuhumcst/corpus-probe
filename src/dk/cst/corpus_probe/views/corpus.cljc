(ns dk.cst.corpus-probe.views.corpus
  "Hiccup for the corpus pages: the corpus index, the corpus chooser of
  the search form, the per-corpus info pages and the reading page of one
  text.

  The index and the chooser share one folder-grouped tree over the
  registry: the chooser is a control (see
  dk.cst.corpus-probe.views.chooser), the index a document."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.hiccup :as hiccup]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.chooser :as chooser]
            [dk.cst.corpus-probe.views.result :as result]
            [dk.cst.corpus-probe.views.widgets :as widgets]))

(defn corpus-details
  "The details following a corpus's name in a tree entry for overview map
  `m`: its ID when the name shown is a title, and its token count in
  `ui`, or, standing where the size would, a mark that the corpus cannot
  be read."
  [ui {:keys [id title size] :as m}]
  (list (when title (list [:code id] " "))
        (if size
          (widgets/size-data ui size)
          [:em.size (i18n/tr ui "unavailable")])))

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
  is in the set `selected`, its details in `ui`. A corpus that cannot be
  read is disabled."
  [ui selected {:keys [id title size hidden?] :as m}]
  [:li (widgets/hidden-attrs hidden?)
   [:label
    [:input {:type     "checkbox"
             :name     "corpus"
             :value    id
             :checked  (contains? selected id)
             :disabled (nil? size)
             ;; every change is reported: the folder toggles and the counts
             ;; in the summaries are computed from the state, and a count
             ;; that disagreed with the boxes under it would be worse than
             ;; no count
             :on       {:change [:toggle-corpora [id]]}}]
    " " (or title id) " " (corpus-details ui m)]])

(defn labelled-folders
  "Label the label-less tail folder among `folders` \"Other\", in `ui`,
  when it has labelled siblings, or the ungrouped corpora could be
  mistaken for part of the disclosure above them. A lone label-less
  folder stays a bare list."
  [ui folders]
  (cond->> folders
    (next folders) (map (fn [f]
                          (update f :label #(or % (i18n/tr ui "Other")))))))

(defn corpus-toggle
  "dk.cst.corpus-probe.views.widgets/select-all over the corpus `ids`,
  called `label`, with the set of `selected` IDs and the select-all's own
  `opts`.

  It precedes the disclosure rather than sitting in the <summary>, so a
  whole folder can be included without opening it, and because a summary
  is a button, which need not expose the controls nested in it."
  ([label selected ids]
   (corpus-toggle label selected ids nil))
  ([label selected ids opts]
   (widgets/select-all label ids selected [:toggle-corpora (vec ids)] opts)))

;; TODO: if selecting every corpus proves too costly in production, the
;; choices are a `:clear-only?` box here, as the metadata filter has, or
;; a configured cap on how many corpora a search may name. Only the cap
;; bites: ticking each folder still selects the lot, and a URL naming no
;; corpus already means every corpus (see url/with-corpora).
(defn all-toggle
  "The `corpus-toggle` over every corpus on offer, the `ids`, named for the
  registry in `ui`: the one control that selects or clears the lot.

  It carries the chooser's one constraint, that a search needs a corpus:
  invalid while nothing is `selected`, and says so in words, which the
  summary's own figures do not, so the browser refuses the search on the
  control that can put it right. Whatever the chooser shows: a selection
  out of sight is still a selection."
  [ui selected ids]
  (corpus-toggle (i18n/tr ui "All corpora") selected ids
                 {:invalid (when (empty? selected)
                             (i18n/tr ui "Select at least one corpus"))}))

(defn index-folder
  "One resolved `folder` of the corpus index in `ui`, headed at `level`
  (2 for a top-level folder, one more for each folder inside it): its
  label as a heading, when it has one, its corpora as a list (see
  `corpus-item`) and its subfolders after them, each a level down. A
  label-less folder is its list alone, and takes no level."
  [ui level {:keys [label corpora folders]}]
  (list
   (when label [(hiccup/heading level) label])
   ;; classed for the stylesheet, which lines the entries' names, ids
   ;; and sizes up in columns across the list
   (when (seq corpora) [:ul.index (map (partial corpus-item ui) corpora)])
   (map (partial index-folder ui (cond-> level label inc)) folders)))

(defn index-page
  "The corpus index page body in `ui`: the `folders` tree of corpus
  overviews laid out as a document, a heading per folder and a list per
  folder's corpora, the ungrouped tail labelled by `labelled-folders`.

  A document rather than the chooser's tree of disclosures: a reader is
  here to read, not to work a control, so nothing is folded away and the
  headings give the page an outline."
  [ui {:keys [folders]}]
  [:main widgets/main-attrs
   [:h1 (i18n/tr ui "Corpora")]
   (map (partial index-folder ui 2) (labelled-folders ui folders))])

(defn corpus-tree
  "The `folders` of the registry as the tree the chooser takes, in `ui`:
  a node per folder, named by the path of labels down to it, which names
  its disclosure the same in either language, and its corpora its leaves,
  read by their IDs and titles and disabled where they cannot be read."
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

(defn corpus-chooser
  "The corpus selection of the search form: the `folders` tree as the
  chooser over it (see dk.cst.corpus-probe.views.chooser/chooser, which
  the `opts` are for), the IDs in the set `:selected` checked, in `ui`.

  A corpus that cannot be read cannot be chosen, so it is disabled and
  not counted: a folder holding one would otherwise be partly chosen for
  ever, and stand open for ever with it."
  [ui folders {:keys [selected] :or {selected #{}} :as opts}]
  (chooser/chooser
   ui :corpora (corpus-tree ui folders)
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

(defn stat-cell
  "A statistics table cell for count `n` in `ui`, or for the tool's NO
  DATA when the count is nil because an attribute's data files cannot be
  read."
  [ui n]
  (if n
    (widgets/count-cell ui n)
    [:td.num [:em (i18n/tr ui "no data")]]))

(defn p-attr-table
  "The positional attributes of describe `stats` as a statistics table in
  `ui`."
  [ui {:keys [p-attrs] :as stats}]
  (when (seq p-attrs)
    [:table.attributes
     [:caption (widgets/term ui :positional-attributes)]
     [:thead
      [:tr [:th {:scope "col"} (i18n/tr ui "attribute")]
       [:th.num {:scope "col"} (i18n/tr ui "tokens")]
       [:th.num {:scope "col"} (i18n/tr ui "types")]]]
     [:tbody
      (for [{attr :name :keys [tokens types]} p-attrs]
        [:tr [:th {:scope "row"} [:code (name attr)]]
         (stat-cell ui tokens)
         (stat-cell ui types)])]]))

(defn s-attr-table
  "The structural attributes of describe `stats` as a statistics table,
  marking the annotation-carrying ones, in `ui`."
  [ui {:keys [s-attrs] :as stats}]
  (when (seq s-attrs)
    [:table.attributes
     [:caption (widgets/term ui :structural-attributes)]
     [:thead
      [:tr [:th {:scope "col"} (i18n/tr ui "attribute")]
       ;; a column heading takes the plural form of what it counts
       [:th.num {:scope "col"} (i18n/trn ui "region" "regions" 2)]
       [:th {:scope "col"} (i18n/tr ui "annotations")]]]
     [:tbody
      (for [{attr :name :keys [regions values?]} s-attrs]
        [:tr [:th {:scope "row"} [:code (name attr)]]
         (stat-cell ui regions)
         [:td (when values? (i18n/tr ui "with annotations"))]])]]))

(defn a-attr-table
  "The alignment attributes of describe `stats` as a statistics table; no
  KU corpus has any, but hiding one that exists would be unfaithful. The
  headings are in `ui`."
  [ui {:keys [a-attrs] :as stats}]
  (when (seq a-attrs)
    [:table.attributes
     [:caption (widgets/term ui :alignment-attributes)]
     [:thead
      [:tr [:th {:scope "col"} (i18n/tr ui "attribute")]
       [:th.num {:scope "col"} (i18n/tr ui "blocks")]]]
     [:tbody
      (for [{attr :name :keys [blocks]} a-attrs]
        [:tr [:th {:scope "row"} [:code (name attr)]]
         (stat-cell ui blocks)])]]))

(defn info-section
  "The free-text content of the corpus's .info file, verbatim from the
  :info key of `info` (the parsed `info;` output), in the corpus's own
  `corpus-lang`, as a section headed in `ui`."
  [ui info corpus-lang]
  (when-let [text (:info info)]
    [:section.about
     [:h2 (i18n/tr ui "Info")]
     ;; the corpus author's own prose, not program output, so a bare <pre>
     [:pre (widgets/lang-attrs corpus-lang) text]]))

(defn unreadable-section
  "The section shown in `ui` in place of the corpus facts when CWB cannot
  read the corpus's data, saying whether CWB has no data for the registry
  entry at all (`phantom?`) or reading it failed this time.

  Detail-free otherwise: the underlying tool output can name server
  paths, which never reach a rendered page."
  [ui phantom?]
  (widgets/error-section
   (i18n/tr ui "Unreadable corpus")
   [:p (if phantom?
         (i18n/tr ui (str "The registry lists this corpus, but CWB has "
                          "no data for it."))
         (i18n/tr ui "CWB cannot read the data files of this corpus."))]))

(defn info-page
  "The corpus info page body for `data` in `ui`: the corpus title and ID,
  its facts, its attribute statistics and .info text, and links searching
  it and listing its word frequencies, which a `:phantom?` entry cannot
  be and so does not get.

  `data` holds :corpus (the uppercase name), :title (its registry NAME),
  :lang (the language of the title and the .info text, not of the rest of
  the page), and either :stats (describe) + :info (`info;`) or an
  :error."
  [ui {:keys [corpus title stats info error phantom?]
       corpus-lang :lang :as data}]
  [:main widgets/main-attrs
   ;; an <hgroup> groups a heading with its own subheading, so it earns its
   ;; place only when the registry NAME gives the ID one to be grouped with
   (if title
     [:hgroup
      [:h1 (widgets/lang-attrs corpus-lang) title]
      [:p [:code corpus]]]
     [:h1 corpus])
   (if error
     (unreadable-section ui phantom?)
     (list
      (widgets/facts
       (into [[(i18n/tr ui "size") (widgets/size-data ui (:size stats))]
              [(i18n/tr ui "charset") (:charset stats)]]
             ;; minus the charset property: the row above already has it
             (sort-by key (dissoc (:properties info) :charset))))
      (p-attr-table ui stats)
      (s-attr-table ui stats)
      (a-attr-table ui stats)
      (info-section ui info corpus-lang)))
   ;; where this page leads: the two things a reader does with a corpus
   ;; once they have read about it. A <nav> like the site's and the result
   ;; views', and named like them, since a page with two navigations owes
   ;; a reader a way of telling them apart. A corpus CWB has no data for
   ;; cannot be searched, so it is not offered, as the chooser does not
   ;; offer it either
   (when-not phantom?
     [:nav {:aria-label (i18n/tr ui "This corpus")}
      (widgets/link-row
       [[:search (url/search-href {:corpus corpus})
         (str (i18n/tr ui "Search in") " " corpus)]
        [:frequencies (url/results-href {:corpus corpus
                                         :view   "frequencies"})
         (str (i18n/tr ui "Word frequencies of") " " corpus)]]
       nil)])])

(defn text-name
  "What the text with structural annotations `structs` is called, in
  `ui`: its title, else its id, else just a text."
  [ui structs]
  (or (:text_title structs) (:text_id structs) (i18n/tr ui "Text")))

(defn marked
  "The `words` of one block of a text starting at corpus position
  `from`, as text, those from `cpos` to `matchend` (the `hit`; nil for
  none) inside a <mark>, which carries `url/hit-id` when `landing?`, so
  that the page's URL lands on it."
  [from [cpos matchend :as hit] landing? words]
  (if hit
    (->> (map-indexed (fn [i word] [(<= cpos (+ from i) matchend) word]) words)
         (partition-by first)
         (map (fn [run]
                (let [text (str/join " " (map second run))]
                  (if (ffirst run)
                    [:mark (cond-> {} landing? (assoc :id url/hit-id)) text]
                    text))))
         (interpose " "))
    (str/join " " words)))

(defn reading-page
  "The reading page's main content from `data` (see
  dk.cst.corpus-probe.search/text!, plus the `:hit` [cpos matchend] to
  mark and the `:lang` of the corpus text), in `ui`: the text's name,
  the corpus it is from, its structural annotations and its `:blocks`
  as paragraphs, the hit marked in the block that holds it; or the
  `:error` that came instead of the text.

  A document like the frontpage, so the stylesheet gives it a measure."
  [ui {:keys [corpus structs blocks from hit lang error] :as data}]
  [:main.document widgets/main-attrs
   [:h1 (text-name ui structs)]
   (if error
     (result/cqp-error-section ui error [corpus])
     (list
      [:p (i18n/tr ui "in") " " [:a {:href (url/corpus corpus)} [:code corpus]]]
      (widgets/facts structs)
      ;; the corpus text is in its own language while the page around it
      ;; is in the reader's
      [:div (widgets/lang-attrs lang)
       (map (fn [block-from words]
              (let [block-to (+ block-from (dec (count words)))]
                [:p (marked block-from hit
                            (and hit (<= block-from (first hit) block-to))
                            words)]))
            (reductions + from (map count blocks))
            blocks)]))])
