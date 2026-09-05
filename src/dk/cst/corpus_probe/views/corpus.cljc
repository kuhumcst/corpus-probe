(ns dk.cst.corpus-probe.views.corpus
  "Hiccup for the corpus index, the corpus chooser of the search form and
  the per-corpus info pages.

  The index and the chooser share one folder-grouped tree over the
  registry: each folder is a <details> disclosure, so the tree collapses
  without any script; the index lists each corpus as a link to its info
  page, the chooser as a checkbox, both with the token count as a
  machine-readable <data>. The info page maps the facts CWB itself reports
  (the registry entry, `info;` and `cwb-describe-corpus -s`) onto a
  definition list and per-attribute statistics tables, following PLAN.md
  §7."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.controls :as controls]
            [dk.cst.corpus-probe.views.layout :as layout]))

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

(defn answers?
  "True when corpus overview `m` answers `q`, a lower-cased fragment of a
  name: its CQP ID or its title contains it."
  [q {:keys [id title]}]
  (or (controls/answers? q id) (controls/answers? q title)))

(defn narrow
  "`folder` with everything that does not answer `q` marked `:hidden?`,
  itself included when nothing in it survives.

  Marked rather than removed. The checkboxes of a filtered-out corpus stay
  in the document, exactly as those of a collapsed folder do, because a
  checkbox the form cannot see is a corpus dropped from the search without
  anyone saying so.

  A folder whose own label answers `q` keeps everything in it, so naming a
  folder is a way of asking for its corpora rather than a way of finding
  none: below such a folder there is nothing left to narrow."
  [q {:keys [label corpora folders] :as folder}]
  (let [whole?  (controls/answers? q label)
        corpora (mapv #(cond-> % (not (or whole? (answers? q %)))
                               (assoc :hidden? true))
                      corpora)
        folders (mapv (partial narrow (if whole? "" q)) folders)]
    (cond-> (assoc folder :corpora corpora :folders folders)
      (and (every? :hidden? corpora) (every? :hidden? folders))
      (assoc :hidden? true))))

(defn folder-view
  "One resolved `folder` of the corpus tree, rendered by the `opts` of the
  tree it belongs to: its corpora by `:item` and its subfolders
  recursively, wrapped in a <details> disclosure whose summary is
  `(:summary opts)` of the folder and which is open when `(:open? opts)`
  says so of it, preceded by `(:toggle opts)` of it where that yields one.

  The summary is a function rather than the bare label so a closed folder
  can still say how many corpora it holds, which is what keeps a collapsed
  tree navigable at a registry of a hundred and fifty. The label-less tail
  folder of ungrouped corpora renders as a bare list, and so never has a
  toggle: there is no folder there to include or exclude."
  [{:keys [item summary toggle open?] :as opts}
   {:keys [label corpora folders hidden?] :as folder}]
  (let [items      [:ul
                    (map item corpora)
                    (map (fn [f] [:li (folder-view opts f)]) folders)]
        ;; marked on the disclosure rather than on the row beside it: a
        ;; folder the filter has emptied has nothing left to select, so it
        ;; has no toggle, so it has no row to be hidden along with
        disclosure [:details (cond-> {:open (boolean (open? folder))}
                               hidden? (assoc :hidden true))
                    [:summary (summary folder)]
                    items]]
    (if label
      (controls/toggled (and toggle (toggle folder)) disclosure)
      items)))

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

(defn corpus-tree
  "The `folders` tree of corpus overviews rendered by `opts` (see
  `folder-view`), the ungrouped tail labelled in `ui`."
  [ui opts folders]
  (map (partial folder-view opts) (labelled-folders ui folders)))

(defn folder-count
  "The label of `folder` followed by how many corpora it holds, in
  `ui`, so a closed folder still says what is inside it, and by
  how many of them are showing when a filter is hiding the rest."
  [ui folder]
  (let [all   (folder-corpora folder)
        shown (remove :hidden? all)]
    (str (:label folder) " · "
         (when (< (count shown) (count all))
           (str (count shown) " " (i18n/tr ui "of") " "))
         (count all) " " (i18n/tr ui "corpora"))))

(defn folder-selection
  "`folder-count` plus how many of the folder's corpora are in the set
  `selected`, so a closed folder says whether the selection is inside it."
  [ui selected folder]
  (let [n (count (filter (comp selected :id) (folder-corpora folder)))]
    (cond-> (folder-count ui folder)
      (pos? n) (str " · " n " " (i18n/tr ui "selected")))))

(defn chooser-summary
  "What the corpus chooser's disclosure says about the set `selected` out
  of `corpora`, every overview in the registry, in `ui`: that
  it is all of them, which one it is when it is one, how many it is, or
  that none is chosen.

  A single corpus is named rather than counted. Counting is what a reader
  needs while they are still narrowing; once they are down to one, the
  useful thing to say is which one, and saying it here means the tree does
  not have to be opened to find out."
  [ui selected corpora]
  (let [n     (count selected)
        total (count corpora)]
    (cond
      (zero? n)   (i18n/tr ui "Select at least one corpus")
      (= n total) (i18n/tr ui "All corpora")
      (= 1 n)     (or (some #(when (selected (:id %)) (or (:title %) (:id %)))
                            corpora)
                      ;; a URL naming a corpus the registry has lost
                      (first selected))
      :else       (str n " " (i18n/tr ui "of") " " total " "
                       (i18n/tr ui "selected")))))

(defn selectable-ids
  "The IDs of the corpora in `folder` a reader can actually choose: a
  corpus that cannot be read has no size and its own box is disabled, and
  one the filter has hidden is not on offer either.

  So a folder toggle acts on what the reader can see. Under a filter that
  is the point of the filter; the corpora it hides keep whatever state
  they had, which is also what clearing the filter shows again."
  [folder]
  (->> (folder-corpora folder)
       (remove :hidden?)
       (remove (comp nil? :size))
       (map :id)))

(defn corpus-toggle
  "dk.cst.corpus-probe.views.controls/select-all over the corpora in
  `folder`, called `label`, with the set of `selected` IDs.

  It precedes the disclosure rather than sitting in the <summary>, so a
  whole folder can be included or excluded without opening it, and so it
  is a control in its own right: a summary is a button, and a button need
  not expose the controls nested in it."
  [label selected folder]
  (let [ids (selectable-ids folder)]
    (controls/select-all label ids selected [:toggle-corpora (vec ids)])))

(defn folder-toggle
  "`corpus-toggle` over one `folder`, named for it in `ui`."
  [ui selected folder]
  (corpus-toggle (str (i18n/tr ui "All corpora in") " " (:label folder))
                 selected folder))

(defn all-toggle
  "`corpus-toggle` over the whole `folders` tree, named for it in `ui`:
  the one control that selects or clears the registry, which is
  otherwise a click per folder and, under a filter, the only way to take
  everything the filter found at once."
  [ui selected folders]
  (corpus-toggle (i18n/tr ui "All corpora") selected {:folders folders}))

(defn corpus-filter
  "The box narrowing the chooser to the corpora answering what is typed in
  it, in `ui`, holding `q`. See
  dk.cst.corpus-probe.views.controls/filter-box."
  [ui q]
  (controls/filter-box "corpus-filter" (i18n/tr ui "Filter") q
                       [:filter-corpora]))

(defn index-view
  "The corpus index page body in `ui`: the `folders` tree of
  corpus overviews, every folder open.

  The tree is this page's own content rather than navigation inside it, so
  it sits directly in <main> under the page's heading, which the bypass
  link moves the reader to."
  [ui {:keys [folders]}]
  [:main layout/main-attrs
   [:h1 (i18n/tr ui "Corpora")]
   [:div.corpora
    (corpus-tree ui
                 {:item    (partial corpus-item ui)
                  :summary (partial folder-count ui)
                  :open?   (constantly true)}
                 folders)]])

(defn chooser
  "The corpus selection of the search form: the `folders` tree as a group
  of checkboxes, the IDs in the set `selected` checked, behind one
  disclosure summarising the selection.

  Closed unless nothing is selected, when choosing a corpus is the
  reader's next move: a registry of a hundred and fifty corpora is an open
  tree between the reader and every other control in the form, and the
  default selects them all. A closed <details> keeps its checkboxes in the
  document and still submits them, so folding the tree away never narrows
  a search. Inside it, a folder holding part of the selection starts open,
  and where `:client?` each folder carries a `folder-toggle` selecting or
  clearing the whole of it. The wording is in `ui`.

  What is checked follows `:selected`, which is live: it changes under the
  reader as they choose. What is open follows `:served`, the selection the
  page arrived with, which does not. Openness is a starting state, and a
  starting state computed from a live value stops being one: a folder
  whose last selected corpus the reader cleared would shut itself while
  they were working in it.

  `:filter` is the exception, and the reason it is one is that it is not a
  starting state at all. While a reader is filtering, every folder holding
  something they are looking for is open, because a tree of shut folders
  is no answer to having asked where a corpus is; clearing the filter puts
  the folders back the way the page arrived.

  The outermost disclosure is the one exception to that in turn, because
  the filter box is outside it: typing opens it, and emptying the box
  again leaves it open, since the tree closing itself around a reader who
  has just finished searching it would be the rudest thing here. Once it
  has been opened or closed at all, `:open?` says which, and the reader
  owns it from then on."
  [ui folders {:keys [selected served client? open?] q :filter}]
  (let [corpora   (mapcat folder-corpora folders)
        total     (count corpora)
        filtering (not (str/blank? q))
        folders   (cond->> folders
                    filtering (mapv (partial narrow (str/lower-case q))))
        nothing-found? (and filtering (every? :hidden? folders))]
    [:fieldset.corpora
     [:legend (i18n/tr ui "Corpora")]
     (when client? (corpus-filter ui q))
     (when client?
       (controls/filter-status (when nothing-found?
                                 (i18n/tr ui "No corpora found."))))
     (controls/toggled
      (when client? (all-toggle ui selected folders))
      [:details (cond-> {:open (or filtering (if (some? open?)
                                               open?
                                               (empty? served)))
                         :on   {:toggle [:set-chooser-open]}}
                  ;; nothing in it answers, so the message below stands
                  ;; where it was; hidden rather than dropped, because its
                  ;; checkboxes are what a search submits
                  nothing-found? (assoc :hidden true))
       [:summary (chooser-summary ui selected corpora)]
       (corpus-tree ui
                    {:item    (partial chooser-item ui selected)
                     :summary (partial folder-selection ui selected)
                     :toggle  (when client?
                                (partial folder-toggle ui selected))
                     :open?   (fn [folder]
                                (if filtering
                                  (not (:hidden? folder))
                                  (and (seq served)
                                       (not= (count served) total)
                                       (some (comp served :id)
                                             (folder-corpora folder)))))}
                    folders)])]))

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
