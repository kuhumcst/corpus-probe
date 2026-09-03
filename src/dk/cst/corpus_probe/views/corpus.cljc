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
  (:require [clojure.string :as str]))

(defn corpus-href
  "The URL of the info page of the corpus named `id`."
  [id]
  (str "/corpus/" (str/lower-case id)))

(defn group-digits
  "Group the digits of non-negative integer `n` in thousands.

  (group-digits 64600000)
  ;; => \"64,600,000\""
  ;; TODO: group by locale once the UI is localised (milestone 5); Danish
  ;; readers expect a period or a space between groups.
  [n]
  (->> (reverse (str n))
       (partition-all 3)
       (map (comp str/join reverse))
       (reverse)
       (str/join ",")))

(defn count-cell
  "A statistics table cell for count `n`, or for the tool's NO DATA when
  the count is nil because an attribute's data files cannot be read."
  [n]
  [:td.n (if n (group-digits n) [:em "no data"])])

(defn token-count
  "The token count `n` as a <data> element: grouped digits for people, the
  plain number in `value` for machines."
  [n]
  [:data.size {:value (str n)} (str (group-digits n) " tokens")])

(defn corpus-details
  "The details following a corpus's name in a tree entry for overview map
  `m`: its ID when the name shown is a title, and its token count, or a
  mark that the corpus cannot be read."
  [{:keys [id title size] :as m}]
  (list (when title (list [:code id] " "))
        (if size (token-count size) [:em "unavailable"])))

(defn corpus-item
  "One corpus overview map `m` as an index entry: a link to its info page
  named by its title (falling back to its ID), then its details."
  [{:keys [id title] :as m}]
  [:li [:a {:href (corpus-href id)} (or title id)] " " (corpus-details m)])

(defn chooser-item
  "One corpus overview map `m` as a chooser entry: a checkbox labelled by
  its title (falling back to its ID) and its details, checked when its ID
  is in the set `selected`. A corpus that cannot be read is disabled."
  [selected {:keys [id title size] :as m}]
  [:li
   [:label
    [:input {:type     "checkbox"
             :name     "corpus"
             :value    id
             :checked  (contains? selected id)
             :disabled (nil? size)}]
    " " (or title id) " " (corpus-details m)]])

(defn folder-corpora
  "Every corpus overview in resolved `folder`, subfolders included."
  [{:keys [corpora folders] :as folder}]
  (concat corpora (mapcat folder-corpora folders)))

(defn folder-view
  "One resolved `folder` of the corpus tree: its corpora rendered by `item`
  and its subfolders recursively, wrapped in a <details> disclosure titled
  by its label and open when `open?` says so of the folder. The label-less
  tail folder of ungrouped corpora renders as a bare list."
  [item open? {:keys [label corpora folders] :as folder}]
  (let [items [:ul
               (map item corpora)
               (map (fn [f] [:li (folder-view item open? f)]) folders)]]
    (if label
      [:details {:open (boolean (open? folder))} [:summary label] items]
      items)))

(defn labelled-folders
  "Label the label-less tail folder among `folders` \"Other\" when it has
  labelled siblings.

  Otherwise the ungrouped corpora could be mistaken for part of the
  disclosure above them; a lone label-less folder (no grouping configured
  at all) stays a bare list."
  [folders]
  (cond->> folders
    (next folders) (map (fn [f] (update f :label #(or % "Other"))))))

(defn corpus-tree
  "The `folders` tree of corpus overviews rendered with `item` and `open?`
  (see `folder-view`), the ungrouped tail labelled."
  [item open? folders]
  (map (partial folder-view item open?) (labelled-folders folders)))

(defn index-view
  "The corpus index page body: the `folders` tree of corpus overviews as
  navigation, every folder open."
  [{:keys [folders]}]
  [:main
   [:nav.corpora {:aria-label "Corpus index"}
    [:h2 "Corpora"]
    (corpus-tree corpus-item (constantly true) folders)]])

(defn chooser
  "The corpus selection of the search form: the `folders` tree as a group
  of checkboxes, the IDs in the set `selected` checked. A folder holding a
  selected corpus starts open so the selection is visible; the rest start
  closed, since a full registry has over a hundred corpora."
  [folders selected]
  [:fieldset.corpora
   [:legend "Corpora"]
   (corpus-tree (partial chooser-item selected)
                (fn [folder]
                  (some (comp selected :id) (folder-corpora folder)))
                folders)])

(defn facts-list
  "The general facts of a corpus as a definition list: its token count and
  charset from describe `stats`, then the registry properties reported by
  `info` (minus the charset property, which would repeat the charset row)."
  [stats info]
  [:dl.facts
   [:dt "size"] [:dd (token-count (:size stats))]
   [:dt "charset"] [:dd (:charset stats)]
   (mapcat (fn [[k v]] [[:dt (name k)] [:dd v]])
           (sort-by key (dissoc (:properties info) :charset)))])

(defn p-attr-table
  "The positional attributes of describe `stats` as a statistics table."
  [{:keys [p-attrs] :as stats}]
  (when (seq p-attrs)
    [:table.attributes
     [:caption "Positional attributes"]
     [:thead
      [:tr [:th {:scope "col"} "attribute"]
       [:th {:scope "col"} "tokens"]
       [:th {:scope "col"} "types"]]]
     [:tbody
      (for [{attr :name :keys [tokens types]} p-attrs]
        [:tr [:th {:scope "row"} [:code (name attr)]]
         (count-cell tokens)
         (count-cell types)])]]))

(defn s-attr-table
  "The structural attributes of describe `stats` as a statistics table,
  marking the annotation-carrying ones with the tool's own wording."
  [{:keys [s-attrs] :as stats}]
  (when (seq s-attrs)
    [:table.attributes
     [:caption "Structural attributes"]
     [:thead
      [:tr [:th {:scope "col"} "attribute"]
       [:th {:scope "col"} "regions"]
       [:th {:scope "col"} "annotations"]]]
     [:tbody
      (for [{attr :name :keys [regions values?]} s-attrs]
        [:tr [:th {:scope "row"} [:code (name attr)]]
         (count-cell regions)
         [:td (when values? "with annotations")]])]]))

(defn a-attr-table
  "The alignment attributes of describe `stats` as a statistics table; no
  KU corpus has any, but hiding one that exists would be unfaithful."
  [{:keys [a-attrs] :as stats}]
  (when (seq a-attrs)
    [:table.attributes
     [:caption "Alignment attributes"]
     [:thead
      [:tr [:th {:scope "col"} "attribute"]
       [:th {:scope "col"} "blocks"]]]
     [:tbody
      (for [{attr :name :keys [blocks]} a-attrs]
        [:tr [:th {:scope "row"} [:code (name attr)]]
         (count-cell blocks)])]]))

(defn lang-attrs
  "The attribute map marking an element's text as being in `lang`, when
  the language is known."
  [lang]
  (cond-> {} lang (assoc :lang lang)))

(defn info-text
  "The free-text content of the corpus's .info file, verbatim from the
  :info key of `info` (the parsed `info;` output), in the corpus's own
  `lang`."
  [info lang]
  (when-let [text (:info info)]
    [:section.about
     [:h3 "Info"]
     [:pre (lang-attrs lang) text]]))

(defn unreadable-section
  "The alert shown in place of the corpus facts when CWB cannot read the
  corpus's data. Deliberately detail-free: the underlying tool output can
  name server paths, which never reach a rendered page."
  []
  [:section.error {:role "alert"}
   [:h3 "Could not read corpus"]
   [:p "CWB could not read this corpus's data files."]])

(defn info-view
  "The corpus info page body for `data`: the corpus title and ID, its
  facts, attribute statistics and .info text, and links searching it and
  listing its word frequencies.

  `data` holds :corpus (the uppercase name), :title (its registry NAME, when
  set), :lang (its language code, when known: the title and the .info text
  are in the corpus's own language, the rest of the page is not), and
  either :stats (describe) + :info (`info;`) or an :error, which is
  replaced by a fixed unreadable-corpus alert."
  [{:keys [corpus title lang stats info error] :as data}]
  [:main
   [:article.corpus-info
    [:hgroup
     (if title
       [:h2 (lang-attrs lang) title]
       [:h2 corpus])
     (when title [:p [:code corpus]])]
    (if error
      (unreadable-section)
      (list
       (facts-list stats info)
       (p-attr-table stats)
       (s-attr-table stats)
       (a-attr-table stats)
       (info-text info lang)))
    [:p
     [:a {:href (str "/?corpus=" corpus)} (str "Search " corpus)]
     " · "
     [:a {:href (str "/frequencies?corpus=" corpus "&attr=word")}
      (str "Word frequencies of " corpus)]]]])
