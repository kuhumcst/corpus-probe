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
            [dk.cst.corpus-probe.i18n :as i18n]))

(defn corpus-href
  "The URL of the info page of the corpus named `id`, in language `lang`."
  [lang id]
  (str "/corpus/" (str/lower-case id) "?lang=" lang))

(defn count-cell
  "A statistics table cell for count `n` in language `lang`, or for the
  tool's NO DATA when the count is nil because an attribute's data files
  cannot be read."
  [lang n]
  [:td.n (if n (i18n/group-digits lang n) [:em (i18n/tr lang :no-data)])])

(defn token-count
  "The token count `n` as a <data> element, in language `lang`: grouped
  digits for people, the plain number in `value` for machines."
  [lang n]
  [:data.size {:value (str n)}
   (str (i18n/group-digits lang n) " " (i18n/tr lang :tokens))])

(defn corpus-details
  "The details following a corpus's name in a tree entry for overview map
  `m`: its ID when the name shown is a title, and its token count in
  language `lang`, or a mark that the corpus cannot be read."
  [lang {:keys [id title size] :as m}]
  (list (when title (list [:code id] " "))
        (if size
          (token-count lang size)
          [:em (i18n/tr lang :unavailable)])))

(defn corpus-item
  "One corpus overview map `m` as an index entry: a link to its info page
  named by its title (falling back to its ID), then its details in
  language `lang`."
  [lang {:keys [id title] :as m}]
  [:li [:a {:href (corpus-href lang id)} (or title id)] " "
   (corpus-details lang m)])

(defn chooser-item
  "One corpus overview map `m` as a chooser entry: a checkbox labelled by
  its title (falling back to its ID) and its details, checked when its ID
  is in the set `selected`, its details in language `lang`. A corpus that
  cannot be read is disabled."
  [lang selected {:keys [id title size] :as m}]
  [:li
   [:label
    [:input {:type     "checkbox"
             :name     "corpus"
             :value    id
             :checked  (contains? selected id)
             :disabled (nil? size)}]
    " " (or title id) " " (corpus-details lang m)]])

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
  "Label the label-less tail folder among `folders` \"Other\", in language
  `lang`, when it has labelled siblings.

  Otherwise the ungrouped corpora could be mistaken for part of the
  disclosure above them; a lone label-less folder (no grouping configured
  at all) stays a bare list."
  [lang folders]
  (cond->> folders
    (next folders) (map (fn [f]
                          (update f :label #(or % (i18n/tr lang :other)))))))

(defn corpus-tree
  "The `folders` tree of corpus overviews rendered with `item` and `open?`
  (see `folder-view`), the ungrouped tail labelled in language `lang`."
  [lang item open? folders]
  (map (partial folder-view item open?) (labelled-folders lang folders)))

(defn index-view
  "The corpus index page body in language `lang`: the `folders` tree of
  corpus overviews as navigation, every folder open."
  [lang {:keys [folders]}]
  [:main
   [:nav.corpora {:aria-label (i18n/tr lang :corpus-index)}
    [:h2 (i18n/tr lang :corpora-heading)]
    (corpus-tree lang (partial corpus-item lang) (constantly true) folders)]])

(defn chooser
  "The corpus selection of the search form: the `folders` tree as a group
  of checkboxes, the IDs in the set `selected` checked. A folder holding a
  selected corpus starts open so the selection is visible; the rest start
  closed, since a full registry has over a hundred corpora. The wording is
  in language `lang`."
  [lang folders selected]
  [:fieldset.corpora
   [:legend (i18n/tr lang :corpora-heading)]
   (corpus-tree lang
                (partial chooser-item lang selected)
                (fn [folder]
                  (some (comp selected :id) (folder-corpora folder)))
                folders)])

(defn facts-list
  "The general facts of a corpus as a definition list: its token count and
  charset from describe `stats`, then the registry properties reported by
  `info` (minus the charset property, which would repeat the charset row).
  The row labels are in language `lang`; the registry property names are
  CWB's own and stay as they are."
  [lang stats info]
  [:dl.facts
   [:dt (i18n/tr lang :size)] [:dd (token-count lang (:size stats))]
   [:dt (i18n/tr lang :charset)] [:dd (:charset stats)]
   (mapcat (fn [[k v]] [[:dt (name k)] [:dd v]])
           (sort-by key (dissoc (:properties info) :charset)))])

(defn p-attr-table
  "The positional attributes of describe `stats` as a statistics table in
  language `lang`."
  [lang {:keys [p-attrs] :as stats}]
  (when (seq p-attrs)
    [:table.attributes
     [:caption (i18n/tr lang :p-attrs-heading)]
     [:thead
      [:tr [:th {:scope "col"} (i18n/tr lang :attribute)]
       [:th {:scope "col"} (i18n/tr lang :tokens)]
       [:th {:scope "col"} (i18n/tr lang :types)]]]
     [:tbody
      (for [{attr :name :keys [tokens types]} p-attrs]
        [:tr [:th {:scope "row"} [:code (name attr)]]
         (count-cell lang tokens)
         (count-cell lang types)])]]))

(defn s-attr-table
  "The structural attributes of describe `stats` as a statistics table,
  marking the annotation-carrying ones, in language `lang`."
  [lang {:keys [s-attrs] :as stats}]
  (when (seq s-attrs)
    [:table.attributes
     [:caption (i18n/tr lang :s-attrs-heading)]
     [:thead
      [:tr [:th {:scope "col"} (i18n/tr lang :attribute)]
       [:th {:scope "col"} (i18n/tr lang :regions)]
       [:th {:scope "col"} (i18n/tr lang :annotations)]]]
     [:tbody
      (for [{attr :name :keys [regions values?]} s-attrs]
        [:tr [:th {:scope "row"} [:code (name attr)]]
         (count-cell lang regions)
         [:td (when values? (i18n/tr lang :with-annots))]])]]))

(defn a-attr-table
  "The alignment attributes of describe `stats` as a statistics table; no
  KU corpus has any, but hiding one that exists would be unfaithful. The
  headings are in language `lang`."
  [lang {:keys [a-attrs] :as stats}]
  (when (seq a-attrs)
    [:table.attributes
     [:caption (i18n/tr lang :a-attrs-heading)]
     [:thead
      [:tr [:th {:scope "col"} (i18n/tr lang :attribute)]
       [:th {:scope "col"} (i18n/tr lang :blocks)]]]
     [:tbody
      (for [{attr :name :keys [blocks]} a-attrs]
        [:tr [:th {:scope "row"} [:code (name attr)]]
         (count-cell lang blocks)])]]))

(defn lang-attrs
  "The attribute map marking an element's text as being in `lang`, when
  the language is known."
  [lang]
  (cond-> {} lang (assoc :lang lang)))

(defn info-text
  "The free-text content of the corpus's .info file, verbatim from the
  :info key of `info` (the parsed `info;` output), in the corpus's own
  `corpus-lang`; its heading is in the UI language `lang`."
  [lang info corpus-lang]
  (when-let [text (:info info)]
    [:section.about
     [:h3 (i18n/tr lang :info)]
     [:pre (lang-attrs corpus-lang) text]]))

(defn unreadable-section
  "The alert shown in language `lang` in place of the corpus facts when CWB
  cannot read the corpus's data, saying whether CWB has no data for the
  registry entry at all (`phantom?`) or reading it failed this time.

  Deliberately detail-free otherwise: the underlying tool output can name
  server paths, which never reach a rendered page."
  [lang phantom?]
  [:section.error {:role "alert"}
   [:h3 (i18n/tr lang :unreadable)]
   [:p (i18n/tr lang (if phantom? :undefined-why :unreadable-why))]])

(defn info-view
  "The corpus info page body for `data` in UI language `lang`: the corpus
  title and ID, its facts, attribute statistics and .info text, and links
  searching it and listing its word frequencies, which a `:phantom?` entry
  cannot be and so does not get.

  `data` holds :corpus (the uppercase name), :title (its registry NAME, when
  set), :lang (its language code, when known: the title and the .info text
  are in the corpus's own language, the rest of the page is not), and
  either :stats (describe) + :info (`info;`) or an :error, which is
  replaced by a fixed alert saying whether the entry is a `:phantom?`."
  [lang {:keys [corpus title stats info error phantom?]
         corpus-lang :lang :as data}]
  [:main
   [:article.corpus-info
    [:hgroup
     (if title
       [:h2 (lang-attrs corpus-lang) title]
       [:h2 corpus])
     (when title [:p [:code corpus]])]
    (if error
      (unreadable-section lang phantom?)
      (list
       (facts-list lang stats info)
       (p-attr-table lang stats)
       (s-attr-table lang stats)
       (a-attr-table lang stats)
       (info-text lang info corpus-lang)))
    ;; a corpus CWB has no data for cannot be searched, so it is not
    ;; offered, as the chooser does not offer it either
    (when-not phantom?
      [:p
       [:a {:href (str "/?corpus=" corpus "&lang=" lang)}
        (str (i18n/tr lang :search-in) " " corpus)]
       " · "
       [:a {:href (str "/frequencies?corpus=" corpus "&attr=word&lang=" lang)}
        (str (i18n/tr lang :word-freqs) " " corpus)]])]])
