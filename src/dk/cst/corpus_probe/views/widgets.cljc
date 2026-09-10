(ns dk.cst.corpus-probe.views.widgets
  "The generic parts the views are built from: selects, a live region, a
  pager, a link row and the tab strip made of one, a definition list,
  side notes and count cells, a checkbox over a group, an error section,
  attribute values, glossary terms and the attribute maps every page
  shares. Nothing here knows what it is listing: a caller hands in its
  words, already translated."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.url :as url]))

(def main-id
  "The id of every page's <main>, which the bypass link targets."
  "main")

(def main-attrs
  "The attributes every page's <main> carries: `main-id`, and a tabindex
  letting the bypass link move focus into it rather than only scroll."
  {:id main-id :tabindex "-1"})

(defn lang-attrs
  "The attribute map marking an element's text as being in `lang`, when
  the language is known."
  [lang]
  (cond-> {} lang (assoc :lang lang)))

(defn hidden-attrs
  "The attribute map hiding an element while `hidden?`.

  Hidden rather than left out: a hidden control is still submitted,
  where one left out drops a choice from the search without a word."
  [hidden?]
  (cond-> {} hidden? (assoc :hidden true)))

(defn option
  "The option `value` of a select, called `label`, chosen when it is what
  is `selected`."
  [selected value label]
  [:option {:value value :selected (= value selected)} label])

(defn select
  "A select named `id` over `options` (see `option`), its `label` before
  it unless not `visible?`, bound by `form-id` to the form it submits
  with and applying itself as it is changed."
  ([form-id id label options]
   (select form-id id label options true))
  ([form-id id label options visible?]
   ;; the form attribute lets it stand beside the result it acts on
   ;; rather than inside the query form
   (let [control [:select {:id id :name id :form form-id
                           :on {:change [:apply-view]}}
                  options]]
     (if visible?
       (list [:label {:for id} label] " " control)
       (assoc-in control [1 :aria-label] label)))))

(defn status
  "A live region holding `content`, classed by `placement` where a
  layout gives it a place of its own.

  Render it whether or not there is anything to say: a live region
  announces changes to what it holds, and one created already full
  announces nothing."
  ([content]
   ;; a div, so the empty one costs no margins
   [:div.status {:role "status"} content])
  ([placement content]
   [:div.status {:class placement :role "status"} content]))

(defn pager
  "The links from `position` (where in a sequence the reader is) to the
  page before and the page after, `prev` and `next` each [href label]
  or nil where out of range; nil without either."
  [prev next position]
  (when (or prev next)
    [:ul.row.pager
     (when-let [[href label] prev]
       [:li.pager-prev [:a {:href href :rel "prev"} label]])
     [:li position]
     (when-let [[href label] next]
       [:li.pager-next [:a {:href href :rel "next"} label]])]))

(defn link-row
  "A list of links read as one row: each of `links`, [key href label],
  a link, the one keyed `current` marked as the page being shown."
  [links current]
  [:ul.row
   (for [[k href label] links]
     [:li [:a (cond-> {:href href}
                (= k current) (assoc :aria-current "page"))
           label]])])

(defn tabs
  "A `link-row` of `links` read as a tab strip named `label` in a
  navigation landmark, `current` keying the one being shown, which is
  drawn as a tab standing on the line the row sits on.

  The line itself belongs to whatever draws the boundary there: the
  masthead's own border, the answer's top edge. The tab covers a pixel
  of it, so the two read as one shape."
  [label links current]
  [:nav.tabs {:aria-label label} (link-row links current)])

(defn attribute-value
  "Render attribute value `v` semantically by its key `k`: a text title as
  `<cite>`, a four-digit year as `<time>`, otherwise as it is."
  [k v]
  (let [n (name k)]
    (cond
      (str/ends-with? n "_title")
      [:cite v]

      (and (str/ends-with? n "_year") (string? v) (re-matches #"\d{4}" v))
      [:time v]

      :else
      v)))

(defn facts
  "A definition list of `pairs`, a key and what it holds each: the key
  named as it is, the value rendered by `attribute-value`."
  [pairs]
  [:dl.facts
   (for [[k v] pairs]
     (list [:dt (name k)] [:dd (attribute-value k v)]))])

(defn note
  "A side note beside what it is about, `content` in small muted type:
  a figure or a sign standing for something the layout has no room to
  say, which `title` says in words under the pointer."
  ([content]
   (note content nil))
  ([content title]
   (if title
     [:small.note {:title title} content]
     [:small.note content])))

(defn help
  "A contextual help button beside a control: a `?` saying `what` the
  control takes, linked to the glossary `entry` that explains it at
  length. The words are the link's name too, `?` being none."
  [what entry]
  (note [:a.help-link {:href       (url/glossary-entry entry)
                       :title      what
                       :aria-label what}
         "?"]))

(defn count-badge
  "How many entries a disclosure holds, `n`, or how many of the `total`
  it holds are chosen, as a `note` beside the name in its summary.

  `title` says in words what the figures count, which they cannot say
  themselves. `mark` stands after them where they are not the whole
  story, the title saying what else there is."
  ([n]
   (count-badge n nil nil nil))
  ([n total]
   (count-badge n total nil nil))
  ([n total title]
   (count-badge n total title nil))
  ([n total title mark]
   (cond-> (note (if total (str "(" n "/" total ")") (str "(" n ")")) title)
     ;; the mark is the title in one character, so there is nothing in
     ;; it for a screen reader to spell out
     mark (conj [:span {:aria-hidden "true"} mark]))))

(defn count-cell
  "A table cell of the count `n` in `ui`, its digits grouped, with
  whatever `more` says of it after it: a rate in parentheses, or nothing."
  [ui n & more]
  (into [:td.num (i18n/group-digits ui n)] more))

(defn size-data
  "The token count `n` as a <data> element, in `ui`: grouped digits for
  people, the plain number in `value` for machines."
  [ui n]
  [:data.size {:value (str n)}
   (str (i18n/group-digits ui n) " " (i18n/tr ui "tokens"))])

(defn select-all
  "A checkbox taking every entry of `items` at once, dispatching `action`,
  with `chosen?` saying which of them already are and `label` naming it
  for a screen reader; nil when there is nothing to take. Checked when
  they all are, indeterminate when only some are.

  In `opts`, `:clear-only?` disables it while there is nothing to clear,
  for a list where taking everything means nothing; `:invalid` is the
  message it reports while a group that must not be left empty is, HTML
  being able to require one box but not one of a group; and `:mixed?`
  marks it indeterminate whatever the boxes say, for a group narrowed by
  something other than them, which is then also something to clear."
  ([label items chosen? action]
   (select-all label items chosen? action nil))
  ([label items chosen? action {:keys [clear-only? invalid mixed?]}]
   (when (seq items)
     (let [n (count (filter chosen? items))]
       [:input {:type                "checkbox"
                :checked             (= n (count items))
                ;; Replicant drops a false attribute value, so this is
                ;; absent rather than the string "false", which on a
                ;; boolean attribute would disable the control outright
                :disabled            (boolean (and clear-only? (zero? n)
                                                   (not mixed?)))
                :aria-label          label
                ;; neither the indeterminate state nor a custom validity
                ;; is an attribute, so both are set as properties on render
                :replicant/on-render [:set-checkbox-state
                                      {:indeterminate (or (boolean mixed?)
                                                          (< 0 n (count items)))
                                       :invalid       invalid}]
                :on                  {:change action}}]))))

(defn error-section
  "An error under `heading`, an h2 of its own, then its `body`."
  [heading body]
  ;; no alert role: every error here arrives by a full page load, which a
  ;; live region does not announce, and the role would flatten the heading
  [:section.error
   [:h2 heading]
   body])

(defn term
  "The jargon `k` as the interface shows it, in `ui`: an <abbr> with its
  expansion where it is one, linked to its glossary entry (the key is
  the entry's id) unless `linked?` is false, as inside another link, a
  label or a legend."
  ([ui k]
   (term ui k true))
  ([ui k linked?]
   (let [[label expansion]
         (case k
           :kwic                  ["KWIC" (i18n/tr ui "key word in context")]
           :cqp                   ["CQP" "Corpus Query Processor"]
           :cpos                  ["cpos" (i18n/tr ui "corpus position")]
           :match                 [(i18n/tr ui "match")]
           :frequency             [(i18n/tr ui "frequency")]
           :metadata              [(i18n/tr ui "Metadata")]
           :positional-attributes [(i18n/tr ui "Positional attributes")]
           :structural-attributes [(i18n/tr ui "Structural attributes")]
           :alignment-attributes  [(i18n/tr ui "Alignment attributes")]
           :per-million           [(i18n/tr ui "per million")])
         shown (if expansion [:abbr {:title expansion} label] label)]
     (if linked?
       [:a {:href (url/glossary-entry (name k))} shown]
       shown))))
