(ns dk.cst.corpus-probe.views.widgets
  "The generic parts the components are built from: a labelled select
  and its options, a live region, a pager, a row of links, a definition
  list, a badge and a cell for a count, a checkbox taking a group at
  once, an error section, the semantic cell of an attribute value, the
  jargon linked to its glossary entry, and the attribute maps every page
  shares.

  Nothing here knows what it is listing. A caller says what the entries
  are and hands in its words, already translated; the literals here are
  the jargon and the unit of a size, which are literals so that the
  translation scanner finds them."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.url :as url]))

(def main-id
  "The id of every page's <main>. Named once, so the bypass link (see
  dk.cst.corpus-probe.views/skip-link) and the element it reaches cannot
  drift apart without the whole app noticing."
  "main")

(def main-attrs
  "The attributes every page's <main> carries: `main-id`, and the tabindex
  that lets the bypass link move focus into it rather than only scrolling
  to it."
  {:id main-id :tabindex "-1"})

(defn lang-attrs
  "The attribute map marking an element's text as being in `lang`, when
  the language is known."
  [lang]
  (cond-> {} lang (assoc :lang lang)))

(defn hidden-attrs
  "The attribute map hiding an element while `hidden?`. Hidden rather
  than left out: a hidden control is still in the document and still
  submitted, where one left out would drop a choice from the search
  without anyone saying so."
  [hidden?]
  (cond-> {} hidden? (assoc :hidden true)))

(defn option
  "The option `value` of a select, called `label`, chosen when it is what
  is `selected`."
  [selected value label]
  [:option {:value value :selected (= value selected)} label])

(defn select
  "A select named `id` over `options` (see `option`), its `label` before
  it, bound by `form-id` to the form it submits with, so it can stand
  beside the result it acts on rather than inside the query form, and
  applying itself as it is changed: choosing is asking.

  The label is `visible?` unless told otherwise: a control standing in a
  phrase that already reads as its label is named for a screen reader
  alone, and the phrase says it for everyone else."
  ([form-id id label options]
   (select form-id id label options true))
  ([form-id id label options visible?]
   (let [control [:select {:id id :name id :form form-id
                           :on {:change [:apply-view]}}
                  options]]
     (if visible?
       (list [:label {:for id} label] " " control)
       (assoc-in control [1 :aria-label] label)))))

(defn status
  "A live region holding `content`, rendered whether or not there is
  anything to say: a live region announces a change to what it holds,
  so one created already full has no change to announce, and several
  screen readers say nothing at all. A <div>, so the empty one costs no
  margins. `placement` classes the region for a layout that gives it a
  place of its own."
  ([content]
   [:div.status {:role "status"} content])
  ([placement content]
   [:div.status {:class placement :role "status"} content]))

(defn pager
  "The links from `position` (where in a sequence the reader is) to the
  page before and the page after, `prev` and `next` each [href label],
  nil where out of range; nil without either.

  A list, so assistive technology can say how many options there are,
  and an absent direction is left out rather than held open by an empty
  element, which nothing positioned. The links carry the `rel` values
  browsers and crawlers use for sequential pages."
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

(defn count-badge
  "How many entries a disclosure holds, `n`, or how many of the `total`
  it holds are chosen, beside the name in its summary: in parentheses
  and as a side note, which the user agent sets smaller and the
  stylesheet greys, so a shut disclosure says what is inside it and how
  much of it is taken without either number competing with the name.

  Both numbers count the same entries, so a filter narrows them together
  and neither is read against a population the other does not have."
  ([n]
   [:small.count (str "(" n ")")])
  ([n total]
   [:small.count (str "(" n "/" total ")")]))

(defn count-cell
  "A table cell of the count `n` in `ui`, its digits grouped, classed
  for the stylesheet to set as the number it is, with whatever `more`
  says of it after it: a rate in parentheses, or nothing."
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
  with `chosen?` saying which of them already are and `label` naming it;
  nil when there is nothing to take.

  Checked when they all are and partly checked when only some are. That
  third state is one no attribute carries, so it is set as a property on
  every render rather than from the markup.

  It has no visible label of its own, because it sits beside the
  disclosure it governs and that disclosure is named: repeating the name
  would say the same thing twice to anyone who can see both.

  `:clear-only?` in `opts` is for a list where taking everything means
  nothing (see dk.cst.corpus-probe.views.search.filter/clear-toggle): the
  control keeps its shape and its three states, and is disabled while
  nothing is chosen, which is the only state from which it could do the
  thing it must not. A caller asking for it pairs it with an action that
  clears.

  `:invalid` in `opts` is the message the control reports while a group
  that must not be left empty is. HTML can require one box but not one
  of a group, so the group's constraint goes on the control that governs
  it, which is in view whether or not the disclosure is open, and the
  browser reports it there on submit. No attribute carries a custom
  validity either, so it is set with the third state."
  ([label items chosen? action]
   (select-all label items chosen? action nil))
  ([label items chosen? action {:keys [clear-only? invalid]}]
   (when (seq items)
     (let [n (count (filter chosen? items))]
       [:input {:type                "checkbox"
                :checked             (= n (count items))
                ;; Replicant drops a false attribute value, so this is
                ;; absent rather than the string "false", which on a
                ;; boolean attribute would disable the control outright
                :disabled            (boolean (and clear-only? (zero? n)))
                :aria-label          label
                :replicant/on-render [:set-checkbox-state
                                      {:indeterminate (< 0 n (count items))
                                       :invalid       invalid}]
                :on                  {:change action}}]))))

(defn error-section
  "An error under `heading`, an h2 of its own, then its `body`.

  No live region: every error here arrives by a full page load, where a
  region that is already populated announces nothing, while the alert
  role costs the section its own semantics and flattens its heading. An
  h2, since it sits inside a region headed by the page's own h1."
  [heading body]
  [:section.error
   [:h2 heading]
   body])

(defn term
  "The jargon `k` as the interface shows it, in `ui`: CWB's own word, as
  an <abbr> with its expansion where it is one, linked to its glossary
  entry (the key is the entry's id) unless `linked?` is false: inside
  another link, in a label whose click belongs to its control, or in
  the machinery of a form or a result (a legend, a table's head, a
  caption), which the glossary is linked from the prose instead of. Not
  only abbreviations: match and frequency are terms too."
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
