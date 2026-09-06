(ns dk.cst.corpus-probe.views.controls
  "The controls a chooser is built from (see
  dk.cst.corpus-probe.views.tree): a way to take a group of checkboxes at
  once, a way to find one among hundreds, the counts beside a
  disclosure's name and the fieldset the whole stands in. HTML has none
  of them, so they are built here, and the first two exist only where
  the client runs to answer them.

  Nothing here knows what it is listing. A caller says what the entries
  are, which of them are chosen and what to dispatch."
  (:require [dk.cst.corpus-probe.i18n :as i18n]))

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
  nothing (see dk.cst.corpus-probe.views.page/clear-toggle): the control
  keeps its shape and its three states, and is disabled while nothing is
  chosen, which is the only state from which it could do the thing it
  must not. A caller asking for it pairs it with an action that clears.

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

(defn entry-count
  "How many entries a disclosure holds, `n`, or how many of the `total`
  it holds are chosen, beside the name in its summary: in parentheses and
  as a side note, which the user agent sets smaller and the stylesheet
  greys, so a shut disclosure says what is inside it and how much of it
  is taken without either number competing with the name.

  Both numbers count the same entries, so a filter narrows them together
  and neither is read against a population the other does not have."
  ([n]
   [:small.count (str "(" n ")")])
  ([n total]
   [:small.count (str "(" n "/" total ")")]))

(defn toggled
  "`disclosure` with `control` beside it as one row, the row being there
  whether or not there is a control to put in it.

  Always the row, because the alternative was a <details> where a <div>
  had been, and everything after an element that changes kind is rebuilt:
  a reader pressing the mouse on a control further down the list had it
  taken from under them, focus with it, by the very render their pressing
  it caused. A nil control renders as nothing and holds its place."
  [control disclosure]
  [:div.group control disclosure])

(defn filter-box
  "A box narrowing what is under it to whatever answers what is typed in
  it: `id` names it, `label` says what it is for, `q` is what it holds
  and `action` is what each change dispatches.

  It goes in the summary of the disclosure it narrows, which is the line
  in view whether that disclosure is open or shut. So finding an entry
  still never starts with opening a list of hundreds to look for the box,
  and the box costs no line of its own: the summary already had one.
  Clicking it works the box and not the disclosure (measured in
  Chromium), while the words beside it still work the disclosure.

  Its label is its placeholder, with `aria-label` saying the same to a
  reader who is never shown one. A word beside it would cost the line it
  just saved, and the summary says what the list is.

  `actions` are what it dispatches: `:input` for every change to what it
  holds, and `:focus` as it takes focus, which is a reader asking to see
  the list it narrows (see dk.cst.corpus-probe.ui/engage!).

  It carries no name, so it is not part of the search: what a reader typed
  to find something is how they found it, not what they asked for. Enter
  is swallowed for the same reason, since a text field in a form otherwise
  submits it, and a reader half way through choosing has not asked for an
  answer yet.

  What it found is reported by `filter-status`, which takes the place of
  the list rather than sitting here."
  [id label q actions]
  [:input.find {:id           id
                :type         "search"
                :placeholder  label
                :aria-label   label
                :value        (or q "")
                :autocomplete "off"
                :on           (assoc actions :keydown [:swallow-enter])}])

(defn filter-status
  "The region reporting what a `filter-box` found: `message` when it found
  nothing, and nothing at all otherwise.

  It stands under the disclosure it reports on, where the entries it
  found none of would be, so a reader is told what happened where they
  were looking. The entries are hidden one by one rather than dropped:
  their checkboxes are what the form submits, and a filter narrows what a
  reader sees, not what they have chosen. The disclosure itself is never
  hidden, since it now holds the box being typed in.

  Rendered whether or not it holds anything, and outside the disclosure:
  a live region announces a change to what it holds, so one created
  already full has no change to announce, and neither has one revealed
  from inside a disclosure as it fills. A <div>, so the empty one costs
  no margins.

  What it comes after must keep its kind, or the diff rebuilds it and a
  rebuilt live region is a new one, which announces nothing. That is why
  the row above it is a <div> whether or not it has a select-all to hold
  (see `toggled`, which is for the rows that carry no live region)."
  [message]
  [:div.empty {:role "status"} message])

(defn fieldset
  "The box a list too long to work through by hand stands in, which the
  corpus chooser and the metadata filter are twice over: its `:legend`
  names it, `:control` takes every entry at once beside the disclosure
  the entries are behind (see `select-all`), `:box` narrows what that
  disclosure shows (see `filter-box`), `:status` says what the box found
  (see `filter-status`), `:details` are the attributes of the disclosure
  itself and `:tag` the fieldset's own hiccup tag. The `entries` go
  inside the disclosure.

  The summary is the box and nothing else but how many of the `:total`
  entries are `:chosen`, in figures at the end of the line (see
  `entry-count`). Two figures read the same way in every such list,
  where a sentence about corpora and a sentence about values are two
  things to learn, and the line the sentence took is the line the box
  needed. What the figures do not say aloud the summary's own name does,
  worded in `ui`.

  The row is a <div> whether or not there is a control to put in it, so
  that the live region under it keeps its identity as the filter empties
  the list (see `filter-status`). The region belongs to the box and is
  rendered with it: without a client there is neither.

  `:leave` is what focus leaving the fieldset dispatches, which is one of
  the two ways a reader is known to have finished choosing from the list
  (see dk.cst.corpus-probe.ui/leave!); the other is a click anywhere
  else, which no element of the fieldset can hear."
  [ui {:keys [tag legend control box chosen total details status leave]}
   & entries]
  [tag (cond-> {} leave (assoc :on {:focusout leave}))
   [:legend legend]
   [:div.group
    control
    (into [:details details
           [:summary {:aria-label (str chosen " " (i18n/tr ui "of") " "
                                        total " " (i18n/tr ui "selected"))}
            box
            (entry-count chosen total)]]
          entries)]
   (when box (filter-status status))])
