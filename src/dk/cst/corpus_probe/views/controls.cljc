(ns dk.cst.corpus-probe.views.controls
  "The controls the corpus chooser and the metadata filter both need.

  The two fieldsets are the same problem twice: a list of checkboxes too
  long to work through by hand, behind disclosures, wanting a way to take
  a group of them at once and a way to find one among hundreds. HTML has
  neither, so both are built here rather than written twice, and both
  exist only where the client runs to answer them.

  Nothing here knows what it is listing. A caller says what the entries
  are, which of them are chosen and what to dispatch; corpora and metadata
  values differ in all three and in nothing else."
  (:require [clojure.string :as str]))

(defn answers?
  "True when `s` contains `q`, a lower-cased fragment of a name; false for
  a missing `s`, which answers nothing."
  [q s]
  (boolean (and s (str/includes? (str/lower-case (str s)) q))))

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

(defn toggled
  "`disclosure` with `control` beside it as one row, or the disclosure
  alone where there is no control to put there."
  [control disclosure]
  (if control
    [:div.group control disclosure]
    disclosure))

(defn filter-box
  "A box narrowing what is under it to whatever answers what is typed in
  it: `id` names it, `label` labels it, `q` is what it holds and `action`
  is what each change dispatches.

  It sits outside the disclosure it narrows, so that finding an entry
  never starts with opening a list of hundreds to look for the box.

  It carries no name, so it is not part of the search: what a reader typed
  to find something is how they found it, not what they asked for. Enter
  is swallowed for the same reason, since a text field in a form otherwise
  submits it, and a reader half way through choosing has not asked for an
  answer yet.

  What it found is reported by `filter-status`, which takes the place of
  the list rather than sitting here."
  [id label q action]
  [:p.find
   [:label {:for id} label]
   " "
   [:input {:id           id
            :type         "search"
            :value        (or q "")
            :autocomplete "off"
            :on           {:input   action
                           :keydown [:swallow-enter]}}]])

(defn filter-status
  "The region reporting what a `filter-box` found: `message` when it found
  nothing, and nothing at all otherwise.

  It stands where the list it reports on would be, and that list is
  hidden while nothing in it answers, so a reader is told what happened
  where they were looking rather than beside the box with an empty
  disclosure below. The list is hidden and not dropped: its checkboxes
  are what the form submits, and a filter narrows what a reader sees, not
  what they have chosen.

  Rendered whether or not it holds anything, and outside the disclosure
  it reports on. A live region announces a change to what it holds, so
  one created already full has no change to announce, and neither has one
  revealed from inside a disclosure as it fills. A <div>, so the empty one
  costs no margins.

  It comes before the disclosure rather than after it, which looks the
  same (the disclosure is hidden whenever this has anything to say) and
  survives the diff: a row whose select-all has just disappeared changes
  from a <div> to a <details>, and everything after an element that
  changes kind is rebuilt, this region included. A rebuilt live region is
  a new one, and a new one announces nothing."
  [message]
  [:div.empty {:role "status"} message])
