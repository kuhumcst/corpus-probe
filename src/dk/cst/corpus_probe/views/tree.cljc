(ns dk.cst.corpus-probe.views.tree
  "A chooser over a tree of checkboxes behind disclosures, with a box to
  search it. The corpus chooser and the metadata filter are both one of
  these, and differ only in what they list, how a leaf is drawn and what
  their controls are called (see dk.cst.corpus-probe.views.corpus/chooser
  and dk.cst.corpus-probe.views.page/filter-fieldset).

  The tree is nodes and leaves. A node is {:id :label :items :nodes}, its
  leaves under `:items` and the nodes under it under `:nodes`; a leaf is
  {:id :text} and whatever else its instance draws it from. The ids name
  the leaves in the selection and the nodes among the open disclosures,
  `:label` names a node to the reader, and `:text` is what the filter box
  reads of a leaf, a node's being its label. A leaf that is `:disabled?`
  is listed but never chosen, and a node that is `:in-force?` constrains
  the search by more than its leaves and so is shown at rest whatever is
  chosen under it (see `only-chosen`).

  The chooser has two faces (see `chooser`), and the rules deciding them
  are the pure functions here, which the client applies to its own state
  too (see dk.cst.corpus-probe.ui/lists). The markup is deliberately not
  the ARIA tree pattern: nested <details> with native checkboxes, which
  works without a script and submits every box, and promises no
  arrow-key navigation it does not have."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.views.controls :as controls]))

(defn answers?
  "True when node or leaf `x` answers `q`, a lower-cased fragment of a
  name: its `:text`, or its `:label` without one, contains it. One with
  neither answers nothing."
  [q x]
  (let [s (or (:text x) (:label x))]
    (boolean (and s (str/includes? (str/lower-case (str s)) q)))))

(defn narrow
  "`node` with everything that does not answer `q` marked `:hidden?`,
  itself included when nothing in it survives.

  Marked rather than removed. The checkboxes of a filtered-out leaf stay
  in the document, exactly as those of a collapsed node do, because a
  checkbox the form cannot see is a choice dropped from the search
  without anyone saying so.

  A node whose own label answers `q` keeps everything in it, so naming a
  node is a way of asking for what is in it rather than a way of finding
  none: below such a node there is nothing left to narrow."
  [q {:keys [items nodes] :as node}]
  (let [whole? (answers? q node)
        items  (mapv #(cond-> % (not (or whole? (answers? q %)))
                              (assoc :hidden? true))
                     items)
        nodes  (mapv (partial narrow (if whole? "" q)) nodes)]
    (cond-> (assoc node :items items :nodes nodes)
      (and (every? :hidden? items) (every? :hidden? nodes))
      (assoc :hidden? true))))

(defn offered
  "The ids of the leaves of `node`, the nodes under it included, that a
  reader can choose as the page stands: not one that is `:disabled?`,
  and not one the filter has hidden.

  So a control over a node acts on what the reader can see. Under a
  filter that is the point of the filter; the leaves it hides keep
  whatever state they had, which is also what clearing the filter shows
  again."
  [{:keys [items nodes]}]
  (concat (->> items (remove :hidden?) (remove :disabled?) (map :id))
          (mapcat offered nodes)))

(defn counted
  "`node` and the nodes under it, each stamped with `:offered`, the ids
  it offers (see `offered`).

  Stamped because it is counted later than it is decided: every count in
  the chooser is of what the filter left, while the resting view then
  hides what is not chosen (see `only-chosen`), and a count of what is
  left after that would say two of two of everything."
  [node]
  (let [node (update node :nodes #(mapv counted %))]
    (assoc node :offered (vec (offered node)))))

(defn node-seq
  "Every node among `nodes` and under them, parents first."
  [nodes]
  (mapcat #(tree-seq :nodes :nodes %) nodes))

(defn open-at-rest
  "The disclosures of the chooser over `nodes` that stand open while
  nobody is choosing, with the leaves in the set `selected` chosen: the
  id of every node chosen in part, and `:root` while the tree as a whole
  is.

  In part, and only in part: a node chosen whole or not at all is shut,
  since its box and its count say which, and a shut node shows nothing
  a reader cannot see from those two. Part chosen is the one state they
  cannot show."
  [nodes selected]
  (let [nodes (map counted nodes)
        part? (fn [offered]
                (< 0 (count (filter selected offered)) (count offered)))]
    (into (if (part? (mapcat :offered nodes)) #{:root} #{})
          (comp (filter (comp part? :offered)) (map :id))
          (node-seq nodes))))

(defn matching
  "The ids of the nodes among `nodes` holding something that answers `q`
  (see `narrow`): what typing `q` opens, since a reader who has asked
  where something is has asked to be shown it."
  [q nodes]
  (->> nodes
       (map (comp counted (partial narrow (str/lower-case q))))
       (node-seq)
       (remove :hidden?)
       (map :id)))

(defn only-chosen
  "`node` with every leaf not in the set `held` marked `:hidden?`, all of
  them when every leaf it offers is held, and itself when nothing under
  it is held and it is not `:in-force?`.

  What the chooser shows while nobody is choosing from it: what the
  search will read, and nothing else. A node held whole says so in one
  row, its own, which is what its ticked box and its count are for;
  naming its leaves under it would say the same thing again, once per
  leaf. Marked rather than removed, for the reason `narrow` marks rather
  than removes."
  [held {:keys [items nodes offered in-force?] :as node}]
  (let [whole? (and (seq offered) (every? held offered))
        items  (mapv #(cond-> % (or whole? (not (held (:id %))))
                              (assoc :hidden? true))
                     items)
        nodes  (mapv #(cond-> (only-chosen held %)
                        whole? (assoc :hidden? true))
                     nodes)]
    (cond-> (assoc node :items items :nodes nodes)
      (and (not whole?)
           (not in-force?)
           (every? :hidden? items)
           (every? :hidden? nodes))
      (assoc :hidden? true))))

(defn node-count
  "How many of the leaves a counted `node` offers are in the set
  `selected`, beside how many there are (see
  dk.cst.corpus-probe.views.controls/entry-count), so a shut node still
  says what is inside it and how much of it is chosen.

  Both numbers are of what a filter left, so that a filter narrows them
  together; what the resting view hides is still counted, or every count
  at rest would be of a selection alone."
  [selected {:keys [offered]}]
  (controls/entry-count (count (filter selected offered)) (count offered)))

(defn node-summary
  "What the disclosure of a counted `node` says of it, unless its
  instance says more: its label and its count over the set `selected`
  (see `node-count`)."
  [selected node]
  (list (:label node) " " (node-count selected node)))

(defn node-view
  "One `node` of the tree, rendered by the `opts` of the chooser it
  belongs to: its leaves by `:item` and the nodes under it recursively,
  wrapped in a <details> disclosure whose summary is `(:summary opts)` of
  the node, holding `(:extra opts)` of it before the list where there is
  one, open when `(:open? opts)` says so of it, and preceded by
  `(:toggle opts)` of it where the node is shown and that yields one.

  The summary is a function rather than the bare label so a shut node
  can still say how many leaves it holds, which is what keeps a
  collapsed tree navigable at a registry of a hundred and fifty. The
  disclosure dispatches `(:on-toggle opts)` of the node as it opens or
  shuts, which is how the reader's own word on it is kept (see
  dk.cst.corpus-probe.ui/toggle-open!). A node without a label renders
  as its bare list, and so never has a control: there is nothing there
  to include or exclude.

  The row is there whether or not there is a control to put in it (see
  dk.cst.corpus-probe.views.controls/toggled), and a hidden node has
  none: its control stands outside the disclosure that is hidden, and
  would otherwise be a checkbox floating beside nothing."
  [{:keys [item summary extra toggle open? on-toggle] :as opts}
   {:keys [label items nodes hidden?] :as node}]
  (let [entries    [:ul
                    (map item items)
                    (map (fn [n] [:li (node-view opts n)]) nodes)]
        disclosure [:details (cond-> {:open (boolean (open? node))}
                               on-toggle (assoc :on {:toggle (on-toggle node)})
                               hidden?   (assoc :hidden true))
                    [:summary (summary node)]
                    (when extra (extra node))
                    entries]]
    (if label
      (controls/toggled (when (and toggle (not hidden?)) (toggle node))
                        disclosure)
      entries)))

(defn chooser
  "The fieldset over the `nodes` of list `k` in `ui`, `k` being the name
  the client knows the list by (see dk.cst.corpus-probe.ui/lists): the
  leaves as checkboxes, the ids in the set `:selected` checked, behind
  one disclosure counting the selection (see
  dk.cst.corpus-probe.views.controls/fieldset), named `:legend`.

  It has two faces, and `:choosing?` says which. **At rest** it is what
  the search will read and nothing else: a leaf that is not chosen is
  hidden, so is a node with nothing chosen in it, and a node chosen
  whole hides what is under it, its own ticked row saying it (see
  `only-chosen`). A registry of a hundred and fifty corpora says itself
  in a line or two that way. **While the reader is choosing** nothing is
  hidden, since a chooser showing only what is chosen is nothing to
  choose from. A filter overrides both: what answers `:filter` is shown
  and what does not is hidden, at every level (see `narrow`), and
  `:not-found` is said when nothing does. Ticking a box is doing, not
  asking, and changes no face: unticking at rest takes a choice out of
  the search without unfolding the list over the reader who did it.

  What is open is `:open`, the set of disclosures standing open, `:root`
  for the fieldset's own and its `:id` for each node's, and nothing else
  decides it. The reader's own openings and shuttings go into that set,
  and it is recomputed from the selection only as they finish, never
  under their hands (see `open-at-rest` and
  dk.cst.corpus-probe.ui/lists). Without a client to keep such a set, it
  is the resting one.

  `:held` is what the resting face treats as chosen: the selection, and
  whatever the reader unticked at rest since they last left, so that a
  box unticked there stays in place, to be ticked again, until they have
  gone (see dk.cst.corpus-probe.ui/tick). Hidden is never dropped: a
  closed disclosure and a hidden row both keep their checkboxes in the
  document and submit them, so no face of this narrows a search.

  The instance says how it is drawn: `:item` renders a leaf, `:summary`
  what a node's disclosure says (see `node-summary`, the default),
  `:extra` what else a node's disclosure holds before its list, given
  the node and whether the chooser is at rest, `:control` the control
  beside the whole fieldset, given the ids offered, `:toggle` the one
  beside a node, and `:after` what follows the tree. The controls, the
  box and the toggles are rendered only where `:client?` runs to answer
  them, and `:busy?` marks the fieldset while the client is fetching
  what it lists."
  [ui k nodes {:keys [selected held open choosing? client? busy? legend
                      not-found control toggle item summary extra after]
               q     :filter
               :or   {selected #{}}}]
  (let [held      (or held selected)
        open      (or open (open-at-rest nodes held))
        filtering (not (str/blank? q))
        resting?  (not (or choosing? filtering))
        nodes     (cond->> nodes
                    filtering (mapv (partial narrow (str/lower-case q))))
        nodes     (mapv counted nodes)
        offered   (mapcat :offered nodes)
        nodes     (cond->> nodes
                    resting? (mapv (partial only-chosen held)))
        nothing-found? (and filtering (every? :hidden? nodes))]
    (controls/fieldset
     ui
     {:tag     (keyword (str "fieldset.chooser." (name k)))
      :legend  legend
      :control (when (and client? control) (control offered))
      :box     (when client?
                 (controls/filter-box (str (name k) "-filter")
                                      (i18n/tr ui "Filter") q
                                      {:input [:filter k]
                                       :focus [:engage k]}))
      :chosen  (count (filter selected offered))
      :total   (count offered)
      :details (cond-> {:open (contains? open :root)
                        :on   {:toggle [:toggle-open k :root]}}
                 busy? (assoc :aria-busy "true"))
      :leave   [:leave k]
      :status  (when nothing-found? not-found)}
     (map (partial node-view
                   {:item      item
                    :summary   (or summary (partial node-summary selected))
                    :extra     (when extra #(extra % resting?))
                    :toggle    (when client? toggle)
                    :open?     (comp (partial contains? open) :id)
                    :on-toggle (fn [{:keys [id]}] [:toggle-open k id])})
          nodes)
     after)))
