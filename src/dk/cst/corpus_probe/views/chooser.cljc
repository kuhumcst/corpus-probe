(ns dk.cst.corpus-probe.views.chooser
  "A chooser over a tree of checkboxes behind disclosures, with a box to
  search it; the corpus chooser and the metadata filter are both one. A
  node is {:id :label :items :nodes} and a leaf {:id :text}, the ids
  naming leaves in the selection and nodes among the open disclosures.

  Nothing is ever removed from the tree, only marked `:hidden?`: a
  checkbox out of the document is a choice dropped from the search. The
  pure rules deciding what is shown the client applies to its own state."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.views.widgets :as widgets]))

(defn answers?
  "True when node or leaf `x` answers `q`, a lower-cased fragment of a
  name: its `:text`, or its `:label` without one, contains it."
  [q x]
  (let [s (or (:text x) (:label x))]
    (boolean (and s (str/includes? (str/lower-case (str s)) q)))))

(defn narrow
  "Mark `:hidden?` everything in `node` that does not answer `q`, and
  `node` itself when nothing in it survives; a node whose own label
  answers keeps everything in it."
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
  and not one the filter has hidden."
  [{:keys [items nodes]}]
  (concat (->> items (remove :hidden?) (remove :disabled?) (map :id))
          (mapcat offered nodes)))

(defn counted
  "Stamp `node` and the nodes under it with `:offered`, the ids each of
  them offers (see `offered`).

  Stamped before the resting view hides what is not chosen, so every
  count is of what the filter left rather than of the selection alone."
  [node]
  (let [node (update node :nodes #(mapv counted %))]
    (assoc node :offered (vec (offered node)))))

(defn node-seq
  "Every node among `nodes` and under them, parents first."
  [nodes]
  (mapcat #(tree-seq :nodes :nodes %) nodes))

(defn only-chosen
  "The resting view of `node`: everything the reader has not chosen is
  marked `:hidden?`, `held` saying what counts as chosen, so what shows
  is what the search will read.

  A leaf stays when held, a node when something under it is held or it
  is `:in-force?`. A labelled node whose leaves are all held hides them,
  its summary row speaking for them; a bare list has no such row."
  [held {:keys [items nodes offered in-force? label] :as node}]
  (let [whole? (and label (seq offered) (every? held offered))
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

(defn open-at-rest
  "Which disclosures stand open while nobody is choosing from `nodes`,
  `selected` being the leaves they have chosen: every node chosen in
  part, by id, and `:root` whenever the resting view shows anything.

  A node chosen whole, or not at all, is shut: its own row says which,
  having both a label and a count. The root has only the count, so
  shutting it would leave a ticked box that names nothing."
  [nodes selected]
  (let [nodes (map counted nodes)
        part? (fn [offered]
                (< 0 (count (filter selected offered)) (count offered)))
        show? (fn [node] (not (:hidden? (only-chosen selected node))))]
    (into (if (some show? nodes) #{:root} #{})
          (comp (filter (comp part? :offered)) (map :id))
          (node-seq nodes))))

(defn matching
  "The ids of the nodes among `nodes` holding something that answers `q`
  (see `narrow`): what typing `q` opens."
  [q nodes]
  (->> nodes
       (map (comp counted (partial narrow (str/lower-case q))))
       (node-seq)
       (remove :hidden?)
       (map :id)))

(defn node-count
  "How many of the leaves a counted `node` offers are in the set
  `selected`, beside how many there are, as a badge."
  [selected {:keys [offered]}]
  (widgets/count-badge (count (filter selected offered)) (count offered)))

(defn node-summary
  "What the disclosure of a counted `node` says of it by default: its
  label and its count over the set `selected`."
  [selected node]
  (list (:label node) " " (node-count selected node)))

(defn toggled
  "Put `control` beside `disclosure` as one row, the row being there
  whether or not there is a control to put in it."
  [control disclosure]
  ;; always the row, even with no control: an element that changes kind
  ;; rebuilds everything after it, the control a reader is pressing included.
  ;; The control stands before the disclosure rather than in its summary:
  ;; a summary is a button, and a button need not expose controls in it
  [:div.chooser-group control disclosure])

(defn node-view
  "One `node` of the tree, drawn by the `opts` of the chooser it belongs
  to: its leaves by `:item` and the nodes under it recursively, in a
  disclosure; a node without a label is its bare list, and has no
  control."
  [{:keys [item summary extra toggle open? on-toggle] :as opts}
   {:keys [label items nodes hidden?] :as node}]
  (let [entries    [:ul.chooser-list
                    (map item items)
                    (map (fn [n] [:li (node-view opts n)]) nodes)]
        ;; nested details with native checkboxes, not the ARIA tree
        ;; pattern: it works without a script, submits every box, and
        ;; promises no arrow-key navigation it does not have
        disclosure [:details (cond-> (assoc (widgets/hidden-attrs hidden?)
                                            :open (boolean (open? node)))
                               on-toggle (assoc :on {:toggle (on-toggle node)}))
                    [:summary.chooser-summary (summary node)]
                    (when extra (extra node))
                    entries]]
    (if label
      ;; a hidden node gets no control: it would stand outside the hidden
      ;; disclosure, a checkbox beside nothing
      (toggled (when (and toggle (not hidden?)) (toggle node))
               disclosure)
      entries)))

(defn filter-box
  "A box narrowing what is under it to whatever answers what is typed in
  it: `id` names it, `label` says what it is for, `q` is what it holds
  and `actions` are what it dispatches, `:input` on every change and
  `:focus` as it takes focus."
  [id label q actions]
  ;; no name, so it is not submitted: what was typed to find a thing is
  ;; not the search. Enter is swallowed for the same reason
  [:input.chooser-find
   {:id           id
    :type         "search"
    :placeholder  label
    :aria-label   label
    :value        (or q "")
    :autocomplete "off"
    :on           (assoc actions :keydown [:swallow-enter :event/key])}])

(defn fieldset
  "The box a long list stands in, named for the client by list `k` in a
  data attribute and worded in `ui`: its `:legend` and `:class`, the
  `:control` taking every entry at once, the `:box` narrowing the
  disclosure the `entries` are behind, how many of the `:total` entries
  are `:chosen`, the `:details` attributes of the disclosure, the
  `:status` the box reports and what focus leaving it dispatches,
  `:leave`."
  [ui k {:keys [class legend control box chosen total details status leave]}
   & entries]
  [:fieldset.chooser.box (cond-> {:data-list (name k)}
                           class (assoc :class class)
                           leave (assoc :on {:focusout leave}))
   [:legend legend]
   ;; the row keeps its kind with or without a control, so the live
   ;; region after it is never rebuilt; a rebuilt one announces nothing
   [:div.chooser-group
    control
    (into [:details details
           ;; the box sits in the summary, the one line in view whether the
           ;; list is open or shut; a click in it works the box, not the
           ;; disclosure
           [:summary.chooser-summary
            {:aria-label (str chosen " " (i18n/tr ui "of") " "
                              total " " (i18n/tr ui "selected"))}
            box
            (widgets/count-badge chosen total)]]
          entries)]
   ;; outside the disclosure: a live region revealed from inside one as
   ;; it fills announces nothing either
   (when box (widgets/status "chooser-status" status))])

(defn chooser
  "The fieldset over the `nodes` of list `k` in `ui`, `k` being the name
  the client knows the list by: the leaves as checkboxes, the ids in
  `:selected` checked, behind one disclosure counting them.

  Two faces, by `:choosing?`: at rest only what is chosen shows; while
  choosing, everything. A `:filter` overrides both, and `:not-found` is
  said when nothing answers it. `:held` is what the resting face treats
  as chosen, the selection by default; `:open` is the set of open
  disclosures, `:root` for the fieldset's own. The instance supplies the
  rest: `:item`, `:summary`, `:extra`, `:control`, `:toggle` and
  `:after` draw its parts, and the controls and the box are rendered
  only where `:client?` runs."
  [ui k nodes {:keys [selected held open choosing? client? busy? class
                      legend not-found control toggle item summary extra
                      after]
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
    (fieldset
     ui k
     {:class   class
      :legend  legend
      :control (when (and client? control) (control offered))
      :box     (when client?
                 (filter-box (str (name k) "-filter")
                             (i18n/tr ui "Filter") q
                             {:input [:filter k :event.target/value]
                              :focus [:engage k]}))
      :chosen  (count (filter selected offered))
      :total   (count offered)
      :details (cond-> {:open (contains? open :root)
                        :on   {:toggle [:toggle-open k :root
                                        :event.target/open]}}
                 busy? (assoc :aria-busy "true"))
      :leave   [:leave k :event/focus-left?]
      :status  (when nothing-found? not-found)}
     (map (partial node-view
                   {:item      item
                    :summary   (or summary (partial node-summary selected))
                    :extra     (when extra #(extra % resting?))
                    :toggle    (when client? toggle)
                    :open?     (comp (partial contains? open) :id)
                    :on-toggle (fn [{:keys [id]}]
                                 [:toggle-open k id :event.target/open])})
          nodes)
     after)))
