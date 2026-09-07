(ns dk.cst.corpus-probe.client.lists
  "The two lists a reader chooses from, the corpus chooser and the
  metadata filter, and the rules the client applies to their state: pure
  functions over the state map, which
  dk.cst.corpus-probe.client.actions/act calls. Both lists are one
  chooser (see dk.cst.corpus-probe.views.chooser/chooser)."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.views.chooser :as chooser]
            [dk.cst.corpus-probe.views.corpus :as corpus-views]
            [dk.cst.corpus-probe.views.search.filter :as filter-views]))

(def lists
  "The two lists by the name their controls send, each with what the
  client needs to keep its state apart from the rest of the page.

  That state is under `:lists` in the state, per list: `:open`, the set
  of its disclosures standing open, `:root` for its own and the id of
  each node for theirs, which the document mirrors exactly (see
  `toggle-open`); `:choosing?`, whether the reader is choosing from it,
  which shows everything in it rather than only what is chosen (see
  `engage`); `:unticked`, what they have unticked at rest since they
  last left, which stays in place until then (see `tick`); and
  `:filter`, what its box holds (see `apply-filter`).

  Here, per list: `:tree` builds its tree from the state, with what is
  held chosen kept in it, and `:chosen` reads its selection out of the
  state as the set of leaf ids the tree names."
  {:corpora {:tree   (fn [state _]
                       (corpus-views/corpus-tree (i18n/->ui (:lang state))
                                                 (:folders state)))
             :chosen (fn [state] (set (get-in state [:params :corpus])))}
   :values  {:tree   (fn [state held]
                       (filter-views/filter-tree (:filter-controls state)
                                                 held))
             :chosen (fn [state]
                       (filter-views/filter-pairs
                        (get-in state [:filter-controls :selected])))}})

(defn held
  "What the resting view of list `k` (see `lists`) treats as chosen in
  `state`: its selection, and what was unticked at rest since the reader
  last left."
  [state k]
  (into ((:chosen (lists k)) state) (get-in state [:lists k :unticked])))

(defn rest-open
  "The disclosures of list `k` (see `lists`) that stand open at rest in
  `state` with `held` chosen (see
  dk.cst.corpus-probe.views.chooser/open-at-rest)."
  [state k held]
  (chooser/open-at-rest ((:tree (lists k)) state held) held))

(defn settle
  "`state` with list `k` (see `lists`) at rest: nobody choosing from it,
  nothing unticked, and open exactly what the resting view opens over
  what is chosen now, except the root, which stays shut if the reader
  shut it: a reader who folded the list up has said so."
  [state k]
  (let [resting (rest-open state k ((:chosen (lists k)) state))]
    (update-in state [:lists k] assoc
               :choosing? false
               :unticked  #{}
               :open      (cond-> resting
                            (not (contains? (get-in state [:lists k :open])
                                            :root))
                            (disj :root)))))

(defn tick
  "`state` once the `ids` of list `k` (see `lists`) have been ticked or,
  when `unticking?`, unticked.

  At rest, what is unticked stays in place until the reader leaves (see
  `held`), so that a box unticked by mistake is there to be ticked
  again, and only what the change leaves chosen whole or not at all
  shuts, which is never anything the reader is looking into. While they
  are choosing, nothing moves at all."
  [state k ids unticking?]
  (let [{:keys [choosing? open]} (get-in state [:lists k])
        state (update-in state [:lists k :unticked]
                         (if unticking? into #(apply disj % ids)) ids)]
    (cond-> state
      (not choosing?)
      (assoc-in [:lists k :open]
                (set/intersection open (rest-open state k (held state k)))))))

(defn engage
  "`state` with the reader choosing from list `k` (see `lists`): nothing
  in it is hidden from here until they leave, and its root stands open,
  since the box that asks for this sits in the root's summary and is
  reached whether the root is open or shut."
  [state k]
  (update-in state [:lists k]
             #(-> % (assoc :choosing? true) (update :open conj :root))))

(defn leave
  "`state` with list `k` (see `lists`) at rest, the reader having gone
  elsewhere (see `settle`), unless its filter box still holds something:
  a filter in force is a reader still looking, and the list stays as the
  filter left it. `state` itself for a list already at rest with nothing
  unticked, which is most lists most of the time."
  [state k]
  (let [{:keys [choosing? unticked] q :filter} (get-in state [:lists k])]
    (if (and (str/blank? q) (or choosing? (seq unticked)))
      (settle state k)
      state)))

(defn toggle-open
  "`state` with disclosure `id` of list `k` (see `lists`) recorded as
  `open?`, the reader having worked it, and what that says answered: a
  disclosure coming open while nobody is choosing is the reader asking
  to choose (see `engage`), and the root shutting is them finishing (see
  `leave`).

  A <details> fires its own toggle when this client opens or shuts it
  too. That echo says what the state already says, so it is ignored, and
  only a toggle that differs from the state is the reader's: the state
  is what the document was rendered from, so the two differ only where a
  reader has worked the disclosure since."
  [state k id open?]
  (let [{:keys [open choosing?]} (get-in state [:lists k])]
    (if (= open? (contains? open id))
      state
      (let [state (update-in state [:lists k :open] (if open? conj disj) id)]
        (cond
          (and open? (not choosing?))    (engage state k)
          (and (not open?) (= :root id)) (leave state k)
          :else                          state)))))

(defn apply-filter
  "`state` with list `k` (see `lists`) narrowed to whatever answers `q`,
  and every disclosure holding something that does open: a reader who
  has asked where something is has asked to be shown it."
  [state k q]
  (cond-> (assoc-in state [:lists k :filter] q)
    (not (str/blank? q))
    (update-in [:lists k :open] into
               (chooser/matching q ((:tree (lists k)) state (held state k))))))

(defn select-corpora
  "The selected corpus IDs `corpus` with every ID in `ids` added when
  `add?`, and with all of them removed otherwise.

  Sorted, so that a selection reads the same however the reader arrived
  at it, and a set throughout, so that selecting a folder whose corpora
  are already selected cannot list one of them twice."
  [corpus ids add?]
  (let [selected (set corpus)]
    (vec (sort (if add? (into selected ids) (reduce disj selected ids))))))

(defn choose-values
  "`selected`, each metadata attribute mapped to the values chosen under
  it, with every value in `values` added under `attr`, or all of them
  taken away when they are all there already.

  One rule for one value and for every value of an attribute, as the
  corpus chooser has one rule for a corpus and for a folder: a box that is
  on turns off, and an attribute only partly chosen fills rather than
  clearing the part the reader already had.

  An attribute left with nothing chosen is dropped rather than kept empty,
  so that an attribute the reader has finished with does not go on being
  counted as one they are filtering by."
  [selected attr values]
  (let [chosen (set (get selected attr))
        chosen (if (every? chosen values)
                 (reduce disj chosen values)
                 (into chosen values))]
    (if (seq chosen)
      (assoc selected attr chosen)
      (dissoc selected attr))))

(defn chosen-corpora
  "The selected corpus IDs of `state` in a settled order, which is what
  the metadata filters are asked for and remembered by."
  [state]
  (vec (sort (get-in state [:params :corpus]))))

(defn filters-stale?
  "True when the metadata filters `state` holds are not the ones the
  corpora now selected offer, and the reader is looking at them.

  Only while the filter stands open: filters nobody has opened are
  filters nobody has to fetch, and a corpus selection is usually changed
  several times before anyone asks what metadata it carries. Unless
  nothing is on show at all (see
  dk.cst.corpus-probe.views.search.filter/filterable?), since a fieldset
  that is not there is one the reader cannot open to ask. And never for
  no corpora: a search cannot run without one, and what the server
  answers for none is nothing, which would only take the fieldset away."
  [{:keys [filters-for filter-controls] :as state}]
  (and (or (contains? (get-in state [:lists :values :open]) :root)
           (not (filter-views/filterable? filter-controls)))
       (seq (chosen-corpora state))
       (not= (chosen-corpora state) filters-for)))
