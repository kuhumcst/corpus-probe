(ns dk.cst.corpus-probe.client.lists
  "The two lists a reader chooses from, the corpus chooser and the
  metadata filter, and the rules the client applies to their state: pure
  functions over the state map, which the pure step calls."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.views.chooser :as chooser]
            [dk.cst.corpus-probe.views.corpus :as corpus-views]
            [dk.cst.corpus-probe.views.search.filter :as filter-views]))

(def lists
  "The two lists by the name their controls send: `:tree` builds each
  list's tree from the state with what is held chosen kept in it, and
  `:chosen` reads its selection out as the set of leaf ids the tree
  names.

  A list's own state is under `:lists`: `:open`, the disclosures standing
  open, `:root` for its own; `:choosing?`, while the reader is choosing;
  `:unticked`, what they unticked at rest since they last left; and
  `:filter`, what its box holds."
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
  "What the resting view of list `k` treats as chosen in `state`: its
  selection, and what was unticked at rest since the reader last left."
  [state k]
  (into ((:chosen (lists k)) state) (get-in state [:lists k :unticked])))

(defn rest-open
  "The disclosures of list `k` that stand open at rest in `state` with
  `held` chosen (see dk.cst.corpus-probe.views.chooser/open-at-rest)."
  [state k held]
  (chooser/open-at-rest ((:tree (lists k)) state held) held))

(defn settle
  "Put list `k` of `state` at rest: nobody choosing from it, nothing
  unticked, and open exactly what the resting view opens over what is
  chosen now, except the root, which stays shut if the reader shut it."
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
  "Record in `state` that the reader ticked the `ids` of list `k`, or
  unticked them when `unticking?`, and adjust what the list shows.

  Nothing moves while they are choosing. At rest an unticked row stays
  until they leave (see `held`), a node may shut but never open, and the
  root follows the resting view: a node springing open would move the
  page, and a shut root would answer a tick with a ticked box and no list."
  [state k ids unticking?]
  (let [{:keys [choosing? open]} (get-in state [:lists k])
        state (update-in state [:lists k :unticked]
                         (if unticking? into #(apply disj % ids)) ids)]
    (cond-> state
      (not choosing?)
      (assoc-in [:lists k :open]
                (let [resting (rest-open state k (held state k))]
                  (cond-> (set/intersection open resting)
                    (contains? resting :root) (conj :root)))))))

(defn engage
  "Set list `k` of `state` to choosing: nothing in it is hidden until the
  reader leaves, and its root stands open, since the box that asks for
  this sits in the root's summary."
  [state k]
  (update-in state [:lists k]
             #(-> % (assoc :choosing? true) (update :open conj :root))))

(defn leave
  "Put list `k` of `state` at rest now the reader has gone elsewhere (see
  `settle`), unless its filter box still holds something: a filter in
  force is a reader still looking. `state` unchanged for a list already
  at rest."
  [state k]
  (let [{:keys [choosing? unticked] q :filter} (get-in state [:lists k])]
    (if (and (str/blank? q) (or choosing? (seq unticked)))
      (settle state k)
      state)))

(defn toggle-open
  "Record in `state` that the reader worked disclosure `id` of list `k`,
  leaving it `open?`, and answer what that says: one coming open while
  nobody is choosing is the reader asking to choose (see `engage`), and
  the root shutting is them finishing (see `leave`)."
  [state k id open?]
  (let [{:keys [open choosing?]} (get-in state [:lists k])]
    ;; a <details> fires toggle when this client opens or shuts it too,
    ;; an echo of what the state already says
    (if (= open? (contains? open id))
      state
      (let [state (update-in state [:lists k :open] (if open? conj disj) id)]
        (cond
          (and open? (not choosing?))    (engage state k)
          (and (not open?) (= :root id)) (leave state k)
          :else                          state)))))

(defn apply-filter
  "Narrow list `k` of `state` to whatever answers `q`, opening every
  disclosure that holds something: a reader who has asked where
  something is has asked to be shown it."
  [state k q]
  (cond-> (assoc-in state [:lists k :filter] q)
    (not (str/blank? q))
    (update-in [:lists k :open] into
               (chooser/matching q ((:tree (lists k)) state (held state k))))))

(defn select-corpora
  "The selected corpus IDs `corpus` with every ID in `ids` added when
  `add?`, and with all of them removed otherwise; sorted, so that a
  selection reads the same however the reader arrived at it."
  [corpus ids add?]
  (let [selected (set corpus)]
    (vec (sort (if add? (into selected ids) (reduce disj selected ids))))))

(defn drop-values
  "Take every value in `values` away from `attr` of `selected`, the map
  of metadata attribute to the values chosen under it."
  [selected attr values]
  (let [chosen (reduce disj (set (get selected attr)) values)]
    (if (seq chosen)
      (assoc selected attr chosen)
      ;; dropped rather than kept empty, or it would still count as one
      ;; the reader filters by
      (dissoc selected attr))))

(defn choose-values
  "Add every value in `values` under `attr` of `selected` (see
  `drop-values`), or take them all away when they are all there already:
  one rule for one value and for every value of an attribute, as the
  corpus chooser has for a corpus and a folder."
  [selected attr values]
  (let [chosen (set (get selected attr))]
    (if (every? chosen values)
      (drop-values selected attr values)
      (assoc selected attr (into chosen values)))))

(defn chosen-corpora
  "The selected corpus IDs of `state` in a settled order, which is what
  the metadata filters are asked for and remembered by."
  [state]
  (vec (sort (get-in state [:params :corpus]))))

(defn filters-stale?
  "True when the metadata filters `state` holds are not the ones the
  corpora now selected offer, and the reader is looking at them: a
  selection is changed several times before anyone asks what metadata
  it carries. Looking is the filter standing open, or nothing on show at
  all, which a reader cannot open to ask; and never for no corpora,
  which a search cannot run on and the server answers with nothing."
  [{:keys [filters-for filter-controls] :as state}]
  (and (or (contains? (get-in state [:lists :values :open]) :root)
           (not (filter-views/filterable? filter-controls)))
       (seq (chosen-corpora state))
       (not= (chosen-corpora state) filters-for)))
