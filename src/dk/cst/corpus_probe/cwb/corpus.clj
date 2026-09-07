(ns dk.cst.corpus-probe.cwb.corpus
  "What CQP reports about a corpus, memoised per corpus until it is
  re-encoded: its attributes from `show cd;`, its facts from `info;` and
  the overview the corpus index shows; the predicates over its attribute
  descriptions; and the registry organised for display. Everything the UI
  knows about a corpus derives from its registry entry plus these two CQP
  commands."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.cwb :as cwb]
            [dk.cst.corpus-probe.cwb.command :as command]
            [dk.cst.corpus-probe.cwb.parse :as parse]
            [dk.cst.corpus-probe.cwb.registry :as registry]))

(defonce ^{:doc "Cache of per-corpus facts: a delay per key [registry
  corpus label build-stamp], the stamp keying stale entries out when a
  corpus is re-encoded under a running JVM."}
  facts-cache
  (atom {}))

(defn- without-superseded
  "Remove from `cache` the entries of the same registry, corpus and label
  as key `k` (older stamps of the same facts)."
  [cache [registry corpus label :as k]]
  (into {} (remove (fn [[[r c l] _]]
                     (and (= r registry) (= c corpus) (= l label))))
        cache))

(defn facts!
  "Return the cached facts of `corpus` in `ctx` under cache key part
  `label` (a keyword or vector of the caller's own, so that two callers
  cannot share a fact by accident), computing them with no-arg `f` on a
  miss. Concurrent misses share one computation, and one that throws is
  forgotten so the next caller retries."
  [{:keys [registry] :as ctx} corpus label f]
  (let [k [registry corpus label (registry/build-stamp ctx corpus)]
        d (get (swap! facts-cache
                      (fn [cache]
                        (if (contains? cache k)
                          cache
                          (assoc (without-superseded cache k) k (delay (f))))))
               k)]
    (try @d
         (catch Exception e
           (swap! facts-cache
                  (fn [cache]
                    (cond-> cache (identical? d (get cache k)) (dissoc k))))
           (throw e)))))

(defn corpus-ctx
  "Return `ctx` configured for `corpus`: the name validated (it is spliced
  into commands outside the QueryLock sandbox) and the corpus's own
  :charset set for the round trip."
  [ctx corpus]
  (command/valid-corpus-name corpus)
  (assoc ctx :charset (registry/charset ctx corpus)))

(defn cqp-facts!
  "Run CQP `command` against activated `corpus` (an uppercase CQP corpus
  name) via `ctx` and parse its output lines with `parse-fn`, cached per
  registry, corpus and command until the corpus is re-encoded."
  [ctx corpus command parse-fn]
  (command/valid-corpus-name corpus)
  (facts! ctx corpus [::cqp command]
          (fn []
            (-> (cwb/batch! (corpus-ctx ctx corpus) corpus command
                            [(str corpus ";") command])
                (second)
                (parse-fn)))))

(defn attributes!
  "Return the attribute descriptions of `corpus` as reported by `show cd;`
  via `ctx`, cached until the corpus is re-encoded. Unlike the registry,
  this marks which s-attributes carry annotation values (:values?)."
  [ctx corpus]
  (cqp-facts! ctx corpus "show cd;" parse/show-cd->attributes))

(defn info!
  "Return the corpus facts of `corpus` as reported by `info;` via `ctx`
  (see dk.cst.corpus-probe.cwb.parse/info->map), cached until the corpus
  is re-encoded."
  [ctx corpus]
  (cqp-facts! ctx corpus "info;" parse/info->map))

(defn overview
  "Summarize registry entry map `m` for the corpus index: its uppercase CQP
  :id, its :title (the registry NAME, when set) and its :language (see
  dk.cst.corpus-probe.cwb.registry/language)."
  [{:keys [id name] :as m}]
  {:id       (str/upper-case id)
   :title    (not-empty name)
   :language (registry/language m)})

(defn phantom?
  "True when exception `e` says CWB has no data for a registry entry: CQP
  reporting the corpus as undefined, or a cwb-* tool reporting its data as
  missing (`:phantom?` in the ex-data)."
  [e]
  (let [{:keys [error phantom?]} (ex-data e)]
    (boolean (or phantom?
                 (re-find #"is undefined" (str (:message error)))))))

(defn overview!
  "The `overview` of registry entry map `m` plus its :size in tokens via
  `ctx`, cached until the entry changes; the size is nil for a phantom
  entry (see `phantom?`)."
  [ctx m]
  (let [{:keys [id] :as summary} (overview m)]
    (facts! ctx id ::overview
            (fn []
              ;; a phantom is cached like any outcome, costing one process
              ;; rather than one per request; any other failure propagates
              ;; uncached, so a transient one is retried
              (assoc summary
                     :size (try (:size (info! ctx id))
                                (catch clojure.lang.ExceptionInfo e
                                  (if (phantom? e) nil (throw e)))))))))

(defn attr-names
  "The names of the `attributes` matching `pred`."
  [pred attributes]
  (->> (filter pred attributes)
       (mapv :name)))

(defn positional?
  "True when attribute description `m` is a positional attribute."
  [m]
  (= :positional (:type m)))

(defn annotated-s-attr?
  "True when attribute description `m` is an s-attribute carrying values."
  [{:keys [type values?] :as m}]
  (and (= :structural type) values?))

(defn countable-attr?
  "True when attribute description `m` is one whose values a result can
  be counted or narrowed by: a positional attribute, or a structural one
  carrying values."
  [m]
  (or (positional? m) (annotated-s-attr? m)))

(defn attribute
  "The description among `attributes` of the attribute named `attr`, a
  keyword or string; nil when the corpus lacks it."
  [attributes attr]
  (let [attr (keyword attr)]
    (some #(when (= attr (:name %)) %) attributes)))

(def unit-attrs
  "The names a unit of text goes by among a corpus's s-attributes, in the
  order they are looked for: a sentence is `s` in CWB's own corpora and
  `sentence` in the KU ones, a paragraph `p` or `paragraph`."
  {:sentence  [:s :sentence]
   :paragraph [:p :paragraph]
   :text      [:text]})

(defn unit-attr
  "The s-attribute among `attributes` (descriptions as `attributes!`
  reports them) marking `unit`, a key of `unit-attrs`; nil when the
  corpus marks none."
  [attributes unit]
  (some (set (attr-names #(= :structural (:type %)) attributes))
        (unit-attrs unit)))

(defn corpus-lang
  "The language code of the corpus named `corpus` among the registry
  `entries`, when its entry records a plausible one."
  [entries corpus]
  (some (fn [{:keys [id] :as m}]
          (when (= corpus (str/upper-case id))
            (registry/language m)))
        entries))

(defn split-known
  "Split the `selected` corpus names into [known unknown] by the registry
  `entries`, so that only names the registry has reach a command."
  [entries selected]
  (let [known? (set (map (comp str/upper-case :id) entries))]
    [(filterv known? selected) (vec (remove known? selected))]))

(defn readable-corpora!
  "The names of the registry `entries` CWB can read right now, via `ctx`,
  in registry order: what a request that names no corpus searches, and
  exactly the set the chooser lets a reader tick."
  [ctx entries]
  (into []
        (comp (filter :size) (map (comp str/upper-case :id)))
        (cwb/pmap-n (cwb/parallelism ctx)
                    #(cwb/attempt (:id %) (fn [] (overview! ctx %)))
                    entries)))

(defn corpus-tree!
  "The registry `entries` summarized via `ctx`, in parallel, and grouped
  by the configured folder tree (see
  dk.cst.corpus-probe.cwb.registry/grouped-corpora); an entry whose size
  cannot be read right now is summarized sizeless, and uncached."
  [ctx entries]
  (registry/grouped-corpora
   (:folders ctx)
   (vec (cwb/pmap-n (cwb/parallelism ctx)
                    (fn [entry]
                      (let [summary (cwb/attempt (:id entry)
                                                 #(overview! ctx entry))]
                        (if (:error summary) (overview entry) summary)))
                    entries))))
