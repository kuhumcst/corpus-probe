(ns dk.cst.corpus-probe.cwb.corpus
  "What CQP reports about a corpus, memoised per corpus until it is
  re-encoded: its attributes from `show cd;`, its facts from `info;` and
  the overview the corpus index shows; the context a corpus's batches run
  under; the predicates over its attribute descriptions; and the registry
  organised for display, its corpora summarised and grouped in folders.

  The registry itself is read by dk.cst.corpus-probe.cwb.registry, so no
  corpus configuration is duplicated in the application: everything the
  UI knows about a corpus derives from its entry there plus these two
  CQP commands."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.cwb :as cwb]
            [dk.cst.corpus-probe.cwb.command :as command]
            [dk.cst.corpus-probe.cwb.parse :as parse]
            [dk.cst.corpus-probe.cwb.registry :as registry]))

(defonce ^{:doc "Cache of per-corpus facts: a delay per key
  [registry corpus label build-stamp]. The stamp keys stale entries out
  when a corpus is re-encoded under a running JVM (see
  dk.cst.corpus-probe.cwb.registry/build-stamp)."}
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
  `label` (a keyword or vector of this namespace's or the caller's own,
  so that two callers cannot share a fact by accident), computing them
  with no-arg `f` on a miss.

  Concurrent misses share one computation: the cache holds a delay per key,
  so the first caller runs `f` while the others wait for its value. A
  computation that throws is forgotten again, so the next caller retries.
  Entries live until the corpus is re-encoded or its registry entry
  changes (see dk.cst.corpus-probe.cwb.registry/build-stamp); the entry
  they supersede is dropped then."
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
  "Return `ctx` configured for `corpus`: validates the corpus name (it is
  spliced into commands outside the QueryLock sandbox, see
  dk.cst.corpus-probe.cwb.command/valid-corpus-name) and sets the corpus's
  own charset for the round trip (see
  dk.cst.corpus-probe.cwb.registry/charset)."
  [ctx corpus]
  (command/valid-corpus-name corpus)
  (assoc ctx :charset (registry/charset ctx corpus)))

(defn cqp-facts!
  "Run CQP `command` against activated `corpus` (an uppercase CQP corpus
  name) via `ctx` and parse its output lines with `parse-fn`, cached per
  registry + corpus + command until the corpus is re-encoded.

  The corpus name is validated first, since it is spliced into the
  activation command; the batch runs in the corpus's own charset (see
  `corpus-ctx`), read on a miss along with the facts."
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
  via the installation in `ctx`, cached until the corpus is re-encoded.

  Unlike the registry, this marks which s-attributes carry annotation values
  (:values?), which decides what `tabulate` can extract per hit."
  [ctx corpus]
  (cqp-facts! ctx corpus "show cd;" parse/show-cd->attributes))

(defn info!
  "Return the corpus facts of `corpus` as reported by `info;` via the
  installation in `ctx` (see dk.cst.corpus-probe.cwb.parse/info->map),
  cached until the corpus is re-encoded."
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
  missing (`:phantom?` in the ex-data).

  The two tools say it differently and mean the same thing, and it stays
  true until the entry changes."
  [e]
  (let [{:keys [error phantom?]} (ex-data e)]
    (boolean (or phantom?
                 (re-find #"is undefined" (str (:message error)))))))

(defn overview!
  "The `overview` of registry entry map `m` plus its :size in tokens via
  `ctx`, cached until the entry changes.

  The size is nil for a phantom entry (see `phantom?`), an outcome cached
  like any other so a phantom costs one process rather than one per
  request; any other failure to read the size propagates uncached, so a
  transient one is retried.

  The cache key follows the registry entry, not the corpus data, so
  restoring the data of a phantom takes a restart to be noticed."
  [ctx m]
  (let [{:keys [id] :as summary} (overview m)]
    (facts! ctx id ::overview
            (fn []
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
  "The description among `attributes` (as `attributes!` reports them, or
  the per-attribute statistics of a cwb-* tool) of the attribute named
  `attr`, a keyword or string; nil when the corpus lacks it."
  [attributes attr]
  (let [attr (keyword attr)]
    (some #(when (= attr (:name %)) %) attributes)))

(def unit-attrs
  "The names a unit of text goes by among a corpus's s-attributes, in the
  order they are looked for: a sentence is `s` in CWB's own corpora and
  `sentence` in the KU ones, a paragraph `p` or `paragraph`. The units
  themselves are dk.cst.corpus-probe.cqp/units."
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
  `entries`, when its entry records a plausible one (see
  dk.cst.corpus-probe.cwb.registry/language)."
  [entries corpus]
  (some (fn [{:keys [id] :as m}]
          (when (= corpus (str/upper-case id))
            (registry/language m)))
        entries))

(defn split-known
  "Split the `selected` corpus names into [known unknown] by the registry
  `entries`, so that only names the registry has reach a command and the
  rest are reported without spawning anything."
  [entries selected]
  (let [known? (set (map (comp str/upper-case :id) entries))]
    [(filterv known? selected) (vec (remove known? selected))]))

(defn readable-corpora!
  "The names of the registry `entries` CWB can read right now, via `ctx`,
  in registry order.

  This is what a request that names no corpus searches: exactly the set
  the chooser would let a reader tick, since it disables the rest. The
  overviews are cached, and the chooser asks for the same ones on every
  page, so this costs nothing on a warm cache."
  [ctx entries]
  (into []
        (comp (filter :size) (map (comp str/upper-case :id)))
        (cwb/pmap-n (cwb/parallelism ctx)
                    #(cwb/attempt (:id %) (fn [] (overview! ctx %)))
                    entries)))

(defn corpus-tree!
  "The registry `entries` (maps as from
  dk.cst.corpus-probe.cwb.registry/entries) summarized via `ctx`, in
  parallel since each summary is a CQP round trip on a cache miss, and
  grouped by its configured folder tree (see
  dk.cst.corpus-probe.cwb.registry/grouped-corpora). An entry whose size
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
