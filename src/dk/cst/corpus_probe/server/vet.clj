(ns dk.cst.corpus-probe.server.vet
  "Startup self-checks: does this machine have what the app drives, and
  does the registry read?

  Nothing here stops the server. Each check logs what it found and
  returns it, because none of these failures is visible in a result: a
  missing cwb-* tool leaves search working while corpus pages fail, and a
  missing gawk or locale leaves CQP quietly sorting in corpus order."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.cwb :as cwb]
            [dk.cst.corpus-probe.cwb.corpus :as corpus]
            [dk.cst.corpus-probe.cwb.registry :as registry]
            [dk.cst.corpus-probe.cwb.tools :as tools]
            [dk.cst.corpus-probe.search.cache :as cache]
            [taoensso.telemere :as t])
  (:import [java.io File]))

(def timeout-ms
  "How long a self-check waits for a command that should answer at once."
  5000)

(defn log-problems!
  "Log each of `problems` as an event `id` with the options `f` gives it
  (a function of the problem to its :level and :data), and return them."
  [id f problems]
  (doseq [problem problems]
    ;; the macro reads a map literal as its options and anything else as
    ;; a level, so the options are spelt out here
    (let [{:keys [level data]} (f problem)]
      (t/event! id {:level level :data data})))
  problems)

(defn missing-tools!
  "The `commands` that cannot be launched on this machine at all, whatever
  the others then exit with.

  The cwb-* tools exit non-zero for `-h` and for no arguments alike, so
  their exit code says nothing about whether they are installed; being
  launchable at all does."
  [commands]
  (vec (remove (fn [command]
                 (try
                   (not (cwb/timeout? (cwb/run! [command "-h"] timeout-ms {})))
                   (catch Exception _ false)))
               commands)))

(defn tool-problems!
  "Log the CWB programs `ctx` drives that this machine cannot launch, and
  return them: its :cqp (default cqp) and the cwb-* tools.

  A PATH reaching cqp need not reach the rest, and the failure is quiet:
  search keeps working while corpus pages, frequency lists and metadata
  filters all fail. Logs the CQP version too, since the app generates the
  subset that is safe on the oldest supported one."
  [ctx]
  (let [missing (log-problems! ::tool-missing
                               (fn [command]
                                 {:level :warn :data {:command command}})
                               (missing-tools! (cons (:cqp ctx "cqp")
                                                     tools/tool-names)))]
    (when (empty? missing)
      ;; the app's own timeout is for a query; a banner answers at once
      (t/event! ::cwb-version
                {:data {:cqp (try (cwb/version! (assoc ctx
                                                       :timeout-ms timeout-ms))
                                  (catch Exception _ nil))}}))
    missing))

(def collation-probe
  "Words whose order differs between a Danish collation and byte order: æ,
  ø and å sort after z under the one and before it, in another order
  again, under the other."
  ["æble" "zebra" "åben" "sol" "øje"])

(def probe-line
  "The line number each word of `collation-probe` is written on, which is
  what the pipeline prints back in place of the word."
  (into {} (map-indexed (fn [i word] [word (str (inc i))])) collation-probe))

(def probe-input
  "`collation-probe` as the lines CQP's ExternalSort writes to its temp
  file: a line number, a TAB and the sort key."
  (str/join (map (fn [word] (str (probe-line word) "\t" word "\n"))
                 collation-probe)))

(defn probe-charset
  "The charset LC_ALL value `sort-locale` names, so the probe reaches
  `sort` in the encoding the locale reads; UTF-8 when it names none this
  JVM has.

  (probe-charset \"da_DK.ISO8859-1\")
  ;; => \"ISO8859-1\""
  [sort-locale]
  (let [named (second (str/split (str sort-locale) #"\."))]
    (if (and named (try (java.nio.charset.Charset/isSupported named)
                        (catch Exception _ false)))
      named
      "UTF-8")))

(defn pipeline-order!
  "The line numbers the sort pipeline CQP runs puts `collation-probe` in
  under LC_ALL `sort-locale`; nil when the pipeline cannot be run at all.

  Running CQP's own pipeline (docs/research/gap-nqr-persistence.md §3) is
  the only way to learn what its sort will really do here, because sort,
  gawk and the locale all have to work together and CQP reports none of
  it: it falls back to corpus order and says nothing. The pipeline exits
  with gawk's status, not sort's, so what came back is judged instead."
  [sort-locale]
  (try
    (let [res   (cwb/run!
                 ["sh" "-c" "sort -k 2 -k 1n | gawk '{print $1}'"]
                 timeout-ms
                 {:in      probe-input
                  :charset (probe-charset sort-locale)
                  :env     {"LC_ALL" (str sort-locale)}})
          order (when-not (cwb/timeout? res)
                  (str/split-lines (str/trim (:out res))))]
      (when (= (set order) (set (vals probe-line)))
        order))
    (catch Exception _ nil)))

(defn collator-order
  "The line numbers `sort-locale`'s collator puts `collation-probe` in:
  how the app itself orders metadata and frequency values."
  [sort-locale]
  (mapv probe-line (sort (cwb/->collator {:sort-locale sort-locale})
                         collation-probe)))

(defn collation-problems!
  "Log what would make CQP sort differently from the app itself under the
  :sort-locale of `ctx`, and return it: :sort-locale-unset when there is
  none to follow, :pipeline-broken when CQP's sort pipeline does not run
  here, and :collation-mismatch when it runs but disagrees with the app's
  collator.

  The app orders values with a java.text.Collator while CQP orders a
  concordance with a shell pipeline: the setting is worth nothing unless
  the two agree."
  [{:keys [sort-locale] :as ctx}]
  (let [order    (when-not (str/blank? (str sort-locale))
                   (pipeline-order! sort-locale))
        expected (when order (collator-order sort-locale))]
    (log-problems! ::collation-fallback
                   (fn [problem]
                     {:level :warn
                      :data  {:problem problem :sort-locale sort-locale}})
                   (cond
                     (str/blank? (str sort-locale)) [:sort-locale-unset]
                     (nil? order)                   [:pipeline-broken]
                     (not= order expected)          [:collation-mismatch]
                     :else                          []))))

(defn cache-problems!
  "Log what would stop `ctx` saving query results in its cache directory,
  and return it: :cache-unusable when the directory is neither there nor
  creatable, or cannot be written, and :cache-over-disk when its byte
  budget is larger than the filesystem has left; nothing when `ctx` keeps
  no cache.

  An unusable directory fails every save, so it is an error rather than a
  warning; a budget larger than the disk only fails the saves that fill
  it, and goes on failing them, since nothing evicts until a budget is
  reached that never can be."
  [ctx]
  (let [^File dir (cache/directory ctx)
        _         (when dir (.mkdirs dir))
        ;; whether it can be written is learnt by writing: File.canWrite
        ;; answers from the permission bits alone, which a read-only mount
        ;; or an ACL can contradict
        writable  (and dir (.isDirectory dir)
                       (try (.delete (File/createTempFile "probe" nil dir))
                            (catch Exception _ false)))
        free      (when dir (.getUsableSpace dir))]
    (log-problems! ::cache-problem
                   (fn [problem]
                     {:level (if (= problem :cache-unusable) :error :warn)
                      :data  {:problem    problem
                              :cache-dir  (:cache-dir ctx)
                              :max-bytes  (cache/max-bytes ctx)
                              :free-bytes free}})
                   (cond
                     (nil? dir)                     []
                     (not writable)                 [:cache-unusable]
                     (< free (cache/max-bytes ctx)) [:cache-over-disk]
                     :else                          []))))

(defn corpus-problem!
  "Vet registry entry map `m` against the installation in `ctx`: nil when
  CWB can read its corpus, else [id reason] saying why it cannot.

  The reason is :undefined when CWB has no data for the entry, else the
  type of the failure (:timeout, :cqp, :misaligned) or :unreadable when
  it carries none. The type is safe to log; the message it comes with can
  name server paths."
  [ctx m]
  (let [{:keys [id]} (corpus/overview m)]
    (try
      (when-not (:size (corpus/overview! ctx m))
        [id :undefined])
      (catch Exception e
        [id (get-in (ex-data e) [:error :type] :unreadable)]))))

(defn registry-problems!
  "Read every corpus of the `ctx` registry once, in parallel, log the ones
  CWB cannot open and return them as the [id reason] pairs of
  `corpus-problem!`.

  A registry that holds no corpus at all is logged as a problem rather
  than a clean run, since a mistyped :registry path reads exactly like an
  empty one. Reading them all also caches the corpus index's token counts
  before the first request."
  [ctx]
  (let [started (System/nanoTime)
        entries (registry/entries ctx)
        broken  (log-problems! ::corpus-unreadable
                               (fn [[id reason]]
                                 {:level :warn
                                  :data  {:corpus id :reason reason}})
                               (vec (keep identity
                                          (cwb/pmap-n (cwb/parallelism ctx)
                                                      #(corpus-problem! ctx %)
                                                      entries))))]
    (t/event! ::registry-vetted
              {:level (if (seq entries) :info :warn)
               :data  {:registry   (:registry ctx)
                       :corpora    (count entries)
                       :unreadable (count broken)
                       :ms         (quot (- (System/nanoTime) started)
                                         1000000)}})
    broken))

(comment
  (def ctx {:registry (str (System/getProperty "user.dir")
                           "/dev/corpus/registry")
            :sort-locale "da_DK.UTF-8"})

  (tool-problems! ctx)
  ;; => []

  (collation-problems! ctx)
  ;; => []

  (collation-problems! {:sort-locale "zz_ZZ.UTF-8"})
  ;; => [:collation-mismatch]

  (registry-problems! ctx)
  ;; => []
  #_.)
