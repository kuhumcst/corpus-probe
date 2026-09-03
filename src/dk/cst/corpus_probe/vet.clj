(ns dk.cst.corpus-probe.vet
  "Startup self-checks: does this machine have what the app drives, and
  does the registry read?

  Nothing here stops the server. Each check logs what it found and returns
  it, because none of these failures is visible in a result: a missing
  cwb-* tool leaves search working while corpus pages and metadata filters
  fail, a missing gawk or locale leaves CQP quietly sorting in corpus
  order, and a registry entry whose data are gone only shows up when
  someone opens it.

  A corpus that cannot be read is not dropped. The registry says it
  exists, so the chooser keeps it, disabled, and its info page says CWB
  has no data for it (see dk.cst.corpus-probe.corpus/phantom?)."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.corpus :as corpus]
            [dk.cst.corpus-probe.cqp :as cqp]
            [dk.cst.corpus-probe.search :as search]
            [taoensso.telemere :as t]))

(def timeout-ms
  "How long a self-check waits for a command that should answer at once."
  5000)

(defn installed?
  "True when `command` can be launched on this machine at all, whatever it
  then exits with.

  The cwb-* tools exit non-zero for `-h` and for no arguments alike, so
  their exit code says nothing about whether they are installed; being
  launchable at all does."
  [command]
  (try
    (not= ::cqp/timeout (cqp/run-process! [command "-h"] timeout-ms {}))
    (catch Exception _ false)))

(def cwb-tools
  "The CWB programs the app runs besides cqp. Each is invoked by bare
  name, so the PATH that reaches cqp has to reach these too."
  ["cwb-describe-corpus" "cwb-lexdecode" "cwb-s-decode"])

(defn tools!
  "Log the CWB programs `ctx` drives that this machine cannot launch, and
  return them: its :cqp (default cqp) and the `cwb-tools`.

  A PATH reaching cqp need not reach the rest, and the failure is quiet:
  search keeps working while corpus pages, frequency lists and metadata
  filters all fail. Logs the CQP version too, since the app generates the
  subset that is safe on the oldest supported one."
  [ctx]
  (let [missing (vec (remove installed? (cons (:cqp ctx "cqp") cwb-tools)))]
    (doseq [command missing]
      (t/event! ::tool-missing {:level :warn :data {:command command}}))
    (when (empty? missing)
      ;; the app's own timeout is for a query; a banner answers at once
      (t/event! ::cwb-version
                {:data {:cqp (try (cqp/version! (assoc ctx
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

(defn pipeline-order
  "The line numbers the sort pipeline CQP runs puts `collation-probe` in
  under LC_ALL `sort-locale`; nil when the pipeline cannot be run at all.

  The pipeline is CQP's own, `sort -k 2 -k 1n | gawk '{print $1}'` (see
  docs/research/gap-nqr-persistence.md section 3). Running it is the only
  way to learn what CQP's sort will really do here, because sort, gawk and
  the locale all have to work together and CQP reports none of it: it
  falls back to corpus order and says nothing.

  The pipeline exits with gawk's status, not sort's, so what came back is
  judged instead: anything but a reordering of the probe means the
  pipeline itself is broken."
  [sort-locale]
  (try
    (let [res   (cqp/run-process!
                 ["sh" "-c" "sort -k 2 -k 1n | gawk '{print $1}'"]
                 timeout-ms
                 {:in      probe-input
                  :charset (probe-charset sort-locale)
                  :env     {"LC_ALL" (str sort-locale)}})
          order (when (not= ::cqp/timeout res)
                  (str/split-lines (str/trim (:out res))))]
      (when (= (set order) (set (vals probe-line)))
        order))
    (catch Exception _ nil)))

(defn collator-order
  "The line numbers `sort-locale`'s collator puts `collation-probe` in:
  how the app itself orders metadata and frequency values (see
  dk.cst.corpus-probe.search/->collator)."
  [sort-locale]
  (mapv probe-line (sort (search/->collator {:sort-locale sort-locale})
                         collation-probe)))

(defn collation-problems
  "What would make CQP sort differently from the app itself under
  `sort-locale`: :sort-locale-unset when there is none to follow,
  :pipeline-broken when CQP's sort pipeline does not run here, and
  :collation-mismatch when it runs but disagrees with the app's collator.

  The app orders values with a java.text.Collator while CQP orders a
  concordance with a shell pipeline. Both are meant to follow
  :sort-locale, and the setting is worth nothing unless they agree."
  [sort-locale]
  (if (str/blank? (str sort-locale))
    [:sort-locale-unset]
    (let [order (pipeline-order sort-locale)]
      (cond
        (nil? order)                             [:pipeline-broken]
        (not= order (collator-order sort-locale)) [:collation-mismatch]
        :else                                    []))))

(defn collation!
  "Log the `collation-problems` of `config`'s :sort-locale and return
  them."
  [{:keys [sort-locale] :as config}]
  (let [problems (collation-problems sort-locale)]
    (doseq [problem problems]
      (t/event! ::collation-fallback
                {:level :warn
                 :data  {:problem problem :sort-locale sort-locale}}))
    problems))

(defn corpus!
  "Vet registry entry map `m` against the installation in `ctx`: nil when
  CWB can read its corpus, else [id reason] saying why it cannot.

  The reason is :undefined when CWB has no data for the entry (see
  dk.cst.corpus-probe.corpus/phantom?), else the type of the failure
  (:timeout, :cqp, :misaligned) or :unreadable when it carries none. The
  type is safe to log; the message it comes with can name server paths."
  [ctx m]
  (let [{:keys [id]} (corpus/overview m)]
    (try
      (when-not (:size (corpus/overview! ctx m))
        [id :undefined])
      (catch Exception e
        [id (get-in (ex-data e) [:error :type] :unreadable)]))))

(defn registry!
  "Read every corpus of the `ctx` registry once, in parallel, log the ones
  CWB cannot open and return them as the [id reason] pairs of `corpus!`.

  A registry that holds no corpus at all is logged as a problem rather
  than a clean run, since a mistyped :registry path reads exactly like an
  empty one. Reading them all also caches the corpus index's token counts
  before the first request."
  [ctx]
  (let [started (System/nanoTime)
        corpora (corpus/corpora ctx)
        broken  (vec (keep identity
                           (search/pmap-n (search/parallelism ctx)
                                          #(corpus! ctx %)
                                          corpora)))]
    (doseq [[id reason] broken]
      (t/event! ::corpus-unreadable
                {:level :warn :data {:corpus id :reason reason}}))
    (t/event! ::registry-vetted
              {:level (if (seq corpora) :info :warn)
               :data  {:registry   (:registry ctx)
                       :corpora    (count corpora)
                       :unreadable (count broken)
                       :ms         (quot (- (System/nanoTime) started)
                                         1000000)}})
    broken))

(comment
  (def ctx {:registry (str (System/getProperty "user.dir")
                           "/dev/corpus/registry")
            :sort-locale "da_DK.UTF-8"})

  (tools! ctx)
  ;; => []

  (collation! ctx)
  ;; => []

  (collation-problems "zz_ZZ.UTF-8")
  ;; => [:collation-mismatch]

  (registry! ctx)
  ;; => []
  #_.)
