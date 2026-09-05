(ns dk.cst.corpus-probe.tools
  "Read-only wrappers for the cwb-* command-line tools, complementing the
  CQP driver with corpus facts CQP itself does not report.

  `cwb-describe-corpus` gives the corpus info pages their statistics,
  `cwb-lexdecode` gives whole-corpus frequency lists and `cwb-s-decode`
  gives the metadata filters their value lists and the frequency
  breakdowns the size of the text behind each value."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.corpus :as corpus]
            [dk.cst.corpus-probe.cqp :as cqp]
            [dk.cst.corpus-probe.parse :as parse]
            [dk.cst.corpus-probe.query :as query]))

(defn run-tool!
  "Run the cwb tool command vector `cmd` for `corpus` (a name already
  validated by the caller) via `ctx` and return its stdout lines.

  The output is decoded in the corpus's own charset. Throws on `ctx`'s
  :timeout-ms (default 30000, destroying the process tree like the CQP
  driver) and on a non-zero exit, with the tool's error text; the registry
  diagnostics of dk.cst.corpus-probe.cqp/diagnostic? do not count as
  errors."
  [{:keys [timeout-ms] :or {timeout-ms 30000} :as ctx} corpus cmd]
  (let [res (cqp/run-process! cmd timeout-ms
                              {:charset (corpus/charset ctx corpus)})]
    (when (= res ::cqp/timeout)
      (throw (ex-info "cwb tool timed out"
                      {:corpus corpus :cmd cmd :timeout-ms timeout-ms})))
    (when-not (zero? (:exit res))
      (throw (ex-info "cwb tool failed"
                      {:corpus corpus :cmd cmd :exit (:exit res)
                       :message (:error (cqp/stderr->outcome (:err res)))})))
    (str/split (:out res) #"\n")))

(defn describe-corpus!
  "Describe `corpus` (an uppercase CQP corpus name) with per-attribute
  statistics via `cwb-describe-corpus -s` against the `ctx` registry,
  cached until the corpus's registry file changes.

  Returns the map of dk.cst.corpus-probe.parse/describe->map. The corpus
  name is validated first, like every name spliced into a command. Throws
  when the tool fails or times out, and when its output reports no size:
  the tool exits 0 for a registry entry whose data files are gone,
  printing ERROR in place of the size."
  [ctx corpus]
  (query/valid-corpus-name corpus)
  (corpus/with-facts-cache!
    ctx corpus "cwb-describe-corpus -s"
    (fn []
      (let [stats (parse/describe->map
                   (run-tool! ctx corpus ["cwb-describe-corpus"
                                          "-r" (:registry ctx)
                                          "-s" (str corpus)]))]
        (when (nil? (:size stats))
          (throw (ex-info "cwb-describe-corpus reported missing corpus data"
                          {:corpus corpus :phantom? true})))
        stats))))

(defn lexicon!
  "The frequency lexicon of positional attribute `attr` of `corpus` (an
  uppercase CQP corpus name) via `cwb-lexdecode -f` against the `ctx`
  registry.

  Returns [{:values [<s>] :freq <n>} ...] sorted by frequency (see
  dk.cst.corpus-probe.parse/lexicon->freqs), the shape a grouped query
  result has, so the whole corpus can stand in for a query. Both names are
  spliced into the command: the corpus name is validated, and `attr` must
  be one of the corpus's positional attributes. Not cached: a large
  corpus's lexicon runs to millions of entries, too much heap to keep per
  attribute for the JVM's lifetime."
  ;; TODO: stream large lexicons and exports row by row instead of
  ;; building them in memory (milestone 5, with the NQR caching).
  [ctx corpus attr]
  (query/valid-corpus-name corpus)
  (let [p-attrs (->> (corpus/attributes! ctx corpus)
                     (filter #(= :positional (:type %)))
                     (map :name)
                     (set))]
    (when-not (p-attrs (keyword attr))
      (throw (ex-info "Not a positional attribute of this corpus"
                      {:corpus corpus :attr attr :p-attrs p-attrs})))
    (parse/lexicon->freqs
     (run-tool! ctx corpus ["cwb-lexdecode" "-r" (:registry ctx)
                            "-fb" "-P" (name attr) (str corpus)]))))

(def value-limit
  "The most distinct values an annotated s-attribute may have and still be
  offered as a metadata filter: a longer list of checkboxes is no longer a
  usable control, and a text ID or title has one value per region anyway."
  500)

(def region-limit
  "The most regions an s-attribute may have before its values are not
  even decoded: an attribute with millions of regions (sentence IDs, say)
  all but always exceeds `value-limit`, and decoding it costs seconds."
  1000000)

(defn attribute-values!
  "The values of positional attribute `attr` of `corpus` (an uppercase
  CQP corpus name) via `ctx`, in lexicon order, cached until the corpus
  is re-encoded; nil when the attribute has more than `value-limit`
  types (see `describe-corpus!`), which are not decoded, being no list a
  reader could pick from, or when the corpus lacks it. Both names are
  checked as `lexicon!` checks them."
  [ctx corpus attr]
  (corpus/with-facts-cache!
    ctx corpus (str "cwb-lexdecode " (name attr))
    (fn []
      (let [types (some #(when (= (keyword attr) (:name %)) (:types %))
                        (:p-attrs (describe-corpus! ctx corpus)))]
        (when (and types (<= types value-limit))
          (mapv (comp first :values) (lexicon! ctx corpus attr)))))))

(defn annotated-stats!
  "The describe statistics of annotated s-attribute `attr` of `corpus`
  (an uppercase CQP corpus name) via `ctx`: its name and its region
  count (see `describe-corpus!`).

  Both names are spliced into the command that decodes the attribute, so
  the corpus name is validated and anything but an annotated
  s-attribute of the corpus is rejected."
  [ctx corpus attr]
  (query/valid-corpus-name corpus)
  (let [attr  (keyword attr)
        stats (some #(when (= attr (:name %)) %)
                    (:s-attrs (describe-corpus! ctx corpus)))]
    (when-not (:values? stats)
      (throw (ex-info "Not an annotated structural attribute of this corpus"
                      {:corpus corpus :attr attr})))
    stats))

(defn s-decode!
  "The lines `cwb-s-decode` prints with `flags` for annotated
  s-attribute `attr` of `corpus` against the `ctx` registry (see
  `annotated-stats!` for the checks both names pass first); nil when the
  attribute has more than `region-limit` regions, which are not even
  decoded."
  [ctx corpus attr flags]
  (when (<= (:regions (annotated-stats! ctx corpus attr)) region-limit)
    (run-tool! ctx corpus (-> ["cwb-s-decode" "-r" (:registry ctx)]
                              (into flags)
                              (conj (str corpus) "-S" (name attr))))))

(defn annotation-values!
  "The values of annotated s-attribute `attr` of `corpus` (an uppercase
  CQP corpus name) with the number of regions carrying each, via
  `cwb-s-decode` against the `ctx` registry, cached until the corpus's
  registry file changes.

  Returns [{:values [<s>] :freq <n>} ...] sorted by value (see
  dk.cst.corpus-probe.parse/s-decode->freqs), or nil when the attribute
  has too many values to list (see `value-limit` and `region-limit`).
  Both names are checked before anything is decoded (see `s-decode!`)."
  [ctx corpus attr]
  (corpus/with-facts-cache!
    ctx corpus (str "cwb-s-decode " (name attr))
    (fn []
      (some-> (s-decode! ctx corpus attr ["-n"])
              (parse/s-decode->freqs value-limit)))))

(defn annotation-sizes!
  "The number of tokens carrying each value of annotated s-attribute
  `attr` of `corpus` (an uppercase CQP corpus name), via `cwb-s-decode`
  against the `ctx` registry, cached until the corpus's registry file
  changes.

  Returns {<value> <tokens>} (see
  dk.cst.corpus-probe.parse/s-decode->sizes), or nil when the attribute
  has too many regions to decode (see `region-limit`). What a rate per
  million is measured against when a breakdown groups by the attribute:
  the text carrying each value rather than the whole corpus, so that a
  year with more text does not look busier. No `value-limit` applies,
  this being a map a breakdown reads rather than a list of checkboxes.
  Both names are checked as `annotation-values!` checks them."
  [ctx corpus attr]
  (corpus/with-facts-cache!
    ctx corpus (str "cwb-s-decode sizes " (name attr))
    (fn []
      (some-> (s-decode! ctx corpus attr [])
              (parse/s-decode->sizes)))))
