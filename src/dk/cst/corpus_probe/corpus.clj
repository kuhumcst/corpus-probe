(ns dk.cst.corpus-probe.corpus
  "Corpus metadata read from the CWB registry and from CQP itself.

  A registry file names a corpus's data location, encoding and attributes
  (see docs/research/cwb-core.md §2.5); everything the UI knows about a
  corpus derives from it plus CQP's `show cd;` and `info;` commands, so no
  corpus configuration is duplicated in the application."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [dk.cst.corpus-probe.cqp :as cqp]
            [dk.cst.corpus-probe.parse :as parse]
            [dk.cst.corpus-probe.query :as query]))

(defn- registry-line
  "Parse one registry `line` into a [k v] entry, or nil for comments."
  [line]
  (or (when-let [[_ k v] (re-matches #"##::\s+(\S+)\s+=\s+\"([^\"]*)\".*" line)]
        [(keyword k) v])
      (when-let [[_ k v] (re-matches #"(NAME|ID|HOME|INFO)\s+\"?([^\"]*?)\"?\s*"
                                     line)]
        [(keyword (str/lower-case k)) v])
      (when-let [[_ k v] (re-matches #"(ATTRIBUTE|STRUCTURE|ALIGNED)\s+(\S+).*"
                                     line)]
        [({"ATTRIBUTE" :p-attrs
           "STRUCTURE" :s-attrs
           "ALIGNED"   :aligned} k) (keyword v)])))

(defn read-registry
  "Parse the CWB registry entry file `f` into a registry entry map.

  Returns {:id <s> :name <s> :home <s> :info <s> :charset <s> :language <s>
  :p-attrs [<kw> ...] :s-attrs [<kw> ...] :aligned [<kw> ...]} with
  attributes in declaration order, which is the order CQP displays them in."
  [f]
  (->> (str/split (slurp f) #"\n")
       (keep registry-line)
       (reduce (fn [m [k v]]
                 (if (#{:p-attrs :s-attrs :aligned} k)
                   (update m k (fnil conj []) v)
                   (assoc m k v)))
               {:p-attrs [] :s-attrs [] :aligned []})))

(defn registry-file?
  "True when `f` looks like a registry entry: a plain file named like a
  corpus ID. Subdirectories and files with other names are not entries."
  [^java.io.File f]
  (and (.isFile f)
       (boolean (re-matches #"[a-z0-9_-]+" (.getName f)))))

(defn corpora
  "Read every registry entry in `ctx`'s :registry directory into registry
  entry maps, sorted by :id.

  The :id is the entry's filename, which is the name CQP resolves a corpus
  by, whatever the ID field inside says. Files that are not entries
  (subdirectories, names that are not corpus IDs, text without a HOME line)
  are skipped."
  [{:keys [registry] :as ctx}]
  (->> (.listFiles (io/file registry))
       (filter registry-file?)
       (map (fn [^java.io.File f] (assoc (read-registry f) :id (.getName f))))
       (filter :home)
       (sort-by :id)
       (vec)))

(defn language
  "The language of the corpus with registry entry map `m`, when its language
  property is a plausible code (two or three letters) rather than the
  `??` placeholder cwb-encode writes."
  [{:keys [language] :as m}]
  (when (re-matches #"[a-z]{2,3}" (str language))
    language))

(def cwb->charset
  "CWB charset property values mapped to Java charset names (the CWB names
  come from the ECorpusCharset enum in cl/cl.h)."
  {"ascii"    "US-ASCII"
   "utf8"     "UTF-8"
   "latin1"   "ISO-8859-1"
   "latin2"   "ISO-8859-2"
   "latin3"   "ISO-8859-3"
   "latin4"   "ISO-8859-4"
   "cyrillic" "ISO-8859-5"
   "arabic"   "ISO-8859-6"
   "greek"    "ISO-8859-7"
   "hebrew"   "ISO-8859-8"
   "latin5"   "ISO-8859-9"
   "latin6"   "ISO-8859-10"
   "latin7"   "ISO-8859-13"
   "latin8"   "ISO-8859-14"
   "latin9"   "ISO-8859-15"})

(defn registry-file
  "The registry file of `corpus` in `ctx`. The filename is the lowercase
  corpus ID."
  ^java.io.File [{:keys [registry] :as ctx} corpus]
  (io/file registry (str/lower-case (str corpus))))

(defn charset
  "Return the Java charset name for `corpus` in `ctx`, read from the
  `##:: charset` property of its registry entry; defaults to UTF-8.

  CQP transcodes nothing, so both commands sent to and output read from a
  corpus must use its own encoding."
  [ctx corpus]
  (let [f (registry-file ctx corpus)]
    (or (when (registry-file? f)
          (cwb->charset (:charset (read-registry f))))
        "UTF-8")))

(defonce ^{:doc "Cache of per-corpus facts: a delay per key
  [registry corpus label registry-file-mtime]. The mtime keys stale
  entries out when a corpus is re-encoded under a running JVM."}
  facts-cache
  (atom {}))

(defn- without-superseded
  "Remove from `cache` the entries of the same registry, corpus and label
  as key `k` (older mtimes of the same facts)."
  [cache [registry corpus label :as k]]
  (into {} (remove (fn [[[r c l] _]]
                     (and (= r registry) (= c corpus) (= l label))))
        cache))

(defn with-facts-cache!
  "Return the cached facts of `corpus` in `ctx` under cache key part
  `label`, computing them with no-arg `f` on a miss.

  Concurrent misses share one computation: the cache holds a delay per key,
  so the first caller runs `f` while the others wait for its value. A
  computation that throws is forgotten again, so the next caller retries.
  Entries live until the corpus's registry file changes; the entry they
  supersede is dropped then."
  [{:keys [registry] :as ctx} corpus label f]
  (let [k [registry corpus label (.lastModified (registry-file ctx corpus))]
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

(defn corpus-facts!
  "Run CQP `command` against activated `corpus` (an uppercase CQP corpus
  name) via `ctx` and parse its output lines with `parse-fn`, cached per
  registry + corpus + command until the corpus's registry file changes.

  The corpus name is validated first, since it is spliced into the
  activation command; the batch runs in the corpus's own charset."
  [ctx corpus command parse-fn]
  (query/valid-corpus-name corpus)
  (with-facts-cache!
    ctx corpus command
    (fn []
      (let [ctx (assoc ctx :charset (charset ctx corpus))
            {:keys [results error]} (cqp/run-batch! ctx [(str corpus ";")
                                                         command])]
        (when error
          (throw (ex-info "Could not read corpus facts"
                          {:corpus corpus :command command :error error})))
        (parse-fn (second results))))))

(defn attributes!
  "Return the attribute descriptions of `corpus` as reported by `show cd;`
  via the installation in `ctx`, cached until the corpus's registry file
  changes.

  Unlike the registry, this marks which s-attributes carry annotation values
  (:values?), which decides what `tabulate` can extract per hit."
  [ctx corpus]
  (corpus-facts! ctx corpus "show cd;" parse/show-cd->attributes))

(defn info!
  "Return the corpus facts of `corpus` as reported by `info;` via the
  installation in `ctx` (see dk.cst.corpus-probe.parse/info->map), cached
  until the corpus's registry file changes."
  [ctx corpus]
  (corpus-facts! ctx corpus "info;" parse/info->map))

(defn overview
  "Summarize registry entry map `m` for the corpus index: its uppercase CQP
  :id, its :title (the registry NAME, when set) and its :language (see
  `language`)."
  [{:keys [id name] :as m}]
  {:id       (str/upper-case id)
   :title    (not-empty name)
   :language (language m)})

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
    (with-facts-cache!
      ctx id "overview"
      (fn []
        (assoc summary
               :size (try (:size (info! ctx id))
                          (catch clojure.lang.ExceptionInfo e
                            (if (phantom? e) nil (throw e)))))))))
