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
  "Parse the CWB registry file `f` into a corpus map.

  Returns {:id <s> :name <s> :home <s> :info <s> :charset <s> :language <s>
  :p-attrs [<kw> ...] :s-attrs [<kw> ...] :aligned [<kw> ...]} with
  attributes in declaration order -- the order CQP displays them in."
  [f]
  (->> (str/split (slurp f) #"\n")
       (keep registry-line)
       (reduce (fn [m [k v]]
                 (if (#{:p-attrs :s-attrs :aligned} k)
                   (update m k (fnil conj []) v)
                   (assoc m k v)))
               {:p-attrs [] :s-attrs [] :aligned []})))

(defn corpora
  "Read every registry file in `ctx`'s :registry directory into corpus maps,
  sorted by :id. Files whose names are not valid corpus IDs are skipped."
  [{:keys [registry] :as ctx}]
  (->> (.listFiles (io/file registry))
       (filter #(re-matches #"[a-z0-9_-]+" (.getName ^java.io.File %)))
       (map read-registry)
       (sort-by :id)
       (vec)))

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
  "The registry file of `corpus` in `ctx` -- the filename is the lowercase
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
    (or (when (.exists f)
          (cwb->charset (:charset (read-registry f))))
        "UTF-8")))

(defonce ^{:doc "Cache of `show cd;` attribute descriptions, keyed by
  [registry corpus registry-file-mtime] -- the mtime keys stale entries out
  when a corpus is re-encoded under a running JVM."}
  attribute-cache
  (atom {}))

(defn attributes!
  "Return the attribute descriptions of `corpus` (an uppercase CQP corpus
  name) as reported by `show cd;` via the installation in `ctx`, cached per
  registry + corpus until the corpus's registry file changes.

  Unlike the registry, this marks which s-attributes carry annotation values
  (:values?), which decides what `tabulate` can extract per hit."
  [{:keys [registry] :as ctx} corpus]
  (when-not (query/corpus-name? corpus)
    (throw (ex-info "Invalid corpus name" {:corpus corpus})))
  (let [k [registry corpus (.lastModified (registry-file ctx corpus))]]
    (or (get @attribute-cache k)
        (let [{:keys [results error]} (cqp/run-batch! ctx [(str corpus ";")
                                                           "show cd;"])]
          (when error
            (throw (ex-info "Could not read corpus attributes"
                            {:corpus corpus :error error})))
          (let [attrs (parse/show-cd->attributes (second results))]
            (swap! attribute-cache assoc k attrs)
            attrs)))))
