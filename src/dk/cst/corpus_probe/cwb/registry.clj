(ns dk.cst.corpus-probe.cwb.registry
  "The CWB registry read from disk: the entry files naming each corpus's
  data location, encoding and attributes (see docs/research/cwb-core.md
  §2.5), and the folders the configuration groups the corpora in.

  Nothing here runs CQP; what CQP reports about a corpus is
  dk.cst.corpus-probe.cwb.corpus's."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [dk.cst.corpus-probe.cqp :as cqp]))

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

(defn entry
  "Parse the registry entry `file` into a registry entry map.

  Returns {:id <s> :name <s> :home <s> :info <s> :charset <s> :language <s>
  :p-attrs [<kw> ...] :s-attrs [<kw> ...] :aligned [<kw> ...]} with
  attributes in declaration order, which is the order CQP displays them in."
  [file]
  (->> (str/split (slurp file) #"\n")
       (keep registry-line)
       (reduce (fn [m [k v]]
                 (if (#{:p-attrs :s-attrs :aligned} k)
                   (update m k (fnil conj []) v)
                   (assoc m k v)))
               {:p-attrs [] :s-attrs [] :aligned []})))

(defn entry-file?
  "True when `file` looks like a registry entry: a plain file named like a
  corpus ID. Subdirectories and files with other names are not entries."
  [^java.io.File file]
  (and (.isFile file)
       (boolean (re-matches #"[a-z0-9_-]+" (.getName file)))))

(defn entry-file
  "The registry entry file of `corpus` in `ctx`. The filename is the
  lowercase corpus ID."
  ^java.io.File [{:keys [registry] :as ctx} corpus]
  (io/file registry (str/lower-case (str corpus))))

(defn entries
  "Read every registry entry in `ctx`'s :registry directory into registry
  entry maps, sorted by :id.

  The :id is the entry's filename, which is the name CQP resolves a corpus
  by, whatever the ID field inside says. Files that are not entries
  (subdirectories, names that are not corpus IDs, text without a HOME line)
  are skipped."
  [{:keys [registry] :as ctx}]
  (->> (.listFiles (io/file registry))
       (filter entry-file?)
       (map (fn [^java.io.File file] (assoc (entry file) :id (.getName file))))
       (filter :home)
       (sort-by :id)
       (vec)))

(defn entry-of
  "The registry entry map of the corpus `id` names under `ctx`, read as
  `entries` reads it, or nil when `id` is no corpus name or names no
  entry. The name is matched case-insensitively, so a path can carry it
  in lowercase; it is checked before it becomes a filename."
  [ctx id]
  (let [corpus (str/upper-case (str id))]
    (when (cqp/corpus-name? corpus)
      (let [file (entry-file ctx corpus)]
        (when (entry-file? file)
          (assoc (entry file) :id (.getName file)))))))

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

(defn charset
  "Return the Java charset name for `corpus` in `ctx`, read from the
  `##:: charset` property of its registry entry; defaults to UTF-8.

  CQP transcodes nothing, so both commands sent to and output read from a
  corpus must use its own encoding."
  [ctx corpus]
  (let [file (entry-file ctx corpus)]
    (or (when (entry-file? file)
          (cwb->charset (:charset (entry file))))
        "UTF-8")))

(defn data-file
  "The file holding the token stream of `corpus` under `ctx`: the data of
  its first positional attribute, which cwb-encode rewrites every time the
  corpus is encoded. nil when the registry entry does not say where the
  data are."
  ^java.io.File [ctx corpus]
  (let [file (entry-file ctx corpus)]
    (when (entry-file? file)
      (let [{:keys [home p-attrs]} (entry file)]
        (when home
          (io/file home (str (name (or (first p-attrs) :word)) ".corpus")))))))

(defn build-stamp
  "What `corpus` reads as under `ctx`: the modification time and length of
  its registry entry, and the same of its token stream (see `data-file`)
  when that is there to read.

  Part of every saved result's name (see dk.cst.corpus-probe.search.cache) and
  of every cached fact's key (see dk.cst.corpus-probe.cwb.corpus/facts!),
  and it has to cover both. cwb-encode rewrites the entry only when
  passed -R, so rebuilding a corpus in place leaves the entry
  byte-identical while every word changes underneath it. The entry counts
  too, declaring the charset everything is read in, so correcting a
  mis-declared one changes which matches exist without touching the data."
  [ctx corpus]
  (let [^java.io.File file (entry-file ctx corpus)
        ^java.io.File data (data-file ctx corpus)]
    [(.lastModified file) (.length file)
     (when (and data (.isFile data)) [(.lastModified data) (.length data)])]))

(defn folder-corpora
  "Every corpus overview in resolved `folder`, subfolders included."
  [{:keys [corpora folders] :as folder}]
  (concat corpora (mapcat folder-corpora folders)))

(defn folder-ids
  "Every corpus ID named by the configured `folders` tree."
  [folders]
  (set (mapcat folder-corpora folders)))

(defn empty-folder?
  "True when resolved `folder` holds no corpora at any depth."
  [{:keys [corpora folders] :as folder}]
  (and (empty? corpora) (every? empty-folder? folders)))

(defn resolve-folder
  "Replace the corpus IDs of configured `folder` (and of its subfolders)
  with their overview maps from `by-id`, dropping IDs the registry does not
  know and subfolders left empty by that."
  [by-id {:keys [label corpora folders] :as folder}]
  {:label   label
   :corpora (into [] (keep by-id) corpora)
   :folders (into [] (comp (map #(resolve-folder by-id %))
                           (remove empty-folder?))
                  folders)})

(defn grouped-corpora
  "Group the corpus `overviews` by the configured `folders` tree; corpora no
  folder claims follow as a final label-less folder, so a corpus never
  disappears because the configuration lags behind the registry. Folders
  the registry leaves empty are dropped."
  [folders overviews]
  (let [by-id     (into {} (map (juxt :id identity)) overviews)
        unclaimed (vec (remove (comp (folder-ids folders) :id) overviews))]
    (cond-> (into [] (comp (map #(resolve-folder by-id %))
                           (remove empty-folder?))
                  folders)
      (seq unclaimed) (conj {:label nil :corpora unclaimed :folders []}))))
