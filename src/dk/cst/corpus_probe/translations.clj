(ns dk.cst.corpus-probe.translations
  "The UI translations, read from the gettext PO files on the classpath.

  One file per language under resources/i18n/, keyed by the English
  source string, so adding a language is dropping in a file and naming
  it here rather than editing a map in the source. English needs no
  file: it is what the msgids are written in.

  The server reads the files at load time; the ClojureScript build
  cannot, so it inlines the tables at compile time through
  `inline-tables`. After editing a PO file, force a recompile of the
  frontend: the build cannot see through the macro to the file."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [pottery.core :as pottery]))

(def po-files
  "The bundled translations: language code to its PO file on the
  classpath. English is absent because it is the source language."
  {"da" "i18n/da.po"})

(defn unescape
  "Undo the PO escaping of `s`: a backslash quotes the character after
  it.

  The reader unescapes a newline and leaves everything else, so a msgid
  holding a double quote (the CQP example query does) would otherwise
  come back with its backslashes still in it and match nothing the
  source passes to `tr`."
  [s]
  (str/replace s #"\\(.)" "$1"))

(defn read-po
  "The table of the PO file at classpath `path`, unescaped.

  A key is an English msgid, or a [singular plural] pair of them for a
  plural entry, and its value is the translation or the pair of
  translated forms."
  [path]
  ;; a plural entry has a pair for its key and a pair for its value
  (let [unescape* #(if (vector? %) (mapv unescape %) (unescape %))]
    (-> (pottery/read-po-file (io/resource path))
        (update-vals unescape*)
        (update-keys unescape*))))

(defn tables
  "The bundled translation tables, language code to its table (see
  `read-po`)."
  []
  (update-vals po-files read-po))

(defmacro inline-tables
  "The bundled tables as a literal, for the ClojureScript build."
  []
  (tables))
