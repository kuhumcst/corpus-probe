(ns dk.cst.corpus-probe.i18n-scan
  "The gettext extraction: every UI string the source passes to
  dk.cst.corpus-probe.i18n, collected into the template a translator
  starts a new language from.

  Regenerate it with `clojure -M:i18n`. A test compares the committed
  template against a fresh extraction, so it cannot drift from the
  source without the suite saying so.

  Only a string literal is visible to the extraction: the scanner reads
  the source as data and never evaluates it. That is why a view that
  picks one string of several does so with a `case` of `tr` calls
  rather than by looking a table up: a string that never appears as a
  literal argument cannot reach the template."
  (:require [clojure.java.io :as io]
            [pottery.po :as po]
            [pottery.scan :as scan]))

(def source-dir
  "The tree whose UI strings the template carries."
  "src")

(def excluded-files
  "Files the scan skips: the i18n namespace's own examples call `tr`
  with strings that are documentation rather than interface."
  #{"src/dk/cst/corpus_probe/i18n.cljc"})

(def template-file
  "The gettext template, committed beside the translations it seeds."
  "resources/i18n/template.pot")

(def extractor
  "The forms that name a UI string: a `tr`, `trx` or `trn` call,
  qualified or not.

  A `trn` pair becomes one plural entry and a `trx` context becomes part
  of the msgid, both as dk.cst.corpus-probe.i18n reads them back. A
  `(str ...)` of literals counts as one string, so a msgid too long for
  one line still reaches the template."
  (scan/make-extractor
   [(:or 'tr 'i18n/tr) _ (s :guard string?)] s
   [(:or 'tr 'i18n/tr) _ (['str & parts] :seq)] (apply str parts)
   [(:or 'trx 'i18n/trx) _ (c :guard string?) (s :guard string?)] (str c "|" s)
   [(:or 'trx 'i18n/trx) _ (c :guard string?) (['str & parts] :seq)]
   (str c "|" (apply str parts))
   [(:or 'trn 'i18n/trn) _ (s1 :guard string?) (s2 :guard string?) _] [s1 s2]
   [(:or 'tr 'i18n/tr 'trx 'i18n/trx 'trn 'i18n/trn) & _]
   (scan/extraction-warning "No literal UI string in:")))

(def header
  "The gettext header block that opens the template.

  The PO reader skips the first block of a file, so the template and
  every translation of it must open with one."
  (str "msgid \"\"\nmsgstr \"\"\n"
       "\"Content-Type: text/plain; charset=UTF-8\\n\"\n"
       "\"Plural-Forms: nplurals=2; plural=(n != 1);\\n\"\n"))

(defn scan
  "Every UI string of `source-dir`, as pottery scan results."
  []
  (remove (comp excluded-files ::scan/filename)
          (scan/scan-files {:dir source-dir :extract-fn extractor})))

(defn msgids
  "Every UI string the source names, as the keys a read table has: a
  string, or a [singular plural] pair."
  []
  (set (map ::scan/value (mapcat ::scan/expressions (scan)))))

(defn scan!
  "Write the gettext template of the UI to `template-file`."
  []
  (io/make-parents template-file)
  (spit template-file (str header "\n" (po/gen-template (scan)) "\n"))
  (println "Wrote" template-file))

(defn -main
  "Regenerate the template from the command line: clojure -M:i18n"
  [& _]
  (scan!)
  (shutdown-agents))
