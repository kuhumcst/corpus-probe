(ns dk.cst.corpus-probe.cqp
  "The lexical rules of CQP, the query language of the IMS Open Corpus
  Workbench: how a literal or a regex is escaped for a double-quoted
  string, what a name may be, and the units of text a query is kept
  within. Shared by the query compiler and the command generator, so a
  value is escaped one way wherever it is spliced."
  (:require [clojure.string :as str]))

(defn escape-literal
  "Escape `s` for a double-quoted CQP regular expression: the PCRE
  metacharacters backslashed and the quote doubled.

  The safe superset of what CWB::CQP, CEQL and Korp escape
  (docs/research/gap-simple-search.md §3)."
  [s]
  ;; a function rather than a replacement string: `$0` is the match on
  ;; the JVM and literal text in JavaScript
  (-> s
      (str/replace #"[.?*+|(){}\[\]^$\\]" #(str "\\" %))
      (str/replace "\"" "\"\"")))

(defn flatten-whitespace
  "Replace newlines and TABs in `s` with spaces."
  [s]
  ;; a newline would detach the QueryLock wrapping of a command batch and
  ;; a TAB collide with the hardened profile's separator frames
  (str/replace s #"[\n\r\t]+" " "))

(defn escape-value
  "Escape `value` for a double-quoted regex on a command line: as
  `escape-literal` does, and its TABs and line breaks as their regex
  escapes, which match the same bytes."
  [value]
  (-> (escape-literal value)
      (str/replace "\t" "\\t")
      (str/replace "\n" "\\n")
      (str/replace "\r" "\\r")))

(defn regex-value
  "The regular expression `value` as a reader wrote it, made safe for a
  double-quoted CQP literal: quotes doubled, line breaks and TABs
  flattened. Its backslashes are the reader's own, so a regex ending in
  one is CQP's to refuse."
  [value]
  (str/replace (flatten-whitespace (str value)) "\"" "\"\""))

(defn name?
  "True when `s` is a name CQP's lexer accepts, for an attribute or a
  query result: an interpolation guard like `corpus-name?`."
  [s]
  (boolean (re-matches #"[a-zA-Z_][a-zA-Z0-9_-]*" (str s))))

(defn corpus-name?
  "True when `s` is an uppercase CQP corpus name: an interpolation guard,
  since a corpus name is spliced into a command outside the QueryLock."
  [s]
  (boolean (re-matches #"[A-Z][A-Z0-9_-]*" (str s))))

(def units
  "The units of text a search of several tokens can be kept within, in
  display order, each as [unit name]: the keyword whose name is the
  `within` param value, and CQP's name for it, which a corpus naming its
  attributes otherwise renames at run time (see
  dk.cst.corpus-probe.cwb.command/within-clause)."
  [[:sentence "s"] [:paragraph "p"] [:text "text"]])
