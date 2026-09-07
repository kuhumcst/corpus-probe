(ns dk.cst.corpus-probe.cqp
  "The lexical rules of CQP, the query language of the IMS Open Corpus
  Workbench: how a literal or a regex is escaped for a double-quoted CQP
  string, what a name may be, and the units of text a query is kept
  within. Shared by the query compiler (dk.cst.corpus-probe.query) on
  both sides and the command generator (dk.cst.corpus-probe.cwb.command)
  on the server, so a value is escaped one way wherever it is spliced.

  The process that runs CQP is dk.cst.corpus-probe.cwb."
  (:require [clojure.string :as str]))

(defn escape-literal
  "Escape `s` for embedding inside a double-quoted CQP regular expression:
  backslash-escape the PCRE metacharacters and double the quote character.

  This is the safe superset of the escaping used by CWB::CQP, CEQL and Korp
  (docs/research/gap-simple-search.md §3)."
  [s]
  ;; a function rather than a replacement string: `$0` is the match on
  ;; the JVM and literal text in JavaScript
  (-> s
      (str/replace #"[.?*+|(){}\[\]^$\\]" #(str "\\" %))
      (str/replace "\"" "\"\"")))

(defn flatten-whitespace
  "Replace newlines and TABs in `s` with spaces.

  Applied to user-supplied CQP before embedding it in a command batch: a
  newline would detach the QueryLock wrapping and a TAB would collide with
  the hardened profile's separator frames."
  [s]
  (str/replace s #"[\n\r\t]+" " "))

(defn escape-value
  "Escape `value` for a double-quoted regex on a command line: its
  metacharacters and quotes as `escape-literal` does, and the control
  characters a command line cannot carry, TAB (which the hardened profile
  frames its output with) and the line breaks that end a command, as
  their regex escapes, which match the same bytes."
  [value]
  (-> (escape-literal value)
      (str/replace "\t" "\\t")
      (str/replace "\n" "\\n")
      (str/replace "\r" "\\r")))

(defn regex-value
  "The regular expression `value` as a reader wrote it, made safe for a
  double-quoted CQP literal: quotes doubled and line breaks and TABs
  flattened (see `flatten-whitespace`). Backslashes are the reader's own,
  so a regex ending in one is CQP's to refuse, as it is in CQP mode."
  [value]
  (str/replace (flatten-whitespace (str value)) "\"" "\"\""))

(defn name?
  "True when `s` is a name CQP's lexer accepts, for an attribute or a
  query result: a letter or underscore followed by letters, digits,
  underscores and hyphens.

  An interpolation guard like `corpus-name?`: an attribute a reader chose
  to sort by is spliced into a command outside the QueryLock, so it is
  held to the lexer's rule here and checked against the corpus's
  inventory by dk.cst.corpus-probe.search."
  [s]
  (boolean (re-matches #"[a-zA-Z_][a-zA-Z0-9_-]*" (str s))))

(defn corpus-name?
  "True when `s` is a syntactically valid uppercase CQP corpus name.

  Used as an interpolation guard: corpus names are spliced into command
  strings outside the QueryLock sandbox, so anything else must be rejected."
  [s]
  (boolean (re-matches #"[A-Z][A-Z0-9_-]*" (str s))))

(def units
  "The units of text a search of several tokens can be kept within, in
  display order, each as [unit name]: the unit's keyword, whose name is
  its `within` param value, and what CQP calls it in a `within` clause
  by CWB's usual names for the attributes. A corpus naming them otherwise
  renames them at run time (see
  dk.cst.corpus-probe.cwb.command/within-clause)."
  [[:sentence "s"] [:paragraph "p"] [:text "text"]])
