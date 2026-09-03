(ns dk.cst.corpus-probe.query
  "CQP command generation: the hardened display profile, safe embedding of
  user input, the simple-search compiler and KWIC page batches.

  Only the verified 3.4.27-safe CQP subset is generated here (PLAN.md
  appendix B), so the commands run unchanged against both the production
  server and a current 3.5.0 installation."
  (:require [clojure.string :as str]))

(def hardened-profile
  "Display settings making KWIC output unambiguously parseable: every
  separator becomes a TAB-framed marker letter, and TAB can never occur
  inside a positional-attribute value. Inline annotation values are never
  shown (ShowTagAttributes off) -- they are unescaped, may contain TAB, and
  can crash CQP (docs/research/gap-kwic-parsing.md)."
  (str "set AttributeSeparator \"\tA\t\"; "
       "set TokenSeparator \"\tT\t\"; "
       "set LeftKWICDelim \"\tL\t\"; "
       "set RightKWICDelim \"\tR\t\"; "
       "set StructureDelimiter \"\tS\t\"; "
       "set ShowTagAttributes off;"))

(def kwic-defaults
  "Default KWIC paging options, shared by `kwic-commands` and the result
  maps of dk.cst.corpus-probe.search so the two cannot disagree."
  {:context 5 :page 0 :page-size 25})

(defn corpus-name?
  "True when `s` is a syntactically valid uppercase CQP corpus name.

  Used as an interpolation guard: corpus names are spliced into command
  strings outside the QueryLock sandbox, so anything else must be rejected."
  [s]
  (boolean (re-matches #"[A-Z][A-Z0-9_-]*" (str s))))

(defn escape-literal
  "Escape `s` for embedding inside a double-quoted CQP regular expression:
  backslash-escape the PCRE metacharacters and double the quote character.

  This is the safe superset of the escaping used by CWB::CQP, CEQL and Korp
  (docs/research/gap-simple-search.md §3)."
  [s]
  (-> s
      (str/replace #"[.?*+|(){}\[\]^$\\]" "\\\\$0")
      (str/replace "\"" "\"\"")))

(defn flatten-whitespace
  "Replace newlines and TABs in `s` with spaces.

  Applied to user-supplied CQP before embedding it in a command batch: a
  newline would detach the QueryLock wrapping and a TAB would collide with
  the hardened profile's separator frames."
  [s]
  (str/replace s #"[\n\r\t]+" " "))

(defn simple->cqp
  "Compile simple-search `input` into a CQP query string.

  Each whitespace-separated word becomes one token pattern, following
  Korp's simple search: `[word = \"<escaped>\"]` with `.*` affixes for
  :prefix?/:suffix? and the %c flag for :case-insensitive?. A :within
  s-attribute name appends a `within` clause. Returns nil for blank input
  rather than a match-everything query.

  (simple->cqp \"lille hund\" {:case-insensitive? true})
  ;; => [word = \"lille\" %c] [word = \"hund\" %c]"
  ([input]
   (simple->cqp input {}))
  ([input {:keys [case-insensitive? prefix? suffix? within]}]
   (when-not (str/blank? input)
     (let [pattern (fn [word]
                     (str "[word = \""
                          (when suffix? ".*")
                          (escape-literal word)
                          (when prefix? ".*")
                          "\"" (when case-insensitive? " %c") "]"))]
       (str (->> (str/split (str/trim input) #"\s+")
                 (map pattern)
                 (str/join " "))
            (when within (str " within " (name within))))))))

(defn locked-query
  "Wrap user-supplied CQP `query` in a QueryLock sandbox, returning one
  command string whose result lands in Last.

  Under QueryLock only queries execute; assignments, redirection (including
  the shell-escalating `> \"| cmd\"`) and every other command are rejected
  by CQP itself. The random key follows the practice of CWB::CQP, cwb-ccc
  and CQPweb. The query's terminating `;` and the `unlock` each sit on
  their own line, so a trailing `#` comment in the query can swallow
  neither."
  [query]
  (let [key   (inc (rand-int 999999))
        query (-> (flatten-whitespace query)
                  (str/trim)
                  (str/replace #";+\s*$" ""))]
    (str "set QueryLock " key ";\n" query "\n;\nunlock " key ";")))

(defn kwic-commands
  "Build the command batch for one KWIC page of `query` (raw CQP) against
  `corpus`, returning a vector of command strings.

  Batch order, relied upon by dk.cst.corpus-probe.search:
  0 display profile + context, 1 corpus activation, 2 locked query,
  3 `size`, 4 `show` + `cat` page, 5 `dump` page, then one `tabulate` page
  per entry of `struct-attrs`.

  `p-attrs` are the corpus's positional attributes (registry order) to show;
  `struct-attrs` the annotated s-attributes to fetch per hit. Each gets its
  own single-column `tabulate` command so that a whole output line is one
  annotation value -- annotation values may legally contain TAB, so packing
  them into one TAB-separated row would misalign the columns. `context` is
  in tokens; `page`/`page-size` select the hits and are clamped to sane
  values, since CQP treats negative range bounds as the entire result."
  [corpus query {:keys [p-attrs struct-attrs context page page-size]
                 :or   {context   (:context kwic-defaults)
                        page      (:page kwic-defaults)
                        page-size (:page-size kwic-defaults)}}]
  (let [from       (* (max 0 page) (max 1 page-size))
        to         (dec (+ from (max 1 page-size)))
        page-range (str "Last " from " " to)
        show       (when (next p-attrs)
                     (str "show " (str/join " " (map #(str "+" (name %))
                                                     (rest p-attrs))) "; "))]
    (into [(str hardened-profile " set Context " (long context) " words;")
           (str corpus ";")
           (locked-query query)
           "size Last;"
           (str show "cat " page-range ";")
           (str "dump " page-range ";")]
          (map #(str "tabulate " page-range " match " (name %) ";"))
          struct-attrs)))
