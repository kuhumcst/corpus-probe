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
  shown (ShowTagAttributes off), since they are unescaped, may contain TAB,
  and can crash CQP (docs/research/gap-kwic-parsing.md)."
  (str "set AttributeSeparator \"\tA\t\"; "
       "set TokenSeparator \"\tT\t\"; "
       "set LeftKWICDelim \"\tL\t\"; "
       "set RightKWICDelim \"\tR\t\"; "
       "set StructureDelimiter \"\tS\t\"; "
       "set ShowTagAttributes off;"))

(def page-defaults
  "Default concordance paging: the first page of 25 hits."
  {:page 0 :page-size 25})

(def max-row
  "The highest row CQP can address: its range bounds are C ints, and a
  larger number wraps negative, which CQP reads as the entire result."
  Integer/MAX_VALUE)

(defn page-rows
  "The row range [from to] of page `page` with `page-size` hits per page:
  0-based and inclusive, as `cat` and `dump` take it.

  Negative or zero values are clamped to the first page of one row, and
  the page to the last one below `max-row`, since CQP treats a negative
  range bound as the entire result."
  [page page-size]
  (let [size (max 1 page-size)
        from (* (min (max 0 page) (quot (- max-row size) size)) size)]
    [from (dec (+ from size))]))

(def kwic-defaults
  "Default KWIC display options, shared by `kwic-commands` and
  dk.cst.corpus-probe.search so the two cannot disagree: five tokens of
  context and the rows of the default page."
  {:context 5
   :rows    (page-rows (:page page-defaults) (:page-size page-defaults))})

(defn corpus-name?
  "True when `s` is a syntactically valid uppercase CQP corpus name.

  Used as an interpolation guard: corpus names are spliced into command
  strings outside the QueryLock sandbox, so anything else must be rejected."
  [s]
  (boolean (re-matches #"[A-Z][A-Z0-9_-]*" (str s))))

(defn valid-corpus-name
  "Return `corpus` when it is a valid corpus name (see `corpus-name?`),
  else throw; the guard every command builder applies before splicing a
  corpus name into a command."
  [corpus]
  (when-not (corpus-name? corpus)
    (throw (ex-info "Invalid corpus name" {:corpus corpus})))
  corpus)

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

(defn filter-query
  "The CQP query matching every region accepted by `filter`: pairs of
  annotated s-attribute name and the values accepted, finest regions first.

  [[:text_year #{\"1591\" \"1583\"}]] matches the texts of either year;
  several attributes must all hold. The values are escaped and matched
  literally, those of one attribute as an alternation. The match is
  anchored at a region start of the first attribute, tests the others at
  that token (their regions containing it, hence the first attribute must
  have the finest regions) and expands to the first attribute's region, so
  that the result activated as a subcorpus restricts later queries to
  those regions: the CQP tutorial's metadata subcorpus idiom.

  (filter-query [[:s_id #{\"2\"}] [:text_year #{\"1591\"}]])
  ;; => <s_id = \"2\"> [_.text_year = \"1591\"] expand to s_id"
  [filter]
  (let [[[attr values] & more] filter
        ;; a TAB in a value would be flattened with the rest of the
        ;; command, so it goes in as the regex escape instead
        escape   (fn [value] (str/replace (escape-literal value) "\t" "\\t"))
        accepted (fn [values]
                   (str "\"" (str/join "|" (map escape (sort values))) "\""))]
    (str "<" (name attr) " = " (accepted values) "> "
         (if (seq more)
           (str "[" (str/join " & " (for [[attr values] more]
                                      (str "_." (name attr) " = "
                                           (accepted values))))
                "]")
           "[]")
         " expand to " (name attr))))

(defn restricted-query
  "The command string running user-supplied CQP `query` under QueryLock
  (see `locked-query`), within the regions of `filter` when there is one.

  The filter query (see `filter-query`) runs under its own lock, its
  result is activated as the subcorpus Filter, and the query then runs
  within it. One string, so it fills one section of a batch just as
  `locked-query` alone does. The activation sits outside the locks, being
  no query, and splices in nothing from the request. A filter matching no
  region makes an empty subcorpus, within which the query finds nothing;
  an attribute the corpus lacks fails the filter query, and CQP's own
  error reaches the caller."
  [query filter]
  (if (empty? filter)
    (locked-query query)
    (str (locked-query (filter-query filter))
         "\nFilter = Last;\nFilter;\n"
         (locked-query query))))

(defn position-query
  "A CQP query matching exactly the span from corpus position `cpos` to
  `matchend`, for re-fetching a known hit with wider context.

  `cpos` and `matchend` are coerced to integers, so no user text reaches the
  query; the span is anchored with the fast `_ = n` position test, guarded by
  the always-true `word=\".*\"` so the token constraint is not position-only
  (which older CQP forbids query-initially)."
  [cpos matchend]
  (let [n (- (long matchend) (long cpos))]
    (str "[word=\".*\" & _ = " (long cpos) "]"
         (when (pos? n) (str " []{" n "}")))))

(def sort-modes
  "The KWIC sort modes, in display order: each maps to the dictionary key
  labelling it (see dk.cst.corpus-probe.i18n/dictionary) and the CQP
  command that reorders the result `Last`. The context sorts order by the
  words nearest the match (up to five tokens either side)."
  [["corpus" :sort-corpus "sort Last;"]
   ["word"   :sort-word   "set ExternalSort on; sort Last by word;"]
   ["left"   :sort-left   "set ExternalSort on; sort Last by word on match[-1] .. match[-5];"]
   ["right"  :sort-right  "set ExternalSort on; sort Last by word on matchend[1] .. matchend[5];"]
   ["random" :sort-random "sort Last randomize 1;"]])

(defn sort-command
  "The CQP command that sorts `Last` for sort mode `mode` (see `sort-modes`);
  an unknown mode falls back to corpus order.

  Word sort delegates to CQP's ExternalSort so the collation follows the
  process locale (Danish, not byte order); random sort uses a fixed seed so
  pagination is stable across requests."
  [mode]
  (or (some (fn [[k _ command]] (when (= k mode) command)) sort-modes)
      "sort Last;"))

(defn kwic-commands
  "Build the command batch for the rows `:rows` of `query` (raw CQP) against
  `corpus`, returning a vector of command strings.

  Batch order, relied upon by dk.cst.corpus-probe.search:
  0 display profile + context, 1 corpus activation, 2 locked query,
  3 `size`, 4 sort, 5 `show` + `cat` rows, 6 `dump` rows, then one `tabulate`
  per entry of `struct-attrs`.

  `p-attrs` are the corpus's positional attributes (registry order) to show;
  `struct-attrs` the annotated s-attributes to fetch per hit. Each gets its
  own single-column `tabulate` command so that a whole output line is one
  annotation value. Annotation values may legally contain TAB, so packing
  them into one TAB-separated row would misalign the columns. `context` is
  in tokens; `rows` is the [from to] row range (see `page-rows`) of the hits
  to fetch, which `cat` clamps to the result silently. `sort` is a sort mode
  (see `sort-modes`); the `cat`, `dump` and `tabulate` rows then follow the
  sorted order. `filter` restricts the query to the regions of a metadata
  filter (see `restricted-query`)."
  [corpus query {:keys [p-attrs struct-attrs context rows sort filter]
                 :or   {context (:context kwic-defaults)
                        rows    (:rows kwic-defaults)}}]
  (let [[from to]  rows
        page-range (str "Last " from " " to)
        show       (when (next p-attrs)
                     (str "show " (str/join " " (map #(str "+" (name %))
                                                     (rest p-attrs))) "; "))]
    (into [(str hardened-profile " set Context " (long context) " words;")
           (str corpus ";")
           (restricted-query query filter)
           "size Last;"
           (sort-command sort)
           (str show "cat " page-range ";")
           (str "dump " page-range ";")]
          (map #(str "tabulate " page-range " match " (name %) ";"))
          struct-attrs)))
