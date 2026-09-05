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
  "Default KWIC display options, shared by `kwic-batch` and
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

(defn valid-result-name
  "Return `nqr` when it is a name CQP accepts for a query result, else
  throw: a letter or underscore followed by letters, digits, underscores
  and hyphens, which is CQP's own lexer rule for one.

  The guard every command builder applies before splicing a result name
  into a command, as `valid-corpus-name` does for a corpus name. CQP also
  rejects a name that is exactly one of its keywords, which the underscore
  every generated name carries rules out."
  [nqr]
  (when-not (re-matches #"[a-zA-Z_][a-zA-Z0-9_-]*" (str nqr))
    (throw (ex-info "Invalid query result name" {:nqr nqr})))
  nqr)

(defn valid-data-directory
  "Return `dir` when it can be embedded in a `set DataDirectory` command,
  else throw: no quote or backslash, which the double-quoted CQP string
  around it has no escape for, and no control character.

  A newline is the one that matters most: it ends the quoted string, and
  CQP reads the rest of the line as commands. The path is configured
  rather than requested, so this catches a misconfiguration rather than an
  attacker."
  [dir]
  (when-not (re-matches #"[^\"\\\p{Cntrl}]*" (str dir))
    (throw (ex-info "Invalid cache directory" {:cache-dir dir})))
  dir)

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

(defn simple->cqp
  "Compile simple-search `input` into a CQP query string.

  Each whitespace-separated word becomes one token pattern, following
  Korp's simple search: `[word = \"<escaped>\"]` with `.*` affixes for
  :prefix?/:suffix? and the %c flag for :case-insensitive?, matching the
  positional attribute :attr (default word) rather than the surface form
  when one is given. Returns nil for blank input rather than a
  match-everything query. The region the words are kept within is each
  corpus's own business (see `within-query`).

  (simple->cqp \"lille hund\" {:case-insensitive? true})
  ;; => [word = \"lille\" %c] [word = \"hund\" %c]

  (simple->cqp \"hund\" {:attr :lemma})
  ;; => [lemma = \"hund\"]"
  ([input]
   (simple->cqp input {}))
  ([input {:keys [case-insensitive? prefix? suffix? attr] :or {attr :word}}]
   (when-not (str/blank? input)
     (let [pattern (fn [word]
                     (str "[" (name attr) " = \""
                          (when suffix? ".*")
                          (escape-literal word)
                          (when prefix? ".*")
                          "\"" (when case-insensitive? " %c") "]"))]
       (->> (str/split (str/trim input) #"\s+")
            (map pattern)
            (str/join " "))))))

(defn within-query
  "`query` with its matches kept within one region of s-attribute `attr`,
  or `query` itself when `attr` is nil.

  The clause a compiled simple search of several words takes, so that
  they cannot be matched across a sentence boundary. It is never appended
  to CQP a reader wrote: that may carry a within clause already, and CQP
  allows one.

  (within-query \"[] []\" :s)
  ;; => [] [] within s"
  [query attr]
  (if attr
    (str query " within " (name attr))
    query))

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

(defn escape-pattern
  "Escape `pattern`, a regex a reader wrote, for a double-quoted CQP
  string: its quotes doubled, which is all it needs, its metacharacters
  being the point of it."
  [pattern]
  (str/replace pattern "\"" "\"\""))

(defn filter-query
  "The CQP query matching every region accepted by `filter`: triples of
  annotated s-attribute name, the values accepted and the patterns
  accepted (nil for none), finest regions first.

  [[:text_year #{\"1591\" \"1583\"}]] matches the texts of either year,
  and [[:text_year #{} [\"15..\"]]] those of any year the pattern
  matches; several attributes must all hold. The values are escaped and
  matched literally, the patterns as the regexes they are, each in a
  group of its own, all of one attribute as an alternation. The match is
  anchored at a region start of the first attribute, tests the others at
  that token (their regions containing it, hence the first attribute must
  have the finest regions) and expands to the first attribute's region, so
  that the result activated as a subcorpus restricts later queries to
  those regions: the CQP tutorial's metadata subcorpus idiom.

  (filter-query [[:s_id #{\"2\"}] [:text_year #{\"1591\"} [\"16..\"]]])
  ;; => <s_id = \"2\"> [_.text_year = \"1591|(16..)\"] expand to s_id"
  [filter]
  (let [[[attr values patterns] & more] filter
        group    (fn [pattern] (str "(" (escape-pattern pattern) ")"))
        accepted (fn [values patterns]
                   (str "\""
                        (str/join "|" (concat (map escape-value (sort values))
                                              (map group patterns)))
                        "\""))]
    (str "<" (name attr) " = " (accepted values patterns) "> "
         (if (seq more)
           (str "[" (str/join " & " (for [[attr values patterns] more]
                                      (str "_." (name attr) " = "
                                           (accepted values patterns))))
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
  ;; TODO: this same shape would restrict a query to the texts another
  ;; query matched, which is the one thing here CQP cannot say in a
  ;; single query. Verified to work: `expand to` runs under QueryLock,
  ;; and a second activation nests inside this filter's. Deferred rather
  ;; than dropped, for the reasons in PLAN.md section 3.
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

(def sample-seed
  "The seed CQP's random number generator is given before a sample is
  drawn, so that a URL naming a sample always names the same hits: the
  same result sampled twice is the same sample, in this process and in
  the next one. Fixed for the reason the random sort mode's seed is."
  1)

(defn sample-command
  "The command reducing the result `Last` to a random sample of `n` of
  its matches, or nil when `n` asks for no sample.

  CQP's own `reduce`, seeded so that the sample is reproducible. It has
  to run before both `size` and `sort`: the size to report is the
  sample's, and `reduce` discards the sort order of the result it
  reduces.

  A sample of more hits than there are is left to CQP, which keeps the
  whole result, being what a reader would expect. A sample of none of
  them is stopped here instead: CQP ignores `reduce ... to 0` silently,
  and a whole result would then be reported as a sample of none of it."
  [n]
  (when (and n (pos? n))
    (str "randomize " sample-seed "; reduce Last to " (long n) ";")))

(defn near-command
  "The commands keeping only the matches of Last that have a token
  matching `word` within `distance` tokens of them, on either side, and
  marking that token as their keyword anchor; nil without a word.

  The manual's own way of finding a word near a hit (section 3.7): the
  search runs from both ends of the match and never inside it, and a
  match left without a keyword is deleted. The word is matched literally
  and regardless of case, as a simple search matches one. Both commands
  run outside the QueryLock, being no queries, so the word is escaped as
  every spliced value is (see `escape-value`).

  (near-command {:word \"kat\" :distance 5})
  ;; => set Last keyword nearest [word = \"kat\" %c] within 5 words from
  ;;    match; delete Last without keyword;"
  ;; TODO: `delete ... without keyword` is in CQP's grammar and not in
  ;; its manual, and is verified on 3.5.0 only; check it on the
  ;; production 3.4.27 before relying on it there (PLAN.md appendix B).
  [{:keys [word distance]}]
  (when-not (str/blank? word)
    (str "set Last keyword nearest [word = \"" (escape-value word) "\" %c]"
         " within " (long distance) " words from match;"
         " delete Last without keyword;")))

(def positions
  "The positions of a match a result is counted or narrowed at, as CQP
  names them (manual sections 3.3 and 3.4): the token before the match,
  its first token, the whole of it as a range, its last token, and the
  token after it."
  ["match[-1]" "match" "match..matchend" "matchend" "matchend[1]"])

(defn valid-position
  "Return `position` when it is one of the `positions`, else throw: the
  guard every command builder applies before splicing one into a
  command."
  [position]
  (when-not (some #{position} positions)
    (throw (ex-info "Invalid position" {:position position})))
  position)

(defn whole-match?
  "True when `position` (see `positions`) is the whole match rather than
  one token of it, which CQP counts with `count` rather than `group` and
  prints differently."
  [position]
  (= "match..matchend" position))

(defn count-command
  "The command counting the values of `attr` at `position` (see
  `positions`) over the matches of Last: CQP's `count` over the whole
  match, whose output dk.cst.corpus-probe.parse/count->freqs reads, and
  its `group` at one token, read by group->freqs.

  Given the s-attribute `within`, `group` counts the regions of it each
  value occurs in rather than the matches (manual section 3.4): a
  document frequency, which `count` cannot give, so it is the caller's
  to ask for one token only."
  ([position attr]
   (count-command position attr nil))
  ([position attr within]
   (if (whole-match? (valid-position position))
     (str "count Last by " (name attr) ";")
     (str "group Last " position " " (name attr)
          (when within (str " within " (name within))) ";"))))

(defn subset-command
  "The commands keeping only the matches of Last whose token at `anchor`
  (see `positions`) has `value` as its `attr`: the hits one row of a
  frequency table counted.

  CQP's own subset at the ends of the match. Beside it, where subset
  cannot reach (its anchors take no offset), the keyword anchor is set on
  the one token there and the matches without one deleted, as
  `near-command` does, so that token is marked. The attribute is read
  through the this label, which reaches the value of a structural
  attribute as well as a positional one. Over the whole match, whose
  value is the string CQP's `count` printed, one token per space, the
  result is intersected with the query matching exactly that sequence,
  which keeps the matches that are it. All of it runs outside the
  QueryLock but the sequence query, so `attr` is checked against the
  corpus by the caller and `value` escaped here."
  [{:keys [anchor attr value]}]
  (let [pattern (str "[_." (name attr) " = \"" (escape-value value) "\"]")
        beside  (fn [side from]
                  (str "set Last keyword nearest " pattern " within " side
                       " 1 words from " from "; delete Last without keyword;"))
        ;; not `pattern`: CQP refuses the this label in query-initial
        ;; position, and the sequence query opens with its first token
        token   (fn [s] (str "[" (name attr) " = \"" (escape-value s) "\"]"))]
    (case (valid-position anchor)
      "match"       (str "Last = subset Last where match: " pattern ";")
      "matchend"    (str "Last = subset Last where matchend: " pattern ";")
      "match[-1]"   (beside "left" "match")
      "matchend[1]" (beside "right" "matchend")
      "match..matchend"
      (str "Q = Last;\n"
           (locked-query (str/join " " (map token (str/split value #" "))))
           "\nLast = intersection Q Last;"))))

(defn narrowing
  "The [section command] pairs narrowing the result Last as `opts` ask:
  to the matches with a value at an anchor (:subset, see
  `subset-command`), then to those with a word nearby (:near, see
  `near-command`), in that order, so that the word is looked for beside
  the hits that are kept. Empty when neither is asked for."
  [{:keys [subset near]}]
  (let [nearing (near-command near)]
    (cond-> []
      subset  (conj [:subset (subset-command subset)])
      nearing (conj [:near nearing]))))

(def sort-modes
  "The KWIC sort modes, in display order: each mode's `sort` param value
  and the CQP command that reorders the result `Last`. The context sorts
  order by the words nearest the match (up to five tokens either side).

  What each mode is called is the interface's business rather than this
  namespace's (see dk.cst.corpus-probe.views.page/sort-label)."
  (let [external "set ExternalSort on; sort Last by word"]
    [["corpus" "sort Last;"]
     ["word"   (str external ";")]
     ["left"   (str external " on match[-1] .. match[-5];")]
     ["right"  (str external " on matchend[1] .. matchend[5];")]
     ["random" "sort Last randomize 1;"]]))

(defn sort-command
  "The CQP command that sorts `Last` for sort mode `mode` (see `sort-modes`);
  an unknown mode falls back to corpus order.

  Word sort delegates to CQP's ExternalSort so the collation follows the
  process locale (Danish, not byte order); random sort uses a fixed seed so
  pagination is stable across requests."
  [mode]
  (or (some (fn [[k command]] (when (= k mode) command)) sort-modes)
      "sort Last;"))

(defn context-spec
  "The width of context as CQP's Context option takes it: `context` as a
  number of words either side of the match, or as an s-attribute
  keyword, one region of which is shown either side.

  (context-spec 5)
  ;; => 5 words

  (context-spec :s)
  ;; => 1 s"
  [context]
  (if (keyword? context)
    (str "1 " (name context))
    (str (long context) " words")))

(defn setup-command
  "The command configuring one KWIC batch: the hardened display profile,
  `context` (see `context-spec`) either side of the match and, when
  `cache-dir` is given, the directory CQP reads and writes saved query
  results in.

  DataDirectory is set here rather than beside the query because setting
  it rescans the corpus list, resetting the active corpus, so it has to
  come before the activation (docs/research/gap-nqr-persistence.md
  section 1)."
  [context cache-dir]
  (str (when cache-dir
         (str "set DataDirectory \"" (valid-data-directory cache-dir) "\"; "))
       hardened-profile " set Context " (context-spec context) ";"))

(defn page-commands
  "The [section command] pairs displaying the rows `[from to]` of the
  query result named `nqr`: a `:cat` section, the `:dump` anchors of the
  same rows and one `:tabulate` section per entry of `struct-attrs`.

  `p-attrs` are the corpus's positional attributes (registry order) to
  show. Each structural attribute gets its own single-column `tabulate`
  command so that a whole output line is one annotation value: annotation
  values may legally contain TAB, so packing them into one TAB-separated
  row would misalign the columns. `cat` clamps the range to the result
  silently, so a range past the last row needs no check."
  [nqr [from to] p-attrs struct-attrs]
  (let [span (str nqr " " from " " to)
        show (when (next p-attrs)
               (str "show " (str/join " " (map #(str "+" (name %))
                                               (rest p-attrs))) "; "))]
    (into [[:cat (str show "cat " span ";")]
           [:dump (str "dump " span ";")]]
          (map (fn [attr]
                 [:tabulate (str "tabulate " span " match " (name attr) ";")]))
          struct-attrs)))

(defn kwic-batch
  "The batch running `query` (raw CQP) against `corpus` and returning the
  rows `:rows` of its result: a vector of [section command] pairs, each
  section naming what its command's output holds (see `batch-sections`).

  `context` is a number of tokens or an s-attribute keyword (see
  `context-spec`); `rows` is the [from to] row range (see
  `page-rows`); `sort` is a sort mode (see `sort-modes`), which the `cat`,
  `dump` and `tabulate` rows then follow; `filter` restricts the query to
  the regions of a metadata filter (see `restricted-query`); `p-attrs`,
  `struct-attrs` and `cache-dir` are as `page-commands` and
  `setup-command` take them.

  `subset` and `near` narrow the result (see `narrowing`), and `sample`
  then reduces what is left to that many random matches (see
  `sample-command`), all before anything is counted or ordered, so that
  the size reported and the rows paged are those of the hits kept.

  Given `nqr`, the result is also saved under that name for
  `stored-kwic-batch` to page later. It is named only after being sorted,
  since the sort order travels with the result into the save file, which
  is what makes a stored result worth having."
  [corpus query {:keys [p-attrs struct-attrs context rows sort filter
                        sample cache-dir nqr]
                 :or   {context (:context kwic-defaults)
                        rows    (:rows kwic-defaults)}
                 :as   opts}]
  (let [sampling (sample-command sample)]
    (-> [[:setup  (setup-command context cache-dir)]
         [:corpus (str corpus ";")]
         [:query  (restricted-query query filter)]]
        (into (narrowing opts))
        (cond-> sampling (conj [:sample sampling]))
        (into [[:size "size Last;"]
               [:sort (sort-command sort)]])
        (cond-> nqr (conj (let [nqr (valid-result-name nqr)]
                            [:save (str nqr " = Last; save " nqr ";")])))
        (into (page-commands "Last" rows p-attrs struct-attrs)))))

(defn stored-kwic-batch
  "The batch returning the rows `:rows` of the saved query result named
  `nqr` of `corpus`: [section command] pairs, as `kwic-batch` returns.

  No query runs and nothing is sorted, the matches and their order both
  coming from the save file, so the options that decided them (the query,
  `sort`, `filter`, `subset`, `near` and `sample`) are none of this one's
  business."
  [corpus nqr {:keys [p-attrs struct-attrs context rows cache-dir]
               :or   {context (:context kwic-defaults)
                      rows    (:rows kwic-defaults)}}]
  (into [[:setup  (setup-command context cache-dir)]
         [:corpus (str corpus ";")]
         [:size   (str "size " (valid-result-name nqr) ";")]]
        (page-commands nqr rows p-attrs struct-attrs)))

(defn batch-sections
  "Group the output `results` of `batch` (its [section command] pairs) by
  section: a map of section key to the vector of that section's output
  line vectors, in batch order.

  A section key repeats, `:tabulate` doing so once per structural
  attribute, so every key holds a vector of sections rather than one."
  [batch results]
  (reduce (fn [m [[section _] lines]]
            (update m section (fnil conj []) lines))
          {}
          (map vector batch results)))
