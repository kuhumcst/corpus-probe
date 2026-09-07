(ns dk.cst.corpus-probe.cwb.command
  "The CQP commands a search is built from: the guards on what is spliced
  into a command, the queries built around a compiled one (a metadata
  filter, a position, the QueryLock), and the commands that narrow,
  sample, sort, count and load a result. The batches a search runs are
  dk.cst.corpus-probe.search.batch's; the compilers of what a reader asked
  are dk.cst.corpus-probe.query's.

  The commands are generated for CWB 3.5.0, the version the app ships
  with in its own container (PLAN.md section 2); appendix B there records
  the 3.4.27-safe subset for reference."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.cqp :as cqp]))

(defn valid-corpus-name
  "Return `corpus` when it is a valid corpus name (see
  dk.cst.corpus-probe.cqp/corpus-name?), else throw; the guard every
  command builder applies before splicing a corpus name into a command."
  [corpus]
  (when-not (cqp/corpus-name? corpus)
    (throw (ex-info "Invalid corpus name" {:corpus corpus})))
  corpus)

(defn valid-result-name
  "Return `nqr` when it is a name CQP accepts for a query result (see
  dk.cst.corpus-probe.cqp/name?), else throw.

  The guard every command builder applies before splicing a result name
  into a command, as `valid-corpus-name` does for a corpus name. CQP also
  rejects a name that is exactly one of its keywords, which the underscore
  every generated name carries rules out."
  [nqr]
  (when-not (cqp/name? nqr)
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

(defn sentence-tags
  "`query` with its sentence tags, `<s>` and `</s>` standing between
  tokens, named after s-attribute `attr`: a compiled extended search
  opens and closes a sentence by CWB's usual name for one (see
  dk.cst.corpus-probe.query/token->cqp), which is not every corpus's (see
  dk.cst.corpus-probe.cwb.corpus/units). `query` itself when `attr` is
  nil or `s`. A tag inside a quoted literal is left alone, standing after
  a quote rather than a space.

  (sentence-tags \"<s> [word = \\\"x\\\"] </s>\" :sentence)
  ;; => <sentence> [word = \"x\"] </sentence>"
  [query attr]
  (if (and attr (not= :s (keyword attr)))
    (-> query
        (str/replace #"(^|\s)<s>(?=\s)" (str "$1<" (name attr) ">"))
        (str/replace #"(?<=\s)</s>(?=\s|$)" (str "</" (name attr) ">")))
    query))

(def within-pattern
  "A query ending in a `within` clause that names a unit of text by its
  CQP name (see dk.cst.corpus-probe.cqp/units): the query before the
  clause, then the name."
  (re-pattern (str "(?s)(.*\\S)\\s+within\\s+("
                   (str/join "|" (map second cqp/units))
                   ")\\s*;?")))

(defn within-clause
  "`query` with a `within` clause at its end naming a unit of text by
  CWB's usual name for it (see dk.cst.corpus-probe.cqp/units) renamed
  after the s-attribute `attrs` gives that unit (unit to attribute), or
  dropped where it gives none, so that a query kept within a unit runs
  in each corpus as far as the corpus marks it, as `sentence-tags` does
  for the tags: the CQP a switch to that mode holds (see
  dk.cst.corpus-probe.query/project), and a reader's own that says the
  same. `query` itself without such a clause.

  (within-clause \"[] [] within s\" {:sentence :sentence})
  ;; => [] [] within sentence"
  [query attrs]
  (let [units (into {} (map (fn [[unit s]] [s unit])) cqp/units)]
    (if-let [[_ head unit] (re-matches within-pattern query)]
      (if-let [attr (get attrs (get units unit))]
        (str head " within " (name attr))
        head)
      query)))

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
        query (-> (cqp/flatten-whitespace query)
                  (str/trim)
                  (str/replace #";+\s*$" ""))]
    (str "set QueryLock " key ";\n" query "\n;\nunlock " key ";")))

(defn filter-query
  "The CQP query matching every region accepted by `filter`: triples of
  annotated s-attribute name, the values accepted and the patterns
  accepted (nil for none), finest regions first.

  [[:text_year #{\"1591\" \"1583\"}]] matches the texts of either year,
  and [[:text_year #{} [\"15..\"]]] those of any year the pattern
  matches; several attributes must all hold. The values are escaped and
  matched literally, the patterns as the regexes they are (see
  dk.cst.corpus-probe.cqp/regex-value), each in a group of its own, all
  of one attribute as an alternation. The match is
  anchored at a region start of the first attribute, tests the others at
  that token (their regions containing it, hence the first attribute must
  have the finest regions) and expands to the first attribute's region, so
  that the result activated as a subcorpus restricts later queries to
  those regions: the CQP tutorial's metadata subcorpus idiom.

  (filter-query [[:s_id #{\"2\"}] [:text_year #{\"1591\"} [\"16..\"]]])
  ;; => <s_id = \"2\"> [_.text_year = \"1591|(16..)\"] expand to s_id"
  [filter]
  (let [[[attr values patterns] & more] filter
        group    (fn [pattern] (str "(" (cqp/regex-value pattern) ")"))
        accepted (fn [values patterns]
                   (str "\""
                        (str/join "|" (concat (map cqp/escape-value
                                                   (sort values))
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
  every spliced value is (see dk.cst.corpus-probe.cqp/escape-value).

  (near-command {:word \"kat\" :distance 5})
  ;; => set Last keyword nearest [word = \"kat\" %c] within 5 words from
  ;;    match; delete Last without keyword;"
  [{:keys [word distance]}]
  (when-not (str/blank? word)
    (str "set Last keyword nearest [word = \"" (cqp/escape-value word)
         "\" %c] within " (long distance) " words from match;"
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
  match, whose output dk.cst.corpus-probe.cwb.parse/count->freqs reads,
  and its `group` at one token, read by group->freqs.

  Given the s-attribute `:within` of `opts`, `group` counts the regions
  of it each value occurs in rather than the matches (manual section
  3.4): a document frequency. Given the attribute `:by`, it counts the
  values of `attr` against each value of that one at the match, a
  cross-tabulation read by group-pairs->freqs. `count` can do neither,
  so both are the caller's to ask for at one token only.

  (count-command \"match\" :lemma {:by :text_year})
  ;; => group Last match lemma by match text_year;"
  ([position attr]
   (count-command position attr {}))
  ([position attr {:keys [within by]}]
   (if (whole-match? (valid-position position))
     (str "count Last by " (name attr) ";")
     (str "group Last " position " " (name attr)
          (when by (str " by match " (name by)))
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
  (let [pattern (str "[_." (name attr) " = \"" (cqp/escape-value value)
                     "\"]")
        beside  (fn [side from]
                  (str "set Last keyword nearest " pattern " within " side
                       " 1 words from " from "; delete Last without keyword;"))
        ;; not `pattern`: CQP refuses the this label in query-initial
        ;; position, and the sequence query opens with its first token
        token   (fn [s]
                  (str "[" (name attr) " = \"" (cqp/escape-value s) "\"]"))]
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
  order by the words nearest the match (up to five tokens either side);
  the reverse sort orders the matches by their word read from its end,
  which puts the words with one suffix together (manual section 3.3).

  A mode naming a positional attribute sorts by that attribute instead
  (see `sort-attr`). What each mode is called is the interface's
  business rather than this namespace's (see
  dk.cst.corpus-probe.views.concordance/sort-label)."
  (let [external "set ExternalSort on; sort Last by word"]
    [["corpus"  "sort Last;"]
     ["word"    (str external ";")]
     ["reverse" (str external " reverse;")]
     ["left"    (str external " on match[-1] .. match[-5];")]
     ["right"   (str external " on matchend[1] .. matchend[5];")]
     ["random"  "sort Last randomize 1;"]]))

(defn sort-attr
  "The positional attribute the sort mode `mode` orders the matches by:
  a keyword when `mode` is an attribute name (see
  dk.cst.corpus-probe.cqp/name?) rather than one of the `sort-modes`;
  nil otherwise.

  (sort-attr \"lemma\")
  ;; => :lemma"
  [mode]
  (when (and (not (some #{mode} (map first sort-modes)))
             (cqp/name? mode))
    (keyword mode)))

(defn sort-command
  "The CQP command that sorts `Last` for sort mode `mode`: one of the
  `sort-modes`, or a positional attribute to sort the matches by (see
  `sort-attr`); anything else falls back to corpus order.

  Word and attribute sorts delegate to CQP's ExternalSort so the
  collation follows the process locale (Danish, not byte order); random
  sort uses a fixed seed so pagination is stable across requests."
  [mode]
  (or (some (fn [[k command]] (when (= k mode) command)) sort-modes)
      (when-let [attr (sort-attr mode)]
        (str "set ExternalSort on; sort Last by " (name attr) ";"))
      "sort Last;"))

(defn load-command
  "The command making the saved query result named `nqr` of `corpus` in
  `cache-dir` its result Last, for commands that count one: the
  directory first, since setting it rescans the corpus list (see
  dk.cst.corpus-probe.search.batch/setup-command), then the activation,
  then the copy, which is what reads the file.

  (load-command \"PROBE\" \"q_1\" \"/cache/PROBE\")
  ;; => set DataDirectory \"/cache/PROBE\"; PROBE; Last = q_1;"
  [corpus nqr cache-dir]
  (str "set DataDirectory \"" (valid-data-directory cache-dir) "\"; "
       corpus "; Last = " (valid-result-name nqr) ";"))
