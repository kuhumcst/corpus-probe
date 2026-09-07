(ns dk.cst.corpus-probe.cwb.command
  "The CQP commands a search is built from: the guards on what is spliced
  into a command, the queries built around a compiled one (a metadata
  filter, a position, the QueryLock), and the commands that narrow,
  sample, sort, count and load a result. Generated for CWB 3.5.0, the
  version the app ships with in its own container."
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
  dk.cst.corpus-probe.cqp/name?), else throw; the guard every command
  builder applies before splicing a result name into a command."
  [nqr]
  (when-not (cqp/name? nqr)
    (throw (ex-info "Invalid query result name" {:nqr nqr})))
  nqr)

(defn valid-data-directory
  "Return `dir` when it can be embedded in a `set DataDirectory` command,
  else throw: no quote or backslash, which the double-quoted CQP string
  around it has no escape for, and no control character."
  [dir]
  ;; a newline ends the quoted string, and CQP reads the rest of the line
  ;; as commands
  (when-not (re-matches #"[^\"\\\p{Cntrl}]*" (str dir))
    (throw (ex-info "Invalid cache directory" {:cache-dir dir})))
  dir)

(defn sentence-tags
  "`query` with its sentence tags, `<s>` and `</s>` standing between
  tokens, renamed after s-attribute `attr`, which is not every corpus's
  name for a sentence; `query` itself when `attr` is nil or `s`. A tag
  inside a quoted literal is left alone.

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
  CWB's usual name for it renamed after the s-attribute `attrs` gives
  that unit (unit to attribute), or dropped where it gives none, so the
  query runs in each corpus as far as the corpus marks the unit; `query`
  itself without such a clause.

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
  or `query` itself when `attr` is nil. Never appended to CQP a reader
  wrote, which may carry a within clause of its own.

  (within-query \"[] []\" :s)
  ;; => [] [] within s"
  [query attr]
  (if attr
    (str query " within " (name attr))
    query))

(defn locked-query
  "Wrap user-supplied CQP `query` in a QueryLock sandbox, returning one
  command string whose result lands in Last. Under QueryLock only queries
  execute; CQP itself rejects assignments, redirection and every other
  command."
  [query]
  (let [key   (inc (rand-int 999999))
        query (-> (cqp/flatten-whitespace query)
                  (str/trim)
                  (str/replace #";+\s*$" ""))]
    ;; the `;` and the unlock on lines of their own, or a trailing `#`
    ;; comment in the query swallows them
    (str "set QueryLock " key ";\n" query "\n;\nunlock " key ";")))

(defn filter-query
  "The CQP query matching every region accepted by `filter`: triples of
  annotated s-attribute name, the values accepted and the patterns
  accepted (nil for none), finest regions first. The values are matched
  literally, the patterns as the regexes they are; several attributes
  must all hold. Activated as a subcorpus, the result restricts later
  queries to those regions: the CQP tutorial's metadata subcorpus idiom.

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
    ;; the first attribute anchors the match and is expanded to, so it
    ;; must have the finest regions; the others are tested at its start
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
  (see `locked-query`), within the regions of `filter` when there is one:
  the filter query (see `filter-query`) under its own lock, its result
  activated as the subcorpus Filter, and the query within it. One string,
  so it fills one section of a batch as `locked-query` alone does."
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
  `matchend`, for re-fetching a known hit with wider context; both are
  coerced to integers, so no user text reaches the query."
  [cpos matchend]
  ;; the always-true word test: older CQP forbids a position-only
  ;; constraint query-initially
  (let [n (- (long matchend) (long cpos))]
    (str "[word=\".*\" & _ = " (long cpos) "]"
         (when (pos? n) (str " []{" n "}")))))

(def sample-seed
  "The seed CQP's random number generator is given before a sample is
  drawn, so that a URL naming a sample always names the same hits, in
  this process and in the next one."
  1)

(defn sample-command
  "The command reducing the result `Last` to a random sample of `n` of
  its matches, seeded so that the sample is reproducible; nil when `n`
  asks for no sample. Must run before `size` and `sort`: the size
  reported is the sample's, and `reduce` discards the sort order."
  [n]
  ;; CQP ignores `reduce ... to 0` silently, and the whole result would
  ;; then pass as a sample of none of it
  (when (and n (pos? n))
    (str "randomize " sample-seed "; reduce Last to " (long n) ";")))

(defn near-command
  "The commands keeping only the matches of Last that have a token
  matching `word` (literally, regardless of case) within `distance`
  tokens of them on either side, marking that token as their keyword
  anchor; nil without a word.

  (near-command {:word \"kat\" :distance 5})
  ;; => set Last keyword nearest [word = \"kat\" %c] within 5 words from
  ;;    match; delete Last without keyword;"
  [{:keys [word distance]}]
  ;; outside the QueryLock, being no query, so the word is escaped
  (when-not (str/blank? word)
    (str "set Last keyword nearest [word = \"" (cqp/escape-value word)
         "\" %c] within " (long distance) " words from match;"
         " delete Last without keyword;")))

(def positions
  "The positions of a match a result is counted or narrowed at, as CQP
  names them: the token before the match, its first token, the whole of
  it as a range, its last token, and the token after it."
  ["match[-1]" "match" "match..matchend" "matchend" "matchend[1]"])

(defn valid-position
  "Return `position` when it is one of the `positions`, else throw: the
  guard applied before one is spliced into a command."
  [position]
  (when-not (some #{position} positions)
    (throw (ex-info "Invalid position" {:position position})))
  position)

(defn whole-match?
  "True when `position` (see `positions`) is the whole match rather than
  one token of it, which CQP counts with `count` rather than `group`."
  [position]
  (= "match..matchend" position))

(defn count-command
  "The command counting the values of `attr` at `position` over the
  matches of Last: CQP's `count` over the whole match, its `group` at one
  token. Given the s-attribute `:within` of `opts`, `group` counts the
  regions each value occurs in rather than the matches: a document
  frequency. Given the attribute `:by`, it counts the values of `attr`
  against each value of that one: a cross-tabulation. `count` can do
  neither.

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
  frequency table counted. `attr` is checked against the corpus by the
  caller; `value` is escaped here."
  [{:keys [anchor attr value]}]
  (let [;; the this label reaches the value of a structural attribute as
        ;; well as a positional one
        pattern (str "[_." (name attr) " = \"" (cqp/escape-value value)
                     "\"]")
        ;; subset's anchors take no offset, so beside the match the keyword
        ;; anchor marks the token instead, as `near-command` does
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
  to the matches with a value at an anchor (:subset), then to those with
  a word nearby (:near), in that order, so that the word is looked for
  beside the hits that are kept; empty when neither is asked for."
  [{:keys [subset near]}]
  (let [nearing (near-command near)]
    (cond-> []
      subset  (conj [:subset (subset-command subset)])
      nearing (conj [:near nearing]))))

(def sort-modes
  "The KWIC sort modes, in display order: each mode's `sort` param value
  and the CQP command that reorders the result `Last`. The context sorts
  order by the words nearest the match; the reverse sort orders the
  matches by their word read from its end, putting words with one suffix
  together. A mode naming a positional attribute sorts by that attribute
  instead (see `sort-attr`)."
  ;; ExternalSort, so the collation follows the locale rather than byte
  ;; order; a fixed random seed keeps pagination stable across requests
  (let [external "set ExternalSort on; sort Last by word"]
    [["corpus"  "sort Last;"]
     ["word"    (str external ";")]
     ["reverse" (str external " reverse;")]
     ["left"    (str external " on match[-1] .. match[-5];")]
     ["right"   (str external " on matchend[1] .. matchend[5];")]
     ["random"  "sort Last randomize 1;"]]))

(defn sort-attr
  "The positional attribute the sort mode `mode` orders the matches by: a
  keyword when `mode` is an attribute name rather than one of the
  `sort-modes`; nil otherwise.

  (sort-attr \"lemma\")
  ;; => :lemma"
  [mode]
  (when (and (not (some #{mode} (map first sort-modes)))
             (cqp/name? mode))
    (keyword mode)))

(defn sort-command
  "The CQP command that sorts `Last` for sort mode `mode`: one of the
  `sort-modes`, or a positional attribute to sort the matches by (see
  `sort-attr`); anything else falls back to corpus order."
  [mode]
  (or (some (fn [[k command]] (when (= k mode) command)) sort-modes)
      (when-let [attr (sort-attr mode)]
        (str "set ExternalSort on; sort Last by " (name attr) ";"))
      "sort Last;"))

(defn load-command
  "The command making the saved query result named `nqr` of `corpus` in
  `cache-dir` its result Last, for commands that count one.

  (load-command \"PROBE\" \"q_1\" \"/cache/PROBE\")
  ;; => set DataDirectory \"/cache/PROBE\"; PROBE; Last = q_1;"
  [corpus nqr cache-dir]
  ;; the directory first: setting it rescans the corpus list, which drops
  ;; the activation
  (str "set DataDirectory \"" (valid-data-directory cache-dir) "\"; "
       corpus "; Last = " (valid-result-name nqr) ";"))
