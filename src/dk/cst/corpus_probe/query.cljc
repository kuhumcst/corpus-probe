(ns dk.cst.corpus-probe.query
  "The query a search asks, compiled to CQP: the words of a simple search
  in order, a list of words as alternatives and the tokens of an extended
  search, each word or value escaped for a double-quoted CQP literal; or
  CQP as the reader wrote it, passed through.

  The compilers read the search params as the form submits them and a
  URL carries them (see dk.cst.corpus-probe.url). Shared by the server
  and the client, so both can say what a form will run. The commands the
  compiled query is run by are dk.cst.corpus-probe.commands'."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.url :as url]))

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

(defn simple->cqp
  "Compile simple-search `input` into a CQP query string.

  Each whitespace-separated word becomes one token pattern, following
  Korp's simple search: `[word = \"<escaped>\"]` with `.*` affixes for
  :prefix?/:suffix? and the %c flag for :case-insensitive?, matching the
  positional attribute :attr (default word) rather than the surface form
  when one is given. Under :list?, the input is a list of words, one per
  line or several to a line, and the query is the one token pattern
  matching any of them: an alternation, which the affixes and the flag
  then apply to as a whole.
  Returns nil for blank input rather than a match-everything query. The
  region the words are kept within is each corpus's own business (see
  dk.cst.corpus-probe.commands/within-query).

  (simple->cqp \"lille hund\" {:case-insensitive? true})
  ;; => [word = \"lille\" %c] [word = \"hund\" %c]

  (simple->cqp \"hund\" {:attr :lemma})
  ;; => [lemma = \"hund\"]

  (simple->cqp \"hund\\nkat\" {:list? true :prefix? true})
  ;; => [word = \"(hund|kat).*\"]"
  ([input]
   (simple->cqp input {}))
  ([input {:keys [case-insensitive? prefix? suffix? attr list?]
           :or   {attr :word}}]
   (when-not (str/blank? input)
     (let [pattern (fn [literal]
                     (str "[" (name attr) " = \""
                          (when suffix? ".*")
                          literal
                          (when prefix? ".*")
                          "\"" (when case-insensitive? " %c") "]"))
           words   (remove str/blank? (str/split input #"\s+"))]
       (if list?
         (let [alternatives (map escape-literal (distinct words))]
           (pattern (str "(" (str/join "|" alternatives) ")")))
         (str/join " " (map (comp pattern escape-literal) words)))))))

(defn regex-value
  "The regular expression `value` as a reader wrote it, made safe for a
  double-quoted CQP literal: quotes doubled and line breaks and TABs
  flattened (see `flatten-whitespace`). Backslashes are the reader's own,
  so a regex ending in one is CQP's to refuse, as it is in CQP mode."
  [value]
  (str/replace (flatten-whitespace (str value)) "\"" "\"\""))

(defn condition->cqp
  "The CQP condition of extended-search `condition`: its :attr (a
  positional attribute name, word by default) related by :op (see
  dk.cst.corpus-probe.url/operators) to its :value, the literal escaped
  (see `escape-value`) or, under a regex operator, kept as written (see
  `regex-value`), with the %c flag under :ci?.

  (condition->cqp {:attr :lemma :op \"is\" :value \"hund\" :ci? true})
  ;; => lemma = \"hund\" %c"
  [{:keys [attr op value ci?] :or {attr :word op "is"}}]
  (let [literal (escape-value (str value))
        pattern (case op
                  "prefix"              (str literal ".*")
                  "suffix"              (str ".*" literal)
                  "infix"               (str ".*" literal ".*")
                  ("regex" "not-regex") (regex-value value)
                  literal)]
    (str (name attr)
         (if (#{"not" "not-regex"} op) " != \"" " = \"")
         pattern "\""
         (when ci? " %c"))))

(defn condition-groups
  "The `conditions` of a token in the groups their :join makes (see
  dk.cst.corpus-probe.url/joins): the first opens the first group, each
  `or` adds an alternative to the current one and anything else opens a
  new one."
  [conditions]
  (reduce (fn [groups {:keys [join] :as condition}]
            (if (and (seq groups) (= "or" join))
              (update groups (dec (count groups)) conj condition)
              (conj groups [condition])))
          []
          conditions))

(defn token->cqp
  "The CQP token pattern of extended-search `token`: its :conditions (see
  `condition->cqp`) in the groups their joins make (see
  `condition-groups`), the alternatives of a group joined by | and the
  groups by &, in parentheses where both occur, so that KORP's reading
  holds: the ors bind tighter than the ands, the reverse of CQP's own.
  Any word when the first condition is `any`. Repeated :min to :max
  times when that is not once, and opening a sentence under :start? and
  closing one under :end? as `<s>` tags, which each corpus then names
  after its own sentence attribute (see
  dk.cst.corpus-probe.commands/sentence-tags).

  (token->cqp {:conditions [{:attr :lemma :op \"is\" :value \"hund\"}
                            {:join \"or\" :attr :lemma :op \"is\" :value \"kat\"}
                            {:join \"and\" :attr :pos :op \"prefix\" :value \"N\"}]})
  ;; => [(lemma = \"hund\" | lemma = \"kat\") & pos = \"N.*\"]

  (token->cqp {:conditions [{:op \"any\"}] :min 0 :max 2})
  ;; => []{0,2}"
  [{:keys [conditions start? end?] lo :min hi :max :or {lo 1 hi 1}}]
  (let [groups (map #(map condition->cqp %) (condition-groups conditions))
        body   (when-not (= "any" (:op (first conditions)))
                 (str/join " & " (for [alts groups
                                       :let [alt (str/join " | " alts)]]
                                   (if (and (next groups) (next alts))
                                     (str "(" alt ")")
                                     alt))))]
    (str (when start? "<s> ")
         "[" body "]"
         (when-not (= [1 1] [lo hi])
           (str "{" lo "," hi "}"))
         (when end? " </s>"))))

(defn extended->cqp
  "Compile the extended-search `tokens` (see `token->cqp`) into a CQP
  query string: one token pattern each, in order; nil without tokens.

  (extended->cqp [{:conditions [{:attr :pos :op \"prefix\" :value \"N\"}]}
                  {:conditions [{:op \"any\"}] :max 2}
                  {:conditions [{:attr :word :op \"is\" :value \"hund\"}]}])
  ;; => [pos = \"N.*\"] []{1,2} [word = \"hund\"]"
  [tokens]
  (when (seq tokens)
    (str/join " " (map token->cqp tokens))))

(defn attribute-name?
  "True when `s` is a syntactically valid CQP attribute name: a letter or
  underscore followed by letters, digits, underscores and hyphens.

  An interpolation guard like dk.cst.corpus-probe.commands/corpus-name?:
  an attribute a reader chose to sort by is spliced into a command
  outside the QueryLock, so it is held to CQP's own lexer rule for a
  name here and checked against the corpus's inventory by
  dk.cst.corpus-probe.search."
  [s]
  (boolean (re-matches #"[a-zA-Z_][a-zA-Z0-9_-]*" (str s))))

(defn match-param
  "The affixes the `match` query param value `v` asks `simple->cqp` for,
  as its options: the start of the form matched for prefix, the end for
  suffix, either for infix, and none for the whole form, which is what a
  URL leaves out.

  One param rather than one per end, because a query that may fall at
  either end may fall anywhere, and two params both set said so to
  nobody."
  [v]
  (case v
    "prefix" {:prefix? true}
    "suffix" {:suffix? true}
    "infix"  {:prefix? true :suffix? true}
    {}))

(defn repeat-param
  "The repeat query param value `v` as a number of tokens: an integer
  from 0 to 99, else `default`."
  [v default]
  (let [n (some-> v str parse-long)]
    (if (and n (<= 0 n 99)) n default)))

(defn within-param
  "The unit of text the `within` query param value `v` names (see
  dk.cst.corpus-probe.url/units): the sentence unless it names another."
  [v]
  (if (some #{v} url/units) (keyword v) :sentence))

(defn condition-params
  "The condition `row` of an extended-search token (see
  dk.cst.corpus-probe.url/token-rows) as `condition->cqp` takes it: its
  :attr (a keyword; word unless the name is a plausible attribute name,
  since it is spliced into the query), its :op (one of
  dk.cst.corpus-probe.url/operators, equality otherwise), its :value as
  typed, :ci? for its ignore-case box and, when it has one, its :join
  (one of dk.cst.corpus-probe.url/joins, and otherwise)."
  [{:keys [attr op v ci join]}]
  (cond-> {:attr  (keyword (if (attribute-name? (str attr)) attr "word"))
           :op    (if (some #{op} url/operators) op "is")
           :value (str v)
           :ci?   (some? ci)}
    (some? join) (assoc :join (if (some #{join} url/joins) join "and"))))

(defn token-params
  "The tokens of the extended search `params` describe, as
  `extended->cqp` takes them: each token asking for anything (see
  dk.cst.corpus-probe.url/asks?), in order, with the
  :conditions of it that ask (see dk.cst.corpus-probe.url/condition-asks?
  and `condition-params`), its repeat as :min and :max (see
  `repeat-param`), the most never below the least, and :start? and :end?
  for the sentence edges it stands at."
  [params]
  (into []
        (comp (filter url/asks?)
              (map (fn [{:keys [conditions start end] lo :min hi :max}]
                     (let [lo (repeat-param lo 1)]
                       {:conditions (mapv condition-params
                                          (filter url/condition-asks?
                                                  conditions))
                        :min        lo
                        :max        (max lo (repeat-param hi lo))
                        :start?     (some? start)
                        :end?       (some? end)}))))
        (url/token-rows params)))

(defn ->cqp
  "The CQP query for `params`: CQP-mode input as typed, an extended
  search compiled from its tokens (see `token-params` and
  `extended->cqp`), and simple-mode input, or a list of words in list
  mode, compiled (see `simple->cqp`); nil when there is nothing to
  search for.

  Simple is the default and CQP mode is opt-in: a request naming no mode
  is read as a plain word search, since CQP mode answers a bare word with
  a parse error naming a corpus the reader never mentioned."
  [{:keys [q mode ci match in] :as params}]
  (case mode
    "cqp"      (when-not (str/blank? q) q)
    "extended" (extended->cqp (token-params params))
    (when-not (str/blank? q)
      (simple->cqp q (assoc (match-param match)
                            :case-insensitive? (some? ci)
                            :attr  (keyword (or (url/present in)
                                                (url/default :in)))
                            :list? (= mode "list"))))))

(defn within-unit
  "The unit of text the search `params` describe is kept within (see
  dk.cst.corpus-probe.search/units): the one the `within` param names,
  the sentence by default (see `within-param`), for a simple or an
  extended search of several tokens, which should not be matched across
  a boundary, and for an extended search of a token that opens or
  closes a sentence.

  Nil for one token, which cannot straddle a boundary and could only be
  refused, where it stands outside every sentence; nil for a list, which
  is one token; and nil for CQP, which says so itself."
  [{:keys [q mode within] :as params}]
  (when (case mode
          ("cqp" "list") false
          "extended"     (let [tokens (token-params params)]
                           (or (next tokens)
                               (some #(or (:start? %) (:end? %)) tokens)))
          (next (str/split (str/trim (str q)) #"\s+")))
    (within-param within)))

