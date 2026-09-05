(ns dk.cst.corpus-probe.query
  "The query a search asks, as one value: the tokens of an extended search
  kept within a unit of text, which the words of a simple search and the
  alternatives of a list are too, at their own sizes; or CQP as the
  reader wrote it, which the app compiles into and never out of. It is
  read from the search params by the mode that carries them (see `of`),
  printed back as the params of a mode (see `params`), compiled to CQP
  (see `->cqp`), and held by each of the four forms as far as that form
  can (see `project` and `loss`).

  Every word or value is escaped for a double-quoted CQP literal. Shared
  by the server and the client, so both can say what a form will run.
  The commands the compiled query is run by are
  dk.cst.corpus-probe.commands'."
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

(defn regex-value
  "The regular expression `value` as a reader wrote it, made safe for a
  double-quoted CQP literal: quotes doubled and line breaks and TABs
  flattened (see `flatten-whitespace`). Backslashes are the reader's own,
  so a regex ending in one is CQP's to refuse, as it is in CQP mode."
  [value]
  (str/replace (flatten-whitespace (str value)) "\"" "\"\""))

(def literal-ops
  "The operators (see dk.cst.corpus-probe.url/operators) that match a
  literal value, escaped, as an equality with affixes: the ones a simple
  search or a list has, and the ones alternatives on one attribute can
  be written as one alternation (see `alternatives?`)."
  #{"is" "prefix" "suffix" "infix"})

(defn affixed
  "`literal`, escaped already, with the affixes of the literal operator
  `op`: `.*` after it for prefix, before it for suffix, both for infix."
  [op literal]
  (case op
    "prefix" (str literal ".*")
    "suffix" (str ".*" literal)
    "infix"  (str ".*" literal ".*")
    literal))

(defn condition->cqp
  "The CQP condition of extended-search `condition`: its :attr (a
  positional attribute name, word by default) related by :op (see
  dk.cst.corpus-probe.url/operators) to its :value, the literal escaped
  (see `escape-value`) and affixed (see `affixed`) or, under a regex
  operator, kept as written (see `regex-value`), with the %c flag under
  :ci?.

  (condition->cqp {:attr :lemma :op \"is\" :value \"hund\" :ci? true})
  ;; => lemma = \"hund\" %c"
  [{:keys [attr op value ci?] :or {attr :word op "is"}}]
  (str (name attr)
       (if (#{"not" "not-regex"} op) " != \"" " = \"")
       (if (#{"regex" "not-regex"} op)
         (regex-value value)
         (affixed op (escape-value (str value))))
       "\""
       (when ci? " %c")))

(defn literal-shape
  "What `condition` matches its value with: its attribute, its operator
  and its case flag, defaults filled in, so that two conditions of one
  shape differ in their value alone."
  [{:keys [attr op ci?]}]
  [(or attr :word) (or op "is") (boolean ci?)])

(defn alternatives?
  "True when `conditions`, one group of a token (see `condition-groups`),
  are alternatives of one literal value: two or more, all of one shape
  (see `literal-shape`) with a literal operator (see `literal-ops`).
  What a list is, and what compiles as one alternation (see
  `alternation->cqp`)."
  [conditions]
  (let [[_ op] (literal-shape (first conditions))]
    (boolean (and (next conditions)
                  (contains? literal-ops op)
                  (apply = (map literal-shape conditions))))))

(defn alternation->cqp
  "The CQP condition of `conditions` that are alternatives of one literal
  value (see `alternatives?`): the values escaped, as one alternation,
  affixed and flagged as the one condition they amount to.

  (alternation->cqp [{:attr :lemma :op \"prefix\" :value \"hund\"}
                     {:attr :lemma :op \"prefix\" :value \"kat\" :join \"or\"}])
  ;; => lemma = \"(hund|kat).*\""
  [conditions]
  (let [[attr op ci?] (literal-shape (first conditions))
        values        (map #(escape-value (str (:value %))) conditions)]
    (str (name attr) " = \""
         (affixed op (str "(" (str/join "|" values) ")"))
         "\""
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

(defn group->cqp
  "The CQP of one group of `conditions` (see `condition-groups`): one
  alternation when they are alternatives of one literal value (see
  `alternatives?`), else each condition (see `condition->cqp`) joined by
  |, in parentheses when `grouped?` and there are several, so that the
  ors bind tighter than the ands they stand among."
  [conditions grouped?]
  (if (alternatives? conditions)
    (alternation->cqp conditions)
    (let [alts (str/join " | " (map condition->cqp conditions))]
      (if (and grouped? (next conditions)) (str "(" alts ")") alts))))

(defn token->cqp
  "The CQP token pattern of extended-search `token`: its :conditions in
  the groups their joins make (see `group->cqp`), the groups joined by
  &, in parentheses where a group of several alternatives stands among
  other groups, so that KORP's reading holds: the ors bind tighter than
  the ands, the reverse of CQP's own. Any word when the first condition
  is `any`. Repeated :min to :max times when that is not once, and
  opening a sentence under :start? and closing one under :end? as `<s>`
  tags, which each corpus then names after its own sentence attribute
  (see dk.cst.corpus-probe.commands/sentence-tags).

  (token->cqp {:conditions [{:attr :lemma :op \"is\" :value \"hund\"}
                            {:join \"or\" :attr :lemma :op \"is\" :value \"kat\"}
                            {:join \"and\" :attr :pos :op \"prefix\" :value \"N\"}]})
  ;; => [lemma = \"(hund|kat)\" & pos = \"N.*\"]

  (token->cqp {:conditions [{:op \"any\"}] :min 0 :max 2})
  ;; => []{0,2}"
  [{:keys [conditions start? end?] lo :min hi :max :or {lo 1 hi 1}}]
  (let [groups (condition-groups conditions)
        body   (when-not (= "any" (:op (first conditions)))
                 (str/join " & " (map #(group->cqp % (boolean (next groups)))
                                      groups)))]
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

(defn match-op
  "The operator (see `literal-ops`) the `match` query param value `v`
  gives every word of a simple search or a list: the start of the form
  matched for prefix, the end for suffix, either for infix, and equality
  for the whole form, which is what a URL leaves out.

  One param rather than one per end, because a query that may fall at
  either end may fall anywhere, and two params both set said so to
  nobody."
  [v]
  (if (contains? #{"prefix" "suffix" "infix"} v) v "is"))

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
  typed and :ci? for its ignore-case box. Its :join is its token's to
  read (see `token-params`)."
  [{:keys [attr op v ci]}]
  {:attr  (keyword (if (attribute-name? (str attr)) attr "word"))
   :op    (if (some #{op} url/operators) op "is")
   :value (str v)
   :ci?   (some? ci)})

(defn joined
  "`conditions` (see `condition-params`) with the :join of each after the
  first read from its `join` field: one of dk.cst.corpus-probe.url/joins,
  and otherwise; the first has none, since it joins nothing."
  [conditions]
  (into []
        (map-indexed (fn [i {:keys [join] :as condition}]
                       (if (zero? i)
                         (dissoc condition :join)
                         (assoc condition
                                :join (if (some #{join} url/joins)
                                        join
                                        "and")))))
        conditions))

(defn token-params
  "The tokens of the extended search `params` describe, as
  `extended->cqp` takes them: each token asking for anything (see
  dk.cst.corpus-probe.url/asks?), in order, with the :conditions of it
  that ask (see dk.cst.corpus-probe.url/condition-asks?,
  `condition-params` and `joined`), its repeat as :min and :max (see
  `repeat-param`), the most never below the least, and :start? and :end?
  for the sentence edges it stands at."
  [params]
  (into []
        (comp (filter url/asks?)
              (map (fn [{:keys [conditions start end] lo :min hi :max}]
                     (let [lo (repeat-param lo 1)]
                       {:conditions (joined
                                     (map (fn [{:keys [join] :as row}]
                                            (assoc (condition-params row)
                                                   :join join))
                                          (filter url/condition-asks?
                                                  conditions)))
                        :min        lo
                        :max        (max lo (repeat-param hi lo))
                        :start?     (some? start)
                        :end?       (some? end)}))))
        (url/token-rows params)))

(defn condition
  "The condition matching `word` as the options of a simple search or a
  list say: the `in` attribute (word by default), the `match` operator
  (see `match-op`) and the `ci` flag."
  [{:keys [in ci match]} word]
  {:attr  (keyword (or (url/present in) (url/default :in)))
   :op    (match-op match)
   :value word
   :ci?   (some? ci)})

(defn token
  "A token of `conditions`, once, at no sentence edge: what a word of a
  simple search or the alternatives of a list are."
  [conditions]
  {:conditions (vec conditions) :min 1 :max 1 :start? false :end? false})

(defn words
  "The words of `q`, whitespace-separated as a search box or a list holds
  them; none for a blank."
  [q]
  (remove str/blank? (str/split (str q) #"\s+")))

(defn of
  "The query the search `params` carry, read by their mode (see
  dk.cst.corpus-probe.url/mode): CQP as typed, `{:cqp text}`; or tokens
  kept within a unit, `{:tokens [...] :within :sentence}`, the tokens of
  an extended search (see `token-params`), the words of a simple search
  one token each, or the words of a list as one token of alternatives
  (see `condition` and `token`), the unit the one the `within` param
  names where the mode reads it (see `within-param`). Nil when nothing is
  asked, which counts every token.

  Each mode reads its own field (see dk.cst.corpus-probe.url/field) and
  what a mode does not read is not read (see
  dk.cst.corpus-probe.url/fields): the field of another mode, or tokens
  under a mode that reads none, ask nothing here. Simple is the default,
  so a URL naming no mode and carrying q is a plain word search."
  [{:keys [within] :as params}]
  (let [mode   (url/mode params)
        text   (get params (url/field mode))
        unit   (within-param (when (url/reads? mode :within) within))
        tokens (fn [tokens]
                 (when (seq tokens) {:tokens (vec tokens) :within unit}))
        words  (words text)]
    (case mode
      "cqp"      (when-not (str/blank? text) {:cqp text})
      "extended" (tokens (token-params params))
      "list"     (tokens (when (seq words)
                           [(token (map-indexed
                                    (fn [i word]
                                      (cond-> (condition params word)
                                        (pos? i) (assoc :join "or")))
                                    (distinct words)))]))
      (tokens (map #(token [(condition params %)]) words)))))

(defn ->cqp
  "The CQP of `query` (see `of`): the text as typed, or the tokens
  compiled (see `extended->cqp`); nil for no query."
  [{:keys [cqp tokens]}]
  (or cqp (extended->cqp tokens)))

(defn within
  "The unit of text `query` (see `of`) is kept within (see
  dk.cst.corpus-probe.search/units): its unit, for two or more tokens,
  which should not be matched across a boundary, or for a token that
  opens or closes a sentence.

  Nil for one token, which cannot straddle a boundary and could only be
  refused where it stands outside every sentence, so for a list too; and
  nil for CQP, which says so itself."
  [{:keys [tokens within]}]
  (when (or (next tokens) (some #(or (:start? %) (:end? %)) tokens))
    within))

(def unit-names
  "What CQP calls each unit of text (see dk.cst.corpus-probe.url/units)
  in a `within` clause, by CWB's usual names for the attributes; a
  corpus naming them otherwise renames them at run time (see
  dk.cst.corpus-probe.commands/sentence-tags)."
  {:sentence "s" :paragraph "p" :text "text"})

(defn condition->params
  "The fields of `condition`, the `c`th of token `n`, as the extended
  form submits them and its URL carries them (see
  dk.cst.corpus-probe.url/token-key), nothing at its default."
  [n c {:keys [attr op value ci? join]}]
  (let [k (fn [field] (keyword (url/token-key n c field)))]
    (cond-> {}
      (not= (:attr url/token-defaults) (name attr))
      (assoc (k :attr) (name attr))

      (not= (:op url/token-defaults) op)
      (assoc (k :op) op)

      (not (str/blank? value))
      (assoc (k :v) value)

      ci?
      (assoc (k :ci) "on")

      (and (> c 1) (not= (:join url/token-defaults) join))
      (assoc (k :join) join))))

(defn token->params
  "The fields of `token`, the `n`th of an extended search, as its form
  submits them and its URL carries them: those of its conditions (see
  `condition->params`), its repeat and its sentence edges, nothing at
  its default."
  [n {:keys [conditions start? end?] lo :min hi :max}]
  (let [k (fn [field] (keyword (url/token-key n 1 field)))]
    (cond-> (into {}
                  (map-indexed (fn [i c]
                                 (condition->params n (inc i) c)))
                  conditions)
      (not= 1 lo) (assoc (k :min) (str lo))
      (not= 1 hi) (assoc (k :max) (str hi))
      start?      (assoc (k :start) "on")
      end?        (assoc (k :end) "on"))))

(defn word-params
  "The params of the words of `query` as a simple search or a list
  spells them, by `mode`: the values of its tokens in order, or of its
  one token's alternatives, in q, one line each for a list, with the
  first condition's attribute, operator and case flag as the options
  every word shares (see `condition`). For a query the form holds (see
  `project`)."
  [mode {:keys [tokens within]}]
  (let [[attr op ci?] (literal-shape (first (:conditions (first tokens))))
        list?         (= "list" mode)
        values        (if list?
                        (map :value (:conditions (first tokens)))
                        (map (comp :value first :conditions) tokens))]
    (cond-> {(url/field mode) (str/join (if list? "\n" " ") values)}
      (not= (url/default :in) (name attr)) (assoc :in (name attr))
      (not= "is" op)                       (assoc :match op)
      ci?                                  (assoc :ci "on")
      (and (not list?) (not= :sentence within))
      (assoc :within (name within)))))

(defn params
  "The search params of `mode` that carry `query`, as the form of that
  mode submits them and its URL cites them: the words or the lines in
  the mode's own field (see dk.cst.corpus-probe.url/field) with their
  options (see `word-params`), the tokens as their fields (see
  `token->params`) with the unit, or the CQP text; nothing at its
  default, and no mode, which the field says. For a query the form holds
  (see `project`), so that reading them back (see `of`) gives the query
  again."
  [mode query]
  (cond
    (nil? query)         {}
    (= "cqp" mode)       {:cqp (->cqp query)}
    (= "extended" mode)  (let [{:keys [tokens within]} query
                               fields (map-indexed (fn [i t]
                                                     (token->params (inc i) t))
                                                   tokens)]
                           (cond-> (into {} fields)
                             (not= :sentence within)
                             (assoc :within (name within))))
    :else                (word-params mode query)))

(def max-alternatives
  "The most words a list is carried into the extended form as, one
  condition each; a longer list is not (see `loss`).
  TODO: a guess, to be set by the lists readers paste."
  50)

(defn list-token?
  "True when `tokens` are what a list is: one token, once, at no edge,
  whose conditions are alternatives of one literal value (see
  `alternatives?`), or one such condition."
  [tokens]
  (let [{:keys [conditions start? end?] lo :min hi :max} (first tokens)]
    (boolean (and (= 1 (count tokens))
                  (not start?) (not end?) (= [1 1] [lo hi])
                  (or (= 1 (count conditions))
                      (alternatives? conditions))
                  (contains? literal-ops (:op (first conditions)))))))

(defn word?
  "True when `condition` is one a search box can hold as a word: a
  literal operator (see `literal-ops`) and a value with no space in it."
  [{:keys [op value]}]
  (boolean (and (contains? literal-ops op)
                (not (str/blank? value))
                (not (re-find #"\s" value)))))

(defn token-loss
  "What the simple form or the list form cannot hold of `token`, the
  `i`th: its first condition when it is any word, `[:any-word i]`, when
  its operator is none a word has (see `literal-ops`) or the word is
  matched otherwise than `shape` (see `literal-shape`) says,
  `[:options i]`, or when its value is no word (see `word?`),
  `[:value i]`; every condition after the first, `[:condition i c]`; its
  repeat, `[:repeat i]`; its sentence edge, `[:edge i]`."
  [shape i {:keys [conditions start? end?] lo :min hi :max}]
  (let [[{:keys [op] :as first-condition} & more] conditions
        literal? (contains? literal-ops op)]
    (cond-> []
      (= "any" op)
      (conj [:any-word i])

      (or (and (not= "any" op) (not literal?))
          (and (word? first-condition)
               (not= shape (literal-shape first-condition))))
      (conj [:options i])

      (and literal? (not (word? first-condition)))
      (conj [:value i])

      (seq more)
      (into (map-indexed (fn [j _] [:condition i (+ 2 j)]) more))

      (not= [1 1] [lo hi])
      (conj [:repeat i])

      (or start? end?)
      (conj [:edge i]))))

(defn shape
  "The shape (see `literal-shape`) every word carried into a simple
  search or a list takes: that of the first token's first condition
  that is a word (see `word?`); the plain word otherwise."
  [tokens]
  (or (some #(when (word? %) (literal-shape %))
            (map (comp first :conditions) tokens))
      (literal-shape nil)))

(defn loss
  "What the form of `mode` cannot hold of `query` (see `of`), as items
  the interface words: nothing for no query, or for a query the form
  holds whole. The CQP form holds every query, since the tokens compile
  (see `project`); the extended form holds every token query but a list
  past `max-alternatives`, `[:list n]`, and no CQP, `[:cqp text]`, which
  the app never reads. The simple form and the list form read CQP as
  words, `[:reading]`; hold a list as its words in order, `[:any]`, and
  words in order as a list, `[:order]`; and of any other token, what
  `token-loss` says."
  [mode query]
  (let [{:keys [cqp tokens]} query]
    (cond
      (nil? query) []
      (= "cqp" mode) []
      (= "extended" mode)
      (cond
        cqp [[:cqp cqp]]
        :else (into [] (keep (fn [{:keys [conditions]}]
                               (when (> (count conditions) max-alternatives)
                                 [:list (count conditions)])))
                    tokens))
      cqp [[:reading]]
      (list-token? tokens)
      (if (and (= "simple" mode) (next (:conditions (first tokens))))
        [[:any]]
        [])
      :else
      (into (if (and (= "list" mode) (next tokens)) [[:order]] [])
            (mapcat (fn [i token] (token-loss (shape tokens) i token))
                    (range 1 (inc (count tokens)))
                    tokens)))))

(defn fits?
  "True when the form of `mode` holds `query` whole (see `loss`)."
  [mode query]
  (empty? (loss mode query)))

(defn carried
  "The words of `tokens` a simple search or a list carries (see `loss`):
  a list's alternatives, or the first condition of each token that is a
  word (see `word?`), in order."
  [tokens]
  (if (list-token? tokens)
    (map :value (:conditions (first tokens)))
    (keep (fn [{:keys [conditions]}]
            (let [c (first conditions)] (when (word? c) (:value c))))
          tokens)))

(defn project
  "`query` (see `of`) as the form of `mode` holds it, which is the query
  itself where the form holds it whole (see `loss`): the CQP form holds
  the tokens compiled, kept within their unit by name (see `unit-names`);
  the extended form holds no CQP and no list past `max-alternatives`,
  and starts blank, nil; the simple form and the list form hold CQP as
  its words, and of tokens the words they carry (see `carried`), in
  order or as alternatives, matched as the first of them is (see
  `shape`). Nil when nothing is carried.

  TODO: a unit named in the CQP is CWB's name for it, which a corpus
  naming it otherwise must rename when the query runs, as it renames
  the sentence tags."
  [mode {:keys [cqp tokens within] :as query}]
  (cond
    (nil? query) nil
    (= "cqp" mode)
    (if cqp
      query
      {:cqp (str (->cqp query)
                 (some->> (dk.cst.corpus-probe.query/within query)
                          unit-names
                          (str " within ")))})
    (= "extended" mode)
    (when (fits? mode query) query)
    cqp
    (of {(url/field mode) cqp})
    :else
    (let [[attr op ci?] (shape tokens)
          conditions    (map #(hash-map :attr attr :op op :value % :ci? ci?)
                             (carried tokens))]
      (when (seq conditions)
        (if (= "list" mode)
          {:tokens [(token (map-indexed
                            (fn [i c] (cond-> c (pos? i) (assoc :join "or")))
                            (distinct conditions)))]
           :within :sentence}
          {:tokens (mapv #(token [%]) conditions)
           :within within})))))

(defn form-rows
  "The rows of the extended form holding `query` (see `project`): its
  tokens as their fields (see `params`), read as the form shows them
  (see dk.cst.corpus-probe.url/form-tokens), then one blank token, which
  is how a reader without the client adds one, and which the client
  drops (see dk.cst.corpus-probe.ui/own-rows)."
  [query]
  (url/form-tokens (concat (url/token-rows (params "extended" query))
                           [{:conditions [{}]}])))

(def reading-items
  "The kinds of loss item (see `loss`) that change how a query is read
  rather than take a part of it away: a form holding one runs the query
  as it reads it, and says so."
  #{:order :any :reading})

(defn arrived
  "What the search `params` of a submitted form ask, once a change of its
  mode radio is allowed for: the `:form` shown, which is the ticked mode
  (see dk.cst.corpus-probe.url/mode), the mode the query came `:from`
  when that is another, which the name of the field it arrived in says
  (see dk.cst.corpus-probe.url/typed), the query `:held` by that form
  (see `project`), the `:query` that runs, what the form could not keep
  as `:loss` items (see `loss`) and, for a hand-written URL, the
  `:unread` keys it carried (see dk.cst.corpus-probe.url/unread).

  A form whose mode radio was changed submits the old mode's field, named
  for the old mode, under the new mode's radio. Between the modes that
  have a field the ticked mode reads the text, since text typed after
  ticking is in that mode, and the reading the text had before is what
  the reader is told of. Across a field and the tokens, the shape
  decides: a field under the extended mode is read as the mode it is
  named for and seeds the tokens; tokens under a mode with a field are
  projected into it.

  A form holds what it holds and runs it, unless part of the query was
  lost: then nothing runs, the form shows what it kept and the line says
  the rest, so that the reader is told before the loss. What arrived
  with a switch is the switch's own business, so only a URL that names
  no switch has unread params to report."
  [params]
  (let [form   (url/mode params)
        own    (of params)
        origin (url/typed params)
        from   (when (and origin (not= origin form)) origin)
        switch (cond
                 (or (nil? from) own)  nil
                 (= "extended" form)   :in
                 (= "extended" from)   :out
                 :else                 :reading)
        other  (when switch (of (assoc params :mode from)))
        held   (case switch
                 :reading   (of (assoc params
                                       :mode form
                                       (url/field form)
                                       (get params (url/field from))))
                 (:in :out) (project form other)
                 own)
        loss   (if other (loss form other) [])
        lost?  (some (complement (comp reading-items first)) loss)]
    {:form   form
     :from   from
     :held   held
     :query  (when-not lost? held)
     :loss   loss
     :unread (if switch #{} (url/unread params))}))
