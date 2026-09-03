All sources are read and verified. Composing the report now.

# CQP Query Language — Complete Feature Inventory for a Web UI

**Primary sources.** The canonical document is *The IMS Open Corpus Workbench (CWB): CQP Interface and Query Language Manual* (Evert & The CWB Development Team, 2005–2022, covering CWB 3.5) — PDF: https://cwb.sourceforge.io/files/CQP_Manual.pdf, HTML: https://cwb.sourceforge.io/files/CQP_Manual/, linked from https://cwb.sourceforge.io/documentation.php. This manual is the current edition of the former "CQP Query Language Tutorial"; the old tutorial URL https://cwb.sourceforge.io/files/CQP_Tutorial.pdf now serves the same manual. Grammar-level details below were verified directly against the CQP source (`parser.y`, `parser.l`, `parse_actions.c`, `groups.c`, `ascii-print.c`, `options.c/.h`) at https://svn.code.sf.net/p/cwb/code/cwb/trunk/cqp/. Every CQP command is terminated with `;` (Manual §2.1).

**Data model context** (Manual §1.2): tokens are rows numbered by *corpus position* (cpos, from 0); token-level annotation layers are *positional attributes* (p-attributes; `word` is the p-attribute holding the surface form); XML elements become *structural attributes* (s-attributes) — non-overlapping, non-recursive token regions, with recursion handled by auto-renaming embedded regions `<np1>`, `<np2>`, …; XML tag key–value pairs become annotated s-attributes named `element_key` (e.g. `np_h`, `chapter_title`); sentence alignment between parallel corpora is an *alignment attribute* (a-attribute) named after the lowercased target corpus (Manual §5.1).

---

## 1. Token expressions

Source: Manual §2.2, §2.5, §2.6, §A.1.

- **Attribute test**: `[pos = "JJ"]`, `[lemma = "go"]`. Square brackets required; value is a string in single or double quotes, interpreted as a **regular expression that must match the whole annotation string**. `"interesting"` alone is shorthand for `[word = "interesting"]`; the implicit attribute is configurable via `set DefaultNonbrackAttr lemma;` (§2.5).
- **Regex flavour**: PCRE in CWB 3.5 (POSIX 1003.2 in CWB 3.0) (§A.1). Character-level regex syntax: `.` matchall, `[...]`/`[^...]` character sets and ranges, `? * + {n} {n,m}` repetition, `(...)` grouping, `|` alternation, e.g. `"interest(s|(ed|ing)(ly)?)?"` (§2.2, §A.1). PCRE Unicode property escapes work, e.g. `".*\pP.*"` for tokens containing punctuation (§6.3).
- **Literal escaping**: special characters `. ? * + | ( ) [ ] { } ^ $` must be backslash-escaped to match literally: `"\?"` → *?*, `"\$\."` → *$.* (§2.2, §A.1). `^`/`$` anchors are useless in CQP (whole-string match) but must still be escaped (§A.1). Quotes inside values: pick the other quote character (`"'em"`, `'12"-screen'`), backslash-escape (`'\'em'`), or double the quote (`'''em'`, `"12""-screen"`) (§2.2). CWB 3.0-era LaTeX-style diacritic escapes (`"B\"ar"` → *Bär*) and `\xDF` hex escapes exist but are deprecated/Latin-1-only (§2.2).
- **Flags** appended after the regex (§2.2, §2.5):
  - `%c` — ignore case: `"interesting" %c;`, `[lemma = "pole" %c]`
  - `%d` — ignore diacritics; combinable as `%cd`: `"wahrung" %cd;`
  - `%l` — match **literally**, disabling regex interpretation: `[word = "?" %l]` (§2.5).
- **Negated match**: `!=` — annotation must *not* match: `[pos != "N.*"]` (§2.5). In XML tags only `=` and `!=` are supported (§4.3). Inequalities `< <= > >=` are allowed **only for integers** (cpos, `strlen()`, `int()` casts), not strings — enforced since v3.4.17 (§4.1).
- **Matchall**: `[]` matches any token (§2.5).
- **Boolean combinations** inside `[...]` (§2.6): `&` (and), `|` (or), `!` (not), `->` (implication), parentheses for grouping:
  - `[lemma="under.+" & pos="V.*"];`
  - `[(lemma="go") & !(word="went"%c | word="gone"%c)];`
  - attribute–attribute comparison as strings: `[lemma="under.+" & word!=lemma];`
- **Feature-set operators** (for `|`-delimited set-valued attributes, §6.6): `contains` (membership: `[alemma contains "Zeuge"]`), `not contains`, `matches` (every member must match: `[agr matches ".*:Pl:.*"]`); both take regexes and accept `%c`/`%d`; `ambiguity(att)` returns set cardinality (`[ambiguity(alemma) > 3]`).
- **Word-list reference**: `[lemma = $week]`, `[word = RE($pref) %cd]` — see §9 below.
- **Built-in functions** usable inside token constraints and global constraints only (not in `group`/`tabulate`) (§8.3): `f(att)` type frequency; `dist(a,b)`/`distance(a,b)` signed distance; `distabs(a,b)`; `int(str)` numeric cast; `lbound(att)`/`rbound(att)` true at first/last token of a region (`[(pos="VBG") & lbound(s)]`, §4.2); `lbound_of(att,a)`/`rbound_of(att,a)` cpos of region start/end (v3.4.13+; `"\d+" :: lbound_of(s, match) = lbound_of(chapter, match);`); `unify(fs1,fs2)`; `ambiguity(fs)`; `add/sub/mul`; `prefix`, `is_prefix`, `minus`; `ignore(a)`; `normalize(str,"cd")` (v3.4.11+); `strlen(str)` (v3.4.17+, e.g. `[word=".*ment" & strlen(word) >= 16]`).
- **The `this` label `_`**: refers to the current corpus position inside a pattern: `[pos="ADJ." & _ < 500]`, `[_ = 666]` (fast cpos lookup, v3.4.17+), `[(pos="NNS?") & (lemma = _.np_h)]` (access s-attribute annotation of containing region) (§4.1, §4.3). Position-only constraints may not appear query-initially (§4.1).

## 2. Sequence queries (token-level regular expressions)

Source: Manual §2.7, §2.8.

Each `[...]` pattern behaves like one "character" in a regex over tokens (a subset of POSIX syntax):

- **Concatenation**: `"in" "any|every" [pos = "NN"];`
- **Repetition**: `?` (0–1), `*` (0+), `+` (1+), `{n}`, `{n,m}`; also `{n,}` and `{,m}` (grammar/TAB table, §8.5): `[pos="JJ.*"]* [pos="N.*"]+`
- **Optionality / wildcard gaps** with matchall: `"right" []? "left";` `"no" "sooner" []* "than";` `"as" []{1,3} "as";` (§2.8)
- **Grouping** `( ... )` and **disjunction** `|`: `([pos="RB"]? [pos="JJ.*"])* `, order-independent search `"left" "to" "right" | "right" "to" "left";` (§2.7–2.8)
- Combined PP example (§2.7 Fig. 2): `[pos="IN"] [pos="DT"]? ([pos="RB"]? [pos="JJ.*"])* [pos="N.*"]+;`

## 3. Structural constraints

Source: Manual §4.2–§4.4, §8.1, §8.7; grammar `SearchSpace`/`Description` in parser.y.

- **XML-tag syntax**: `<s>` matches at a region start, `</s>` at a region end: `<s> [pos="VBG"];` (participle at sentence start); region queries `<np> []* ([pos="JJ.*"] []*){3,} </np>;`. With `StrictRegions` on (default), a start/end tag pair encloses exactly one region; off, tags match any boundaries independently (§4.2). Tags can be mixed/nested: `<s><np>[]*</np> []* <np>[]*</np></s>;`. The built-in macro `/region[np]` ≡ `<np> []* </np>` (§4.2).
- **Bare s-attribute name as token predicate**: inside `[...]`, `np` is true iff the token lies in an `<np>` region: `[(pos="NNS?") & !np];` (§4.2).
- **lbound/rbound**: `[(pos="VBG") & lbound(s)]`; plus `lbound_of`/`rbound_of` for positions (§4.2, §8.3).
- **`within` clause**: restricts each match to a single region: `[pos="NN"] []* [pos="NN"] within np;`; the idiomatic `... within s;` avoids crossing sentence boundaries; **only one** `within` clause per query (§4.4 note in §4.2). Grammar: `within [N] <s-attr>` or `within N` (a plain token-window), e.g. `within 3 s` (parser.y `SearchSpace`/`Description`). Without any `within`, gap scanning is bounded by the `HardBoundary` option (default 500 tokens; options.h).
- **Constraints on tag annotations**: `<np_h = "bank"> []* </np_h>;` (operator `=` default and omissible, `!=` allowed, `%c %d` flags allowed); multiple key constraints need stacked tags `<np_h="bank"><np_len="[1-6]"> []* </np_len></np_h>;` (§4.3). Values are also reachable via labels: `<np> a:[] []* </np> :: a.np_h = "bank";` or `/region[np,a] :: int(a.np_len) > 30;` — note `[np_h="bank"]` does **not** work (§4.3).
- **Recursion levels**: embedded regions are `<np1>`, `<np2>` …; any-level NP: `(<np>|<np1>|<np2>) []* (</np2>|</np1>|</np>);` — CQP pairs matching start/end tags (§4.3).
- **Region elements** (v3.4.31+, §8.7): `<<np>>` matches an entire region (no content constraints): `"dine" [pos="IN"] <<np>>;` ≡ `"dine" [pos="IN"] <np> []+ </np>;`. `<<NQRname>>` treats a named query result as **ad-hoc annotation** (may overlap/nest, unlike s-attributes): `<<NP>> "about" <<NP>>;`; repeatable: `<<NP>> ([pos="IN|TO"] <<NP>>){5};`. Labels/target markers attach after `<<` and before `>>`: `<<NP @0>> "after" <<@1 a: NP b:>> :: distabs(a,b) >= 2;`. Zero-width form `<<NP/>>` anchors a query at previous-result start positions without consuming tokens (used for anchored/pre-filtered queries): `Cand = MU(meet "the"%c [lemma="creature"] 1 5); <<Cand/>> "the"%c [pos="JJ.*"]{3,} [lemma="creature"];`.
- **Zero-width assertions** (§8.1): `[: ... :]` tests a Boolean constraint between tokens without consuming one (look-ahead: `[pos="NNS?"]{2,} [: pos != "NNS?" :];`); `_` inside refers to the *following* token; the matchall assertion `[::]` is a no-op used to place labels/targets at otherwise inaccessible points: `@[::] /region[np]`, `a:[::] ( …|…|… ) b:[::]`.

## 4. Anchors, labels, global constraints

Source: Manual §3.3, §3.7, §4.1, §4.5, §8.6.

- **match / matchend**: every match is a token range represented by these two anchors (§3.3); both usable as labels in constraints: `... :: distabs(match, matchend) >= 5;` (§4.1).
- **target `@`**: prepend to one pattern: `"in" @[pos="DT"] [lemma="case"];` — bold in KWIC. Only one token can be target; under repetition or multiple `@`, the earliest-cpos token wins per the manual §3.3 (the numbered-marker section §8.6 says the marker "encountered last during evaluation" wins for multiple plain `@`s — the manual states both; the §3.3 formulation applies to markers inside repetitions).
- **keyword `@1`** (v3.4.16+): second anchor, underlined in KWIC: `"in" @[pos="DT"] @1[pos="J.*"]? [lemma="case"];` (§3.3).
- **Numbered target markers `@0`–`@9`** (v3.4.16+, §8.6): up to 10 potential anchors; which two are live is set by `set AnchorNumberTarget n;` / `set AnchorNumberKeyword n;` (defaults 0 and 1; may not be equal). All markers accept an optional colon (`@0:`, `@:`) for macro-friendliness. Plain `@` always sets target regardless of `ant`.
- **Labels**: `adj:[pos="JJ.*"]` binds the token's cpos; annotations via `adj.word`, `adj.lemma`; labels are query-internal only (§4.1). Uses: value comparison `a:[] "and" b:[] :: a.word = b.word;`; long-distance dependencies `a:[pos="PP"] []{0,5} b:[pos="VB.*"] :: b.pos = "VBZ" -> a.lemma = "he|she|it";`; within patterns `a:[] [pos = a.pos]{3};` (four identical tags). A label can't be used inside its own pattern (use `_`). Labels on optional patterns may be undefined — guard with `:: a -> a.pos="JJ"`. Label scope pitfalls in macros are handled with the implicit unique `$$` prefix and the `/undef[a,b,c]` built-in macro (§8.2).
- **Global constraint `::`**: one Boolean expression appended after the pattern sequence, evaluated over labels/anchors/functions: `a:[pos="DT"] ... :: distabs(a,b) >= 5;`, `adj:[pos="ADJ."] :: adj < 500;` (§4.1). MU queries do not support it (§8.4).
- **Match selectors** (v3.4.32+, §4.5): `show label1[offset] .. label2[offset]` after the query (positioned after `::`-constraint and `within`, before alignment constraints, `cut`, `expand`) returns a sub-span as the final match: `"it"%c [lemma="be"] [pos="DT"] noun:[pos="NNS?"] "that"%c @[pos="V.*"] show noun .. noun;`. `match`/`matchend` may stand for "leave that end unchanged": `show match[1]..matchend[-1];`. Undefined labels or inverted ranges discard the match.
- **Anchor field names** (parser.l): the FIELD tokens are exactly `match`, `matchend`, `target` (alias `collocate`), `keyword` — used in `sort on`, `group`, `tabulate`, `subset`, `delete`, `size`, `undump with`.

## 5. Named query results, subqueries, set operations, expand

Source: Manual §3.1–§3.2, §3.5, §6.3; parser.y `CorpusSetExpr`.

- **Naming**: `Go = [lemma="go"] "and" [];` (capitalised names by convention; result not auto-displayed). Fully qualified as `DICKENS:Go`; unqualified names are resolved against the active corpus. Implicit NQR `Last` holds the most recent query result and is what `cat`, `sort`, `count` operate on by default (§3.1). `show named;` lists NQRs with `md*` flags (in **m**emory / saved to **d**isk / modified `*`); `size Go;` count; `size Go target;` counts matches with target set (parser.y `SizeCmd` + §3.3); `cat Go;`, `cat Go 5 9;` (6th–10th match); `save Go;` / `discard Go;` with `set DataDirectory "...";` for persistence; copying: `B = A;`, `C = Last;` (§3.1–3.2, §3.5).
- **Subqueries** (§6.3): activating an NQR (`First;` after `First = [lemma="interest"] expand to s;`) makes its matches a virtual s-attribute `match`; the prompt becomes `DICKENS:First[624]>` and every subsequent query carries an implicit `within match`. Re-activating the system corpus (`DICKENS;`) exits. Activated matches must be non-overlapping. Tags `<match>`, `</match>`, `<target>`, `<keyword>` (always length 1 for target/keyword) are usable: `<match> [pos="W.*"];`, `</target> []* </match>;`. Appending the **keep operator `!`** turns a subquery into a filter returning the full activated ranges containing a hit (≡ implicit `expand to match`): `B = [pos="JJ.*"] !;`. Typical uses: metadata subcorpora (`HardTimes = <novel_title = "Hard Times"> [] expand to novel;`), iterative refinement, and pre-filtering for speed.
- **Set operations** (§3.5; grammar): `A = union B C;` (alias `join`), `A = intersection B C;` (aliases `inter`, `intersect`), `A = difference B C;` (alias `diff`). Also `subset` by anchor constraint: `PP1 = subset PP where match: "in";`, `PP2 = subset PP1 where matchend: [lemma="time"];` — the field label takes `match:`/`matchend:`/`target:`/`keyword:` syntax (parser.l FIELDLABEL). `cat` accepts a set expression directly (parser.y).
- **expand** (§4.2): grow matches to containing regions — as a command `B = A expand to np;`, one-sided `C = B expand left to s;` (or `right`); or as a query modifier after all other modifiers: `[pos="JJ.*"] ([]* [pos="JJ.*"]){2} within np cut 20 expand to np;`.
- **Alignment constraints** (parallel corpora, §5.2): `:TARGET-CORPUS query` after the main query (after `within`, before `cut`): `"nuclear"%c "power"%c :EUROPARL-DE [lemma="Kernkraft"];`; negation `:EUROPARL-DE ! "(Kern|Atom).*"`; chainable (all must hold). NQR translation to the aligned corpus: `Time = from Zeit to EUROPARL-EN;` (v3.4.9+, §5.3).

## 6. Post-processing commands

Source: Manual §2.9, §3.3, §3.5–3.7; parser.y/parse_actions.c for `delete`.

- **sort** — full form (grammar comment in parser.y; §2.9, §3.3):
  `sort [NQR] by <attribute> [%c|%d|%cd] [on <anchor>[<offset>] [.. <anchor>[<offset>]]] [asc|ascending|desc|descending] [reverse];`
  - `sort by word;` (alphabetical, re-displays); `sort by word %cd;`
  - bare `sort;` restores natural corpus-position order
  - `descending`, and `reverse` = sort by suffix (character-reversed key); combinable: `sort by word descending reverse;`
  - context sorting via anchors: right context `sort by word %cd on matchend[1] .. matchend[42];`; left context word-wise `on match[-1] .. match[-42];` (end < start ⇒ token comparison right-to-left); left context character-wise `on match[-42] .. match[-1] reverse;`
  - **randomize**: `sort A randomize;` (random order), `sort A randomize 42;` (stable, seeded — enables reproducible and *incremental* samples: sort with fixed seed, then `cut Sample1 0 99; cut Sample2 100 199;`) (§3.6)
  - `set ExternalSort on;` delegates to the system `sort` (allows locale collation via `LC_COLLATE`; don't combine with `%c/%d`) (§3.3).
- **count** (§2.9, §3.1): frequency of matching sequences; same key syntax as sort:
  `count [NQR] by <attribute> [%c|%d|%cd] [on <anchor-range>] [cut <n>] [> "file"];`
  e.g. `count by lemma cut 10;`, `count by lemma on match[1] .. matchend[-1];`, `count Go by lemma cut 5 > "go.cnt";`. Side effect: it **sorts the NQR** by the counted key so instances of each type are contiguous; output lines reference the KWIC line range:
  ```
  13 go and see [#128-#140]
  10 go and sit [#144-#153]
  9 go and do [#29-#37]
  7 go and fetch [#42-#48]
  ```
  then `cat Go 128 140;` shows the *go and see* block (§3.1). `descending`/`reverse` keywords go before `cut` (§2.9).
- **reduce** (§3.6): destructive random thinning — `reduce A to 10%;` or `reduce A to 100;`; seed via the standalone command `randomize 42;` for reproducibility. Grammar also supports `reduce A to maximal matches;` (drops matches contained in longer ones; parser.y `RMaximalMatches`) [only the grammar documents this variant].
- **cut** (§3.5): `cut A 50;` (first 50 = `cut A 0 49;`), `cut A 50 99;` (range); restores corpus order. As a **query modifier**: `"time" cut 50;` stops query evaluation early (memory/latency control for web interfaces; guaranteed first-n; not applicable together with alignment constraints) (§3.5).
- **delete** (verified in parser.y/parse_actions.c; not described in the manual):
  - `delete A with target;` — deletes matches whose target (or `keyword`) anchor **is set**;
  - `delete A without target;` — deletes matches whose anchor is unset;
  - `delete A <n> [<m>];` — deletes a line range (0-based, current sort order);
  - `with[out] match/matchend` is rejected as meaningless. Source: https://svn.code.sf.net/p/cwb/code/cwb/trunk/cqp/parser.y (rule `Delete`), `parse_actions.c` `do_delete_lines`.
- **shuffle**: does **not** exist as a command (absent from the lexer/parser and the reserved-word list, Manual §A.4); its role is filled by `sort [A] randomize [seed];`.
- **set target/keyword/match/matchend post-hoc** (§3.7, Fig. 3):
  `set <NQR> (keyword|target) (leftmost|rightmost|nearest|farthest) [pattern] within [left|right] <n> (words|s|...) from (match|matchend|keyword|target) [inclusive];`
  e.g. `set A keyword nearest [pos="NNS?"] within right 5 words from match;`. Anchor copying with optional offset: `set A target match;`, `set A target target[-1];`, `set NPobj match target;`; deletion: `set A target NULL;`; forced update with trailing `!` (`set Elephants match keyword !;` — drops matches when source anchor undefined). Conditional-update semantics from v3.4.31.
- **randomize** command: `randomize 42;` seeds the RNG for subsequent `reduce` (§3.6; parser.y `RandomizeCmd`).

## 7. Frequency-distribution commands and output shapes

Source: Manual §3.4, §7.3; groups.c/ascii-print.c.

- **`count … by`**: see above. Terminal shape (pretty): `freq SPACE string [#first-#last]`. With `set PrettyPrint off;` (backend mode): `frequency TAB first-line TAB string` per line (§7.3).
- **`group` unary**: `group <NQR> <anchor>[offset] <attribute> [within <s-attr>] [cut <n>] [> "file"];`
  - `group Go matchend pos;`
  - `group NP target lemma cut 50;`
  - offset: `group NP match[-1] lemma cut 100;`
  - s-attribute values: `group A match novel_title;` (§4.4)
  - `within <s-attr>` (v3.4.26+) switches to **document frequencies** over regions: `group Go matchend lemma within novel cut 3;`; items outside regions and undefined anchors are silently dropped (so `(none)` rows disappear).
- **`group` pairwise**: `group <NQR> <anchor> <att> by <anchor> <att> [within ...] [cut n] [> file];`
  - `group NP matchend word by target lemma;`
  - `group Go matchend lemma by matchend pos;`
  - Results are sorted by pair frequency (not grouped), and **the two items print in the opposite order to the command** (§3.4).
  - `foreach` is an exact synonym of `by` (parser.y `GroupBy`).
- **`group … group by`** (v3.4.9+): nested counts, `group Go matchend lemma group by matchend pos;` (`group foreach` also accepted); `cut` applies to individual pairs (§3.4).
- **Exact terminal shapes** (ascii-print.c `ascii_print_group`):
  - pretty-printed: `fprintf(dest, "%-28s  %-28s\t%6d\n", …)` — i.e. a left-justified 28-char source-value column (blank on repeated rows of the same group), a 28-char target-value column, TAB, right-justified 6-char frequency; a separator bar precedes each nested group. Undefined/unused values print as `(none)` / `(all)`.
  - `PrettyPrint off` (backend): `[source-value TAB] target-value TAB frequency`, with `(none)`/`(all)` replaced by empty strings — matching the manual's documented shape `[attribute value TAB] attribute value TAB frequency` (§7.3).
- Anchors traversed out of corpus order (e.g. after `set … keyword nearest …` with a large window) make `within`-grouping abort with an error (§3.4).

## 8. tabulate

Source: Manual §7.3.

- Syntax: `tabulate <NQR> [<first> <last>] <colspec>, <colspec>, ... [> "file" | > "| pipe"];`
  where each column spec is `anchor[offset]` or a range `anchor[offset] .. anchor[offset]`, optionally followed by an attribute name and `%c`/`%d` flags:
  - `tabulate A match, matchend, target, keyword;` — prints corpus positions; identical to `dump A;` output when all anchors are defined
  - `tabulate A match novel_title, match book_num, match chapter_title;` — p-attributes and annotated s-attributes both allowed
  - `tabulate A match[-5]..match[-1] lemma, matchend[1]..matchend[5] lemma;` — collocate context
  - `tabulate A 100 119 <colspecs>;` — restricted match range
  - `tabulate A match .. matchend word %c > "| sort | uniq -c | sort -nr";`
- **Output format**: one TAB-separated row per match; values of tokens *within* a range column are separated by single blanks (not TABs); undefined values print as empty string (or `-1` for cpos columns; behaviour fixed in v3.4.10); an anchor range whose end precedes its start is treated as empty (unlike sort/count); ordering of range endpoints must be non-decreasing or behaviour is unspecified.

## 9. Word lists, macros, matching strategies

**Word lists** (Manual §6.2):
- `define $week = "Monday Tuesday Wednesday Thursday Friday";` then `[lemma = $week];` (not allowed inside XML tags)
- add/remove: `define $week += "Saturday Sunday";`, `-=`
- from file (one word per line): `define $week < "/home/weekdays.txt";` (v3.4.11+: auto-decompression of `.gz`/`.bz2`, and shell pipes: `define $week < "| perl -pe 's/\s+/\n/g' words.txt";`)
- inspect: `show $week;`, `show var;`
- list values are matched **literally**; `%c/%d` are not allowed on `$list`. For regex lists use `RE()`: `define $pref="under.+ over.+"; [(lemma=RE($pref)) & (pos="VBG")];`, flags allowed: `[word = RE($pref) %cd];`
- lists can build type hierarchies (`define $noun = $common_noun; define $noun += $proper_noun;`).

**Macros** (Manual §6.4–§6.5):
- Definition file syntax (start marker and terminating `;` on their own lines; `#` comments; wrap bodies in parentheses for safe scoping):
  ```
  MACRO pp($0=Prep $1=N_Adj)
  ( [(pos="IN") & (word="$0")] [pos="DT"] [pos="JJ.*"]{$1} [pos="NNS?"] )
  ;
  ```
- Load: `define macro < "macros.txt";`; interactive one-liner: `define macro np(0) '[pos="DT"] [pos="JJ.*"]+ [pos="NNS?"]';`
- Invoke: `/np[]`, `/pp["under", 2]` — quotes required for string arguments (numbers/simple identifiers may be bare); interpolation is **plain string substitution** of `$0`…`$9` (≤10 args); overloading by arity; named-argument prototypes `MACRO pp($0=Prep $1=N_Adj)` (names are documentation only); `IMPORT other_macros.txt` lines in definition files; nested (non-recursive) calls form a CFG; unique per-expansion `$$` prefix and `/undef[...]` for label hygiene (§8.2).
- Built-ins: `/region[att]`, `/region[att,label]`, `/codist[...]`, `/unify[att, l1, l2, ...]`; view any definition with `show macro np(0);`; list with `show macro;` / `show macro region;`.

**Matching strategies** (Manual §6.1):
- `set MatchingStrategy (traditional | shortest | standard | longest);`
- `shortest`: quantifiers match minimally; optional edges never included. `longest`: maximal. `standard` (default): early-match — optional elements at the *start* included, at the *end* excluded. `traditional`: keeps all nested matches from multiple passes (overlapping results; useful for co-occurrence counts).
- Reproduced example (Manual Fig. 4) for `DET? ADJ* NN (PREP DET? ADJ* NN)*` on *the old book on the table in the room*:
  ```
  shortest:    book | table | room                       (3 matches)
  longest:     the old book on the table in the room     (1 match)
  standard:    the old book | the table | the room       (3 matches)
  traditional: the old book | old book | book | the table | table | the room | room  (7 overlapping)
  ```
- Duplicate matches from different passes are removed under every strategy; overlapping non-nested matches are never discarded.
- **Embedded modifiers** (v3.4.12+): `(?shortest)`, `(?standard)`, `(?longest)`, `(?traditional)` at query start set the strategy for that query only — designed for web UIs: `(?longest) [pos="NP.*"]+;`
- There is **no** "traceback" strategy; the fourth mode is `traditional` (Manual §6.1, options.c). Strategies apply only to standard queries, not MU/TAB (§6.1, §8.4–8.5).

## 10. set/show options affecting query semantics (vs. display)

Source: Manual §2.4, §A.5; options.c.

Semantics-affecting:
- `set MatchingStrategy …;` (`ms`) — see above.
- `set StrictRegions (on|off);` (`sr`, default on) — whether paired XML tags must enclose a single region (§4.2, §A.5.1).
- `set DefaultNonbrackAttr <p-att>;` (`da`) — which attribute bare `"regex"` patterns test (§2.5).
- `set AnchorNumberTarget n;` / `set AnchorNumberKeyword n;` (`ant`/`ank`) — which `@0..@9` markers feed target/keyword (§8.6).
- `set HardBoundary <n>;` (`hb`, default 500) — token limit on gap scanning for queries without `within` (options.c/h; not listed in Manual §A.5) [semantics verified from source].
- `set HardCut <n>;` (`hc`, default 0 = off) — implicit cut on every query (options.c) [unverified beyond source definition].
- `set Optimize (on|off);` (`o`) — experimental optimisations (§2.4).
- `set Registry "..."`, `set DataDirectory "..."` — corpus visibility / NQR persistence (§2.1, §3.2).
- `set AutoSubquery (on|off);` (`sub`) — auto-activate an NQR (entering subquery mode) on creation (§A.5.1).
- `set ExternalSort (on|off);` + `ExternalSortCommand` — external sorting, changes collation behaviour (§3.3).
- `set QueryLock <n>;` / `unlock <n>;` — locks the session to queries only (blocks `cat`, `sort`, `group`, …) for untrusted web input (§7.1).
- `randomize <seed>;` — RNG state for `reduce`/`sort randomize` (§3.6).

Display/back-end only (a UI still needs them): `Context/LeftContext/RightContext` (`set Context 5 words;`, `set Context s;`, `set Context 3 s;`), `PrintMode (ascii|sgml|html|latex)`, `PrintOptions (hdr|num|tbl|bdr|wrap|…)`, `PrintStructures "novel_title, chapter_num"`, `show +pos +lemma;`/`show -cpos;`, `ShowTagAttributes`, `LD`/`RD` KWIC delimiters, `AttributeSeparator` (v3.4.18+) and `TokenSeparator` (v3.4.24+) for parseable KWIC, `StructureDelimiter` (v3.4.25+), `AutoShow`, `Paging`/`Pager`, `Highlighting`, `ShowTargets`, `Colour`, `PrettyPrint`, `ProgressBar`, `Timing`, `AutoSave`/`SaveOnExit`, `HistoryFile`/`WriteHistory` (§2.3–2.4, §7.1, §A.5).

## 11. Expert query types and interchange (a web UI's back-end concerns)

- **MU (meet-union) queries** (§8.4; official since v3.4.12): LISP-like prefix notation, `MU` keyword; positional windows or s-attribute scope:
  - `MU(meet [pos="NN.*"] [lemma="lovely"] -2 2);` — first pattern's positions filtered by co-occurrence of second within the window; asymmetric.
  - `MU(meet "tea"%c "cakes"%c s);` — co-occurrence within a region.
  - `MU(union "tea"%c "coffee"%c);` ≡ `"tea"%c | "coffee"%c;`
  - nesting: `MU(meet (meet "in" "due" 1 1) "course" 2 2);`
  - negation (v3.4.30+): `MU(meet "ground" not [pos="DT"] -3 -1);`
  - combinable with `!` (subquery filter), `cut`, `expand to s`; **not** with labels, `@`, assertions, `::`, alignment constraints, or `within` clauses. Match = position of the leftmost-specified pattern only.
- **TAB queries** (§8.5; stable since v3.4.12): `TAB` keyword, fixed token patterns with gap operators between them (gaps are implicit matchalls; `[]` itself is illegal):
  - `TAB "in" "due" "course";`
  - `TAB "cats" {0,2} "dogs";` ≡ `"cats" []{0,2} "dogs";`
  - `TAB "girl" {2} "girl" within s;`
  - gap operators: `? * + {n} {n,k} {n,} {,k}`. Greedy left-to-right fixing of each item (no backtracking; documented failure example with `TAB "time" * "for" ? "coffee" within s;`); always early-match; no alternation, quantified patterns, targets, or labels; exhaustive only when all gaps are fixed-size, or all gaps are `*` under a `within` clause.
- **dump/undump** (§3.3, §7.2): `dump A;`, `dump A 9 14;`, `dump A > "dump.tbl";` (auto-`.gz/.bz2` v3.4.11+), pipes `dump A > "| gawk '{print $2 - $1 + 1}' | sort -nr | uniq -c | less";`. Output: one row per match, four TAB-separated columns `match matchend target keyword`, `-1` = unset:
  ```
  1019887 1019888 -1 -1
  1924977 1924979 1924978 -1
  1986623 1986624 -1 -1
  ```
  `undump B < "mydump.tbl";` reads two-column tables (optionally `undump B with target [keyword] < ...;` for 3/4 columns); stdin variant needs a leading match-count header line; `undump B with target ascending < ...;` validates sorted non-overlapping input (recommended when building subcorpora).
- **Backend/child mode** (§7.1): `cqp -c` — version banner, no `.cqprc`, unbuffered output, `PARSE ERROR` on stderr, `.EOL.;` emits the marker line `-::-EOL-::-`, ProgressBar lines formatted `-::-PROGRESS-::- TAB pass TAB total TAB message`. With `set PrettyPrint off;`: `show corpora;` one name per line; `show named;` lines `flags TAB corpus:name TAB n_matches`; `show cd;` lines `p-Att|s-Att|a-Att TAB name TAB [-V] TAB [*]` (4-column form since v3.4.18); `show active;` prints the active corpus.
- **Reserved words** (§A.4): cannot name corpora/NQRs/labels — includes `by cat cut define delete desc diff dump expand group join keyword match matchend meet MU reduce save set show size sort subset TAB tabulate target to union undump where with within` etc.; since v3.4.13 backtick-quoting lifts the restriction: `` `MU` = [lemma = "meeting|union"]; ``.
- **KWIC sample** (Manual §2.3) for a UI's concordance reference:
  ```
  15921: ry moment an <interesting> case of spo
  17747: appeared to <interest> the Spirit
  20189: ge , with an <interest> he had neve
  ```
- **Scripts** (§2.10): `cqp -f script.txt`; `source "script.txt";` in-session (v3.4.22+); `#`-comments; no script arguments (use macros).
- **Easter-egg-adjacent**: built-in Boyer–Moore regex optimiser for prefix/suffix/infix patterns (`"under.+"`, `".+ment"`); `set CLDebug on;` reveals whether a regex is optimised (§8.8).

**Not part of CQP**: `shuffle` (absent); a "traceback" matching strategy (the fourth strategy is `traditional`); `%l` on word lists; multiple `within` clauses per query; string inequality comparisons.

**Sources:** [CQP Interface and Query Language Manual (PDF)](https://cwb.sourceforge.io/files/CQP_Manual.pdf) · [HTML edition](https://cwb.sourceforge.io/files/CQP_Manual/) · [CWB documentation index](https://cwb.sourceforge.io/documentation.php) · CWB source: [parser.y](https://svn.code.sf.net/p/cwb/code/cwb/trunk/cqp/parser.y), [parser.l](https://svn.code.sf.net/p/cwb/code/cwb/trunk/cqp/parser.l), [parse_actions.c](https://svn.code.sf.net/p/cwb/code/cwb/trunk/cqp/parse_actions.c), [groups.c](https://svn.code.sf.net/p/cwb/code/cwb/trunk/cqp/groups.c), [ascii-print.c](https://svn.code.sf.net/p/cwb/code/cwb/trunk/cqp/ascii-print.c), [options.c](https://svn.code.sf.net/p/cwb/code/cwb/trunk/cqp/options.c), [options.h](https://svn.code.sf.net/p/cwb/code/cwb/trunk/cqp/options.h)