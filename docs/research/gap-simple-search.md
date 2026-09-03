# Simple-search → CQP compilation: Korp and CEQL (spec for reimplementation)

Both Korp and CQPweb/BNCweb implement "simple search" purely as a **compiler to CQP**: user sugar is translated into a CQP query string, escaped so user text lands inside CQP double-quoted regex literals; a single query engine (CQP) executes everything. Verified against korp-frontend @ `ffcd1c2` (2026-05-12, branch `dev`), korp-backend (branch `dev`), CWB::CEQL from the CWB v3.5.0 CPAN distribution, and CQPweb trunk's PHP port.

---

## 1. Korp (korp-frontend)

Logic lives in three small files:
- Query building: [`app/scripts/search/simple-search.ts`](https://github.com/spraakbanken/korp-frontend/blob/dev/app/scripts/search/simple-search.ts) (`buildSimpleWordCqp`, `buildSimpleLemgramCqp`)
- UI/controller: [`app/scripts/components/search/simple-search.ts`](https://github.com/spraakbanken/korp-frontend/blob/dev/app/scripts/components/search/simple-search.ts)
- Serialization: `stringify` in [`app/scripts/cqp_parser/cqp.ts`](https://github.com/spraakbanken/korp-frontend/blob/dev/app/scripts/cqp_parser/cqp.ts); escaping: `regescape` in [`app/scripts/util.ts`](https://github.com/spraakbanken/korp-frontend/blob/dev/app/scripts/util.ts)

### 1.1 Word search

Algorithm (`buildSimpleWordCqp(input, prefix, suffix, ignoreCase)`):
1. Trim the input, split on whitespace `/\s+/` — **each whitespace-separated word becomes one CQP token**.
2. Per word: `value = regescape(word)`; if *prefix* checkbox ("word is a beginning"): `value += ".*"`; if *suffix*: `value = ".*" + value`. The UI's third checkbox *midfix* ("contains") is just prefix∧suffix (the controller keeps `midfix = prefix && suffix` in sync).
3. Emit condition `word = "value"`, with flag `%c` appended when case-insensitive.

Emitted CQP (one bracket per input word, space-joined):

| Options | Input `gå` | Emitted |
|---|---|---|
| none | | `[word = "gå"]` |
| prefix | | `[word = "gå.*"]` |
| suffix | | `[word = ".*gå"]` |
| midfix (both) | | `[word = ".*gå.*"]` |
| case-insensitive | | `[word = "gå" %c]` |
| phrase `hej då`, prefix+ci | | `[word = "hej.*" %c] [word = "då.*" %c]` |

Note the exact serialization in `stringify`: `[` + `type op "val"` (+ ` %<flags>`) + `]`; multiple flags would concatenate (`%cd`); tokens joined with a single space.

### 1.2 Escaping (`regescape`)

```js
s.replace(/[.|?|+|*||'|()^$\\]/g, "\\$&").replace(/"/g, '""')
```
- Backslash-escapes exactly this set: `. | ? + * ' ( ) ^ $ \` (the regex's inner `|` duplicates are just redundant class members).
- Double quote `"` is escaped by **doubling** (`"` → `""`), the CQP ≥3.0 convention for quotes inside a same-quoted string.
- **Not escaped: `[ ] { }`** — user input containing brackets/braces reaches the PCRE regex raw (a latent Korp gap; a reimplementation should escape them, see §3). The inverse `unregescape` simply strips backslashes (`\\`→`\`, `\`→``).
- The lemgram value chosen from autocomplete is also passed through `regescape` (so `ge..vb.1` → `ge\.\.vb\.1`) before being embedded.

### 1.3 Lemgram search

Triggered when the user picks a lemgram from the autocomplete dropdown instead of a raw string ([user manual](https://github.com/spraakbanken/korp-frontend/blob/dev/doc/user_manual_eng.md): word vs. lemgram search; lemgram search is always case-insensitive — no `%c` is ever emitted for it). `buildSimpleLemgramCqp(input, prefix, suffix)` emits **one** token whose conditions are OR-ed inside the bracket:

- base: `lex contains "ge\.\.vb\.1"`
- prefix checkbox adds: `complemgram contains "ge\.\.vb\.1\+.*"`
- suffix checkbox adds: `complemgram contains ".*\+ge\.\.vb\.1:.*"` (complemgram values are `part1+part2+…:probability`)

E.g. with both: `[lex contains "ge\.\.vb\.1" | complemgram contains "ge\.\.vb\.1\+.*" | complemgram contains ".*\+ge\.\.vb\.1:.*"]`. `contains` is native CQP feature-set matching (attribute values like `|a|b|`); Korp's operator expansion (`operatorMap` in `cqp.ts`) passes `contains` through unchanged. This branch is only relevant if your corpus has SALDO-style `lex`/`complemgram` feature-set attributes; for a plain `lemma` attribute, `[lemma = "..."]` (or `contains` if `lemma` is a feature set) is the analogue.

### 1.4 "In free order" and `within` — not in the CQP string

- The *in free order* checkbox does not change the CQP. The frontend sends `in_order=false` as an API parameter; it is enabled only when the query has ≥2 tokens and no wildcard/repetition/structure (`supportsInOrder` in `cqp.ts`).
- korp-backend (`korp/utils.py`, `query_optimize`) then rewrites the token sequence into a CQP **MU query**: `MU (meet (meet [tok1] [tok2] <within>) [tok3] <within>) expand to <within>` for free order (ordered multi-token queries get `1 1` distances instead, as an optimization); wildcards are forbidden in free-order queries ([korp-backend](https://github.com/spraakbanken/korp-backend/blob/dev/korp/utils.py)).
- `within`: the frontend sends `default_within` (first key of the `default_within` config, typically `sentence`); the backend appends ` within sentence` to every query via `make_cqp` (`cqp + " within %s"`). So the executed query is e.g. `[word = "hej"] [word = "då"] within sentence`.

---

## 2. CEQL (CQPweb/BNCweb "Simple Query")

Canonical implementation: `CWB::CEQL` in the CWB CPAN distribution — [POD](https://metacpan.org/pod/CWB::CEQL), [raw source](https://fastapi.metacpan.org/v1/source/SCHTEPF/CWB-v3.5.0/lib/CWB/CEQL.pm) (note: it is in the `CWB-v3.5.0` tarball, not a separate `CWB-CEQL` dist). CQPweb's PHP port [`ceqlparser.php`](https://svn.code.sf.net/p/cwb/code/gui/cqpweb/trunk/lib/ceqlparser.php) is a line-for-line port (identical wildcard table and `literal_string`), driven from [`query-lib.php`](https://svn.code.sf.net/p/cwb/code/gui/cqpweb/trunk/lib/query-lib.php) with a subclass [`perl/cqpwebCEQL.pm`](https://svn.code.sf.net/p/cwb/code/gui/cqpweb/trunk/lib/perl/cqpwebCEQL.pm).

### 2.1 Parameters (defaults from `CWB::CEQL::new`)

`pos_attribute`=`pos`; `lemma_attribute`=`lemma`; `simple_pos` (hashref tag→CQP regex, e.g. `{"N"=>"NN.*"}`) and `simple_pos_attribute` = undef; `s_attributes`={`s`=>1}; **`default_ignore_case`=1** (case-insensitive by default!), **`default_ignore_diac`=0**; per-attribute overrides via `ignore_case`/`ignore_diac` hashes keyed by attribute *type* (`word_attribute`, `lemma_attribute`, `pos_attribute`, `simple_pos_attribute`, `s_attributes`; non-word/lemma types default to 0); `tab_optimisation`=0. The word attribute is hard-coded `word` in the core (`wordform_pattern` returns `"word".$test`).

### 2.2 Wildcard patterns → regex

A pattern is split into items at unescaped `? * + [ , ]` and escape sequences, then each item is mapped (`wildcard_item` / `literal_string`):

| CEQL | regex | CEQL | regex |
|---|---|---|---|
| `?` | `.` | `\a` / `\A` | `\pL` / `\pL+` |
| `*` | `.*` | `\l` / `\L` | `\p{Ll}` / `\p{Ll}+` |
| `+` | `.+` | `\u` / `\U` | `\p{Lu}` / `\p{Lu}+` |
| `[a,b]` | `(a\|b)` | `\d` / `\D` | `\pN` / `\pN+` |
| `[a,b,]` (empty alt) | `(a\|b)?` | `\w` / `\W` | `[\pL\pN'-]` / `[\pL\pN'-]+` |

- **Literal text**: backslashes are stripped (`\?` etc. un-escape), then regex metacharacters `. ? * + | ( ) { } [ ] ^ $` are backslash-escaped, then `"` → `""` (`literal_string`). A literal `\\` or trailing `\` in a pattern is an error. The CEQL metacharacter set users must `\`-escape is `? * + , : ! @ / ( ) [ ] { } _ - < >`.
- The whole pattern is wrapped in double quotes; leading `!` (negation) selects operator `!=` vs `=` (`negated_wildcard_pattern`); a lone `!` is a literal.

### 2.3 Token expressions → `[...]`

`token_expression` splits on the single unescaped `_`:

| CEQL | CQP (with default `default_ignore_case=1`) |
|---|---|
| `can` | `[word="can"%c]` |
| `+able` | `[word=".+able"%c]` |
| `{go}` | `[lemma="go"%c]` (also `go%` shorthand: trailing unescaped `%` = lemma) |
| `_MD` | `[pos="MD"]` (word part empty or `*`/`+` with a POS is dropped) |
| `can_!MD` | `[word="can"%c & pos!="MD"]` |
| `{walk}_{!V}` | `[lemma="walk"%c & pos!="V.*"]` (simple POS: table lookup, `!` → `!=`, value used verbatim as regex with `"`→`""`; `%` suffix also marks simple POS) |
| `{fiancee}:cd_N*:C` | `[lemma="fiancee"%cd & pos="N.*"]` |

Flags `:c :C :d :D` (any mix, e.g. `:Cd`) are stripped from the end of each constraint (`_parse_constraint_flags`) and combined with the per-attribute defaults into `%c`, `%d`, `%cd`, or nothing (`_apply_constraint_flags`). POS/simple-POS/s-attribute constraints default to *sensitive* (no flag) unless configured.

### 2.4 Phrase queries

`phrase_query` whitespace-splits after spacing out unescaped `( | )` (+quantifier) and XML tags; each item via `phrase_element`:
- Token expressions → `[...]` as above; whitespace = token sequence (concatenated with spaces).
- `+` → `[]` (arbitrary token), `*` → `[]?` (optional token), clusters like `++***` → `[]{2,5}` (`{#+, #+ + #*}`; `[]{n}` if no `*`).
- Groups `( A | B )` → `(A | B)`; quantifiers `? * + {N} {N,M} {N,}` appended directly to `)`; empty alternatives forbidden at phrase level.
- `@expr` / `@0:`–`@9:` target/keyword anchors are passed through as CQP `@`/`@N:` markers.
- XML tags: `<s>` → `<s>`, `</s>` → `</s>`; annotated start tags `<ne_type=org:c>` → `<ne_type="org"%c>` (pattern is a full negatable wildcard pattern). Tag names must be keys of `s_attributes`, else error.
- Optional leading matching-strategy modifier `(?longest)`/`(?shortest)`/`(?standard)`/`(?traditional)` is validated and passed through to CQP.
- `tab_optimisation`: a phrase of plain `[...]` tokens and gaps rewrites to a CQP `TAB` query (`TAB [tok1] {1} [tok2]`, gap quantifiers `+ * ?`/`{n,m}`); disallowed for leading/trailing/consecutive gaps or non-standard strategy.

Example: `\, is n't it \?` → `[word=","%c] [word="is"%c] [word="n't"%c] [word="it"%c] [word="?"%c]` [unverified exact output, derived from the rules above].

### 2.5 Proximity queries and `within`

If the input contains an unescaped `<<…<<`/`>>…>>` pair, it is a proximity query → CQP **MU/meet** notation (`proximity_query`, `distance_expression`): `A <<N>> B` → `MU(meet A B -N N)`; `A <<K,N<< B` → `(meet A B -N -K)`; `A >>K,N>> B` → `(meet A B K N)`; `A <<s>> B` → `(meet A B s)` (region must be in `s_attributes`); left-associative chaining; plain word sequences inside proximity queries expand as `a >>1,1>> b >>2,2>> c`.

**`within` handling:** core CEQL has *no* `within` clause — sentence/region restriction is expressed only through `<s>…</s>` tags and `<<s>>` structural distances, both gated by the `s_attributes` whitelist. CQPweb does not append `within` to the CEQL output either; scope restriction happens separately (subcorpora/restrictions, `scope-lib.php`). If your engine needs a boundary guarantee, append CQP's native trailing `within <s-attr>` yourself (as korp-backend does with `default_within`).

### 2.6 CQPweb configuration deltas

`query-lib.php` (`process_simple_query_new`) maps corpus config → CEQL params: `word` fixed; `primary_annotation`→`pos_attribute` (`_X` slot), `secondary_annotation`→`lemma_attribute` (`{x}`), `tertiary_annotation`+mapping table→simple POS (`_{X}`), `combo_annotation`→`{lemma/TAG}` combos (subclass emits `combo_attr op "(lemma_rx)_tag_rx"` or falls back to `(lemma_constraint & simple_pos_constraint)`); per-annotation case/accent sensitivity from corpus metadata fills `ignore_case`/`ignore_diac`; the UI's case-sensitive/insensitive mode toggles `default_ignore_case`. Parse failure returns null plus user-facing error backtrace (`ErrorMessage`).

---

## 3. Which to reimplement; safe escaping

**Korp's design is far simpler** for a minimal single-box search over `word`/`pos`/`lemma`/`msd`: no grammar at all — trim, whitespace-split, escape, optionally wrap with `.*`, emit `[word = "…"( %c)?]` per token, join with spaces, optionally append `within sentence`. Everything a "simple search" UI with case/prefix/suffix checkboxes needs is ~15 lines (`buildSimpleWordCqp` + `regescape`). Its lemgram branch is Språkbanken-specific and can be replaced by `[lemma = "…"]` or dropped. CEQL, by contrast, is a full recursive grammar (shift-reduce parser framework, wildcard sub-language, groups, proximity/MU, XML tags, per-attribute flag defaults) — worth it only if you want user-facing wildcards `? * +`, `{lemma}`, `_POS` syntax; even then, the useful minimal subset is: the wildcard table in §2.2, `literal_string` escaping, the `_`-split of §2.3, and `+`/`*` gap tokens.

**Escaping function.** Reference: `CWB::CQP::quotemeta` ([source](https://fastapi.metacpan.org/v1/source/SCHTEPF/CWB-v3.5.0/lib/CWB/CQP.pm)) escapes `( ) { } [ ] | . ? * + $ \` with backslashes and rewrites `^` → `[^]` (historical workaround for CQP's latex-style escapes like `\^o`; CWB 3.5 abolished those per the comment in CEQL.pm, so plain `\^` is fine on 3.5+); its companion `quote` wraps in `"…"`/`'…'`, escaping embedded quotes **by doubling**, and rejects strings ending in an odd number of backslashes. CEQL's `literal_string` escapes the same set plus `^` with a backslash and doubles `"`. Korp's `regescape` omits `[ ] { }` (and escapes `'`, harmlessly). The safe superset for "embed user literal inside a CQP double-quoted regex":

```
escape with backslash:  . ? * + | ( ) [ ] { } ^ $ \
then double the quote:  "  →  ""
```

emitting the value as `attr = "ESCAPED"` (+ ` %c` for case-, ` %d` for diacritic-insensitivity), with prefix/suffix search realized by concatenating unescaped `.*` outside the escaped literal — exactly Korp's construction with the bracket gap fixed.