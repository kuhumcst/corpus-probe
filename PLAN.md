# corpus-probe — a minimal, faithful CWB web frontend in Clojure(Script)

A plan for a web application that puts a simple, semantic-HTML face directly on
the [IMS Open Corpus Workbench](https://cwb.sourceforge.io/) (CWB) — driving its
query processor `cqp` as a child process, translating its terminal output as
directly as possible into HTML, and replacing the KORP installation at
<https://alf.hum.ku.dk/korp> for day-to-day corpus search.

**Recommendation in one paragraph.** Run `cqp -c` (child mode) as a short-lived
process per request from a Clojure backend (Pedestal 0.8 + http-kit connector),
speaking the same line protocol every serious CWB frontend uses (`.EOL.;` →
`-::-EOL-::-` sentinel, errors on stderr). Persist query results as CQP's own
saved named-query files so pagination and sorting are stateless and cost ~20 ms
per page even at millions of hits. Parse the output with a hardened separator
profile (TAB-framed delimiters — TAB and LF are provably the only bytes CWB can
never emit inside a token value) into plain data, and render it with
Replicant from shared `.cljc` hiccup — server-side for first paint, client-side
for interaction. No database, no user accounts, no config sprawl: the corpus
registry, `show cd;` and `info;` already describe everything the UI needs.

Everything in this document marked *verified* was tested empirically against a
real CWB 3.5.0 installation (Homebrew `cwb3`) and, where it matters, against
CWB source code and a from-source build of the 3.4 line. The full evidence base
(ten research reports, ~230 KB) lives in [docs/research/](docs/research/):

| Report | Contents |
|---|---|
| [cwb-core.md](docs/research/cwb-core.md) | Data model, registry format, all 20 command-line tools with real outputs |
| [cqp-language.md](docs/research/cqp-language.md) | Complete CQP query-language feature inventory (manual + grammar source) |
| [cqp-integration.md](docs/research/cqp-integration.md) | Child-mode protocol, CQi, display options with byte-exact outputs |
| [existing-frontends.md](docs/research/existing-frontends.md) | CQPweb, korp-backend/-frontend, Glossa/fglossa, TEITOK, TXM — lessons |
| [clojure-stack.md](docs/research/clojure-stack.md) | Replicant/Pedestal/babashka.process evaluation with versions |
| [korp-at-ku.md](docs/research/korp-at-ku.md) | Survey of the live alf.hum.ku.dk installation, feature cutline |
| [gap-kwic-parsing.md](docs/research/gap-kwic-parsing.md) | Byte-exact `cat` grammar, hostile-corpus tests, safe-delimiter proof |
| [gap-version-skew.md](docs/research/gap-version-skew.md) | 3.4.27 ↔ 3.5.0 compatibility, verified both directions |
| [gap-simple-search.md](docs/research/gap-simple-search.md) | Korp simple-search and CEQL compilation rules, escaping spec |
| [gap-nqr-persistence.md](docs/research/gap-nqr-persistence.md) | Save/restore mechanics, sort persistence, Danish collation recipe |

---

## 1. CWB in brief, and where a web app can attach

CWB stores corpora in a read-only indexed binary format described by a plain-text
**registry** file per corpus. Tokens carry **positional attributes** (`word`,
`pos`, `lemma`, …); XML-ish markup becomes **structural attributes** — regions
of token positions, optionally annotated (`text_title`, `s_id`, …); parallel
corpora link via **alignment attributes**. The query processor **cqp** executes
the CQP query language over these indexes and formats results as terminal text.

Four possible integration surfaces, evaluated:

1. **`cqp -c` (child mode)** — *chosen*. Designed exactly for this ("when CQP is
   being used as the back-end to a program providing a friendlier
   user-interface, for instance over the web", `man cqp`). Used by CQPweb,
   korp-backend, cglossa and fglossa. Battle-tested, line-oriented, trivially
   driven from `babashka.process`.
2. **cqpserver / CQi** — rejected. A TCP protocol whose spec is a 2000 "v0.1a"
   document; lacks KWIC formatting, tabulate and count; CWB's own developers
   say it "never found wide-spread use" and CWB 4.0 will replace it. The only
   Clojure client ([cqp-clj](https://github.com/emanjavacas/cqp-clj)) is a dead
   2015 proof of concept.
3. **The other CWB command-line tools** — used selectively, read-only:
   `cwb-describe-corpus` (corpus info pages), `cwb-lexdecode` (frequency
   lexicon / word lists, faster than the equivalent CQP query),
   `cwb-scan-corpus` (n-gram and joint frequency scans), `cwb-s-decode`
   (text inventories, metadata value lists).
4. **korp-backend's JSON API** — kept as a fallback only. The existing Flask
   backend at `alf.hum.ku.dk/korp/backend` works today and could serve a new
   frontend, but going through it would inherit its JSON shapes and its
   heuristic, lossy KWIC parser, and would defeat the goal of a faithful,
   self-contained CWB translation layer. Its *patterns* are copied instead
   (§5, §7).

There is no maintained Clojure CWB library to reuse — Clojars has zero hits for
cwb/cqp/concordance. What does exist and is used here: `babashka.process` for
the child process, and [cglossa](https://github.com/textlab/cglossa)
(Tekstlab's Clojure Glossa, superseded by the F# fglossa) as a pattern
reference for CQP driving from Clojure. The driver we need is small (§5) —
roughly the size of CWB's own `CWB::CQP` Perl module, ~450 lines.

## 2. The installation being replaced (verified 2026-09-03)

- korp-frontend 9.1.0 (AngularJS 1.x) over korp-backend 8.1.0 over
  **CQP 3.4.27**, registry of **158 corpora — 155 queryable**; three are
  phantom registry entries (`DUDSDFKBILLE2`, `LSPBYGGERI`, `SAXODEL1`) that
  make batch `corpus_info` calls fail wholesale. All 155 are **UTF-8**, all
  Danish, all monolingual (`"a": []` — no alignment anywhere), and
  `protected_corpora` is empty.
- Corpus families: LSP (DK-CLARIN specialised language, ~13 M words), MeMo
  (Danish novels 1870–1899, up to 64.6 M tokens, ~46 `text_*` metadata
  fields), FT_KORPUS (parliament speeches 2009–2017, 47 M tokens, **`word`
  only** — no pos/lemma), medieval ballads, Saxo, threat letters with rich
  forensic metadata.
- KU has already switched off word picture, lemgram autocomplete and maps in
  every mode file; the MySQL tables behind them don't exist
  (`korp.lemgram_index` missing) and `/relations` returns empty. The
  `struct_values` endpoint is broken (misconfigured binary path).

Two consequences. First, the minimal feature set (§3) is not a guess — it is
what this installation actually uses. Second, **the scholarly value of these
corpora lives in `text_*` metadata** (author/gender/publisher/typeface for
MeMo; speaker/party/role/date for FT; sender/victim/outcome for THREATS), so
metadata display and filtering are first-class requirements, not extras.

CWB version policy, verified in both directions with from-source builds
([gap-version-skew.md](docs/research/gap-version-skew.md)): the on-disk format
is identical between 3.4.x and 3.5.0. The app therefore ships with **its own
CWB 3.5.0, built from source in a container**, reading the KU data through a
read-only bind mount at the same absolute paths the registry names, while the
host's KORP keeps its 3.4.27 untouched. The container also carries gawk,
coreutils sort and the generated da_DK.UTF-8 locale for ExternalSort, a
writable volume for the result cache and a sized /tmp for sort files. The app
generates CQP for 3.5.0; Appendix B records the 3.4.27-safe subset for the
day it has to run on the host's binary instead. The wire protocol needs no
version branching at all.

## 3. Feature cutline

**Must have (v1)**
- Corpus chooser with folder grouping and multi-corpus selection (155 corpora
  are unusable as a flat list; folder structure mirrors the current modes).
- Raw **CQP query** input — the lingua franca and escape hatch; the current
  installation links the CQP tutorial from its front page.
- **Simple search**: one box + case/prefix/suffix checkboxes, compiled to CQP
  (§8) — never a second query engine.
- **KWIC concordance**: paging, sorting (left/right context, match, seeded
  random; correct Danish collation §9), adjustable/expandable context, match
  highlighted, all token + text attributes inspectable (sidebar/hover).
- **Frequency views**: group hits by attribute (`count by`), relative and
  absolute per corpus; whole-corpus frequency lists via `cwb-lexdecode`.
- Metadata filtering via CQP structural constraints, with value lists
  precomputed by `cwb-scan-corpus`.
- Corpus info pages (`cwb-describe-corpus -s`, registry, `.info` file).
- CSV/TSV export of KWIC and frequency tables; shareable URL state; da/en UI.

**Nice to have (v2)** — time distribution (the date metadata exists), sort by
arbitrary attribute, log-likelihood comparison of two searches, reading mode,
collocation tables (`cwb-scan-corpus` around dumped match positions).

**Considered and deferred, to reconsider if a reader asks** — *restriction
to a subcorpus built from a query's results* ("search only the texts where
X occurs"; CQPweb has it, KORP never has). Verified to work in CQP: a query
`expand to text`, activated, with the real query run inside it, and the
activations nest, so it composes with the metadata filter. It would reuse
the shape of §8's `restricted-query` and needs no new state, the previous
query travelling in the URL. Deferred because its value here is unproven:
the *sentence*-level reading of "within these results" needs no feature at
all (`[word="kat"] []* [word="hund"] within s` is one ordinary query), so
only the text-level reading is a real gap; CQPweb's audience is a teaching
one working a single corpus hard; and no KU reader has asked for it. The
same effort spent on query building for non-experts (the v2
attribute/operator builder of §8) would change more of what a linguist can
ask. One request from a real reader flips this.

**Deliberately dropped** (each was verified absent/disabled/unused at KU):
word picture, lemgram search, maps, dependency trees, parallel-corpus views,
auth/user accounts, news widgets — and above all **any database**. Everything
in v1 is served from CWB indexes plus a file cache. CQPweb's single biggest
operational burden is MySQL; Korp stays light only because its DB is optional.

## 4. Architecture

```
browser ── transit/JSON ──> Pedestal (http-kit connector)
                              │
                    ┌─────────┼──────────────┐
                    │ routes/interceptors    │
                    │  /api/corpora          │  cwb-describe-corpus, registry
                    │  /api/query            │  cqp -c   (KWIC pages)
                    │  /api/count            │  cqp -c   (group/count/tabulate)
                    │  /api/wordlist         │  cwb-lexdecode
                    │  /api/values           │  cwb-scan-corpus (cached)
                    │  /               (SSR) │  replicant.string/render
                    └─────────┬──────────────┘
                              │ babashka.process, one process per request
                              ▼
                  cqp -c -r /path/to/registry
                              │
              NQR cache dir (set DataDirectory) ── saved query results,
                                                   sortidx persisted
```

- **One `cqp -c` process per request batch** (the Korp/cglossa/fglossa
  pattern): commands written to stdin, stdout read to the sentinel, process
  exits. Crash isolation for free, no pool, no session affinity. CQP starts in
  milliseconds; state between requests lives in CQP's own saved named-query
  files (§9), so the app tier is stateless.
- The frontend is a thin Replicant app over the same data the API serves;
  first paint is server-rendered from identical `.cljc` view functions.
- Everything the UI knows about a corpus (attributes, size, charset, title)
  comes from CWB itself at runtime — `show cd;`, `info;`, the registry file —
  cached in memory keyed on registry-file mtime. App-level configuration is
  one EDN file: registry path, cache dir, corpus folder tree, display labels.

## 5. The cqp driver (the protocol, verified)

The complete contract a Clojure driver must implement — every line of this was
tested against 3.5.0 and cross-checked against 3.4.x source
([cqp-integration.md](docs/research/cqp-integration.md),
[gap-version-skew.md](docs/research/gap-version-skew.md)):

1. Spawn `cqp -c -r <absolute-registry-path>`. Child mode already implies
   PrettyPrint/paging/highlighting/AutoShow off; send `set PrettyPrint off;`
   anyway (every wrapper does).
2. First stdout line is the banner `CQP version 3.5.0` — parse it as a
   liveness/version probe.
3. Commands are `;`-terminated. After each real command send the pseudo-command
   `.EOL.;` **on its own line** (a trailing `#` comment in user input would
   otherwise swallow the terminator — CQPweb learned this the hard way). CQP
   answers with the line `-::-EOL-::-` on stdout: read lines until it appears;
   everything before it is that command's output. The marker arrives even after
   errors, so the stream always resynchronises. EOF before the marker means the
   process died — treat as fatal, discard any cache file involved.
4. **Any output on stderr means the last command failed.** That is the entire
   error protocol. Human-readable `CQP Error:` lines, plus the machine marker
   `PARSE ERROR` for syntax errors. Read stderr from a separate thread; treat
   the blob between commands as one opaque message. Exit code 0 is meaningless.
5. `set ProgressBar on;` yields TAB-separated
   `-::-PROGRESS-::-  pass  total  message` lines interleaved before real
   output — filter them; optionally surface them as progress events.
6. Timeouts: there is no in-band cancel. Enforce a wall-clock budget per
   process and destroy the process tree on expiry or client disconnect
   (`:shutdown :destroy-tree` in babashka.process covers JVM exit; pair with
   an explicit watchdog).
7. Charset: CQP transcodes nothing — bytes in, bytes out, per corpus. All 155
   KU corpora are utf8 (verified individually), but implement the lookup
   anyway: read `##:: charset` from the registry (or `info;`), decode/encode
   per corpus. One map lookup now prevents silent mojibake later.
8. Never send `exit;` mid-batch (kills the session, no marker); never let user
   input reach the command stream unquoted (§8).

Sketch of the driver surface (names as they will appear in the code):

```clojure
(defn run-batch!
  "Run `commands` in a fresh cqp child process against `corpus`,
  returning {:results [...] :errors [...]} with one entry per command."
  [{:keys [registry cache-dir timeout-ms] :as ctx} corpus commands]
  ...)
```

Every command's output is split off by the sentinel, so `:results` aligns with
`commands` positionally — the same design as `CWB::CQP::exec`.

## 6. Capturing output: the fidelity ladder

The central empirical finding
([gap-kwic-parsing.md](docs/research/gap-kwic-parsing.md)): CQP applies **no
escaping whatsoever** to any value it prints (`ascii_convert_string` is the
identity function), and only **TAB (0x09) and LF (0x0A)** — the VRT column and
line separators — can never occur inside a positional-attribute value.
Everything else (`/`, `|`, `<`, `>`, `"`, spaces, control bytes, the default
KWIC delimiters themselves) is legal token content. Therefore:

**Tier 1 — hardened `cat` profile** (the workhorse for KWIC):

```
set AttributeSeparator "<TAB>A<TAB>";  set TokenSeparator     "<TAB>T<TAB>";
set LeftKWICDelim      "<TAB>L<TAB>";  set RightKWICDelim     "<TAB>R<TAB>";
set StructureDelimiter "<TAB>S<TAB>";  set ShowTagAttributes  off;
set PrintStructures    "";
```

(each `<TAB>` a literal tab byte — CQP does not interpret `\t` escapes). With
TAB impossible in p-values, these frames are unforgeable: splitting a line on
TAB yields an unambiguous token/attribute/tag/match-boundary structure. This
is the profile's *only* job; the human-facing KWIC look is recreated in HTML.

**Tier 2 — TSV commands** for everything tabular, all verified byte-exact:

| Command | Output shape | Used for |
|---|---|---|
| `size A;` | one integer | hit counts |
| `dump A 0 49;` | `match⇥matchend⇥target⇥keyword`, `-1` unset | anchors, positions |
| `tabulate A match word, match text_title;` | TAB columns, one row/match | per-hit metadata (single-anchor columns only) |
| `count A by lemma;` | `freq⇥first-row⇥value` | frequency breakdown |
| `group A match lemma;` | `value⇥freq` (pairwise: `v1⇥v2⇥freq`) | grouped frequencies |
| `show cd;` | `p-Att⇥name⇥[-V]⇥[*]` | attribute inventory |
| `cwb-lexdecode -f -b -P lemma C` | `freq⇥string` | frequency lexicon |
| `cwb-scan-corpus C lemma pos` | `freq⇥v1⇥v2` | scans, metadata values |
| `cwb-s-decode C -S text_title` | `start⇥end⇥value` | text inventory |

**Hard rules distilled from the hostile-corpus tests:**
- Split output on `\n` only — never a `splitlines`-style function (VT, FF,
  U+2028 etc. are legal token bytes; Korp documents the same rule).
- Never show annotated s-attributes inline (`show +s_id`): annotation values
  are unescaped, may contain `>` and TAB, and a ~4 KB annotation **crashes CQP
  outright** via a `sprintf` buffer overflow in `compose_kwic_token`
  (reproduced; worth reporting upstream). `ShowTagAttributes off` avoids the
  crash; per-hit metadata comes from `tabulate` instead.
- Never use `tabulate` *range* columns (`match[-3]..matchend[3] word`) for
  data: tokens within a range are space-joined and spaces are legal in values.
  Single-anchor columns are fully safe. Context tokens come from the hardened
  `cat` line, which carries them unambiguously.
- Target/keyword anchors are structurally unreachable in child-mode `cat`
  output (the highlighted print path requires a TTY) — recover them from
  `dump`, which is trivially reliable.
- `PrintStructures` values are unescaped and heuristic to parse (Korp's parser
  silently drops lines it cannot repair) — so this plan simply doesn't use
  PrintStructures for data; `tabulate` columns carry the same information
  losslessly.
- s-attribute annotation values may contain TAB (verified), so when one is the
  final column of a TSV row, split with a bounded `rsplit`/split-limit.

The parser namespace turns each of these into plain data with golden-file
tests: every parser is developed against byte-exact captured outputs checked
into `test/resources/`, including the hostile corpus from the research
(tokens containing `/`, `<s>`, spaces, control bytes, the delimiters
themselves).

## 7. Faithful semantic HTML

"Faithful" means: the DOM preserves CWB's own output structure and vocabulary,
so anyone who knows cqp recognises what they see — not that we scrape ANSI art.
CQP itself points the way: its `set PrintMode html` (1990s `<UL>`/`<B>` markup)
and `sgml` mode (`<CONCORDANCE><LINE><MATCHNUM><STRUCS><CONTENT><MATCH><TOKEN>`)
define exactly which distinctions CWB considers part of a concordance. The
renderer maps the parsed Tier-1/2 data onto modern equivalents of that same
structure:

```clojure
;; one KWIC line -> hiccup (shared .cljc, rendered on both sides)
[:tr.kwic-line {:data-cpos 1019887}
 [:td.cpos "1019887"]
 [:th.structs {:scope "row"} "MEMO_1885 · Pontoppidan"]  ; from tabulate cols
 [:td.left  [:span.token {:title "pron · den"} "den"] " " ...]
 [:td.match [:mark [:span.token "hund"]]]                ; <mark> = the match
 [:td.right ...]]
```

- The concordance is a `<table>` — which is literally what CQP's own
  `PrintMode html` + `PrintOptions tbl` emits (cpos cell, left cell, match
  cell, right cell); we keep that shape and its right-aligned left context.
- Every token is an element carrying its full attribute set (title attr and/or
  sidebar on click — the data is already parsed, no extra round trip).
- Match = `<mark>`; target/keyword (from `dump`) get their own classes —
  *more* faithful than terminal cat, which cannot show anchors over a pipe.
- Monospace KWIC with center-aligned match column is the canonical corpus-
  linguistics reading experience; CSS reproduces the terminal aesthetic
  (including a literal "terminal view" that renders the concordance in a
  `<pre>`-style block virtually indistinguishable from an interactive cqp
  session — cheap to build since it's the same data, and a nice nod to CWB).
- Frequency tables (`count`/`group`/`lexdecode`) map to `<table>` with the
  same columns CQP prints, plus computed relative frequencies; `info;` and
  `cwb-describe-corpus` map to definition lists on corpus pages.

## 8. Search input

Two entry modes in v1, one query engine:

**Raw CQP** — passed through verbatim, wrapped in CQP's own sandbox designed
for exactly this (verified: it blocks every non-query command including the
shell-executing `cat A > "| cmd"` redirection):

```
set QueryLock <random-int>;   ...user query...   unlock <random-int>;
```

plus process-per-request isolation and timeouts. Query errors come back to the
UI verbatim from stderr — cqp's error messages (with the `<--` position
pointer) are good, and showing them unaltered is part of the faithfulness.

**Simple search** — Korp's design, which is ~15 lines and no grammar
([gap-simple-search.md](docs/research/gap-simple-search.md)): trim, split on
whitespace (each word = one CQP token), escape, apply prefix/suffix as
unescaped `.*` outside the literal, `%c` when case-insensitive, join, append
`within sentence`. The escaping function is the verified safe superset
(backslash-escape `. ? * + | ( ) [ ] { } ^ $ \`, then double `"`) — fixing the
`[ ] { }` gap Korp's own `regescape` has. CEQL-style wildcards (`?`, `*`,
`{lemma}`, `_POS`) are a possible v2 layer over the same compiler; the full
CEQL grammar is documented in the research if ever wanted.

An extended (attribute/operator/value row) builder is v2; with the actual KU
attribute inventories (often just word/pos/lemma/msd, and word-only for FT)
the simple box plus raw CQP brackets most of the space.

## 9. Pagination, sorting, caching (no database)

Verified recipe ([gap-nqr-persistence.md](docs/research/gap-nqr-persistence.md)),
measured at 2M matches on an M-series Mac:

- **Cache miss**: `set DataDirectory "<cache>"` (before corpus activation —
  activation is reset by it), activate corpus, run query into a named result
  `q_<hash>_<uuid>`, `size`, optional `sort`, `save`, `cat q… 0 49`, exit.
  Then atomically `rename` the file to `CORPUS:q_<hash>` (Korp's trick — no
  half-written cache is ever visible). Names must start with a letter (an md5
  can start with a digit — hence the prefix) and never equal a CQP keyword.
- **Cache hit**: fresh process, `set DataDirectory`, activate, `cat q_<hash>
  50 99`, exit. **Sort order persists in the save file** (the sortidx is
  serialized), so paging a sorted result costs ~20 ms — ~35× cheaper than
  re-sorting per request (Korp's approach) and ~270× cheaper than external
  re-sort. `cat` clamps out-of-range pages silently; no bounds pre-check.
- Cache key: hash of (corpus, query, within, sample, sort-mode, seed) **plus a
  corpus build stamp** (registry mtime) — a stale save file against a re-indexed
  corpus crashes CQP with SIGBUS (verified). Any abnormal exit ⇒ delete the
  file and re-run. Reap files by mtime (Korp uses 20 min).
- **Danish collation** requires `set ExternalSort on;` with
  `LC_ALL=da_DK.UTF-8` in the child environment (internal sort is byte order;
  `%c`/`%d` folding is wrong for æ/ø/å twice over — verified). ExternalSort
  shells out to `sort | gawk` with **gawk hardcoded**: gawk becomes a
  deployment dependency, and its absence must be treated as an error (CQP
  "resets to default ordering" on stderr and would otherwise silently serve
  corpus order). Seeded `sort … randomize 42` is stable across processes and
  needs no locale.
- Sizes/counts additionally memoized in-memory (Caffeine or an atom + TTL) —
  measure before adding anything more. This is the entire caching story.

## 10. Technology stack

Choices from [clojure-stack.md](docs/research/clojure-stack.md), all versions
verified current as of 2026-09:

| Layer | Choice | Why |
|---|---|---|
| HTTP server | Pedestal 0.8.1, http-kit connector (http-kit 2.8.1) | Existing Pedestal experience; 0.8 (Sep 2025) is freshly modernized — connector API drops the servlet stack, http-kit supported, SSE first-class (core.async channel per stream) for streamed/progress results; interceptors fit per-request process lifecycle (attach in `:enter`, kill in `:leave`/`:error`) |
| Child process | babashka.process 0.6.25 | Idiomatic ProcessBuilder wrapper; long-lived pipes, `:shutdown :destroy-tree`; keep stderr separate |
| Parsing | Hand-rolled line parsers | The wire format is a stable line protocol; instaparse would be overkill (reserved for a possible CQP-syntax highlighter later). ANSI handling unnecessary — child mode cannot emit colour (verified) |
| Frontend | Replicant 2026.07.1 | Data-driven hiccup, no React/npm treadmill; the app is "render server data, re-render on new page" — exactly Replicant's model; declared stable/production-quality. Event handlers are data vectors that `replicant.dom/set-dispatch!` routes to one hand-written dispatcher; Nexus, the companion action-dispatch library, was considered and left out, since a client with a dozen actions that each swap the state or start a fetch has no action chains for it to organise |
| SSR | `replicant.string/render` from shared `.cljc` views | One set of view functions for server first-paint and client updates |
| Wire | transit-clj/-cljs (charred where plain JSON is wanted) | Lossless EDN-shaped payloads between two Clojure ends; charred aligns with Pedestal's own JSON choice |
| Build | deps.edn + shadow-cljs 3.5.0 | Standard; shadow reads deps.edn |
| Dev | portal, dataspex, portfolio | Portfolio is a genuinely good fit: develop KWIC components against canned parser fixtures with no backend running |

**Honest alternative considered**: no ClojureScript at all — ring + reitit +
http-kit, hiccup SSR, htmx/Datastar streaming HTML fragments over SSE. One
language, no cljs build. Rejected (narrowly) because snappy client-side
interaction on loaded concordance pages — sort toggles, sidebar inspection,
context expansion over thousands of rows — is core UX here, and Replicant with
shared `.cljc` views keeps the total complexity close to the htmx option while
keeping those interactions local. Revisit only if the cljs build feels heavy in
practice.

## 11. Project layout

```
corpus-probe/
├── deps.edn  shadow-cljs.edn
├── resources/config.edn            ; registry path, sort locale, folder tree
├── src/dk/cst/corpus_probe/
│   ├── cqp.clj                     ; child-process driver (§5)
│   ├── parse.clj                   ; output parsers -> data (§6)
│   ├── query.clj                   ; CQP generation: simple-search compiler,
│   │                               ;   escaping, QueryLock wrapping (§8)
│   ├── corpus.clj                  ; registry, show cd, info -> corpus facts
│   ├── search.clj                  ; KWIC, concordance, frequency tables
│   ├── tools.clj                   ; cwb-describe-corpus, cwb-lexdecode
│   │                               ;   (scan-corpus and s-decode to come)
│   ├── stats.cljc                  ; relative frequencies
│   ├── export.clj                  ; TSV/CSV exports of concordances and
│   │                               ;   frequency tables
│   ├── api.clj                     ; Pedestal routes + SSR handlers
│   ├── server.clj                  ; config, start/stop, CSP
│   ├── ui.cljs                     ; Replicant client
│   └── views/                      ; .cljc shared hiccup: layout, page
│       └── ...                     ;   (search), kwic, frequencies, corpus
│                                   ;   (index, chooser, info)
├── test/…                          ; golden-file tests against captured outputs
│   └── resources/                  ;   (the captures and a registry entry;
│                                   ;   hostile cases live inline in the tests)
├── dev/                            ; encode.sh, capture-golden.sh, user.clj
└── docs/research/                  ; the evidence base for this plan
```

Local development runs against corpora encoded from VRT fixtures in the repo
(`cwb-encode`/`cwb-makeall` in a dev script — Homebrew `cwb3` provides the
tools; a tiny Danish corpus was already encoded this way while verifying this
plan). Production points `config.edn` at the existing server registry.

## 12. Milestones

1. **Driver + parsers** (pure backend, REPL-driven): `cqp.clj`, `parse.clj`,
   golden tests from captured outputs, hostile-corpus regression fixtures.
   Exit: `(kwic ctx "MEMO_1880" "[lemma=\"hund\"]" {:page 0})` returns clean
   data at the REPL.
2. **KWIC vertical slice**: query form (raw CQP), server-rendered concordance
   with paging, hardened profile end-to-end, QueryLock, timeouts. Exit: usable
   read-only concordancer for one corpus.
3. **Interactivity**: Replicant client takes over after SSR; sidebar attribute
   inspection, context expansion (re-query by cpos via `dump` + wider
   context), sorting incl. Danish external sort, URL state.
4. **Breadth**: corpus chooser + multi-corpus search (serial fill then
   parallel counts, Korp-style), simple-search compiler, frequency views,
   corpus info pages, export.
5. **Cutover**: metadata filtering with `cwb-scan-corpus` value lists, da/en
   i18n, deploy next to the existing KORP (`/korp` untouched), fix or filter
   the three phantom registry corpora, run CWB 3.5.0 in the app's own
   container (§2).

## 13. Risks and mitigations

| Risk | Mitigation |
|---|---|
| CQP crash on inline annotated s-attrs (verified overflow) | Never `show +` annotated s-attributes; `ShowTagAttributes off`; metadata via `tabulate`; report upstream |
| User CQP is a shell-execution vector via `cat … > "\| cmd"` | QueryLock wrapping (verified to block it) + process isolation + no server state worth stealing; never interpolate unescaped input |
| Runaway queries | Wall-clock watchdog, destroy process tree, kill on client disconnect; optional ProgressBar events to the UI |
| Stale NQR cache vs re-indexed corpus ⇒ SIGBUS | Build-stamp in cache key; abnormal exit ⇒ poison + retry (verified behaviour) |
| gawk missing ⇒ silently wrong sort order | Startup self-check for gawk + system sort; treat ExternalSort stderr as an error, not a warning |
| 3.4.27 vs 3.5.0 drift | The app runs its own 3.5.0 in a container over the same data (§2); Appendix B lists what to avoid should it ever run on the host's 3.4.27; the protocol itself is identical |
| Phantom corpora break batch metadata calls | Vet the corpus list at startup (`cwb-describe-corpus` per corpus, drop failures loudly) |
| Replicant is young | API declared stable; worst case the `.cljc` hiccup views are portable to any hiccup renderer — the investment is in driver/parsers/views, not the renderer |

## Appendix A — verified output samples (local CWB 3.5.0)

Default ASCII KWIC (`show +pos +lemma; set Context 1 s; set PrintStructures "text_id, s_id";`):

```
    9: <text_id t1><s_id 2>:  Katten/NCSD/kat jagter/VPRA/jagte en/D/en lille/AN/lille <hund/NCSI/hund> i/PP/i haven/NCSD/have ./PUN/.
```

`set PrintMode sgml` (CWB's own structural view of the same concordance —
the model for §7's HTML):

```
<CONCORDANCE>
<attribute type=positional name="word" anr=0>
<attribute type=positional name="lemma" anr=1>
<LINE><MATCHNUM>9</MATCHNUM><STRUCS>&lt;text_id t1&gt;</STRUCS><CONTENT> <TOKEN>Katten/kat</TOKEN> … <MATCH><TOKEN>hund/hund</TOKEN></MATCH> …</CONTENT></LINE>
</CONCORDANCE>
```

Child-mode protocol (`cqp -c`), commands `PROBE;` `.EOL.;` `size Last;` `.EOL.;`:

```
stdout:  CQP version 3.5.0
         -::-EOL-::-
         0
         -::-EOL-::-
stderr:  CQP Error:
             Corpus ``Last'' is undefined
```

`count A by lemma;` / `group A match lemma by match pos;` / `tabulate` /
`dump` / `show cd;` / `cwb-lexdecode -f` / `cwb-scan-corpus` samples: see
[docs/research/cqp-integration.md](docs/research/cqp-integration.md) and
[docs/research/cwb-core.md](docs/research/cwb-core.md) — all byte-exact.

## Appendix B — the 3.4.27-safe CQP subset

For reference: the deployment runs 3.5.0 (§2), so nothing here constrains
what the app generates today. Several commands it does generate are verified
on 3.5.0 only (`sort ... reverse`, the pairwise `group ... by`, `delete ...
without keyword`, assigning a saved result to `Last`) and would need checking
first if it ever had to run on the host's 3.4.27.

Safe (all verified present): `.EOL.;` protocol; activation; standard queries
incl. `(?longest)`-style strategy modifiers; `size`, `cat` (ranges), `dump`,
`undump`, `sort` (incl. `randomize <seed>`), `count`, `group` (incl. document
frequencies), `tabulate`; `show cd;` (4-column), `info;`; `set
AttributeSeparator/TokenSeparator/StructureDelimiter`; Context in **token or
s-attribute units**; `@0`–`@9` anchors; `save`/`set DataDirectory`.

Avoid generating (3.5-only or version-sensitive): `<<region>>` elements,
match selectors (`show a[..] .. b[..]`), conditional `set NQR anchor` forms,
unescaped `^`/`$` as literals (semantics changed in 3.4.34 — always send
fully-escaped regexes, identical on both), character-unit context widths
(off-by-one fixed in 3.5), `%c`/`%d` on non-UTF-8 corpora (moot today), NQR
files > 2 GiB.

## Sources

Primary: [CQP Manual](https://cwb.sourceforge.io/files/CQP_Manual/) ·
[CWB Encoding Manual](https://cwb.sourceforge.io/files/CWB_Encoding_Tutorial/) ·
CWB 3.5.0/3.4.22/3.0.0 source (SourceForge) ·
[CWB::CQP](https://metacpan.org/dist/CWB) ·
[korp-backend](https://github.com/spraakbanken/korp-backend) /
[korp-frontend](https://github.com/spraakbanken/korp-frontend) ·
[CQPweb](https://cwb.sourceforge.io/cqpweb.php) ·
[cglossa](https://github.com/textlab/cglossa) /
[fglossa](https://github.com/textlab/fglossa) ·
[Replicant](https://replicant.fun) · [Pedestal](https://pedestal.io) ·
live probes of <https://alf.hum.ku.dk/korp> (2026-09-03).
Full citations with URLs per claim: [docs/research/](docs/research/).
