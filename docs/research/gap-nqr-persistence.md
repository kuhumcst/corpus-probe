All experiments are complete. Compiling the report.

# Stateless paginated/sorted concordances over short-lived `cqp -c` processes — verified findings

**Test environment.** CWB 3.5.0 (Homebrew `cwb3`, compiled 2026-07-05) on macOS (Darwin 25.6.0, Apple `sort` 2.3-Apple, no GNU coreutils in PATH). Two purpose-built Danish corpora (utf8 + latin1 encodings of identical 5,000-token text with æ/ø/å/aa words, `cwb-encode -c utf8|latin1` + `cwb-makeall`) plus a 2,000,000-token utf8 corpus for timing. Everything below was verified empirically on this setup unless marked otherwise.

## 1. save/restore mechanics

**On-disk filename.** `save NAME;` under `set DataDirectory "<dir>"` creates exactly `<dir>/<CORPUS>:<NAME>` (e.g. `DKUTF8:A` — colon-separated, uppercase corpus ID). Format (from `save_subcorpus()` in [corpmanag.c](https://svn.code.sf.net/p/cwb/code/cwb/trunk/cqp/corpmanag.c), confirmed by hexdump): int32 magic `0x89462802`, NUL-terminated registry path *as passed via `-r`* (relative paths are stored relative — always pass an absolute `-r`), NUL-terminated corpus name, padding to 4-byte boundary, int32 size, then `size` × (match,matchend) int32 pairs, then optional length-prefixed sortidx/targets/keywords arrays. Native byte order — "platform-dependent uncompressed binary format" per [CQP manual §3.2](https://cwb.sourceforge.io/files/CQP_Manual/3_2.html). Unsorted file ≈ 8 bytes/match; sorted ≈ 12 bytes/match (2M matches = 24 MB). The embedded registry path does **not** have to match the `-r` of the loading process — loading with a different registry defining the same corpus worked.

**Loading and activation.** When DataDirectory is set, CQP scans it and registers every `CORPUS:NAME` file *without* loading it; data is mmap-loaded lazily on first access (`load_corpusnames()`/deferred `attach_subcorpus()` in corpmanag.c; "the actual data is only read into memory when the query results are accessed", manual §3.2). Consequences, all verified:
- `set DataDirectory "<dir>"; DKUTF8; cat A 0 4;` works in a fresh process. `Last = A;` is **not** needed — `cat A <start> <end>` suffices. Korp issues `Last = query_data_<md5>;` only as an alias so its subsequent generic `sort`/`cut`/`cat Last` commands are name-agnostic ([korp/views/query.py](https://raw.githubusercontent.com/spraakbanken/korp-backend/master/korp/views/query.py)).
- The corpus does not *strictly* need activation: both unqualified `cat A` (no corpus active) and qualified `cat DKUTF8:A` worked. But: (a) with same-named NQRs under two corpora and no active corpus, unqualified resolution silently picked an arbitrary one (directory-scan order); (b) [manual §3.1](https://cwb.sourceforge.io/files/CQP_Manual/3_1.html) documents that qualified-NQR access "should not be used with cat or any other command that generates KWIC output" (context-descriptor corruption bug; I could not reproduce it in 3.5.0, but don't rely on that); (c) unqualified names are "automatically prefixed with the currently activated corpus" (§3.1). **Always activate the corpus, and do it *after* `set DataDirectory`** — `check_available_corpora()` explicitly resets the current corpus to NULL when the corpus list is rescanned (corpmanag.c: "due to list being fiddled with, current corpus is no longer valid -> reset it"); this is why CQPweb's `set_data_directory()` re-activates the corpus afterwards ([cqp.php](https://svn.code.sf.net/p/cwb/code/gui/cqpweb/trunk/lib/cqp.php)) and the manual says to re-activate.

**NQR name constraints** (lexer: `id = [a-zA-Z_][a-zA-Z0-9_-]*`, [parser.l](https://svn.code.sf.net/p/cwb/code/cwb/trunk/cqp/parser.l)):
- Leading digit → hard parse error (`1abc` lexes as INTEGER: "CQP Syntax Error: syntax error, unexpected INTEGER").
- A name exactly equal to any lowercase keyword fails: verified for `by` ("unexpected BY_SYM"), `cut`, `to`, `on`, `sort`, `save`, `cat`; full keyword list in parser.l includes `cd`, `group`, `count`, `size`, `dump`, `undump`, `set`, `show`, `reduce`, `expand`, `delete`, `match`, `matchend`, `target`, `keyword`, etc. Longest-match lexing means a *prefix* collision is harmless (`cd0f3a…` is fine).
- `_foo`, `foo-bar`, lowercase names, and digits after the first char all work; the manual's "should begin with capital letter" (§3.1) is convention, not enforced in 3.5.0.
- Since an md5 is 32 hex chars it can never equal a keyword, but it *can* start with a digit — that leading-digit rule is the real reason Korp names caches `query_data_<md5>` (query.py: `cache_query = "query_data_%s" % checksum`).

**Missing/stale save files** (all on a 3.5.0 process with exit code 0 unless noted):
- Missing name / nonexistent DataDirectory: stderr gets ``CQP Error:  Corpus `NOSUCH' is undefined``; stdout empty; **exit code 0** — a backend must parse stderr, not the exit code (Korp reads stderr and raises; [korp/cwb.py](https://raw.githubusercontent.com/spraakbanken/korp-backend/master/korp/cwb.py)).
- Corrupt magic: stderr `Magic number incorrect in <path>` + `Corpus ``X'' is undefined`; clean failure.
- File whose match positions exceed the current corpus (i.e. corpus re-indexed smaller since the save): **CQP crashes with SIGBUS (exit 138, no stderr output at all)** when the position is touched — the loader validates only the magic number and ≥8-byte size, not positions.
- Truncated file: **loads silently and serves garbage** (rows past the truncation read as cpos 0 via zero-filled mmap pages). 
⇒ Cache invalidation is entirely the application's job: key the cache filename on (query, corpus, corpus mtime/build id) and treat abnormal exit (signal) as cache-poison — delete the file and retry. Korp additionally writes to `<CORPUS>:query_data_<md5>_<uuid4>` and `os.rename()`s to the final name after the process exits (query.py), so concurrent requests never read a half-written file; copy that.
- `save X` with no DataDirectory set: silent no-op (no file, no error). `save` into an unwritable/nonexistent dir: stderr `cannot open output file <path>`, no crash.

## 2. Sort persistence — the central result

**`save` after `sort` persists the sort order, and a fresh process pages in that order.** Verified for both `sort NAME by word;` and `sort NAME randomize 42;`: process 1 ran query → sort → `save`; process 2 ran only `set DataDirectory …; CORPUS; cat NAME 0 5;` / `cat NAME 5 9;` and produced identical, correctly-ordered, correctly-offset pages. The sortidx array is serialized in the save file (file size grows from 8 to 12 bytes/match; corpmanag.c writes the `sortidx` component). `dump NAME <start> <end>` follows the same persisted order (manual [footnote 18](https://cwb.sourceforge.io/files/CQP_Manual/footnode.html): dump "dumps the matches of a named query in their current sort order"). A saved NQR does **not** reload in corpus-position order — re-sorting per request is *not* required.

**Seeded randomize is doubly stable.** `sort NAME randomize 42` recomputed in a fresh process on the same result gives the identical order — with a seed, keys are computed per-match from the match's corpus positions (`cl_set_rng_state(range[i].start + seed, …)` in `SortSubcorpusRandomize()`, [ranges.c](https://svn.code.sf.net/p/cwb/code/cwb/trunk/cqp/ranges.c)), independent of process history. So for seeded-random pagination *either* strategy works: persist the sortidx once, or re-issue `sort NAME randomize <seed>` every request. Unseeded `sort NAME randomize;` also came out identical across fresh processes (internal RNG starts from a fixed default state) — but don't rely on that; pass an explicit seed.

**Re-sorting only changes memory.** After loading a saved NQR and re-sorting it, the on-disk file is untouched (md5-identical) until you `save` again; `show named;` flags go `md-` (memory+disk) → `m-*` (modified, disk copy stale). So one cached cpos-ordered file can serve *different* sort orders per request Korp-style, or you can cache one file *per (query, sort)* and skip per-request sorting entirely.

**Why Korp re-sorts anyway.** Korp saves the NQR *before* sorting (query.py order: `NAME = Last; save NAME;` … then `set ExternalSort yes;` + sort command + `cat Last <start> <end>;`), deliberately caching in cpos order so one cache file serves any requested sort mode; the price is a re-sort on every request.

**Per-request re-sort cost** (2M-match NQR on 2M-token corpus, M-series Mac; fresh process each time):

| operation | wall time |
|---|---|
| query 2M matches + `save` (no sort) | 0.07 s |
| + internal `sort by word` + save (24 MB file) | 0.70 s |
| fresh process: load saved sorted NQR + `cat` one page | **0.02 s** |
| fresh process: Korp-style internal re-`sort by word` + cat | 0.68 s |
| fresh process: re-`sort randomize 42` + cat | 0.17 s |
| fresh process: **external** locale-aware re-sort + cat | 5.4 s |

⇒ paging from a persisted sortidx is ~35× cheaper than internal re-sort and ~270× cheaper than external re-sort at 2M matches; at ≤100K matches internal re-sort is <50 ms and either approach is fine.

## 3. ExternalSort

**Mechanics** (from `SortExternally()` in ranges.c, confirmed by intercepting the pipeline): CQP writes a temp file — observed at `/tmp/cqp-tempfile.XXXXXX`, ignoring `TMPDIR` on this build — with one line per match: `<match-index> <sort-key tokens…>` (with `%c`/`%d` flags, a canonicalized key column first, then `\t`, then the raw tokens), then runs `popen("sort -k 2 -k 1n [-r] <tmpfile> | gawk '{print $1}'")` and reads back the permuted indices into sortidx. `ExternalSortCommand` defaults to `"sort -k 2 -k 1n "` ([options.c](https://svn.code.sf.net/p/cwb/code/cwb/trunk/cqp/options.c)); **`gawk` is hardcoded** in the pipe. On a stock Mac (no gawk) it fails: stderr `sh: gawk: command not found` / `sort: Broken pipe` / `CQP Error: External sort failed (reset to default ordering).` — and `cat` then silently serves **corpus order**, so a backend ignoring stderr serves wrongly-ordered pages. `brew install gawk` fixes it. External sort is skipped entirely under `-x` insecure mode and for `count` (`if (UseExternalSort && !insecure && !count_mode)`), so don't combine `-x` with ExternalSort.

**It works fine under `cqp -c` with piped stdio** — the child inherits the environment through `popen()`, nothing about child mode interferes.

**Collation env vars.** The collation is whatever the system `sort` sees: POSIX precedence `LC_ALL` > `LC_COLLATE` > `LANG`. Documented in [CQP manual §3.3 footnote 7](https://cwb.sourceforge.io/files/CQP_Manual/footnode.html): "set the LC_COLLATE or LC_ALL environment variable to an appropriate locale before running CQP. You should not use the %c and %d flags in this case." Empirically on macOS (utf8 corpus, words spanning a–z/æ/ø/å/aa):
- `LC_ALL=C`: byte order (see §4).
- `LC_ALL=da_DK.UTF-8`: **correct Danish order** — …sol sø zebra æble æg ærlig øje ørn øst å åben ål år — æ<ø<å after z, *and* the traditional aa=å rule holds (aaben/aal interleave with åben/ål).
- `LC_COLLATE=da_DK.UTF-8` **alone: wrong order** on macOS/BSD sort (LC_CTYPE stays C, multibyte decoding breaks). `LC_COLLATE` + `LC_CTYPE=da_DK.UTF-8` = correct, same as LC_ALL. Note **Korp sets only `LC_COLLATE`** (cwb.py: `env["LC_COLLATE"] = self.locale`) — that works on typical Linux servers where LANG supplies a UTF-8 LC_CTYPE, but is fragile; export `LC_ALL` (or COLLATE+CTYPE). glibc behavior with LC_COLLATE alone is reportedly self-contained per collation table [unverified — not tested on Linux; test on the deployment OS].
- Latin1 corpus: keys are written as **raw latin1 bytes**. With `LC_ALL=da_DK.ISO8859-1`: correct Danish order. With a mismatched `da_DK.UTF-8` locale it *happened* to come out right on Apple's sort, but the input is invalid UTF-8 — undefined behavior, don't rely on it [order under glibc unverified]; match the locale charset to the corpus charset.

**The %c/%d restriction.** It is a documented *should-not*, not an enforced error: 3.5.0 emits no warning and happily combines them — the folded key (utf8 `%d`: å→a, but æ/ø *unfolded*) becomes the primary sort column, which fights locale collation and yields hybrid orders (observed: å-words folded in among a-words while æ/ø-words stayed after z). Exactly why footnote 7 says don't. Historical note: the manual §3.3 body itself *demonstrates* `set ExternalSort on; sort by word %cd;` for speed — the restriction applies specifically when you're using ExternalSort *for locale collation*.

## 4. Internal sort baseline

Internal sort uses `cl_string_qsort_compare()` on the corpus charset — pure **byte order**, no locale (ranges.c):
- utf8 corpus, `sort by word;`: ASCII a–z first, then å < æ < ø (UTF-8 byte order C3A5 < C3A6 < C3B8) — wrong for Danish twice over (order and placement of aa).
- utf8, `sort by word %cd;`: charset-aware *folding* then byte compare: `%d` strips true diacritics (å→a, blå→bla) but leaves æ/ø untouched (they're separate letters, not accented ones), so å-words interleave with a-words while æ/ø-words stay after z. Still not Danish.
- latin1 corpus, plain: byte order å(E5) < æ(E6) < ø(F8) after z. With `%cd`: latin1's folding table maps æ and ø too (æble→a…, øje→o…), so *all* three interleave with a/o words. Charset changes `%d` semantics — another reason per-corpus charset consistency matters.

⇒ Correct Danish order is only obtainable via ExternalSort + Danish locale (without `%c`/`%d`).

## 5. Tested recipe

Per-process invariants: spawn `cqp -c -r /abs/path/to/registry`, write commands to stdin encoded in **the corpus charset** (a UTF-8 "æ" sent to a latin1 corpus silently matches nothing), read stdout, and treat *any* stderr line except the whitelisted noise as failure (Korp's whitelist in cwb.py). Exit code 0 is meaningless; exit >128 (SIGBUS/SEGV) ⇒ delete the cache file and re-run from scratch. Start every session with `set PrettyPrint off;` (Korp) — optionally `set ProgressBar off;` (CQPweb). Use `.EOL.;` (prints `-::-EOL-::-`) as a section delimiter when batching commands, as Korp does.

**Request 1 (cache miss) — Danish-sorted:**
```
set PrettyPrint off;
set DataDirectory "/cache/dir";        # before corpus activation (activation is reset by this)
DKUTF8;
Q_<hash>_<uuid> = <the query>;
size Q_<hash>_<uuid>;
set ExternalSort on;                    # needs gawk + system sort on PATH; no %c/%d flags
sort Q_<hash>_<uuid> by word;           # process env: LC_ALL=da_DK.UTF-8 (charset must match corpus)
save Q_<hash>_<uuid>;
show +word; set Context 5 words;        # display config, per request
cat Q_<hash>_<uuid> 0 49;
exit;
```
then `rename("/cache/dir/DKUTF8:Q_<hash>_<uuid>", "/cache/dir/DKUTF8:Q_<hash>")` for atomicity, and store `size` (memcached/redis) so hit-count needn't be recomputed. For seeded-random instead: `sort Q… randomize <seed>;` (internal, no locale/ExternalSort needed). Name rule: start with a letter/underscore, chars `[A-Za-z0-9_-]`, never a bare digit-initial hash, never exactly a CQP keyword — a fixed prefix like `query_data_` satisfies all three.

**Request N (cache hit) — no re-sort needed:**
```
set PrettyPrint off;
set DataDirectory "/cache/dir";
DKUTF8;
show +word; set Context 5 words;
cat Q_<hash> 50 99;
exit;
```
`cat NAME <start> <end>` is 0-based inclusive, follows the persisted sortidx, clamps `end` past the last match, and yields empty output (no error) when `start` is past the end — pagination needs no bounds pre-check. `dump Q_<hash> 50 99` gives the same rows as cpos 4-tuples if you'd rather render context yourself. Cost measured at 2M matches: ~20 ms per request total.

Cache one file per (corpus, query, sort-mode[, seed]); include a corpus build-stamp in the hash since stale files crash (SIGBUS) or silently serve garbage. Korp's alternative — cache only the unsorted result and re-issue `set ExternalSort yes; sort …` every request — is correct too and lets one cache serve all sort modes, at 0.2–5 s per request on very large results (fine ≤ ~100K matches, where internal re-sort stays under ~50 ms).

Sources: [CQP Manual](https://cwb.sourceforge.io/files/CQP_Manual/) §3.1, §3.2, §3.3 (+footnotes 7, 18), §7.1; `cqp` man page (`-l`, `-x`); CQP source [corpmanag.c](https://svn.code.sf.net/p/cwb/code/cwb/trunk/cqp/corpmanag.c), [ranges.c](https://svn.code.sf.net/p/cwb/code/cwb/trunk/cqp/ranges.c), [options.c](https://svn.code.sf.net/p/cwb/code/cwb/trunk/cqp/options.c), [parser.l](https://svn.code.sf.net/p/cwb/code/cwb/trunk/cqp/parser.l); Korp [query.py](https://raw.githubusercontent.com/spraakbanken/korp-backend/master/korp/views/query.py) / [cwb.py](https://raw.githubusercontent.com/spraakbanken/korp-backend/master/korp/cwb.py); CQPweb [cqp.php](https://svn.code.sf.net/p/cwb/code/gui/cqpweb/trunk/lib/cqp.php). Test artifacts (corpora, registry, cache files, downloaded sources): `/private/tmp/claude-501/-Users-rqf595-Code-corpus-probe/a3908530-2a88-4d2f-9f1a-81ac8d36e642/scratchpad/cwbtest/`.