I have all the empirical and source-level data needed. Compiling the deliverable.

---

# CWB 3.5.0 `cat` KWIC Tokenization Grammar — Tested Byte-Exact Report

Installed and verified against **CWB 3.5.0** (`cqp -v` → `Version: 3.5.0`, `brew install cwb3`, macOS arm64). All findings below are reproduced empirically by driving `cqp -c` over a pipe with the `.EOL.;` → `-::-EOL-::-` sentinel protocol, and cross-checked against the **release-3.5.0 source** (`https://svn.code.sf.net/p/cwb/code/cwb/release/3.5.0/cqp/`). Test corpora `HOSTILE`, `HOSTILEX` (`-x`), `HOSTNUM` (`-n`), `HOSTILE2`, `GAP`, `BIG`, `NUL`, `MAGIC` were built with `cwb-encode`/`cwb-makeall` in `/private/tmp/.../scratchpad/cqp_probe_x9/`. In byte transcripts below, `^I`=TAB(0x09), and `\xNN` are raw single bytes.

The corpus deliberately contains p-attribute values with `/ | < > " &`, embedded space (`New York`), and control bytes VT(0x0B)/BEL(0x07)/ESC(0x1B); words literally equal to `<s>`, `</s>`, `<np>`, `-::-EOL-::-`, `---:::`; annotated s-attribute `s_n` with values containing `>`, `"`, raw TAB, and `>: `; and multi-token matches.

## 0. Verified line grammar of ascii `cat` (source: `concordance.c` `compose_kwic_line`/`compose_kwic_token`, `ascii-print.c` `ASCIIPrintDescriptionRecord`)

One concordance line is emitted as, in order:

```
[ "%6d.\t"        if PrintOptions +num ]          <- NOTE: emits a stray TAB
  "%9d: "         if show +cpos (CPOSPrintFormat, default on)
[ PrintStructures block  "<name val><name2 val2>" ": "  if PrintStructures set ]
  <left context>   TokenSeparator-joined tokens
  [TokenSeparator] LeftKWICDelim
  <match tokens>   TokenSeparator-joined
                   RightKWICDelim
  <right context>  TokenSeparator-joined
  "\n"             (AfterLine)
```
Each **token** is assembled by `compose_kwic_token` as:
```
{ StructureDelimiter StructureBeginPrefix("<") tagname[ " " annot ] StructureBeginSuffix(">") StructureDelimiter }*   (open tags, ascending)
  attrval1 AttributeSeparator attrval2 AttributeSeparator ...                                                          (p-attrs, show order)
{ StructureDelimiter StructureEndPrefix("</") tagname StructureEndSuffix(">") StructureDelimiter }*                    (close tags, descending)
```
Defaults: `TokenSeparator=" "`, `AttributeSeparator="/"`, `StructureDelimiter=""`, `LeftKWICDelim="<"`, `RightKWICDelim=">"`. `ascii_convert_string` (the `printToken` hook) is an **identity function** — **no escaping is ever applied to any value** (`ascii-print.c`: `const char *ascii_convert_string(const char *s){ return s; }`). Every hostile byte in a p- or s-value passes through verbatim.

Baseline (default settings), match `[word="multi"][word="token"]`:
```
        0:                           <multi token> match a/b x|y New York vt
```
The token `a/b` is already ambiguous against `AttributeSeparator="/"`; the context token ` <s>/LEADSPACE/ls` shows a word literally `<s>` glued exactly like an inline tag.

---

## 1. `set AttributeSeparator` / `set TokenSeparator` — control characters (Task 1)

**Syntax, tested:** CQP's option-string parser does **not** process C escapes. `set AttributeSeparator "\t"` and `set AttributeSeparator '\t'` both store the literal 2-byte sequence backslash-t; `"\x07"` stores 4 literal bytes; `"\a"` stores 2. Both quote styles (`"…"` and `'…'`) are accepted and behave identically. To install a real control character you must place the **raw byte** inside the quotes. This matches the manual §7.1 verbatim: control codes "have to be included as literal characters in the set command because CQP doesn't support escape sequences such as `\t` or `\x09`" (https://cwb.sourceforge.io/files/CQP_Manual/7_1.html).

- Multi-character separators are accepted (`set AttributeSeparator "::"`, `"\tA\t"`).
- Empty string **resets to the PDR default** (`set AttributeSeparator ""` → readback `<default>`; renders `/` again), not to "no separator". The manual's claim that an empty string disables the separator is misleading — it restores the default `/`.
- `set X;` with no value reads the current value back.

**BEL/ESC verified** (`AttributeSeparator`=BEL 0x07, `TokenSeparator`=ESC 0x1B), token `a/b`:
```
3: i\x07P\x07L\x1btoken\x07P\x07L\x1bmatch\x07P\x07L\x1b<a/b\x07PO/S\x07le/mma>\x1bx|y\x07P|Q\x07l|m\x1bNew York\x07NP S
```
Values containing the default `/` (`a/b`, `PO/S`, `le/mma`) no longer break attribute splitting: attributes are now BEL-delimited, tokens ESC-delimited. **TAB, BEL, ESC are all accepted** and, being impossible in p-values (see §7), give an unambiguous *p-attribute layer*.

---

## 2. Inline structural tags with `show +s +s_n` (Task 2)

**Tags are glued to the neighbouring token with NO TokenSeparator between them** (source: in `compose_kwic_token` the open/close tags and the p-attrs are concatenated into the *same* `token` buffer; `TokenSeparator` is only inserted *between* whole tokens in `compose_kwic_line`). Tested (`show +s +np +s_n`, `ShowTagAttributes on`):
```
0:  <<s><s_n a>b>multi/P/L token/P/L> match/P/L</s_n></s> <s>...
19:  ...<np>start/P/L <multi/P/L token/P/L</np>> end/P/L</s>
```
- Open tag `<s><s_n a>b>` is prefixed directly onto `multi/P/L` with no space.
- Close tags `</s_n></s>` are suffixed directly onto the preceding token.
- The annotation value `a>b` is printed **raw** inside `<s_n a>b>` → the tag body itself contains a `>` that is **not** the tag terminator. `"` and TAB in annotations pass through identically (`HOSTILE2`: `<s_n tab^Iinside>`, `<s_n x>: y>`).

**`set ShowTagAttributes off`** drops the annotation, leaving the bare name: `<s_n>` (source `compose_kwic_token`: `if (show_tag_attributes && region->annot) sprintf(body,"%s %s",name,annot); else cl_strcpy(body,name);`). Default is `on`.

**Distinguishing a real tag from a token whose word is literally `<s>`:** With default settings this is **impossible** — ` <s>/LEADSPACE/ls` and a real `<s>` open tag are byte-indistinguishable except that the tag carries no `AttributeSeparator`, which a heuristic can't rely on. The **only** reliable disambiguation is `set StructureDelimiter` to a byte guaranteed absent from the data: tags are then wrapped in that byte on both sides, tokens are not. Tested (`StructureDelimiter=\x01`):
```
0:  <\x01<s>\x01\x01<s_n>\x01multi/P/L token/P/L> match/P/L\x01</s_n>\x01\x01</s>\x01 ...
```
Each tag is now framed by `\x01…\x01`; the token `x<s>y/MIDTAG/x` is not. With a TAB-based `StructureDelimiter` (see §8) this is unambiguous **for tag names**, but the *annotation body* inside the frame can still contain TAB (proved), so annotated s-attributes shown inline are never fully safe.

---

## 3. Match span and target/keyword anchors (Task 3)

- **LeftKWICDelim / RightKWICDelim**: settable to control chars, multi-char strings, or empty. Tested: `set LeftKWICDelim "\x02"; set RightKWICDelim "\x03"` → `…\x02multi/P/L token/P/L\x03 match…`; empty delims → match merges into context (`multi/P/L token/P/L match/P/L …`, no marker). Defaults `<`/`>`. These affect ascii mode only (source `options.c`).
- **`set ShowTargets on` produces NO marker in `cqp -c`.** Tested repeatedly (with `ShowTargets on`, `show +targets`, `set Highlighting on`) — the `(0)`/`(1)` parenthesised markers **never appear** over a pipe. Root cause (release-3.5.0 source): the plain `ASCIIPrintDescriptionRecord` has `printField = NULL` (`ascii-print.c`); anchor separators are emitted only by `ASCIIHighlightedPrintDescriptionRecord`, and `ascii_print_concordance_line` selects the highlighted PDR only when `apply_highlighting = interactive && highlighting`. Child mode is non-interactive, so target/keyword markers are structurally unreachable. Even interactively they are ANSI colour escapes (or `(N)` only when termcap lacks colour) wrapping tokens — not clean data.
- **Recover anchors via `dump` or `tabulate` instead.** `dump C` yields TAB-separated integers `match matchend target keyword`, `-1` for unset:
```
0	1	1	2
19	20	20	-1
```
This is the only reliable way to get target/keyword positions from a backend.

---

## 4. `set PrintStructures` with hostile values (Task 4)

**No escaping whatsoever** (source `compose_kwic_print_structures`, same identity `printToken`). Tested:
```
set PrintStructures "s_n";           -> 0: <s_n a>b>:  <multi/P/L token/P/L> match/P/L …
set PrintStructures "s_n, text_id";  -> 0: <s_n a>b><text_id t1>:  <multi/P/L …
```
- A value containing `>` (`a>b`) is emitted raw inside `<s_n a>b>`.
- Multiple structures are concatenated with **no separator** (`…><…`), so `><` is the only boundary marker (Korp splits on it and then repairs values that themselves contain `><`).
- A value containing a **raw TAB** (`HOSTILE2` s_n=`tab␉inside`) is emitted raw: `<s_n tab␉inside>: …` — this breaks any TAB-column scheme.
- A value ending in `>: ` (`x>: y`) yields `<s_n x>: y>: …`, colliding with Korp's fallback `line.split(">: ")`.
The PrintStructures block is terminated by `": "` (`AfterPrintStructures`) and the cpos prefix is `"%9d: "`, so with cpos on there are **two** `: ` runs plus a leading TokenSeparator before the match delimiter (`…>:  <multi…` — double space) because `add_sep_to_line_if_not_empty` fires when the prefix already filled the line.

---

## 5. Edge cases (Task 5)

- **Zero context** (`set Context 0 [words|characters]`, or bare `0`): only the match, e.g. `        0:  <multi/P/L token/P/L>`.
- **Match at corpus start** (cpos 0, 0 context): `        0:  <multi/P/L>` — left context empty, no crash.
- **Match at corpus end**: right context simply stops (`… multi/P/L token/P/L <end/P/L>`).
- **`set Context 1 s`** on a match inside a region: whole region rendered as context. On the 50 001-token `BIG` sentence: a **single 543 002-byte line**, produced in <0.1 s, **no truncation** — `cat` uses an auto-growing `ClAutoString` with no length cap. There is **no "line too long" truncation in `cat`**.
- **Line-length limits live elsewhere:** (a) `cwb-encode` caps any p- or s-value at **4095 characters** (`CL_MAX_LINE_LENGTH-1`), warning `exceeds maximum string length … truncated`; (b) Korp's `run_cwb_scan` drops any output line ≥ 65536 bytes.
- **Match not inside the context structure** (`GAP` corpus, `set Context 1 s` on a token outside every `s`): CWB falls back to a **±20-token window** (source `compose_kwic_line`: `start = match_start - 20`), padding missing positions with empty tokens (visible as trailing empty `…\tA\t\tT\t…` fields). Setting `Context N nonexistent-struct` silently degrades similarly.
- **CRASH / DoS (new finding).** With `ShowTagAttributes on` and an inline annotated s-attribute whose value approaches 4095 chars, `compose_kwic_token` executes `sprintf(body, "%s %s", region->name, region->annot)` into `static char body[CL_MAX_LINE_LENGTH]` (= 4096 bytes) (`concordance.c:229-232`). name + space + 4095-char annot overflows the buffer. Reproduced: `cqp -c` exits **133 (SIGTRAP, stack-smashing abort)** on `cat` of a `BIG`-style corpus with a 4095-char `s_n`; the same query with `ShowTagAttributes off` exits 0. **A crafted corpus can crash any backend that shows annotated structural tags inline.** File this upstream.

---

## 6. Korp reference parser — every edge case it defends against (Task 6)

From `korp/cwb.py` `run_cqp`, `korp/views/query.py` `query_corpus`/`query_parse_lines`, `korp/utils.py`:

1. **`reply.split("\n")`, never `splitlines()`** — explicit comment: *"We don't use splitlines() since it might split on special characters in the data"*. Correct and necessary: `str.splitlines()` also breaks on VT(0x0B), FF(0x0C), FS/GS/RS(0x1C–0x1E), NEL(0x85), LS/PS(U+2028/9) — all of which are legal p-attribute bytes (VT, FF, ESC proven present in the corpus). Only `\n` is a true record boundary.
2. **Match span via magic string tokens, not `<`/`>`.** `LEFT_DELIM="---:::"`, `RIGHT_DELIM=":::---"`, installed as `set LeftKWICDelim '---::: '; set RightKWICDelim ' :::---'` (surrounding spaces baked in) so `line.split()` isolates them and `if word == LEFT_DELIM` finds the match boundary. Sidesteps the default `<`/`>` colliding with tag/word `<`/`>`. **Residual fragility (proved):** a token whose word value is exactly `---:::` collides — in word-only output `cat` emits `---::: ---::: word :::---` and the context token is misread as match start. With ≥2 shown p-attrs the `/pos…` suffix protects it.
3. **`__UNDEF__` → `None`** via `translate_undef` (missing-column sentinel, default from `cwb-encode -U`).
4. **PrintStructures line split is heuristic and self-repairing:** try `rsplit(":  ", 1)` (two spaces), else `split(">: ", 1)`; then `lineattr[2:-1].split("><")` to separate structures, then a repair loop that re-joins fragments whose head isn't a known s-attr name (defends against `><` **inside** an annotation value). Still breaks on values containing `:  ` or `>: ` (our `x>: y`).
5. **Opening tags glued to a token start:** `while word[0]=='<'` loop distinguishes `<s_n VALUE>` (value continues into following whitespace-split fragments, collected until a `>` appears: `if ">" not in word: struct_value.append(word); continue`) from valueless `<s>`; strips the tag off the following token.
6. **Closing tags glued to a token end:** `while word[-1]=='>' and "</" in word: rsplit("</",1)`, verifying the stripped name is a known s-attr before accepting.
7. **`/` inside p-values:** `word.rsplit("/", nr_splits)` with `nr_splits = len(p_attrs)-1` — limits splits so leading attribute values containing `/` are not over-split. (A `/` in the *last, i.e. leftmost after rsplit* attribute value still corrupts the split; not fully safe.)
8. **Catch-all `except IndexError: continue`** — comment: *"Attributes containing '>' or '<' can make some lines unparseable. We skip them"* — Korp **silently drops** hits it cannot parse. Not 100% fidelity.
9. **`if "start" not in match: continue`** — comment: *"CQP bug - CQP can't handle too long sentences, skipping"* — a hit whose LEFT_DELIM was lost (huge region / truncation) is **dropped entirely**.
10. **`run_cwb_scan` line guard** `len(line) < 65536`.
11. **Error suppression** in `run_cqp`: ignores `No such attribute:`, `is not defined for corpus`, empty-result assertion `cl->range && cl->size > 0`, and `invalid UTF8 string passed to cl_string_canonical`.
12. **`read_attributes`** parses `show cd` output (`p-Att␉word␉␉*`, `s-Att␉s_n␉-V␉`) via `(line+" X").split(None,2)` keyed on the first-column type.

**Conclusion:** Korp's own parser is explicitly heuristic and lossy — it drops unparseable lines (8, 9) and its match-delimiter and attribute splitting are defeatable by crafted values (2, 4, 7). It is *not* a 100%-fidelity reference; it is a best-effort parser that trades faithfulness for robustness.

---

## 7. `tabulate` as a replacement for `cat`, and the definitive forbidden-byte set (Task 7)

**`tabulate` grammar, from source `output.c` `print_tabulation`:**
- Columns are separated by a **hardcoded TAB** (`if (item->next) fprintf(dst->stream, "\t")`).
- Tokens **inside a range column** `a..b` are separated by a **hardcoded single space** (`if (cpos < end) fprintf(dst->stream, " ")` — comment: *"tokens in a range item are separated by blanks"*). **Neither separator is configurable.**
- Rows end in `\n`. Values are printed with `fprintf("%s", …)` — **no escaping**.
- An undefined anchor (target/keyword = -1) prints a single `-1` (numeric) or empty string (attribute) for the whole range. Out-of-bounds cpos (via offset) prints `-1`/empty but **still emits the interior spaces** (`multi token end    ` / `      `).

Tested behaviours:
```
tabulate C match word, matchend word;      -> multi␉token          (single-anchor cols: TAB-separated, unambiguous)
tabulate E match..matchend word;           -> a/b x|y New York vt␋vt   (RANGE: New York's space makes 4 tokens look like 5)
tabulate C keyword word;                    -> "match" / ""         (unset keyword -> empty)
tabulate C match s_n;                       -> a>b / probes         (s-attr values, unescaped)
```

**Verdict on `tabulate`:** It **can** carry per-token, multi-attribute data with contexts, and single-anchor columns are perfectly safe (TAB between columns, `\n` between rows — bytes that can't occur in p-values). **But range columns (`match..matchend`, `match[-N]..match[-1]`) space-join their tokens, and p-attribute values legally contain spaces** (`New York` proven). There is no escaping and no way to change the space, so **range columns are irrecoverably ambiguous for space-containing tokens.** `tabulate` therefore *cannot* be a drop-in `cat` replacement for variable-width context in one command.

**Definitive: which bytes are impossible in a p-attribute value.** Only **TAB (0x09)** and **newline/LF (0x0A)** — the `.vrt` column and line separators — are structurally forbidden, plus **NUL (0x00)**, which `cwb-encode` silently truncates the value at (C-string terminator; tested `a\x00b` → stored `a`, 1 byte). **Everything else round-trips:** CR (0x0D), VT (0x0B), FF (0x0C), BEL (0x07), ESC (0x1B), space, `/`, `|`, `<`, `>`, `"`, `&`, and all UTF-8 — all verified stored and re-decoded byte-for-byte. Consequently **TAB and LF are the only two bytes CWB can never emit inside a token value**, which is exactly why they are the only trustworthy delimiters. **S-attribute annotation values are strictly more hostile:** LF is impossible (the tag must be one line) but **TAB is allowed** (verified: `<s n="tab␉inside">` stored and rendered with a raw TAB) — so annotations cannot be safely carried in any TAB-delimited stream without last-field/`rsplit(maxsplit=…)` handling.

---

## Deliverable: verdict and tested full-fidelity recipe

**`cat` cannot be made unambiguous in the general (hostile) case.** A TAB-framed separator profile makes the *token / p-attribute / tag-name* layer unambiguous, but three defects are unfixable from `cat` options alone: (a) inline annotation values can contain raw TAB and are unescaped (breaks any TAB scheme; also can **crash** CQP, §5); (b) target/keyword markers are unavailable over a pipe (§3); (c) `PrintStructures` values are unescaped and can contain TAB / `>: ` / `><` (§4). Use `cat` only for human display, never as a parse source for adversarial corpora.

**Hardened `cat` profile** (best `cat` can do — safe for the p-attribute layer and tag *names* only; do **not** show annotated s-attributes inline):
```
set PrettyPrint off;
set AttributeSeparator "␉A␉";   set TokenSeparator "␉T␉";
set StructureDelimiter "␉S␉";   set LeftKWICDelim "␉L␉";  set RightKWICDelim "␉R␉";
set ShowTagAttributes off;      set PrintStructures "";     show -targets;
```
(each `␉` = a raw TAB byte; TAB is impossible in p-values, so the frames are unforgeable there). Parse: split the line on `\n` (never `splitlines`), split fields on `␉`, and the framing tokens `A T S L R` mark attribute/token/structure/left/right boundaries deterministically. Recover match/target/keyword and any annotation values **out of band**.

**Recommended 100 %-faithful pipeline (dump + decode; tested, hostile-proof):**
1. Run the query in `cqp -c`; `dump Last;` → per hit `match␉matchend␉target␉keyword` as integers (`-1` = unset). Integers cannot be corrupted by any content.
2. Read positional data by **integer cpos**, not by parsing KWIC text:
   - Whole-corpus token table once: `cwb-decode -C -r <REG> <CORPUS> -P word -P pos -P lemma` → strictly `word␉pos␉lemma\n` per token (TAB between attributes, `\n` between tokens — the two bytes that can never occur in a value). Build a cpos→token array (cpos = line index over the token lines) and slice the ranges from step 1. For structural context boundaries, add `-S s` / `-V s_n` to the same `cwb-decode` run (tags appear on their own lines; `<s_n a>b>` inline) or use `cwb-s-decode <CORPUS> -S s_n` → `start␉end␉value\n` and recover the value with `split('\t', 2)` / `rsplit(maxsplit=?)` so an embedded TAB in the annotation stays intact (LF is impossible there).
   - Or, for a fixed window without a full decode: `tabulate Last match[-K] word, match[-K] pos, …, matchend[+K] word, …;` using **single-anchor columns only** (never `a..b` ranges) — TAB-separated, `\n`-terminated, fully unambiguous; add a parallel `… match[-K]` (no attribute) column per token to get its cpos and distinguish an empty-string value from an out-of-bounds `-1`.
3. Never rely on `cat` for target/keyword anchors, for annotated s-attribute values, or for any corpus you do not control.

This decode-by-cpos approach is faithful precisely because it addresses tokens by integer position and transports values only over TAB/LF-delimited channels — the unique byte pair that CWB guarantees absent from positional-attribute values.

### Primary sources
- CQP Manual §7.1 (separators, control-char rule): https://cwb.sourceforge.io/files/CQP_Manual/7_1.html
- CQP Manual §2.3 (KWIC/`cat` display options): https://cwb.sourceforge.io/files/CQP_Manual/2_3.html
- CQP release-3.5.0 source: `concordance.c` (line grammar, `body[4096]` overflow at L229-232), `ascii-print.c` (identity `printToken`, `printField=NULL` in plain PDR), `output.c` (`print_tabulation` separators), `options.c` (option table/defaults), `parser.y` (`.EOL.` → `-::-EOL-::-` at L523-525) — https://svn.code.sf.net/p/cwb/code/cwb/release/3.5.0/cqp/
- Korp backend: `korp/views/query.py` (`query_corpus`, `query_parse_lines`), `korp/cwb.py` (`run_cqp` `split("\n")`, `run_cwb_scan` 65536 guard, `read_attributes`), `korp/utils.py` (`LEFT_DELIM`/`RIGHT_DELIM`, `translate_undef`, `END_OF_LINE`) — https://raw.githubusercontent.com/spraakbanken/korp-backend/master/korp/{views/query.py,cwb.py,utils.py}

Reproduction workspace (scripts, `.vrt`, corpora, per-experiment transcripts): `/private/tmp/claude-501/-Users-rqf595-Code-corpus-probe/a3908530-2a88-4d2f-9f1a-81ac8d36e642/scratchpad/cqp_probe_x9/` (`cqpdrv.py` = the sentinel driver; `make_vrt*.py`; `exp1.py`–`exp10.py`).