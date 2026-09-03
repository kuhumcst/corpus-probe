All research is complete. Compiling the report.

# Driving CQP programmatically: invocation modes, the child-mode line protocol, CQi, and the exact shape of terminal output

All sample outputs below were generated empirically for this report with CQP **3.5.0** (Homebrew build, macOS, compiled 2026-07-05) against a purpose-built toy corpus (p-attributes `word`, `pos`, `lemma`; s-attributes `s`, `s_n`, `text`, `text_id`, `text_author`; charset utf8), and cross-checked against the CQP C source in the CWB SVN trunk (https://svn.code.sf.net/p/cwb/code/cwb/trunk/cqp/) and the CWB::CQP Perl module v3.5.0 (https://metacpan.org/dist/CWB/source/lib/CWB/CQP.pm, raw: https://fastapi.metacpan.org/v1/source/SCHTEPF/CWB-v3.5.0/lib/CWB/CQP.pm). In the samples, `<TAB>` marks a literal tab character; everything else is byte-exact.

## 1. Invocation modes

`cqp -h` (3.5.0) lists the relevant flags: `-r dir` (registry), `-l dir` (subcorpus/data directory), `-I file` (init file), `-M file` (macro file), `-m` (disable macros), `-e` (readline editing), `-C` (ANSI colours, "experimental"), `-f filename` (batch mode), `-p` (pager off), `-P pager`, `-s` (auto subquery), `-c` (child process mode), `-i` ("print matching ranges only (binary output)"), `-W/-L/-R num` (context width in chars), `-D corpus` (default corpus), `-b num` (hard boundary for Kleene star), `-x` (insecure mode when SETUID), `-d mode` (debug modes).

- **Interactive** (no flags): prints a prompt `[no corpus]> ` / `TINY> ` on stdout, pages results through `less -FRX -+S` (default Pager) unless `-p`, and — unlike child mode — **auto-displays the concordance immediately after executing a query**, without an explicit `cat`. Empirically, piping `TINY;\n"lazy";\ncat;` into interactive cqp prints the KWIC block twice (once from the query, once from `cat`).
- **Batch (`-f file`)**: executes the file's commands, writes results to stdout, errors to stderr, prints **no version banner**, does **not** stop on ordinary errors (a bogus command mid-file produced `CQP Error:` on stderr and the remaining commands still ran), and exits 0 even after errors. `-f` may be combined with `-c` to get the banner and `.EOL.` markers in a batch run. Parse errors while reading an **init** file (`-I`) are fatal (`exit(cqp_error_status ? … : 1)`, cqp.c).
- **Child mode (`-c`)**: the mode all wrappers use; see §2.
- **`-i`**: prints each match as a raw binary pair of 32-bit ints (match, matchend) in native byte order — on this little-endian machine, matches at cpos 7, 11, 28 emitted bytes `07 00 00 00 07 00 00 00 0b 00 00 00 0b …`. [Byte order is native/platform-dependent — unverified across platforms.]
- **`cqpserver`**: separate binary implementing the CQi protocol; see §3.

## 2. Child mode (`-c`): the exact line protocol

The authoritative in-source description is the comment block on `int child_process` in `options.c` (https://svn.code.sf.net/p/cwb/code/cwb/trunk/cqp/options.c):

> Child process mode (used by Perl interface (CQP.pm) and by CQPweb (cqp.php))
> - don't automatically read in user's .cqprc and .cqpmacros
> - print CQP version on startup
> - now: output blank line after each command -> SHOULD BE CHANGED
> - command ".EOL.;" prints special line (``-::-EOL-::-''), which parent can use to recognise end of output
> - print message CQP_PARSE_ERROR_LINE i.e. "PARSE ERROR" on STDERR when a parse error occurs (which parent can easily recognise)

The `-c` case in `parse_options()` additionally sets `silent = True`, `pretty_print = paging = highlighting = False`, `autoshow = auto_save = False`, and `progress_bar_child_mode(1)` (machine-readable progress lines). So `-c` already disables PrettyPrint; wrappers still send `set PrettyPrint off` defensively (CWB::CQP constructor, cwb-ccc, CQPweb, korp all do).

### Startup banner
The first line on stdout is a version banner. Exact 3.5.0 output: `CQP version 3.5.0`. All three major wrappers parse it with the same regex (CWB::CQP line 162; cwb-ccc `ccc/cqp.py`; CQPweb `VERSION_REGEX`):

```
^CQP\s+(?:\w+\s+)*([0-9]+)\.([0-9]+)(?:\.b?([0-9]+))?(?:\s+(.*))?$
```

i.e. optional words between `CQP` and the version, optional `.b<n>` beta revision, optional trailing compile date.

### Command dispatch and the `.EOL.` marker
CQP commands are `;`-terminated; newlines are insignificant except that `#` starts a comment to end of line. There is **no per-command framing in the protocol itself** — the wrapper creates framing by sending the pseudo-command `.EOL.;` after each real command. `.EOL.` is a grammar token (`EOL_SYM`, parser.y line 287); its action is exactly (parser.y ~line 523):

```c
EOLCmd: EOL_SYM  { printf("-::-EOL-::-\n"); fflush(stdout); }
```

So the end-of-output marker is the line `-::-EOL-::-` (nothing else on the line). It is identical in every PrintMode (verified in ascii, sgml, html, latex). Grammar comment: "`.EOL.` must be allowed in query lock mode" — so the marker works inside QueryLock too.

How the three main wrappers write a command:
- **CWB::CQP** (`run()`, lines 440–457): strips newlines from the command (`s/\n+/ /g`), strips trailing `;`, then writes `"$cmd;\n .EOL.;\n"` — the marker command on its own line, preceded by a space.
- **cwb-ccc** (`Exec()`): writes `cmd + ';\n;.EOL.;\n'` (an extra empty command before the marker).
- **CQPweb** (`execute()`, lib/cqp.php): converts tabs to spaces, strips one trailing `;`, then writes `"$command\n; .EOL. ;\n"` with this comment: "an \n is added between the command and the terminator, to work around an issue where a stray # in the last line will cause CQP to hang rather than report an error (because the terminating ; becomes part of the comment)". A parser MUST put the `.EOL.;` on its own line for the same reason.

Reading: collect stdout lines until the exact line `-::-EOL-::-`; everything before it is the command's output. CWB::CQP skips **empty lines** (`next if $line eq ""`, `_update()`) because older CQP versions emitted a blank line after each command in child mode (see the "SHOULD BE CHANGED" comment and the legacy `printf("\n"); /* so CQP.pm won't block */` still present in the unlock-violation path of parser.y). In 3.5.0 no blank lines are emitted in normal operation, but a robust parser should still skip them. Multiple commands may share one write; each `.EOL.;` produces exactly one marker (verified: 8 commands with 8 `.EOL.;` → exactly 8 markers even when 4 of the commands failed).

### Buffering
stdout is set **unbuffered** unconditionally in `main()` (cqp.c line ~199: `setvbuf(stdout, NULL, _IONBF, 0)`), and in child mode CQP additionally calls `fflush(stdout); fflush(stderr);` after every parse pass (cqp.c lines 420–423). Verified empirically: with stdin held open via a FIFO, each command's output plus marker arrived within <1 s without closing stdin. Also from cqp.c line 390: "in child mode, abort on read errors (to avoid hang-up when parent has died etc.)" — CQP exits when its stdin reaches EOF or errors.

### Error signalling
There is no error channel in stdout; **any output on stderr means the last command failed**. That is the entire error protocol, and it is what CWB::CQP implements (`_update()`: "any output on stderr indicates that something went wrong", detected via `IO::Select`/`select(2)` on the stderr fd; cwb-ccc `Checkerr()` uses `select.select` the same way; CQPweb `check_pipe_for_error()` uses `stream_select`). Exact formats, from `cqpmessage()` in output.c — `fprintf(stderr, "%s:\n\t", msg)` followed by the message and `\n`:

```
CQP Error:
<TAB>Corpus ``NOSUCHCORPUS'' is undefined
```

A syntax error additionally echoes the offending input with a ` <--` pointer and two parser-emitted lines (parser.y lines 115/120; cqp.c line 417 prints the `PARSE ERROR` sentinel, macro `CQP_PARSE_ERROR_LINE "PARSE ERROR\n"` in cqp.h line 35):

```
CQP Error:
<TAB>CQP Syntax Error: syntax error, unexpected ID, expecting ';'
<TAB>
"lazy" foo  <--
Ignoring subsequent input until next ';'...
PARSE ERROR
```

(Interactive mode says "until end of line" instead of "until next ';'".) Because recovery skips to the next `;`, the following `.EOL.;` still executes — the marker is emitted **even after errors**, so a parser can always resynchronise on `-::-EOL-::-`. Caveat: continuation lines of an error message are not always tab-indented (the invalid-encoding error's second line starts at column 0), so treat the whole stderr blob between commands as one message rather than parsing its shape. Note also that some failing commands still print to stdout: `size Undefined` prints `0` on stdout *and* the error on stderr. The process exit code is 0 even after errors.

### Options relevant to the protocol
- `set PrettyPrint off;` — belt-and-braces (already off under `-c`); the manual (§7.1, https://cwb.sourceforge.io/files/CQP_Manual/7_1.html): "The output of many CQP commands is neatly formatted for human readers; this pretty printing feature can be switched off".
- `set ProgressBar on;` — in child mode progress is line-oriented (manual §7.1: progress messages "printed in separate lines on stdout"). Exact format (verified on a 4M-token corpus), TAB-separated: marker, pass number, total passes, message:

```
-::-PROGRESS-::-<TAB>1<TAB>1<TAB>    preparing
-::-PROGRESS-::-<TAB>1<TAB>1<TAB>  0% complete
-::-PROGRESS-::-<TAB>1<TAB>1<TAB> 37% complete
```

CWB::CQP matches `/^-::-PROGRESS-::-/`, splits on `\t`, and extracts the percentage with `/([0-9]+)\%\s*complete/` (the message field can also be free text like `    preparing`); CQPweb's `PROGRESSBAR_REGEX` is `'/^-::-PROGRESS-::-/'`. These lines are interleaved with (i.e. precede) the command's real output and must be filtered out.
- `set QueryLock <key>;` / `unlock <key>;` — see §5.
- Shutdown: CWB::CQP and CQPweb write `exit` and close the pipe; sending `exit;` mid-stream terminates CQP immediately with exit code 0 and **no marker** (verified), which is why CWB::CQP's POD warns never to `exec("exit")` — its SIGCHLD handler treats unexpected child death as fatal ("not safe to continue").

## 3. cqpserver and the CQi protocol

**What it is.** Per https://cwb.sourceforge.io/documentation.php: "The corpus query interface (CQi) is a remote client-server API that provides low-level corpus access as well as (almost) complete CQP functionality", aimed at "CWB-based development in programming languages which cannot easily be linked to C libraries or run an interactive CQP backend". The CWB core ships `cqpserver`, which implements CQi over TCP.

**Spec location.** https://cwb.sourceforge.io/files/cqi_spec.zip — contains `cqi.spec` ("IMS CQi specification, Version 0.1a ;o)", Stefan Evert, 2000), a generator script, and constant-definition headers for C (`cqi.h`), Python, Perl, Java. Tutorial: https://cwb.sourceforge.io/files/cqi_tutorial.pdf ("CQi v1.0 alpha", 2001-03-09). The site adds: "The CQi specification as it stands applies to Versions 3.0 to 3.5 of CWB. You may confidently expect things to be fundamentally different in CWB 4.0."

**Wire format** (cqi_tutorial.pdf "DATA TYPES" + the reference client `CWB::CQI::Client`, https://metacpan.org/dist/CWB-CQI, raw: https://fastapi.metacpan.org/v1/source/SCHTEPF/CWB-CQI-v3.5.0/lib/CWB/CQI/Client.pm):
- BYTE: unsigned 8-bit; BOOL: BYTE 0/1.
- WORD: unsigned 16-bit big-endian (Perl `pack "n"`); command and response codes are WORDs interpreted as two BYTEs — a group byte and a command byte (e.g. group `15` = `CQI_CQP`, `15:01` = `CQI_CQP_QUERY`).
- INT: signed 32-bit, network (big-endian) byte order (`pack "N"` with sign conversion).
- STRING: `WORD n` length prefix + n bytes, not NUL-terminated, must not contain NUL (so max 65535 bytes).
- Lists: `INT n` followed by n elements.
- A call is: command WORD, then arguments in order; response is a status/error WORD (`01:xx CQI_STATUS_*`, `02:xx CQI_ERROR_*`, `04:xx CQI_CL_ERROR_*`, `05:xx CQI_CQP_ERROR_*`) or a data code (`03:xx CQI_DATA_*`) followed by the payload.
- Key commands: `CQI_CTRL_CONNECT(user, password)`, `CQI_CORPUS_LIST_CORPORA()`, `CQI_CL_*` (str2id/id2str/cpos2str/regex2id/struc2cpos …), `CQI_CQP_QUERY(mother_corpus, subcorpus_name, query)`, `CQI_CQP_SUBCORPUS_SIZE`, `CQI_CQP_DUMP_SUBCORPUS(subcorpus, field BYTE, first INT, last INT) -> INT_LIST`, `CQI_CQP_FDIST_1/2`, `CQI_CQP_DROP_SUBCORPUS`.

**Server invocation/auth.** `cqpserver -h`: `-1` (exit after one connection), `-P port` (default `CQI_PORT` = 4877 [unverified — from CQi headers]), `-L` (loopback only), `-q` (fork and quit), plus `user:password` pairs as positional args. Credentials/ACLs can also be declared in the init file via CQP's authorization grammar (parser.y `AuthorizeCmd`): `user <id> "<password>"` with optional per-corpus grants, and `host <ip>|<subnet>|*`.

**Clients.**
- Perl: `CWB::CQI` (reference implementation, ships with CWB/Perl).
- Python: `cqi` on PyPI (https://pypi.org/project/cqi, repo https://github.com/Pevtrick/cqi-py, by Patrick Jentsch; used by the nopaque platform; v0.1.7, low activity).
- Clojure/Java: `cqp-clj` (https://github.com/emanjavacas/cqp-clj), "Clojure/Java implementation of a CQP client following the CQi specification".
- R: PolMine's experimental `cqi` package (https://rdrr.io/github/PolMine/cqi/f/); the archived CRAN `rcqp` embedded the CWB C code directly (https://github.com/cran/rcqp).
- Note: **cwb-ccc is not a CQi client** — it drives `cqp -c` (see its `ccc/cqp.py`).

**Maturity vs `cqp -c`.** The spec is a 2000/2001 "v1.0 alpha"/"0.1a" document that was never revised; the heavyweight production frontends avoid it: CQPweb talks to `cqp -c` via PHP `proc_open` (lib/cqp.php), Korp spawns `cqp -c` per request (korp/cwb.py), cwb-ccc wraps `cqp -c`. CQi lacks most display/output machinery (no KWIC formatting, no tabulate/count; frequency support limited to FDIST_1/2), and CWB 4.0 is expected to replace it. Practical conclusion: `cqp -c` is the battle-tested route; CQi is attractive mainly for network transparency and typed (non-text) results.

## 4. Display options, with exact outputs

Defaults, from the full `set;` listing in 3.5.0 child mode (TSV: abbreviation, name, value): `Context/LeftContext/RightContext = 25 characters`, `LeftKWICDelim = <`, `RightKWICDelim = >`, `PrintMode = ascii`, `AttributeSeparator/TokenSeparator = <default>`, `StructureDelimiter = <none>`, `PrintOptions = -tbl -hdr -wrap -bdr -num`, `PrintStructures = <no value>`, `ShowTagAttributes = yes`, `Colour = no`, `ProgressBar = no`, `PrettyPrint = no` (off because of `-c`).

Baseline (`TINY; "lazy"; cat;`, defaults):

```
        7:  brown fox jumps over the <lazy> dog . A lazy cat sleeps o
       11: mps over the lazy dog . A <lazy> cat sleeps on the old mat
       28:  at the quick cat . Every <lazy> dog has its day .
```

### show +attr / -attr (p-attributes)
`show +pos +lemma;` interleaves attribute values on every token, joined by AttributeSeparator (default `/`):

```
        7: p over/IN/over the/DT/the <lazy/JJ/lazy> dog/NN/dog ./SENT/. A/DT/
```

`show -cpos;` removes the entire `%9d: ` prefix — lines then begin directly with the (possibly space-leading) left context (manual §2.3, https://cwb.sourceforge.io/files/CQP_Manual/2_3.html, verified). `show +s` / `show +text` display s-attribute regions as inline tags `<s>…</s>` within the context; an s-attribute that carries annotation values (e.g. `s_n` from encoding `-S s:0+n`) prints its value in the open tag: `<s_n 2>A lazy…</s_n>`. `set ShowTagAttributes off;` suppresses the value: `<s_n>A lazy…`. (In 3.5.0 the *bare* region attributes `s`/`text` never showed values, because CWB stores XML attributes as separate s-attributes `s_n`, `text_id`; display the annotated one to see values.) `show cd;` emits the attribute inventory as TSV — `p-Att<TAB>word<TAB><TAB>*` / `s-Att<TAB>s_n<TAB>-V<TAB>` (`-V` = has values; `*` marks currently displayed) — Korp parses exactly this (`show cd; .EOL.;` in korp/cwb.py `show_attributes()`).

### Context / LeftContext / RightContext
Units: bare number = characters; `N words` = tokens; `N <s-attr>` = structural units; bare s-attr name = enclosing region (manual §2.3). Verified:
- `set Context 3 words;` → exactly 3 tokens each side: `        7:  jumps over the <lazy> dog . A`
- `set LeftContext 10 chars; set RightContext 2 words;` → left context truncated to exactly 10 characters, mid-word: `        7: s over the <lazy> dog .`
- `set Context 1 s;` → whole sentence(s): `        7: <text_id t1><s_n 1>:  The quick brown fox jumps over the <lazy> dog .`
- `set Context s;` (no number) → enclosing sentence, no truncation.
Character-based context cuts words without any ellipsis mark, and the left context is padded/preceded by a single space after the `:` separator.

### LeftKWICDelim / RightKWICDelim
`set LeftKWICDelim "<<"; set RightKWICDelim ">>";` → `… over the <<lazy>> dog …`. Defaults `<` and `>`. These wrap only the match (and are also used for `<target>`-style anchors via ShowTargets [unverified]).

### AttributeSeparator / TokenSeparator
`set AttributeSeparator "|";` → `the|DT|the <lazy|JJ|lazy> dog|NN|dog`. `set TokenSeparator " || ";` replaces the single-space token joiner: `the|DT|the || <lazy|JJ|lazy> || dog|NN|dog`. Manual §7.1 explicitly advises parsers to set these to characters "disallowed in attribute values, e.g. TAB (#9), BEL (#7) or ESC (#27)" because the default `/` collides with attribute values. `set StructureDelimiter "##"` produced no visible change in 3.5.0 ascii output [effect unverified].

### set PrintStructures
`set PrintStructures "text_id, s_n";` inserts, between the cpos field and the context, each structure as `<name value>` joined by a single space, terminated by `: ` (source of truth: `ASCIIPrintDescriptionRecord` in ascii-print.c — `PrintStructureSeparator = " "`, `AfterPrintStructures = ": "`):

```
        7: <text_id t1><s_n 1>:  brown fox jumps over the <lazy> dog . A lazy cat sleeps o
```

### PrintMode
`set PrintMode ascii|sgml|html|latex;` (abbrev `pm`). Same query, `set PrintMode sgml; cat;`:

```
<CONCORDANCE>
<attribute type=positional name="word" anr=0>
<LINE><MATCHNUM>7</MATCHNUM><CONTENT><TOKEN></TOKEN> <TOKEN>brown</TOKEN> <TOKEN>fox</TOKEN> <TOKEN>jumps</TOKEN> <TOKEN>over</TOKEN> <TOKEN>the</TOKEN> <MATCH><TOKEN>lazy</TOKEN></MATCH> <TOKEN>dog</TOKEN> <TOKEN>.</TOKEN> …</CONTENT></LINE>
</CONCORDANCE>
```

(One `<LINE>` per match, single physical line; char-based context still truncates, producing empty/partial `<TOKEN>` elements; if `show +s` is active, tags are entity-escaped and **glued into the neighbouring token**, e.g. `<TOKEN>&lt;s&gt;A</TOKEN>` — do not combine `show +s` with sgml mode if you want clean tokens.)

`set PrintMode html; cat;` (default options):

```
<HR><UL>
<LI><EM>7:</EM> brown fox jumps over the <B>lazy</B> dog . A lazy cat sleeps o
<LI><EM>11:</EM>mps over the lazy dog . A <B>lazy</B> cat sleeps on the old mat
</UL>
<HR>
```

`set PrintMode latex; cat;`:

```
\begin{itemize}
\item {\em 7:\/}  brown fox jumps over the  {\bf lazy}  dog . A lazy cat sleeps o
\end{itemize}
```

(LaTeX specials are escaped: with PrintStructures, `{\sf $<$text\_id t1$>$}`.)

### PrintOptions
Five flags — `tbl` (table), `hdr` (header), `wrap`, `bdr` (border), `num` (line numbers) — shown in `set;` as `-tbl -hdr -wrap -bdr -num` when all off; `set PrintOptions hdr` switches one on (options accumulate). Effects verified:
- `num` (ascii): prefixes each KWIC line with a 1-based result-row number, right-aligned width 6 + `.` + TAB: `     1.<TAB>        7:  brown fox …`
- `hdr` (ascii): emits a `#`-prefixed header block before the concordance:

```
#---------------------------------------------------------------------------
#
# User:    <unknown> (<unknown>)
# Date:    Thu Sep  3 09:16:43 2026
# Corpus:  tiny ()
# Name:    TINY:Last
# Size:    3 intervals/matches
# Context: 25 characters left, 25 characters right
#
# Query: TINY;  "lazy";
#---------------------------------------------------------------------------
```

- `tbl`/`bdr`/`wrap` are no-ops in ascii mode (verified); in html mode `tbl` switches `<UL>/<LI>` to a real table, one cell each for cpos, left, match, right:

```
<HR><TABLE>
<TR><TD ALIGN=RIGHT nowrap>7:</TD><TD nowrap ALIGN=RIGHT>  brown fox jumps over the </TD><TD nowrap><B>lazy</B></TD><TD nowrap> dog . A lazy cat sleeps o</TD></TR>
</TABLE>
<HR>
```

and html `hdr` emits a `<table>`-based "This concordance was generated by:" block.

### Toggles
`set Colour on` produced no ANSI escapes in child-mode output (colour requires a terminal and/or the experimental `-C` flag); `Highlighting` is forced off by `-c`. `set Timing on` exists (row in `set;`). `set AutoShow on` re-enables interactive-style auto-display of query results [effect in child mode unverified].

## 5. The ASCII KWIC line grammar, and cat/size/dump/tabulate/redirection

From `ASCIIPrintDescriptionRecord` (ascii-print.c lines 61–88) plus observation, an ascii-mode `cat` line is:

```
line        := [rowNum]  cposField  [printStructs]  leftCtx  LKD match RKD  rightCtx
rowNum      := printf("%6d.\t", 1-based row)          # only with PrintOptions num
cposField   := printf("%9d: ", cpos of match start)   # absent after 'show -cpos'
printStructs:= ("<" name " " value ">")* ": "         # only with set PrintStructures; joined by " "
leftCtx     := tokens joined by TokenSeparator (default " "); truncated LEFT-edge if char context
match       := tokens between LeftKWICDelim (default "<") and RightKWICDelim (default ">")
token       := attr values joined by AttributeSeparator (default "/") in 'show' order (word first)
```

Parser-critical facts: the leading number is the **corpus position** (cpos), not a row index, right-aligned in a 9-character field (`"%9d: "` — CPOSPrintFormat), so cpos ≥ 10⁹ would widen the field; a single space follows the `:`, and with character-based context the left context often begins with a further space or a word fragment; inline structure tags from `show +s` appear inside contexts (`… dog .</s> <s>A …`); the only reliable way to split token attributes is to set AttributeSeparator/TokenSeparator to control characters (manual §7.1). Empty results: `cat` on a 0-match query prints nothing (marker only, no error).

- **`size`**: `size A;` prints one line with the match count (`3`). `size` on a **system corpus** prints `1` (one interval), not the token count — use `info;` for that (`Size: 34`, plus `Charset: utf8` — the line CWB::CQP's `activate()` parses with `/^Charset:\s+(\S+)$/`). `size Undefined;` prints `0` on stdout *and* `CQP Error: Corpus ``Undefined'' is undefined` on stderr.
- **`cat` with range**: `cat A 1 2;` prints rows 1..2 (0-based, inclusive) — CQPweb pages results as `cat Last 0 49`, etc. Out-of-range ends are clamped silently; an inverted range prints nothing. No error in any of these cases.
- **`dump`**: `dump A;` → one TSV row per match, exactly four TAB-separated columns match/matchend/target/keyword, `-1` for unset anchors:

```
7<TAB>7<TAB>-1<TAB>-1
11<TAB>11<TAB>-1<TAB>-1
```

`dump A 0 1;` restricts to rows 0..1. `undump` reads the inverse format (first line = row count, then rows) from a file or pipe: CWB::CQP writes a gzipped temp file and runs `undump $nqr with target keyword < 'gzip -cd $tempfile |'`; cwb-ccc uses a plain temp file `undump name < "file";`.
- **`tabulate`**: `tabulate A match word, match pos, matchend[1] lemma;` → TSV, one row per match, one column per spec: `lazy<TAB>JJ<TAB>dog`. Range specs produce space-joined values within one column: `tabulate A 0 1 match[-1]..matchend[1] word, match lemma;` → `the lazy dog<TAB>lazy`. Manual §7.3 (https://cwb.sourceforge.io/files/CQP_Manual/7_3.html): ranges print values "separated by blanks rather than TABs"; an optional `first last` row range precedes the specs.
- **`count`**: `count A by word;` → `3<TAB>0<TAB>lazy` = frequency TAB index-of-first-line-in-(re-sorted)-result TAB string (manual §7.3: "frequency TAB first line TAB string"). **`group`**: `group A match lemma;` → `lazy<TAB>3` = value TAB frequency (two-key form: `value1 TAB value2 TAB freq`).
- **Redirection**: any of these commands accepts `> "target"` (and `>> "append"` [unverified]) where target is a filename in double quotes: `tabulate A match word > "/path/out.txt";` and `dump A > "/path/dump.tsv";` write files with the same formats as stdout and emit nothing to stdout. A target beginning with `|` is a **shell pipeline**: `cat A > "| wc -l > /tmp/x";` executed `wc` (verified) — CQPweb itself uses `dump $subcorpus > "| awk -F '\t' '{print \$2 - \$1 + 1}' | sort -rnu | head -1"` to compute max match length. Input redirection `< "file"` / `< 'cmd |'` works for `undump` and `define $list < "file"`.

## 6. Practical pitfalls

**Encoding.** CQP does no transcoding: output is in the corpus's own charset. A latin1 corpus emitted raw byte `0xE9` for `é` (verified with `od`). Read the charset from `info;` (`Charset:` line) and decode per corpus — exactly what CWB::CQP managed mode (`activate()` + `Encode`) and CQPweb (`filter_input`/`filter_output` via `iconv`, utf8 at the API boundary) do. Two asymmetric failure modes (both verified): sending UTF-8 bytes to a latin1 corpus is **silent** (valid latin1 mojibake, 0 matches, no error); sending invalid UTF-8 to a utf8 corpus raises `CQP Error: Query includes a character or character sequence that is invalid in the encoding specified for this corpus`. CWB::CQP encodes outgoing commands with `Encode::FB_PERLQQ` (invalid chars → escapes) to avoid crashing the pipe. Korp decodes output with `errors="ignore"` and deliberately splits on `"\n"` rather than `splitlines()` "since it might split on special characters in the data" (korp/cwb.py) — corpus tokens can contain `\x0b`, `\x0c`, `\u2028` etc. that `splitlines()` treats as line breaks.

**Buffering.** Not an issue on CQP's side (unbuffered stdout + explicit flushes, §2); the classic bug is on the client side — failing to flush the command pipe (every wrapper sets autoflush) or reading stdout and stderr from the same thread without `select`, which deadlocks when a huge error blob fills the stderr pipe. All three wrappers `select()` on stderr; CQPweb reads at most `MAX_SLV_ERRS_STORED` = 1024 error lines and notes that "segfaults leave stream_select() still reporting more stuff" on stderr.

**Long-running queries.** Child mode gives no in-band cancel. Options actually used: (1) **SIGINT** — sending SIGINT to a `cqp -c` evaluating a query prints `** Aborting evaluation ... (press Ctrl-C again to exit CQP)` on stderr, stops evaluation, and the process stays alive with the named query holding the **partial** result found so far (verified: 18432 partial matches; a second SIGINT exits — handler in cqp.c `sigINT_signal_handler`); (2) watchdog kill — cwb-ccc runs a thread that `kill -9`s the process after 900 s (`CMAXREQUESTPROCTIME`); Korp uses `Popen.communicate(timeout=1)` in a loop and kills the process tree on an abort event; (3) CWB::CQP exposes non-blocking `run()`/`ready($timeout)`/`getline` plus ProgressBar callbacks so the caller can implement its own timeout. Set `set ProgressBar on` to get liveness signals during evaluation.

**CQP dying on hard errors.** Wrappers must monitor the child: CWB::CQP installs a SIGCHLD handler that aborts the whole script if CQP dies unexpectedly, and traps SIGPIPE. Verified hard-death case: `unlock` with the wrong key prints `ALERT! Query lock violation.` to stderr, a bare newline to stdout, and `exit(1)` (parser.y unlock rule) — mid-protocol, no marker. `exit;` smuggled into a command stream likewise kills the session silently (exit 0). Segfaults on corrupt indexes are also handled defensively by CQPweb (see above). After any death, the next read returns EOF instead of `-::-EOL-::-`; a parser should treat EOF-before-marker as fatal.

**Escaping and injection.** CQP strings are `'...'` or `"..."`; a quote char is escaped inside by doubling (`""`) or backslash. The attack surface of raw interpolation is real: `;` starts a new command, and output redirection executes shell pipelines (`> "| cmd"`, §5), so an unescaped `"` in user input can escalate to shell execution. Defences used in practice:
- **QueryLock mode** (designed for this, per CWB::CQP POD "to improve security of CGI scripts"): `set QueryLock <random-key>;` disables all "interactive" commands — under lock, `cat` with redirection, assignments, etc. produced `WARNING: query lock violation attempted` + `PARSE ERROR` per attempt (verified); only queries and `.EOL.;` (and `unlock` with the correct key) are allowed (parser.y `command:` rules guard every other production with `if (query_lock) {warn_query_lock_violation(); YYABORT;}`). CWB::CQP `exec_query()`, cwb-ccc `Query()`, and CQPweb `query()` all wrap user queries in `set QueryLock $key … unlock $key` with a random 1..1,000,000 key. The key must never be predictable, since a guessed `unlock` inside the payload would lift the lock (and a wrong guess kills the process — a DoS, not an escalation).
- **Metacharacter escaping for interpolated strings**: CWB::CQP `quote()` picks `"…"`/`'…'` and doubles inner quotes, and refuses strings ending in an odd number of backslashes ("Cannot quote string … ending in unescaped backslash") — a trailing `\` would escape the closing quote; `quotemeta()` backslash-escapes `(){}[]|.?*+$\` and rewrites `^` to `[^]` "to work around latex escapes like \^o in CQP". CQPweb's `CQP::escape_metacharacters()` is a wrapper around PHP `preg_quote($s, '"')` (CQP ≥3.4.15 uses PCRE), and its Simple Query language (CEQL) is compiled to CQP rather than passed through.
- **Korp does neither**: `make_cqp()` concatenates the frontend-supplied CQP query verbatim (plus `within/cut/expand`), each request runs in a fresh short-lived `cqp -c` process fed one batch via `communicate()`, and the only query parsing (`parse_cqp` in korp/utils.py) is for optimisation, not sanitisation. Isolation there is process-per-request plus corpus-level auth — i.e. a Korp deployment implicitly trusts users with full CQP (including `> "| …"` shell) inside the cqp process's privileges. [That this is exploitable in a given deployment is unverified; the absence of sanitisation is verified from source.]
- Additional CQPweb hardening: tabs in commands are converted to spaces before dispatch, and the `.EOL.` terminator is written on a fresh line to defuse trailing-`#` comment hangs (§2).

**Miscellaneous.** In child mode `.cqprc`/`.cqpmacros` are not auto-read — pass `-I`/`-M` explicitly if needed. Commands with embedded newlines are legal CQP but break naive protocols; CWB::CQP flattens newlines to spaces before sending. `cat` honours `AutoShow`/paging in interactive mode only. `getline`-based readers should strip `\r` (CQPweb does, for Windows). CQP treats an activated corpus name (`TINY;`) as a command whose only output is nothing (errors go to stderr if undefined) — activation and query are separate round trips.

Sources: [CWB::CQP source (CWB v3.5.0, MetaCPAN)](https://metacpan.org/dist/CWB) · [CWB::CQP raw](https://fastapi.metacpan.org/v1/source/SCHTEPF/CWB-v3.5.0/lib/CWB/CQP.pm) · [CQP source, SVN trunk: parser.y, options.c, cqp.c, output.c, ascii-print.c, concordance.c](https://svn.code.sf.net/p/cwb/code/cwb/trunk/cqp/) · [CQP Manual](https://cwb.sourceforge.io/files/CQP_Manual/) ([§2.3 Display options](https://cwb.sourceforge.io/files/CQP_Manual/2_3.html), [§7.1 Running CQP as a backend](https://cwb.sourceforge.io/files/CQP_Manual/7_1.html), [§7.3 Generating frequency tables](https://cwb.sourceforge.io/files/CQP_Manual/7_3.html)) · [CWB documentation page](https://cwb.sourceforge.io/documentation.php) · [CQi spec zip](https://cwb.sourceforge.io/files/cqi_spec.zip) · [CQi tutorial PDF](https://cwb.sourceforge.io/files/cqi_tutorial.pdf) · [CWB::CQI::Client raw](https://fastapi.metacpan.org/v1/source/SCHTEPF/CWB-CQI-v3.5.0/lib/CWB/CQI/Client.pm) · [CQPweb lib/cqp.php (SVN)](https://sourceforge.net/p/cwb/code/HEAD/tree/gui/cqpweb/trunk/lib/cqp.php) · [korp-backend korp/cwb.py](https://raw.githubusercontent.com/spraakbanken/korp-backend/master/korp/cwb.py), [korp/utils.py](https://raw.githubusercontent.com/spraakbanken/korp-backend/master/korp/utils.py), [korp/views/query.py](https://raw.githubusercontent.com/spraakbanken/korp-backend/master/korp/views/query.py) · [cwb-ccc ccc/cqp.py](https://raw.githubusercontent.com/ausgerechnet/cwb-ccc/master/ccc/cqp.py) · [cqi-py](https://github.com/Pevtrick/cqi-py), [PyPI cqi](https://pypi.org/project/cqi) · [cqp-clj](https://github.com/emanjavacas/cqp-clj) · [PolMine cqi (R)](https://rdrr.io/github/PolMine/cqi/f/) · [rcqp mirror](https://github.com/cran/rcqp)