All research complete. Below is the report.

---

# IMS Open Corpus Workbench (CWB): Research Report

**Primary sources used:** the CWB homepage (https://cwb.sourceforge.io/), its documentation index (https://cwb.sourceforge.io/documentation.php), install page (https://cwb.sourceforge.io/install.php), the *CWB Corpus Encoding and Management Manual* v3.5, July 2022 (HTML: https://cwb.sourceforge.io/files/CWB_Encoding_Tutorial/, PDF: https://cwb.sourceforge.io/files/CWB_Encoding_Tutorial.pdf), and the CWB 3.5.0 source tarball (https://sourceforge.net/projects/cwb/files/cwb/cwb-3.5/source/cwb-3.5.0-src.tar.gz — files `man/*.pod`, `CHANGES`, `README`, `COPYING`, `cl/cl.h`, `doc/hard_limits.txt`, `utils/Makefile`). All terminal outputs below marked "actual output" were produced by running the Homebrew-installed CWB 3.5.0 (`cwb-config --version` → `3.5.0`) on the official encoding-tutorial demo data (https://cwb.sourceforge.io/files/encoding_tutorial_data.zip, corpus `VSS`, 8,043 tokens), so they are verified real outputs, not paraphrase.

## 1. What CWB is

CWB is "a collection of open-source tools for managing and querying large text corpora (up to 2 billion words) with linguistic annotations"; its core is the CQP query processor (https://cwb.sourceforge.io/). It is "a highly specialised database and query processor for large text corpora" using "a proprietary read-only format to store corpora with token-level annotations … and shallow structural markup"; the read-only design allows full indexing and compression (source `README`). Components: the C "Corpus Library" (CL), the command-line encode/index/decode utilities, CQP (interactive/slave), `cqpserver` (network daemon speaking the CQi protocol), plus separately distributed CWB/Perl modules and the CQPweb web frontend (https://cwb.sourceforge.io/files/CQP_Tutorial/1_1.html, https://cwb.sourceforge.io/documentation.php).

## 2. The CWB data model

### 2.1 Positional attributes (p-attributes)

- A p-attribute is a token-level annotation layer: one string value per corpus position (token). "P-attributes correspond to *columns* in the input data. Each token occupies a single row and consists of a series of tab-delimited values. Each tab-separated column is encoded as a separate p-attribute" (`man/cwb-encode.pod`).
- The first column is by default the p-attribute `word` (the surface form); further columns (POS, lemma, etc.) are declared with `-P`. **Every CWB corpus must have a p-attribute named `word`** — this is hard-coded throughout the source (`doc/hard_limits.txt`).
- Corpus positions ("cpos") are 0-based signed 32-bit integers; each p-attribute has a *lexicon* of distinct types with internal integer IDs and precomputed frequencies.
- A p-attribute value may be a *feature set* (declared as `-P attr/`): a `|`-delimited, validated and alphabetically normalised set like `|f1|f2|`; CoNLL-style sets without outer bars are accepted, and `_` or empty becomes the empty set `|` (`man/cwb-encode.pod`).

### 2.2 Structural attributes (s-attributes)

- "An s-attribute is made up of a series of regions, where each region has a start point and an end point (expressed in terms of corpus positions in the token sequence), and possibly an annotation value" (`man/cwb-s-decode.pod`). In input they appear as XML start/end tags on lines of their own.
- **Without annotations** (`-S`): regions carry no value (e.g. `-S s:0` for plain `<s>…</s>`).
- **With annotations** (`-V`): each region stores the original XML tag's whole attribute-value string as one annotation (making the input XML fully reconstructible), *in addition to* any split-off attributes (`man/cwb-encode.pod`).
- **XML attribute splitting:** `-S s:0+id+len` stores `<s id="abc" len=42>` as three s-attributes: `s` (unvalued), `s_id` (value `abc`), `s_len` (value `42`). With `-V s:0+id+len`, `s` additionally carries the value `id="abc" len=42` (`man/cwb-encode.pod`).
- **Nesting:** regions of one s-attribute may not nest within themselves; `-S np:3` allows up to 3 levels, auto-generating flat s-attributes `np1`, `np2`, … for the nested instances ("mildly recursive" markup, `README`); `:0` ignores nested regions. `-0 attr` declares a null s-attribute whose tags are discarded; undeclared tags become literal tokens with a warning (or are auto-nulled with `-9`) (`man/cwb-encode.pod`).

### 2.3 Alignment attributes (a-attributes)

- "A-attributes represent the correspondences between pairs of regions in two corpora which contain translation-equivalent text in two languages"; typical use is linking sentence s-attributes of a parallel corpus (`man/cwb-align-encode.pod`).
- "An alignment attribute is made up of a series of alignment beads, each linking a consecutive range of tokens in the source corpus to a corresponding range in the target corpus. Gaps and crossing alignments are allowed, but ranges in the source corpus may not overlap and must appear in natural order… Alignment beads where one of the ranges is empty (i.e. 1:0 and 0:1 alignments) are not represented in the binary format" (`man/cwb-align-decode.pod`).
- An a-attribute belongs to the *source* corpus and **its name is always the lowercase ID of the target corpus**; it is declared in the source corpus's registry file as `ALIGNED my_target_corpus` — this line must be added *before* running `cwb-align-encode` (manually or via CWB/Perl `cwb-regedit MY_SOURCE_CORPUS :add :a my_target_corpus`) (`man/cwb-align-encode.pod`).
- Binary storage: `.alx` files (plus `.alg` for backward compatibility) (`utils/Makefile` comments).

### 2.4 Data directory files (observed after encoding/indexing/compression)

Per p-attribute: `.corpus` (token stream as lexicon IDs; deletable after compression), `.corpus.cnt`, `.lexicon`, `.lexicon.idx`, `.lexicon.srt` (lexicon + sorted index), `.corpus.rev`/`.corpus.rdx` (reverse index; deletable after compression), `.huf`/`.hcd`/`.huf.syn` (Huffman-compressed token stream), `.crc`/`.crx` (compressed index). Per s-attribute: `.rng` (regions); annotated s-attributes additionally `.avs`/`.avx` (annotation strings + index). (Directory listing of the encoded `VSS` corpus; file roles from `man/cwb-huffcode.pod`, `man/cwb-compress-rdx.pod`.)

### 2.5 The corpus registry

- "Registry files contain a formalised description of an indexed corpus — where its data files are, what p- and s-attributes it has, and so on." The registry filename is the all-lowercase corpus ID (allowed: `a-z`, `0-9`, `_`, `-`; ASCII only); the corpus name used by CQP and the tools is the ALL-UPPERCASE version (`man/cwb-encode.pod`).
- **Format** (Appendix A of the Encoding Manual, https://cwb.sourceforge.io/files/CWB_Encoding_Tutorial/A.html; confirmed against a file actually generated by `cwb-encode -R`): a line-oriented plain-text format with `#` comments:

```
##
## registry entry for corpus VSS
##

# long descriptive name for the corpus
NAME ""
# corpus ID (must be lowercase in registry!)
ID   vss
# path to binary data files
HOME /corpora/data/vss
# optional info file (displayed by "info;" command in CQP)
INFO /corpora/data/vss/.info

# corpus properties provide additional information about the corpus:
##:: charset  = "latin1" # character encoding of corpus data
##:: language = "??"     # insert ISO code for language (de, en, fr, ...)

##
## p-attributes (token annotations)
##
ATTRIBUTE word
ATTRIBUTE pos
ATTRIBUTE lemma

##
## s-attributes (structural markup)
##
# <story num=".." title=".."> ... </story>
# (no recursive embedding allowed)
STRUCTURE story
STRUCTURE story_num            # [annotations]
STRUCTURE story_title          # [annotations]
STRUCTURE p
STRUCTURE s

# Yours sincerely, the Encode tool.
```

  Declaration keywords: `NAME` (descriptive name), `ID` (lowercase corpus ID), `HOME` (data directory), `INFO` (info file shown by CQP's `info;`), `##:: property = "value"` corpus properties (notably `charset`, `language`), `ATTRIBUTE` (p-attribute), `STRUCTURE` (s-attribute), `ALIGNED` (a-attribute = lowercase target-corpus ID). Paths with blanks/non-standard characters must be double-quoted, with `\"` and `\\` escapes. A more flexible legacy format is still parsed for backward compatibility, but the format above is the standard and "is in fact enforced by the CWB/Perl scripts" (Appendix A). The grammar lives in `cl/registry.y`/`cl/registry.l` in the source.
- **CORPUS_REGISTRY and lookup order:** every tool takes `-r registry_dir`; "if this option is not specified, then a directory specified by the CORPUS_REGISTRY environment variable will be used; if that is not available, the built-in CWB default will be used" (all `man/*.pod`). The value may be a colon-separated list of directories; a directory prefixed with `?` is optional (no warning if unmounted), e.g. `export CORPUS_REGISTRY=/Corpora/registry:?/Volumes/X/CWB/registry`; the built-in default is *not* implicitly appended to such a list (Encoding Manual §2). Built-in defaults: `/usr/local/share/cwb/registry` (source builds and official binary/deb packages — `config/site/standard`, `config/site/binary-release`, `config/site/linux-deb`), `/usr/share/cwb/registry` for some package-manager installs (Encoding Manual §2), `$(HOMEBREW_ROOT)/share/cwb/registry` for Homebrew (verified: `cwb-config --default-registry` → `/opt/homebrew/share/cwb/registry`).

## 3. Command-line tool inventory

The complete set of programs built and installed by CWB 3.5.0 (`utils/Makefile` PROGRAMS list plus the `cqp/` and `CQi/` targets): `cwb-encode`, `cwb-makeall`, `cwb-huffcode`, `cwb-compress-rdx`, `cwb-decode`, `cwb-lexdecode`, `cwb-s-encode`, `cwb-s-decode`, `cwb-describe-corpus`, `cwb-decode-nqrfile`, `cwb-scan-corpus`, `cwb-align`, `cwb-align-show`, `cwb-align-encode`, `cwb-align-decode`, `cwb-atoi`, `cwb-itoa`, plus `cqp`, `cqpcl` (deprecated), `cqpserver`, and the installed helper script `cwb-config`. (`cwb-check-input`, a well-formedness checker for vertical files, exists in `utils/` source but is *not* in the default build list.) None of the tools support GNU-style `--long-options` (every man page). Nearly all honour `CORPUS_REGISTRY` and `-r`; encode/decode tools also honour `CWB_USE_7Z` and `CWB_COMPRESSOR_PATH` for transparent `.gz/.bz2/.xz` handling (man pages).

### 3.1 cwb-encode — corpus encoder

Reads verticalized text (stdin, `-f file`s, or `-F dir` of `*.vrt[.gz|.bz2]`), writes binary data files to `-d data_dir`, and with `-R registry_file` writes the registry entry. Key options: `-c charset` (ascii, latin1…latin9 minus 11/12, cyrillic, arabic, greek, hebrew, utf8; **default latin1**, explicit declaration strongly recommended), `-x` XML-aware mode (decodes `&lt;` etc., skips comments/declarations), `-s` skip blank lines, `-B` strip whitespace, `-C` replace invalid bytes with `?`, `-9` auto-null unknown XML tags, `-U string` value for missing columns (default `__UNDEF__`), `-n`/`-N id`/`-L s` CoNLL-style numbered input and blank-line sentence boundaries, and the declarations `-p`/`-P`/`-S`/`-V`/`-0` described in §2. Options must precede declarations. Limits: max 2,048 attributes (≤1,024 p-, ≤1,024 s-), practically constrained by the OS open-file limit (~3 files per attribute; rule of thumb 341 attributes under a 1,024-fd limit). Typical invocation (Encoding Manual §2):

```
$ cwb-encode -d /corpora/data/example -xsBC9 -c ascii -f example.vrt \
             -R /usr/local/share/cwb/registry/example -P pos -P lemma -S s
```

(Sources: `man/cwb-encode.pod`; https://cwb.sourceforge.io/files/CWB_Encoding_Tutorial/2.html.)

### 3.2 cwb-makeall — index builder

"Creates a lexicon and index for each p-attribute of an encoded CWB corpus. This is an essential step before the corpus can be queried." Options: `-r`, `-P attr` or trailing attribute list, `-c component` (single index component), `-M megabytes` memory limit, `-V` extra validation (recommended below ~50 M tokens), `-D` debug. `cwb-makeall CORPUS` processes all p-attributes (`man/cwb-makeall.pod`). Actual output:

```
=== Makeall: processing corpus VSS ===
Registry directory: /…/registry
ATTRIBUTE word
 + creating LEXSRT ... OK
 - lexicon      OK
 + creating FREQS ... OK
 - frequencies  OK
 - token stream OK
 + creating REVCIDX ... OK
 + creating REVCORP ... OK
 ? validating REVCORP ... OK
 - index        OK
ATTRIBUTE pos
 …
```

### 3.3 cwb-huffcode — token-stream compression

"Compresses the encoded token sequence of a positional attribute … using Huffmann coding," producing `.huf`, `.hcd`, `.huf.syn` which supersede `.corpus`. `-P attr` (default `word`) or `-A` (all), `-T` skip validation, `-f prefix`, `-v` (repeatable) verbosity (`man/cwb-huffcode.pod`). Actual output:

```
COMPRESSING TOKEN STREAM of VSS.word
- writing code descriptor block to /…/data/vss/word.hcd
- writing compressed item sequence to /…/data/vss/word.huf
- writing sync (every 128 tokens) to /…/data/vss/word.huf.syn
VALIDATING VSS.word
…
!! You can delete the file </…/data/vss/word.corpus> now.
```

### 3.4 cwb-compress-rdx — index compression

"Compresses the index files of a positional attribute," producing `.crc`/`.crx` which supersede `.corpus.rev`/`.corpus.rdx`. Same `-P`/`-A`/`-T`/`-f` conventions; `-d`/`-D file` debug (`man/cwb-compress-rdx.pod`). Actual output:

```
COMPRESSING INDEX of VSS.word
- writing compressed index to /…/data/vss/word.crc
- writing compressed index offsets to /…/data/vss/word.crx
VALIDATING VSS.word
…
!! You can delete the file </…/data/vss/word.corpus.rev> now.
!! You can delete the file </…/data/vss/word.corpus.rdx> now.
```

### 3.5 cwb-describe-corpus — corpus information

`cwb-describe-corpus [-sdh] [-r registry_dir] CORPUS ...` — prints basic info (size, paths, encoding) plus the attribute inventory; `-s` adds per-attribute statistics ("number of tokens and types for a p-attribute, number of regions for an s-attribute, and number of alignment blocks for an a-attribute"); `-d` adds per-file debug detail; multiple corpora may be listed (`man/cwb-describe-corpus.pod`). Actual output of `cwb-describe-corpus -s VSS`:

```
============================================================
Corpus: VSS
============================================================

description:    
registry file:  /…/registry/vss
home directory: /…/data/vss/
info file:      /…/data/vss/.info
encoding:       latin1
size (tokens):  8043

  3 positional attributes
  9 structural attributes
  0 alignment  attributes

p-ATT word                   8043 tokens,     2111 types
p-ATT pos                    8043 tokens,       39 types
p-ATT lemma                  8043 tokens,     1699 types
s-ATT collection                1 regions
s-ATT story                     6 regions
s-ATT story_num                 6 regions (with annotations)
s-ATT story_title               6 regions (with annotations)
s-ATT p                        84 regions
s-ATT s                       459 regions
```

(Without `-s`, the attribute names are listed in columns instead of the statistics.)

### 3.6 cwb-lexdecode — lexicon access / frequency lexicon

`cwb-lexdecode [-fnlbsh] [-r dir] [-P attr] [-p regexp [-cd]] [-F file [-0N]] CORPUS` or `cwb-lexdecode -P attr -S CORPUS`. Prints all or part of a p-attribute's lexicon: `-f` frequency, `-n` internal lexicon ID, `-l` string length, `-s` alphabetical sort, `-b` no column padding (numeric columns are otherwise padded to width 7), `-p regexp` PCRE filter (whole-string anchored; `-c` case-, `-d` accent-insensitive), `-F file` looks up exact strings line-by-line (`-F -` = stdin; `-N` = interpret input as lexicon IDs; `-0` print zero frequencies for unknown items), `-S` prints only token/type counts. Output: one TAB-delimited line per entry: `[id] [freq] [length] string` (`man/cwb-lexdecode.pod`). Actual outputs:

```
$ cwb-lexdecode -S VSS
Tokens:	8043
Types:	2111

$ cwb-lexdecode -f -P pos VSS | sort -nr | head -4
   1101	NN
    839	DT
    783	IN
    603	VBD

$ cwb-lexdecode -nlf -p 'eleph.*' VSS
   1319	      2	      9	elephants
   1977	     14	      8	elephant
```

The Encoding Manual §8 documents the frequency-list idioms `cwb-lexdecode -f -P lemma VSS | sort -nr -k 1 | head -20` and `cwb-lexdecode -f0 -P pos -F tags.txt VSS`, noting `-p`+`-c` "may be considerably faster on a large corpus" than the equivalent CQP query (https://cwb.sourceforge.io/files/CWB_Encoding_Tutorial/8.html).

### 3.7 cwb-decode — corpus decoder

`cwb-decode (-L|-H|-C|-Cx|-X) [-n] [-b attr] [-s start] [-e end] [-p|-f file] [-Sp|-Sf file] CORPUS [-c attr] [-ALL] (-P attr|-S attr|-V attr|-A attr)+`. Modes: whole corpus/range (`-s`/`-e`), *matchlist mode* (`-p`/`-f`: reads cpos pairs, e.g. from a CQP `dump`, and prints those ranges), *subcorpus mode* (`-Sp`/`-Sf`: materialises a physical subcorpus, added in 3.4.15). Output formats: Standard (one token/line, `attr=value` pairs), `-C` compact (VRT, re-encodable by cwb-encode), `-Cx` XML-compatible compact, `-H` concordance-style horizontal, `-X` full XML (`<token>`, `<attr name=…>`, `<tag>`/`<align>` elements), `-L` Lisp. `-n` adds corpus positions; `-b s` inserts a blank line after each `s` region (CoNLL export); extended `-S text=novel`, `-S np:2`, `-S text+id+title` declarations reconstruct renamed/nested/split XML (`man/cwb-decode.pod`). Actual outputs:

```
$ cwb-decode -n -s 0 -e 2 VSS -P word -P pos -P lemma -S s
       0: word=The	pos=DT	lemma=the	<s>:0-15	
       1: word=constant	pos=JJ	lemma=constant	<s>:0-15	
       2: word=hum	pos=NN	lemma=hum	<s>:0-15	

$ cwb-decode -C -s 0 -e 4 VSS -P word -P pos -S s
<s>
The	DT
constant	JJ
hum	NN
of	IN
the	DT
```

Documented full-export idioms: `cwb-decode -C VSS -ALL > vss-corpus.vrt` (exact re-encodable copy) and `cwb-decode -Cx VSS -ALL > vss-corpus.xml`; CoNLL export `cwb-decode -C -b s CORPUS -P id -P word -P pos -P lemma` (Encoding Manual §8; `man/cwb-decode.pod` EXAMPLES).

### 3.8 cwb-s-decode — s-attribute decoder

`cwb-s-decode [-nvh] [-r dir] CORPUS -S attribute`. Prints one line per region: `start TAB end TAB [annotation]`; `-n` suppresses positions, `-v` suppresses values (`man/cwb-s-decode.pod`). Actual output:

```
$ cwb-s-decode VSS -S story_title
0	4194	264
4195	4526	How To Swim
4527	5484	Waiting
5485	6023	A Thrilling Experience
6024	7219	An Example of Idiomatic English
7220	8042	The Garden
```

### 3.9 cwb-s-encode — add s-attributes to an existing corpus

`cwb-s-encode [-BMamsqDh] [-d output_dir] [-f file] [[-r dir] -C corpus] (-S attr|-V attr)`. Reads TAB-delimited `start end [annotation]` lines (regions in corpus order, non-overlapping except in in-memory `-M` mode) and writes a new s-attribute into an existing corpus — typical uses: re-importing `cwb-s-decode` output with added annotations, or encoding CQP `dump`/`tabulate` results as regions. The registry must then be updated manually (`STRUCTURE u_num`) or with CWB/Perl `cwb-regedit`; no `cwb-makeall` run is needed afterwards (`man/cwb-s-encode.pod`).

### 3.10 cwb-scan-corpus — n-gram / joint frequency scans

`cwb-scan-corpus [-Cqh] [-r dir] [-b num] [-o file] [-S] [-f n] [-F attr] [-w attr|-d attr] [-s start] [-e end] [-R file] CORPUS key1 key2 ...` — "computes the joint frequency distribution over a set of specified keys". Key syntax `[?]att[+n][(!)=/regex/[cd]]`: `att` is a p-attribute or *annotated* s-attribute, `+n` a token offset, `/regex/` a PCRE filter, and a leading `?` marks a constraint key that filters but is not counted/printed. Options: `-f n` frequency floor, `-C` drop non-regular-word values, `-w attr` restrict n-grams to within regions of an s-attribute, `-d attr` document frequencies per region, `-F attr` sum a numeric p-attribute instead of counting occurrences ("intended for use with large frequency tables stored as a CWB corpus, e.g. in the BNCweb interface"), `-S` canonical byte-order sort, `-o file` (supports `.gz`/`.bz2` and `"| cmd"` pipes), `-R file` scan only listed ranges. Output: TAB-delimited `frequency value1 value2 …` (`man/cwb-scan-corpus.pod`). Actual outputs:

```
$ cwb-scan-corpus -q -f 10 VSS word+0 word+1 | sort -nr | head -4
77	of	the
46	,	and
38	.	The
34	to	the

$ cwb-scan-corpus -q -f 5 VSS 'lemma+0' '?pos+0=/N.*/' | sort -nr | head -3
74	Ed
27	girl
23	<unknown>

$ cwb-scan-corpus -q VSS story_title | sort -nr | head -3   # tokens per story
4195	264
1196	An Example of Idiomatic English
958	Waiting
```

### 3.11 Alignment tools

- **cwb-align** — "a simple program for aligning units in parallel corpora": given source corpus, target corpus, and a *grid* s-attribute that must exist under the same name in both (typically `s`), it scores candidate pairs by segment length, shared words (case/accent-insensitive), shared letter 4-grams, and optional user-supplied translation-equivalent word pairs, and writes a `.align` file. Documented basic invocation: `cwb-align -o holmes.align HOLMES-EN HOLMES-DE s` (Encoding Manual §9.2, https://cwb.sourceforge.io/files/CWB_Encoding_Tutorial/9_2.html). Explicitly "*not* … state-of-the-art … a basic, fallback option" (`man/cwb-align.pod`). **`.align` format:** header line `source_id TAB grid_attr TAB target_id TAB grid_attr`, then per bead six TAB fields: source start cpos, source end, target start, target end, bead type (`1:1`, `2:1`, `1:2`, `2:2`, plus `1:0`/`0:1`), and optional quality score — e.g. `140 169 137 180 1:2` (`man/cwb-align.pod` OUTPUT FORMAT).
- **cwb-align-encode** — `cwb-align-encode [-DCRvh] [-d data_dir] [-r registry_dir] alignment_file`: encodes a `.align` file as an a-attribute of the source corpus (which must already carry the `ALIGNED target` registry line; see §2.3). Alternative for pre-existing alignments keyed by region IDs: CWB/Perl's `cwb-align-import`, which also does the encoding step (`man/cwb-align-encode.pod`).
- **cwb-align-decode** — `cwb-align-decode [-h] [-r dir] CORPUS -A attribute`: dumps an encoded a-attribute back to `.align`-style text (header uses dummy grid `s`; bead-type and quality columns omitted) (`man/cwb-align-decode.pod`).
- **cwb-align-show** — interactive terminal viewer showing each aligned pair side-by-side in two columns, for reviewing a `.align` file before encoding (`man/cwb-align-show.pod`).

### 3.12 Small helpers and query front-ends

- **cwb-atoi / cwb-itoa** — convert between ASCII integers (one per line) and CWB's uncompressed binary format of 32-bit network-byte-order integers, for low-level inspection/creation of data files (`man/cwb-atoi.pod`, `man/cwb-itoa.pod`).
- **cwb-config** — prints installation facts, one flag at a time: `--version --prefix --bindir --libdir --incdir --mandir --cflags --ldflags --registry --default-registry`; not available on Windows (`man/cwb-config.pod`). Verified: `cwb-config --version` → `3.5.0`.
- **cwb-decode-nqrfile** — decodes the binary file of a saved CQP named query result / subcorpus (created by CQP's `save`) into ASCII cpos integers; takes a plain path, knows nothing of the registry (`man/cwb-decode-nqrfile.pod`).
- **cqp** — the query processor itself; interactive or "child/slave mode" (`-c`) "when CQP is being used as the back-end to a program providing a friendlier user-interface, for instance over the web" (`man/cqp.pod`). **cqpcl** — deprecated one-shot CLI variant, "due to security issues and problems with shell metacharacters," unmaintained since 2011 (`man/cqpcl.pod`). **cqpserver** — network daemon implementing the CQi client-server protocol, with `user`/`host` access-control statements in the init file (`man/cqpserver.pod`; CQi spec at https://cwb.sourceforge.io/files/cqi_spec.zip, tutorial https://cwb.sourceforge.io/files/cqi_tutorial.pdf).
- **cwb-make** (CWB/Perl, not core) — "the recommended front-end for indexing and compression": `cwb-make -V CORPUS` runs cwb-makeall + cwb-huffcode + cwb-compress-rdx and deletes superseded files; applies a default `-M 75` memory limit "optimised for machines of the last millennium" unless overridden (`man/cwb-makeall.pod`; Encoding Manual §2).

## 4. Encoding workflow from VRT files

### 4.1 The VRT format

"The standard CWB input format is one-word-per-line text, with the surface form in the first column and token-level annotations specified as additional TAB-separated columns. XML tags for sentence boundaries and other structural annotation must appear on separate lines. This file format is also called verticalized text and has the customary file extension `.vrt`" (Encoding Manual §2). The official `example.vrt` (from https://cwb.sourceforge.io/files/encoding_tutorial_data.zip; TAB-separated columns word/POS/lemma):

```
<s>
It	PP	it
was	VBD	be
an	DT	an
elephant	NN	elephant
.	SENT	.
</s>
```

Rules (`man/cwb-encode.pod` INPUT FILE FORMAT): file must match the declared charset (UTF-8 BOM allowed); one token per line, punctuation as separate tokens; same column order on every line; XML tag lines contain exactly one tag; XML comments/declarations allowed only in `-x` mode; stray whitespace/blank lines only with `-B`/`-s`; feature-set values allowed in columns and XML attributes; alternative numbered `-n`/CoNLL format with `#` comments and discarded `X-Y`/`X.Y` multiword/trace lines. Max input line length 65,536 bytes (`MAX_INPUT_LINE_LENGTH`, verified in `utils/cwb-encode.c:59`; Appendix B — note the older `doc/hard_limits.txt` figure of 16,383 is stale). `.gz`/`.bz2`/`.xz` inputs auto-decompress. CRLF line endings handled since 3.4.14 (`CHANGES`).

### 4.2 The workflow (Encoding Manual §2, §4)

1. Create an empty **data directory** per corpus (cwb-encode won't create it; delete old files when re-encoding).
2. Choose/confirm the **registry directory** (default or `CORPUS_REGISTRY`/`-r`).
3. **Encode:** `cwb-encode -d /corpora/data/example -xsBC9 -c utf8 -f example.vrt -R /usr/local/share/cwb/registry/example -P pos -P lemma -S s` (the `-xsBC9` cleanup cluster is recommended unless input is known-clean).
4. **Index + compress:** preferably `cwb-make -V EXAMPLE` (CWB/Perl); manually: `cwb-makeall -V EXAMPLE`, then `cwb-huffcode -A EXAMPLE`, then `cwb-compress-rdx -A EXAMPLE`, deleting the files each step says can be deleted. Omit `-V` and set `-M` for corpora above ~50 M tokens.
5. **Verify:** `cwb-describe-corpus -s EXAMPLE`; then query with `cqp -e` (`EXAMPLE; "elephant";`).

Later additions: more p-attributes via additional `cwb-encode -p -` runs on the same data directory [the manual's §6 covers this; mechanism per `man/cwb-encode.pod`], s-attributes via `cwb-s-encode` + registry edit (§3.9), alignments via `cwb-align`/`cwb-align-import` + `ALIGNED` registry line + `cwb-align-encode` (§3.11). Round-trips: `cwb-decode -C … -ALL` output re-encodes exactly; since 3.4.33 nested/split XML can be reconstructed with extended `-S` declarations (Encoding Manual §8).

## 5. Version landscape

Timeline (source `CHANGES`; SourceForge file dates):

- **3.0** (April 2010) — "the first official release of the IMS Open Corpus Workbench" as open source (development at IMS Stuttgart began 1993; open-sourced under GPL in 2005 per https://cwb.sourceforge.io/files/CQP_Tutorial/1_1.html). Latin-1-centric, POSIX regexes. Homepage now labels 3.0 "of historical interest only" (https://cwb.sourceforge.io/).
- **3.1** — Windows support (MinGW cross-compilation), no other features.
- **3.2** (beta) — **UTF-8/Unicode support introduced**; switch from POSIX to **PCRE** regexes; identifiers restricted to ASCII (corpora with accented names must be re-indexed); charset-aware encoding validation.
- **3.4.x** (beta series, ~2013–2021, versions up to 3.4.33) — long incremental series toward 3.5; widely deployed in production. Notable: precise enforcement of the corpus size limit (`CL_MAX_CORPUS_SIZE`; cwb-encode discards input beyond it with a warning), UTF-8-safe cwb-align and cleanup, transparent `.gz`/`.bz2` streams (3.4.11), CoNLL support in encode/decode (3.4.28), subcorpus mode for cwb-decode (3.4.15), hash-based `group`, JIT-capable PCRE, feature-complete CQP additions (`normalize()`, `lbound_of`/`rbound_of`, matching-strategy modifiers).
- **3.5.0** (released 2022-07-24; SourceForge file listing) — **current stable and recommended version**: "Version 3.5 is the current, and probably final, stable version of the 'original' Corpus Workbench. The next stable version will be 4.0. Bugfixes will be provided in v3.5.1+; experimental enhancements in v3.6.0+; preparation for CWB v4 in v3.9.0+" (`CHANGES`). As of 2026-09-03 no 3.5.x bugfix release has appeared on SourceForge (only `cwb-3.5.0` artifacts in the cwb-3.5 directory). The homepage lists "Current stable release: Version 3.5" (https://cwb.sourceforge.io/); install page: "Version 3.5 (CQPweb 3.2) is recommended" (https://cwb.sourceforge.io/install.php).

**Size limits** (Encoding Manual Appendix B, https://cwb.sourceforge.io/files/CWB_Encoding_Tutorial/B.html; verified in `cl/cl.h:309`): maximum corpus size **2,147,483,647 tokens** (`#define CL_MAX_CORPUS_SIZE 2147483647`, signed 32-bit — the homepage's "up to 2 billion words"); p-attribute lexicon and annotated-s-attribute `.avs` string store each ≤ 2 GiB (signed 32-bit byte offsets); annotation strings ≤ 4,096 bytes incl. NUL (`CL_MAX_LINE_LENGTH`); filenames ≤ 1,024 bytes; VRT input lines ≤ 65,536 bytes. Other hard limits: ≤1,024 s-attributes per corpus; cwb-decode ≤1,024 attributes at once; mandatory `word` p-attribute (`doc/hard_limits.txt`).

**UTF-8:** fully supported since the 3.2 line and standard in 3.4/3.5 (`cwb-encode -c utf8`; PCRE Unicode properties in queries; UTF-8-correct KWIC character contexts since 3.4). Default charset is still latin1 and may change, so always pass `-c` (`man/cwb-encode.pod`).

**Licensing:** GPL v2 or later ("The IMS Open Corpus Workbench (CWB) is licensed under the GNU General Public License, version 2 or newer", `COPYING`; Homebrew records `GPL-2.0-or-later`). Bundled/linked third-party components: PCRE (BSD), GLib, GNU Readline (GPLv3), ncurses (`COPYING`).

**Installation** (https://cwb.sourceforge.io/install.php):
- *Debian/Ubuntu/Mint:* project-supplied `.deb` (`cwb_3.5.0-1_amd64.deb`, `cwb-dev_3.5.0-1_amd64.deb`) from https://sourceforge.net/projects/cwb/files/cwb/cwb-3.5/deb/, installed with `sudo dpkg -i`.
- *Fedora/RHEL:* `.rpm` from …/cwb-3.5/rpm/ via `sudo dnf localinstall`.
- *Arch/Manjaro:* PKGBUILD shipped in the source tarball (`packaging/pkgbuild_cwb`, `makepkg -i`).
- *Generic Linux/Unix from source:* tarball + `sudo install-scripts/install-linux` (or `make` per `INSTALL`); needs C compiler, GLib, PCRE, optionally Readline/ncurses.
- *macOS:* **`brew install cwb3`** (`--head` for development version). The `cwb3` formula is now in **homebrew-core** (verified locally: "From: https://github.com/Homebrew/homebrew-core/blob/HEAD/Formula/c/cwb3.rb"), so no third-party tap is required; a binary `.tar.gz` release (…/cwb-3.5/darwin/, `sh install-cwb.sh`) and source builds (`INSTALL-MACOS`) are alternatives.
- *Windows:* WSL recommended; native zip release, Cygwin or MSYS2 possible; CWB/Perl unsupported there.
- *Companion packages:* CWB/Perl modules from CPAN (`CWB`, `CWB-CL`, `CWB-Web`, `CWB-CQI`); CQPweb from source only. R bindings exist third-party (RcppCWB/cwbtools by PolMine, https://github.com/PolMine/RcppCWB) [third-party, not part of CWB].
- *Post-install test:* `cwb-describe-corpus -r registry -s DICKENS` and `cqp -eC -r registry -D DICKENS` against the downloadable Dickens demo corpus.

## 6. Tool outputs a read-only web frontend would plausibly surface (besides cqp)

All of these are read-only, registry-driven, machine-parseable (TAB-delimited or fixed-layout) stdout producers — exactly the shape a web backend can shell out to and render. Precedent: CQPweb/BNCweb are built on CQP plus these utilities; the `-F` option of cwb-scan-corpus is explicitly "intended for use with … the BNCweb interface" (`man/cwb-scan-corpus.pod`).

1. **`cwb-describe-corpus [-s] CORPUS` → corpus info page.** One call yields description, charset, token count, and the full attribute inventory with type counts and region counts (exact output in §3.5) — everything needed for a corpus landing page ("8,043 tokens, 3 token annotations, 459 sentences, 6 stories…"). Parse: `key: value` header lines plus regular `p-ATT/s-ATT/a-ATT` rows.
2. **`cwb-lexdecode -f [-s|-P attr|-p regex -c -d] CORPUS` → word lists, frequency lexicon, "browse vocabulary" and wildcard word-lookup pages.** TAB-delimited `freq TAB string` rows (`-b` to disable padding); `-S` gives instant type/token totals; `-p 'over.+' -c` implements case-insensitive wordform search server-side without CQP (documented as faster than the equivalent CQP query, Encoding Manual §8); `-F -` supports batch lookup of a user's word list with `-f0` zero-fill.
3. **`cwb-scan-corpus CORPUS key…` → frequency-scan pages: n-gram tables, collocation-style counts, per-text/per-genre distributions.** TAB-delimited `freq TAB v1 TAB v2 …`; supports offsets (`word+0 word+1` bigrams), constraint keys (`?pos=/N.*/`), restriction to regions (`-w s`), document frequencies (`-d text`), frequency floors (`-f`), and stable sorting (`-S`) for merging (§3.10 shows real outputs). Suitable for precomputed or on-demand frequency breakdowns.
4. **`cwb-s-decode CORPUS -S attr` → metadata/text-inventory pages.** `start TAB end TAB annotation` per region; e.g. `-S story_title` (real output in §3.8) directly yields a table-of-contents of texts with their token spans — the natural source for a "texts in this corpus" listing, or for mapping cpos ranges (from query hits) back to document metadata.
5. **`cwb-decode` → full-text and context display.** Range mode (`-s`/`-e`) renders a page of running text; matchlist mode (`-p`/`-f`) turns cpos pairs (e.g. from CQP dumps) into KWIC-style lines (`-H`), XML (`-X`) for XSLT/templating, or VRT/CoNLL exports (`-C`, `-Cx`, `-b s`) for a "download this text" feature; `-c s` expands hits to sentence context (§3.7).
6. **`cwb-align-decode CORPUS -A target` → parallel-corpus views.** Bead table (source-range ↔ target-range cpos pairs) that a frontend joins with two `cwb-decode` calls to render aligned segments side by side (§3.11).
7. **`cwb-config --registry/--default-registry/--version`** for backend self-configuration and diagnostics pages (§3.12).

Not frontend-relevant: `cwb-encode`, `cwb-s-encode`, `cwb-makeall`, `cwb-huffcode`, `cwb-compress-rdx`, `cwb-align`, `cwb-align-encode` (all write to the corpus and belong to the offline build pipeline); `cwb-align-show` (interactive TTY); `cwb-atoi`/`cwb-itoa`/`cwb-decode-nqrfile` (low-level debugging); `cqpcl` (deprecated). For live querying itself, the supported integration paths are `cqp -c` as a slave process or `cqpserver`/CQi (`man/cqp.pod`, `man/cqpserver.pod`).

Local artifacts from this research (for reuse): source tree at `/private/tmp/claude-501/-Users-rqf595-Code-corpus-probe/a3908530-2a88-4d2f-9f1a-81ac8d36e642/scratchpad/cwb-3.5.0-src/` (man pages in `man/*.pod`), tutorial demo data at `.../scratchpad/enc_data/encoding_tutorial_data/`, and a fully encoded+indexed+compressed `VSS` corpus at `.../scratchpad/data/vss/` with registry `.../scratchpad/registry/vss`.