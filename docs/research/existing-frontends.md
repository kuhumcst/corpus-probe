# Web Frontends on the IMS Open Corpus Workbench (CWB): Survey and Lessons for a Minimal Frontend

## 1. CQPweb

### Architecture

CQPweb (Andrew Hardie, Lancaster) is described by the CWB project as "a web-based graphical user interface for some elements of the CWB — and in particular, the CQP query processor," generalising the corpus-specific BNCweb design to work with any CWB corpus (https://cwb.sourceforge.io/cqpweb.php). It is written in PHP, with the CEQL simple-query parser originating in the Perl `CWB::CEQL` module (a PHP `ceqlparser.php` now exists in trunk), MySQL/MariaDB as its only supported database, and R for statistics — the trunk contains `rface.php` (R interface) and `pyface.php` alongside ~90 PHP lib files (`collocation-lib.php`, `distribution-lib.php`, `keywords-ui.php`, `dispersion-ui.php`, `multivariate-lib.php`, `subcorpus-act.php`, `useracct-lib.php`, `usercorpus-lib.php`, `api-entrypoint.php`, …) (https://svn.code.sf.net/p/cwb/code/gui/cqpweb/trunk/lib/).

**How it drives cqp** (read from `lib/cqp.php`, 2,977 lines, in the CWB SVN, https://svn.code.sf.net/p/cwb/code/gui/cqpweb/trunk/lib/cqp.php): a `class CQP` starts a child ("slave") process per PHP request with `proc_open("cqp -c -r $cwb_registry")`, holding stdin/stdout/stderr pipes. It then talks to that process **interactively** for the lifetime of the request: each command is written to stdin followed by the marker command `.EOL.;`, and output is read until the sentinel string `-::-EOL-::-` appears on stdout (constants `END_OF_OUTPUT_COMMAND = '.EOL.'`, `END_OF_OUTPUT_SEEK = '-::-EOL-::-'`), with `stream_select` used to monitor stderr for CQP errors. The class exposes `execute()`, `query()`, `querysize()`, `dump()`/`undump()`/`dump_file()` (moving tables of corpus positions in and out of CQP), and sets CQP's `DataDirectory` (`set DataDirectory "$path"`, cqp.php line 1122) so named query results are `save`d as CQP binary files in CQPweb's cache directory.

**MySQL's role** (manual §4.2, https://cwb.sourceforge.io/files/CQPwebAdminManual.pdf): "CQPweb uses an SQL database (MySQL/MariaDB) as its secondary datastore. The CWB indexes contain the main corpus data, but all ancillary data (corpus and text metadata, frequency lists, analysis data for collocation/distribution/etc., cached queries and analyses, user data like categorised queries, as well as CQPweb's system management data) is stored in the SQL database." Query hits are cached twice over: as saved CQP query files in the data directory (records in a `saved_queries`-style table), and as per-query MySQL "databases" built by running CQP `tabulate <qname> match, matchend, match text_id, match[N] word …`, optionally piping through awk, writing a temp file, then bulk-loading it via `LOAD DATA (LOCAL) INFILE` into a freshly `CREATE TABLE`d table registered in `saved_dbs` (`lib/db-lib.php` lines 269–466). These SQL tables are what sorting, distribution, collocation and categorisation post-processing run against. The manual is explicit about the philosophy: "CQPweb is built around a strategy of extremely aggressive caching of dynamically generated data … rooted in its origin as a teaching tool: a common use-pattern for CQPweb is a roomful of students … doing pretty much the same kinds of queries" (manual §4.1).

### Feature set

From the project page (https://cwb.sourceforge.io/cqpweb.php) and the manual: full CQP syntax plus the CEQL "simple query" language (easy `{lemma}` / `_POS` access, configurable per corpus by mapping annotations to primary/secondary/tertiary/combination slots, manual §9); restricted queries over metadata; KWIC concordance with sorting, thinning (random downsample), filtering by adjacent words/tags, extended context of hundreds of words; collocation with multiple association measures; distribution across text-metadata categories; keywords by comparing frequency lists; frequency lists per corpus/subcorpus; subcorpora built from metadata or from queries; manual categorisation of hits; query history and saved queries; per-corpus XML visualisation templates; RTL script support; user accounts with privileges/grants (manual §11), user-uploaded corpora, RSS feeds, downloads, and (since 3.3) an API entry point. Admin features include web-based corpus indexing, annotation/XML templates, metadata installation, and frequency-list building (manual §6).

### Why it is heavyweight

All of the following is documented in the Admin Manual (https://cwb.sourceforge.io/files/CQPwebAdminManual.pdf):

- **Dependency stack** (§1.1): Unix only ("Windows compatibility is planned but not yet achieved" as of the 2021 manual); Apache or another webserver; MySQL v5+/MariaDB 10+ ("Other SQL DB systems, like MS-SQL, SQlite, or Postgresql, don't work with CQPweb"); PHP 7.3+; CWB; optionally Perl + CWB-Perl; R; GNU awk/sort/head.
- **Installation is a multi-chapter procedure**: webscripts install, PHP ini tuning (§1.8), dedicated disk locations with webserver-writable permissions (§1.9), AppArmor/SELinux exceptions (§1.10), Apache configuration (§1.11), SQL database creation with a "Known gotchas" section (§1.12.2) covering `LOAD DATA LOCAL INFILE` being disabled separately in `[mysqld]` and `[client]`, and `secure_file_priv` defaults changing around 2015/16 so the daemon may refuse CQPweb's temp-file loads — with three alternative workarounds each trading off performance or security; then a hand-written config file (§2 lists dozens of variables) and a completion script.
- **Operational burden**: the aggressive cache must be administered (cache size limits, "finding and fixing cache leaks", §4.7–4.8); a command-line admin toolset exists just for maintenance (§5); every corpus needs a web-UI setup workflow (indexing, metadata, frequency lists, CEQL mapping, access rights, §6); and the account/privilege system makes it a multi-user service to run, not a tool to launch. The existence of CQPwebInABox (a pre-installed VM, linked from the project page) and third-party Dockerisations (https://github.com/rahonalab/CQPweb-docker) is itself evidence of the installation burden.

## 2. korp-backend (Språkbanken)

Repository: https://github.com/spraakbanken/korp-backend — Python 3.10+, Flask (WSGI), version 8.3.0 in `korp/__init__.py`; actively maintained (pushed 2026-06). The README calls it "a wrapper for Corpus Workbench": CWB ≥ 3.4.12 is the only hard requirement; MariaDB/MySQL is optional and needed only for Word Picture (`relations_CORPUSNAME` tables), lemgram statistics (`lemgram_index`), and trend diagrams (`timedata`) — all precomputed offline by the Sparv pipeline; Memcached is optional but strongly recommended. OpenAPI spec in `docs/api.yaml`, hosted at https://ws.spraakbanken.gu.se/docs/korp.

### Endpoints (enumerated from `korp/views/*.py` route decorators, cross-checked against `docs/api.yaml`)

| Endpoint | Purpose |
|---|---|
| `/` and `/info` | backend version, CQP version, list of corpora (protected vs open) |
| `/corpus_info` | per-corpus size, attributes, info-block |
| `/corpus_config` | merged YAML config (modes/corpora/attribute presets from `CORPUS_CONFIG_DIR`) serialised for the frontend |
| `/query` | KWIC concordance |
| `/query_sample` | shuffle corpora, query serially until one hit; random example sentence |
| `/count` | frequency statistics grouped by p- or s-attributes |
| `/count_all` | `/count` with query `[]` (whole-corpus frequencies) |
| `/count_time` | frequencies bucketed over time |
| `/timespan` | token distribution over time (MySQL `timedata`) |
| `/loglike` | log-likelihood comparison of two result sets |
| `/relations`, `/relations_time`, `/relations_sentences`, `/relations_time_sentences` | Word Picture: dependency-relation statistics from MySQL, plus the source sentences for a cell |
| `/lemgram_count` | lemgram frequencies (MySQL) |
| `/attr_values`, `/struct_values` | possible values of structural attributes |
| `/cache` | cache maintenance (invalidate on corpus update, clean disk cache) |
| `/optimize` | MySQL table optimisation |
| `/sleep` | test endpoint for the keep-alive machinery |

All accept GET or POST; a `callback` param gives JSONP; `incremental=true` streams progress.

### How it invokes cqp (from `korp/cwb.py`)

One **fresh `cqp` child process per invocation** — not a pool, not a daemon, not CQi: `subprocess.Popen([self.executable, "-c", "-r", self.registry], stdin=PIPE, stdout=PIPE, stderr=PIPE)` with `LC_COLLATE` set in the environment. The complete command script (prefixed with `set PrettyPrint off;`, terminated with `exit;`) is written to stdin in one shot via `communicate()`; output is read in full and split on `\n` (deliberately not `splitlines()`, to survive control characters in corpus data). A 1-second `communicate(timeout=…)` polling loop plus an `abort_event` lets an aborted HTTP request kill the cqp process and its children (via `psutil`). stderr is parsed for `CQP Error:` lines, with a whitelist of ignorable errors. `cwb-scan-corpus` is shelled out the same way for whole-corpus frequency counts (used by `/count` when `simple=true` or the query is `[]`).

The CQP script `/query` builds (from `query_corpus()` in `views/query.py`) is a direct template for a minimal frontend:

```
set PrettyPrint off;
set DataDirectory "<CACHE_DIR>";          # only when caching
CORPUS;
show cd; .EOL.;                            # list attributes + sentinel
A = <cqp query> within sentence; …         # possibly optimized to MU form
size Last;
query_data_<md5> = Last; save query_data_<md5>;   # cache miss: persist hits
Last = query_data_<md5>;                   # cache hit: restore instead of re-query
show +word +pos +lemma;
set Context 10 words;                      # or LeftContext/RightContext
set LeftKWICDelim '---: '; set RightKWICDelim ' :---';
set PrintStructures 'text_author, text_year';
set ExternalSort yes;
sort by word on match[-1] .. match[-3];    # left-sort example
cat Last 0 24;                             # page slice
exit;
```

`/count` instead ends with `tabulate Last match .. matchend word > "| sort | uniq -c | sort -nr";` — piping CQP's tabulate output through shell `sort | uniq -c` (`views/count.py` line 676). `utils.query_optimize()` rewrites multi-token queries into CQP `MU(meet …)` form for speed. The `.EOL.;` / `-::-EOL-::-` sentinel constant is identical to CQPweb's (`korp/utils.py` line 28) — a shared idiom for delimiting output sections in a cqp batch stream.

### Caching (three layers)

1. **Memcached**: per-corpus hit counts (`<prefix>:query_size_<md5-of(cqp,within,cut,…)>`), statistics rows (`CACHE_MAX_STATS` limit), and whole `/corpus_config` responses. Keys are prefixed with a per-corpus version number (`cache_prefix()` in `utils.py`); bumping the version on corpus update invalidates everything without deletion; `setup_cache()` seeds versions from registry-file mtimes.
2. **Disk**: CQP named-query results saved through CQP's own `save` into `CACHE_DIR` (via `set DataDirectory`), restored with `Last = query_data_<checksum>` — so paging through a concordance never re-runs the query; files are `os.utime`-touched on reuse and reaped after `CACHE_LIFESPAN` (default 20 min, `config.py`).
3. **Client-side `query_data`**: `/query` returns a zlib+base64 blob encoding a checksum plus per-corpus hit counts; the frontend echoes it back on subsequent pages, letting the backend skip counting even with no server cache.

Multi-corpus searches run serially just until the requested page is filled, then count the remaining corpora in parallel with a `ThreadPoolExecutor` (`PARALLEL_THREADS`); `@prevent_timeout` runs the generator in a thread and emits `{}` heartbeats into the chunked JSON stream every 5 s to keep proxies from timing out (`utils.py`).

### KWIC JSON encoding

`query_parse_lines()` parses `cat` output — token attributes are `/`-joined per word (split with `rsplit("/", n)` against the attribute order reported by `show cd`), inline `<tag>`/`</tag>` strings become structural open/close markers, and the configured KWIC delimiters (`---:` / `:---`) mark match boundaries. The result:

```json
{"kwic": [{
    "corpus": "ROMI",
    "match": {"position": 45876, "start": 10, "end": 11},
    "structs": {"text_author": "…"},
    "tokens": [
      {"word": "en", "pos": "DT", "lemma": "en",
       "structs": {"open": [{"s": {"n": "12"}}], "close": ["s"]}}, …],
    "aligned": {"romi-de": [ …tokens… ]}
  }],
 "hits": 1234,
 "corpus_hits": {"ROMI": 1000, "SUC": 234},
 "corpus_order": ["ROMI", "SUC"],
 "query_data": "<base64-zlib>",
 "time": 0.42}
```

`match.position` is the corpus position (cpos); `start`/`end` index into `tokens`; with `in_order=false` (free word order) `match` becomes a list. Incremental responses interleave `progress_corpora`, `progress_0…` objects into the same top-level JSON object. Attribute values `__UNDEF__` are translated to null (`translate_undef`). Source: `korp/views/query.py` lines 539–704.

## 3. korp-frontend (Språkbanken)

Repository: https://github.com/spraakbanken/korp-frontend — version 9.15.0, actively developed (pushed 2026-06; the changelog and 3,600+ commits are ongoing). **Framework: still AngularJS 1.8.3** (EOL since January 2022) — `package.json` pins `angular 1.8.3`, `angular-ui-bootstrap`, `angular-dynamic-locale`, `angular-filter`, `angular-ui-sortable`, plus jQuery 3.6 and components-jqueryui, with TypeScript + Webpack + Tailwind layered on top, and a separate widget library per view: SlickGrid (statistics grid), Rickshaw (trend diagram), Chart.js (pie charts), Leaflet + markercluster (map), xstate (search-tab state machine), Peggy (a build-time-compiled CQP grammar, `app/scripts/cqp_parser/CQPParser.peggy`, because the frontend parses and programmatically manipulates CQP expressions itself — documented as covering "only some of the full CQP syntax" and "quite expected to throw errors"). `app/scripts` alone is ~16,900 lines of TS/JS across 177 files.

**Feature set** (doc/user_manual_eng.md; components in `app/scripts/components/`): corpus chooser over a tree of collections with token counts; three search modes — *simple* (word/lemgram with prefix/suffix/case options), *extended* (attribute/operator/value token rows, with pluggable per-attribute widgets: `datasetSelect`, `structServiceSelect`, `structServiceAutocomplete`, `dateInterval`, autocompletes against Karp lexicon services), *advanced* (raw CQP); results as KWIC (paged, sortable, with reading mode/context view) with a **sidebar** showing all positional and structural attributes of the clicked token plus text metadata (and pluggable dependency-tree visualisation); statistics (group-by any attribute, relative + absolute frequencies, per-corpus columns, CSV export, statistics rows clickable to generate sub-searches); trend diagram over `timedata`; word picture (`/relations`); map (geocoded metadata attributes); comparison of two saved searches (`/loglike`); parallel-corpus search; user authentication for protected corpora.

**Configuration model**: three layers merged at startup — `config.yml` (in a separate "configuration directory" pointed at by `run_config.json`), optional per-mode JS files (`modes/<mode>_mode.js`, which may mutate the global `settings` object and set a different backend per mode), and the backend's `/corpus_config?mode=<mode>` response (YAML corpus/attribute/mode definitions maintained server-side). "In case of conflicts, values from a later layer overwrites those from a previous layer. Note that nested values are not merged" (doc/frontend_devel.md). The settings reference runs to ~50 documented keys (`default_within`, `default_overview_context`, `word_picture_conf`, `map_center`, `statistics_postprocess`, …), plus translation file conventions (`corpora_<lang>.json`, `locale-<lang>.json`, an AngularJS locale file per language) and customization hooks (custom extended-search components, custom sidebar renderers, stringify functions for statistics cells).

**Why it is complex, concretely**: (a) a legacy AngularJS 1.x + jQuery codebase now wrapped in TypeScript/Webpack/Tailwind, so contributors face two generations of idioms at once (the dev docs still explain AngularJS DI annotation pitfalls under minification); (b) five heavyweight, unrelated visualisation libraries each serving one results tab; (c) a client-side CQP parser that only partially matches the backend grammar; (d) a three-layer config system with non-merging override semantics, mode files that are executable JS, and a companion config repo (korp-frontend-sb) needed to understand real deployments; (e) i18n threaded through every config value (any label may be a translation object); (f) multi-corpus/multi-mode/parallel/auth concerns baked into core flows. Its feature ambition (word picture, trend, map, comparison, lemgram autocompletion) is what pulls in most of this; each such feature also imposes backend MySQL tables and offline pipeline (Sparv) requirements.

## 4. Other CWB frontends worth knowing

The CWB project's own links page (https://cwb.sourceforge.io/links.php) lists the ecosystem: BNCweb, TXM, SpoCo, ParaVoz, spheroscope, CQPweb as "web interfaces", plus cwb-python/cwb-ccc and RcppCWB as programming APIs.

**Glossa (Tekstlab, University of Oslo)** — a corpus search and results-management UI that has been rewritten three times: Perl/CGI original (https://github.com/noklesta/glossa_svn), Ruby on Rails (rglossa), Clojure/ClojureScript (https://github.com/textlab/cglossa, last pushed 2021), and the current F# implementation **fglossa** (https://github.com/textlab/fglossa, pushed 2026-01): SAFE stack (Saturn backend, Fable/Elmish frontend), SQLite for corpus metadata, notable for first-class spoken-corpus support (transcriptions time-aligned to audio/video, e.g. the NoTa-Oslo corpus, https://tekstlab.uio.no/nota/oslo/english.html) and metadata-driven subcorpus selection. Its CWB integration is the same pattern as Korp's: it pipes a command string into `cqp -c` per request (or `docker exec -i cwb cqp -c`), caps concurrency by counting `pgrep -f cqp` against `maxCqpProcesses = 8`, and has a `killall cqp` escape hatch (`src/Server/fs/Remoting/Search/Cwb/Common.fs`, `Core.fs`).

**TEITOK (Maarten Janssen)** — a web platform (PHP/C++) for viewing, creating and editing TEI-encoded corpora that uses CWB as its search engine: the TEI XML is the master data, exported/indexed into CWB so "texts can be searched efficiently with the rich query language that CWB provides" (http://www.teitok.org, http://teitok.corpuswiki.org). Actively used at LINDAT/CLARIAH-CZ (ParlaMint, historical corpora with facsimile alignment and multiple orthography layers); Docker packaging exists (https://github.com/rahonalab/TEITOK-docker). Interesting as the "document-first" counterpoint: CWB is a derived index, not the primary store.

**BNCweb** — the ancestor of CQPweb: a Web GUI specialised for the British National Corpus, where CEQL originated; PHP/MySQL/CQP with the same cache-into-SQL design (http://bncweb.lancs.ac.uk/). Effectively superseded by CQPweb for new corpora but still the reference deployment for the BNC.

**SpoCo** (Michał Woźniak / Ruprecht von Waldenfels) — "web interface for spoken corpora with aligned audio files," used for Slavic dialect corpora; small Python/CGI codebase over CQP; single-purpose but demonstrates the audio-KWIC alignment feature cheaply (https://bitbucket.org/michauw/spoco).

**ParaVoz v2** — a lightweight web interface for searching parallel corpora over CWB (https://bitbucket.org/rvwfels/paravoz2); relevant if aligned corpora matter.

**TXM (Textométrie, ENS Lyon)** — a desktop (Eclipse RCP/Java) and GWT web-portal text-analysis environment that embeds CQP as its search engine and R for statistics; unlike all the above it talks to CWB through the **cqpserver/CQi socket protocol** rather than piping into `cqp -c` (https://sourceforge.net/projects/txm; TXM leaflet, https://txm.gitpages.huma-num.fr/textometrie/files/documentation/TXM%20Leaftlet%20EN.pdf). Actively maintained; heavyweight in a different direction (desktop platform + portal).

**spheroscope / cwb-ccc (FAU Erlangen, Philipp Heinrich)** — spheroscope is "a web-based GUI for the development of complex queries with the help of CQP macros" (https://github.com/ausgerechnet/spheroscope), built on **cwb-ccc** (https://github.com/ausgerechnet/cwb-ccc), an actively maintained Python library (adapted from cwb-python/Perl `CWB::CL`) providing concordancing, collocates and keywords as pandas DataFrames with its own result caching. For a new minimal Python frontend, cwb-ccc is the closest thing to an off-the-shelf CQP integration layer.

## 5. Synthesis

### (a) The minimal day-to-day feature set

Across CQPweb's and Korp's manuals, the workflows every corpus linguist actually exercises daily reduce to:

1. **Query**: raw CQP always available, plus one simplified entry path (CEQL in CQPweb; Korp's "simple search" builds `[word = "..."]` CQP under the hood). Both systems treat the simple syntax as sugar compiled to CQP — never a second query engine.
2. **KWIC concordance**: paging, left/right/keyword/random sort, adjustable context, match highlighted, click-through to wider context, and per-token/per-text attribute display (Korp's sidebar; CQPweb's popup) — this is where s-attribute metadata surfaces.
3. **Frequency breakdown of matches** grouped by an attribute (Korp `/count` = CQPweb "frequency breakdown") with relative frequencies per subdivision — arguably the single most used analytic view.
4. **Distribution over metadata** (text class, year) — CQPweb distribution / Korp timespan.
5. **Collocation** around the node with standard association measures.
6. **Export** (CSV of KWIC and frequency tables).
7. Restriction to a **subcorpus** defined by metadata.

Everything else — word picture, trend diagrams, maps, comparisons, keywords between corpora, user accounts, categorisation, uploads — is what pushed both flagship systems into MySQL dependencies, offline pipelines, and account administration. Notably, Korp's core (`/query`, `/count`, `/attr_values`, `/info`) needs **no database at all**; the DB exists only for the word-picture/lemgram/trend extras (korp-backend README).

### (b) Integration patterns with cqp worth copying

- **Batch-mode child process per request** (`cqp -c -r <registry>`, script on stdin, read stdout to EOF) — used by Korp and fglossa; simplest possible lifecycle, no daemon state, crash isolation for free. CQPweb's long-lived interactive pipe with the `.EOL.;` → `-::-EOL-::-` sentinel (also used inside Korp's batch scripts to delimit sections) is only needed if one process must serve multiple round-trips. CQi/cqpserver (TXM's route) buys nothing for a single-language web backend and adds a protocol implementation.
- **The standard command prologue**: `set PrettyPrint off;` → corpus activation → `show cd; .EOL.;` to learn the attribute inventory at runtime (so the parser knows the order of `/`-joined values) → query → `size Last;` → display settings (`show +attrs`, `set Context`, `set LeftKWICDelim/RightKWICDelim` with unambiguous sentinel strings, `set PrintStructures`) → `cat Last <start> <end>;` for exact pagination. This is the whole KWIC protocol; Korp's `query.py` is a complete reference implementation including the annoying parsing edge cases (structural tags split mid-token, `<`/`>` inside attribute values, over-long sentences CQP can't print).
- **Cache by saving named queries via CQP itself**: `set DataDirectory "<cache>"` + `save <name>` on first run, `Last = <name>;` to restore — both CQPweb and Korp do this; it makes re-sorting and paging O(page) instead of O(query), using only CWB's native mechanism (no database).
- **Hash-keyed caching + cheap invalidation**: key = hash(cqp, within, cut, flags) per corpus; invalidate by bumping a per-corpus version prefix derived from registry-file mtime (Korp `cache_prefix`) rather than deleting entries.
- **`tabulate … > "| sort | uniq -c | sort -nr"`** for grouped frequencies — CQP can pipe its own output through shell tools, avoiding shipping every match through the app process; `cwb-scan-corpus` for whole-corpus frequency lists.
- **Client-echoed result metadata** (Korp's `query_data` blob) — pagination without server-side session state.
- **Kill-on-disconnect**: both Korp (abort_event + psutil child kill) and fglossa (process cap + killall) guard against runaway CQP queries; a minimal frontend needs at least a timeout+kill.

### (c) Complexity traps to avoid

1. **A second datastore.** CQPweb's single biggest installation and administration burden is MySQL (engine/tablespace lore, `LOAD DATA LOCAL INFILE` and `secure_file_priv` gotchas occupying pages of the manual, cache-leak hunting); Korp only stays light because its DB is optional. Everything in the minimal feature set above can be served from CWB indexes plus a hash-keyed file cache. If relational metadata is truly needed, fglossa's SQLite choice is the escape hatch.
2. **Features that require offline precomputation.** Word picture, trend diagram and lemgram search each drag in a tagging pipeline (Sparv) and DB schema; they are per-institution features, not corpus-linguistics universals.
3. **Accounts, privileges, uploads.** CQPweb's user/grant/user-corpus system is a service-operator feature set; a minimal tool should delegate access control to the web server or network layer.
4. **Parsing CQP in the frontend.** korp-frontend maintains a partial Peggy grammar that admits it will fail on user queries; treat CQP strings as opaque and let cqp itself be the only parser (build simple-search UIs by *generating* CQP, never by round-tripping it).
5. **One widget library per results view** (SlickGrid + Rickshaw + Chart.js + Leaflet + jQueryUI in one bundle) and **framework strata** (AngularJS 1.x under TypeScript/Tailwind) — the maintenance cost of korp-frontend is mostly archaeology, not features.
6. **Executable, multi-layer configuration.** Korp's config.yml + JS mode files + backend YAML with "later layer wins, nested values not merged" semantics, and CQPweb's dozens of config variables plus per-corpus web setup, both show the cost of configurability-first design. A minimal frontend can read nearly everything it needs (attributes, sizes, charset) from the registry file and `show cd; info;` at runtime, leaving configuration to: registry path, list of exposed corpora, and display preferences.
7. **Aggressive caching as a design axiom.** CQPweb's cache made sense for classroom load in 2008; it now requires its own management chapter and admin scripts. Cache exactly one thing (saved named queries keyed by hash, with mtime-based expiry) and measure before adding more.