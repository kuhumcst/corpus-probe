# KORP at KU (alf.hum.ku.dk/korp): installation survey and minimal-replacement feature cutline

## 1. What is installed and what responds

**Frontend.** https://alf.hum.ku.dk/korp/ serves Språkbanken's `korp-frontend` (AngularJS single-page app, single `bundle.js` ~1.7 MB; version string `9.1.0` embedded in the bundle). The HTML still carries Språkbanken boilerplate (Swedish meta description, links to spraakbanken.gu.se user docs, Sparv, `/korplabb`) plus KU branding (nors.ku.dk, cms.ku.dk logo) and a link to the CQP tutorial PDF (http://cwb.sourceforge.net/files/CQP_Tutorial.pdf). UI languages: Danish and English. Mode configuration is loaded per-mode from plain JS files at `https://alf.hum.ku.dk/korp/modes/<mode>_mode.js` (e.g. `default_mode.js`, 200 OK); there is no `config.js` or `modes/common.js` (404).

**Backend.** The bundle hard-codes `korpBackendURL="https://alf.hum.ku.dk/korp/backend"`. Probe results:

| URL | Result |
|---|---|
| `https://alf.hum.ku.dk/korp/backend/info` | 200 — `{"version":"8.1.0","cqp_version":"CQP version 3.4.27", "corpora":[158 ids], "protected_corpora":[]}` (korp-backend 8.1.0 wrapping CWB/CQP 3.4.27) |
| `.../backend/corpus_info?corpus=...` | 200 — per-corpus positional/structural attributes + registry info |
| `.../backend/query` (CQP KWIC) | 200 — works (tested `[word="klima"]` on LSPCLIMATEDMU with `show=word,pos,lemma&show_struct=text_title`) |
| `.../backend/count?group_by=lemma` | 200 — works (absolute + relative frequencies per corpus) |
| `.../backend/count_time`, `.../timespan` | 200 — works (year-bucketed token counts; a MySQL timedata table exists) |
| `.../backend/loglike` | 200 — works (log-likelihood comparison between two corpus sets) |
| `.../backend/relations` (word picture) | 200 but **empty** — no relations data in MySQL |
| `.../backend/lemgram_count` | **ERROR** — `Table 'korp.lemgram_index' doesn't exist` (no lemgram index installed) |
| `.../backend/struct_values` | **ERROR** — `[Errno 2] No such file or directory: '/usr/local/cwb/bin/cqp-scan-corpus'` (misconfigured/missing CWB scan binary) |
| `https://alf.hum.ku.dk/korp/api/info` | 404 (no `/api` prefix; the base is `/korp/backend`) |

`protected_corpora` is empty, so the login machinery in the UI (`/authenticate` endpoint referenced in the bundle) protects nothing in practice.

## 2. Corpora available (158 CWB corpora, all Danish, all written/transcribed text)

The frontend organises them into 11 modes (from `modeConfig` in bundle.js): `default` (modern_texts), `FT`, `memo_all`, `medieval_ballads`, `memo_yearcorpora`, `threats`, `saxo_danish`, `memo_frakturcorr`, `memo_authornovels`, `memo_frakturgold`, `da1800`. **There are no LANCHART/Sprogforandringscentret spoken corpora, no parallel corpora (alignment attribute list `"a": []` for every corpus checked), and no audio.** The nearest thing to speech is FT_KORPUS (transcribed parliamentary speeches, metadata at `<text>` level).

Corpus groups (ids from `/backend/info`; titles/descriptions from the `modes/*_mode.js` files):

- **LSP / “CLARIN Fagsprogligt Korpus (LSP)”** (~13 M words per `default_mode.js`; DK-CLARIN specialised-language corpus): `LSPCONSTRUCTION*`, `LSPBYGGERI*`, `LSPAGRICULTUREJORDBRUGSFORSKNING`, `LSPCLIMATE*`, `LSPECONOMICS*`, `LSPHEALTH1*`, `LSPHEALTH2SUNDHEDDK1–5`, `LSPIT*`, `LSPNANO*`, `GLDALSPBYGGERIA` (one sub-corpus, `lsphealth2sundheddk4`, is commented out in config as broken).
- **MEMO / MeMo** (“MeMo-korpusset med både antikva og fraktur” — Danish novels 1870–1899): year corpora `MEMO_1870`–`MEMO_1899`, `MEMO_ALL` (64.6 M tokens), author-novel corpora (`MEMO_BANGH_*`, `MEMO_PONTOPPIDAN_*`), OCR-corrected fraktur (`MEMO_FRAKTUR_CORR_*`) and a gold standard (`MEMO_FRAKTUR_GOLD`).
- **FT_KORPUS** — “Folketingstidende. Taler fra folketingssalen 2009–2017” (The Danish Parliament Corpus 2009–2017 v2), 46 975 328 tokens.
- **Medieval ballads / “Ældre danske tekster: folkeviser mv”**: `DUDSDFKBILL*` (“Den ældste danske viseoverlevering — Jens Billes haandskrift (1557–1559)”; a line of verse is a `sentence`, a stanza a `p`).
- **Saxo**: `SAXOA`, `SAXOS`, `SAXODEL01–16` (“Saxo Grammaticus delt op i bøger”; note duplicate legacy ids SAXODEL1 vs SAXODEL01).
- **da1800**: `LIT1800JPJNL(T)` — “Dansk 1800-tals litteratur (adl.dk)”, J.P. Jacobsen *Niels Lyhne* test versions.
- **Threats**: `THREATS*` / `THREATS_*` — “Danske trusler. Trusselsbreve mv.” (tiny; THREATSART = 1 853 tokens) with rich forensic metadata.
- `TASTCORPUS`, `TESTCORPUS` — test corpora.

**Attributes of representative corpora** (from `/backend/corpus_info?corpus=MEMO_1880,LSPCLIMATEDMU,FT_KORPUS,DUDSDFKBILLALL,THREATSART,MEMO_ALL,SAXOA`):

| Corpus | Positional attrs | Structural attrs (selection) |
|---|---|---|
| LSPCLIMATEDMU (994 k tokens) | `word, pos, msd, lemma` | `sentence(_id), p(_idp), text_title, text_date(from/to), text_time(from/to)` |
| MEMO_1880 (554 k) | `word, pos, pos2, lemma` | `text_title, text_author, text_pseudonym, text_gender, text_nationality, text_publisher, text_price, text_typeface, text_pages, text_illustrations, corpus(_title/datefrom/dateto)` |
| MEMO_ALL (64.6 M) | `word, normalized, lemma, pos, wordnum_in_sentence, wordnum_in_line, wordnum_global, linenum, pagenum` | ~46 text_* metadata fields (file provenance, period, quarantine/discard flags, …), `paragraph(_id), sentence(_id)` |
| FT_KORPUS (47 M) | **`word` only** — no PoS/lemma | speech metadata on `<text>`: `text_name, text_gender, text_party, text_role, text_MPtitle, text_birth, text_age, text_date, text_timeFrom, text_timeTo, text_duration, text_agendaTitle, text_subject1/2, text_caseNo, text_caseType` |
| DUDSDFKBILLALL (44.6 k) | `word, neutral, lemma, pos, homograf` | `sentence_type, p(_id), text_title, text_date(from/to)` |
| THREATSART | `word, pos, msd, lemma` | `text_Instrument, text_Platform, text_Mediatype, text_Domain, text_Juridoutcome, text_SenderTargetrelation, text_Sender/Victim type/ID/age/gender, …` |

PoS tagsets are corpus-family-specific; `default_mode.js` defines a 12-value dataset (`ADJ ADV CONJ INTERJ N NUM OTHER PREP PRON PROPN UNIK V`) for the modern corpora. Every mode file sets `settings.wordpicture = false`, `settings.autocomplete = false`, `settings.lemgramSelect = false` — i.e. KU has switched off the three features that need Språkbanken's MySQL lemgram/relations infrastructure, matching the backend errors above.

## 3. Standard KORP feature set (for comparison)

From the korp-frontend/korp-backend documentation (https://github.com/spraakbanken/korp-frontend README + `doc/frontend_devel.md`; https://github.com/spraakbanken/korp-backend README; API spec https://ws.spraakbanken.gu.se/docs/korp):

- **Search**: Simple search (word/prefix/suffix, case options), Extended search (token-by-token attribute/operator builder), Advanced search (raw CQP); lemgram autocomplete.
- **Result views**: KWIC with context expansion + paging; Reading mode (full-text display); Statistics tab (`/count`, group-by any attribute, relative/absolute, export, sub-search drill-down); Trend diagram (`/count_time` + timespan, needs time DB); Word picture (`/relations`, needs MySQL relations tables); Comparison of two saved searches (`/loglike`); Map (geocoded struct attributes); dependency-tree view.
- **Interface**: corpus chooser with folder tree and token counts; modes (separate config universes); sidebar showing all positional/structural attributes of the clicked token; localisation; news widget; URL hash state = shareable/bookmarkable searches.
- **Data**: KWIC/statistics export (CSV etc., frontend download plus a `korp_download` cgi at Språkbanken); JSON link to the raw API call.
- **Infra**: backend is a thin Flask/WSGI wrapper over CWB (`cqp`, `cwb-scan-corpus`); Memcached caching; MySQL/MariaDB only for word picture, lemgram index and trend-diagram time data; `limited_access` corpora behind authentication.

## 4. Scoped feature cutline for a minimal faithful CWB web frontend for these corpora

Grounded in: 158 monolingual Danish written corpora, no alignment, no protected corpora, no lemgram/relations DB, heavy per-corpus-family variation in positional attrs (1–9) and structural attrs (5–46), a working `/query`+`/count`+`/loglike`+`/timespan` backend today.

### MUST-HAVE
- **Corpus selection with folder grouping and preselection** — the 158 corpora are only usable via the mode/folder structure (LSP domains, MEMO years, Saxo books); flat lists fail at this scale. Per-corpus title/description from config (mirrors `settings.corporafolders`).
- **Multi-corpus search** — Korp queries many corpora in one request (`corpus=A,B,C`); MEMO-year and LSP-domain workflows depend on it.
- **CQP query input (advanced mode)** — the installation links the CQP tutorial; CQP is the lingua franca and the escape hatch that makes everything else optional.
- **Simplified query entry** — one “simple search” box (word/lemma, case-insensitive, prefix/suffix wildcards) compiling to CQP, plus a minimal extended builder over the *actual* attributes (`word`, `lemma`, `pos` with the 12-value Danish dataset, `msd`); FT_KORPUS must degrade gracefully to word-only.
- **KWIC with paging and expandable/sentence context** — core deliverable; backend `/query` already supplies `context`/`within` (`1 sentence` default here; ballads use verse-line sentences, threats use `1 paragraph`), `start`/`end` paging.
- **Token attribute display (sidebar or hover)** — pos/lemma/msd per token plus **all structural attributes of the hit** — indispensable here because the scholarly value of MEMO (author/gender/publisher/typeface), FT (speaker/party/role/age/date/duration) and THREATS (sender/victim/outcome) lives in `text_*` metadata. Config-driven label ordering per corpus (as in `FT_mode.js` `order:` fields).
- **Metadata filtering in queries** — CQP structural constraints (e.g. `[word="klima"] :: match.text_party="S"`); with per-text metadata this rich, filtering is a primary use case. (Backend supports it via plain CQP even with `struct_values` broken; value lists can be precomputed offline with `cwb-scan-corpus`.)
- **Frequency counts / group-by (statistics)** — `/count` with group_by lemma/pos/word and per-corpus relative frequencies works today and is the second most-used Korp view.
- **Export** — KWIC and statistics as CSV/TSV (frontend-side is enough), plus a “show JSON/API call” link for reproducibility.
- **Shareable URL state** — search params in the URL hash; cheap and heavily relied on for citation in teaching/research.
- **Danish/English UI localisation** — the current install ships both and its users are mixed.

### NICE-TO-HAVE
- **Trend diagram / time distribution** — `/count_time` and `/timespan` work and MEMO year corpora + FT (2009–2017) are date-stamped; valuable but needs date-metadata curation (LSPCLIMATEDMU already shows a spurious empty 2009 bucket).
- **Comparison of two searches (log-likelihood)** — `/loglike` works today and suits the LSP domain-contrast design; low frontend cost, but a power feature few use.
- **Reading mode / full-text view** — useful for MEMO novels and ballads (page/line numbers exist as positional attrs in MEMO_ALL), but expandable KWIC context covers 80 %.
- **Struct-attribute value pickers** (dropdowns of parties, authors, domains) — requires fixing/precomputing what `struct_values` should return; ship free-text filters first.
- **Result sorting** (by match word, left/right context, random sample) — supported by CQP/backend `sort` param, modest UI cost.
- **Backend caching (Memcached or equivalent)** — matters for MEMO_ALL/FT-sized counts, not for correctness.

### DELIBERATELY-DROPPED
- **Word picture (relations)** — disabled in every KU mode file and the backend returns no data; would require building a MySQL dependency-relations database that has never existed here.
- **Lemgram autocomplete / lemgram search / `lemgram_count`** — disabled in config and the `korp.lemgram_index` table does not exist; plain lemma search over the `lemma` attribute covers the need.
- **Map view** — no geocoded structural attributes in any corpus inspected; pure dead weight (the Stamen/OSM tile code is a large chunk of the current bundle).
- **Parallel-corpus support** — every corpus has `"a": []`; no alignment exists to display.
- **Dependency-tree view** — no dependency annotation in any corpus (`ref`/`msd` only, no deprel/dephead).
- **Authentication / protected corpora / user accounts** — `protected_corpora` is `[]`; login code adds server state for zero corpora.
- **News widget, Matomo analytics, “Korp Lab”, Sparv import-chain links** — Språkbanken-service chrome irrelevant to a KU-local install (the current page still ships them as vestiges).
- **Word-picture-era MySQL dependency entirely** — a minimal replacement can be a stateless wrapper over `cqp`/`cwb-scan-corpus` (plus optional precomputed per-year token tables for trends), eliminating the class of breakage observed (`lemgram_index` missing, `cqp-scan-corpus` path wrong).
- **Full extended-search operator zoo** (“starts with/contains/regexp per attribute” matrices) — with `autocomplete`/`lemgramSelect` off, KU users already work close to CQP; simple box + raw CQP brackets the same space at a fraction of the UI surface.

Sources: https://alf.hum.ku.dk/korp/ ; https://alf.hum.ku.dk/korp/bundle.js ; https://alf.hum.ku.dk/korp/backend/info ; https://alf.hum.ku.dk/korp/backend/corpus_info?corpus=MEMO_1880,LSPCLIMATEDMU,FT_KORPUS,DUDSDFKBILLALL ; https://alf.hum.ku.dk/korp/backend/{query,count,count_time,timespan,loglike,relations,struct_values,lemgram_count} probes (2026-09-03); https://alf.hum.ku.dk/korp/modes/{default,FT,memo_all,medieval_ballads,threats,saxo_danish,da1800,memo_yearcorpora,memo_frakturcorr,memo_authornovels,memo_frakturgold}_mode.js ; https://github.com/spraakbanken/korp-frontend (README, doc/frontend_devel.md); https://github.com/spraakbanken/korp-backend (README); https://ws.spraakbanken.gu.se/docs/korp.