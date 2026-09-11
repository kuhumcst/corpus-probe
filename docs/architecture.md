# Architecture

How the code is arranged. [README.md](../README.md) has the design principles,
[features.md](features.md) has the features, and [PLAN.md](../PLAN.md) has the
plan the app was built from and the CWB research behind it.

## Routes

| Path | Serves |
|---|---|
| `/`, `/glossary`, `/cqp` | the Markdown documents |
| `/search` | the search page, rendered on the server |
| `/search/:file` | one TSV or CSV export |
| `/corpora`, `/corpora/:id` | the corpus index, and one corpus |
| `/corpora/:id/text` | one text, as a reading page |
| `/preferences` | POST only. It stores the settings cookie |
| `/api/counts` | the counts the client asks for after the first paint |
| `/api/filters` | the metadata filters the selected corpora offer |
| `/css/*path`, `/js/*path` | the compiled assets |
| `/fonts/*path` | the page's face, the one asset a browser is told to keep |

The table itself is `dk.cst.corpus-probe.server/routes`. The paths live in
`dk.cst.corpus-probe.url`, so the server and the client build them by one rule.
The two `/api` routes answer transit. Every other page is complete HTML.

## Islands

The source under `src/dk/cst/corpus_probe/` is a set of islands. An island is a
root namespace and the helpers under it. The root is what another island calls
first. A helper can be called directly too, as `clojure.string` is.

The islands are ordered, and no namespace requires an island later in the
order:

```
cqp, hiccup, stats, i18n, query, url, settings, docs, cwb, search, views,
client, server
```

## The tree

```
corpus-probe/
├── deps.edn  shadow-cljs.edn
├── resources/config.edn            ; registry path, sort locale, folder tree
├── src/dk/cst/corpus_probe/
│   ├── cqp.cljc                    ; the lexical rules of CQP: escaping,
│   │                               ;   names, the units of text (§8)
│   ├── hiccup.cljc                 ; hiccup helpers: walking, headings
│   ├── stats.cljc                  ; relative frequencies
│   ├── i18n.cljc                   ; the gettext tables of the interface
│   ├── i18n/                       ; po (the PO files), scan (the template)
│   ├── query.cljc                  ; the query as one value, compiled to
│   │                               ;   CQP from words, lists and tokens (§8)
│   ├── query/                      ; mode (the forms and modes), tokens
│   │                               ;   (the extended form's rows), params
│   ├── url.cljc                    ; paths, landing ids and the one query
│   │                               ;   string a search has
│   ├── settings.cljc               ; the search params a reader stores as
│   │                               ;   their own defaults, in one cookie
│   ├── docs.clj                    ; the Markdown documents as hiccup
│   ├── docs/markdown.clj           ; CommonMark plus a definition list
│   ├── cwb.clj                     ; child-process driver (§5), errors,
│   │                               ;   collation, fan-out under a deadline
│   ├── cwb/parse.clj               ; output parsers -> data (§6)
│   ├── cwb/registry.clj            ; registry entries and the folder tree
│   ├── cwb/corpus.clj              ; show cd, info -> corpus facts, cached
│   ├── cwb/command.clj             ; CQP commands: QueryLock wrapping,
│   │                               ;   narrowing, sorting, counting
│   ├── cwb/tools.clj               ; cwb-describe-corpus, cwb-lexdecode,
│   │                               ;   cwb-s-decode and their parsers
│   ├── search.clj                  ; KWIC, concordance, texts, exports
│   ├── search/opts.clj             ; the options as one corpus runs them
│   ├── search/result.clj           ; results stored or run afresh
│   ├── search/batch.clj            ; the batches a search runs
│   ├── search/cache.clj            ; CQP's saved query results, reaped
│   ├── search/frequency.clj        ; frequency tables and filter values
│   ├── search/export.clj           ; TSV/CSV exports of concordances and
│   │                               ;   frequency tables
│   ├── views.cljc                  ; .cljc shared hiccup: the pages by
│   │                               ;   route, their titles, the chrome
│   ├── views/                      ; widgets, chooser, result, search
│   │                               ;   (tokens, filter), concordance,
│   │                               ;   frequency, corpus (index, chooser,
│   │                               ;   info, reading page)
│   ├── client.cljs                 ; Replicant client: state, dispatch
│   ├── client/                     ; router, effects, lists, actions
│   ├── server.clj                  ; config, CSP, routes, start/stop
│   └── server/                     ; request readers, responses and the
│                                   ;   document shell, the search page
│                                   ;   and its endpoints, corpus pages,
│                                   ;   exports, startup self-checks
├── test/…                          ; golden-file tests against captured outputs
│   └── resources/                  ;   (the captures and a registry entry;
│                                   ;   hostile cases live inline in the tests)
├── dev/                            ; encode.sh, encode-big.sh, capture-golden.sh,
│                                   ;   user.clj, cache_bench.clj
└── docs/research/                  ; the evidence base for this plan
```
