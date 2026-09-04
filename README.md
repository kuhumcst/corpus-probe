# corpus-probe

A minimal, faithful web frontend for the [IMS Open Corpus
Workbench](https://cwb.sourceforge.io/) (CWB) in Clojure(Script): it drives
CWB's query processor `cqp` as a child process and translates its terminal
output into semantic HTML.

See [PLAN.md](PLAN.md) for the full implementation plan and
[docs/research/](docs/research/) for the verified research it rests on.

## Status

Milestones 1 to 4 of 5 are built: the `cqp` child-process driver and
output parsers, the KWIC concordance with paging, sorting and context
expansion, the ClojureScript client, and the breadth features (the corpus
chooser and multi-corpus search, simple search, frequency tables, corpus
info pages, TSV/CSV export). Milestone 5, the cutover, is in progress:
metadata filtering, the Danish and English UI and the startup vetting are
done; the deployment next to KORP remains (see PLAN.md section 12).

Startup vets the installation and logs what it finds. It checks that the
CWB programs can be launched, and runs CQP's own sort pipeline (`sort ... |
gawk`) under the configured locale to see whether it collates the way the
app's own collator does, since when it does not CQP quietly serves corpus
order instead. It then reads every registry corpus once, in the
background, and names the ones CWB cannot open. A corpus that cannot be
read is kept, because the registry says it exists: the chooser shows it
disabled and its info page says CWB has no data for it. Logging goes
through [Telemere](https://github.com/taoensso/telemere), which also backs
SLF4J, so Pedestal's own output lands in the same place.

The interface is served in Danish or English, chosen by the `lang` query
parameter, then by the request's `Accept-Language`, then Danish. What CWB
itself says (query errors, attribute names, corpus titles and corpus text)
is always shown verbatim.

At the REPL, one function call in, plain data out:

```clojure
(require '[dk.cst.corpus-probe.search :as search])

(search/kwic! {:registry "/path/to/registry"} "PROBE" "\"hund.*\" %c")
;; => {:corpus "PROBE" :query "\"hund.*\" %c" :size 5 :rows [0 24]
;;     :hits [{:cpos 9
;;             :left  [{:word "Katten" :pos "NCSD" :lemma "kat"} ...]
;;             :match [{:word "hund" :pos "NCSI" :lemma "hund"}]
;;             :right [{:word "i" :pos "PP" :lemma "i"} ...]
;;             :anchors {:match 9 :matchend 9 :target nil :keyword nil}
;;             :structs {:text_id "t1" :text_title "Hverdag" ...}} ...]}
```

## Development

Requires [Clojure](https://clojure.org/guides/install_clojure) and CWB
(`brew install cwb3` on macOS).

```sh
dev/encode.sh          # encode the dev corpus (PROBE) once
clojure -M:dev:nrepl   # start a REPL; see dev/user.clj for entry points
clojure -X:test        # run the tests
clojure -M:cljs -m shadow.cljs.devtools.cli compile app   # build the client
clojure -M -m dk.cst.corpus-probe.server                  # serve (config.edn)
```

Settings come from [resources/config.edn](resources/config.edn), which is
read from the classpath and so lives inside a packaged jar. An installation
puts its own paths and limits in a file of its own and names it, either way
round:

```sh
CORPUS_PROBE_CONFIG=/etc/corpus-probe/config.edn clojure -M -m dk.cst.corpus-probe.server
clojure -J-Dcorpus-probe.config=/etc/corpus-probe/config.edn -M -m dk.cst.corpus-probe.server
```

That file is merged over the built-in one, so it need only carry what it
changes: the registry it serves, where the query result cache goes and how
large it may grow, the timeouts. The `:folders` tree describes the corpora
rather than the machine, so it stays in the jar. A file that is named but
cannot be read stops the server, and the effective settings are logged at
startup.

The parsers are developed against byte-exact golden files in
[test/resources/golden/](test/resources/golden/), regenerated deliberately
with `dev/capture-golden.sh`. Integration tests skip themselves when `cqp`
or the encoded dev corpus is missing.
