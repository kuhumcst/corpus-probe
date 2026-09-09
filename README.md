# corpus-probe

corpus-probe is a web front end for the [IMS Open Corpus
Workbench](https://cwb.sourceforge.io/) (CWB), written in Clojure and
ClojureScript. It starts the CWB query processor `cqp` as a child process. It
turns the terminal output of `cqp` into semantic HTML.

It is an alternative to a KORP installation at the University of Copenhagen,
and a possible replacement for it later. That installation serves 155 Danish
corpora. The largest holds 64.6 million tokens. The deployment beside it is
not done yet, and that KORP installation stays untouched.

- [docs/features.md](docs/features.md) describes each feature in detail.
- [docs/architecture.md](docs/architecture.md) has the routes and the
  namespace tree.
- [PLAN.md](PLAN.md) is the plan the app was built from, kept as written.
- [docs/research/](docs/research/) has the research that the plan rests on.

## Design

- **The server renders every page. The client improves it.** Every page is
  complete HTML from the server. The ClojureScript client takes over after
  that first render. Each feature also works without the client.
- **A URL is a citation.** One rule builds a result URL, on the server and on
  the client. The URL names only the settings that differ from the defaults.
  No URL names a language or a stored preference. As a result, the URL finds
  the same hits for everybody, and it does not change the settings of the
  reader who opens it.
- **CQP stays visible.** A line under the search field shows the CQP that the
  query compiles to. The result heading shows it as well. The field also
  accepts CQP directly. As a result, a reader can learn the query language
  from the interface.
- **One query, several forms.** The query is one value. The search field
  reads its text by shape: CQP, a list of words, or words in order. The
  extended form builds the same query from token rows. When the reader
  changes the form, the app keeps as much of the query as the new form holds.
  A status line says what it dropped.
- **Semantic HTML.** `cqp` writes terminal output. The app writes tables,
  lists and definition lists. As a result, the structure of a concordance
  survives for a screen reader.
- **Nothing disappears.** The corpus tree only adds structure. A corpus that
  no folder names is still in the list. A corpus that CWB cannot open stays
  visible, and the app says why.
- **Danish and English, from PO files.** The interface strings are gettext PO
  files under [resources/i18n/](resources/i18n/). The English string in the
  source is its own key. As a result, a view reads as the sentence that it
  renders. To add a language, add a PO file.
- **Saved results and timeouts.** CQP writes each result to a file. A page
  turn, a new sort and an export read that file instead of the query again. A
  count made before is not made again. The server also refuses work that is
  too slow. A sort of a whole 64-million-token corpus takes more than ten
  minutes, and the query timeout stops it.
- **Islands.** The source under `src/dk/cst/corpus_probe/` is a set of
  islands. An island is a root namespace and the helpers under it. The
  islands are ordered, and no namespace requires an island later in the
  order. [docs/architecture.md](docs/architecture.md) has the tree and the
  order.

## At the REPL

One function call in, plain data out:

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

### Requirements

- Java.
- The [Clojure CLI tools](https://clojure.org/guides/install_clojure).
- CWB and gawk. On macOS, `brew install cwb3 gawk` installs both.

The dev corpora are not in the repository. Encode them:

```sh
dev/encode.sh          # reads dev/corpus/*.vrt
                       # writes dev/corpus/{data,registry}, both gitignored
```

Note: If you move the checkout, the integration tests fail. The registry
files hold absolute paths. `dev/encode.sh` writes them again.

### IntelliJ IDEA

The development environment is one REPL. In Cursive, make this run
configuration:

- Type: **Clojure REPL, Local**
- Execution: **Run with Deps**
- Options: `-M:dev`
- Working directory: the project root

Start the run configuration. It loads [dev/user.clj](dev/user.clj) as the
`user` namespace. Then evaluate these forms, from the comment block at the
end of that file:

```clojure
(start!)   ; the web server and the shadow-cljs watch, in this one JVM
;; => {:app "http://localhost:7373" :watch :watching}

(restart!) ; the server only, not the watch
(stop!)    ; both

config     ; the settings this machine runs on
overrides  ; what this machine adds to resources/config.edn
```

- The watch compiles a ClojureScript file after each save. Then it sends the
  new code to the open page.
- `dk.cst.corpus-probe.client/reload!` draws the page again after each swap.
  The search on the screen stays.
- `(restart!)` is necessary after a reload of a handler namespace. The route
  table holds the handler functions from the previous start.
- The CWB layer takes `config` as its `ctx`. The comment block calls that
  layer with it.
- A Cursive REPL of type **Remote** attaches to an environment that another
  process started. Set it to use the port file `.nrepl-port`. Both this run
  configuration and `clojure -M:dev:serve` write that file.
- For a ClojureScript REPL, connect a second Cursive REPL of type **Remote**.
  Use the port in `.shadow-cljs/nrepl.port`, which the watch writes.
- After a change to a PO file, compile the client again. The build inlines
  the translation tables with a macro. It does not see the change.

### From the command line

```sh
clojure -M:dev:serve   # the server and the watch, then an nREPL (.nrepl-port)
clojure -M:dev:nrepl   # an nREPL only, then evaluate (start!) in it
clojure -X:test        # the Clojure tests
clojure -M:i18n        # the translation template, extracted again
clojure -M -m dk.cst.corpus-probe.server   # the server alone, no dev settings

# the ClojureScript builds
clojure -M:dev -m shadow.cljs.devtools.cli watch app     # the watch alone
clojure -M:dev -m shadow.cljs.devtools.cli compile app   # one build of the client

# the ClojureScript tests
clojure -M:dev -m shadow.cljs.devtools.cli compile test && node target/test.js
```

- [.claude/launch.json](.claude/launch.json) names the first command `dev`,
  for the Claude app.
- shadow-cljs permits one watch for each project. Start only one of these
  commands.

### Settings

[resources/config.edn](resources/config.edn) holds the settings. The server
reads this file from the classpath. As a result, the file is inside a
packaged jar.

- The file holds the paths that `dev/encode.sh` writes. As a result, a new
  checkout needs no other settings.
- `overrides` in [dev/user.clj](dev/user.clj) holds what a development
  machine adds.
- An installation can name a settings file of its own. There are two ways:

```sh
CORPUS_PROBE_CONFIG=/etc/corpus-probe/config.edn clojure -M -m dk.cst.corpus-probe.server
clojure -J-Dcorpus-probe.config=/etc/corpus-probe/config.edn -M -m dk.cst.corpus-probe.server
```

- The server merges that file over the built-in file. As a result, the file
  holds only the settings that it changes.
- Those settings are the registry, the location and size limit of the query
  result cache, and the timeouts.
- The `:folders` tree describes the corpora, not the machine. It stays in the
  jar.
- If the server cannot read the named file, the server stops.
- The server writes the effective settings to the log at startup.

Each port is set in one file only:

| Port | File | Read by |
|---|---|---|
| 7373, the app | `resources/config.edn` | `config` |
| 9630, the watch | `shadow-cljs.edn` | `overrides` |

No port is in the source code. `shadow-cljs.edn` also sets `:strict true`. If
the port is busy, the watch does not start. It does not move to the next free
port.

The Content-Security-Policy blocks the socket of the watch. It also blocks
the `eval` of the module loader of shadow-cljs. The `:dev-client` setting in
`overrides` names the origin of the watch. Then the browser permits both.
The strict policy is the default. Only a development machine widens it.

### Golden files

- The tests compare the parsers against byte-exact golden files in
  [test/resources/golden/](test/resources/golden/).
- When `cqp` or the encoded dev corpus is absent, the integration tests skip
  themselves.

CAUTION: `dev/capture-golden.sh` replaces the golden files. The tests then
compare against the new output of `cqp`. A regression becomes the reference.
