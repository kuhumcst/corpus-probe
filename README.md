# corpus-probe

A minimal, faithful web frontend for the [IMS Open Corpus
Workbench](https://cwb.sourceforge.io/) (CWB) in Clojure(Script): it drives
CWB's query processor `cqp` as a child process and translates its terminal
output into semantic HTML.

See [PLAN.md](PLAN.md) for the full implementation plan and
[docs/research/](docs/research/) for the verified research it rests on.

## Status

Milestone 1 (of 5): the `cqp` child-process driver and output parsers.
One function call in, plain data out:

```clojure
(require '[dk.cst.corpus-probe.search :as search])

(search/kwic! {:registry "/path/to/registry"} "PROBE" "\"hund.*\" %c")
;; => {:corpus "PROBE" :size 5 :page 0 ...
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
```

The parsers are developed against byte-exact golden files in
[test/resources/golden/](test/resources/golden/), regenerated deliberately
with `dev/capture-golden.sh`. Integration tests skip themselves when `cqp`
or the encoded dev corpus is missing.
