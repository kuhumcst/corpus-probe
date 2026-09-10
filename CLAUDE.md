# corpus-probe

A web front end for the IMS Open Corpus Workbench (CWB). It runs `cqp` as a
child process and turns its terminal output into semantic HTML.

- [README.md](README.md) has the design and the setup.
- [docs/architecture.md](docs/architecture.md) has the routes and the
  namespace tree. Keep it current when either changes.
- [docs/features.md](docs/features.md) has each feature in detail.
- [PLAN.md](PLAN.md) is the original plan and the CWB research. It is frozen.
  Do not update it to match the code.

## The development environment

Start it before any check. Do not start servers by hand.

- If port 7373 is in use, the environment is already up. Simon starts the
  same one from IntelliJ, and Cursive writes `.nrepl-port` as well. Attach to
  it and start nothing.
- If it is not up, `.claude/launch.json` has one entry, `dev`. Start it by
  name.
- It gives the app on 7373, the shadow-cljs watch on 9630, and an nREPL.
- The nREPL port is in `.nrepl-port`. The ClojureScript one is in
  `.shadow-cljs/nrepl.port`.
- Evaluate against that running nREPL. Do not start a `clojure -M -e` JVM for
  work the REPL can do.
- The watch sends each ClojureScript edit to the open page. No manual
  `compile app` is necessary between checks.
- shadow-cljs permits one watch for each project. Never start a second one.

CAUTION: Do not stop a server with `lsof -ti :PORT | xargs kill`. That
command lists every process on the port, the browser included, and kills the
browser that holds the page. Use `-sTCP:LISTEN`, or kill the recorded PID.

If the integration tests fail wholesale, run `dev/encode.sh`. The registry
files hold absolute paths, and a move of the checkout invalidates them.

## Traps

- Do not `require ... :reload-all` the server namespace. It breaks Pedestal's
  interceptor identity check, and the JVM then needs a restart.
- After a reload of a handler namespace, call `(restart!)`. The route table
  holds the handler functions from the previous start.
- A PO file edit takes two reloads, and both are easy to miss. Remove
  `.shadow-cljs/builds/app` and build the client again, because the build
  inlines the translation tables with a macro. Then reload
  `dk.cst.corpus-probe.i18n`, because the server reads its tables once when
  the namespace loads. `(restart!)` does not re-read them. Skip the second
  and the server sends the English msgid while the client renders the
  translation, so a new label flashes from one to the other on every load.
  The test suite does not catch it, running in a fresh JVM that reads the
  PO files as they are.
- Do not test CSS with an injected `<style>` element. The
  Content-Security-Policy is `style-src 'self'`, so the sheet never parses and
  `s.sheet` is null. Edit `resources/public/css/style.css` and reload.
- Do not read headers with `curl -I`. Pedestal answers HEAD without the
  Content-Type and Cache-Control that a GET carries. Use
  `curl -s -D- -o /dev/null`.
- After a ClojureScript edit, read the browser console for `BUILD-WARNING`.
  An undeclared Var is a runtime ReferenceError, and the Clojure test suite
  does not compile the client.

### The browser pane is a hidden tab

`document.hidden` is true in it. As a result:

- Timers are clamped to about 1000 ms and `requestAnimationFrame` never fires.
- Real clicks, key presses, focus events and scroll events do not reach the
  page. Drive handlers with synthetic events, and say that you did.
- Styles are not recalculated, so an added class shows no color change.
- Layout reads are live. `getBoundingClientRect` still forces layout, so
  geometry is measurable.

Never quote a timing measured there. For anything scroll-driven or
focus-driven, instrument the page and ask Simon for the numbers.

## Code

- Clojure and ClojureScript follow the `clojure-style` skill. Invoke it for
  every edit.
- Example calls belong in a `(comment ...)` block in the namespace, and the
  block ends with `#_.`.
- Docstrings are one or two lines. A reason for a choice is a short comment
  beside the form, not a paragraph in the docstring.
- Comments say what the code cannot: a rule, a source, a trap. CSS comments
  never name colors or pixel counts, because those drift.
- Do not use em-dashes anywhere, in code, comments, docstrings or documents.
- Semantic HTML is a primary goal. Ask whether the markup can be more
  semantic.
- Add a `TODO` in the source for a deferred design decision. A note in the
  chat is not enough.

## Working with Simon

- Simon commits. Never commit unless he asks.
- Never add a `Co-Authored-By` trailer or a "Generated with" line.
- Pause at each milestone for review. Do not run through several milestones.
- One reviewer agent at most, and only for a wide diff. A review workflow is
  for a big design question, not for every slice.
- For a slice, the check is the test suite in a fresh JVM, a client build, and
  your own read of the diff.
