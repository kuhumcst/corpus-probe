# corpus-probe

corpus-probe is a small web front end for the [IMS Open Corpus
Workbench](https://cwb.sourceforge.io/) (CWB), written in Clojure and
ClojureScript. It runs the CWB query processor `cqp` as a child process.
It translates the terminal output of `cqp` into semantic HTML.

See [PLAN.md](PLAN.md) for the implementation plan. See
[docs/research/](docs/research/) for the research that the plan rests
on.

## Status

Milestones 1 to 4 of 5 are complete:

- the `cqp` child-process driver and the output parsers
- the KWIC concordance, with paging, sorting and context expansion
- the ClojureScript client
- the corpus chooser, search in several corpora, simple search,
  frequency tables, corpus pages, and TSV and CSV export

Milestone 5, the cutover, is in progress. The metadata filter, the
Danish and English interface, and the startup checks are complete. The
deployment next to KORP remains (see PLAN.md section 12).

## Features

### Samples

A reader can take a sample of a result that is too large to read. The
concordance keeps a given number of hits. It draws them at random with
the CQP command `reduce`, before it counts or sorts the result. The seed
is fixed. Thus one URL always names the same hits, and a colleague who
opens the link reads the same hits as the sender. The draw is made in
each corpus. Thus a large corpus cannot push a small corpus out of the
sample. The saved result of a corpus does not depend on the other
corpora in the search. The frequency view never uses a sample. A count
of a random hundred hits is a worse answer than a count of all hits,
and it costs the same query.

When a concordance has saved its result, the frequency view of the
same search counts the saved result instead of running the query
again. The saved result is loaded with `Last = <name>` and sorted back
into corpus order, which the document frequency needs. The size of the
file is checked against the size of the result, because a file that
has shrunk reads back zero-filled without an error from CQP. A sampled
result is never counted, and a damaged one is discarded and the query
run.

### Narrow screens

On a narrow screen, the concordance keeps its columns and lets the text
wrap. Thus the aligned match column, which makes the concordance easy
to scan, survives on a phone. Without wrapping, the table is 807px wide
on a 375px screen, and the match is off the screen. The source column
is hidden, so that the contexts have room.

### Simple search

A simple search matches the surface form. The reader can select another
positional attribute of the searched corpora, for example lemma. A
simple search of several words is kept within one sentence, as the CQP
manual advises, or within a paragraph or a text when the reader selects
one. The search uses the name that each corpus gives its sentences.
Until the reader makes a search, a guide of CQP examples stands where
the results will be.

A third query mode, List, takes one word per line and finds any of
them. The compiler turns the list into one token pattern with an
alternation, so the attribute, the case and affix options, the
metadata filter and every view work as for a simple search. The list
stays in the URL, so a search for a list can be shared like any other.

### Extended search

An extended search builds a query from tokens, as KORP does. Each token
is one group of the form. It holds one or more conditions, each with an
attribute of the searched corpora, an operator, a value and a case
option, joined by "and" or "or" as KORP joins them. A token also has a
repeat range and can be made the first or the last word of a sentence.
The operator "any word" matches any token. A value field suggests the
values of an attribute with few of them, such as pos. A search of
several tokens is kept within a sentence, a paragraph or a text, as the
reader chooses. The tokens compile to CQP on the server, and the result
heading shows that CQP, as it does for a CQP query. Each token travels
in the URL as its own fields, `t1.attr=lemma&t1.v=hund`, a second
condition as `t1.2.v=kat&t1.2.join=or`, and a field with its default
value is left out. Without a script, the form
always ends in one empty row, and a reader adds a token by filling it
and searching again. With a script, a button adds a row and another
removes one, and the form shows no empty row of its own. Every row a
reader is asked to fill is required, and the browser reports an empty
one before the search is sent, as it reports an empty query.

### Changing the query mode

A form whose mode radio is changed submits the old mode's field under
the new mode, and names the old mode in a hidden field. The server reads
the query as one value (`dk.cst.corpus-probe.query`), holds it in the
new form as far as that form can, and runs it when the form holds it
whole or reads it in another way. When a part of the query cannot be
held, nothing runs: the form shows what it kept, and a status line under
the modes says what it dropped, so the reader is told before the loss.
The same line names the params of a hand-written URL that the mode does
not read. With the client, the form changes at the click on the radio,
without a round trip, and switching away and back loses nothing while
nothing was edited in between: the query the last switch started from is
remembered, and a form still holding what that switch handed it is read
as the remembered query. A query kept within a unit of text carries the
unit into CQP as `within s`, `within p` or `within text`, and each
corpus renames the unit after its own attribute, or drops the clause
where it marks no such unit, as it renames the sentence tags.

### Documents

The frontpage, the guide and the glossary are Markdown. There is one
file per language under [resources/docs/](resources/docs/). The files
are named like the PO files, `guide.da.md` next to `guide.en.md`. The
server parses them into the hiccup that the views render, so the client
needs no parser. If a language has no file, the server tries the
languages that the request accepts, and then English. Thus a reader who
accepts Danish gets Danish before English. Raw HTML in a file renders
as nothing.

The Markdown is CommonMark plus a definition list of this app's own
(`dk.cst.corpus-probe.markdown`). A term is a line that ends in a
colon. Its definition is indented under it. The glossary and the
examples of the guide are written in this form:

```markdown
KWIC {#kwic}:
  Key word in context: a concordance with one hit on each line.
```

### URLs

Each page has a plain address:

| Page | URL |
|---|---|
| frontpage | `/` |
| search page | `/search` |
| corpus index | `/corpora` |
| one corpus | `/corpora/viser` |
| glossary | `/glossary` |
| an export | `/search/kwic.tsv` |

The terms of the glossary name their own ids, `KWIC {#kwic}:`. Thus a
term in the interface links to its entry in each language. Where the
[CQP manual](https://cwb.sourceforge.io/files/CQP_Manual/) has more,
the entry links to its section.

A result URL is a citation. One rule builds it on the server and on the
client (`dk.cst.corpus-probe.url`). The URL names only the settings
that differ from the defaults. A simple search of the word attribute,
in corpus order, with five words of context, is `/search?q=x`. The
corpora are one comma-separated parameter. When each readable corpus is
selected, the URL names no corpus, because that is the same search.
The search page itself starts with no corpus selected. The reader
selects the corpora first, and the browser refuses a search without
one.
Pages are numbered from one, as the page numbers itself.

### Result controls

A result has its own controls, in two rows. The first row sets how the
hits are read: the sort, the context and the sample. The sort can order
the hits by the match read from its end, which puts the words that
share a suffix together. This is the `reverse` option of the CQP
command `sort`. The sort can also order the hits by any positional
attribute of the searched corpora, for example lemma or pos. A corpus
that lacks the attribute reports an error, so a silent corpus order
never stands in for the order that was asked. The context can be
a number of words, or a sentence or a paragraph, under the attribute
that each corpus has for it. The second row is behind a disclosure. It
narrows the hits to those with a given word nearby, within a few tokens
on each side. This is how the manual finds a word near a hit, with its
command `set target`. The word is marked as the keyword anchor, and the
concordance underlines it. The disclosure opens by itself while a
narrowing is in force. A target that the reader marks with `@` in a CQP
query is shown in bold, as `cqp` itself shows both anchors. The
narrowing and the sample travel with the search into the frequency view
and the exports. Without a script, the controls apply through a button.
A browser with a script never shows this button.

### Several corpora

A search of several corpora queries them one at a time until the page
is full. The corpora after the page are only counted. Without a script,
the page waits for every count. With a script, the page arrives as soon
as it is full, and the count follows. Until it arrives, the heading
says "at least" and the hits counted so far, and a status line says how
many corpora are being counted. A count that was made before is not
made again. Thus a page turn, and a return to a result, wait for
nothing.

### Frequencies

The frequency view counts at each position that CQP has:

- the token before the match
- the first or the last token of the match
- the token after the match
- the whole match, as a string

The command `count` gives the whole match. The command `group` sees
only the first token.
Each row links to the hits that it counted. Thus a table is a way into
a concordance, not the end of one. A checkbox adds the number of texts
in which each value occurs.

When the table groups by a structural attribute, for example the year,
each value has text of its own. The table then measures the rate per
million against the tokens of that text, not against the whole corpus.
Thus a year with more text does not look busier. A column shows the
tokens. The tokens come from `cwb-s-decode`, which lists the regions
of the attribute. Under a metadata filter, the app counts the tokens
of the kept regions with the CQP command `group` instead. A blank
query grouped by a structural attribute is a table of the corpus size
per value.

The table can count one attribute against another, for example lemma
by year. The control `columns` selects the second attribute. Each
value of the second attribute is then a column, and the corpora are
summed. This is the CQP command `group ... by ...`. When the second
attribute is structural, the first row holds the tokens of each
column, and each cell shows the rate per million of those tokens in
parentheses, as KORP shows its statistics. The table holds at most
100 columns, the most frequent. A row still links to its hits. The
export has a frequency column and a rate column per value of the
second attribute.

### Exports

A concordance export is written by the CQP command `tabulate`, one line
per hit, and streamed corpus by corpus. The app holds one corpus's
rows at a time. The export reads the result that the concordance
saved, or runs the query and saves it. It holds at most 500,000 hits,
because the driver still reads one corpus's output whole. The page
says so when the export is cut. The positional attributes of the
match and the structural attributes are one column each, over the
union of the searched corpora. A structural attribute is read with a
`tabulate` of its own, because its values may contain TAB. Where the
concordance shows a sentence or a paragraph of context, the export
shows 20 words on each side, because `tabulate` takes token offsets.

### Reading a text

The source column of the concordance and the token panel link to a
reading page for the text behind a hit. The page shows the whole text
as prose, with its metadata first and the hit marked, and the link
lands on the hit. The text is one CQP match: a position query expanded
to the corpus's own text attribute, read with no context. Its
paragraphs are the ones the corpus marks. If the corpus marks none,
each sentence is a paragraph. A corpus without a text attribute says
so instead.

### Metadata filter

The metadata filter accepts the values that the reader ticks. It also
accepts a pattern for each attribute, matched as a regular expression.
For an attribute with numeric values, it accepts a range from one
number to another. That is how a reader asks for a decade of years, or
a year of dates. It is also the only way to an attribute with too many
values to list.

### Startup checks

At startup, the server checks the installation and logs the result. It
makes sure that the CWB programs start. It runs the sort pipeline of
CQP (`sort ... | gawk`) with the configured locale, to make sure that
it collates as the collator of the app does. When the two do not agree,
CQP serves corpus order without a warning. Then the server reads each
corpus of the registry once, in the background, and logs the corpora
that CWB cannot open. The server keeps a corpus that it cannot read,
because the registry says that the corpus exists. The chooser shows the
corpus as disabled, and its page says that CWB has no data for it.
Logging goes through [Telemere](https://github.com/taoensso/telemere),
which also serves SLF4J. Thus the output of Pedestal lands in the same
place.

### Languages

The interface is served in Danish or English. The server selects the
language from the stored preference of the reader, then from the
`Accept-Language` header, then Danish. No URL names a language. Thus a
shared link does not set the language of the person who opens it. Text
from CWB (query errors, attribute names, corpus titles and corpus text)
is always shown as it is.

The translations are [gettext](https://www.gnu.org/software/gettext/)
PO files under [resources/i18n/](resources/i18n/). The server reads
them with [pottery](https://github.com/brightin/pottery). Each interface
string is written in the source in English, and that English is its
key. Thus a view reads as the sentence that it renders, and a string
without a translation falls back to English. To add a language, add a
`.po` file and name it in `dk.cst.corpus-probe.translations/po-files`.
Translators work in Poedit or Weblate with [the
template](resources/i18n/template.pot). The template is extracted from
the source, and the test suite makes sure that it does not drift.

### At the REPL

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

corpus-probe requires [Clojure](https://clojure.org/guides/install_clojure)
and CWB (`brew install cwb3` on macOS).

```sh
dev/encode.sh          # encode the dev corpus (PROBE) once
clojure -M:dev:nrepl   # start a REPL; see dev/user.clj for entry points
clojure -X:test        # run the tests
clojure -M:i18n        # re-extract the translation template
clojure -M:cljs -m shadow.cljs.devtools.cli compile app   # build the client
clojure -M:cljs -m shadow.cljs.devtools.cli compile test && node target/test.js
                       # run the shared query compiler's tests in JavaScript
clojure -M -m dk.cst.corpus-probe.server                  # serve (config.edn)
```

The server listens on <http://localhost:7373>.

To work on the client, run two processes: a watch that recompiles on
save, and a server that lets the watch through.

```sh
clojure -M:cljs -m shadow.cljs.devtools.cli watch app     # recompile on save
CORPUS_PROBE_CONFIG=dev/watch.edn \
  clojure -M -m dk.cst.corpus-probe.server                # serve, watch allowed
```

The watch pushes recompiled code over a socket on its own port. The
Content-Security-Policy blocks this socket unless a configuration names
it. That is the purpose of [dev/watch.edn](dev/watch.edn). It is a file
outside the jar, not a default, so that the strict policy is the one
that ships. `dk.cst.corpus-probe.ui/reload!` renders again after each
swap. Thus a saved file shows up, and the search on screen stays.

After you edit a PO file, force a recompile of the client. The
ClojureScript build inlines the tables through a macro and does not see
the file change.

Settings come from [resources/config.edn](resources/config.edn). The
server reads it from the classpath, so it is inside a packaged jar. An
installation puts its own paths and limits in a file of its own and
names that file, in one of two ways:

```sh
CORPUS_PROBE_CONFIG=/etc/corpus-probe/config.edn clojure -M -m dk.cst.corpus-probe.server
clojure -J-Dcorpus-probe.config=/etc/corpus-probe/config.edn -M -m dk.cst.corpus-probe.server
```

The server merges that file over the built-in one. Thus the file only
has to contain the settings that it changes. Those are the registry,
the location and size limit of the query result cache, and the
timeouts. The `:folders` tree
describes the corpora, not the machine, so it stays in the jar. If the
named file cannot be read, the server stops. The server logs the
effective settings at startup.

The parsers are developed against byte-exact golden files in
[test/resources/golden/](test/resources/golden/). Regenerate them
deliberately with `dev/capture-golden.sh`. The integration tests skip
themselves when `cqp` or the encoded dev corpus is missing.
