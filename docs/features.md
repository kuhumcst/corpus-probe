# Features

This document describes what corpus-probe does, feature by feature.
[README.md](../README.md) has the design principles behind it.

## Samples

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

## Narrow screens

On a narrow screen, the concordance keeps its columns and lets the text
wrap. Thus the aligned match column, which makes the concordance easy
to scan, survives on a phone. Without wrapping, the table is 807px wide
on a 375px screen, and the match is off the screen. The source column
is hidden, so that the contexts have room.

## Simple search

The search field reads its text by its shape (`query.mode/shape`): text that
begins as CQP does, with a bracket, a quotation mark, a tag or a group,
runs as CQP; text with a line break is a list, one word per line, which
finds any one of the words; anything else is words in order. The field
is a text area, so that a list can be typed or pasted. On the client
Enter submits the search and Shift+Enter starts a line; without the
client, Enter starts a line and the Search button submits. A line under
the field says how the text is read, with the CQP that words or a list
run as, so that a reader sees the reading before a search is spent on
it and can learn CQP from it.

A simple search matches the surface form. The reader can select another
positional attribute of the searched corpora, for example lemma. A
simple search of several words is kept within one sentence, as the CQP
manual advises, or within a paragraph or a text when the reader selects
one. The search uses the name that each corpus gives its sentences.
Until the reader makes a search, a key to the readings of the field and
to the extended form stands where the results will be, with a link to
the CQP guide, a page of CQP examples.

A list is one token pattern with an alternation, so the attribute, the
case and affix options, the metadata filter and every view work as for
a simple search. The list stays in the URL, so a search for a list can
be shared like any other.

## Extended search

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

## Changing the form of the query

The query has two forms, the field and the extended form, chosen by a
radio. A form whose radio is changed submits the old form's query under
the new radio. The server reads the query as one value
(`dk.cst.corpus-probe.query`) and holds it in the new form as far as
that form can: the field's text seeds the tokens, read by its shape,
and the tokens are handed to the field as the CQP they compile to,
which the field reads.
The extended form cannot read CQP, nor a list of more than fifty words.
Then nothing runs: the form shows what it kept, and a status line under
the radios says what it dropped, so the reader is told before the loss.
The same line names the params of a hand-written URL that the mode does
not read. With the client, the form changes at the click on the radio,
without a round trip, and switching away and back loses nothing while
nothing was edited in between: what the form the last switch left held
is remembered, and a form still holding what that switch handed it
gets the remembered form back. Without the client, a Change mode button
submits the form without its checks, so an empty form can change form
too, and a submit whose query string is not the search's citation is
redirected to it, so the address bar shows the one URL the search has.
Under the tokens, a line shows the CQP they run as. A bare word in a
CQP query is answered with what CQP takes it for, above CQP's own
error. A query kept within a unit of text carries the unit into CQP as
`within s`, `within p` or `within text`, and each corpus renames the
unit after its own attribute, or drops the clause where it marks no
such unit, as it renames the sentence tags.

## Documents

The frontpage, the search help, the CQP guide and the glossary are
Markdown. There is one file per language under
[resources/docs/](../resources/docs/). The files are named like the PO
files, `help.da.md` next to `help.en.md`. The
server parses them into the hiccup that the views render, so the client
needs no parser. If a language has no file, the server tries the
languages that the request accepts, and then English. Thus a reader who
accepts Danish gets Danish before English. Raw HTML in a file renders
as nothing.

The Markdown is CommonMark plus a definition list of this app's own
(`dk.cst.corpus-probe.docs.markdown`). A term is a line that ends in a
colon. Its definition is indented under it. The glossary, the help and
the examples of the CQP guide are written in this form:

```markdown
KWIC {#kwic}:
  Key word in context: a concordance with one hit on each line.
```

## URLs

Each page has a plain address:

| Page | URL |
|---|---|
| frontpage | `/` |
| search page | `/search` |
| corpus index | `/corpora` |
| one corpus | `/corpora/viser` |
| glossary | `/glossary` |
| CQP guide | `/cqp` |
| an export | `/search/kwic.tsv` |

The terms of the glossary name their own ids, `KWIC {#kwic}:`. Thus a
term in the interface links to its entry in each language. Where the
[CQP manual](https://cwb.sourceforge.io/files/CQP_Manual/) has more,
the entry links to its section.

A result URL is a citation. One rule builds it on the server and on the
client (`dk.cst.corpus-probe.url`). The URL names only the settings
that differ from the defaults. A simple search of the word attribute,
in corpus order, with five words of context, is `/search?q=x`. No URL
names a query mode: the field's text, `q`, says it by its shape, words
in order, a list or CQP, and the token fields say an extended search,
and a URL carries only what that mode reads. The
corpora are one comma-separated parameter. When each readable corpus is
selected, the URL names no corpus, because that is the same search.
The search page itself starts with no corpus selected. The reader
selects the corpora first, and the browser refuses a search without
one.
Pages are numbered from one, as the page numbers itself.

## Result controls

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

## Several corpora

A search of several corpora queries them one at a time until the page
is full. The corpora after the page are only counted. Without a script,
the page waits for every count. With a script, the page arrives as soon
as it is full, and the count follows. Until it arrives, the heading
says "at least" and the hits counted so far, and a status line says how
many corpora are being counted. A count that was made before is not
made again. Thus a page turn, and a return to a result, wait for
nothing.

## Frequencies

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

## Exports

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

## Reading a text

The source column of the concordance and the token panel link to a
reading page for the text behind a hit. The page shows the whole text
as prose, with its metadata first and the hit marked, and the link
lands on the hit. The text is one CQP match: a position query expanded
to the corpus's own text attribute, read with no context. Its
paragraphs are the ones the corpus marks. If the corpus marks none,
each sentence is a paragraph. A corpus without a text attribute says
so instead.

## Metadata filter

The metadata filter accepts the values that the reader ticks. It also
accepts a pattern for each attribute, matched as a regular expression.
For an attribute with numeric values, it accepts a range from one
number to another. That is how a reader asks for a decade of years, or
a year of dates. It is also the only way to an attribute with too many
values to list.

## Startup checks

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

## Languages

The interface is served in Danish or English. The server selects the
language from the stored preference of the reader, then from the
`Accept-Language` header, then Danish. No URL names a language. Thus a
shared link does not set the language of the person who opens it. Text
from CWB (query errors, attribute names, corpus titles and corpus text)
is always shown as it is.

The translations are [gettext](https://www.gnu.org/software/gettext/)
PO files under [resources/i18n/](../resources/i18n/). The server reads
them with [pottery](https://github.com/brightin/pottery). Each interface
string is written in the source in English, and that English is its
key. Thus a view reads as the sentence that it renders, and a string
without a translation falls back to English. To add a language, add a
`.po` file and name it in `dk.cst.corpus-probe.i18n.po/po-files`.
Translators work in Poedit or Weblate with [the
template](../resources/i18n/template.pot). The template is extracted from
the source, and the test suite makes sure that it does not drift.

## Preferences

The reader does not build their settings again on each visit. Two
cookies hold them, and no URL names either of them. The first holds the
language (see above). The second holds the settings of the search form:
the corpora, the form of the query, how the query is matched and how a
result is shown. The query itself is not a setting, and neither is the
metadata filter. Both belong to one search.

Only what departs from the app's own defaults is stored. Thus the
settings stay small. A form is not a search, though: a URL that names no
corpus searches them all, but a chooser with every box ticked and one
with none are different, and the form refuses to search from the empty
one. So a selection of every corpus is stored as a scope of its own, and
written out again for the form that reads it. A registry of many corpora
would not fit in a cookie by name.

With a script, the settings are stored as the reader changes them: a
change to any control of the search form stores what the form then says.
Thus a corpus ticked and not searched on survives a walk to the glossary
and back. Only a control the reader works changes anything; arriving
somewhere never does.

Without a script there is nothing to notice a change, so a search stores
the settings it was made with instead. The server can tell a submit from
a followed link: a GET form submits its defaults and its empty fields
too, so a submit never asks in the words of a citation, while a link
this app writes always does. Thus a link that a colleague shares does
not change the settings of the person who opens it, either way.

The **Preferences** box at the foot of the form holds the controls.
**Save** stores the form as it stands. **Reset** forgets what is stored
and returns to the bare form, which is what a reset looks like.
Forgetting is storing nothing, and it leaves the language alone. Save
is offered while the form departs from the defaults and says something
that is not stored already; Reset while there is anything to go back
from. Without a script neither is refused: their state is read when the
page renders, and it goes stale as soon as a box is ticked.

Saving asks the server for nothing, because the server words no part of
the page from the stored settings. The page stays as it is; the Save
button goes quiet and Reset wakes up. Focus moves to the box, because a
quiet button holds no focus and the reader would otherwise be dropped on
the document and lose their place in the form. A live region in the box
says that it was saved. The region is clipped and never seen: a reader
who is watching the screen has the buttons to tell them, and one who is
not has nothing else. It says so for the act and not for the state it
left, so the next thing the reader does empties it, and a second save is
spoken as the first was.

A reset does ask again: the bare form it leaves behind is the server's
to describe. Every routed navigation lands the reader at the top of what
arrived, on the results where a search found any and on the main content
otherwise. A real page load announces itself and resets focus; one the
client makes must do the same by hand, or a reader who is not watching
the screen is told nothing.

**Save automatically** is that storing as you go. It is on until the
reader says otherwise, so only turning it off is stored. Turned off, the
Save button is the only way to store anything; turned on, it stores the
form at once, which is what asking for it to be stored from now on
means, and the Save button has nothing left to do and says so by going
quiet. The choice stores itself the moment it changes, which is the one
thing it cannot leave to the button beside it.

The stored settings fill a form that asks for nothing: the bare
`/search`. A URL that asks anything is read as it stands. Thus a result
URL is still a citation, and it finds the same hits for everybody. A
seeded form runs no search of its own, because the reader has asked for
none yet.
