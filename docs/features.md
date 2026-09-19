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

## The folded form

Once a search has hits, the boxes under the query fold away under one
line that says Search options. The query line stays, and the answer takes the
whole width under it. Open, the boxes stand where they always stood,
and a muted line at the foot of the rail folds them again. The fold is
a disclosure, so it opens without a script, and the controls it hides
still submit with the form. A new search folds it again. The other
view of the same answer, another page of it, or the other language
leaves it as the reader left it. Nothing folds while there are no
hits, since the boxes are then what the reader needs. With a script,
the fold is animated: the answer and the rail slide to their new
places (see Motion). A new search folds it as its answer lands, an
answer onto the bare page folds it around itself, and emptying the
field unfolds it as the answer goes.

## Narrow screens

On a narrow screen, the concordance keeps its columns and lets the text
wrap. Thus the aligned match column, which makes the concordance easy
to scan, survives on a phone. Without wrapping, the table is 807px wide
on a 375px screen, and the match is off the screen. The source column
is hidden, so that the contexts have room.

## Focus

The client decides where focus goes whenever it changes what is on the
page. The rules are few, and `dk.cst.corpus-probe.client.focus` holds
them all.

A landing is a place the client may put the reader that is not a tab
stop: the main content, the results, the concordance and its card, the
metadata box, the preferences box and the history. Each is focusable
by its id, Tab skips it, and it shows no focus ring, since a ring
around a whole region tells the reader nothing.

One rule needs no asking. A control that goes away, or goes quiet,
under the reader's focus hands it to the nearest landing that survives.
A browser drops focus from a control it disables or takes out of the
document, and a reader dropped on the document has lost their place.
Thus the Save button, the Clear filter box, the Clear button of the
history and the last control of a filter all leave the reader on their
box, and no action says so. A control that stays keeps focus, and a
reader who moved focus themselves is left where they went. A control
that can go from under the reader stands in a landing; that is the
whole of what a new one has to do.

An action that knows a better place says so. A token or a condition
added to the extended form takes focus, and one removed hands it to the
row in its place. A step of the concordance's cursor takes focus with
it, and the page scrolls to keep the token in view, clear of the card.
The card's close button, and Escape in the card, put focus back in the
concordance, from where Tab reaches the cursor again.

A routed navigation lands the reader as a real one would: at the place
the URL's fragment names, else on the results when the page has any,
else at the start of the main content. A real page load announces
itself and resets focus; one the client makes must do the same by hand,
or a reader who is not watching the screen is told nothing. It leaves
focus alone where a control of the search form holds it: the form
outlives a search, so the reader is still standing where they submitted
from. The page still scrolls to the results. A search that found
nothing leaves the query selected when the reader asked from the field
or its button (see Preferences).

Focus is read as well as moved. Focus settling outside the concordance
and its card closes the card. Focus leaving a chooser for a tab stop
outside it, or a press elsewhere on the page, puts the list at rest,
and focus entering its find box engages it.

## Motion

Motion says that something arrived, opened or changed, and never
decorates. Every rule of it stands in one block of the stylesheet,
headed Motion, under the query for a reader who has not asked their
system for less; nothing moves for one who has. It has two lengths,
quick for what changes or goes and longer for what comes, since the eye
needs longer to read an arrival than a departure; two curves, since
what comes slows to a stop and what goes speeds away; and one look,
faded and blurred, which is what busy contents fade to and what
everything that arrives starts from. All of it is inert until the
client says its first render has settled: that render makes every
element the server drew anew, and none of them is an arrival. So motion
needs a script, as the card does. The client reads from the stylesheet
whether motion is on and tells the views, which mark a departure only
then: a leaving element is kept until its transition ends, and with
none to wait for it would be kept for nothing.

There are five kinds. What the client puts on the page comes into
focus from the look: a view marks it with `widgets/arrival-attrs`,
which has it arriving for its first frame, and asks nothing else. A row
added to the extended form arrives this way, and so does an answer: the
results region is keyed by the question, so a new question mounts it
anew and it comes into focus where the old one faded out, while a page,
a sort or a view of the same question changes in place. So does every
page: its main content is keyed by its path, so a corpus page or a
document comes into focus whole under the masthead's tabs, while the
search page, one path for every search, changes in place; and so do the
help and the history when the field is emptied. The card also drops
from its token, or slides up from the foot as a sheet, which its own
arriving and leaving states add.

What comes and goes outside the flow leaves the same way, on to
nothing, marked with `widgets/departure-attrs`. In the flow it goes at
once: a leaving element keeps its place until its fade ends, and what
arrives would jump when it went. So the card fades out, and a removed
row does not.

What a disclosure opens arrives as anything does, whichever disclosure
it is; what it shuts goes at once. A fold's marker turns rather than
being redrawn.

A state eases: the cursor's fill, a token fading at the edge of the
window and a control going quiet move over the quick length rather than
switching. What an answer on its way will replace is busy, marked with
`widgets/busy-attrs`, and fades out to the look until it lands: the
results while the next page is fetched, the help and the history while
a first answer is, the metadata box while its filters are, and the
whole of any other page while the next one is; the search page keeps
its form, which outlives a search, and the answer alone keeps its box
crisp and blurs what is in it, since the tabs stand on its top edge.
Going out is quick; the answer then comes in over the arrival's length,
and the pointer says progress meanwhile. The words for the wait are
spoken and never seen. A step is no arrival, though: with the card up,
the fill snaps to the next token as the card does. The scrolls are the
client's, which alone knows a step from an arrival, and glide only
while motion is on: the concordance glides between two tokens of one
page and jumps to a new page or a new width.

What moves whole, the browser pictures and slides. A step that makes
such a move renders inside a view transition, and the client marks the
page with that move's name while it runs, so the block keeps a set of
rules for each move: what the move pictures, and how those pictures
behave. What a move does not name is not pictured and stays live, the
page itself included.

One move is defined: the search options folding, unfolding, or coming
and going. Whatever brings it about, a press on their line, an answer
that folds them as it lands, or the field emptied that takes the answer
away, the rail and the answer's contents slide to their new places over
the arrival's length. One cause is not the reader's: where a search
runs out of time before it finds hits the page says none, and a count
landing later may find some, so the options appear and the page moves
under a reader who did nothing. It is the only such case, and rare
enough to live with. The answer's box is not pictured, so the line the
tabs stand on stays live under the pictures, and what is pictured of
the answer begins at that line, where the help it replaces begins, so
the travel is sideways and nothing drifts down as it widens. The
pictures are clipped, not stretched, and ride the fold together.

Under the pictures the page's own motion stands down: nothing arrives
with a fade and nothing leaves with one, since what is caught mid-fade,
or still leaving, is a picture of the wrong thing. That is why a move
is one change of shape and never a kind of render.

To refine the motion, change the block or the tokens. To give something
new an arrival, mark it; to say it is busy, mark it. To make a change
move whole, add the question that recognises it to the client's moves
and its rules to the block under its name, taking a press from the
browser where it would act first. To add a kind, add its rule to the
block and, where the client has to mark elements for it, a helper
beside the three.

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

The field is required while the page holds no result, so the bare form
reports an empty query rather than searching for nothing. Once there is
a result, submitting an empty field is how the reader starts over: it
goes to `/search`, the bare page, which forgets the view, the order and
the page the result was read in. A search runs only when it asks
something, in either view.

A simple search matches the surface form. The reader can select another
positional attribute of the searched corpora, for example lemma. A
simple search of several words is kept within one sentence, as the CQP
manual advises, or within a paragraph or a text when the reader selects
one. The search uses the name that each corpus gives its sentences.
Any search, CQP included, can ask that every hit have a given word
nearby, within a number of words on either side. The control is a row
of the Scope box, before the case box: the word, and the distance
beside a plus-minus sign. The word is read as the row above reads the
query, in its attribute, with its affix and its case, so a lemma search
near a lemma finds every form of it, and the field's placeholder names
the attribute in force. CQP and the extended form have no such row, so
the word is read there as a whole form of the word attribute, case
included. A blank word asks nothing, and the distance is disabled until
there is one, so nothing rides along with an empty field. This is how
the manual finds a word near a hit, with its command `set target`. The
word is marked as the keyword anchor, and the concordance underlines
it. A search that found nothing keeps the word where it was asked,
since it may be why nothing was found.
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
one, on the search button: its bubble hangs under the control that is
invalid, and on a box in the chooser it would cover the boxes under
it. The chooser opens as the search is refused, so the corpora are
there to tick. Without a script the server refuses it, as an error
where the answer would be.
Pages are numbered from one, as the page numbers itself.

The **Search** link in the masthead is the address of the result being
looked at, its view, its order and its page with it. It keeps that
address while the reader is on a corpus, the glossary or the frontpage,
so that coming back lands where they left. The memory is the client's:
without a script, and on a page loaded afresh, the link is the bare
search page.

## Result controls

A result has its own controls, in one row, which set how the hits are
read: the sort, the context and the sample. The sample is a number
field, blank for all the hits. The sort can order the hits
by the match read from its end, which puts the words that share a
suffix together. This is the `reverse` option of the CQP command
`sort`. The sort can also order the hits by any positional attribute of
the searched corpora, for example lemma or pos. A corpus that lacks the
attribute reports an error, so a silent corpus order never stands in
for the order that was asked. The context can be a number of words, or
a sentence or a paragraph, under the attribute that each corpus has for
it. A target that the reader marks with `@` in a CQP query is shown in
bold, as `cqp` itself shows both anchors. The sample travels with the
search into the frequency view and the exports. Without a script, the
controls apply through a button. A browser with a script never shows
this button.

A search that found nothing, and one that failed everywhere, show no
table, no downloads, no controls and no switch between the views.

## Several corpora

A search of several corpora queries them one at a time until the page
is full. The corpora after the page are only counted. Beside the count
stands a sign. It opens, on a row of its own under the controls, the
corpora the hits are in and the corpora the search left out, which
colour the sign. Without a script,
the page waits for every count. With a script, the page arrives as soon
as it is full, and the count follows. Until it arrives, the heading
gives the hits counted so far, and the number updates itself when the
count lands; nothing says that a count is being made. A count that was
made before is not made again. Thus a page turn, and a return to a
result, wait for nothing.

## Frequencies

The frequency view counts at each position that CQP has:

- the token before the match
- the first or the last token of the match
- the token after the match
- the whole match, as a string

The command `count` gives the whole match. The command `group` sees
only the first token.
Each row links to the hits that it counted. Thus a table is a way into
a concordance, not the end of one. The concordance then says so over
its hits, "Showing only the hits where the lemma is "hund" first in the
match", with a link to all of them, and its title says the same.
That
slice has no control of its own: it is a way of reading one answer,
not a question a reader would compose. It stays while the sort, the
context or the sample change, and goes when the question changes, so a
new query is not sliced by it. Without a script it rides along until
the link is followed. A checkbox adds the number of texts in which
each value occurs.

When the table groups by a structural attribute, for example the year,
each value has text of its own. The table then measures the rate per
million against the tokens of that text, not against the whole corpus.
Thus a year with more text does not look busier. A column shows the
tokens. The tokens come from `cwb-s-decode`, which lists the regions
of the attribute. Under a metadata filter, the app counts the tokens
of the kept regions with the CQP command `group` instead.

The table can count one attribute against another, for example lemma
by year. The select after the position chooses the second attribute:
it reads "per corpus" until one is chosen, and "per text_year" then, so
the row of controls reads as one sentence. Each
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

## Token details

A token in the concordance is a button. Focus on it, from a click or
from the arrow keys, opens a card that names the token: the word as its
heading, the other positional attributes under it, its source corpus
with a link to the text itself, and the structural attributes of the
text under a fold. The fold's line stands at the foot of the card,
reading More shut and Less open, so the rows it opens stand above it
rather than split from the rest. It stays as the reader left it while
the cursor moves, since the text is the same for every token of the
row, and a corpus may mark it with dozens of attributes.

The card is filled with the accent and set in white, which is what says
it floats over the page. Above a phone's width, the token is the card's
title: it takes the same fill while the card is open, and the card
hangs from it and out to its right, where a tooltip would stand,
flipping to the token's left or above it where the window runs out. It
is placed with CSS anchor positioning, so the concordance never moves
to make room for it, and it follows the token as the page scrolls. A
phone, and a browser without anchor positioning, shows the card as a
sheet at the foot of the viewport, which the cursor is kept clear of.
Focus leaving both the concordance and the card closes it, and so does
its own button. The card follows the table in the document, so the tab
order runs from the tokens to the card and on to the pager, and the
cursor's keys still move the cursor from inside the card; Escape there
closes it and puts focus back in the concordance.

The card fades in with a small drop from the token, whose fill comes in
with it, or slides up from the foot as a sheet, and goes the same way
(see Motion).

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

The filter offers the metadata of the corpora that the chooser shows as
chosen. A URL that names no corpus searches every corpus, but a form
that shows none has chosen none, and offers nothing to filter by. The
empty box keeps its place in the rail. It says that the reader has
chosen no corpus, or that the corpora they chose carry no metadata.

A filter that the reader set stays on screen. A ticked value stays, and
so do a pattern and a range. It makes no difference whether the corpora
still offer the attribute, or whether any corpus is chosen at all. A
field out of the document is a constraint that the form no longer
submits, while the state still holds it. It would come back on the next
corpus that the reader ticks, and they would not know why. When the
reader takes back the last of the filter, the chooser goes, and the
control they used goes with it. Focus lands on the box, as it does
whenever a control goes from under it (see Focus).

The client reads the filter over `/api/filters` when the corpus
selection has settled. It does not wait for the reader to open the box.
The box keeps the attributes that it has until the answer arrives, and
marks itself busy, dimmed until the answer lands (see Motion). It does
not empty itself first. An emptied list loses
its own control, and the row then moves sideways while the reader is
still ticking corpora. No corpus carries no metadata, and the client
answers that without asking.

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
button goes quiet and Reset wakes up, and focus lands on the box (see
Focus). A live region in the box says that it was saved. The region is
clipped and never seen: a reader who is watching the screen has the
buttons to tell them, and one who is not has nothing else. It says so
for the act and not for the state it left, so the next thing the reader
does empties it, and a second save is spoken as the first was.

A reset does ask again: the bare form it leaves behind is the server's
to describe. The reader lands on it as on every page the client fetches
(see Focus).

A search that keeps focus says what it found in a live region instead,
clipped and never seen. It holds the query and the heading, "hund. 3
hits": a live region announces changes alone, and two searches running
to the same count would otherwise leave the line unchanged.

A search that found nothing, or that could not be run at all, leaves the
query selected in the field, so that whatever the reader tries instead
replaces it in one keystroke. Only a search they asked for themselves,
from the field or from the button beside it: a control next to the
result is where the reader is working, and a page they reached by a link
is not a search of theirs, so neither has the caret taken away from it. A
search still being counted has not answered yet, and the field is offered
only once the count says there was nothing.

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

## Recent searches

The search page shows the searches the reader has made lately, in the
column that the result tabs take. It holds that column whenever the tabs
are not in it: before any search, and after one that found nothing or
could not be run, which is where a reader most wants the search that did
find something.

The rail names each search by what it asked, and under that by the
corpora it was asked of, the word its hits had to be near, the metadata
filter it was narrowed by and the hits it found. A reader who has
searched nothing keeps the rail anyway,
saying that their searches appear there, with the Clear button quiet:
the place is worth knowing about before there is anything in it, and the
column does not fill up under the reader as they search.

An entry is a question that the form asked. The reader's settings say
how an answer is read; these say what was asked. So an entry holds the
query, the word its hits must be near, the corpora and the metadata
filter, and holds nothing of the sort, the context, the view or the
page, which belong to the reading of an answer. It holds none of the
narrowings either: a sample of the hits and the subset behind a
frequency row both narrow an answer to a question that the history
holds already. Thus working a control beside a result never writes an
entry.

Emptying the field puts the page back to that start without a submit:
the answer goes, the guide and the rail take its place, the address in
the bar stops citing a result that is no longer on screen and the title
stops naming it. The form itself is left as it stands, so the corpora
the reader chose are still chosen. The address goes onto the history
rather than over it, so a field emptied by accident has a way back. The
guide travels with every search page for this, a kilobyte of it: fetching
it at the moment the field empties would land on whatever the reader
typed next.

The canonical query string of the question is both the entry's link and
its name. The same question asked again moves to the head of the list
instead of repeating. It takes the count with it, and a narrowed answer
counts a part of what the question found, so it reports no count and the
count that the question has stands. The list holds ten searches. The
oldest go when it is longer than that, or when it no longer fits.

The history is stored in the browser, not in a cookie. No request needs
it, and a search of a pasted list of words is longer than a cookie may
be. Thus it needs a script: a reader without one has no rail, as they
have no inspector. **Clear** forgets the whole list, and forgetting is
storing nothing, as it is for the settings. The button goes quiet as it
is pressed, and focus lands on the box (see Focus); a live region says
what happened, clipped and never seen, as the Preferences box does when
it stores.
