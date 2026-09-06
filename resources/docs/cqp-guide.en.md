# CQP guide

[CQP](/glossary#cqp) is the query language of the Corpus Workbench. A
query is a sequence of patterns. Each pattern matches one
[token](/glossary#token). The examples below use the
[attributes](/glossary#positional-attributes) word, lemma and pos. The
page of each [corpus](/corpora) lists its attributes. Replace x and y
with your own words.

To run a query, type it in the field on the [search page](/search).
Text that begins with a bracket, a quotation mark or a tag runs as CQP,
and the line under the field says so. The controls under Matching are
then gone: the query says what to match. To see how the form writes
CQP, type words in order or one word per line: the line under the field
holds that search as CQP.

## Word forms

`"x"`:
  Finds the word form x. The text between the quotation marks is a
  [regular expression](/glossary#regex).

`"x.*" %c`:
  Finds all word forms that start with x. The `%c` ignores the
  difference between capital and small letters.

`"x|y"`:
  Finds the word form x or the word form y.

## Conditions

`[lemma = "x"]`:
  Finds all forms of the [lemma](/glossary#positional-attributes) x. The
  brackets can hold each attribute of the corpus.

`[pos != "N.*"]`:
  Finds all tokens with a pos tag that does not start with N. The `!=`
  means "is not". The value is also a regular expression.

`[lemma = "x" & pos = "A.*"]`:
  Finds the tokens that satisfy both conditions. Use `&` for "and",
  `|` for "or" and `!` for "not".

`[word = ".*" & strlen(word) >= 12]`:
  Finds all tokens with 12 characters or more.

## Sequences

`[lemma = "x"] [pos = "N.*"]`:
  Finds a sequence of two tokens. Each pair of brackets matches one
  token.

`"x" []{0,2} "y"`:
  Finds x and y with zero, one or two tokens between them.

`"x" []* "y" within s`:
  Finds x followed by y in the same
  [sentence](/glossary#structural-attributes).

`"x" @[pos = "A.*"] [pos = "N.*"]`:
  The `@` marks one token of the match as the
  [target](/glossary#match). The [KWIC](/glossary#kwic) shows the target
  in bold.

`a:[] "y" b:[] :: a.word = b.word`:
  Finds a word, then y, then the same word again. The labels a and b
  name tokens. The condition after `::` compares the two tokens.

See [the CQP manual](https://cwb.sourceforge.io/files/CQP_Manual/) for
the full syntax. See [the glossary](/glossary) for the terms.
