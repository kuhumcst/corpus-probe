# Query help

To do a simple search, type one word or several words in order. A
simple search needs no syntax. To find any word of a list, select List
as the query mode and type one word per line.

The examples below are [CQP](/glossary#cqp) queries. To use one, select
CQP as the query mode. Replace x and y with your own words.

Each example uses one of the common
[attributes](/glossary#positional-attributes): word, lemma or pos. The
attribute control in the form lists the attributes of the selected
[corpora](/glossary#corpus).

`"x"`:
  Finds the word form x. The text between the quotation marks is a
  [regular expression](/glossary#regex).

`"x.*" %c`:
  Finds all word forms that start with x. The `%c` ignores the
  difference between capital and small letters.

`"x|y"`:
  Finds the word form x or the word form y.

`[lemma = "x"]`:
  Finds all forms of the [lemma](/glossary#positional-attributes) x. The
  brackets can hold each attribute of the corpus.

`[pos != "N.*"]`:
  Finds all tokens with a pos tag that does not start with N. The `!=`
  means "is not". The value is also a regular expression.

`[lemma = "x" & pos = "A.*"]`:
  Finds the tokens that satisfy both conditions. Use `&` for "and",
  `|` for "or" and `!` for "not".

`[lemma = "x"] [pos = "N.*"]`:
  Finds a sequence of two [tokens](/glossary#token). Each pair of
  brackets matches one token.

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

`[word = ".*" & strlen(word) >= 12]`:
  Finds all tokens with 12 characters or more.

See [the CQP manual](https://cwb.sourceforge.io/files/CQP_Manual/) for
the full syntax. See [the glossary](/glossary) for the terms.
