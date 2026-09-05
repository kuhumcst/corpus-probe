# Glossary

The interface uses the words that the Corpus Workbench uses. This page
explains them.

Alignment attributes {#alignment-attributes}:
  Links between the [regions](/glossary#region) of two corpora. The two
  corpora are translations of each other. A corpus page lists the
  alignment attributes of a corpus, if the corpus has any.
  [Section 5 of the CQP manual](https://cwb.sourceforge.io/files/CQP_Manual/5.html)
  describes queries over aligned corpora.

Concordance {#concordance}:
  A list of all occurrences of the search term. Each occurrence is shown
  in its [context](/glossary#context). This interface shows a
  concordance as a [KWIC](/glossary#kwic) and calls it KWIC.

Context {#context}:
  The [tokens](/glossary#token) on each side of a
  [match](/glossary#match). The context control sets the width: a
  number of words, or the sentence or paragraph that contains the match.
  The [position](/glossary#cpos) of a hit opens more context for that
  hit.
  [Section 2.3 of the CQP manual](https://cwb.sourceforge.io/files/CQP_Manual/2_3.html)
  describes the display options of CQP, the context among them.

Corpus {#corpus}:
  A collection of texts that is encoded for the Corpus Workbench. Each
  word is a [token](/glossary#token). Each token has annotations. Each
  text has [metadata](/glossary#metadata). The page [Corpora](/corpora)
  lists the corpora with their size and their attributes.
  [Section 1.2 of the CQP manual](https://cwb.sourceforge.io/files/CQP_Manual/1_2.html)
  describes the data model of a corpus.

Corpus order {#corpus-order}:
  The order of the [hits](/glossary#hit) in the corpus. CWB returns the
  hits in this order. It is the default order of a
  [KWIC](/glossary#kwic).
  [Section 2.9 of the CQP manual](https://cwb.sourceforge.io/files/CQP_Manual/2_9.html)
  describes the other orders.

cpos {#cpos}:
  Corpus position: the number of a [token](/glossary#token), counted
  from the start of the corpus. CWB uses this number as the address of
  a token. The first column of the [KWIC](/glossary#kwic) shows the
  position of each match. A result URL contains positions when hits are
  expanded.
  [Section 1.2 of the CQP manual](https://cwb.sourceforge.io/files/CQP_Manual/1_2.html)
  describes corpus positions.

CQP {#cqp}:
  The Corpus Query Processor: the query language of
  [CWB](/glossary#cwb), and the program that runs the queries. A CQP
  query is a sequence of token patterns: a word form in quotation marks,
  such as `"x"`, or a condition in brackets, such as `[lemma = "x"]`.
  Each value is a [regular expression](/glossary#regex) over an
  [attribute](/glossary#positional-attributes). In the query mode CQP,
  the query goes to CQP as you typed it. In the simple mode, the
  interface writes the query for you. [The CQP
  manual](https://cwb.sourceforge.io/files/CQP_Manual/) has the full
  syntax.

CWB {#cwb}:
  The [IMS Open Corpus Workbench](https://cwb.sourceforge.io/): the
  software that stores the corpora and answers the queries.
  corpus-probe is a front end to CWB.

Frequency {#frequency}:
  The number of times that a value occurs in the [hits](/glossary#hit)
  or in a corpus. The frequencies view counts the values of an attribute
  at a position of the [match](/glossary#match). It counts in each
  corpus and in all corpora together. It also gives each frequency
  [per million](/glossary#per-million).
  [Section 3.4 of the CQP manual](https://cwb.sourceforge.io/files/CQP_Manual/3_4.html)
  describes frequency distributions.

Hit {#hit}:
  One occurrence of the search term. A hit is one line of the
  [KWIC](/glossary#kwic): the [match](/glossary#match) with its
  [context](/glossary#context). The heading of a result gives the number
  of hits. A [sample](/glossary#sample) keeps some of the hits.

KWIC {#kwic}:
  Key word in context: a [concordance](/glossary#concordance) with one
  [hit](/glossary#hit) on each line. The [match](/glossary#match) is in
  the middle of the line, and some words of [context](/glossary#context)
  are on each side. You can read the column of matches from top to
  bottom. CWB calls this display KWIC, and so does this interface. The
  KWIC view lists the hits of a result, and the
  [frequencies](/glossary#frequency) view counts them.
  [Section 2.3 of the CQP manual](https://cwb.sourceforge.io/files/CQP_Manual/2_3.html)
  describes the display options of a KWIC.

Match {#match}:
  The [tokens](/glossary#token) that the query matched in one
  [hit](/glossary#hit). The match is in the match column of the
  [KWIC](/glossary#kwic). A query can mark one token of the match as the
  *target* with `@`, and the KWIC shows the target in bold. The word
  that the near control asks for is the *keyword*, and the KWIC
  underlines it. A [frequency](/glossary#frequency) table counts at one
  position of the match: before it, at its start or its end, over all
  of it, or after it.
  [Section 3.3 of the CQP manual](https://cwb.sourceforge.io/files/CQP_Manual/3_3.html)
  describes the anchors match, matchend, target and keyword.

Metadata {#metadata}:
  The values of the annotated [structural
  attributes](/glossary#structural-attributes): for example the author,
  the year or the title of a text. The metadata filter keeps a search to
  some of the texts. It lists the values that the selected corpora have.
  It also accepts a pattern for each attribute, and a range for an
  attribute whose values are numbers.
  [Section 4.2 of the CQP manual](https://cwb.sourceforge.io/files/CQP_Manual/4_2.html)
  describes structural attributes with annotations.

Per million {#per-million}:
  A [frequency](/glossary#frequency) scaled to one million
  [tokens](/glossary#token) of the corpus. With this scale you can
  compare corpora of different sizes. For example, a word that occurs 3
  times in a corpus of 47 tokens has 63,830 per million.

Positional attributes {#positional-attributes}:
  The annotations of each [token](/glossary#token). Each attribute has
  one value for each token. `word` is the form as written. Most corpora
  also have `lemma`, the dictionary form, and `pos`, the part of speech.
  A simple search matches one attribute, and a [CQP](/glossary#cqp)
  query names the attributes in its brackets. The attribute control
  lists the attributes that the selected corpora share.
  [Section 2.5 of the CQP manual](https://cwb.sourceforge.io/files/CQP_Manual/2_5.html)
  describes how a query uses them.

Region {#region}:
  One stretch of a [structural
  attribute](/glossary#structural-attributes): one sentence, or one
  text. The [metadata](/glossary#metadata) filter shows the number of
  regions with each value. For example, `1591 · 3 regions` means that
  three texts have the year 1591.
  [Section 4.2 of the CQP manual](https://cwb.sourceforge.io/files/CQP_Manual/4_2.html)
  describes regions and their attributes.

Registry {#registry}:
  The list of corpora that CWB has. The registry has one file for each
  [corpus](/glossary#corpus). The file names the attributes of the
  corpus and the location of its data. If CWB cannot read the data of a
  corpus, the interface shows the corpus as unavailable.

Regular expression {#regex}:
  A pattern that a value must match, in the syntax that
  [CQP](/glossary#cqp) uses. For example, `x.*` matches each form that
  starts with x, and `x|y` matches x or y. In a CQP query, each string
  in quotation marks is a regular expression. The simple search escapes
  the characters that you type. Thus a dot in a simple search is a dot.
  [Appendix A.1 of the CQP manual](https://cwb.sourceforge.io/files/CQP_Manual/A_1.html)
  summarises the syntax.

Sample {#sample}:
  A random selection of a given number of [hits](/glossary#hit). The
  interface takes the sample before it counts and sorts the hits. Thus
  you can read a part of a result that is too large to read in full.
  The selection is fixed: the same URL always gives the same sample.
  The interface takes a sample in each corpus separately.
  [Section 3.6 of the CQP manual](https://cwb.sourceforge.io/files/CQP_Manual/3_6.html)
  describes random subsets.

Structural attributes {#structural-attributes}:
  The markup of stretches of [tokens](/glossary#token): sentences
  (`s`), texts (`text`) and other units that the corpus marks. Each
  stretch is a [region](/glossary#region) from a first token to a last
  token. An annotated structural attribute has a value for each region,
  for example `text_year`. The [metadata](/glossary#metadata) filter
  offers these values.
  [Section 4.2 of the CQP manual](https://cwb.sourceforge.io/files/CQP_Manual/4_2.html)
  describes them.

Token {#token}:
  One unit of text, as the corpus is encoded: a word, a number or a
  punctuation mark. The size of a [corpus](/glossary#corpus) is its
  number of tokens. A *type* is one distinct value. A corpus has as many
  types of `word` as it has different word forms.
  [Section 1.2 of the CQP manual](https://cwb.sourceforge.io/files/CQP_Manual/1_2.html)
  describes tokens and their attributes.
