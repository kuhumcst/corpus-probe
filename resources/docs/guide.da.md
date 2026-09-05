# Hjælp til forespørgsler

For at lave en simpel søgning skal du skrive ét ord eller flere ord i
rækkefølge. En simpel søgning kræver ingen syntaks.

Eksemplerne nedenfor er [CQP](/glossary#cqp)-forespørgsler. Vælg CQP
som søgetype for at bruge dem. Erstat x og y med dine egne ord.

Hvert eksempel bruger en af de almindelige
[attributter](/glossary#positional-attributes): word, lemma eller pos.
Attributvælgeren i formularen viser attributterne i de valgte
[korpusser](/glossary#corpus).

`"x"`:
  Finder ordformen x. Teksten mellem anførselstegnene er et
  [regulært udtryk](/glossary#regex).

`"x.*" %c`:
  Finder alle ordformer, der begynder med x. `%c` ser bort fra
  forskellen på store og små bogstaver.

`"x|y"`:
  Finder ordformen x eller ordformen y.

`[lemma = "x"]`:
  Finder alle former af [lemmaet](/glossary#positional-attributes) x.
  Klammerne kan indeholde enhver attribut i korpusset.

`[pos != "N.*"]`:
  Finder alle tokens med et pos-tag, der ikke begynder med N. `!=`
  betyder "er ikke". Værdien er også et regulært udtryk.

`[lemma = "x" & pos = "A.*"]`:
  Finder de tokens, der opfylder begge betingelser. Brug `&` for "og",
  `|` for "eller" og `!` for "ikke".

`[lemma = "x"] [pos = "N.*"]`:
  Finder en sekvens af to [tokens](/glossary#token). Hvert par klammer
  matcher ét token.

`"x" []{0,2} "y"`:
  Finder x og y med nul, ét eller to tokens imellem.

`"x" []* "y" within s`:
  Finder x efterfulgt af y i samme
  [sætning](/glossary#structural-attributes).

`"x" @[pos = "A.*"] [pos = "N.*"]`:
  `@` udpeger ét token i matchet som [target](/glossary#match).
  [KWIC'en](/glossary#kwic) viser target med fed.

`a:[] "y" b:[] :: a.word = b.word`:
  Finder et ord, derefter y, derefter det samme ord igen. Etiketterne a
  og b navngiver tokens. Betingelsen efter `::` sammenligner de to
  tokens.

`[word = ".*" & strlen(word) >= 12]`:
  Finder alle tokens på 12 tegn eller flere.

Se [CQP-manualen](https://cwb.sourceforge.io/files/CQP_Manual/) for den
fulde syntaks. Se [ordlisten](/glossary) for begreberne.
