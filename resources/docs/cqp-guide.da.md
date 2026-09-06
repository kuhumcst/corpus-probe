# CQP-vejledning

[CQP](/glossary#cqp) er Corpus Workbenchs forespørgselssprog. En
forespørgsel er en række mønstre. Hvert mønster matcher ét
[token](/glossary#token). Eksemplerne nedenfor bruger
[attributterne](/glossary#positional-attributes) word, lemma og pos.
Siden for hvert [korpus](/corpora) viser dets attributter. Erstat x og
y med dine egne ord.

For at køre en forespørgsel skal du skrive den i søgefeltet på
[søgesiden](/search). Tekst, der begynder med en klamme, et
anførselstegn eller et tag, kører som CQP, og linjen under søgefeltet siger
det. Valgene under Matchning er så væk: forespørgslen siger selv, hvad
der skal matches. For at se, hvordan formularen skriver CQP, skal du
skrive ord i rækkefølge eller ét ord pr. linje: linjen under søgefeltet
viser den søgning som CQP.

## Ordformer

`"x"`:
  Finder ordformen x. Teksten mellem anførselstegnene er et
  [regulært udtryk](/glossary#regex).

`"x.*" %c`:
  Finder alle ordformer, der begynder med x. `%c` ser bort fra
  forskellen på store og små bogstaver.

`"x|y"`:
  Finder ordformen x eller ordformen y.

## Betingelser

`[lemma = "x"]`:
  Finder alle former af [lemmaet](/glossary#positional-attributes) x.
  Klammerne kan indeholde enhver attribut i korpusset.

`[pos != "N.*"]`:
  Finder alle tokens med et pos-tag, der ikke begynder med N. `!=`
  betyder "er ikke". Værdien er også et regulært udtryk.

`[lemma = "x" & pos = "A.*"]`:
  Finder de tokens, der opfylder begge betingelser. Brug `&` for "og",
  `|` for "eller" og `!` for "ikke".

`[word = ".*" & strlen(word) >= 12]`:
  Finder alle tokens på 12 tegn eller flere.

## Sekvenser

`[lemma = "x"] [pos = "N.*"]`:
  Finder en sekvens af to tokens. Hvert par klammer matcher ét token.

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

Se [CQP-manualen](https://cwb.sourceforge.io/files/CQP_Manual/) for den
fulde syntaks. Se [ordlisten](/glossary) for begreberne.
