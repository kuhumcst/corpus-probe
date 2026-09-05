# Hjælp til forespørgsler

For at lave en simpel søgning skal du skrive ét ord eller flere ord i
rækkefølge. En simpel søgning kræver ingen syntaks. En søgning på flere
ord holder sig inden for én sætning. For at holde den inden for et
afsnit eller en tekst skal du vælge den enhed. For at finde et hvilket
som helst ord fra en liste skal du vælge Liste som søgetype og skrive
ét ord pr. linje.

En udvidet søgning bygger en forespørgsel af [tokens](/glossary#token).
Vælg Udvidet som søgetype. Hvert token har en eller flere betingelser.
En betingelse har en attribut, en operator og en værdi. Operatoren
"ethvert ord" matcher et hvilket som helst token. En betingelse efter
den første er forbundet med betingelserne før den med "og" eller med
"eller". Gentag-felterne angiver, hvor mange gange i træk tokenet kan
forekomme. Et token kan være det første ord i en sætning eller det
sidste. En søgning med flere tokens holder sig inden for én sætning, ét
afsnit eller én tekst. Udfyld den tomme række for at tilføje et token.

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
