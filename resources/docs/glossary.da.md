# Ordliste

Grænsefladen bruger de ord, som Corpus Workbench bruger. Denne side
forklarer dem.

Alignment-attributter {#alignment-attributes}:
  Forbindelser mellem [regionerne](/glossary#region) i to korpusser. De
  to korpusser er oversættelser af hinanden. En korpusside viser et
  korpus' alignment-attributter, hvis korpusset har nogen.
  [Afsnit 5 i CQP-manualen](https://cwb.sourceforge.io/files/CQP_Manual/5.html)
  beskriver forespørgsler i korpusser med alignment.

cpos {#cpos}:
  Korpusposition: nummeret på et [token](/glossary#token), talt fra
  korpussets begyndelse. CWB bruger dette nummer som adressen på et
  token. Første kolonne i [KWIC'en](/glossary#kwic) viser positionen for
  hvert match. En resultat-URL indeholder positioner, når forekomster er
  foldet ud.
  [Afsnit 1.2 i CQP-manualen](https://cwb.sourceforge.io/files/CQP_Manual/1_2.html)
  beskriver korpuspositioner.

CQP {#cqp}:
  Corpus Query Processor: forespørgselssproget i [CWB](/glossary#cwb)
  og det program, der udfører forespørgslerne. En CQP-forespørgsel er en
  række tokenmønstre. Et mønster er en ordform i anførselstegn, fx
  `"x"`, eller en betingelse i klammer, fx `[lemma = "x"]`. Hver værdi
  er et [regulært udtryk](/glossary#regex) over en
  [attribut](/glossary#positional-attributes). I søgetypen CQP sendes
  forespørgslen til CQP, som du skrev den, bortset fra to navne: et
  sætningsmærke, `<s>`, og enheden i en `within`-klausul, `within s`,
  som hvert korpus omdøber efter sin egen attribut. I den simple søgning
  skriver grænsefladen forespørgslen for dig.
  [CQP-manualen](https://cwb.sourceforge.io/files/CQP_Manual/) har den
  fulde syntaks.

CWB {#cwb}:
  [IMS Open Corpus Workbench](https://cwb.sourceforge.io/): det program,
  der opbevarer korpusserne og besvarer forespørgslerne. corpus-probe er
  en brugerflade til CWB.

Forekomst {#hit}:
  Én forekomst af det søgte. En forekomst er én linje i
  [KWIC'en](/glossary#kwic): [matchet](/glossary#match) med sin
  [kontekst](/glossary#context). Overskriften på et resultat angiver
  antallet af forekomster. En [stikprøve](/glossary#sample) beholder
  nogle af forekomsterne.

Frekvens {#frequency}:
  Antallet af gange, en værdi forekommer i [forekomsterne](/glossary#hit)
  eller i et korpus. Frekvensvisningen tæller værdierne af en attribut
  på en position i [matchet](/glossary#match). Den tæller i hvert korpus
  og i alle korpusser under ét. Den angiver også hver frekvens
  [pr. million](/glossary#per-million).
  [Afsnit 3.4 i CQP-manualen](https://cwb.sourceforge.io/files/CQP_Manual/3_4.html)
  beskriver frekvensfordelinger.

Konkordans {#concordance}:
  En liste over alle forekomster af det søgte. Hver forekomst vises i
  sin [kontekst](/glossary#context). Denne grænseflade viser en
  konkordans som en [KWIC](/glossary#kwic) og kalder den KWIC.

Kontekst {#context}:
  De [tokens](/glossary#token), der står på hver side af et
  [match](/glossary#match). Kontekstvælgeren sætter bredden: et antal
  ord eller den sætning eller det afsnit, som indeholder matchet.
  [Positionen](/glossary#cpos) for en forekomst åbner mere kontekst til
  den forekomst.
  [Afsnit 2.3 i CQP-manualen](https://cwb.sourceforge.io/files/CQP_Manual/2_3.html)
  beskriver visningsindstillingerne i CQP, blandt andet konteksten.

Korpus {#corpus}:
  En samling tekster, der er kodet til Corpus Workbench. Hvert ord er et
  [token](/glossary#token). Hvert token har annotationer. Hver tekst har
  [metadata](/glossary#metadata). Siden [Korpusser](/corpora) viser
  korpusserne med deres størrelse og deres attributter.
  [Afsnit 1.2 i CQP-manualen](https://cwb.sourceforge.io/files/CQP_Manual/1_2.html)
  beskriver et korpus' datamodel.

Korpusrækkefølge {#corpus-order}:
  Den rækkefølge, [forekomsterne](/glossary#hit) har i korpusset. CWB
  leverer forekomsterne i denne rækkefølge. Det er standardrækkefølgen i
  en [KWIC](/glossary#kwic).
  [Afsnit 2.9 i CQP-manualen](https://cwb.sourceforge.io/files/CQP_Manual/2_9.html)
  beskriver de andre rækkefølger.

KWIC {#kwic}:
  Key word in context, søgeord i kontekst: en
  [konkordans](/glossary#concordance) med én
  [forekomst](/glossary#hit) på hver linje. [Matchet](/glossary#match)
  står i midten af linjen, og nogle ord [kontekst](/glossary#context)
  står på hver side. Du kan læse kolonnen af match fra top til bund. CWB
  kalder denne visning KWIC, og det gør denne grænseflade også.
  KWIC-visningen viser forekomsterne i et resultat, og
  [frekvensvisningen](/glossary#frequency) tæller dem.
  [Afsnit 2.3 i CQP-manualen](https://cwb.sourceforge.io/files/CQP_Manual/2_3.html)
  beskriver visningsindstillingerne for en KWIC.

Match {#match}:
  De [tokens](/glossary#token), som forespørgslen matchede i én
  [forekomst](/glossary#hit). Matchet står i matchkolonnen i
  [KWIC'en](/glossary#kwic). En forespørgsel kan udpege ét token i
  matchet som *target* med `@`, og KWIC'en viser target med fed. Det
  ord, som feltet Sammen med beder om, er *keyword*, og KWIC'en
  understreger det. En [frekvenstabel](/glossary#frequency) tæller på én
  position i matchet: før det, ved dets start, ved dets slutning, over
  hele matchet eller efter det.
  [Afsnit 3.3 i CQP-manualen](https://cwb.sourceforge.io/files/CQP_Manual/3_3.html)
  beskriver ankrene match, matchend, target og keyword.

Metadata {#metadata}:
  Værdierne af de annoterede [strukturelle
  attributter](/glossary#structural-attributes): fx forfatteren, året
  eller titlen på en tekst. Metadatafiltret begrænser en søgning til
  nogle af teksterne. Det viser de værdier, som de valgte korpusser
  har. Det tager også et mønster for hver attribut og et interval for
  en attribut, hvis værdier er tal.
  [Afsnit 4.2 i CQP-manualen](https://cwb.sourceforge.io/files/CQP_Manual/4_2.html)
  beskriver strukturelle attributter med annotationer.

Positionelle attributter {#positional-attributes}:
  Annotationerne på hvert [token](/glossary#token). Hver attribut har
  én værdi for hvert token. `word` er formen, som den er skrevet. De
  fleste korpusser har også `lemma`, opslagsformen, og `pos`,
  ordklassen. En simpel søgning matcher én attribut, og en
  [CQP](/glossary#cqp)-forespørgsel nævner attributterne i sine
  klammer. Attributvælgeren viser de attributter, som de valgte
  korpusser har fælles.
  [Afsnit 2.5 i CQP-manualen](https://cwb.sourceforge.io/files/CQP_Manual/2_5.html)
  beskriver, hvordan en forespørgsel bruger dem.

Pr. million {#per-million}:
  En [frekvens](/glossary#frequency) skaleret til en million
  [tokens](/glossary#token) i korpusset. Med denne skala kan du
  sammenligne korpusser af forskellig størrelse. Et ord, der forekommer
  3 gange i et korpus på 47 tokens, har fx 63.830 pr. million.

Region {#region}:
  Én strækning af en [strukturel
  attribut](/glossary#structural-attributes): én sætning eller én tekst.
  [Metadatafiltret](/glossary#metadata) viser antallet af regioner med
  hver værdi. `1591 · 3 regioner` betyder fx, at tre tekster har året
  1591.
  [Afsnit 4.2 i CQP-manualen](https://cwb.sourceforge.io/files/CQP_Manual/4_2.html)
  beskriver regioner og deres attributter.

Registret {#registry}:
  Listen over de korpusser, CWB har. Registret har én fil for hvert
  [korpus](/glossary#corpus). Filen angiver korpussets attributter og
  placeringen af dets data. Hvis CWB ikke kan læse et korpus' data,
  viser grænsefladen korpusset som utilgængeligt.

Regulært udtryk {#regex}:
  Et mønster, som en værdi skal matche, i den syntaks, som
  [CQP](/glossary#cqp) bruger. `x.*` matcher fx hver form, der begynder
  med x, og `x|y` matcher x eller y. I en CQP-forespørgsel er hver
  streng i anførselstegn et regulært udtryk. Den simple søgning escaper
  de tegn, du skriver. Et punktum i en simpel søgning er derfor et
  punktum.
  [Appendiks A.1 i CQP-manualen](https://cwb.sourceforge.io/files/CQP_Manual/A_1.html)
  opsummerer syntaksen.

Stikprøve {#sample}:
  Et tilfældigt udvalg af et givet antal [forekomster](/glossary#hit).
  Grænsefladen tager stikprøven, før den tæller og sorterer
  forekomsterne. Derfor kan du læse en del af et resultat, som er for
  stort til at læse i sin helhed. Udvalget ligger fast: den samme URL
  giver altid den samme stikprøve. Grænsefladen tager en stikprøve i
  hvert korpus for sig.
  [Afsnit 3.6 i CQP-manualen](https://cwb.sourceforge.io/files/CQP_Manual/3_6.html)
  beskriver tilfældige udvalg.

Strukturelle attributter {#structural-attributes}:
  Opmærkningen af strækninger af [tokens](/glossary#token): sætninger
  (`s`), tekster (`text`) og andre enheder, som korpusset opmærker. Hver
  strækning er en [region](/glossary#region) fra et første token til et
  sidste token. En annoteret strukturel attribut har en værdi for hver
  region, fx `text_year`. [Metadatafiltret](/glossary#metadata) tilbyder
  disse værdier.
  [Afsnit 4.2 i CQP-manualen](https://cwb.sourceforge.io/files/CQP_Manual/4_2.html)
  beskriver dem.

Token {#token}:
  Én enhed tekst, sådan som korpusset er kodet: et ord, et tal eller et
  tegnsætningstegn. Størrelsen af et [korpus](/glossary#corpus) er dets
  antal tokens. En *type* er én bestemt værdi. Et korpus har lige så
  mange typer af `word`, som det har forskellige ordformer.
  [Afsnit 1.2 i CQP-manualen](https://cwb.sourceforge.io/files/CQP_Manual/1_2.html)
  beskriver tokens og deres attributter.
