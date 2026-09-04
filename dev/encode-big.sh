#!/usr/bin/env bash
# Encode STOR, a two-million-token synthetic Danish corpus, for measuring
# the saved query result cache (dk.cst.corpus-probe.cache) at a size the
# dev corpora cannot reach: PROBE, VISER and TALER hold 42 to 48 tokens
# each, where every query is instant and a cache proves nothing.
#
# It gets a registry of its own, dev/corpus/registry-big, so that the test
# suite still sees exactly the three corpora it expects. Output is
# gitignored. Takes a minute or so; re-run only when it is missing.
#
#   dev/encode-big.sh
#
# The measurements it exists for are in the (comment ...) block at the end
# of src/dk/cst/corpus_probe/cache.clj.
set -euo pipefail
cd "$(dirname "$0")/corpus"

TOKENS=${TOKENS:-2000000}

rm -rf data/stor registry-big
mkdir -p data/stor registry-big

# A Zipf-ish draw over a small Danish vocabulary, so that word frequencies
# and the sort keys are shaped like a real corpus. The æ/ø/å words are what
# make the Danish collation of a sorted page observable. Seeded, so the
# corpus is the same one every time it is built.
gawk -v tokens="$TOKENS" '
BEGIN {
  srand(42)
  nw = split("hund kat hus træ blå grøn øje ære åben zebra sol vand strand " \
             "månen ridderen fruen havfruen borgen eng port år øre ål æble " \
             "aabne bølge kvinde mand barn dag nat land by vej stor lille " \
             "gammel ny god ond komme gå se tage give finde vide sige", w, " ")
  np = split("N V A ADV PRON CONJ", p, " ")
  sentence = 0; text = 0
  for (i = 0; i < tokens; i++) {
    if (i % 20 == 0) {
      if (sentence) print "</s>"
      if (i % 1000 == 0) {
        if (text) print "</text>"
        text++
        printf "<text id=\"t%d\" title=\"Tekst %d\" year=\"%d\">\n", \
               text, text, 1500 + (text % 500)
      }
      sentence++
      printf "<s id=\"s%d\">\n", sentence
    }
    # rand()^3 skews the draw towards the head of the vocabulary
    k = int(nw * rand() ^ 3) + 1
    printf "%s\t%s\t%s\n", w[k], p[int(np * rand()) + 1], w[k]
  }
  print "</s>"
  print "</text>"
}' > stor.vrt

cwb-encode -d "$PWD/data/stor" -f stor.vrt -R "$PWD/registry-big/stor" \
  -c utf8 -xsB -P pos -P lemma -S s:0+id -S text:0+id+title+year
cwb-makeall -r "$PWD/registry-big" -V STOR
rm -f stor.vrt

sed -i '' -e 's|^NAME ""|NAME "Stor syntetisk korpus (dev)"|' \
          -e 's|^##:: language = "??"|##:: language = "da"|' registry-big/stor
printf 'To millioner syntetiske tokens til maaling af NQR-cachen.\n' \
  > data/stor/.info

cwb-describe-corpus -r "$PWD/registry-big" -s STOR
