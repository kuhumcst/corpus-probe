#!/usr/bin/env bash
# Encode the dev corpora from dev/corpus/*.vrt using the local CWB
# installation (brew install cwb3). Output goes to dev/corpus/{data,registry},
# both gitignored. Re-run freely; existing output is replaced.
#
# Three corpora with deliberately different shapes, mirroring the variation
# in the KU registry (docs/research/korp-at-ku.md): PROBE (the original
# fixture), VISER (extra text_author metadata, a descriptive NAME) and TALER
# (word only, no pos/lemma, like FT_KORPUS).
set -euo pipefail
cd "$(dirname "$0")/corpus"

rm -rf data registry
mkdir -p data/probe data/viser data/taler registry

# Absolute paths: the registry's HOME field is resolved against cqp's cwd
# when relative, which breaks any caller not started from this directory.
cwb-encode -d "$PWD/data/probe" -f probe.vrt -R "$PWD/registry/probe" \
  -c utf8 -xsB -P pos -P lemma -S s:0+id -S text:0+id+title+year
cwb-makeall -r "$PWD/registry" -V PROBE

cwb-encode -d "$PWD/data/viser" -f viser.vrt -R "$PWD/registry/viser" \
  -c utf8 -xsB -P pos -P lemma -S s:0+id -S text:0+id+title+year+author
cwb-makeall -r "$PWD/registry" -V VISER

cwb-encode -d "$PWD/data/taler" -f taler.vrt -R "$PWD/registry/taler" \
  -c utf8 -xsB -S s:0+id -S text:0+id+speaker+party+year
cwb-makeall -r "$PWD/registry" -V TALER

# cwb-encode cannot set the long NAME or the language property, so fill them
# in afterwards for the new corpora (the production registries have both).
# PROBE stays untouched: its registry values are baked into the golden files.
set_meta() { # registry-file long-name
  sed -i '' -e "s|^NAME \"\"|NAME \"$2\"|" \
            -e "s|^##:: language = \"??\"|##:: language = \"da\"|" "$1"
}
set_meta registry/viser "Danske folkeviser (dev)"
set_meta registry/taler "Folketingstaler (dev)"

printf 'To smaa folkeviser til udvikling af corpus-probe.\n' > data/viser/.info
printf 'Opdigtede folketingstaler til udvikling af corpus-probe.\nKun word, ingen pos/lemma (som FT_KORPUS).\n' > data/taler/.info

cwb-describe-corpus -r "$PWD/registry" -s PROBE VISER TALER
