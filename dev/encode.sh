#!/usr/bin/env bash
# Encode the dev corpus (PROBE) from dev/corpus/probe.vrt using the local CWB
# installation (brew install cwb3). Output goes to dev/corpus/{data,registry},
# both gitignored. Re-run freely; existing output is replaced.
set -euo pipefail
cd "$(dirname "$0")/corpus"

rm -rf data/probe registry
mkdir -p data/probe registry

# Absolute paths: the registry's HOME field is resolved against cqp's cwd
# when relative, which breaks any caller not started from this directory.
cwb-encode -d "$PWD/data/probe" -f probe.vrt -R "$PWD/registry/probe" \
  -c utf8 -xsB -P pos -P lemma -S s:0+id -S text:0+id+title+year
cwb-makeall -r "$PWD/registry" -V PROBE
cwb-describe-corpus -r "$PWD/registry" -s PROBE
