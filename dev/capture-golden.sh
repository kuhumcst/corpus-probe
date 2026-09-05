#!/usr/bin/env bash
# Capture byte-exact cqp outputs from the dev corpus into test/resources/golden/.
# The parsers in dk.cst.corpus-probe.parse are tested against these files, so
# re-run only deliberately (and re-run dev/encode.sh first). Separator strings
# contain literal TAB bytes, produced here via printf '%b'.
set -euo pipefail
cd "$(dirname "$0")"
REG="$PWD/corpus/registry"
OUT="../test/resources/golden"
mkdir -p "$OUT"

run_cqp() { printf '%b' "$1" | cqp -c -r "$REG" | tail -n +2; }  # drop banner

PROFILE='set AttributeSeparator "\tA\t";
set TokenSeparator "\tT\t";
set LeftKWICDelim "\tL\t";
set RightKWICDelim "\tR\t";
set StructureDelimiter "\tS\t";
set ShowTagAttributes off;
set Context 5 words;
PROBE;
show +pos +lemma;
'

run_cqp "${PROFILE}"'A = "hund.*" %c; cat A;'                > "$OUT/kwic-hardened.txt"
run_cqp "${PROFILE}"'show +s; A = "hund.*" %c; cat A;'       > "$OUT/kwic-hardened-structs.txt"
run_cqp 'PROBE; show cd;'                                    > "$OUT/show-cd.txt"
run_cqp 'PROBE; info;'                                       > "$OUT/info.txt"
run_cqp 'PROBE; A = "hund.*" %c; dump A;'                    > "$OUT/dump.tsv"
run_cqp 'PROBE; B = [pos = "N.*"]; group B match lemma;'     > "$OUT/group.txt"

cwb-describe-corpus -r "$REG" -s PROBE                       > "$OUT/describe.txt"
cwb-lexdecode -r "$REG" -fb -P lemma PROBE                   > "$OUT/lexdecode.tsv"
cwb-s-decode -r "$REG" -n PROBE -S text_title                > "$OUT/s-decode.txt"
cwb-s-decode -r "$REG" PROBE -S text_title                   > "$OUT/s-decode-regions.txt"

# A full child-mode session with .EOL. markers and one failing command, for
# testing the driver's section splitting: stdout and stderr captured separately.
printf '%b' 'PROBE;\n.EOL.;\nA = "hund.*" %c;\n.EOL.;\nsize A;\n.EOL.;\nbogus;\n.EOL.;\ncat A 0 1;\n.EOL.;\n' \
  | cqp -c -r "$REG" > "$OUT/session-raw.stdout" 2> "$OUT/session-raw.stderr"

ls -la "$OUT"
