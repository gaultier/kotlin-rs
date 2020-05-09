#!/usr/bin/env bash

set -eu


DIFF_TOOL="${DIFF_TOOL:-diff}"
BIN="${BIN:-../target/debug/kotlin}"
PARALLEL_OPTS="${PARALLEL_OPTS:--k --timeout 2}"

cd e2e
for f in *.kts
do
    if ! ${DIFF_TOOL} \
        <(awk -F '// Expect: ' '/Expect/{print $2}' "$f") \
        <(${BIN} -f "$f" jvm); then
            echo "[$f] differs"
    fi
done

cd ..
