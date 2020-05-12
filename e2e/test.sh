#!/usr/bin/env bash

set -eu

DIFF_TOOL="${DIFF_TOOL:-diff}"
BIN="${BIN:-../target/debug/kotlin}"

FILE="$1"
MODE="$2"

if ! ${DIFF_TOOL} \
    <(awk -F '// Expect: ' '/Expect/{print $2}' "$FILE") \
    <(${BIN} -f "$FILE" "$MODE" 2> /dev/null); then
        printf "ERR\t$MODE\t$FILE\n"
        exit 1
else
    printf "OK\t$MODE\t$FILE\n"
fi
