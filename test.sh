#!/usr/bin/env bash

set -eu


DIFF_TOOL="${DIFF_TOOL:-diff}"
BIN="${BIN:-../target/debug/kotlin}"

cd e2e

res=0
for f in *.kts
do
    if ! ${DIFF_TOOL} \
        <(awk -F '// Expect: ' '/Expect/{print $2}' "$f") \
        <(${BIN} -f "$f" jvm); then
            printf "ERR\tjvm\t$f\n"
            res=1
    else
        printf "OK\tjvm\t$f\n"
    fi

    if ! ${DIFF_TOOL} \
        <(awk -F '// Expect: ' '/Expect/{print $2}' "$f") \
        <(${BIN} -f "$f" asm); then
            printf "ERR\tasm\t$f\n"
            res=1
    else
        printf "OK\tasm\t$f\n"
    fi
done

cd ..

exit $res
