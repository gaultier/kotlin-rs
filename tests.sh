#!/bin/sh

set -eu

PARALLEL_OPTS="${PARALLEL_OPTS:- -k --timeout 2}"

cd e2e

parallel $PARALLEL_OPTS ./test.sh ::: *.kts ::: asm jvm

cd ..

