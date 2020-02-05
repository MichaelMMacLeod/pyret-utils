#!/bin/bash

set -e

if [[ ! $# -eq 1 ]]; then
    echo 'usage: horizon-offline-compiler.sh <program>'
    exit 1
fi

make > /dev/stderr

echo "Compiling '$1'..." > /dev/stderr

node build/phaseA/pyret.jarr \
    --build-runnable $1 \
    --require-config src/scripts/standalone-configA.json \
    -type-check
    #--outfile ${1}.js \
    #--builtin-js-dir build/runtime \
    #--builtin-arr-dir src/arr/trove \

echo "done" > /dev/stderr
