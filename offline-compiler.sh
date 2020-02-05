#!/bin/bash

if [[ ! $# -eq 1 ]]; then
    echo 'usage: offline-compiler.sh <program>'
    exit 1
fi

npm run build > /dev/stderr

echo "Compiling and running '$1'..." > /dev/stderr

start_time=$(date +%s.%N)
node build/phaseA/pyret.jarr \
    --builtin-js-dir build/runtime \
    --build-runnable $1 \
    --type-check true
end_time=$(date +%s.%N)
elapsed=$(echo ${end_time}-${start_time} | bc)

echo "(${1} ${elapsed})"

echo "done" > /dev/stderr

