#! /usr/bin/env sh

make clean && npm run web && python -m http.server --directory build/worker
