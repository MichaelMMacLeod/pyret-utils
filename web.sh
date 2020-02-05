#! /usr/bin/env sh

npm run web && python -m http.server --directory build/worker
