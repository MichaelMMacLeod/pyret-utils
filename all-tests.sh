#! /usr/bin/env sh

export BROWSER="firefox"
export BROWSER_BINARY="$(which firefox-developer-edition)"
export BASE_URL="file://$(readlink -f build/worker)"

npm run all-tests
