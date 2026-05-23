#!/usr/bin/env bash

set -e

cd "$(dirname $(cd "$(dirname "$0")" > /dev/null 2>&1; pwd -P))"

cabal build
cabal test
test/cli-test.sh
