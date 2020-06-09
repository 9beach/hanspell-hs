#!/usr/bin/env bash

set -e

cd "$(dirname $(cd "$(dirname "$0")" > /dev/null 2>&1; pwd -P))"

hlint .
stack test
test/cli-test.sh
