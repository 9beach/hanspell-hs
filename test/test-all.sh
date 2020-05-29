#!/usr/bin/env bash

set -e

cd "$(dirname "$(dirname "$(readlink -fm "$0")")")"

hlint .
stack test
test/cli-test.sh
