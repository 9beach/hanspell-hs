#!/usr/bin/env bash

run() { cabal run -v0 hanspell -- "$@"; }

cat test/cli-test.in | run -d > test/cli-test.out.daum  2> test/cli-test.err.daum
cat test/cli-test.in | run -n > test/cli-test.out.naver 2> test/cli-test.err.naver
cat test/cli-test.in | run -a > test/cli-test.out.all   2> test/cli-test.err.all
