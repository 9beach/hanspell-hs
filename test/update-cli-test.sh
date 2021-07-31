#!/usr/bin/env bash

cat test/cli-test.in | stack exec -- hanspell -d > test/cli-test.out.daum 2> test/cli-test.err.daum
cat test/cli-test.in | stack exec -- hanspell -p > test/cli-test.out.pnu 2> test/cli-test.err.pnu
cat test/cli-test.in | stack exec -- hanspell -a > test/cli-test.out.all 2> test/cli-test.err.all
