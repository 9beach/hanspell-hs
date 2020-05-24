#!/usr/bin/env bash

set -e

cat test/cli-test.in | stack run -- hanspell -d 2>&1 | diff - test/cli-test.out.daum
#cat test/cli-test.in | stack run -- hanspell -p 2>&1 | diff - test/cli-test.out.pnu
#cat test/cli-test.in | stack run -- hanspell -a 2>&1 | diff - test/cli-test.out.all
