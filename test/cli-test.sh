#!/usr/bin/env bash

set -e

echo compare hanspell to hanspell-cli
cat test/cli-test.in | stack run -- hanspell -d 2>&1 | diff - test/cli-test.out.daum
echo compare hanspell to hanspell-cli with option --pnu
cat test/cli-test.in | stack run -- hanspell -p 2>&1 | diff - test/cli-test.out.pnu
echo compare hanspell to hanspell-cli with option --all
cat test/cli-test.in | stack run -- hanspell -a 2>&1 | diff - test/cli-test.out.all
