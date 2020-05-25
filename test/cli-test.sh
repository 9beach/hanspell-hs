#!/usr/bin/env bash

set -e

echo Compares hanspell to hanspell-cli with option --daum.
cat test/cli-test.in | stack run -- hanspell -d 2>&1 | diff - test/cli-test.out.daum
echo Compares hanspell to hanspell-cli with option --pnu.
cat test/cli-test.in | stack run -- hanspell -p 2>&1 | diff - test/cli-test.out.pnu
echo Compares hanspell to hanspell-cli with option --all.
cat test/cli-test.in | stack run -- hanspell -a 2>&1 | diff - test/cli-test.out.all
echo Got it.
