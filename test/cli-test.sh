#!/usr/bin/env bash

set -e

out=`mktemp`
err=`mktemp`

run() { cabal run -v0 hanspell -- "$@"; }

echo Test hanspell with option --daum.
cat test/cli-test.in | run -d 2> $err > $out
diff $out test/cli-test.out.daum
diff $err test/cli-test.err.daum

echo Test hanspell with option --naver.
cat test/cli-test.in | run -n 2> $err > $out
diff $out test/cli-test.out.naver
diff $err test/cli-test.err.naver

echo Test hanspell with option --all.
cat test/cli-test.in | run -a 2> $err > $out
diff $out test/cli-test.out.all
diff $err test/cli-test.err.all

echo Got it.
