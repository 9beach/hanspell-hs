#!/usr/bin/env bash

set -e

out=`mktemp`
err=`mktemp`

echo Test hanspell with option --daum.
cat test/cli-test.in | stack exec -- hanspell -d 2> $err > $out
diff $out test/cli-test.out.daum
diff $err test/cli-test.err.daum

echo Test hanspell with option --pnu.
cat test/cli-test.in | stack exec -- hanspell -p 2> $err > $out
diff $out test/cli-test.out.pnu
diff $err test/cli-test.err.pnu

echo Test hanspell with option --all.
cat test/cli-test.in | stack exec -- hanspell -a 2> $err > $out
diff $out test/cli-test.out.all
diff $err test/cli-test.err.all

echo Got it.
