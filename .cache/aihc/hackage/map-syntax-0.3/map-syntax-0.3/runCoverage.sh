#!/bin/sh

set -e

PKGVERSION=$(cabal info . | awk '{print $2;exit}')

echo $PKGVERSION

ROOT=dist-newstyle/build/$PKGVERSION
DIR=$ROOT/hpc/vanilla
HPCDIR=$DIR/mix/testsuite

DESTDIR=hpc

EXCLUDES='Main
Data.Map.Syntax.Tests
Data.Map.Syntax.Util'

EXCL=""

for m in $EXCLUDES; do
    EXCL="$EXCL --exclude=$m"
done

rm -Rf $DESTDIR
mkdir -p $DESTDIR
hpc markup $EXCL --hpcdir=$HPCDIR --destdir=$DESTDIR testsuite # >/dev/null 2>&1

cat <<EOF

Test coverage report written to $DESTDIR.
EOF
