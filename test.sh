#!/bin/bash

set -e

if [ "$1" = "-" ]; then
testdir=`mktemp -d` || exit 1
trap "rm -fr $testdir" exit
outdir=${testdir}
mod=temp_test

cat > ${testdir}/${mod}.m
else
mod="$1"
fi
outdir=${outdir-out}

shift

dist/build/m3/m3 ${testdir:+-I${testdir} -o${outdir}} ${mod}
file=${outdir}/${mod/::/__}
llvm-as ${file}.ll
lli ${file}.bc "$@"
