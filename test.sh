#!/bin/bash

set -e

testdir=`mktemp -d` || exit 1
trap "rm -fr $testdir" exit
outdir=$testdir/out
mkdir $outdir

if [ "$1" = "-" ]; then
outdir=${testdir}
mod=temp_test

cat > ${testdir}/${mod}.m
else
mod="$1"
fi

shift

dist/build/m3/m3 ${testdir:+-I${testdir} -o${outdir}} ${mod}
file=${outdir}/${mod/::/__}
llvm-as ${file}.ll
llc -o ${file}.s ${file}.bc
gcc -o ${file} ${file}.s -lgmp
${file} "$@"
