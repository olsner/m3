#!/bin/sh

fail() {
    echo Failed "$@".
    exit 1
}

for mod in "$@"; do
    dist/build/m3/m3 ${mod} || fail Compiling ${mod}
    file=${mod/::/__}
    cat ${file}.ll && llvm-as ${file}.ll || fail LLVM-assembling ${file}
    lli ${file}.bc || fail Running ${file}
done 
