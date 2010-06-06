#!/bin/bash

fail() {
    echo Failed "$@".
    exit 1
}

mod="$1"
shift

dist/build/m3/m3 ${mod} >/dev/null 2>&1 || (
	dist/build/m3/m3 ${mod}
	fail Compiling ${mod}
)
file=${mod/::/__}
llvm-as ${file}.ll || fail LLVM-assembling ${file}
exec lli ${file}.bc "$@"
