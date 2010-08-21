#!/bin/bash

run() {
    "$@" || exit $?
    return 0
}

mod="$1"
shift

run dist/build/m3/m3 ${mod}
file=out/${mod/::/__}
run llvm-as ${file}.ll
exec lli ${file}.bc "$@"
