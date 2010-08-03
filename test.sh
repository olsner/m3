#!/bin/bash

run() {
    msg=$1
    shift
    if ! "$@" >/dev/null 2>&1; then
        "$@"
        res=$?
        echo Failed "$msg"
        exit $res
    fi
    return 0
}

mod="$1"
shift

run "compiling ${mod}" dist/build/m3/m3 ${mod}
file=${mod/::/__}
run "LLVM-assembling ${file}" llvm-as ${file}.ll
exec lli ${file}.bc "$@"
