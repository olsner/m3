#!/bin/bash

run() {
    "$@" || exit $?
    return 0
}

if [ "$1" = "-" ]; then
mod="temp_test"
cat > tests/${mod}.m
else
mod="$1"
fi

shift

run dist/build/m3/m3 ${mod}
file=out/${mod/::/__}
run llvm-as ${file}.ll
exec lli ${file}.bc "$@"
