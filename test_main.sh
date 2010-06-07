#!/bin/bash

fail() {
    echo Failed "$@".
    exit 1
}

mod=temp_test
rm tests/${mod}.m
(
cat <<EOF
module ${mod};

int main(int argc, [const [const char]] argv)
{
EOF
cat
echo '}'
) > tests/${mod}.m

dist/build/m3/m3 ${mod} >/dev/null 2>&1 || (
	dist/build/m3/m3 ${mod}
	fail Compiling ${mod}
) || exit $?
file=${mod/::/__}
llvm-as ${file}.ll || fail LLVM-assembling ${file}
exec lli ${file}.bc "$@"
