#!/bin/bash

fail() {
    echo Failed "$@".
    exit 1
}

mod=temp_test
rm tests/${mod}.m
(
while [ $# -gt 0 ]; do
	echo "import $1;"
	shift
done
cat <<EOF

module ${mod};

int main(int argc, [const [const char]] argv)
{
EOF
cat
echo '}'
) > tests/${mod}.m

dist/build/m3/m3 ${mod} >/dev/null 2>&1 ||
	dist/build/m3/m3 ${mod} ||
	fail Compiling ${mod}
file=${mod/::/__}
llvm-as ${file}.ll || fail LLVM-assembling ${file}
exec lli ${file}.bc "$@"
