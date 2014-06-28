#!/bin/bash

(
while [ $# -gt 0 ]; do
	echo "import $1;"
	shift
done
cat <<EOF

module temp_test;

int main(int argc, [const [const char]] argv)
{
EOF
cat
echo '}'
) | `dirname $0`/test.sh -
