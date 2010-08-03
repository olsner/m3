#!/bin/bash

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

exec `dirname $0`/test.sh ${mod}
