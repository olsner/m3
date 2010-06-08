import std::io;

module conv1;

int main(int argc, [const [const char]] argv)
{
	char[256] buf;
	snprintf(buf, 256, "foo bar\n");
	// Test that char-array can be converted to pointer-to-char and then to pointer-to-const-char
	printf(buf);
	return 0;
}
