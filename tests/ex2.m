import std::io;

module ex2;

// TODO Maybe argc/argv should rather be queried from stdlib rather than
int main(int argc, [const [const char]] argv)
{
	int test;
	[const[const char]] ptr_const_ptr_const_char;
	test = 42;
	printf("Hello world: %d (%s)\n", test, "And another string argument");
	test = 0;
	return test;
}
