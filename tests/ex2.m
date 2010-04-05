import std::io::printf;

module Main;

// TODO Maybe argc/argv should rather be queried from stdlib rather than
int main(int argc, [const [const char]] argv)
{
	int test;
	[const[const char]] ptr_const_ptr_const_char;
	test = 7;
	printf("Hello world: %d\n", test);
	return test;
}
