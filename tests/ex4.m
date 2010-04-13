import std::io;
import ex2;

module ex4;

// TODO Maybe argc/argv should rather be queried from stdlib rather than
int main(int argc, [const [const char]] argv)
{
	const int test_const = 0;
	[const char] tempString = "Let's store a string in a variable";
	int test = 7, test2 = 8, test3 = 9;
	test = 42;
	printf("Hello world: %d (%s)\n", test, "And another string argument");
	test = 0;
	printf("%s\n", tempString);
	return test_const;
}
