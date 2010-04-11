import std::io;
import ex2;

module ex3;

// TODO Maybe argc/argv should rather be queried from stdlib rather than
int main(int argc, [const [const char]] argv)
{
	int test;
	[const char] tempString;
	tempString = "Let's store a string in a variable";
	test = 42;
	printf("Hello world: %d (%s)\n", test, "And another string argument");
	printf("%s\n", tempString);
	return test;
}
