import std::io;

module hwile;

int main(int argc, [const[const char]] argv)
{
	[const [const char]] p = argv;
	int argc_ = argc;
	printf("%d args:\n", argc_);
	while (argc_--)
	{
		printf("%d of %d\n", argc_, argc);
		printf("%d: %s\n", argc_, p[argc_]);
	}
	return 0;
}
