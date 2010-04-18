module invalid::returns1;

int main(int argc, [const [const char]] argv)
{
	{
		0;
		return 1 /* XXX Should give a parse error. */
	}
	return 0;
}
