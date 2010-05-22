import std::io;
import std::mem;
import std::lib;

import unix::io;

module interp::bf1;

[const char] findloop([const char] start, [const char] end, bool forward)
{
	if (forward)
	{
		[const char] p = start;
		int nest = 1;
		while (p < end)
		{
			if (*p == '[') nest++;
			if (*p == ']') nest--;
			if (!nest)
				return p;
			p++;
		}
		return null;
	}
	else
	{
		[const char] p = end;
		int nest = 1;
		while (p >= start)
		{
			if (*p == ']') nest++;
			if (*p == '[') nest--;
			if (!nest)
				return p;
			p++;
		}
		return null;
	}
}

void run([const char] buffer, int size)
{
	[const char] end = buffer + size;
	[const char] ip = buffer;
	[char] tape = cast<[[char]]>(malloc(512));
	[char] tapep;
	while (ip < end)
	{
		int instr = *ip++;
		if (instr == '>') tapep++;
		else if (instr == '<') tapep--;
		else if (instr == '+') (*tapep)++;
		else if (instr == '-') (*tapep)--;
		else if (instr == '.') write(1, tapep, 1);
		else if (instr == ',') read(0, tapep, 1);
		else if (instr == '[')
		{
			if (!*tapep)
			{
				ip = findloop(ip, end, true);
				if (!ip)
				{
					printf("Loop start: didn't find the end\n");
					exit(1);
				}
			}
		}
		else if (instr == ']')
		{
			if (*tapep)
			{
				ip = findloop(buffer, ip-2, false);
				if (!ip)
				{
					printf("Loop end: didn't find the start\n");
					exit(1);
				}
			}
		}
	}
}

int main(int argc, [const [const char]] argv)
{
	if (argc < 2)
	{
		printf("Usage: %s PROGRAM\n", argv[0]);
		return 1;
	}

	int fd = open(argv[1], O_RDONLY);
	if (fd < 0)
	{
		char[256] buf;
		snprintf(buf, 256, "Opening \"%s\"", argv[1]);
		perror(buf);
		return 1;
	}

	int size = 0;
	int alloced = 512;
	int nread = 0;
	[char] buffer = cast<[[char]]>(malloc(alloced));
	while (read(fd, buffer + size, 1) == 1)
	{
		size++;
		if (alloced == size)
		{
			alloced += 512;
			buffer = cast<[[char]]>(realloc(buffer, alloced));
		}
	}

	run(buffer, size);

	//if (close(fd) < 0)
	{
		perror("close");
		return 1;
	}
}
