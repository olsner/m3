import std::io;

module if2;

int main(int argc, [const [const char]] argv)
{
    if (argc == 1)
    {
        printf("No args\n");
        return 0;
    }
    else if (argc == 2)
    {
        printf("One arg\n");
        return 1;
    }
    else
    {
        printf("Ooh, many args\n");
        return 2;
    }

    return 3;
}
