import std::io;

module if2;

int main(int argc, [const [const char]] argv)
{
    if (false)
        printf("False is true!?\n");
    else if (true)
        printf("True is true!\n");
    else
        printf("False is false, but true is also false?\n");

    return 0;
}
