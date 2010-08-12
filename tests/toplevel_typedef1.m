module toplevel_typedef1;

type myint = int;

int main(int argc, [[const char]] argv)
{
    myint i = 7;
    return i - 7;
}
