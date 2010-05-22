module std::mem;

extern "C" [void] malloc(int size);
extern "C" [void] realloc([void] ptr, int size);
extern "C" void free([void]);
extern "C" [void] memset([void] ptr, int c, int n);
