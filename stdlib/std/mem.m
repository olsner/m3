module std::mem;

extern "C" [void] malloc(int size);
extern "C" [void] realloc([void] ptr, int size);
