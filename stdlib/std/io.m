
module std::io;

// Many of these actually return (e.g. errors)
extern "C" void printf([const char] format, ...);
extern "C" void snprintf([char] buf, int bufsize, [const char] format, ...);
extern "C" void perror([const char] prefix);
extern "C" void putchar(char c);
