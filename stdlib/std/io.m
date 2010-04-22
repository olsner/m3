
module std::io;

extern "C" void printf([const char] format, ...);
extern "C" void snprintf([char] buf, int bufsize, [const char] format, ...);
extern "C" void perror([const char] prefix);
