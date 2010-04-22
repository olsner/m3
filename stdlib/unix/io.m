
module unix::io;

/* oflags for open() */
const int O_RDONLY = 0;
const int O_WRONLY = 1;
const int O_RDWR = 2;

extern "C" int open([const char] filename, int oflag, ...);
extern "C" int read(int fd, [void] buf, int nbytes);
extern "C" int close(int fd);
