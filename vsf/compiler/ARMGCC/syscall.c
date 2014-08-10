#include <sys/types.h>
#include <sys/stat.h>

static char *heap = NULL;
caddr_t _sbrk(int incr)
{
	extern char _ebss;
	char *prev_heap;
	
	if (heap == NULL)
	{
		heap = &_ebss;
	}
	prev_heap = heap;
	
	heap += incr;
	return (caddr_t)prev_heap;
}

int _open(const char *name, int flags, int mode)
{
	return -1;
}

int _close(int fd)
{
	return 0;
}

int _fstat(int fd, struct stat *st)
{
	st->st_mode = S_IFCHR;
	return 0;
}

int _isatty(int fd)
{
	return 1;
}

int _lseek(int fd, int ptr, int dir)
{
	return 0;
}

int _read(int fd, char *buf, int len)
{
	return 0;
}

int _write(int fd, const void *buf, int len)
{
	return len;
}

