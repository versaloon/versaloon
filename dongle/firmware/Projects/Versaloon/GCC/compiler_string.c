#include "app_cfg.h"

#include <stdlib.h>

#ifndef USE_BUILDIN_STRING_H


int strlen(const char *str)
{
	const char *p = str;
	
	while (*p)p++;
	return (int)(p - str);
}

int strcmp(const char *str0, const char *str1)
{
	for (; *str0 && *str1 && *str0 == *str1; str0++, str1++);
	return (*str0 - *str1);
}

char* strcpy(char *dest, const char *source)
{
	char *p = dest;
	
	if (*source)
	{
		while ((*p++ = *source++));
		*p = '\0';
	}
	return dest;
}

char* strncpy(char *dest, const char *source, int len)
{
	char *p = dest;
	
	if (*source)
	{
		while (len-- && (*p++ = *source++));
		*p = '\0';
	}
	return dest;
}

char* strchr(const char *str, int c)
{
	while (*str && *str != (char)c) str++;
	return (*str == (char)c) ? (char *)str : (char *)NULL;
}

char* strstr(const char *str, const char *substr)
{
	char *pstr, *psub;
	
	if (!*substr)
		return (char *)NULL;
	while (*str)
	{
		pstr = (char *)str;
		psub = (char *)substr;
		do {
			if (!*psub) return pstr;
		} while (*pstr++ == *psub++);
		str++;
	}
	return (char *)NULL;
}

char* strcat(char *dest, const char *source)
{
	char *p = dest;
	
	while (*p)p++;
	while ((*p++ = *source++));
	return dest;
}

char* strncat(char *dest, const char *source, int len)
{
	char *p = dest;
	
	while (*p)p++;
	while (len-- && (*p++ = *source++));
	return dest;
}

char* strdup(const char *source)
{
	char *p = (char *)malloc(strlen(source) + 1);
	return strcpy(p, source);
}

void* memcpy(void *dest, const void *source, int len)
{
	char *p = dest;
	
	while(len--)
		*(char*)p++ = *(char*)source++;
	return dest;
}

int memcmp(const void *m0, const void *m1, int len)
{
	const char *u8_0 = (const char *)m0;
	const char *u8_1 = (const char *)m1;
	
	for (; len > 0; ++u8_0, ++u8_1, --len)
		if (*u8_0 != *u8_1)
			return (*u8_0 - *u8_1);
	return 0;
}

void *memset(void *m, int c, int len)
{
	char *p = (char *)m;
	
	for (; len > 0; len--)
		*p++ = (char)c;
	return m;
}

#endif
