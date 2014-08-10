#ifndef __COMPILER_H_INCLUDED__
#define __COMPILER_H_INCLUDED__

//#define USE_BUILDIN_STRING_H

#ifdef USE_BUILDIN_STRING_H
#include <string.h>
#else
extern void* memcpy(void *m0, const void *m1, int len);
extern int memcmp(const void *m0, const void *m1, int len);
extern void *memset(void *m, int c, int len);

int strlen(const char *str);
int strcmp(const char *str0, const char *str1);
char* strcpy(char *dest, const char *source);
char* strncpy(char *dest, const char *source, int len);
char* strcat(char *dest, const char *source);
char* strncat(char *dest, const char *source, int len);
char* strchr(const char *str, int c);
char* strstr(const char *str, const char *substr);
char* strdup(const char *source);
#endif

#define __VSF_FUNCNAME__				__func__

#define ROOTFUNC	
#define PACKED_HEAD	
#define PACKED_MID	__attribute__ ((packed))
#define PACKED_TAIL	

#endif	// __COMPILER_H_INCLUDED__
