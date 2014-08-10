/***************************************************************************
 *   Copyright (C) 2009 - 2010 by Simon Qian <SimonQian@SimonQian.com>     *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#ifndef __APP_LOG_H_INCLUDED__
#define __APP_LOG_H_INCLUDED__

#define LOG_LINE_END		"\r\n"

#define ERROR_LEVEL			0
#define WARNING_LEVEL		0
#define INFO_LEVEL			1
#define DEBUG_LEVEL			2

#define LOG_DEFAULT_LEVEL	INFO_LEVEL

extern int verbosity;
extern int verbosity_pos;
extern int verbosity_stack[16];

#define LOG_MUTE()			verbosity = -1
#define LOG_PUSH()			verbosity_stack[verbosity_pos++] = verbosity
#define LOG_POP()			verbosity = verbosity_stack[--verbosity_pos]

#define _GETTEXT(STR)		STR

#define LOG_BUF_STD(size, buff, len, func) \
			LOG_BUF_STD_ADDR16(0, size, buff, len, func)

#define LOG_BUF(buff, len, func, format, n)	\
			LOG_BUF_ADDR16(0, buff, len, func, format, n)

#define LOG_BUF_STD_ADDR16(addr16, size, buff, len, func) \
	switch (size)\
	{\
	case 1:\
		LOG_BUF_ADDR16(addr16, (uint8_t *)buff, len, func, "%02X", 16);\
		break;\
	case 2:\
		LOG_BUF_ADDR16(addr16, (uint16_t *)buff, len, func, "%04X", 8);\
		break;\
	case 4:\
		LOG_BUF_ADDR16(addr16, (unsigned int *)buff, len, func, "%08X", 4);\
		break;\
	}

#define LOG_BUF_ADDR16(addr16, buff, len, func, format, n)	\
	do{\
		char line[256], s[16];\
		int __i, __j;\
		for (__i = 0; __i < (int)(len); __i += (n))\
		{\
			SNPRINTF(line, 5, "%04X", (uint16_t)((addr16) + __i));\
			for (__j = __i; __j < __i + (n) && __j < (int)(len); __j++)\
			{\
				SNPRINTF(s, sizeof(s), " " format, (buff)[__j]);\
				strncat(line, s, sizeof(s));\
			}\
			func("%s", line);\
		}\
	}while(0)

#define LOG_BUF_STD_ADDR32(addr32, size, buff, len, func) \
	switch (size)\
	{\
	case 1:\
		LOG_BUF_ADDR32(addr32, (uint8_t *)buff, len, func, "%02X", 16);\
		break;\
	case 2:\
		LOG_BUF_ADDR32(addr32, (uint16_t *)buff, len, func, "%04X", 8);\
		break;\
	case 4:\
		LOG_BUF_ADDR32(addr32, (unsigned int *)buff, len, func, "%08X", 4);\
		break;\
	}

#define LOG_BUF_ADDR32(addr32, buff, len, func, format, n)	\
	do{\
		char line[256], s[16];\
		int __i, __j;\
		for (__i = 0; __i < (int)(len); __i += (n))\
		{\
			SNPRINTF(line, 9, "%08X", (uint32_t)((addr32) + __i));\
			for (__j = __i; __j < __i + (n) && __j < (int)(len); __j++)\
			{\
				SNPRINTF(s, sizeof(s), " " format, (buff)[__j]);\
				strncat(line, s, sizeof(s));\
			}\
			func("%s", line);\
		}\
	}while(0)

#define LOG_BUF_STD_ADDR64(addr64, size, buff, len, func) \
	switch (size)\
	{\
	case 1:\
		LOG_BUF_ADDR64(addr64, (uint8_t *)buff, len, func, "%02X", 16);\
		break;\
	case 2:\
		LOG_BUF_ADDR64(addr64, (uint16_t *)buff, len, func, "%04X", 8);\
		break;\
	case 4:\
		LOG_BUF_ADDR64(addr64, (unsigned int *)buff, len, func, "%08X", 4);\
		break;\
	}

#define LOG_BUF_ADDR64(addr64, buff, len, func, format, n)	\
	do{\
		char line[256], s[16];\
		int __i, __j;\
		for (__i = 0; __i < (int)(len); __i += (n))\
		{\
			SNPRINTF(line, 17, "%016" PRIX64, (uint64_t)((addr64) + __i));\
			for (__j = __i; __j < __i + (n) && __j < (int)(len); __j++)\
			{\
				SNPRINTF(s, sizeof(s), " " format, (buff)[__j]);\
				strncat(line, s, sizeof(s));\
			}\
			func("%s", line);\
		}\
	}while(0)

#if 1
#	define LOG_ERROR(format, ...)	\
		do{\
			if (verbosity >= ERROR_LEVEL)\
			{\
				FPRINTF(stderr, _GETTEXT("Error:  " format LOG_LINE_END), \
						##__VA_ARGS__);\
			}\
		}while(0)
#	define LOG_WARNING(format, ...)	\
		do{\
			if (verbosity >= WARNING_LEVEL)\
			{\
				FPRINTF(stdout, _GETTEXT("Warning:" format LOG_LINE_END), \
						##__VA_ARGS__);\
				FFLUSH(stdout);\
			}\
		}while(0)
#	define LOG_INFO(format, ...)	\
		do{\
			if (verbosity >= INFO_LEVEL)\
			{\
				FPRINTF(stdout, _GETTEXT("Info:   " format LOG_LINE_END), \
						##__VA_ARGS__);\
				FFLUSH(stdout);\
			}\
		}while(0)
#	define LOG_DEBUG(format, ...)	\
		do{\
			if (verbosity >= DEBUG_LEVEL)\
			{\
				FPRINTF(stdout, _GETTEXT("Debug:  " format LOG_LINE_END), \
						##__VA_ARGS__);\
				FFLUSH(stdout);\
			}\
		}while(0)
#	define LOG_BUG(format, ...)		\
		do{\
			FPRINTF(stderr, _GETTEXT("Bug:    " format LOG_LINE_END), \
					##__VA_ARGS__);\
		}while(0)
#elif 1
#	define LOG_ERROR(format, ...)	\
		do{\
			if (verbosity >= ERROR_LEVEL)\
			{\
				FPRINTF(stderr, "Error:  %s:%d %s: ", \
						__FILE__, __LINE__, __FUNCTION__);\
				FPRINTF(stderr, _GETTEXT(format LOG_LINE_END), ##__VA_ARGS__);\
			}\
		}while(0)
#	define LOG_WARNING(format, ...)	\
		do{\
			if (verbosity >= WARNING_LEVEL)\
			{\
				FPRINTF(stdout, "Warning:%s:%d %s: ", \
						__FILE__, __LINE__, __FUNCTION__);\
				FPRINTF(stdout, _GETTEXT(format LOG_LINE_END), ##__VA_ARGS__);\
				FFLUSH(stdout);\
			}\
		}while(0)
#	define LOG_INFO(format, ...)	\
		do{\
			if (verbosity >= INFO_LEVEL)\
			{\
				FPRINTF(stdout, "Info:   %s:%d %s: ", \
						__FILE__, __LINE__, __FUNCTION__);\
				FPRINTF(stdout, _GETTEXT(format LOG_LINE_END), ##__VA_ARGS__);\
				FFLUSH(stdout);\
			}\
		}while(0)
#	define LOG_DEBUG(format, ...)	\
		do{\
			if (verbosity >= DEBUG_LEVEL)\
			{\
				FPRINTF(stdout, "Debug:  %s:%d %s: ", \
						__FILE__, __LINE__, __FUNCTION__);\
				FPRINTF(stdout, _GETTEXT(format LOG_LINE_END), ##__VA_ARGS__);\
				FFLUSH(stdout);\
			}\
		}while(0)
#	define LOG_BUG(format, ...)		\
		do{\
			FPRINTF(stderr, "Bug:    %s:%d %s: ", \
					__FILE__, __LINE__, __FUNCTION__);\
			FPRINTF(stderr, _GETTEXT(format LOG_LINE_END), ##__VA_ARGS__);\
		}while(0)
#endif

#endif /* __APP_LOG_H_INCLUDED__ */

