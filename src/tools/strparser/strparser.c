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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <ctype.h>
#include <inttypes.h>

#include "compiler.h"

#include "vsf_err.h"

#include "app_type.h"
#include "app_io.h"
#include "app_log.h"
#include "app_err.h"

#include "strparser.h"

static uint8_t strparser_is_divider(char div)
{
	uint32_t i;
	
	for (i = 0; i < strlen(STRPARSER_DIV_CHARS); i++)
	{
		if (STRPARSER_DIV_CHARS[i] == div)
		{
			return 1;
		}
	}
	return 0;
}

static const uint64_t strparser_maxvalue[] =
{0,
0xFF, 0xFFFF, 0xFFFFFF, 0xFFFFFFFF,
0xFFFFFFFFFF, 0xFFFFFFFFFFFF, 0xFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF};

vsf_err_t strparser_check(char * str, char * format)
{
	uint8_t buff[256];
	
	memset(buff, 0, sizeof(buff));
	return strparser_parse(str, format, buff, sizeof(buff));
}

uint32_t strparser_getsize(char * format)
{
	char *ptr_tmp;
	uint32_t size;
	uint32_t param;
	
	if (NULL == format)
	{
		return 0;
	}
	
	size = 0;
	while (format[0])
	{
		if ('%' == format[0])
		{
			param = strtoul(&format[1], &ptr_tmp, 0);
			if (ptr_tmp != &format[1])
			{
				size += param;
			}
		}
		format++;
	}
	return size;
}

vsf_err_t strparser_parse(char * str, char * format, uint8_t * buff,
						uint32_t size)
{
	char *ptr_tmp;
	uint64_t val_tmp, param;
	uint32_t cur_index;
	char cur_ch;
	uint8_t has_param;
	uint8_t radix;
	uint32_t size_avail = size;
	
	if ((NULL == str) || (NULL == format) || (NULL == buff) || (0 == size))
	{
		return VSFERR_FAIL;
	}
	
	cur_index = 0;
	while (format[cur_index] != '\0')
	{
		if ('\0' == str[0])
		{
			return VSFERR_FAIL;
		}
		
		cur_ch = format[cur_index++];
		switch (cur_ch)
		{
		case '%':
			has_param = 0;
			param = 0;
			param = strtoull(&format[cur_index], &ptr_tmp, 0);
			if (ptr_tmp != &format[cur_index])
			{
				has_param = 1;
				cur_index += (ptr_tmp - &format[cur_index]);
			}
			
			cur_ch = format[cur_index++];
			switch (cur_ch)
			{
			case 'i':
			case 'I':
			case 'd':
			case 'D':
				// integer value, parsed by strtoull with radix 0
				radix = 0;
				goto parse_integer;
			case 'b':
			case 'B':
				radix = 2;
				goto parse_integer;
			case 'x':
			case 'X':
				// integer value, parsed by strtoull with radix 16
				radix = 16;
parse_integer:
				if (!has_param)
				{
					LOG_DEBUG(ERRMSG_NOT_DEFINED, "integer size");
					return VSFERR_FAIL;
				}
				if (param > 8)
				{
					LOG_DEBUG("%d is invalid for integer size, max is %d",
								(uint32_t)param, 8);
					return VSFERR_FAIL;
				}
				
				if ('\0' == str[0])
				{
					LOG_DEBUG("get NULL while parsing value");
					return VSFERR_FAIL;
				}
				val_tmp = 0;
				val_tmp = strtoull(str, &ptr_tmp, radix);
				if (str == ptr_tmp)
				{
					// parse failed
					LOG_DEBUG(ERRMSG_FAILURE_HANDLE_DEVICE, "parse", str);
					return VSFERR_FAIL;
				}
				str = ptr_tmp;
				while ((str[0] != '\0') && strparser_is_divider(str[0]))
				{
					str++;
				}
				
				if (val_tmp > strparser_maxvalue[param])
				{
					LOG_DEBUG("value execeed range!!");
					return VSFERR_FAIL;
				}
				if (size && (size_avail < param))
				{
					LOG_DEBUG(ERRMSG_BUFFER_OVERFLOW, TO_STR(buff));
					return VSFERR_FAIL;
				}
				memcpy(buff, &val_tmp, (size_t)param);
				buff += param;
				size_avail -= (uint16_t)param;
				break;
			case 'c':
			case 'C':
				// character value
				if (size && !size_avail)
				{
					LOG_DEBUG(ERRMSG_BUFFER_OVERFLOW, TO_STR(buff));
					return VSFERR_FAIL;
				}
				
				*buff++ = *str++;
				size_avail--;
				
				while ((str[0] != '\0') && strparser_is_divider(str[0]))
				{
					str++;
				}
				break;
			case 's':
			case 'S':
				// string value
				while ((str[0] != '\0') && !strparser_is_divider(str[0]))
				{
					if (size && !size_avail)
					{
						LOG_DEBUG(ERRMSG_BUFFER_OVERFLOW, TO_STR(buff));
						return VSFERR_FAIL;
					}
					
					*buff++ = *str++;
					size_avail--;
				}
				if (size && !size_avail)
				{
					LOG_DEBUG(ERRMSG_BUFFER_OVERFLOW, TO_STR(buff));
					return VSFERR_FAIL;
				}
				*buff++ = '\0';
				while ((str[0] != '\0') && strparser_is_divider(str[0]))
				{
					str++;
				}
				break;
			default:
				LOG_DEBUG(ERRMSG_INVALID_OPTION, cur_ch);
				return VSFERR_FAIL;
				break;
			}
			break;
		default:
			if (str[0] != cur_ch)
			{
				LOG_DEBUG("format error: %s", str);
				return VSFERR_FAIL;
			}
			str++;
			break;
		}
	}
	
	return VSFERR_NONE;
}

static uint64_t strparser_get_u64(uint8_t *buff, uint32_t size)
{
	uint8_t i;
	uint64_t ret;
	
	if (NULL == buff)
	{
		return 0;
	}
	
	if (size > 8)
	{
		size = 8;
	}
	
	ret = 0;
	for (i = 0; i < size; i++)
	{
		ret += (uint64_t)buff[i] << (i * 8);
	}
	return ret;
}

// this function will alloc buffer and return the pointer to the buffer
// caller should free this buffer
char * strparser_solve(char *format, uint8_t *buff, uint32_t size)
{
	char *ptr_tmp;
	uint32_t cur_index;
	char cur_ch;
	uint64_t param, value;
	uint8_t has_param;
	char tmp_str[256], tmp_format[255];
	char *ret = NULL;
	uint8_t radix;
	
	REFERENCE_PARAMETER(size);
	
	if ((NULL == format) || (NULL == buff))
	{
		return NULL;
	}
	ret = (char*)malloc(1024);
	if (NULL == ret)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return NULL;
	}
	memset(ret, 0, 1024);
	memset(tmp_str, 0, sizeof(tmp_str));
	memset(tmp_format, 0, sizeof(tmp_format));
	
	cur_index = 0;
	while (format[cur_index] != '\0')
	{
		cur_ch = format[cur_index++];
		switch (cur_ch)
		{
		case '%':
			has_param = 0;
			param = 0;
			param = strtoull(&format[cur_index], &ptr_tmp, 0);
			if (ptr_tmp != &format[cur_index])
			{
				has_param = 1;
				cur_index += (ptr_tmp - &format[cur_index]);
			}
			
			cur_ch = format[cur_index++];
			switch (cur_ch)
			{
			case 'i':
			case 'I':
			case 'd':
			case 'D':
				// integer value, parsed by strtoull with radix 0
				radix = 10;
				goto solve_integer;
			case 'b':
			case 'B':
				radix = 2;
				goto solve_integer;
			case 'x':
			case 'X':
				// integer value, parsed by strtoull with radix 16
				radix = 16;
solve_integer:
				if (!has_param)
				{
					LOG_DEBUG(ERRMSG_NOT_DEFINED, "integer size");
					free(ret);
					ret = NULL;
					return ret;
				}
				if (param > 8)
				{
					LOG_DEBUG("%d is invalid for integer size, max is %d",
								(uint32_t)param, 8);
					free(ret);
					ret = NULL;
					return ret;
				}
				
				value = strparser_get_u64(buff, (uint32_t)param);
				if (10 == radix)
				{
					if (param <= 4)
					{
						sprintf(tmp_format, "%%d");
						sprintf(tmp_str, tmp_format, (uint32_t)value);
					}
					else
					{
						sprintf(tmp_format, "%%"PRIu64);
						sprintf(tmp_str, tmp_format, value);
					}
				}
				else if (2 == radix)
				{
					// FIXME: solve interger in binary form
					if (param <= 4)
					{
						sprintf(tmp_format, "0b%%d");
						sprintf(tmp_str, tmp_format, (uint32_t)value);
					}
					else
					{
						sprintf(tmp_format, "0b%%"PRIu64);
						sprintf(tmp_str, tmp_format, value);
					}
				}
				else// if (16 == radix)
				{
					if (param < 4)
					{
						sprintf(tmp_format, "0x%%0%dX", (int)(param * 2));
						sprintf(tmp_str, tmp_format, (uint32_t)value);
					}
					else
					{
						sprintf(tmp_format, "0x%%0%d"PRIX64, (int)(param * 2));
						sprintf(tmp_str, tmp_format, value);
					}
				}
				buff += param;
				break;
			case 'c':
			case 'C':
				// character value
				tmp_str[0] = buff[0];
				tmp_str[1] = '\0';
				buff++;
				break;
			case 's':
			case 'S':
				// string value
				strcpy(tmp_str, (char*)buff);
				buff += strlen((char*)buff) + 1;
				break;
			default:
				LOG_DEBUG(ERRMSG_INVALID_OPTION, cur_ch);
				free(ret);
				ret = NULL;
				return ret;
				break;
			}
			strcat(ret, tmp_str);
			if (format[cur_index] != '\0')
			{
				// not the last data, insert a DIV_CHAR
				tmp_str[0] = STRPARSER_DIV_CHAR_DEFAULT;
				tmp_str[1] = '\0';
				strcat(ret, tmp_str);
			}
			break;
		default:
			tmp_str[0] = cur_ch;
			tmp_str[1] = '\0';
			strcat(ret, tmp_str);
			break;
		}
	}
	
	return ret;
}

