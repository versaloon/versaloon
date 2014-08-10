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

#include "compiler.h"

#include "vsf_err.h"

#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"
#include "fileparser.h"

#include "s19.h"

enum  S19_TYPE
{
	S19_S0 = 0,
	S19_S1 = 1,
	S19_S2 = 2,
	S19_S3 = 3,
	S19_S4 = 4,
	S19_S5 = 5,
	S19_S6 = 6,
	S19_S7 = 7,
	S19_S8 = 8,
	S19_S9 = 9
};

vsf_err_t read_s19_file(FILE *s19_file, WRITE_MEMORY_CALLBACK callback,
					 void *buffer, uint32_t seg_offset, uint32_t addr_offset)
{
	uint8_t line_buf[4 + 0xFF * 2 + 2], checksum, *data;
	char ch, *ptr, tmp_buff[3];
	uint32_t length, i, addr = 0;
	enum S19_TYPE type;
	
#ifdef PARAM_CHECK
	if ((NULL == s19_file) || (NULL == callback))
	{
		LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
		return VSFERR_INVALID_PARAMETER;
	}
#endif
	
	rewind(s19_file);
	while (!feof(s19_file))
	{
		// ignore empty lines
		do{
			ch = (char)fgetc(s19_file);
			if (EOF == ch)
			{
				return VSFERR_NONE;
			}
		}while (('\n' == ch) || ('\r' == ch));
		// check first character of a line, which MUST be 'S'
		if (ch != 'S')
		{
			return VSFERR_FAIL;
		}
		
		length = 0;
		// read line
		do{
			ch = (char)fgetc(s19_file);
			line_buf[length++] = ch;
		}while ((ch != '\n') && (ch != '\r') && (ch != EOF));
		length--;
		
		// process line
		if ((length < 9) || ((length % 2) == 0)
			|| (line_buf[0] < '0') || (line_buf[0] > '9'))
		{
			return VSFERR_FAIL;
		}
		type = (enum S19_TYPE)(line_buf[0] - '0');
		length--;
		
		tmp_buff[2] = '\0';
		for (i = 0; i < length; i+=2)
		{
			tmp_buff[0] = line_buf[1 + i];
			tmp_buff[1] = line_buf[2 + i];
			line_buf[i >> 1] = (uint8_t)strtoul((const char *)tmp_buff,
														&ptr, 16);
			if (ptr != &tmp_buff[2])
			{
				return VSFERR_FAIL;
			}
		}
		i >>= 1;
		
		// valid check
		length = line_buf[0];
		if ((length + 1) != i)
		{
			return VSFERR_FAIL;
		}
		checksum = 0;
		for (i = 0; i < length + 1; i++)
		{
			checksum += line_buf[i];
		}
		if (checksum != 0xFF)
		{
			return VSFERR_FAIL;
		}
		length--;
		
		// process data according to data type
		switch (type)
		{
		case S19_S0:
			// block header, should be ignored
			break;
		case S19_S1:
			addr = (line_buf[1] << 8) + line_buf[2];
			data = &line_buf[3];
			length -= 2;
			goto parse_data;
		case S19_S2:
			addr = (line_buf[1] << 16) + (line_buf[2] << 8) + line_buf[3];
			data = &line_buf[4];
			length -= 3;
			goto parse_data;
		case S19_S3:
			addr = (line_buf[1] << 24) + (line_buf[2] << 16)
					+ (line_buf[3] << 8) + line_buf[4];
			data = &line_buf[5];
			length -= 4;
			goto parse_data;
			// data sequence
parse_data:
			if (callback(fileparser_cur_ext, addr_offset + addr,
										seg_offset, data, length, buffer))
			{
				return VSFERR_FAIL;
			}
			break;
		case S19_S5:
			// record count, ignore
			break;
		case S19_S7:
		case S19_S8:
		case S19_S9:
			// end of block
			return VSFERR_NONE;
			break;
		case S19_S4:
		case S19_S6:
			// invalid type, simply ignore
		default:
			LOG_WARNING(ERRMSG_INVALID_VALUE_MESSAGE, line_buf[3],
						"s19 type", "current line ignored!!");
			break;
		}
	}
	
	return VSFERR_FAIL;
}

static vsf_err_t write_s19_line(FILE *s19_file, uint8_t data_len,
						uint32_t data_addr, enum S19_TYPE type, uint8_t *data)
{
	uint8_t line_buf[4 + 0xFF * 2 + 2], checksum = 0, pos = 0, addr_len;
	uint32_t i;
	
#ifdef PARAM_CHECK
	if ((NULL == s19_file) || ((data_len > 0) && (NULL == data)))
	{
		LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
		return VSFERR_INVALID_PARAMETER;
	}
#endif
	
	memset(line_buf, 0, sizeof(line_buf));
	line_buf[pos] = 'S';
	pos += 1;
	
	// type
	line_buf[pos] = (uint8_t)(type + '0');
	pos += 1;
	
	// data length
	if ((S19_S2 == type) || (S19_S8 == type))
	{
		// 3 bytes address
		addr_len = 3;
	}
	else if ((S19_S3 == type) || (S19_S7 == type))
	{
		// 4 bytes address
		addr_len = 4;
	}
	else
	{
		addr_len = 2;
	}
	checksum += data_len + addr_len + 1;
	sprintf((char *)line_buf + pos, "%02X", data_len + addr_len + 1);
	pos += 2;
	
	// address
	checksum += (uint8_t)data_addr;
	checksum += (uint8_t)(data_addr >> 8);
	if ((S19_S2 == type) || (S19_S8 == type))
	{
		// 3 bytes address
		checksum += (uint8_t)(data_addr >> 16);
		sprintf((char *)line_buf + pos, "%06X", data_addr);
		pos += 6;
	}
	else if ((S19_S3 == type) || (S19_S7 == type))
	{
		// 4 bytes address
		checksum += (uint8_t)(data_addr >> 16);
		checksum += (uint8_t)(data_addr >> 24);
		sprintf((char *)line_buf + pos, "%08X", data_addr);
		pos += 8;
	}
	else
	{
		sprintf((char *)line_buf + pos, "%04X", data_addr);
		pos += 4;
	}
	
	// data
	for (i = 0; i < data_len; i++)
	{
		checksum += data[i];
		sprintf((char *)line_buf + pos, "%02X", data[i]);
		pos += 2;
	}
	
	// checksum
	checksum = ~checksum;
	sprintf((char *)line_buf + pos, "%02X", checksum);
	pos += 2;
	
	// \n\r
	sprintf((char *)line_buf + pos, "\r\n");
	pos += 2;
	
	if (pos == fwrite(line_buf, 1, pos, s19_file))
	{
		return VSFERR_NONE;
	}
	else
	{
		return VSFERR_FAIL;
	}
}

vsf_err_t write_s19_file(FILE *s19_file, uint32_t file_addr,
						uint8_t *buff, uint32_t buff_size,
						uint32_t seg_addr, uint32_t start_addr,
						ADJUST_MAPPING_CALLBACK remap)
{
	uint8_t tmp;
	uint32_t tmp_addr;
	
	REFERENCE_PARAMETER(file_addr);
	REFERENCE_PARAMETER(seg_addr);
	
	// write data
	while (buff_size)
	{
		if (buff_size > 16)
		{
			tmp = 16;
		}
		else
		{
			tmp = (uint8_t)buff_size;
		}
		
		// write
		tmp_addr = start_addr;
		if ((remap != NULL) && remap(&tmp_addr, 0))
		{
			return VSFERR_FAIL;
		}
		if (write_s19_line(s19_file, (uint8_t)tmp, tmp_addr, S19_S3, buff))
		{
			return VSFERR_FAIL;
		}
		
		start_addr += tmp;
		buff += tmp;
		buff_size -= tmp;
	}
	
	return VSFERR_NONE;
}

vsf_err_t write_s19_file_end(FILE *s19_file)
{
	return write_s19_line(s19_file, 0, 0, S19_S7, NULL);
}

