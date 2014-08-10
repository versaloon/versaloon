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

#include "hex.h"

enum  HEX_TYPE
{
	HEX_TYPE_DATA				= 0x00,
	HEX_TYPE_EOF				= 0x01,
	HEX_TYPE_EXT_SEG_ADDR		= 0x02,
	HEX_TYPE_START_SEG_ADDR		= 0x03,
	HEX_TYPE_EXT_LINEAR_ADDR	= 0x04,
	HEX_TYPE_START_LINEAR_ADDR	= 0x05
};

vsf_err_t read_hex_file(FILE *hex_file, WRITE_MEMORY_CALLBACK callback,
						void *buffer, uint32_t seg_offset, uint32_t addr_offset)
{
	uint8_t line_buf[10 + 0xFF * 2 + 2], checksum;
	char ch, *ptr, tmp_buff[3];
	uint32_t length, i, ext_addr0 = 0, ext_addr1 = 0, addr = 0;
	uint16_t seg_addr = 0;
	
#ifdef PARAM_CHECK
	if ((NULL == hex_file) || (NULL == callback))
	{
		LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
		return VSFERR_INVALID_PARAMETER;
	}
#endif
	
	rewind(hex_file);
	while (!feof(hex_file))
	{
		// ignore empty lines
		do{
			ch = (char)fgetc(hex_file);
			if (EOF == ch)
			{
				return VSFERR_NONE;
			}
		}while (('\n' == ch) || ('\r' == ch));
		// check first character of a line, which MUST be ':'
		if (ch != ':')
		{
			return VSFERR_FAIL;
		}
		length = 0;
		// read line
		do{
			ch = (char)fgetc(hex_file);
			line_buf[length++] = ch;
		}while ((ch != '\n') && (ch != '\r') && (ch != EOF));
		length--;
		
		// process line
		if ((length < 10) || ((length % 2) == 1))
		{
			return VSFERR_FAIL;
		}
		tmp_buff[2] = '\0';
		for (i = 0; i < length; i+=2)
		{
			tmp_buff[0] = line_buf[i];
			tmp_buff[1] = line_buf[i + 1];
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
		if ((length + 5) != i)
		{
			return VSFERR_FAIL;
		}
		checksum = 0;
		while (i > 0)
		{
			checksum += line_buf[i-- - 1];
		}
		if (checksum != 0)
		{
			return VSFERR_FAIL;
		}
		
		// process data according to data type
		addr = GET_BE_U16(&line_buf[1]);
		switch (line_buf[3])
		{
		case HEX_TYPE_DATA: //Type 0
			// data record
			if (callback(fileparser_cur_ext,
					ext_addr0 + ext_addr1 + addr_offset + addr,
					seg_addr + seg_offset, &line_buf[4], length, buffer))
			{
				return VSFERR_FAIL;
			}
			break;
		case HEX_TYPE_EOF: // Type 1
			// end of file
			return VSFERR_NONE;
			break;
		case HEX_TYPE_EXT_SEG_ADDR: //Type 2
			// bit 4-19 of address
			if ((length != 2) || (addr != 0))
			{
				return VSFERR_FAIL;
			}
			ext_addr0 = (line_buf[4] << 12) | (line_buf[5] << 4);
			break;
		case HEX_TYPE_START_SEG_ADDR: //Type3
			// segment address
			if (addr != 0)
			{
				return VSFERR_FAIL;
			}
			seg_addr = (line_buf[4] << 8) | line_buf[5];
			break;
		case HEX_TYPE_EXT_LINEAR_ADDR: //Type 4
			// high 16 bit of address
			if ((length != 2) || (addr != 0))
			{
				return VSFERR_FAIL;
			}
			ext_addr1 = (line_buf[4] << 24) | (line_buf[5] << 16);
			break;
		case HEX_TYPE_START_LINEAR_ADDR: //Type 5
			//Skip hex record type 5 - Start Linear Address Record , but don't log a warning
			//The Start Linear Address Record is used to specify the execution start address for the object file.
			break;
		default:
			LOG_WARNING(ERRMSG_INVALID_VALUE_MESSAGE, line_buf[3],
						"hex type", "current line ignored!!");
			break;
		}
	}
	
	return VSFERR_FAIL;
}

static vsf_err_t write_hex_line(FILE *hex_file, uint8_t data_len,
								uint16_t data_addr, uint8_t type, uint8_t *data)
{
	uint8_t line_buf[10 + 0xFF * 2 + 2], checksum = 0, pos = 0;
	uint32_t i;
	
#ifdef PARAM_CHECK
	if ((NULL == hex_file) || ((data_len > 0) && (NULL == data)))
	{
		LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
		return VSFERR_INVALID_PARAMETER;
	}
#endif
	
	memset(line_buf, 0, sizeof(line_buf));
	line_buf[pos] = ':';
	pos += 1;
	
	// data length
	checksum += data_len;
	sprintf((char *)line_buf + pos, "%02X", data_len);
	pos += 2;
	
	// address
	checksum += (uint8_t)data_addr;
	checksum += (uint8_t)(data_addr >> 8);
	sprintf((char *)line_buf + pos, "%04X", data_addr);
	pos += 4;
	
	// type
	checksum += type;
	sprintf((char *)line_buf + pos, "%02X", type);
	pos += 2;
	
	// data
	for (i = 0; i < data_len; i++)
	{
		checksum += data[i];
		sprintf((char *)line_buf + pos, "%02X", data[i]);
		pos += 2;
	}
	
	// checksum
	checksum = ~checksum + 1;
	sprintf((char *)line_buf + pos, "%02X", checksum);
	pos += 2;
	
	// \n\r
	sprintf((char *)line_buf + pos, "\r\n");
	pos += 2;
	
	if (pos == fwrite(line_buf, 1, pos, hex_file))
	{
		return VSFERR_NONE;
	}
	else
	{
		return VSFERR_FAIL;
	}
}

vsf_err_t write_hex_file(FILE *hex_file, uint32_t file_addr,
						uint8_t *buff, uint32_t buff_size,
						uint32_t seg_addr, uint32_t start_addr,
						ADJUST_MAPPING_CALLBACK remap)
{
	uint8_t tmp;
	uint16_t addr_high_orig, addr_tmp_big_endian;
	uint32_t tmp_addr;
	
	file_addr = file_addr;
	
	// write seg_addr
	seg_addr = ((seg_addr >> 8) & 0x000000FF) | ((seg_addr << 8) & 0x0000FF00);
	if (write_hex_line(hex_file, 2, (uint16_t)0, HEX_TYPE_START_SEG_ADDR,
							(uint8_t *)&seg_addr))
	{
		return VSFERR_FAIL;
	}
	
	// write ext_addr
	addr_high_orig = start_addr >> 16;
	addr_tmp_big_endian = ((addr_high_orig >> 8) & 0x000000FF)
							| ((addr_high_orig << 8) & 0x0000FF00);
	if (write_hex_line(hex_file, 2, (uint16_t)0, HEX_TYPE_EXT_LINEAR_ADDR,
							(uint8_t *)&addr_tmp_big_endian))
	{
		return VSFERR_FAIL;
	}
	
	// write data
	while (buff_size)
	{
		// write ext_addr if necessary
		if (addr_high_orig != (start_addr >> 16))
		{
			addr_high_orig = start_addr >> 16;
			addr_tmp_big_endian = ((addr_high_orig >> 8) & 0x000000FF)
									| ((addr_high_orig << 8) & 0x0000FF00);
			if (write_hex_line(hex_file, 2, (uint16_t)0, HEX_TYPE_EXT_LINEAR_ADDR,
									(uint8_t *)&addr_tmp_big_endian))
			{
				return VSFERR_FAIL;
			}
		}
		
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
		if (write_hex_line(hex_file, (uint8_t)tmp,
						(uint16_t)(tmp_addr & 0xFFFF), HEX_TYPE_DATA, buff))
		{
			return VSFERR_FAIL;
		}
		
		start_addr += tmp;
		buff += tmp;
		buff_size -= tmp;
	}
	
	return VSFERR_NONE;
}

vsf_err_t write_hex_file_end(FILE *hex_file)
{
	if (fwrite(":00000001FF\r\n", 1, 13, hex_file) != 13)
	{
		return VSFERR_FAIL;
	}
	else
	{
		return VSFERR_NONE;
	}
}

