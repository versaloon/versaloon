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

#include "compiler.h"

#include "vsf_err.h"

#include "app_type.h"
#include "app_io.h"
#include "app_log.h"
#include "app_err.h"

#include "fileparser.h"

#include "port.h"

#include "hex.h"
#include "s19.h"

vsf_err_t read_bin_file(FILE *bin_file, WRITE_MEMORY_CALLBACK callback,
					void *buffer, uint32_t seg_offset, uint32_t addr_offset);
vsf_err_t write_bin_file(FILE *bin_file, uint32_t file_addr, uint8_t *buff,
					uint32_t buff_size, uint32_t seg_addr, uint32_t start_addr,
					ADJUST_MAPPING_CALLBACK remap);

char *fileparser_cur_ext = NULL;
static struct file_parser_t file_parser[] =
{
	{"HEX", read_hex_file, write_hex_file, write_hex_file_end},
	{"BIN", read_bin_file, write_bin_file, NULL},
	{"S19", read_s19_file, write_s19_file, write_s19_file_end}
};

static uint8_t check_file_ext(char *file_name, char *ext)
{
	uint8_t i;
	char *file_ext = &file_name[strlen(file_name) - 1];
	
	while ((*file_ext != '.') && (*file_ext != FILE_SEPARATOR))
	{
		file_ext--;
	}
	if ((*file_ext != '.') || ('\0' == *(file_ext + 1)))
	{
		return 0;
	}
	file_ext++;
	
	// check
	for (i = 0; i < strlen(ext); i++)
	{
		if (toupper(file_ext[i]) != ext[i])
		{
			return 0;
		}
	}
	return 1;
}

static vsf_err_t get_file_parser(char *file_name, uint8_t *index)
{
	uint8_t i;
	
	for (i = 0; i < dimof(file_parser); i++)
	{
		// check file ext
		if (check_file_ext(file_name, file_parser[i].ext))
		{
			break;
		}
	}
	if (i >= dimof(file_parser))
	{
		// file type not supported
		return VSFERR_FAIL;
	}
	
	*index = i;
	return VSFERR_NONE;
}

vsf_err_t parse_file(char *file_name, FILE *file, void *para,
				  WRITE_MEMORY_CALLBACK callback,
				  uint32_t seg_offset, uint32_t addr_offset)
{
	uint8_t i;
	
	if (get_file_parser(file_name, &i) || (NULL == file_parser[i].parse_file))
	{
		// hope target handler will handle this file
		return VSFERR_NONE;
	}
	
	fileparser_cur_ext = file_parser[i].ext;
	return file_parser[i].parse_file(file, callback, para,
										seg_offset, addr_offset);
}

vsf_err_t end_file(struct filelist *fl)
{
	if (NULL == fl)
	{
		return VSFERR_FAIL;
	}
	
	do {
		uint8_t i;
		
		if ((fl->file != NULL) && (fl->path != NULL) && fl->access)
		{
			if (get_file_parser(fl->path, &i))
			{
				continue;
			}
			
			if ((file_parser[i].end_file != NULL)
				&& file_parser[i].end_file(fl->file))
			{
				return VSFERR_FAIL;
			}
		}
		
		fl = FILELIST_GetNext(fl);
	} while (fl != NULL);
	
	return VSFERR_NONE;
}

vsf_err_t save_target_to_file(struct filelist *fl, uint8_t *buff,
					uint32_t buff_size, uint32_t seg_addr, uint32_t start_addr,
					int32_t fseg, int32_t faddr, ADJUST_MAPPING_CALLBACK remap)
{
	uint8_t i;
	struct filelist *target_file = fl;
	
	if (NULL == fl)
	{
		return VSFERR_FAIL;
	}
	
	// find a most suitable file to write
	do {
		if (((seg_addr + fseg) == fl->seg_offset)
			&& ((start_addr + faddr) >= fl->addr_offset))
		{
			target_file = fl;
			// if start_addr is match, that's it
			// else find for better match
			if (start_addr == fl->addr_offset)
			{
				break;
			}
		}
		fl = FILELIST_GetNext(fl);
	} while ((fl != NULL) && (fl->path != NULL));
	
	// write target to file
	if (NULL == target_file->path)
	{
		return VSFERR_FAIL;
	}
	if (NULL == target_file->file)
	{
		target_file->file = fopen(target_file->path, "wb");
		if (NULL == target_file->file)
		{
			LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "open", target_file->path);
			return VSFERR_FAIL;
		}
	}
	
	if (get_file_parser(target_file->path, &i)
		|| (NULL == file_parser[i].save_target_to_file))
	{
		return VSFERR_FAIL;
	}
	
	target_file->access = 1;
	return file_parser[i].save_target_to_file(target_file->file, start_addr,
								buff, buff_size, seg_addr, start_addr, remap);
}

vsf_err_t read_bin_file(FILE *bin_file, WRITE_MEMORY_CALLBACK callback,
					 void *buffer, uint32_t seg_offset, uint32_t addr_offset)
{
	uint8_t cur_buff[8];
	uint32_t addr = 0, cur_len;
	
	rewind(bin_file);
	while (1)
	{
		cur_len = fread(cur_buff, 1, sizeof(cur_buff), bin_file);
		if (0 == cur_len)
		{
			if (feof(bin_file))
			{
				break;
			}
			else
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read input binary file");
				return ERRCODE_FAILURE_OPERATION;
			}
		}
		
		if (callback(fileparser_cur_ext, addr + addr_offset, seg_offset,
						cur_buff, cur_len, buffer))
		{
			return VSFERR_FAIL;
		}
		addr += cur_len;
	}
	
	return VSFERR_NONE;
}

vsf_err_t write_bin_file(FILE *bin_file, uint32_t file_addr,
						uint8_t *buff, uint32_t buff_size,
						uint32_t seg_addr, uint32_t start_addr,
						ADJUST_MAPPING_CALLBACK remap)
{
	uint32_t file_size = 0;
	vsf_err_t err = VSFERR_NONE;
	
	REFERENCE_PARAMETER(remap);
	
	// seg_addr is not used in binary file
	seg_addr = seg_addr;
	
	// check
	if (start_addr < file_addr)
	{
		return VSFERR_FAIL;
	}
	
	// get file size
	fseek(bin_file, 0L, SEEK_END);
	file_size = ftell(bin_file);
	rewind(bin_file);
	
	if (file_size < (start_addr - file_addr))
	{
		uint32_t append_size = start_addr - file_addr - file_size;
		uint8_t *buff_append = (uint8_t *)malloc(append_size);
		if (NULL == buff_append)
		{
			return VSFERR_FAIL;
		}
		memset(buff_append, 0, append_size);
		
		if (fwrite(buff_append, 1, append_size, bin_file) != append_size)
		{
			err = VSFERR_FAIL;
		}
		free(buff_append);
		buff_append = NULL;
	}
	if (err)
	{
		return err;
	}
	
	// write data
	if (fwrite(buff, 1, buff_size, bin_file) != buff_size)
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

