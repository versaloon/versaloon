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

#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"

#include "memlist.h"
#include "filelist.h"
#include "fileparser.h"

extern struct filelist *fl_in, *fl_out;

static void target_free_data_buffer(struct program_context_t *context)
{
	struct program_area_t *prog_area = NULL;
	uint8_t i;
	
	if ((NULL == context) || (NULL == context->pi))
	{
		return;
	}
	
	for (i = 0; i < dimof(target_area_name); i++)
	{
		// special_string cannot be freed
		if (SPECIAL_STRING_CHAR == target_area_name[i].name)
		{
			continue;
		}
		
		prog_area = target_get_program_area(context->pi, i);
		if (prog_area != NULL)
		{
			if (prog_area->buff != NULL)
			{
				free(prog_area->buff);
				prog_area->buff = NULL;
			}
			prog_area->size = 0;
			if (prog_area->memlist != NULL)
			{
				MEMLIST_Free(&prog_area->memlist);
			}
			if (prog_area->exact_memlist != NULL)
			{
				MEMLIST_Free(&prog_area->exact_memlist);
			}
		}
	}
}

static vsf_err_t target_alloc_data_buffer(struct program_context_t *context)
{
	struct program_area_t *prog_area = NULL;
	struct chip_area_info_t *area_info = NULL;
	uint8_t i;
	
	if ((NULL == context) || (NULL == context->pi) || (NULL == context->param))
	{
		return VSFERR_FAIL;
	}
	
	for (i = 0; i < dimof(target_area_name); i++)
	{
		prog_area = target_get_program_area(context->pi, i);
		if ((prog_area != NULL) && (NULL == prog_area->buff) &&
			(prog_area->size > 0))
		{
			prog_area->buff = (uint8_t *)malloc(prog_area->size);
			if (NULL == prog_area->buff)
			{
				return VSFERR_NOT_ENOUGH_RESOURCES;
			}
			
			area_info = target_get_chip_area(context->param, i);
			if ((strlen(context->param->chip_name) > 0) && (area_info != NULL))
			{
				memset(prog_area->buff, (uint8_t)area_info->default_value,
						prog_area->size);
			}
		}
	}
	
	return VSFERR_NONE;
}

static vsf_err_t target_write_buffer_from_file_callback(char * ext,
				uint32_t address, uint32_t seg_addr, uint8_t* data,
				uint32_t length, void* buffer)
{
	uint32_t i;
	int8_t area_idx;
	char area_name;
	uint8_t *area_buff;
	struct memlist **area_memlist, **area_exact_memlist;
	uint32_t area_seg, area_addr, area_size, area_page_size;
	struct program_context_t *context = (struct program_context_t *)buffer;
	struct target_info_t *target = NULL;
	struct chip_area_info_t *area_info = NULL;
	struct program_area_t *prog_area = NULL;
	uint32_t mem_addr;
	
	if ((NULL == context) || (NULL == context->pi) || (NULL == context->param) ||
		(NULL == context->target) || !strlen(context->param->chip_name) ||
		(NULL == ext))
	{
		LOG_BUG(ERRMSG_NOT_INITIALIZED, "target", "");
		return VSFERR_FAIL;
	}
	target = context->target;
	
	// remap if adjust_mapping is defined and format is not BIN
	if ((strcmp(ext, "BIN")) &&
		(target->adjust_mapping != NULL) &&
		target->adjust_mapping(&address, TARGET_MAPPING_FROM_FILE))
	{
		LOG_BUG(ERRMSG_FAILURE_OPERATE_ADDRESS, "remap target address",
				address);
		return VSFERR_FAIL;
	}
	
	// find a right target to fill the memory
	i = 0;
	while (target->program_area_map[i].name != 0)
	{
		area_name = target->program_area_map[i].name;
		area_idx = target_area_idx(area_name);
		if (area_idx < 0)
		{
			i++;
			continue;
		}
		area_info = target_get_chip_area(context->param, (uint32_t)area_idx);
		prog_area = target_get_program_area(context->pi, (uint32_t)area_idx);
		if ((NULL == area_info) || (NULL == prog_area))
		{
			i++;
			continue;
		}
		
		area_seg = area_info->seg + target->program_area_map[i].fseg_addr;
		area_addr = area_info->addr + target->program_area_map[i].fstart_addr;
		area_size = area_info->size;
		area_page_size = area_info->page_size;
		
		area_buff = prog_area->buff;
		area_memlist = &(prog_area->memlist);
		area_exact_memlist = &(prog_area->exact_memlist);
		
		if ((area_seg != seg_addr) || (area_addr > address)
			|| ((area_addr + area_size) < (address + length)))
		{
			// not this area
			i++;
			continue;
		}
		
		// found
		if (0 == area_page_size)
		{
			// default page size is 256 bytes
			area_page_size = 256;
		}
		context->pi->areas_defined |= target_area_mask(area_name);
		mem_addr = address - area_addr;
		if (area_buff != NULL)
		{
			// put in area_buff
			memcpy(area_buff + mem_addr, data, length);
			if (MEMLIST_Add(area_memlist, address, length, area_page_size,
							area_buff + mem_addr))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "add memory list");
				return ERRCODE_FAILURE_OPERATION;
			}
			if (MEMLIST_Add(area_exact_memlist, address, length, 1, NULL))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "add memory list");
				return ERRCODE_FAILURE_OPERATION;
			}
		}
		else
		{
			LOG_ERROR(ERRMSG_INVALID_BUFFER, "area_buff");
			return VSFERR_FAIL;
		}
		
		return VSFERR_NONE;
	}
	
	// not found
	return VSFERR_FAIL;
}

vsf_err_t target_data_free(struct program_context_t *context)
{
	target_free_data_buffer(context);
	return VSFERR_NONE;
}

vsf_err_t target_data_read(struct program_context_t *context)
{
	uint32_t file_for_read = 0, file_for_write = 0;
	
	if ((NULL == context) || (NULL == context->pi) || (NULL == context->op))
	{
		return VSFERR_FAIL;
	}
	
	// check file
	target_prepare_operations(context, &file_for_read, &file_for_write);
	if ((file_for_read > 0) &&
		((NULL == fl_in) || (NULL == fl_in->path) || (NULL == fl_in->file)))
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "input file");
		return VSFERR_FAIL;
	}
	if ((file_for_write > 0) &&
		((NULL == fl_out) || (NULL == fl_out->path)))
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "output file");
		return VSFERR_FAIL;
	}
	
	// malloc buffer
	if (target_alloc_data_buffer(context))
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return VSFERR_FAIL;
	}
	
	// read file
	if (file_for_read > 0)
	{
		struct filelist *fl = fl_in;
		
		while ((fl != NULL) && (fl->path != NULL) && (fl->file != NULL)
			&& (strlen(fl->path) > 4))
		{
			if (parse_file(fl->path, fl->file, (void *)context,
								&target_write_buffer_from_file_callback,
								fl->seg_offset, fl->addr_offset))
			{
				LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "parse input file",
							fl->path);
				return VSFERR_FAIL;
			}
			
			fl = FILELIST_GetNext(fl);
		}
	}
	
	return VSFERR_NONE;
}

vsf_err_t target_data_save(struct program_context_t *context)
{
	struct program_area_map_t *p_map;
	
	if ((NULL == context) || (NULL == context->op) || (NULL == context->pi) ||
		(NULL == context->param) || (NULL == context->target) ||
		(NULL == context->target->program_area_map))
	{
		return VSFERR_FAIL;
	}
	p_map = (struct program_area_map_t *)context->target->program_area_map;
	
	while (p_map->name != 0)
	{
		if ((p_map->data_pos) &&
			(context->op->read_operations & target_area_mask(p_map->name)))
		{
			uint8_t *buff = NULL;
			uint32_t size = 0;
			struct chip_area_info_t *area;
			int8_t area_idx;
			
			area_idx = target_area_idx(p_map->name);
			if (area_idx < 0)
			{
				p_map++;
				continue;
			}
			area = target_get_chip_area(context->param, (uint32_t)area_idx);
			if (NULL == area)
			{
				LOG_ERROR(ERRMSG_INVALID_TARGET, "area");
				return VSFERR_FAIL;
			}
			target_get_target_area(context->pi, p_map->name, &buff, &size);
			if ((buff != NULL) && (size > 0) && (fl_out != NULL))
			{
				if (save_target_to_file(fl_out, buff,
						size, area->seg, area->addr, p_map->fseg_addr,
						p_map->fstart_addr,
						context->target->adjust_mapping))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION,
								"write data to file");
					return VSFERR_FAIL;
				}
			}
		}
		
		p_map++;
	}
	end_file(fl_out);
	
	return VSFERR_NONE;
}

static uint32_t target_get_memlist_count(struct memlist *list)
{
	uint32_t count = 0;
	
	while (list != NULL)
	{
		count++;
		list = MEMLIST_GetNext(list);
	}
	return count;
}

vsf_err_t target_generate_data(struct target_cfg_data_info_t *cfg_data_info,
					struct program_context_t *context, const char *filename)
{
	FILE *datafile;
	bool little;
	uint8_t align;
	uint32_t offset, addrwidth;
	uint32_t pa_size, data_size, memlist_size;
	uint32_t data_pos, target_data_pos;
	uint32_t temp_len;
	struct program_area_t *prog_area = NULL;
	struct memlist *temp_list = NULL;
	uint32_t i;
	uint8_t *buff, *buff_ptr;
	
	if ((NULL == cfg_data_info) || (NULL == filename) || (NULL == context) ||
		(NULL == context->target) || (NULL == context->pi) ||
		(NULL == context->target->program_area_map) ||
		(cfg_data_info->align != 4) || (cfg_data_info->addr_width != 32))
	{
		return VSFERR_INVALID_PARAMETER;
	}
	
	offset = cfg_data_info->addr;
	addrwidth = cfg_data_info->addr_width / 8;
	align = cfg_data_info->align;
	little = cfg_data_info->little_endian;
	
	datafile = fopen(filename, "wb");
	if (NULL == datafile)
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "open", filename);
		return VSFERR_FAIL;
	}
	
	#define DATASIZE(len)	(((len) + align - 1) / align) * align
	
	memlist_size = 2 * sizeof(uint32_t) + 2 * addrwidth;
	pa_size = dimof(target_area_name) * (4 * addrwidth + sizeof(uint32_t));
	pa_size = DATASIZE(pa_size);
	buff = (uint8_t *)malloc(pa_size);
	if (NULL == buff)
	{
		fclose(datafile);
		datafile = NULL;
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return VSFERR_FAIL;
	}
	memset(buff, 0, pa_size);
	
	#define SET_U8(p, v)					\
		do {\
			*(uint8_t *)(p) = (uint8_t)(v);\
			(p) += 1;\
		} while (0)
	#define SET_U16(p, v)					\
		do {\
			if (little)\
			{\
				SET_LE_U16((p), (v));\
			}\
			else\
			{\
				SET_BE_U16((p), (v));\
			}\
			(p) += 2;\
		} while (0)
	#define SET_U32(p, v)					\
		do {\
			if (little)\
			{\
				SET_LE_U32((p), (v));\
			}\
			else\
			{\
				SET_BE_U32((p), (v));\
			}\
			(p) += 4;\
		} while (0)
	#define SET_U64(p, v)					\
		do {\
			if (little)\
			{\
				SET_LE_U64((p), (v));\
			}\
			else\
			{\
				SET_BE_U64((p), (v));\
			}\
			(p) += 8;\
		} while (0)
	#define SET_U32(p, v)					\
		do {\
			if (little)\
			{\
				SET_LE_U32((p), (v));\
			}\
			else\
			{\
				SET_BE_U32((p), (v));\
			}\
			(p) += 4;\
		} while (0)
	#define SET_ABS_PTR(p, v)				\
		do {\
			switch (addrwidth)\
			{\
			case 1:\
				SET_U8((p), (v));\
				break;\
			case 2:\
				SET_U16((p), (v));\
				break;\
			case 4:\
				SET_U32((p), (v));\
				break;\
			case 8:\
				SET_U64((p), (v));\
				break;\
			}\
		} while (0)
	#define SET_PTR(p, v)					SET_ABS_PTR((p), offset + (v))
	#define SET_DATA_PTR(p, v, size)		\
		do {\
			SET_PTR((p), (v));\
			data_pos += size;\
		} while (0)
	#define SET_DATA(p, src, copy_len, len)	\
			do {\
				memcpy((p), (src), (copy_len));\
				(p) += (len);\
			} while (0)
	
	target_data_pos = 0;
	data_pos = pa_size;
	buff_ptr = buff;
	for (i = 0; i < dimof(target_area_name); i++)
	{
		prog_area = target_get_program_area(context->pi, i);
		if (NULL == prog_area)
		{
			fclose(datafile);
			datafile = NULL;
			free(buff);
			buff = NULL;
			return VSFERR_FAIL;
		}
		
		if ((prog_area->size > 0) && (prog_area->memlist != NULL) &&
			(prog_area->exact_memlist != NULL))
		{
			//	struct program_area_t
			//	{
			//		char *cli_str;
			if (prog_area->cli_str != NULL)
			{
				SET_PTR(buff_ptr, data_pos);
				data_pos += DATASIZE(strlen(prog_area->cli_str) + 1);
			}
			else
			{
				SET_ABS_PTR(buff_ptr, 0);
			}
			//		uint8_t *buff;
			SET_ABS_PTR(buff_ptr, 0);
			//		uint32_t size;
			SET_U32(buff_ptr, prog_area->size);
			//		struct memlist *memlist;
			SET_PTR(buff_ptr, data_pos);
			temp_len = DATASIZE(memlist_size);
			data_pos += temp_len * target_get_memlist_count(prog_area->memlist);
			temp_list = prog_area->memlist;
			while (temp_list != NULL)
			{
				target_data_pos += DATASIZE(temp_list->len);
				temp_list = MEMLIST_GetNext(temp_list);
			}
			//		struct memlist *exact_memlist;
			SET_PTR(buff_ptr, data_pos);
			temp_len = DATASIZE(memlist_size);
			data_pos += temp_len *
							target_get_memlist_count(prog_area->exact_memlist);
			//	}
		}
		else
		{
			// defined in command line, simply set to 0 here
			//	struct program_area_t
			//	{
			//		char *cli_str;
			SET_ABS_PTR(buff_ptr, 0);
			//		uint8_t *buff;
			SET_ABS_PTR(buff_ptr, 0);
			//		uint32_t size;
			SET_U32(buff_ptr, 0);
			//		struct memlist *memlist;
			SET_ABS_PTR(buff_ptr, 0);
			//		struct memlist *exact_memlist;
			SET_ABS_PTR(buff_ptr, 0);
			//	}
		}
	}
	if (fwrite(buff, 1, pa_size, datafile) != pa_size)
	{
		free(buff);
		buff = NULL;
		fclose(datafile);
		datafile = NULL;
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "write data to file");
		return VSFERR_FAIL;
	}
	free(buff);
	buff = NULL;
	
	data_size = target_data_pos + data_pos - pa_size;
	target_data_pos = data_pos;
	buff = (uint8_t *)malloc(data_size);
	if (NULL == buff)
	{
		fclose(datafile);
		datafile = NULL;
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return VSFERR_FAIL;
	}
	memset(buff, 0, data_size);
	buff_ptr = buff;
	for (i = 0; i < dimof(target_area_name); i++)
	{
		prog_area = target_get_program_area(context->pi, i);
		if (NULL == prog_area)
		{
			fclose(datafile);
			datafile = NULL;
			free(buff);
			buff = NULL;
			return VSFERR_FAIL;
		}
		
		if ((prog_area->size > 0) && (prog_area->memlist != NULL) &&
			(prog_area->exact_memlist != NULL))
		{
			//	struct program_area_t
			//	{
			//		char *cli_str;
			if (prog_area->cli_str != NULL)
			{
				temp_len = strlen(prog_area->cli_str) + 1;
				SET_DATA(buff_ptr, prog_area->cli_str, temp_len,
							DATASIZE(temp_len));
			}
			//		uint8_t *buff;
			//		uint32_t size;
			//		struct memlist *memlist;
			temp_list = prog_area->memlist;
			while (temp_list != NULL)
			{
				//	struct memlist
				//	{
				// 		uint32_t addr;
				SET_U32(buff_ptr, temp_list->addr);
				// 		uint32_t len;
				SET_U32(buff_ptr, temp_list->len);
				// 		uint8_t *buff;
				SET_PTR(buff_ptr, data_pos);
				data_pos += DATASIZE(temp_list->len);
				temp_list = MEMLIST_GetNext(temp_list);
				//		struct sllist list;
				if (temp_list != NULL)
				{
					SET_PTR(buff_ptr,
						pa_size + buff_ptr - buff + DATASIZE(memlist_size));
				}
				else
				{
					SET_ABS_PTR(buff_ptr, 0);
				}
				//	}
			}
			//		struct memlist *exact_memlist;
			temp_list = prog_area->exact_memlist;
			while (temp_list != NULL)
			{
				//	struct memlist
				//	{
				// 		uint32_t addr;
				SET_U32(buff_ptr, temp_list->addr);
				// 		uint32_t len;
				SET_U32(buff_ptr, temp_list->len);
				// 		uint8_t *buff;
				SET_ABS_PTR(buff_ptr, 0);
				temp_list = MEMLIST_GetNext(temp_list);
				//		struct sllist list;
				if (temp_list != NULL)
				{
					SET_PTR(buff_ptr,
						pa_size + buff_ptr - buff + DATASIZE(memlist_size));
				}
				else
				{
					SET_ABS_PTR(buff_ptr, 0);
				}
				//	}
			}
			//	}
		}
	}
	for (i = 0; i < dimof(target_area_name); i++)
	{
		prog_area = target_get_program_area(context->pi, i);
		if (NULL == prog_area)
		{
			fclose(datafile);
			datafile = NULL;
			free(buff);
			buff = NULL;
			return VSFERR_FAIL;
		}
		
		if ((prog_area->size > 0) && (prog_area->memlist != NULL) &&
			(prog_area->exact_memlist != NULL))
		{
			temp_list = prog_area->memlist;
			while (temp_list != NULL)
			{
				SET_DATA(buff_ptr, temp_list->buff, temp_list->len,
							DATASIZE(temp_list->len));
				temp_list = MEMLIST_GetNext(temp_list);
			}
		}
	}
	if (fwrite(buff, 1, data_size, datafile) != data_size)
	{
		free(buff);
		buff = NULL;
		fclose(datafile);
		datafile = NULL;
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "write data to file");
		return VSFERR_FAIL;
	}
	free(buff);
	buff = NULL;
	
	fclose(datafile);
	datafile = NULL;
	return VSFERR_NONE;
}
