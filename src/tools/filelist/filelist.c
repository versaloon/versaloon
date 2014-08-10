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
#include "app_log.h"
#include "app_err.h"

#include "scripts.h"
#include "filelist.h"
#include "strparser.h"

VSS_HANDLER(filelist_add_inputfile);
VSS_HANDLER(filelist_add_outputfile);
static const struct vss_cmd_t filelist_cmd[] =
{
	VSS_CMD(	"input-file",
				"add input file, format: input-file/I FILE[@SEG,ADDR]",
				filelist_add_inputfile,
				NULL),
	VSS_CMD(	"I",
				"add input file, format: input-file/I FILE[@SEG,ADDR]",
				filelist_add_inputfile,
				NULL),
	VSS_CMD(	"output-file",
				"add output file, format: output-file/O FILE[@SEG,ADDR]",
				filelist_add_outputfile,
				NULL),
	VSS_CMD(	"O",
				"add output file, format: output-file/O FILE[@SEG,ADDR]",
				filelist_add_outputfile,
				NULL),
	VSS_CMD_END
};
struct vss_cmd_list_t filelist_cmd_list = VSS_CMD_LIST("filelist", filelist_cmd);

static void FILELIST_InsertLast(struct filelist *fl, struct filelist *newitem)
{
	if (NULL == fl)
	{
		return;
	}
	
	while (FILELIST_GetNext(fl) != NULL)
	{
		fl = FILELIST_GetNext(fl);
	}
	
	sllist_insert(fl->list, newitem->list);
}

vsf_err_t FILELIST_Open(struct filelist *fl, char *attr)
{
	if (NULL == fl)
	{
		return VSFERR_FAIL;
	}
	
	do{
		if ((NULL == fl->file) && (fl->path != NULL))
		{
			fl->file = fopen((const char *)fl->path, (const char *)attr);
			if (NULL == fl->file)
			{
				return VSFERR_FAIL;
			}
		}
		fl = FILELIST_GetNext(fl);
	} while(fl != NULL);
	
	return VSFERR_NONE;
}

vsf_err_t FILELIST_Add(struct filelist **fl, char *path, uint32_t seg_offset,
					uint32_t addr_offset)
{
	struct filelist *newitem = NULL;
	
	if (NULL == fl)
	{
		return VSFERR_FAIL;
	}
	
	newitem = (struct filelist*)malloc(sizeof(struct filelist));
	if (NULL == newitem)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return VSFERR_FAIL;
	}
	newitem->path = (char *)malloc(strlen(path) + 1);
	if (NULL == newitem->path)
	{
		free(newitem);
		newitem = NULL;
		
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return VSFERR_FAIL;
	}
	strcpy(newitem->path, path);
	newitem->seg_offset = seg_offset;
	newitem->addr_offset = addr_offset;
	sllist_init_node(newitem->list);
	newitem->file = NULL;
	newitem->access = 0;
	
	if (NULL == *fl)
	{
		*fl = newitem;
	}
	else
	{
		FILELIST_InsertLast(*fl, newitem);
	}
	
	return VSFERR_NONE;
}

void FILELIST_Free(struct filelist **fl)
{
	struct filelist *tmp1, *tmp2;
	
	if (NULL == fl)
	{
		return;
	}
	
	tmp1 = *fl;
	while (tmp1 != NULL)
	{
		tmp2 = tmp1;
		tmp1 = FILELIST_GetNext(tmp1);
		sllist_init_node(tmp2->list);
		if (tmp2->path != NULL)
		{
			free(tmp2->path);
			tmp2->path = NULL;
		}
		if (tmp2->file != NULL)
		{
			fclose(tmp2->file);
			tmp2->file = NULL;
		}
		free(tmp2);
	}
	tmp1 = tmp2 = NULL;
	*fl = NULL;
}

extern struct filelist *fl_in, *fl_out;
static vsf_err_t filelist_add_file(struct filelist **fl, char *file)
{
	uint32_t seg_offset, addr_offset;
	uint32_t i;
	
	for (i = strlen(file) - 1; i > 0; i--)
	{
		if ('@' == file[i])
		{
			break;
		}
	}
	seg_offset = addr_offset = 0;
	if (i > 0)
	{
		uint32_t buff[2];
		char format[] = "%4d,%4d";
		
		file[i] = '\0';
		if (strparser_parse(&file[i + 1], format, (uint8_t*)buff, sizeof(buff)))
		{
			LOG_ERROR(ERRMSG_NOT_SUPPORT_AS, &file[i + 1], format);
			return VSFERR_FAIL;
		}
		seg_offset = buff[0];
		addr_offset = buff[1];
	}
	
	if (FILELIST_Add(fl, file, seg_offset, addr_offset))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "add file: ", file);
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t filelist_check_collision(struct filelist *fl1,
										struct filelist *fl2)
{
	struct filelist *fl_out_tmp = fl1;
	
	while (fl_out_tmp != NULL)
	{
		struct filelist *fl_in_tmp = fl2;
		
		while (fl_in_tmp != NULL)
		{
			if (!strcmp(fl_out_tmp->path, fl_in_tmp->path))
			{
				return VSFERR_FAIL;
			}
			
			fl_in_tmp = FILELIST_GetNext(fl_in_tmp);
		}
		
		fl_out_tmp = FILELIST_GetNext(fl_out_tmp);
	}
	
	return VSFERR_NONE;
}

VSS_HANDLER(filelist_add_inputfile)
{
	VSS_CHECK_ARGC_2(1, 2);
	
	if (1 == argc)
	{
		FILELIST_Free(&fl_in);
		return VSFERR_NONE;
	}
	
	if (filelist_add_file(&fl_in, (char *)argv[1]))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "add input file");
		return VSFERR_FAIL;
	}
	if (filelist_check_collision(fl_in, fl_out))
	{
		LOG_ERROR("inputfile CANNOT be meanwhile outputfile!");
		return VSFERR_FAIL;
	}
	if ((fl_in != NULL) && FILELIST_Open(fl_in, "rb"))
	{
		FILELIST_Free(&fl_in);
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "open input file");
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

VSS_HANDLER(filelist_add_outputfile)
{
	VSS_CHECK_ARGC_2(1, 2);
	
	if (1 == argc)
	{
		FILELIST_Free(&fl_out);
		return VSFERR_NONE;
	}
	
	if (filelist_add_file(&fl_out, (char *)argv[1]))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "add output file");
		return VSFERR_FAIL;
	}
	if (filelist_check_collision(fl_in, fl_out))
	{
		LOG_ERROR("inputfile CANNOT be meanwhile outputfile!");
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

