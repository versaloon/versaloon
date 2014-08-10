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

#include "app_cfg.h"
#if TARGET_SVF_EN

#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"
#include "scripts.h"

#include "filelist.h"
#include "pgbar.h"

#include "byte_tap.h"
#include "svf.h"
#include "svf_player.h"
#include "svf_parser.h"

#define CUR_TARGET_STRING			SVFP_STRING

const struct program_area_map_t svfp_program_area_map[] =
{
	{0, 0, 0, 0, 0, AREA_ATTR_NONE}
};

const struct program_mode_t svfp_program_mode[] =
{
	{'*', SET_FREQUENCY, IFS_JTAG_LL},
	{0, NULL, 0}
};

EXECUTE_HANDLER(svfp);
const struct program_functions_t svfp_program_functions =
{
	EXECUTE_FUNCNAME(svfp),			// execute
	NULL, NULL, NULL, NULL, NULL
};


#define SVF_SET_FREQ_CMD			"FREQUENCY %.02f HZ"

VSS_HANDLER(svfp_help)
{
	VSS_CHECK_ARGC(1);
	PRINTF("Usage of %s:"LOG_LINE_END, CUR_TARGET_STRING);
	PRINTF("  -F,  --frequency <FREQUENCY>              set JTAG frequency, in KHz"LOG_LINE_END);
	PRINTF(LOG_LINE_END);
	return VSFERR_NONE;
}

const struct vss_cmd_t svfp_notifier[] =
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				svfp_help,
				NULL),
	VSS_CMD_END
};




extern struct filelist *fl_in;

EXECUTE_HANDLER(svfp)
{
	char *first_command = NULL;
	struct program_info_t *pi = context->pi;
	FILE *svf_file = NULL;
	uint32_t svf_file_size = 0, command_num = 0;
	char *svfp_command_buffer = NULL;
	uint32_t svfp_command_buffer_len = 0;
	vsf_err_t err = VSFERR_NONE;
	
	if (pi->frequency)
	{
		first_command = (char*)malloc(strlen(SVF_SET_FREQ_CMD) + 20);
		sprintf(first_command, SVF_SET_FREQ_CMD, (float)pi->frequency * 1000);
	}
	
	if ((NULL == fl_in) || (NULL == fl_in->path) || (NULL == fl_in->file)
		|| (strlen(fl_in->path) <= 4)
		|| (toupper(fl_in->path[strlen(fl_in->path) - 4]) != '.')
		|| (toupper(fl_in->path[strlen(fl_in->path) - 3]) != 'S')
		|| (toupper(fl_in->path[strlen(fl_in->path) - 2]) != 'V')
		|| (toupper(fl_in->path[strlen(fl_in->path) - 1]) != 'F'))
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "svf file");
		return VSFERR_FAIL;
	}
	svf_file = fl_in->file;
	
	fseek(svf_file, 0L, SEEK_END);
	svf_file_size = ftell(svf_file);
	rewind(svf_file);
	
	svf_parser_init();
	
	if (tap_init(context->prog))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "open jtag");
		svf_parser_fini();
		return ERRCODE_FAILURE_OPERATION;
	}
	
	if (first_command != NULL)
	{
		if (svf_parser_run_command(first_command))
		{
			LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "execute first command",
						first_command);
			err = VSFERR_FAIL;
			goto leave_program_mode;
		}
	}
	
	if (verbosity < DEBUG_LEVEL)
	{
		pgbar_init("executing svf |", "|", 0, svf_file_size,
				   PROGRESS_STEP, '=');
	}
	
	// parse commands and run
	while (!svf_parser_get_command(svf_file, &svfp_command_buffer,
											  &svfp_command_buffer_len))
	{
		if (svf_parser_run_command(svfp_command_buffer))
		{
			if (verbosity < DEBUG_LEVEL)
			{
				pgbar_fini();
			}
			LOG_ERROR("Command execute failed at line %d", svf_line_number);
			err = VSFERR_FAIL;
			goto leave_program_mode;
		}
		if (verbosity < DEBUG_LEVEL)
		{
			if (svf_file_index * 100 / svf_file_size > 3)
			{
				pgbar_update(svf_file_index);
				svf_file_index = 0;
			}
		}
		command_num++;
	}
	
	// commit last commands
	if (tap_commit())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "do jtag");
		err = VSFERR_FAIL;
		goto leave_program_mode;
	}
	else if (svf_parser_check_tdo())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "check tdo data");
		err = VSFERR_FAIL;
		goto leave_program_mode;
	}
	
	if (verbosity < DEBUG_LEVEL)
	{
		pgbar_update(svf_file_index);
		pgbar_fini();
	}
	LOG_INFO("%d commands execute finised OK", command_num);
	
leave_program_mode:
	// free all
	if (first_command != NULL)
	{
		free(first_command);
		first_command = NULL;
	}
	tap_fini();
	tap_commit();
	svf_parser_fini();
	if (svfp_command_buffer)
	{
		free(svfp_command_buffer);
		svfp_command_buffer = NULL;
	}
	
	return err;
}

#endif
