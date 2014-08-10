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

#include "port.h"
#include "app_cfg.h"
#if TARGET_COMISP_EN
#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "interfaces.h"
#include "target.h"
#include "scripts.h"

#include "strparser.h"

#include "comisp.h"
#include "stm32isp.h"
#include "lpcarmisp.h"

#include "comisp_internal.h"

#include "comport.h"

#define CUR_TARGET_STRING			COMISP_STRING

VSS_HANDLER(comisp_comm);
VSS_HANDLER(comisp_print_cominfo);

static const struct vss_cmd_t comisp_cmd[] =
{
	VSS_CMD(	"comport",
				"set com port, format: "
				"comport/C COM_BAUDRATE_DATALEN_PARITY_STOP_HANDSHAKE_EXTRA",
				comisp_comm,
				NULL),
	VSS_CMD(	"C",
				"set com port, format: "
				"comport/C COM_BAUDRATE_DATALEN_PARITY_STOP_HANDSHAKE_EXTRA",
				comisp_comm,
				NULL),
	VSS_CMD_END
};
struct vss_cmd_list_t comisp_cmd_list = VSS_CMD_LIST("comisp", comisp_cmd);

ENTER_PROGRAM_MODE_HANDLER(comisp);
LEAVE_PROGRAM_MODE_HANDLER(comisp);
struct program_functions_t comisp_program_functions;

const struct comisp_param_t comisp_chips_param[] = {
//	chip_name,			com_mode,																												program_functions
//						{comport,	baudrate,	datalength,	paritybit,			stopbit,		handshake,				aux_pin},
#if TARGET_STM32F1_EN
	{"comisp_stm32",	{"",		-1,			8,			COMM_PARITYBIT_EVEN,COMM_STOPBIT_1,	COMM_PARAMETER_UNSURE,	COMM_PARAMETER_UNSURE}, &stm32isp_program_functions},
#endif
#if TARGET_LPC1000_EN
	{"comisp_lpcarm",	{"",		-1,			8,			COMM_PARITYBIT_NONE,COMM_STOPBIT_1,	COMM_PARAMETER_UNSURE,	COMM_PARAMETER_UNSURE}, &lpcarmisp_program_functions},
#endif
};
static uint8_t comisp_chip_index = 0;

struct com_mode_t com_mode;
static const struct com_mode_t com_mode_default = {"", -1, 0,
		COMM_PARAMETER_UNSURE, COMM_PARAMETER_UNSURE, COMM_PARAMETER_UNSURE,
		COMM_PARAMETER_UNSURE};

VSS_HANDLER(comisp_print_cominfo)
{
	uint8_t i;
	
	VSS_CHECK_ARGC(2);
	
	i = (uint8_t)strtoul(argv[1], NULL, 0);
	
	PRINTF("baudrate = %d, ", comisp_chips_param[i].com_mode.baudrate);
	PRINTF("datalength = %d, ", comisp_chips_param[i].com_mode.datalength);
	PRINTF("paritybit = %c, ", comisp_chips_param[i].com_mode.paritybit);
	PRINTF("stopbit = %c, ", comisp_chips_param[i].com_mode.stopbit);
	PRINTF("handshake = %c, ", comisp_chips_param[i].com_mode.handshake);
	PRINTF("auxpin = %c",comisp_chips_param[i].com_mode.auxpin);
	return VSFERR_NONE;
}

VSS_HANDLER(comisp_comm)
{
	// port: 256 bytes
	// baudrate: 4 bytes
	// datalength, paritybit, stopbit: 3 bytes
	// handle, extra: 2 bytes
	uint8_t comm_setting[256 + 4 + 3 + 2], *ptr;
	vsf_err_t err;
	uint8_t i;
	char* formats[] =
	{
		// port(s):baudrate(4d):datalength(1d):parity(c):stop(1d)
		// :handshake(1d):extra(1d)
		// Eg: COM1:115200_8N1_HA
		"%s%4d%1d%c%c%c%c",
		// port(s):baudrate(4d):datalength(1d):parity(c):stop(1d)
		// Eg: COM1:115200_8N1
		"%s%4d%1d%c%c",
		// port(s):baudrate(4d)
		// Eg: COM1:115200
		"%s%4d",
		// port(s)
		// Eg: COM1
		"%s"
	};
	
	VSS_CHECK_ARGC(2);
	
	err = VSFERR_FAIL;
	for (i = 0; i < dimof(formats); i++)
	{
		memset(comm_setting, 0, sizeof(comm_setting));
		err = strparser_parse((char*)argv[1], formats[i],
								comm_setting, sizeof(comm_setting));
		if (!err)
		{
			break;
		}
	}
	
	if (err)
	{
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);
		vss_print_help(argv[0]);
		return ERRCODE_INVALID_OPTION;
	}
	
	ptr = comm_setting;
	memcpy(&com_mode, &com_mode_default, sizeof(com_mode));
	strncpy(com_mode.comport, (char*)ptr, sizeof(com_mode.comport));
	ptr += strlen(com_mode.comport) + 1;
	if (i < 3)
	{
		com_mode.baudrate = *(uint32_t*)ptr;
		ptr += 4;
		if (i < 2)
		{
			com_mode.datalength = ptr[0];
			com_mode.paritybit = ptr[1];
			com_mode.stopbit = ptr[2];
			ptr += 3;
			if (i < 1)
			{
				com_mode.handshake = ptr[0];
				com_mode.auxpin = ptr[1];
				ptr += 2;
			}
		}
	}
	return VSFERR_NONE;
}

VSS_HANDLER(comisp_chip)
{
	uint8_t i;
	VSS_CHECK_ARGC(2);
	for (i = 0; i < dimof(comisp_chips_param); i++)
	{
		if (!strcmp(comisp_chips_param[i].chip_name, argv[1]))
		{
			comisp_chip_index = i;
			
			if ((NULL == com_mode.comport) || !strlen(com_mode.comport))
			{
				return VSFERR_FAIL;
			}
			if (com_mode.baudrate <= 0)
			{
				if (comisp_chips_param[i].com_mode.baudrate <= 0)
				{
					com_mode.baudrate = COMM_DEFAULT_BAUDRATE;
				}
				else
				{
					com_mode.baudrate = comisp_chips_param[i].com_mode.baudrate;
				}
			}
			if (com_mode.datalength <= 0)
			{
				if (comisp_chips_param[i].com_mode.datalength <= 0)
				{
					com_mode.datalength = COMM_DEFAULT_DATALENGTH;
				}
				else
				{
					com_mode.datalength =
						comisp_chips_param[i].com_mode.datalength;
				}
			}
			if (COMM_PARAMETER_UNSURE == com_mode.paritybit)
			{
				if (COMM_PARAMETER_UNSURE ==
					comisp_chips_param[i].com_mode.paritybit)
				{
					com_mode.paritybit = COMM_DEFAULT_PARITYBIT;
				}
				else
				{
					com_mode.paritybit =
						comisp_chips_param[i].com_mode.paritybit;
				}
			}
			if (COMM_PARAMETER_UNSURE == com_mode.stopbit)
			{
				if (COMM_PARAMETER_UNSURE ==
					comisp_chips_param[i].com_mode.stopbit)
				{
					com_mode.stopbit = COMM_DEFAULT_STOPBIT;
				}
				else
				{
					com_mode.stopbit = comisp_chips_param[i].com_mode.stopbit;
				}
			}
			if (COMM_PARAMETER_UNSURE == com_mode.handshake)
			{
				if (COMM_PARAMETER_UNSURE ==
					comisp_chips_param[i].com_mode.handshake)
				{
					com_mode.handshake = COMM_DEFAULT_HANDSHAKE;
				}
				else
				{
					com_mode.handshake = comisp_chips_param[i].com_mode.handshake;
				}
			}
			if (COMM_PARAMETER_UNSURE == com_mode.auxpin)
			{
				if (COMM_PARAMETER_UNSURE ==
					comisp_chips_param[i].com_mode.auxpin)
				{
					com_mode.auxpin = COMM_DEFAULT_AUXPIN;
				}
				else
				{
					com_mode.auxpin = comisp_chips_param[i].com_mode.auxpin;
				}
			}
			
			memcpy(&comisp_program_functions,
					comisp_chips_param[i].program_functions,
					sizeof(comisp_program_functions));
			comisp_program_functions.enter_program_mode = \
					ENTER_PROGRAM_MODE_FUNCNAME(comisp);
			comisp_program_functions.leave_program_mode = \
					LEAVE_PROGRAM_MODE_FUNCNAME(comisp);
			return VSFERR_NONE;
		}
	}
	return VSFERR_FAIL;
}

const struct vss_cmd_t comisp_notifier[] =
{
	VSS_CMD(	"chip",
				"select target chip for internal call",
				comisp_chip,
				NULL),
	VSS_CMD(	"E",
				"print com info, format: E",
				comisp_print_cominfo,
				NULL),
	VSS_CMD_END
};

ENTER_PROGRAM_MODE_HANDLER(comisp)
{
	struct program_functions_t *pf =
					comisp_chips_param[comisp_chip_index].program_functions;
	
	if ((NULL == com_mode.comport) || !strlen(com_mode.comport))
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "comport");
		return VSFERR_FAIL;
	}
	// comm init
	if (comm_open(com_mode.comport, com_mode.baudrate, 8,
		com_mode.paritybit, com_mode.stopbit, com_mode.handshake))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPEN, com_mode.comport);
		return ERRCODE_FAILURE_OPEN;
	}
	
	if (pf->enter_program_mode != NULL)
	{
		return pf->enter_program_mode(context);
	}
	return VSFERR_NONE;
}

LEAVE_PROGRAM_MODE_HANDLER(comisp)
{
	struct program_functions_t *pf =
					comisp_chips_param[comisp_chip_index].program_functions;
	vsf_err_t err = VSFERR_NONE;
	
	if (pf->leave_program_mode != NULL)
	{
		err = pf->leave_program_mode(context, success);
	}
	
	comm_close();
	return err;
}
#endif
