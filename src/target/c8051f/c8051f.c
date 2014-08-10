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
#if TARGET_C8051F_EN

#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"
#include "scripts.h"

#include "c8051f.h"
#include "c8051f_internal.h"

#define CUR_TARGET_STRING			C8051F_STRING

const struct program_area_map_t c8051f_program_area_map[] =
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR},
	{0, 0, 0, 0, 0, AREA_ATTR_NONE}
};

const struct program_mode_t c8051f_program_mode[] =
{
	{'j', SET_FREQUENCY, IFS_JTAG_HL},
	{'c', SET_FREQUENCY, IFS_C2 | IFS_GPIO},
	{0, NULL, 0}
};

struct program_functions_t c8051f_program_functions;

VSS_HANDLER(c8051f_help)
{
	VSS_CHECK_ARGC(1);
	PRINTF("Usage of %s:"LOG_LINE_END, CUR_TARGET_STRING);
	PRINTF("  -m,  --mode <MODE>                        set mode<j|c>"LOG_LINE_END);
	PRINTF(LOG_LINE_END);
	return VSFERR_NONE;
}

VSS_HANDLER(c8051f_mode)
{
	uint8_t mode;
	
	VSS_CHECK_ARGC(2);
	mode = (uint8_t)strtoul(argv[1], NULL,0);
	switch (mode)
	{
	case C8051F_JTAG:
		memcpy(&c8051f_program_functions, &c8051fjtag_program_functions,
				sizeof(c8051f_program_functions));
		break;
	case C8051F_C2:
		memcpy(&c8051f_program_functions, &c8051fc2_program_functions,
				sizeof(c8051f_program_functions));
		break;
	}
	return VSFERR_NONE;
}

const struct vss_cmd_t c8051f_notifier[] =
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				c8051f_help,
				NULL),
	VSS_CMD(	"mode",
				"set programming mode of target for internal call",
				c8051f_mode,
				NULL),
	VSS_CMD_END
};

#endif
