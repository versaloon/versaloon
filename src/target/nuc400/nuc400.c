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
#if TARGET_NUC400_EN

#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"
#include "scripts.h"

#include "nuc400.h"
#include "nuc400_internal.h"
#include "cm.h"
#include "adi_v5p1.h"

#define CUR_TARGET_STRING			NUC400_STRING

struct program_area_map_t nuc400_program_area_map[] =
{
//	{BOOTLOADER_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR | AREA_ATTR_EP},
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR | AREA_ATTR_EP | AREA_ATTR_RAW},
	{FUSE_CHAR, 0, 0, 0, 0, AREA_ATTR_EWR},
	{UNIQUEID_CHAR, 0, 0, 0, 0, AREA_ATTR_R},
	{USRSIG_CHAR, 0, 0, 0, 0, AREA_ATTR_EWR},
	{0, 0, 0, 0, 0, AREA_ATTR_NONE}
};

const struct program_mode_t nuc400_program_mode[] =
{
	{'s', "", IFS_SWD},
	{0, NULL, 0}
};

struct program_functions_t nuc400_program_functions;

VSS_HANDLER(nuc400_help)
{
	VSS_CHECK_ARGC(1);
	PRINTF("Usage of %s:"LOG_LINE_END, CUR_TARGET_STRING);
	PRINTF("  -m,  --mode <MODE>                        set mode<j|s|i>"LOG_LINE_END);
	PRINTF("  -x,  --execute <ADDRESS>                  execute program"LOG_LINE_END);
	PRINTF("  -F,  --frequency <FREQUENCY>              set JTAG frequency, in KHz"LOG_LINE_END);
	PRINTF(LOG_LINE_END);
	return VSFERR_NONE;
}

VSS_HANDLER(nuc400_mode)
{
	uint8_t mode;
	
	VSS_CHECK_ARGC(2);
	mode = (uint8_t)strtoul(argv[1], NULL,0);
	switch (mode)
	{
	case NUC400_SWD:
		nuc400_program_area_map[0].attr |= AREA_ATTR_RNP;
		vss_call_notifier(cm_notifier, "chip", "cm_nuc400");
		memcpy(&nuc400_program_functions, &cm_program_functions,
				sizeof(nuc400_program_functions));
		break;
	}
	return VSFERR_NONE;
}

const struct vss_cmd_t nuc400_notifier[] =
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				nuc400_help,
				NULL),
	VSS_CMD(	"mode",
				"set programming mode of target for internal call",
				nuc400_mode,
				NULL),
	VSS_CMD_END
};

#endif
