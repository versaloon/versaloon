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
#if TARGET_KINETIS_EN
#if !TARGET_ARM_ADI_EN
#	error TARGET_ARM_ADI_EN MUST be defined for KINETIS
#endif

#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"
#include "scripts.h"

#include "kinetis.h"
#include "kinetis_internal.h"
#include "comisp.h"
#include "cm.h"
#include "adi_v5p1.h"

#define CUR_TARGET_STRING			KINETIS_STRING

struct program_area_map_t kinetis_program_area_map[] =
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR | AREA_ATTR_RAE | AREA_ATTR_RAW},
	{FUSE_CHAR, 0, 0, 0, 0, AREA_ATTR_EWR | AREA_ATTR_RAW},
	{UNIQUEID_CHAR, 0, 0, 0, 0, AREA_ATTR_R},
	{0, 0, 0, 0, 0, AREA_ATTR_NONE}
};

const struct program_mode_t kinetis_program_mode[] =
{
	{'j', SET_FREQUENCY, IFS_JTAG_HL},
	{'s', "", IFS_SWD},
	{'i', USE_COMM, 0},
	{0, NULL, 0}
};

struct program_functions_t kinetis_program_functions;

VSS_HANDLER(kinetis_help)
{
	VSS_CHECK_ARGC(1);
	PRINTF("Usage of %s:"LOG_LINE_END, CUR_TARGET_STRING);
	PRINTF("  -C,  --comport <COMM_ATTRIBUTE>           set com port"LOG_LINE_END);
	PRINTF("  -m,  --mode <MODE>                        set mode<j|s|i>"LOG_LINE_END);
	PRINTF("  -x,  --execute <ADDRESS>                  execute program"LOG_LINE_END);
	PRINTF("  -F,  --frequency <FREQUENCY>              set JTAG frequency, in KHz"LOG_LINE_END);
	PRINTF(LOG_LINE_END);
	return VSFERR_NONE;
}

VSS_HANDLER(kinetis_mode)
{
	uint8_t mode;
	
	VSS_CHECK_ARGC(2);
	mode = (uint8_t)strtoul(argv[1], NULL,0);
	switch (mode)
	{
#if TARGET_ARM_ADI_EN
	case KINETIS_JTAG:
	case KINETIS_SWD:
		vss_call_notifier(cm_notifier, "chip", "cm_kinetis");
		memcpy(&kinetis_program_functions, &cm_program_functions,
				sizeof(kinetis_program_functions));
		break;
#endif
	default:
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

const struct vss_cmd_t kinetis_notifier[] =
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				kinetis_help,
				NULL),
	VSS_CMD(	"mode",
				"set programming mode of target for internal call",
				kinetis_mode,
				NULL),
	VSS_CMD_END
};

uint32_t kinetis_get_sram_size(uint32_t mcuid)
{
	uint32_t sram_size = 0;
	
	if (mcuid & 0xFFFF0000)
	{
		// CortexM0P series
		mcuid = (mcuid >> 16) & 0x0F;
		sram_size = 512;
		while (mcuid--)
		{
			sram_size <<= 1;
		}
	}
	else
	{
		// CortexM4
		// not valid
	}
	return sram_size;
}

const uint32_t KINETIS_PFSIZE[] =
{
	8    * 1024,		// 0x0
	16   * 1024,		// 0x1
	0    * 1024,		// 0x2
	32   * 1024,		// 0x3
	0    * 1024,		// 0x4
	64   * 1024,		// 0x5
	0    * 1024,		// 0x6
	128  * 1024,		// 0x7
	0    * 1024,		// 0x8
	256  * 1024,		// 0x9
	0    * 1024,		// 0xA
	0    * 1024,		// 0xB
	0    * 1024,		// 0xC
	0    * 1024,		// 0xD
	0    * 1024,		// 0xE
	128  * 1024,		// 0xF
};

uint32_t kinetis_get_flash_size(uint32_t fcfg1)
{
	fcfg1 = (fcfg1 >> 24) & 0x0F;
	return KINETIS_PFSIZE[fcfg1];
}

const uint16_t KINETIS_PINID[] =
{
	16,		// 0x0
	24,		// 0x1
	32,		// 0x2
	0,		// 0x3
	48,		// 0x4
	64,		// 0x5
	80,		// 0x6
	81,		// 0x7
	100,	// 0x8
	0,		// 0x9
	0,		// 0xA
	0,		// 0xB
	0,		// 0xC
	0,		// 0xD
	0,		// 0xE
	0,		// 0xF
};

void kinetis_print_device(uint32_t mcuid, uint32_t fcfg1)
{
	uint8_t FAMID, SUBFAMID, REVID, DIEID, PINID;
	uint16_t pin_number;
	char *series = "unknown";
	uint32_t sram_size = 0;
	uint32_t flash_size = 0;
	
	if (mcuid & 0xFFFF0000)
	{
		// CortexM0P series
		FAMID = (mcuid >> 28) & 0x0F;
		SUBFAMID = (mcuid >> 24) & 0x0F;
		REVID = (mcuid >> 12) & 0x0F;
		DIEID = (mcuid >> 7) & 0x1F;
		PINID = (mcuid >> 0) & 0x0F;
		
		series = "KL";
		LOG_INFO("KINETIS series: %s family", series);
		LOG_INFO("KINETIS chip: %s%01d%01d", series, FAMID, SUBFAMID);
		LOG_INFO("KINETIS device die number: %02X", DIEID);
	}
	else
	{
		// CortexM4 series
		REVID = (mcuid >> 12) & 0x0F;
		PINID = (mcuid >> 0) & 0x0F;
		
		series = "K";
		LOG_INFO("KINETIS series: %s family", series);
	}
	
	// Common feature
	LOG_INFO("KINETIS device revision number: %02X", REVID);
	sram_size = kinetis_get_sram_size(mcuid);
	flash_size = kinetis_get_flash_size(fcfg1);
	if (sram_size)
	{
		LOG_INFO("KINETIS sram size: %d", sram_size);
	}
	if (flash_size)
	{
		LOG_INFO("KINETIS flash size: %dK", flash_size / 1024);
	}
	pin_number = KINETIS_PINID[PINID];
	if (pin_number)
	{
		LOG_INFO("KINETIS pin number: %d", pin_number);
	}
	else
	{
		LOG_INFO("KINETIS pin number: unknown");
	}
}

#endif
