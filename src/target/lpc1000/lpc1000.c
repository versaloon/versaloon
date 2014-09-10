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
#if TARGET_LPC1000_EN
#if !TARGET_ARM_ADI_EN && !TARGET_COMISP_EN
#	error TARGET_ARM_ADI_EN or TARGET_COMISP_EN MUST be defined for LPC1000
#endif

#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"
#include "scripts.h"

#include "lpc1000.h"
#include "lpc1000_internal.h"
#include "comisp.h"
#include "cm.h"
#include "adi_v5p1.h"

#define CUR_TARGET_STRING			LPC1000_STRING

struct program_area_map_t lpc1000_program_area_map[] =
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR | AREA_ATTR_RAE},
	{UNIQUEID_CHAR, 0, 0, 0, 0, AREA_ATTR_R},
	{0, 0, 0, 0, 0, AREA_ATTR_NONE}
};

const struct program_mode_t lpc1000_program_mode[] =
{
	{'j', SET_FREQUENCY, IFS_JTAG_HL},
	{'s', "", IFS_SWD},
	{'i', USE_COMM, 0},
	{0, NULL, 0}
};

vsf_err_t (*lpc1000_enter_program_mode_save)(struct program_context_t *context);
ENTER_PROGRAM_MODE_HANDLER(lpc1000);
struct program_functions_t lpc1000_program_functions;

VSS_HANDLER(lpc1000_help)
{
	VSS_CHECK_ARGC(1);
	PRINTF("Usage of %s:"LOG_LINE_END, CUR_TARGET_STRING);
	PRINTF("  -C,  --comport <COMM_ATTRIBUTE>           set com port"LOG_LINE_END);
	PRINTF("  -m,  --mode <MODE>                        set mode<j|s|i>"LOG_LINE_END);
	PRINTF("  -x,  --execute <ADDRESS>                  execute program"LOG_LINE_END);
	PRINTF("  -F,  --frequency <FREQUENCY>              set JTAG frequency, in KHz"LOG_LINE_END);
	PRINTF("  -A,  --auto-adjust                        add checksum for first 7 vectors"LOG_LINE_END);
	PRINTF(LOG_LINE_END);
	return VSFERR_NONE;
}

VSS_HANDLER(lpc1000_mode)
{
	uint8_t mode;
	
	VSS_CHECK_ARGC(2);
	mode = (uint8_t)strtoul(argv[1], NULL,0);
	switch (mode)
	{
#if TARGET_ARM_ADI_EN
	case LPC1000_JTAG:
	case LPC1000_SWD:
		vss_call_notifier(cm_notifier, "chip", "cm_lpc1000");
		memcpy(&lpc1000_program_functions, &cm_program_functions,
				sizeof(lpc1000_program_functions));
		lpc1000_enter_program_mode_save =
								cm_program_functions.enter_program_mode;
		break;
#endif
#if TARGET_COMISP_EN
	case LPC1000_ISP:
		vss_call_notifier(comisp_notifier, "chip", "comisp_lpcarm");
		memcpy(&lpc1000_program_functions, &comisp_program_functions,
				sizeof(lpc1000_program_functions));
		lpc1000_enter_program_mode_save =
								comisp_program_functions.enter_program_mode;
		break;
#endif
	default:
		return VSFERR_FAIL;
	}
	lpc1000_program_functions.enter_program_mode =
								ENTER_PROGRAM_MODE_FUNCNAME(lpc1000);
	return VSFERR_NONE;
}

#if TARGET_COMISP_EN
VSS_HANDLER(lpc1000_extra)
{
	char cmd[2];
	
	VSS_CHECK_ARGC(1);
	cmd[0] = '0' + COMISP_LPCARM;
	cmd[1] = '\0';
	return vss_call_notifier(comisp_notifier, "E", cmd);
}
#endif

const struct vss_cmd_t lpc1000_notifier[] =
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				lpc1000_help,
				NULL),
	VSS_CMD(	"mode",
				"set programming mode of target for internal call",
				lpc1000_mode,
				NULL),
#if TARGET_COMISP_EN
	VSS_CMD(	"extra",
				"print extra information for internal call",
				lpc1000_extra,
				NULL),
#endif
	VSS_CMD_END
};

ADJUST_SETTING_HANDLER(lpc1000)
{
	struct chip_area_info_t *flash_info = NULL, *sram_info = NULL;
	
	flash_info = target_get_chip_area(param, APPLICATION_IDX);
	sram_info = target_get_chip_area(param, RAM_IDX);
	if ((NULL == flash_info) || (NULL == sram_info))
	{
		return VSFERR_FAIL;
	}
	
	if (!pi->kernel_khz)
	{
		pi->kernel_khz = param->param[LPC1000_PARAM_IRC_KHZ];
	}
	
	switch (program_mode)
	{
	case LPC1000_JTAG:
	case LPC1000_SWD:
		lpc1000_program_area_map[0].attr |= AREA_ATTR_RNP;
		
		if((param->chip_id & 0xFFFFFF00) == 0x00008100)
		{
			flash_info->page_size = 64;
		}
		else
		{
			if (sram_info->size >= 9 * 1024)
			{
				flash_info->page_size = 4 * 1024;
			}
			else if (sram_info->size >= 3 * 1024)
			{
				flash_info->page_size = 1 * 1024;
			}
			else
			{
				flash_info->page_size = 512;
			}
		}
		break;
	case LPC1000_ISP:
		lpc1000_program_area_map[0].attr &= ~AREA_ATTR_NP;
		if (sram_info->size >= 4 * 1024 + 0x200 + 32)
		{
			flash_info->page_size =  4 * 1024;
		}
		else if (sram_info->size >= 1 * 1024 + 0x200 + 32)
		{
			flash_info->page_size = 1 * 1024;
		}
		else if (sram_info->size >= 512 + 0x200 + 32)
		{
			flash_info->page_size = 512;
		}
		else
		{
			flash_info->page_size = 256;
		}
		break;
	}
	flash_info->page_num = flash_info->size / flash_info->page_size;
	
	return VSFERR_NONE;
}

uint8_t lpc1000_get_sector_idx_by_addr(struct program_context_t *context,
										uint32_t addr)
{
	struct chip_param_t *param = context->param;
	
	REFERENCE_PARAMETER(context);
	
	if((param->chip_id & 0xFFFFFF00) == 0x00008100)
	{
		return (uint8_t)(addr / (1 * 1024));
	}
	else
	{
		if (addr < (4 * 1024 * 16))
		{
			return (uint8_t)(addr / (4 * 1024));
		}
		else if (addr < (512 * 1024))
		{
			return (uint8_t)(16 + (addr - (4 * 1024 * 16)) / (32 * 1024));
		}
		else
		{
			return 0;
		}
	}
}



ENTER_PROGRAM_MODE_HANDLER(lpc1000)
{
	if (NULL == lpc1000_enter_program_mode_save)
	{
		LOG_BUG(ERRMSG_NOT_INITIALIZED, "lpc1000", "");
		return VSFERR_FAIL;
	}
	
	return lpc1000_enter_program_mode_save(context);
}

#endif
