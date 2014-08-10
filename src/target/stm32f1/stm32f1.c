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
#if TARGET_STM32F1_EN
#if !TARGET_ARM_ADI_EN && !TARGET_COMISP_EN
#	error TARGET_ARM_ADI_EN or TARGET_COMISP_EN MUST be defined for STM32F1
#endif

#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"
#include "scripts.h"

#include "stm32f1.h"
#include "stm32f1_internal.h"
#include "comisp.h"
#include "cm.h"
#include "adi_v5p1.h"

#define CUR_TARGET_STRING			STM32F1_STRING

struct program_area_map_t stm32f1_program_area_map[] =
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR | AREA_ATTR_RAE | AREA_ATTR_RAW},
	{FUSE_CHAR, 0, 0, 0, 0, AREA_ATTR_EWR | AREA_ATTR_RAW},
	{UNIQUEID_CHAR, 0, 0, 0, 0, AREA_ATTR_R},
	{0, 0, 0, 0, 0, AREA_ATTR_NONE}
};

const struct program_mode_t stm32f1_program_mode[] =
{
	{'j', SET_FREQUENCY, IFS_JTAG_HL},
	{'s', "", IFS_SWD},
	{'i', USE_COMM, 0},
	{0, NULL, 0}
};

struct program_functions_t stm32f1_program_functions;

VSS_HANDLER(stm32f1_help)
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

VSS_HANDLER(stm32f1_mode)
{
	uint8_t mode;
	
	VSS_CHECK_ARGC(2);
	mode = (uint8_t)strtoul(argv[1], NULL,0);
	switch (mode)
	{
#if TARGET_ARM_ADI_EN
	case STM32F1_JTAG:
	case STM32F1_SWD:
		stm32f1_program_area_map[0].attr |= AREA_ATTR_RNP;
		vss_call_notifier(cm_notifier, "chip", "cm_stm32f1");
		memcpy(&stm32f1_program_functions, &cm_program_functions,
				sizeof(stm32f1_program_functions));
		break;
#endif
#if TARGET_COMISP_EN
	case STM32F1_ISP:
		stm32f1_program_area_map[0].attr &= ~AREA_ATTR_RNP;
		vss_call_notifier(comisp_notifier, "chip", "comisp_stm32");
		memcpy(&stm32f1_program_functions, &comisp_program_functions,
				sizeof(stm32f1_program_functions));
		break;
#endif
	default:
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

#if TARGET_COMISP_EN
VSS_HANDLER(stm32f1_extra)
{
	char cmd[2];
	
	VSS_CHECK_ARGC(1);
	cmd[0] = '0' + COMISP_STM32;
	cmd[1] = '\0';
	return vss_call_notifier(comisp_notifier, "E", cmd);
}
#endif

const struct vss_cmd_t stm32f1_notifier[] =
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				stm32f1_help,
				NULL),
	VSS_CMD(	"mode",
				"set programming mode of target for internal call",
				stm32f1_mode,
				NULL),
#if TARGET_COMISP_EN
	VSS_CMD(	"extra",
				"print extra information for internal call",
				stm32f1_extra,
				NULL),
#endif
	VSS_CMD_END
};

uint16_t stm32f1_get_flash_size(uint32_t mcuid, uint32_t flash_sram_reg)
{
	uint16_t den = mcuid & STM32F1_DEN_MSK;
	uint16_t flash_size = flash_sram_reg & 0xFFFF;
	
	if (0xFFFF == flash_size)
	{
		switch (den)
		{
		case STM32F1_DEN_LOW:
			return 32;
		case STM32F1_DEN_MEDIUM:
			return 128;
		case STM32F1_DEN_HIGH:
			return 512;
		case STM32F1_DEN_CONNECTIVITY:
			return 256;
		case STM32F1_DEN_VALUELINE:
			return 128;
		case STM32F1_DEN_XL:
			return 1024;
		default:
			return 1024;
		}
	}
	else
	{
		if (flash_size <= 128)
		{
			flash_size = 128;
		}
		else if (flash_size <= 512)
		{
			flash_size = 512;
		}
		else if (flash_size <= 1024)
		{
			flash_size = 1024;
		}
		else
		{
			flash_size = 0;
		}
		return flash_size;
	}
}

void stm32f1_print_device(uint32_t mcuid)
{
	char rev_char = 0;
	uint16_t den, rev;
	
	den = mcuid & STM32F1_DEN_MSK;
	rev = (mcuid & STM32F1_REV_MSK) >> 16;
	switch (den)
	{
	case STM32F1_DEN_LOW:
		LOG_INFO("STM32F1 type: low-density device");
		switch (rev)
		{
		case 0x1000:
			rev_char = 'A';
			break;
		}
		break;
	case STM32F1_DEN_MEDIUM:
		LOG_INFO("STM32F1 type: medium-density device");
		switch (rev)
		{
		case 0x0000:
			rev_char = 'A';
			break;
		case 0x2000:
			rev_char = 'B';
			break;
		case 0x2001:
			rev_char = 'Z';
			break;
		case 0x2003:
			rev_char = 'Y';
			break;
		}
		break;
	case STM32F1_DEN_HIGH:
		LOG_INFO("STM32F1 type: high-density device");
		switch (rev)
		{
		case 0x1000:
			rev_char = 'A';
			break;
		case 0x1001:
			rev_char = 'Z';
			break;
		case 0x1003:
			rev_char = 'Y';
			break;
		}
		break;
	case STM32F1_DEN_CONNECTIVITY:
		LOG_INFO("STM32F1 type: connectivity device");
		switch (rev)
		{
		case 0x1000:
			rev_char = 'A';
			break;
		case 0x1001:
			rev_char = 'Z';
			break;
		}
		break;
	case STM32F1_DEN_VALUELINE:
		LOG_INFO("STM32F1 type: value-line device");
		switch (rev)
		{
		case 0x1000:
			rev_char = 'A';
			break;
		case 0x1001:
			rev_char = 'Z';
			break;
		}
		break;
	case STM32F1_DEN_XL:
		LOG_INFO("STM32F1 type: XL device");
		switch (rev)
		{
		case 0x1000:
			rev_char = 'A';
			break;
		}
		break;
	default:
		LOG_INFO("STM32F1 type: unknown device(%08X)", mcuid);
		break;
	}
	if (rev_char != 0)
	{
		LOG_INFO("STM32F1 revision: %c", rev_char);
	}
}

#endif
