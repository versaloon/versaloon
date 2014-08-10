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
#if TARGET_EE93CX6_EN

#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"
#include "scripts.h"
#include "app_scripts.h"

#include "dal/dal.h"
#include "dal/mal/mal.h"
#include "dal/ee93cx6/ee93cx6_drv.h"

#include "ee93cx6.h"
#include "ee93cx6_internal.h"

#define CUR_TARGET_STRING			EE93CX6_STRING

struct program_area_map_t ee93cx6_program_area_map[] =
{
	{EEPROM_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR | AREA_ATTR_EP},
	{0, 0, 0, 0, 0, AREA_ATTR_NONE}
};

const struct program_mode_t ee93cx6_program_mode[] =
{
	{'b', SET_FREQUENCY, IFS_MICROWIRE},
	{'w', SET_FREQUENCY, IFS_MICROWIRE},
	{0, NULL, 0}
};

ENTER_PROGRAM_MODE_HANDLER(ee93cx6);
LEAVE_PROGRAM_MODE_HANDLER(ee93cx6);
ERASE_TARGET_HANDLER(ee93cx6);
WRITE_TARGET_HANDLER(ee93cx6);
READ_TARGET_HANDLER(ee93cx6);
const struct program_functions_t ee93cx6_program_functions =
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(ee93cx6),
	LEAVE_PROGRAM_MODE_FUNCNAME(ee93cx6),
	ERASE_TARGET_FUNCNAME(ee93cx6),
	WRITE_TARGET_FUNCNAME(ee93cx6),
	READ_TARGET_FUNCNAME(ee93cx6)
};

static uint8_t ee93cx6_origination_mode;

VSS_HANDLER(ee93cx6_help)
{
	VSS_CHECK_ARGC(1);
	PRINTF("Usage of %s:"LOG_LINE_END, CUR_TARGET_STRING);
	PRINTF("  -F,  --frequency <FREQUENCY>              set MicroWire frequency, in KHz"LOG_LINE_END);
	PRINTF("  -m,  --mode <MODE>                        set mode<b|w>"LOG_LINE_END);
	PRINTF(LOG_LINE_END);
	return VSFERR_NONE;
}

VSS_HANDLER(ee93cx6_mode)
{
	uint8_t mode;
	
	VSS_CHECK_ARGC(2);
	mode = (uint8_t)strtoul(argv[1], NULL,0);
	switch (mode)
	{
	case EE93CX6_MODE_BYTE:
	case EE93CX6_MODE_WORD:
		ee93cx6_origination_mode = mode;
		break;
	default:
		return VSFERR_FAIL;
		break;
	}
	return VSFERR_NONE;
}

const struct vss_cmd_t ee93cx6_notifier[] =
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				ee93cx6_help,
				NULL),
	VSS_CMD(	"mode",
				"set programming mode of target for internal call",
				ee93cx6_mode,
				NULL),
	VSS_CMD_END
};






static struct ee93cx6_drv_param_t ee93cx6_drv_param;
static struct ee93cx6_drv_interface_t ee93cx6_drv_ifs;
static struct mal_info_t ee93cx6_mal_info =
{
	{0, 0}, NULL, 0, 0, 0, &ee93cx6_drv
};
static struct dal_info_t ee93cx6_dal_info =
{
	&ee93cx6_drv_ifs,
	&ee93cx6_drv_param,
	NULL,
	&ee93cx6_mal_info,
};

ENTER_PROGRAM_MODE_HANDLER(ee93cx6)
{
	struct chip_param_t *param = context->param;
	struct program_info_t *pi = context->pi;
	struct chip_area_info_t *eeprom_info = NULL;
	
	eeprom_info = target_get_chip_area(param, EEPROM_IDX);
	if (NULL == eeprom_info)
	{
		return VSFERR_FAIL;
	}
	
	if (pi->ifs_indexes != NULL)
	{
		if (dal_config_interface(EE93CX6_STRING, pi->ifs_indexes,
												&ee93cx6_dal_info))
		{
			return VSFERR_FAIL;
		}
	}
	else
	{
		ee93cx6_drv_ifs.mw_port = 0;
	}
	
	ee93cx6_drv_param.addr_bitlen =
							(uint8_t)param->param[EE93CX6_PARAM_ADDR_BITLEN];
	ee93cx6_drv_param.cmd_bitlen =
							(uint8_t)param->param[EE93CX6_PARAM_OPCODE_BITLEN];
	ee93cx6_drv_param.iic_khz = pi->frequency;
	if (EE93CX6_MODE_BYTE == ee93cx6_origination_mode)
	{
		ee93cx6_drv_param.origination_mode = EE93CX6_ORIGINATION_BYTE;
	}
	else
	{
		ee93cx6_drv_param.origination_mode = EE93CX6_ORIGINATION_WORD;
	}
	if (mal.init(&ee93cx6_dal_info))
	{
		return VSFERR_FAIL;
	}
	ee93cx6_mal_info.capacity.block_size = eeprom_info->page_size;
	ee93cx6_mal_info.capacity.block_number = eeprom_info->page_num;
	
	return dal_commit();
}

LEAVE_PROGRAM_MODE_HANDLER(ee93cx6)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	mal.fini(&ee93cx6_dal_info);
	return dal_commit();
}

ERASE_TARGET_HANDLER(ee93cx6)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(area);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(size);
	
	if (mal.eraseall(&ee93cx6_dal_info))
	{
		return VSFERR_FAIL;
	}
	return dal_commit();
}

WRITE_TARGET_HANDLER(ee93cx6)
{
	struct chip_area_info_t *eeprom_info = NULL;
	
	switch (area)
	{
	case EEPROM_CHAR:
		eeprom_info = target_get_chip_area(context->param, EEPROM_IDX);
		if ((NULL == eeprom_info) || (size % eeprom_info->page_size))
		{
			return VSFERR_FAIL;
		}
		size /= eeprom_info->page_size;
		
		if (mal.writeblock(&ee93cx6_dal_info, addr, buff, size))
		{
			return VSFERR_FAIL;
		}
		return dal_commit();
		break;
	default:
		return VSFERR_FAIL;
	}
}

READ_TARGET_HANDLER(ee93cx6)
{
	struct chip_area_info_t *eeprom_info = NULL;
	
	switch (area)
	{
	case EEPROM_CHAR:
		eeprom_info = target_get_chip_area(context->param, EEPROM_IDX);
		if ((NULL == eeprom_info) || (size % eeprom_info->page_size))
		{
			return VSFERR_FAIL;
		}
		size /= eeprom_info->page_size;
		
		if (mal.readblock(&ee93cx6_dal_info, addr, buff, size))
		{
			return VSFERR_FAIL;
		}
		return dal_commit();
		break;
	default:
		return VSFERR_FAIL;
	}
}

#endif
