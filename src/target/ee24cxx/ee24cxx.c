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
#if TARGET_EE24CXX_EN

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
#include "dal/ee24cxx/ee24cxx_drv.h"

#include "ee24cxx.h"
#include "ee24cxx_internal.h"

#define CUR_TARGET_STRING			EE24CXX_STRING

struct program_area_map_t ee24cxx_program_area_map[] =
{
	{EEPROM_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR | AREA_ATTR_EP},
	{0, 0, 0, 0, 0, AREA_ATTR_NONE}
};

const struct program_mode_t ee24cxx_program_mode[] =
{
	{'*', SET_FREQUENCY, IFS_I2C},
	{0, NULL, 0}
};

ENTER_PROGRAM_MODE_HANDLER(ee24cxx);
LEAVE_PROGRAM_MODE_HANDLER(ee24cxx);
ERASE_TARGET_HANDLER(ee24cxx);
WRITE_TARGET_HANDLER(ee24cxx);
READ_TARGET_HANDLER(ee24cxx);
const struct program_functions_t ee24cxx_program_functions =
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(ee24cxx),
	LEAVE_PROGRAM_MODE_FUNCNAME(ee24cxx),
	ERASE_TARGET_FUNCNAME(ee24cxx),
	WRITE_TARGET_FUNCNAME(ee24cxx),
	READ_TARGET_FUNCNAME(ee24cxx)
};

VSS_HANDLER(ee24cxx_help)
{
	VSS_CHECK_ARGC(1);
	PRINTF("Usage of %s:"LOG_LINE_END, CUR_TARGET_STRING);
	PRINTF("  -F,  --frequency <FREQUENCY>              set IIC frequency, in KHz"LOG_LINE_END);
	PRINTF(LOG_LINE_END);
	return VSFERR_NONE;
}

const struct vss_cmd_t ee24cxx_notifier[] =
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				ee24cxx_help,
				NULL),
	VSS_CMD_END
};






static struct ee24cxx_drv_param_t ee24cxx_drv_param;
static struct ee24cxx_drv_interface_t ee24cxx_drv_ifs;
static struct mal_info_t ee24cxx_mal_info =
{
	{0, 0}, NULL, 0, 0, 0, &ee24cxx_drv
};
static struct dal_info_t ee24cxx_dal_info =
{
	&ee24cxx_drv_ifs,
	&ee24cxx_drv_param,
	NULL,
	&ee24cxx_mal_info,
};

ENTER_PROGRAM_MODE_HANDLER(ee24cxx)
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
		if (dal_config_interface(EE24CXX_STRING, pi->ifs_indexes,
												&ee24cxx_dal_info))
		{
			return VSFERR_FAIL;
		}
	}
	else
	{
		ee24cxx_drv_ifs.iic_port = 0;
	}
	
	if (pi->chip_address)
	{
		ee24cxx_drv_param.iic_addr = (uint8_t)pi->chip_address;
	}
	else
	{
		ee24cxx_drv_param.iic_addr = (uint8_t)param->param[EE24CXX_PARAM_BASE_ADDR];
	}
	ee24cxx_drv_param.iic_khz = pi->frequency;
	if (mal.init(&ee24cxx_dal_info))
	{
		return VSFERR_FAIL;
	}
	ee24cxx_mal_info.capacity.block_size = eeprom_info->page_size;
	ee24cxx_mal_info.capacity.block_number = eeprom_info->page_num;
	
	return dal_commit();
}

LEAVE_PROGRAM_MODE_HANDLER(ee24cxx)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	mal.fini(&ee24cxx_dal_info);
	return dal_commit();
}

ERASE_TARGET_HANDLER(ee24cxx)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(area);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(size);
	
	// no need to erase
	return VSFERR_NONE;
}

WRITE_TARGET_HANDLER(ee24cxx)
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
		
		if (mal.writeblock(&ee24cxx_dal_info, addr, buff, size))
		{
			return VSFERR_FAIL;
		}
		return dal_commit();
		break;
	default:
		return VSFERR_FAIL;
	}
}

READ_TARGET_HANDLER(ee24cxx)
{
	struct chip_area_info_t *eeprom_info = NULL;
	
	switch (area)
	{
	case CHIPID_CHAR:
		return VSFERR_NONE;
		break;
	case EEPROM_CHAR:
		eeprom_info = target_get_chip_area(context->param, EEPROM_IDX);
		if ((NULL == eeprom_info) || (size % eeprom_info->page_size))
		{
			return VSFERR_FAIL;
		}
		size /= eeprom_info->page_size;
		
		if (mal.readblock(&ee24cxx_dal_info, addr, buff, size))
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
