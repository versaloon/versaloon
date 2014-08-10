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
#if TARGET_DF25XX_EN

#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"
#include "scripts.h"
#include "app_scripts.h"

#include "dal/mal/mal.h"
#include "dal/df25xx/df25xx_drv.h"

#include "df25xx.h"
#include "df25xx_internal.h"

#define CUR_TARGET_STRING			DF25XX_STRING

struct program_area_map_t df25xx_program_area_map[] =
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR},
	{0, 0, 0, 0, 0, AREA_ATTR_NONE}
};

const struct program_mode_t df25xx_program_mode[] =
{
	{'*', SET_FREQUENCY, IFS_SPI | IFS_GPIO},
	{0, NULL, 0}
};

ENTER_PROGRAM_MODE_HANDLER(df25xx);
LEAVE_PROGRAM_MODE_HANDLER(df25xx);
ERASE_TARGET_HANDLER(df25xx);
WRITE_TARGET_HANDLER(df25xx);
READ_TARGET_HANDLER(df25xx);
const struct program_functions_t df25xx_program_functions =
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(df25xx),
	LEAVE_PROGRAM_MODE_FUNCNAME(df25xx),
	ERASE_TARGET_FUNCNAME(df25xx),
	WRITE_TARGET_FUNCNAME(df25xx),
	READ_TARGET_FUNCNAME(df25xx)
};

VSS_HANDLER(df25xx_help)
{
	VSS_CHECK_ARGC(1);
	PRINTF("Usage of %s:"LOG_LINE_END, CUR_TARGET_STRING);
	PRINTF("  -F,  --frequency <FREQUENCY>              set SPI frequency, in KHz"LOG_LINE_END);
	PRINTF(LOG_LINE_END);
	return VSFERR_NONE;
}

const struct vss_cmd_t df25xx_notifier[] =
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				df25xx_help,
				NULL),
	VSS_CMD_END
};






static struct df25xx_drv_info_t df25xx_drv_info;
static struct df25xx_drv_param_t df25xx_drv_param;
static struct df25xx_drv_interface_t df25xx_drv_ifs;
static struct mal_info_t df25xx_mal_info =
{
	{0, 0}, NULL, 0, 0, 0, &df25xx_drv
};
static struct dal_info_t df25xx_dal_info =
{
	&df25xx_drv_ifs,
	&df25xx_drv_param,
	&df25xx_drv_info,
	&df25xx_mal_info,
};

ENTER_PROGRAM_MODE_HANDLER(df25xx)
{
	struct program_info_t *pi = context->pi;
	struct chip_area_info_t *flash_info = NULL;
	
	flash_info = target_get_chip_area(context->param, APPLICATION_IDX);
	if (NULL == flash_info)
	{
		return VSFERR_FAIL;
	}
	
	if (pi->ifs_indexes != NULL)
	{
		if (dal_config_interface(DF25XX_STRING, pi->ifs_indexes,
									&df25xx_dal_info))
		{
			return VSFERR_FAIL;
		}
	}
	else
	{
		df25xx_drv_ifs.cs_port = 0;
		df25xx_drv_ifs.cs_pin = GPIO_SRST;
		df25xx_drv_ifs.spi_port = 0;
	}
	
	df25xx_drv_param.spi_khz = pi->frequency;
	df25xx_mal_info.capacity.block_number = flash_info->page_num;
	df25xx_mal_info.capacity.block_size = flash_info->page_size;
	if (mal.init(&df25xx_dal_info))
	{
		return VSFERR_FAIL;
	}
	
	return dal_commit();
}

LEAVE_PROGRAM_MODE_HANDLER(df25xx)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	mal.fini(&df25xx_dal_info);
	return dal_commit();
}

ERASE_TARGET_HANDLER(df25xx)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(area);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(size);
	
	if (mal.eraseall(&df25xx_dal_info))
	{
		return VSFERR_FAIL;
	}
	return dal_commit();
}

WRITE_TARGET_HANDLER(df25xx)
{
	struct chip_area_info_t *flash_info = NULL;
	
	switch (area)
	{
	case APPLICATION_CHAR:
		flash_info = target_get_chip_area(context->param, APPLICATION_IDX);
		if ((NULL == flash_info) || (size % flash_info->page_size))
		{
			return VSFERR_FAIL;
		}
		size /= flash_info->page_size;
		
		if (mal.writeblock(&df25xx_dal_info, addr, buff, size))
		{
			return VSFERR_FAIL;
		}
		return dal_commit();
		break;
	default:
		return VSFERR_FAIL;
	}
}

READ_TARGET_HANDLER(df25xx)
{
	struct chip_area_info_t *flash_info = NULL;
	
	switch (area)
	{
	case CHIPID_CHAR:
		if (mal.getinfo(&df25xx_dal_info))
		{
			return VSFERR_FAIL;
		}
		SET_LE_U16(&buff[0], df25xx_drv_info.device_id);
		buff[2] = df25xx_drv_info.manufacturer_id;
		return VSFERR_NONE;
		break;
	case APPLICATION_CHAR:
		flash_info = target_get_chip_area(context->param, APPLICATION_IDX);
		if ((NULL == flash_info) || (size % flash_info->page_size))
		{
			return VSFERR_FAIL;
		}
		size /= flash_info->page_size;
		
		if (mal.readblock(&df25xx_dal_info, addr, buff, size))
		{
			return VSFERR_FAIL;
		}
		return VSFERR_NONE;
		break;
	default:
		return VSFERR_FAIL;
	}
}

#endif
