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
#if TARGET_SD_EN

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
#include "dal/sd/sd_common.h"
#include "dal/sd/sd_spi_drv.h"

#include "sd.h"
#include "sd_internal.h"

#define CUR_TARGET_STRING			SD_STRING

#define SD_SPI_STRING				"sd_spi"
#define SD_SDIO_STRING				"sd_sdio"

struct program_area_map_t sd_program_area_map[] =
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_WR},
	{0, 0, 0, 0, 0, AREA_ATTR_NONE}
};

const struct program_mode_t sd_program_mode[] =
{
	{'s', SET_FREQUENCY, IFS_SPI | IFS_GPIO},
	{'d', SET_FREQUENCY, IFS_SDIO},
	{0, NULL, 0}
};

ENTER_PROGRAM_MODE_HANDLER(sd);
LEAVE_PROGRAM_MODE_HANDLER(sd);
ERASE_TARGET_HANDLER(sd);
WRITE_TARGET_HANDLER(sd);
READ_TARGET_HANDLER(sd);
const struct program_functions_t sd_program_functions =
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(sd),
	LEAVE_PROGRAM_MODE_FUNCNAME(sd),
	ERASE_TARGET_FUNCNAME(sd),
	WRITE_TARGET_FUNCNAME(sd),
	READ_TARGET_FUNCNAME(sd)
};

VSS_HANDLER(sd_help)
{
	VSS_CHECK_ARGC(1);
	PRINTF("Usage of %s:"LOG_LINE_END, CUR_TARGET_STRING);
	PRINTF("  -F,  --frequency <FREQUENCY>              set SPI frequency, in KHz"LOG_LINE_END);
	PRINTF(LOG_LINE_END);
	return VSFERR_NONE;
}

const struct vss_cmd_t sd_notifier[] =
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				sd_help,
				NULL),
	VSS_CMD_END
};






static struct sd_info_t sd_info;
static struct sd_param_t sd_param;
static struct sd_spi_drv_info_t sd_spi_drv_info;
static struct sd_spi_drv_interface_t sd_spi_drv_ifs;
static struct mal_info_t sd_mal_info =
{
	{0, 0}, &sd_info, 0, 0, 0, &sd_spi_drv
};
static struct dal_info_t sd_dal_info =
{
	NULL,
	&sd_param,
	NULL,
	&sd_mal_info,
};

ENTER_PROGRAM_MODE_HANDLER(sd)
{
	struct program_info_t *pi = context->pi;
	struct chip_area_info_t *flash_info = NULL;
	uint64_t capacity;
	uint32_t page_size;
	
	flash_info = target_get_chip_area(context->param, APPLICATION_IDX);
	if (NULL == flash_info)
	{
		return VSFERR_FAIL;
	}
	
	sd_dal_info.ifs = &sd_spi_drv_ifs;
	sd_dal_info.info = &sd_spi_drv_info;
	if (pi->ifs_indexes != NULL)
	{
		if (dal_config_interface(SD_SPI_STRING, pi->ifs_indexes, &sd_dal_info))
		{
			return VSFERR_FAIL;
		}
	}
	else
	{
		sd_spi_drv_ifs.cs_port = 0;
		sd_spi_drv_ifs.cs_pin = GPIO_SRST;
		sd_spi_drv_ifs.spi_port = 0;
	}
	
	sd_param.kHz = pi->frequency;
	if (mal.init(&sd_dal_info))
	{
		return VSFERR_FAIL;
	}
	capacity = sd_mal_info.capacity.block_number *
				sd_mal_info.capacity.block_size;
	LOG_INFO("Card capacity: %d MB", (int)(capacity >> 20));
	switch (sd_info.cardtype)
	{
	case SD_CARDTYPE_SD_V1:
		LOG_INFO("Card type: SD V1");
		break;
	case SD_CARDTYPE_SD_V2:
		LOG_INFO("Card type: SD V2");
		break;
	case SD_CARDTYPE_SD_V2HC:
		LOG_INFO("Card type: SD V2HC");
		break;
	case SD_CARDTYPE_MMC:
		LOG_INFO("Card type: MMC");
		break;
	case SD_CARDTYPE_MMC_HC:
		LOG_INFO("Card type: MMC HC");
		break;
	default:
		LOG_INFO("Card type: UNKNOWN");
		break;
	}
	
	page_size = flash_info->page_size = 4 * 1024;
	if (capacity > (16 << 20))
	{
		flash_info->page_num = (uint32_t)((16 << 20) / page_size);
	}
	else
	{
		flash_info->page_num = (uint32_t)(capacity / page_size);
	}
	flash_info->size = flash_info->page_num * flash_info->page_size;
	
	return dal_commit();
}

LEAVE_PROGRAM_MODE_HANDLER(sd)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	mal.fini(&sd_dal_info);
	return dal_commit();
}

ERASE_TARGET_HANDLER(sd)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(area);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(size);
	
	if (mal.eraseall(&sd_dal_info))
	{
		return VSFERR_FAIL;
	}
	return dal_commit();
}

WRITE_TARGET_HANDLER(sd)
{
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		if (size % 512)
		{
			return VSFERR_FAIL;
		}
		size /= 512;
		
		if (mal.writeblock(&sd_dal_info, addr, buff, size))
		{
			return VSFERR_FAIL;
		}
		return dal_commit();
		break;
	default:
		return VSFERR_FAIL;
	}
}

READ_TARGET_HANDLER(sd)
{
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case CHIPID_CHAR:
		memset(buff, 0, size);
		return VSFERR_NONE;
		break;
	case APPLICATION_CHAR:
		if (size % 512)
		{
			return VSFERR_FAIL;
		}
		size /= 512;
		
		if (mal.readblock(&sd_dal_info, addr, buff, size))
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
