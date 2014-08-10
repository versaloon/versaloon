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
#if TARGET_NAND_EN

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
#include "dal/nand/nand_drv.h"

#include "nand.h"
#include "nand_internal.h"

#define CUR_TARGET_STRING			NAND_STRING

struct program_area_map_t nand_program_area_map[] =
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR | AREA_ATTR_EP},
	{0, 0, 0, 0, 0, AREA_ATTR_NONE}
};

const struct program_mode_t nand_program_mode[] =
{
	{'*', "", IFS_EBI},
	{0, NULL, 0}
};

ENTER_PROGRAM_MODE_HANDLER(nand);
LEAVE_PROGRAM_MODE_HANDLER(nand);
ERASE_TARGET_HANDLER(nand);
WRITE_TARGET_HANDLER(nand);
READ_TARGET_HANDLER(nand);
const struct program_functions_t nand_program_functions =
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(nand),
	LEAVE_PROGRAM_MODE_FUNCNAME(nand),
	ERASE_TARGET_FUNCNAME(nand),
	WRITE_TARGET_FUNCNAME(nand),
	READ_TARGET_FUNCNAME(nand)
};

VSS_HANDLER(nand_help)
{
	VSS_CHECK_ARGC(1);
	PRINTF("Usage of %s:"LOG_LINE_END, CUR_TARGET_STRING);
	PRINTF(LOG_LINE_END);
	return VSFERR_NONE;
}

const struct vss_cmd_t nand_notifier[] =
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				nand_help,
				NULL),
	VSS_CMD_END
};






static struct nand_drv_info_t nand_drv_info;
static struct nand_drv_param_t nand_drv_param;
static struct nand_drv_interface_t nand_drv_ifs;
static struct mal_info_t nand_mal_info =
{
	{0, 0}, NULL, 0, 0, 0, &nand_drv
};
static struct dal_info_t nand_dal_info =
{
	&nand_drv_ifs,
	&nand_drv_param,
	&nand_drv_info,
	&nand_mal_info,
};

ADJUST_SETTING_HANDLER(nand)
{
	REFERENCE_PARAMETER(program_mode);
	
	if (pi->raw)
	{
		struct chip_area_info_t *flash_info = NULL;
		uint32_t page_size, page_size_all, page_num_per_block;
		
		flash_info = target_get_chip_area(param, APPLICATION_IDX);
		if (NULL == flash_info)
		{
			return VSFERR_FAIL;
		}
		
		page_size = param->param[NAND_PARAM_WRITE_PAGE_SIZE];
		page_num_per_block = param->param[NAND_PARAM_ERASE_PAGE_SIZE] / page_size;
		page_size_all = page_size + 16 * page_size / 512;
		
		flash_info->page_size = page_num_per_block * page_size_all;
		flash_info->size = flash_info->page_size * flash_info->page_num;
	}
	return VSFERR_NONE;
}

ENTER_PROGRAM_MODE_HANDLER(nand)
{
	struct program_info_t *pi = context->pi;
	struct chip_param_t *param = context->param;
	struct chip_area_info_t *flash_info = NULL;
	
	flash_info = target_get_chip_area(param, APPLICATION_IDX);
	if (NULL == flash_info)
	{
		return VSFERR_FAIL;
	}
	
	if (pi->ifs_indexes != NULL)
	{
		if (dal_config_interface(NAND_STRING, pi->ifs_indexes, &nand_dal_info))
		{
			return VSFERR_FAIL;
		}
	}
	else
	{
		nand_drv_ifs.ebi_port = 0;
		nand_drv_ifs.nand_index = 1;
	}
	
	if (pi->raw)
	{
		uint32_t page_size, page_size_all, page_num_per_block, block_num;
		
		page_size = param->param[NAND_PARAM_WRITE_PAGE_SIZE];
		block_num = flash_info->page_num;
		if (0 == page_size)
		{
			page_num_per_block = 0;
		}
		else
		{
			page_num_per_block = param->param[NAND_PARAM_ERASE_PAGE_SIZE] / page_size;
		}
		
		// fix for spare area
		page_size_all = page_size + 16 * page_size / 512;
		
		nand_mal_info.capacity.block_number = block_num;
		nand_mal_info.capacity.block_size = page_num_per_block * page_size_all;
		nand_mal_info.read_page_size = page_size_all;
		nand_mal_info.write_page_size = page_size_all;
		nand_mal_info.erase_page_size = page_size_all * page_num_per_block;
	}
	else
	{
		nand_mal_info.capacity.block_number = flash_info->page_num;
		nand_mal_info.capacity.block_size = flash_info->page_size;
		nand_mal_info.read_page_size = param->param[NAND_PARAM_READ_PAGE_SIZE];
		nand_mal_info.write_page_size = param->param[NAND_PARAM_WRITE_PAGE_SIZE];
		nand_mal_info.erase_page_size = param->param[NAND_PARAM_ERASE_PAGE_SIZE];
	}
	
	nand_drv_param.nand_info.common_info.data_width = 8;
	nand_drv_param.nand_info.common_info.wait_signal = EBI_WAIT_NONE;
	nand_drv_param.nand_info.param.clock_hz = 0;
	nand_drv_param.nand_info.param.ecc.ecc_enable = false;
	nand_drv_param.nand_info.param.ecc.ecc_page_size =
								(uint16_t)nand_mal_info.read_page_size ?
								(uint16_t)nand_mal_info.read_page_size : 512;
	nand_drv_param.nand_info.param.timing.ale_to_re_cycle = 1;
	nand_drv_param.nand_info.param.timing.cle_to_re_cycle = 1;
	nand_drv_param.nand_info.param.timing.setup_cycle = 10;
	nand_drv_param.nand_info.param.timing.wait_cycle = 10;
	nand_drv_param.nand_info.param.timing.hold_cycle = 10;
	nand_drv_param.nand_info.param.timing.hiz_cycle = 10;
	nand_drv_param.nand_info.param.timing.setup_cycle_attr = 10;
	nand_drv_param.nand_info.param.timing.wait_cycle_attr = 10;
	nand_drv_param.nand_info.param.timing.hold_cycle_attr = 10;
	nand_drv_param.nand_info.param.timing.hiz_cycle_attr = 10;
	nand_drv_param.nand_info.param.addr.cmd = 0x00010000;
	nand_drv_param.nand_info.param.addr.addr = 0x00020000;
	nand_drv_param.nand_info.param.addr.data = 0x00000000;
	nand_drv_param.block_read_en = (bool)param->param[NAND_PARAM_BLOCK_READ_EN];
	nand_drv_param.col_addr_size =
							(uint8_t)param->param[NAND_PARAM_COL_ADDR_SIZE];
	nand_drv_param.col_addr_msb =
							(uint8_t)param->param[NAND_PARAM_COL_ADDR_MSB];
	nand_drv_param.row_addr_size =
							(uint8_t)param->param[NAND_PARAM_ROW_ADDR_SIZE];
	nand_drv_param.row_addr_lsb =
							(uint8_t)param->param[NAND_PARAM_ROW_ADDR_LSB];
	
	if (mal.init(&nand_dal_info) || mal.getinfo(&nand_dal_info))
	{
		return VSFERR_FAIL;
	}
	
	return dal_commit();
}

LEAVE_PROGRAM_MODE_HANDLER(nand)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	mal.fini(&nand_dal_info);
	return dal_commit();
}

ERASE_TARGET_HANDLER(nand)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(size);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		addr /= nand_mal_info.erase_page_size;
		addr *= context->param->param[NAND_PARAM_ERASE_PAGE_SIZE];
		return mal.eraseblock(&nand_dal_info, addr, 1);
	default:
		return VSFERR_FAIL;
	}
}

WRITE_TARGET_HANDLER(nand)
{
	struct chip_area_info_t *flash_info = NULL;
	uint32_t i;
	
	switch (area)
	{
	case APPLICATION_CHAR:
		flash_info = target_get_chip_area(context->param, APPLICATION_IDX);
		if ((NULL == flash_info) || (size % flash_info->page_size))
		{
			return VSFERR_FAIL;
		}
		size /= nand_mal_info.write_page_size;
		addr /= nand_mal_info.write_page_size;
		addr *= context->param->param[NAND_PARAM_WRITE_PAGE_SIZE];
		
		for (i = 0; i < size; i++)
		{
			if (mal.writeblock(&nand_dal_info, addr, buff, 1))
			{
				return VSFERR_FAIL;
			}
			addr += context->param->param[NAND_PARAM_WRITE_PAGE_SIZE];
			buff += nand_mal_info.write_page_size;
		}
		return VSFERR_NONE;
		break;
	default:
		return VSFERR_FAIL;
	}
}

READ_TARGET_HANDLER(nand)
{
	struct chip_area_info_t *flash_info = NULL;
	uint32_t i;
	
	switch (area)
	{
	case CHIPID_CHAR:
		if (mal.getinfo(&nand_dal_info))
		{
			return VSFERR_FAIL;
		}
		buff[3] = nand_drv_info.manufacturer_id;
		buff[2] = (uint8_t)nand_drv_info.device_id[0];
		buff[1] = (uint8_t)nand_drv_info.device_id[1];
		buff[0] = (uint8_t)nand_drv_info.device_id[2];
		return VSFERR_NONE;
		break;
	case APPLICATION_CHAR:
		flash_info = target_get_chip_area(context->param, APPLICATION_IDX);
		if ((NULL == flash_info) || (size % flash_info->page_size))
		{
			return VSFERR_FAIL;
		}
		size /= nand_mal_info.read_page_size;
		addr /= nand_mal_info.read_page_size;
		addr *= context->param->param[NAND_PARAM_READ_PAGE_SIZE];
		
		for (i = 0; i < size; i++)
		{
			if (mal.readblock(&nand_dal_info, addr, buff, 1))
			{
				return VSFERR_FAIL;
			}
			addr += context->param->param[NAND_PARAM_READ_PAGE_SIZE];
			buff += nand_mal_info.read_page_size;
		}
		return VSFERR_NONE;
		break;
	default:
		return VSFERR_FAIL;
	}
}

#endif
