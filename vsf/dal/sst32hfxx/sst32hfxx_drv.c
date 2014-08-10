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

#include "app_cfg.h"
#include "app_type.h"

#include "interfaces.h"
#include "../mal/mal.h"
#include "../mal/mal_driver.h"

#if DAL_SST32HFXX_EN
#include "sst32hfxx_drv.h"

// This driver reuse a lot code from cfi_drv
// So some function has the same name as in the cfi_drv

static uint32_t cfi_get_cmd(uint8_t cmd, uint8_t data_width)
{
	switch (data_width)
	{
	case 1:
		return (uint32_t)cmd;
	case 2:
		return (uint32_t)SYS_TO_LE_U16(cmd);
	case 4:
		return SYS_TO_LE_U32(cmd);
	default:
		return 0;
	}
}

static vsf_err_t cfi_read(struct dal_info_t *info, uint32_t address,
							uint8_t data_size, uint8_t *buff, uint32_t count)
{
	struct sst32hfxx_drv_param_t *param = (struct sst32hfxx_drv_param_t *)info->param;
	struct sst32hfxx_drv_interface_t *ifs = (struct sst32hfxx_drv_interface_t *)info->ifs;
	
	address += param->nor_info.common_info.mux_addr_mask + param->base_addr;
	return interfaces->ebi.read(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR,
								address, data_size, buff, count);
}

static vsf_err_t cfi_write(struct dal_info_t *info, uint32_t address,
							uint8_t data_size, uint8_t *buff, uint32_t count)
{
	struct sst32hfxx_drv_param_t *param = (struct sst32hfxx_drv_param_t *)info->param;
	struct sst32hfxx_drv_interface_t *ifs = (struct sst32hfxx_drv_interface_t *)info->ifs;
	
	address += param->nor_info.common_info.mux_addr_mask + param->base_addr;
	return interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR,
									address, data_size, buff, count);
}

static vsf_err_t cfi_write_cmd(struct dal_info_t *info, uint8_t cmd, 
							uint8_t data_width, uint32_t address)
{
	uint32_t data = cfi_get_cmd(cmd, data_width);
	
	return cfi_write(info, address, data_width, (uint8_t *)&data, 1);
}

static vsf_err_t cfi_wait_busy(struct dal_info_t *info, uint64_t address)
{
	uint32_t cur_status = 0, orig_status = 0;
	struct sst32hfxx_drv_param_t *param = (struct sst32hfxx_drv_param_t *)info->param;
	uint8_t data_width = param->nor_info.common_info.data_width / 8;
	
	cfi_read(info, (uint32_t)address, data_width, (uint8_t *)&orig_status, 1);
	
	do {
		cfi_read(info, 0, data_width, (uint8_t *)&cur_status, 1);
		interfaces->peripheral_commit();
		
		if ((cur_status ^ orig_status) & 0x0040)
		{
			if (cur_status & 0x0020)
			{
				cfi_read(info, 0, data_width, (uint8_t *)&orig_status, 1);
				cfi_read(info, 0, data_width, (uint8_t *)&cur_status, 1);
				interfaces->peripheral_commit();
				return ((cur_status ^ orig_status) & 0x0040) ?
							VSFERR_FAIL : VSFERR_NONE;
			}
		}
		else
		{
			break;
		}
		
		orig_status = cur_status;
	} while (1);
	return VSFERR_NONE;
}

static vsf_err_t cfi_drv_getinfo(struct dal_info_t *info)
{
	uint32_t manufacturer_id = 0, device_id = 0;
	struct sst32hfxx_drv_info_t *pinfo = (struct sst32hfxx_drv_info_t *)info->info;
	struct sst32hfxx_drv_param_t *param = (struct sst32hfxx_drv_param_t *)info->param;
	uint8_t data_width = param->nor_info.common_info.data_width / 8;
	
	cfi_write_cmd(info, 0xAA, data_width, 0x5555 << 1);
	cfi_write_cmd(info, 0x55, data_width, 0x2AAA << 1);
	cfi_write_cmd(info, 0x90, data_width, 0x5555 << 1);
	
	cfi_read(info, 0x0000 << 1, data_width, (uint8_t *)&manufacturer_id, 1);
	cfi_read(info, 0x0001 << 1, data_width, (uint8_t *)&device_id, 1);
	interfaces->peripheral_commit();
	pinfo->manufacturer_id = (uint8_t)manufacturer_id;
	pinfo->device_id = (uint16_t)device_id;

	cfi_write_cmd(info, 0xAA, data_width, 0x5555 << 1);
	cfi_write_cmd(info, 0x55, data_width, 0x2AAA << 1);
	cfi_write_cmd(info, 0xF0, data_width, 0x5555 << 1);
	return VSFERR_NONE;
}

static vsf_err_t cfi_drv_init_nb(struct dal_info_t *info)
{
	struct sst32hfxx_drv_interface_t *ifs = (struct sst32hfxx_drv_interface_t *)info->ifs;
	struct sst32hfxx_drv_param_t *param = (struct sst32hfxx_drv_param_t *)info->param;
	
	interfaces->ebi.init(ifs->ebi_port);
	interfaces->ebi.config(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							&param->nor_info);
	
	return cfi_drv_getinfo(info);
}

static vsf_err_t cfi_drv_fini(struct dal_info_t *info)
{
	struct sst32hfxx_drv_interface_t *ifs = (struct sst32hfxx_drv_interface_t *)info->ifs;
	
	interfaces->ebi.fini(ifs->ebi_port);
	return VSFERR_NONE;
}

static vsf_err_t cfi_drv_eraseall_nb_start(struct dal_info_t *info)
{
	struct sst32hfxx_drv_param_t *param = (struct sst32hfxx_drv_param_t *)info->param;
	uint8_t data_width = param->nor_info.common_info.data_width / 8;
	
	cfi_write_cmd(info, 0xAA, data_width, 0x5555 << 1);
	cfi_write_cmd(info, 0x55, data_width, 0x2AAA << 1);
	cfi_write_cmd(info, 0x80, data_width, 0x5555 << 1);
	cfi_write_cmd(info, 0xAA, data_width, 0x5555 << 1);
	cfi_write_cmd(info, 0x55, data_width, 0x2AAA << 1);
	cfi_write_cmd(info, 0x10, data_width, 0x5555 << 1);
	return VSFERR_NONE;
}

static vsf_err_t cfi_drv_eraseall_nb_isready(struct dal_info_t *info)
{
	uint32_t val1 = 0, val2 = 0;
	struct sst32hfxx_drv_param_t *param = (struct sst32hfxx_drv_param_t *)info->param;
	uint8_t data_width = param->nor_info.common_info.data_width / 8;
	
	cfi_read(info, 0x0000 << 1, data_width, (uint8_t *)&val1, 1);
	cfi_read(info, 0x0000 << 1, data_width, (uint8_t *)&val2, 1);
	interfaces->peripheral_commit();
	
	return (((val1 ^ val2) & 0x0040) == 0) ? VSFERR_NONE : VSFERR_NOT_READY;
}

static vsf_err_t cfi_drv_eraseall_waitready(struct dal_info_t *info)
{
	return cfi_wait_busy(info, 0);
}

static vsf_err_t cfi_drv_eraseall_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return VSFERR_NONE;
}

static vsf_err_t cfi_drv_eraseblock_nb_start(struct dal_info_t *info, 
											uint64_t address, uint64_t count)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	return VSFERR_NONE;
}

static vsf_err_t cfi_drv_eraseblock_nb(struct dal_info_t *info, uint64_t address)
{
	struct sst32hfxx_drv_param_t *param = (struct sst32hfxx_drv_param_t *)info->param;
	uint8_t data_width = param->nor_info.common_info.data_width / 8;
	
	cfi_write_cmd(info, 0xAA, data_width, 0x5555 << 1);
	cfi_write_cmd(info, 0x55, data_width, 0x2AAA << 1);
	cfi_write_cmd(info, 0x80, data_width, 0x5555 << 1);
	cfi_write_cmd(info, 0xAA, data_width, 0x5555 << 1);
	cfi_write_cmd(info, 0x55, data_width, 0x2AAA << 1);
	cfi_write_cmd(info, 0x30, data_width, (uint32_t)address);
	return VSFERR_NONE;
}

static vsf_err_t cfi_drv_eraseblock_nb_isready(struct dal_info_t *info, 
												uint64_t address)
{
	uint32_t val1 = 0, val2 = 0;
	struct sst32hfxx_drv_param_t *param = (struct sst32hfxx_drv_param_t *)info->param;
	uint8_t data_width = param->nor_info.common_info.data_width / 8;
	
	REFERENCE_PARAMETER(address);
	
	cfi_read(info, 0x0000 << 1, data_width, (uint8_t *)&val1, 1);
	cfi_read(info, 0x0000 << 1, data_width, (uint8_t *)&val2, 1);
	interfaces->peripheral_commit();
	
	return (((val1 ^ val2) & 0x0040) == 0) ? VSFERR_NONE : VSFERR_NOT_READY;
}

static vsf_err_t cfi_drv_eraseblock_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return VSFERR_NONE;
}

static vsf_err_t cfi_drv_readblock_nb_start(struct dal_info_t *info, 
								uint64_t address, uint64_t count, uint8_t *buff)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	REFERENCE_PARAMETER(buff);
	
	return VSFERR_NONE;
}

static vsf_err_t cfi_drv_readblock_nb(struct dal_info_t *info, uint64_t address, 
									uint8_t *buff)
{
	uint32_t count, i, cur_count;
	struct sst32hfxx_drv_param_t *param = (struct sst32hfxx_drv_param_t *)info->param;
	uint8_t data_width = param->nor_info.common_info.data_width / 8;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	
	if (mal_info->read_page_size)
	{
		count = (uint32_t)mal_info->read_page_size / data_width;
	}
	else
	{
		count = (uint32_t)mal_info->capacity.block_size / data_width;
	}
	
	i = 0;
	while (i < count)
	{
		cur_count = (count > 1024) ? 1024 : count;
		cfi_read(info, (uint32_t)address, data_width, buff, cur_count);
		address += cur_count * data_width;
		buff += cur_count * data_width;
		i += cur_count;
	}
	return interfaces->peripheral_commit();
}

static vsf_err_t cfi_drv_readblock_nb_isready(struct dal_info_t *info, 
											uint64_t address, uint8_t *buff)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	return VSFERR_NONE;
}

static vsf_err_t cfi_drv_readblock_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return VSFERR_NONE;
}

static vsf_err_t cfi_drv_writeblock_nb_start(struct dal_info_t *info, 
								uint64_t address, uint64_t count, uint8_t *buff)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	REFERENCE_PARAMETER(buff);
	
	cfi_write_cmd(info, 0xAA, 2, 0x5555 << 1);
	cfi_write_cmd(info, 0x55, 2, 0x2AAA << 1);
	cfi_write_cmd(info, 0xF0, 2, 0x5555 << 1);
	return VSFERR_NONE;
}

static vsf_err_t cfi_drv_writeblock_nb(struct dal_info_t *info, uint64_t address, 
										uint8_t *buff)
{
	uint32_t count, i;
	struct sst32hfxx_drv_param_t *param = (struct sst32hfxx_drv_param_t *)info->param;
	uint8_t data_width = param->nor_info.common_info.data_width / 8;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	
	if (mal_info->read_page_size)
	{
		count = (uint32_t)mal_info->read_page_size / data_width;
	}
	else
	{
		count = (uint32_t)mal_info->capacity.block_size / data_width;
	}
	
	for(i = 0; i < count; i++)
	{
		cfi_write_cmd(info, 0xAA, data_width, 0x5555 << 1);
		cfi_write_cmd(info, 0x55, data_width, 0x2AAA << 1);
		cfi_write_cmd(info, 0xA0, data_width, 0x5555 << 1);
		cfi_write(info, (uint32_t)address, data_width, buff, 1);
		
		interfaces->delay.delayus(param->delayus);
		
		address += data_width;
		buff += data_width;
	}
	
	return VSFERR_NONE;
}

static vsf_err_t cfi_drv_writeblock_nb_isready(struct dal_info_t *info, 
											uint64_t address, uint8_t *buff)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	return VSFERR_NONE;
}

static vsf_err_t cfi_drv_writeblock_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return VSFERR_NONE;
}

#if DAL_INTERFACE_PARSER_EN
static vsf_err_t cfi_drv_parse_interface(struct dal_info_t *info, uint8_t *buff)
{
	struct sst32hfxx_drv_interface_t *ifs = (struct sst32hfxx_drv_interface_t *)info->ifs;
	
	ifs->ebi_port = buff[0];
	ifs->nor_index = buff[1];
	return VSFERR_NONE;
}
#endif

const struct mal_driver_t sst32hfxx_nor_drv = 
{
	{
		"sst32hfxx",
#if DAL_INTERFACE_PARSER_EN
		"ebi:%1dnor:%1d",
		cfi_drv_parse_interface,
#endif
	},
	
	MAL_SUPPORT_READBLOCK | MAL_SUPPORT_WRITEBLOCK | MAL_SUPPORT_ERASEBLOCK,
	
	cfi_drv_init_nb,
	NULL,
	cfi_drv_fini,
	cfi_drv_getinfo,
	NULL,
	
	NULL, NULL, NULL, NULL,
	
	cfi_drv_eraseall_nb_start,
	cfi_drv_eraseall_nb_isready,
	cfi_drv_eraseall_waitready,
	cfi_drv_eraseall_nb_end,
	
	cfi_drv_eraseblock_nb_start,
	cfi_drv_eraseblock_nb,
	cfi_drv_eraseblock_nb_isready,
	NULL,
	cfi_drv_eraseblock_nb_end,
	
	cfi_drv_readblock_nb_start,
	cfi_drv_readblock_nb,
	cfi_drv_readblock_nb_isready,
	NULL,
	cfi_drv_readblock_nb_end,
	
	cfi_drv_writeblock_nb_start,
	cfi_drv_writeblock_nb,
	cfi_drv_writeblock_nb_isready,
	NULL,
	cfi_drv_writeblock_nb_end
};

#endif
