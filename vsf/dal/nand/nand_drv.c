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

#include "app_type.h"

#include "interfaces.h"
#include "../mal/mal.h"
#include "../mal/mal_driver.h"

#if DAL_NAND_EN

#include "nand_drv.h"

#define NAND_RESET						0xFF
#define NAND_READ_ID					0x90
#define NAND_READ_STATUS				0x70
#define NAND_READ_SETUP					0x00
#define NAND_READ_CONFIRM				0x30
#define NAND_PROGRAM_SETUP				0x80
#define NAND_PROGRAM_CONFIRM			0x10
#define NAND_ERASE_SETUP				0x60
#define NAND_ERASE_CONFIRM				0xD0

static vsf_err_t nand_drv_write_command8(struct dal_info_t *info, uint8_t cmd)
{
	struct nand_drv_interface_t *ifs = (struct nand_drv_interface_t *)info->ifs;
	struct nand_drv_param_t *param = (struct nand_drv_param_t *)info->param;
	uint32_t cmd_addr = param->nand_info.param.addr.cmd +
						param->nand_info.common_info.mux_addr_mask;
	
	return interfaces->ebi.write(ifs->ebi_port, 
				ifs->nand_index | EBI_TGTTYP_NAND, cmd_addr, 1, &cmd, 1);
}

static vsf_err_t nand_drv_write_address8(struct dal_info_t *info, uint8_t addr)
{
	struct nand_drv_interface_t *ifs = (struct nand_drv_interface_t *)info->ifs;
	struct nand_drv_param_t *param = (struct nand_drv_param_t *)info->param;
	uint32_t addr_addr = param->nand_info.param.addr.addr +
							param->nand_info.common_info.mux_addr_mask;
	
	return interfaces->ebi.write(ifs->ebi_port, 
				ifs->nand_index | EBI_TGTTYP_NAND, addr_addr, 1, &addr, 1);
}

static vsf_err_t nand_drv_write_data8(struct dal_info_t *info, uint8_t *buff, 
									uint32_t count)
{
	struct nand_drv_interface_t *ifs = (struct nand_drv_interface_t *)info->ifs;
	struct nand_drv_param_t *param = (struct nand_drv_param_t *)info->param;
	uint32_t data_addr = param->nand_info.param.addr.data +
							param->nand_info.common_info.mux_addr_mask;
	
	return interfaces->ebi.write(ifs->ebi_port, 
				ifs->nand_index | EBI_TGTTYP_NAND, data_addr, 1, buff, count);
}

static vsf_err_t nand_drv_write_data16(struct dal_info_t *info, uint16_t *buff, 
									uint32_t count)
{
	struct nand_drv_interface_t *ifs = (struct nand_drv_interface_t *)info->ifs;
	struct nand_drv_param_t *param = (struct nand_drv_param_t *)info->param;
	uint32_t data_addr = param->nand_info.param.addr.data +
							param->nand_info.common_info.mux_addr_mask;
	
	return interfaces->ebi.write(ifs->ebi_port, 
				ifs->nand_index | EBI_TGTTYP_NAND, data_addr, 2,
				(uint8_t *)buff, count);
}

static vsf_err_t nand_drv_read_data8(struct dal_info_t *info, uint8_t *buff, 
									uint32_t count)
{
	struct nand_drv_interface_t *ifs = (struct nand_drv_interface_t *)info->ifs;
	struct nand_drv_param_t *param = (struct nand_drv_param_t *)info->param;
	uint32_t data_addr = param->nand_info.param.addr.data +
							param->nand_info.common_info.mux_addr_mask;
	
	return interfaces->ebi.read(ifs->ebi_port, 
				ifs->nand_index | EBI_TGTTYP_NAND, data_addr, 1, buff, count);
}

static vsf_err_t nand_drv_read_data16(struct dal_info_t *info, uint16_t *buff, 
										uint32_t count)
{
	struct nand_drv_interface_t *ifs = (struct nand_drv_interface_t *)info->ifs;
	struct nand_drv_param_t *param = (struct nand_drv_param_t *)info->param;
	uint32_t data_addr = param->nand_info.param.addr.data +
							param->nand_info.common_info.mux_addr_mask;
	
	return interfaces->ebi.read(ifs->ebi_port, 
				ifs->nand_index | EBI_TGTTYP_NAND, data_addr, 2,
				(uint8_t *)buff, count);
}

static vsf_err_t nand_drv_read_status(struct dal_info_t *info, uint8_t *status)
{
	nand_drv_write_command8(info, NAND_READ_STATUS);
	return nand_drv_read_data8(info, status, 1);
}

static vsf_err_t nand_drv_isready(struct dal_info_t *info)
{
	struct nand_drv_interface_t *ifs = (struct nand_drv_interface_t *)info->ifs;
	
	return interfaces->ebi.isready(ifs->ebi_port,
									ifs->nand_index | EBI_TGTTYP_NAND);
}

static vsf_err_t nand_drv_init_nb(struct dal_info_t *info)
{
	struct nand_drv_interface_t *ifs = (struct nand_drv_interface_t *)info->ifs;
	struct nand_drv_param_t *nand_drv_param = 
										(struct nand_drv_param_t *)info->param;
	
	interfaces->ebi.init(ifs->ebi_port);
	interfaces->ebi.config(ifs->ebi_port, ifs->nand_index | EBI_TGTTYP_NAND, 
							&nand_drv_param->nand_info);
	
	return nand_drv_write_command8(info, NAND_RESET);
}

static vsf_err_t nand_drv_getinfo(struct dal_info_t *info)
{
	uint8_t id[4];
	struct nand_drv_info_t *pinfo = (struct nand_drv_info_t *)info->info;
	
	nand_drv_write_command8(info, NAND_READ_ID);
	nand_drv_write_address8(info, 0x00);
	nand_drv_read_data8(info, id, 4);
	if (interfaces->peripheral_commit())
	{
		return VSFERR_FAIL;
	}
	
	pinfo->manufacturer_id = id[0];
	pinfo->device_id[0] = id[1];
	pinfo->device_id[1] = id[2];
	pinfo->device_id[2] = id[3];
	return VSFERR_NONE;
}

static vsf_err_t nand_drv_fini(struct dal_info_t *info)
{
	struct nand_drv_interface_t *ifs = (struct nand_drv_interface_t *)info->ifs;
	
	return interfaces->ebi.fini(ifs->ebi_port);
}

static vsf_err_t nand_drv_eraseblock_nb_start(struct dal_info_t *info, 
											uint64_t address, uint64_t count)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	return VSFERR_NONE;
}

static vsf_err_t nand_drv_eraseblock_nb(struct dal_info_t *info, uint64_t address)
{
	struct nand_drv_param_t *param = (struct nand_drv_param_t *)info->param;
	uint8_t data_width = param->nand_info.common_info.data_width / 8;
	uint32_t block_address = (uint32_t)(address >> param->row_addr_lsb);
	uint8_t i;
	
	nand_drv_write_command8(info, NAND_ERASE_SETUP);
	for (i = 0; i < param->row_addr_size; i += data_width)
	{
		nand_drv_write_address8(info, block_address & 0xFF);
		block_address >>= 8;
	}
	nand_drv_write_command8(info, NAND_ERASE_CONFIRM);
	return VSFERR_NONE;
}

static vsf_err_t nand_drv_eraseblock_nb_isready(struct dal_info_t *info, 
												uint64_t address)
{
	uint8_t status;
	vsf_err_t ret;
	
	REFERENCE_PARAMETER(address);
	ret = nand_drv_isready(info);
	if (VSFERR_NONE == ret)
	{
		if (nand_drv_read_status(info, &status) ||
			interfaces->peripheral_commit() ||
			(status & 1))
		{
			return VSFERR_FAIL;
		}
		return VSFERR_NONE;
	}
	else if (VSFERR_NOT_READY == ret)
	{
		return ret;
	}
	else
	{
		return VSFERR_FAIL;
	}
}

static vsf_err_t nand_drv_eraseblock_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return VSFERR_NONE;
}

static vsf_err_t nand_drv_readblock_nb_start(struct dal_info_t *info, 
								uint64_t address, uint64_t count, uint8_t *buff)
{
	struct nand_drv_param_t *param = (struct nand_drv_param_t *)info->param;
	
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	REFERENCE_PARAMETER(buff);
	
	param->addr_loadded = false;
	return VSFERR_NONE;
}

static vsf_err_t nand_drv_readblock_nb(struct dal_info_t *info, uint64_t address, 
										uint8_t *buff)
{
	struct nand_drv_param_t *param = (struct nand_drv_param_t *)info->param;
	uint8_t data_width = param->nand_info.common_info.data_width / 8;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	uint32_t read_page_size = mal_info->read_page_size;
#if NAND_DRV_ECC_EN
	uint32_t ecc_size = 16 * read_page_size / 512;
	uint8_t ecc_buff[64];
#endif
	vsf_err_t ret;
	
	REFERENCE_PARAMETER(address);
	switch (data_width)
	{
	case 1:
		ret = nand_drv_read_data8(info, buff, read_page_size);
#if NAND_DRV_ECC_EN
		if (!ret)
		{
			nand_drv_read_data8(info, ecc_buff, ecc_size);
			ret = interfaces->peripheral_commit();
		}
#endif
		break;
	case 2:
		read_page_size >>= 1;
		ret = nand_drv_read_data16(info, (uint16_t *)buff, read_page_size);
#if NAND_DRV_ECC_EN
		if (!ret)
		{
			nand_drv_read_data16(info, (uint16_t *)ecc_buff, ecc_size / 2);
			ret = interfaces->peripheral_commit();
		}
#endif
		break;
	default:
		return VSFERR_FAIL;
	}
	if (!param->block_read_en)
	{
		param->addr_loadded = false;
	}
	return ret;
}

static vsf_err_t nand_drv_readblock_nb_isready(struct dal_info_t *info, 
											uint64_t address, uint8_t *buff)
{
	struct nand_drv_param_t *param = (struct nand_drv_param_t *)info->param;
	
	REFERENCE_PARAMETER(buff);
	if (!param->addr_loadded)
	{
		uint32_t address32 =
			(uint32_t)((address & ((1UL << (param->col_addr_msb + 1)) - 1)) |
			((address >> param->row_addr_lsb) << (8 * param->col_addr_size)));
		uint8_t cmd, i;
		
		cmd = NAND_READ_SETUP + ((address >> (param->col_addr_msb + 1)) &
				((1UL << (param->row_addr_lsb - param->col_addr_msb - 1)) - 1));
		nand_drv_write_command8(info, cmd);
		for (i = 0; i < (param->col_addr_size + param->row_addr_size); i++)
		{
			nand_drv_write_address8(info, address32 & 0xFF);
			address32 >>= 8;
		}
		param->addr_loadded = true;
		nand_drv_write_command8(info, NAND_READ_CONFIRM);
	}
	return nand_drv_isready(info);
}

static vsf_err_t nand_drv_readblock_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return VSFERR_NONE;
}

static vsf_err_t nand_drv_writeblock_nb_start(struct dal_info_t *info, 
								uint64_t address, uint64_t count, uint8_t *buff)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	REFERENCE_PARAMETER(buff);
	return VSFERR_NONE;
}

static vsf_err_t nand_drv_writeblock_nb(struct dal_info_t *info, uint64_t address, 
										uint8_t *buff)
{
	struct nand_drv_param_t *param = (struct nand_drv_param_t *)info->param;
	uint8_t data_width = param->nand_info.common_info.data_width / 8;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	uint32_t write_page_size = mal_info->write_page_size;
#if NAND_DRV_ECC_EN
	uint32_t ecc_size = 16 * write_page_size / 512;
	uint8_t ecc_buff[64];
#endif
	uint32_t address32 =
			(uint32_t)((address & ((1UL << (param->col_addr_msb + 1)) - 1)) |
			((address >> param->row_addr_lsb) << (8 * param->col_addr_size)));
	uint8_t i;
	
	nand_drv_write_command8(info, NAND_PROGRAM_SETUP);
	for (i = 0; i < (param->col_addr_size + param->row_addr_size); i++)
	{
		nand_drv_write_address8(info, address32 & 0xFF);
		address32 >>= 8;
	}
	switch (data_width)
	{
	case 1:
		nand_drv_write_data8(info, buff, write_page_size);
#if NAND_DRV_ECC_EN
		// TODO: get ecc into ecc_buff
		memset(ecc_buff, 0xFF, ecc_size);
		nand_drv_write_data8(info, ecc_buff, ecc_size);
#endif
		break;
	case 2:
		nand_drv_write_data16(info, (uint16_t *)buff, write_page_size / 2);
#if NAND_DRV_ECC_EN
		// TODO: get ecc into ecc_buff
		memset(ecc_buff, 0xFF, ecc_size);
		nand_drv_write_data16(info, (uint16_t *)ecc_buff, ecc_size / 2);
#endif
		break;
	default:
		return VSFERR_FAIL;
	}
	return nand_drv_write_command8(info, NAND_PROGRAM_CONFIRM);
}

static vsf_err_t nand_drv_writeblock_nb_isready(struct dal_info_t *info, 
												uint64_t address, uint8_t *buff)
{
	uint8_t status;
	vsf_err_t ret;
	
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	ret = nand_drv_isready(info);
	if (VSFERR_NONE == ret)
	{
		if (nand_drv_read_status(info, &status) ||
			interfaces->peripheral_commit() ||
			(status & 1))
		{
			return VSFERR_FAIL;
		}
		return VSFERR_NONE;
	}
	else if (VSFERR_NOT_READY == ret)
	{
		return ret;
	}
	else
	{
		return VSFERR_FAIL;
	}
}

static vsf_err_t nand_drv_writeblock_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return VSFERR_NONE;
}

#if DAL_INTERFACE_PARSER_EN
static vsf_err_t nand_drv_parse_interface(struct dal_info_t *info, uint8_t *buff)
{
	struct nand_drv_interface_t *ifs = (struct nand_drv_interface_t *)info->ifs;
	
	ifs->ebi_port = buff[0];
	ifs->nand_index = buff[1];
	return VSFERR_NONE;
}
#endif

const struct mal_driver_t nand_drv = 
{
	{
		"nand",
#if DAL_INTERFACE_PARSER_EN
		"ebi:%1dnand:%1d",
		nand_drv_parse_interface,
#endif
	},
	
	MAL_SUPPORT_READBLOCK | MAL_SUPPORT_WRITEBLOCK | MAL_SUPPORT_ERASEBLOCK,
	
	nand_drv_init_nb,
	NULL,
	nand_drv_fini,
	nand_drv_getinfo,
	NULL,
	
	NULL, NULL, NULL, NULL,
	
	NULL, NULL, NULL, NULL,
	
	nand_drv_eraseblock_nb_start,
	nand_drv_eraseblock_nb,
	nand_drv_eraseblock_nb_isready,
	NULL,
	nand_drv_eraseblock_nb_end,
	
	nand_drv_readblock_nb_start,
	nand_drv_readblock_nb,
	nand_drv_readblock_nb_isready,
	NULL,
	nand_drv_readblock_nb_end,
	
	nand_drv_writeblock_nb_start,
	nand_drv_writeblock_nb,
	nand_drv_writeblock_nb_isready,
	NULL,
	nand_drv_writeblock_nb_end
};

#endif
