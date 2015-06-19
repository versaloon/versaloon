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

#if DAL_CFI_EN

#include "cfi_drv.h"

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
	struct cfi_drv_param_t *param = (struct cfi_drv_param_t *)info->param;
	struct cfi_drv_interface_t *ifs = (struct cfi_drv_interface_t *)info->ifs;
	
	address += param->nor_info.common_info.mux_addr_mask + param->base_addr;
	return interfaces->ebi.read(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR,
								address, data_size, buff, count);
}

static vsf_err_t cfi_write(struct dal_info_t *info, uint32_t address,
							uint8_t data_size, uint8_t *buff, uint32_t count)
{
	struct cfi_drv_param_t *param = (struct cfi_drv_param_t *)info->param;
	struct cfi_drv_interface_t *ifs = (struct cfi_drv_interface_t *)info->ifs;
	
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
	struct cfi_drv_param_t *param = (struct cfi_drv_param_t *)info->param;
	uint8_t data_width = param->nor_info.common_info.data_width / 8;
	
	cfi_read(info, (uint32_t)address, data_width, (uint8_t *)&orig_status, 1);
	
	do {
		cfi_read(info, (uint32_t)address, data_width, (uint8_t *)&cur_status,
					1);
		interfaces->peripheral_commit();
		
		if ((cur_status ^ orig_status) & 0x0040)
		{
			if (cur_status & 0x0020)
			{
				cfi_read(info, (uint32_t)address, data_width,
							(uint8_t *)&orig_status, 1);
				cfi_read(info, (uint32_t)address, data_width,
							(uint8_t *)&cur_status, 1);
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
	uint8_t cfi_info8[256];
	uint16_t *cfi_info16 = (uint16_t *)cfi_info8;
	uint32_t *cfi_info32 = (uint32_t *)cfi_info8;
	uint32_t manufacturer_id = 0;
	struct cfi_drv_info_t *pinfo = (struct cfi_drv_info_t *)info->info;
	struct mal_info_t *cfi_mal_info = (struct mal_info_t *)info->extra;
	struct cfi_drv_param_t *param = (struct cfi_drv_param_t *)info->param;
	uint8_t data_width = param->nor_info.common_info.data_width / 8;
	
	cfi_write_cmd(info, 0xAA, data_width, 0x0555 << 1);
	cfi_write_cmd(info, 0x55, data_width, 0x02AA << 1);
	cfi_write_cmd(info, 0x90, data_width, 0x0555 << 1);
	
	cfi_read(info, 0x0000 << 1, data_width, (uint8_t *)&manufacturer_id, 1);
	cfi_read(info, 0x0001 << 1, data_width, (uint8_t *)&pinfo->device_id[0], 1);
	cfi_read(info, 0x000E << 1, data_width, (uint8_t *)&pinfo->device_id[1], 1);
	cfi_read(info, 0x000F << 1, data_width, (uint8_t *)&pinfo->device_id[2], 1);
	interfaces->peripheral_commit();
	
	pinfo->manufacturer_id = (uint8_t)manufacturer_id;
	pinfo->device_id[0] = LE_TO_SYS_U16(pinfo->device_id[0]);
	pinfo->device_id[1] = LE_TO_SYS_U16(pinfo->device_id[1]);
	pinfo->device_id[2] = LE_TO_SYS_U16(pinfo->device_id[2]);
	pinfo->die_num = 1;
	
	cfi_write_cmd(info, 0xAA, data_width, 0x0555 << 1);
	cfi_write_cmd(info, 0x55, data_width, 0x02AA << 1);
	cfi_write_cmd(info, 0xF0, data_width, 0);
	
	cfi_write_cmd(info, 0x98, data_width, 0x0055 << 1);
	cfi_read(info, 0x0010 << 1, data_width, (uint8_t *)cfi_info8,
				sizeof(cfi_info8) / data_width);
	if (interfaces->peripheral_commit() ||
		((1 == data_width) &&
			((cfi_info8[0] != 'Q') || (cfi_info8[1] != 'R') ||
			(cfi_info8[2] != 'Y'))) ||
		((2 == data_width) &&
			((cfi_info16[0] != 'Q') || (cfi_info16[1] != 'R') ||
			(cfi_info16[2] != 'Y'))) ||
		((4 == data_width) &&
			((cfi_info32[0] != 'Q') || (cfi_info32[1] != 'R') ||
			(cfi_info32[2] != 'Y'))))
	{
		return VSFERR_FAIL;
	}
	if (!cfi_mal_info->capacity.block_number ||
		!cfi_mal_info->capacity.block_size)
	{
		switch (data_width)
		{
		case 1:
			cfi_mal_info->capacity.block_number = cfi_info8[0x1D] + 1;
			cfi_mal_info->capacity.block_size =
					((uint64_t)1 << cfi_info8[0x17]) / (cfi_info8[0x1D] + 1);
			break;
		case 2:
			cfi_mal_info->capacity.block_number = cfi_info16[0x1D] + 1;
			cfi_mal_info->capacity.block_size =
					((uint64_t)1 << cfi_info16[0x17]) / (cfi_info16[0x1D] + 1);
			break;
		case 4:
			cfi_mal_info->capacity.block_number = cfi_info32[0x1D] + 1;
			cfi_mal_info->capacity.block_size =
					((uint64_t)1 << cfi_info32[0x17]) / (cfi_info32[0x1D] + 1);
			break;
		default:
			return VSFERR_FAIL;
		}
	}
	switch (data_width)
	{
	case 1:
		cfi_mal_info->write_page_size = 1 << cfi_info8[0x1A];
		break;
	case 2:
		cfi_mal_info->write_page_size = 1 << cfi_info16[0x1A];
		break;
	case 4:
		cfi_mal_info->write_page_size = 1 << cfi_info32[0x1A];
		break;
	default:
		return VSFERR_FAIL;
	}
	if (1 == cfi_mal_info->write_page_size)
	{
		// write size is not supprted
		return VSFERR_FAIL;
	}
	
	cfi_mal_info->erase_page_size = (uint32_t)cfi_mal_info->capacity.block_size;
	
	cfi_write_cmd(info, 0xAA, data_width, 0x0555 << 1);
	cfi_write_cmd(info, 0x55, data_width, 0x02AA << 1);
	cfi_write_cmd(info, 0xF0, data_width, 0);
	return VSFERR_NONE;
}

static vsf_err_t cfi_drv_init_nb(struct dal_info_t *info)
{
	struct mal_info_t *cfi_mal_info = (struct mal_info_t *)info->extra;
	struct cfi_drv_interface_t *ifs = (struct cfi_drv_interface_t *)info->ifs;
	struct cfi_drv_param_t *param = (struct cfi_drv_param_t *)info->param;
	
	interfaces->ebi.init(ifs->ebi_port);
	interfaces->ebi.config(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR,
							&param->nor_info);
	
	if (cfi_drv_getinfo(info))
	{
		return VSFERR_FAIL;
	}
	
	cfi_mal_info->read_page_size = 0;
	return VSFERR_NONE;
}

static vsf_err_t cfi_drv_fini(struct dal_info_t *info)
{
	struct cfi_drv_interface_t *ifs = (struct cfi_drv_interface_t *)info->ifs;
	
	interfaces->ebi.fini(ifs->ebi_port);
	return VSFERR_NONE;
}

static vsf_err_t cfi_drv_eraseall_nb_start(struct dal_info_t *info)
{
	struct cfi_drv_info_t *pinfo = (struct cfi_drv_info_t *)info->info;
	struct cfi_drv_param_t *param = (struct cfi_drv_param_t *)info->param;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	uint8_t data_width = param->nor_info.common_info.data_width / 8;
	uint64_t die_size = mal_info->capacity.block_size *
						mal_info->capacity.block_number / pinfo->die_num;
	uint8_t i;
	uint32_t addr_tmp;
	
	for (i = 0; i < pinfo->die_num; i++)
	{
		addr_tmp = (uint32_t)(i * die_size);
		cfi_write_cmd(info, 0xAA, data_width, (0x0555 << 1) + addr_tmp);
		cfi_write_cmd(info, 0x55, data_width, (0x02AA << 1) + addr_tmp);
		cfi_write_cmd(info, 0x80, data_width, (0x0555 << 1) + addr_tmp);
		cfi_write_cmd(info, 0xAA, data_width, (0x0555 << 1) + addr_tmp);
		cfi_write_cmd(info, 0x55, data_width, (0x02AA << 1) + addr_tmp);
		cfi_write_cmd(info, 0x10, data_width, (0x0555 << 1) + addr_tmp);
	}
	return VSFERR_NONE;
}

static vsf_err_t cfi_drv_eraseall_nb_isready(struct dal_info_t *info)
{
	uint32_t val1 = 0, val2 = 0;
	bool ready;
	struct cfi_drv_info_t *pinfo = (struct cfi_drv_info_t *)info->info;
	struct cfi_drv_param_t *param = (struct cfi_drv_param_t *)info->param;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	uint8_t data_width = param->nor_info.common_info.data_width / 8;
	uint64_t die_size = mal_info->capacity.block_size *
						mal_info->capacity.block_number / pinfo->die_num;
	uint8_t i;
	
	ready = true;
	for (i = 0; i < pinfo->die_num; i++)
	{
		cfi_read(info, (uint32_t)((0x0000 << 1) + i * die_size), data_width,
					(uint8_t *)&val1, 1);
		cfi_read(info, (uint32_t)((0x0000 << 1) + i * die_size), data_width,
					(uint8_t *)&val2, 1);
		interfaces->peripheral_commit();
		
		ready = ready && (((val1 ^ val2) & 0x0040) == 0);
	}
	return ready ? VSFERR_NONE : VSFERR_FAIL;
}

static vsf_err_t cfi_drv_eraseall_waitready(struct dal_info_t *info)
{
	struct cfi_drv_info_t *pinfo = (struct cfi_drv_info_t *)info->info;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	uint64_t die_size = mal_info->capacity.block_size *
						mal_info->capacity.block_number / pinfo->die_num;
	uint8_t i;
	
	for (i = 0; i < pinfo->die_num; i++)
	{
		if (VSFERR_NONE != cfi_wait_busy(info, i * die_size))
		{
			return VSFERR_FAIL;
		}
	}
	return VSFERR_NONE;
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

static vsf_err_t cfi_drv_eraseblock_nb(struct dal_info_t *info,
										uint64_t address)
{
	struct cfi_drv_info_t *pinfo = (struct cfi_drv_info_t *)info->info;
	struct cfi_drv_param_t *param = (struct cfi_drv_param_t *)info->param;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	uint8_t data_width = param->nor_info.common_info.data_width / 8;
	uint64_t die_size = mal_info->capacity.block_size *
						mal_info->capacity.block_number / pinfo->die_num;
	uint32_t cmd_address = (uint32_t)(address / die_size * die_size);
	
	cfi_write_cmd(info, 0xAA, data_width, (0x0555 << 1) + cmd_address);
	cfi_write_cmd(info, 0x55, data_width, (0x02AA << 1) + cmd_address);
	cfi_write_cmd(info, 0x80, data_width, (0x0555 << 1) + cmd_address);
	cfi_write_cmd(info, 0xAA, data_width, (0x0555 << 1) + cmd_address);
	cfi_write_cmd(info, 0x55, data_width, (0x02AA << 1) + cmd_address);
	cfi_write_cmd(info, 0x30, data_width, (uint32_t)address);
	return VSFERR_NONE;
}

static vsf_err_t cfi_drv_eraseblock_nb_isready(struct dal_info_t *info,
												uint64_t address)
{
	struct cfi_drv_info_t *pinfo = (struct cfi_drv_info_t *)info->info;
	uint32_t val1 = 0, val2 = 0;
	struct cfi_drv_param_t *param = (struct cfi_drv_param_t *)info->param;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	uint8_t data_width = param->nor_info.common_info.data_width / 8;
	uint64_t die_size = mal_info->capacity.block_size *
						mal_info->capacity.block_number / pinfo->die_num;
	uint32_t cmd_address = (uint32_t)(address / die_size * die_size);
	
	cfi_read(info, cmd_address, data_width, (uint8_t *)&val1, 1);
	cfi_read(info, cmd_address, data_width, (uint8_t *)&val2, 1);
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
	struct cfi_drv_info_t *pinfo = (struct cfi_drv_info_t *)info->info;
	struct cfi_drv_param_t *param = (struct cfi_drv_param_t *)info->param;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	uint8_t data_width = param->nor_info.common_info.data_width / 8;
	uint64_t die_size = mal_info->capacity.block_size *
						mal_info->capacity.block_number / pinfo->die_num;
	uint8_t i;
	uint32_t addr_tmp;
	
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	REFERENCE_PARAMETER(buff);
	
	for (i = 0; i < pinfo->die_num; i++)
	{
		addr_tmp = (uint32_t)(i * die_size);
		cfi_write_cmd(info, 0xAA, data_width, (0x0555 << 1) + addr_tmp);
		cfi_write_cmd(info, 0x55, data_width, (0x02AA << 1) + addr_tmp);
		cfi_write_cmd(info, 0xF0, data_width, addr_tmp);
	}
	return VSFERR_NONE;
}

static vsf_err_t cfi_drv_readblock_nb(struct dal_info_t *info, uint64_t address,
									uint8_t *buff)
{
	uint32_t count, i, cur_count;
	struct cfi_drv_param_t *param = (struct cfi_drv_param_t *)info->param;
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
	return VSFERR_NONE;
}

static vsf_err_t cfi_drv_writeblock_nb(struct dal_info_t *info,
										uint64_t address, uint8_t *buff)
{
	struct cfi_drv_info_t *pinfo = (struct cfi_drv_info_t *)info->info;
	struct cfi_drv_param_t *param = (struct cfi_drv_param_t *)info->param;
	uint8_t data_width = param->nor_info.common_info.data_width / 8;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	uint32_t write_page_size = mal_info->write_page_size;
	uint64_t die_size = mal_info->capacity.block_size *
						mal_info->capacity.block_number / pinfo->die_num;
	uint32_t cmd_address = (uint32_t)(address / die_size * die_size);
	
	cfi_write_cmd(info, 0xAA, data_width, (0x0555 << 1) + cmd_address);
	cfi_write_cmd(info, 0x55, data_width, (0x02AA << 1) + cmd_address);
	
	if (2 == write_page_size)
	{
		cfi_write_cmd(info, 0xA0, data_width, (0x0555 << 1) + cmd_address);
		cfi_write(info, (uint32_t)address, data_width, buff,
					write_page_size / data_width);
	}
	else
	{
		cfi_write_cmd(info, 0x25, data_width, (uint32_t)address);
		cfi_write_cmd(info, (uint8_t)(write_page_size / data_width - 1),
						data_width, (uint32_t)address);
		cfi_write(info, (uint32_t)address, data_width, buff,
					write_page_size / data_width);
		cfi_write_cmd(info, 0x29, data_width, (uint32_t)address);
	}
	return VSFERR_NONE;
}

static vsf_err_t cfi_drv_writeblock_nb_isready(struct dal_info_t *info,
											uint64_t address, uint8_t *buff)
{
	uint32_t status = 0, verify_data;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	struct cfi_drv_param_t *param = (struct cfi_drv_param_t *)info->param;
	uint8_t data_width = param->nor_info.common_info.data_width / 8;
	uint32_t write_page_size = mal_info->write_page_size;
	
	cfi_read(info, (uint32_t)address + write_page_size - data_width, data_width,
				(uint8_t *)&status, 1);
	interfaces->peripheral_commit();
	
	switch (data_width)
	{
	case 1:
		verify_data = buff[write_page_size - 1];
		break;
	case 2:
		verify_data = ((uint16_t *)buff)[write_page_size / 2 - 1];
		break;
	case 4:
		verify_data = ((uint32_t *)buff)[write_page_size / 4 - 1];
		break;
	default:
		return VSFERR_FAIL;
	}
	
	if (verify_data == status)
	{
		return VSFERR_NONE;
	}
	else if ((status & 0x20) || (status & 0x02))
	{
		cfi_read(info, (uint32_t)address + write_page_size - data_width,
					data_width, (uint8_t *)&status, 1);
		if (interfaces->peripheral_commit() ||
			(verify_data != status))
		{
			return VSFERR_FAIL;
		}
		return VSFERR_NONE;
	}
	
	return VSFERR_NOT_READY;
}

static vsf_err_t cfi_drv_writeblock_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return VSFERR_NONE;
}

#if DAL_INTERFACE_PARSER_EN
static vsf_err_t cfi_drv_parse_interface(struct dal_info_t *info, uint8_t *buff)
{
	struct cfi_drv_interface_t *ifs = (struct cfi_drv_interface_t *)info->ifs;
	
	ifs->ebi_port = buff[0];
	ifs->nor_index = buff[1];
	return VSFERR_NONE;
}
#endif

const struct mal_driver_t cfi_drv =
{
	{
		"cfi",
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
	cfi_wait_busy,
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
