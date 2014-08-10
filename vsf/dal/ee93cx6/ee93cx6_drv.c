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

#include "../mal/mal.h"
#include "../mal/mal_driver.h"

#if DAL_EE93CX6_EN

#include "ee93cx6_drv_cfg.h"
#include "ee93cx6_drv.h"

#define EE93CX6_OPCODE_READ					0x02
#define EE93CX6_OPCODE_READ_BITLEN			2
#define EE93CX6_OPCODE_WRITE				0x01
#define EE93CX6_OPCODE_WRITE_BITLEN			2
#define EE93CX6_OPCODE_ERASE				0x03
#define EE93CX6_OPCODE_ERASE_BITLEN			2

#define EE93CX6_OPCODE_WEN					0x03
#define EE93CX6_OPCODE_WEN_BITLEN			4
#define EE93CX6_OPCODE_WDS					0x00
#define EE93CX6_OPCODE_WDS_BITLEN			4
#define EE93CX6_OPCODE_ERAL					0x02
#define EE93CX6_OPCODE_ERAL_BITLEN			4
#define EE93CX6_OPCODE_WRAL					0x01
#define EE93CX6_OPCODE_WRAL_BITLEN			4


static vsf_err_t ee93cx6_drv_poll(struct ee93cx6_drv_interface_t *ifs)
{
	return interfaces->microwire.poll(ifs->mw_port, 
							EE93CX6_POLL_INTERVAL_US, EE93CX6_POLL_RETRY_CNT);
}

static vsf_err_t ee93cx6_drv_init_nb(struct dal_info_t *info)
{
	struct ee93cx6_drv_interface_t *ifs = 
								(struct ee93cx6_drv_interface_t *)info->ifs;
	struct ee93cx6_drv_param_t *param = 
								(struct ee93cx6_drv_param_t *)info->param;
	uint32_t cmd;
	
	if (EE93CX6_ORIGINATION_BYTE != param->origination_mode)
	{
		param->cmd_bitlen += param->addr_bitlen - 1;
	}
	if (param->addr_bitlen > 32)
	{
		return VSFERR_INVALID_PARAMETER;
	}
	if (!param->iic_khz)
	{
		param->iic_khz = 2000;
	}
	
	interfaces->microwire.init(ifs->mw_port);
	interfaces->microwire.config(ifs->mw_port, param->iic_khz, 1);
	
	cmd = EE93CX6_OPCODE_WEN << (param->cmd_bitlen - EE93CX6_OPCODE_WEN_BITLEN);
	interfaces->microwire.transport(ifs->mw_port, cmd, param->cmd_bitlen, 0, 0, 
									0, 0, NULL, 0);
	
	return VSFERR_NONE;
}

static vsf_err_t ee93cx6_drv_fini(struct dal_info_t *info)
{
	struct ee93cx6_drv_interface_t *ifs = 
								(struct ee93cx6_drv_interface_t *)info->ifs;
	struct ee93cx6_drv_param_t *param = 
								(struct ee93cx6_drv_param_t *)info->param;
	uint32_t cmd;
	
	cmd = EE93CX6_OPCODE_WDS << (param->cmd_bitlen - EE93CX6_OPCODE_WDS_BITLEN);
	interfaces->microwire.transport(ifs->mw_port, cmd, param->cmd_bitlen, 0, 0, 
									0, 0, NULL, 0);
	
	interfaces->microwire.fini(ifs->mw_port);
	return interfaces->peripheral_commit();
}

static vsf_err_t ee93cx6_drv_eraseall_nb_start(struct dal_info_t *info)
{
	struct ee93cx6_drv_interface_t *ifs = 
								(struct ee93cx6_drv_interface_t *)info->ifs;
	struct ee93cx6_drv_param_t *param = 
								(struct ee93cx6_drv_param_t *)info->param;
	uint32_t cmd;
	
	cmd = EE93CX6_OPCODE_ERAL << 
							(param->cmd_bitlen - EE93CX6_OPCODE_ERAL_BITLEN);
	return interfaces->microwire.transport(ifs->mw_port, cmd, param->cmd_bitlen,
											0, 0, 0, 0, NULL, 0);
}

static vsf_err_t ee93cx6_drv_eraseall_waitready(struct dal_info_t *info)
{
	struct ee93cx6_drv_interface_t *ifs = 
								(struct ee93cx6_drv_interface_t *)info->ifs;
	
	return ee93cx6_drv_poll(ifs);
}

static vsf_err_t ee93cx6_drv_eraseall_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return VSFERR_NONE;
}

static vsf_err_t ee93cx6_drv_eraseblock_nb_start(struct dal_info_t *info, 
											uint64_t address, uint64_t count)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	return VSFERR_NONE;
}

static vsf_err_t ee93cx6_drv_eraseblock_nb(struct dal_info_t *info, 
										uint64_t address)
{
	struct ee93cx6_drv_interface_t *ifs = 
								(struct ee93cx6_drv_interface_t *)info->ifs;
	struct ee93cx6_drv_param_t *param = 
								(struct ee93cx6_drv_param_t *)info->param;
	
	switch (param->origination_mode)
	{
	case EE93CX6_ORIGINATION_BYTE:
		return interfaces->microwire.transport(ifs->mw_port, 
			EE93CX6_OPCODE_ERASE, EE93CX6_OPCODE_ERASE_BITLEN, 
			(uint32_t)address, param->addr_bitlen, 0, 0, NULL, 0);
		break;
	case EE93CX6_ORIGINATION_WORD:
		return interfaces->microwire.transport(ifs->mw_port, 
			EE93CX6_OPCODE_ERASE, EE93CX6_OPCODE_ERASE_BITLEN, 
			(uint32_t)(address / 2), param->addr_bitlen - 1, 0, 0, 
			NULL, 0);
		break;
	default:
		return VSFERR_INVALID_PARAMETER;
	}
}

static vsf_err_t ee93cx6_drv_eraseblock_waitready(struct dal_info_t *info, 
													uint64_t address)
{
	struct ee93cx6_drv_interface_t *ifs = 
								(struct ee93cx6_drv_interface_t *)info->ifs;
	
	REFERENCE_PARAMETER(address);
	return ee93cx6_drv_poll(ifs);
}

static vsf_err_t ee93cx6_drv_eraseblock_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return VSFERR_NONE;
}

static vsf_err_t ee93cx6_drv_readblock_nb_start(struct dal_info_t *info, 
								uint64_t address, uint64_t count, uint8_t *buff)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	REFERENCE_PARAMETER(buff);
	return VSFERR_NONE;
}

static vsf_err_t ee93cx6_drv_readblock_nb(struct dal_info_t *info, 
										uint64_t address, uint8_t *buff)
{
	struct ee93cx6_drv_interface_t *ifs = 
								(struct ee93cx6_drv_interface_t *)info->ifs;
	struct ee93cx6_drv_param_t *param = 
								(struct ee93cx6_drv_param_t *)info->param;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	uint8_t i;
	
	switch (param->origination_mode)
	{
	case EE93CX6_ORIGINATION_BYTE:
		for (i = 0; i < mal_info->capacity.block_size; i++)
		{
			if (interfaces->microwire.transport(ifs->mw_port, 
					EE93CX6_OPCODE_READ, EE93CX6_OPCODE_READ_BITLEN, 
					(uint32_t)(address + i), param->addr_bitlen, 
					0, 0, &buff[i], 8))
			{
				return VSFERR_FAIL;
			}
		}
		break;
	case EE93CX6_ORIGINATION_WORD:
		for (i = 0; i < mal_info->capacity.block_size; i += 2)
		{
			if (interfaces->microwire.transport(ifs->mw_port, 
					EE93CX6_OPCODE_READ, EE93CX6_OPCODE_READ_BITLEN, 
					(uint32_t)((address + i) / 2), param->addr_bitlen - 1, 
					0, 0, &buff[i], 16))
			{
				return VSFERR_FAIL;
			}
		}
		break;
	default:
		return VSFERR_INVALID_PARAMETER;
	}
	
	return VSFERR_NONE;
}

static vsf_err_t ee93cx6_drv_readblock_waitready(struct dal_info_t *info, 
												uint64_t address, uint8_t *buff)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	return VSFERR_NONE;
}

static vsf_err_t ee93cx6_drv_readblock_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return VSFERR_NONE;
}

static vsf_err_t ee93cx6_drv_writeblock_nb_start(struct dal_info_t *info, 
								uint64_t address, uint64_t count, uint8_t *buff)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	REFERENCE_PARAMETER(buff);
	return VSFERR_NONE;
}

static vsf_err_t ee93cx6_drv_writeblock_nb(struct dal_info_t *info, 
										uint64_t address, uint8_t *buff)
{
	struct ee93cx6_drv_interface_t *ifs = 
								(struct ee93cx6_drv_interface_t *)info->ifs;
	struct ee93cx6_drv_param_t *param = 
								(struct ee93cx6_drv_param_t *)info->param;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	uint8_t i;
	uint16_t *ptr16;
	
	switch (param->origination_mode)
	{
	case EE93CX6_ORIGINATION_BYTE:
		for (i = 0; i < mal_info->capacity.block_size; i++)
		{
			if (interfaces->microwire.transport(ifs->mw_port, 
					EE93CX6_OPCODE_WRITE, EE93CX6_OPCODE_WRITE_BITLEN, 
					(uint32_t)(address + i), param->addr_bitlen, 
					buff[i], 8, NULL, 0))
			{
				return VSFERR_FAIL;
			}
		}
		break;
	case EE93CX6_ORIGINATION_WORD:
		ptr16 = (uint16_t *)buff;
		for (i = 0; i < mal_info->capacity.block_size; i += 2)
		{
			if (interfaces->microwire.transport(ifs->mw_port, 
					EE93CX6_OPCODE_WRITE, EE93CX6_OPCODE_WRITE_BITLEN, 
					(uint32_t)((address + i) / 2), param->addr_bitlen - 1, 
					SYS_TO_LE_U16(ptr16[i / 2]), 16, NULL, 0))
			{
				return VSFERR_FAIL;
			}
		}
		break;
	default:
		return VSFERR_INVALID_PARAMETER;
	}
	
	return VSFERR_NONE;
}

static vsf_err_t ee93cx6_drv_writeblock_waitready(struct dal_info_t *info, 
												uint64_t address, uint8_t *buff)
{
	struct ee93cx6_drv_interface_t *ifs = 
								(struct ee93cx6_drv_interface_t *)info->ifs;
	
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	return ee93cx6_drv_poll(ifs);
}

static vsf_err_t ee93cx6_drv_writeblock_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return VSFERR_NONE;
}

#if DAL_INTERFACE_PARSER_EN
static vsf_err_t ee93cx6_drv_parse_interface(struct dal_info_t *info, 
											uint8_t *buff)
{
	struct ee93cx6_drv_interface_t *ifs = 
								(struct ee93cx6_drv_interface_t *)info->ifs;
	
	ifs->mw_port = buff[0];
	return VSFERR_NONE;
}
#endif

const struct mal_driver_t ee93cx6_drv = 
{
	{
		"ee93cx6",
#if DAL_INTERFACE_PARSER_EN
		"microwire:%1d",
		ee93cx6_drv_parse_interface,
#endif
	},
	
	MAL_SUPPORT_ERASEALL | MAL_SUPPORT_ERASEBLOCK | 
	MAL_SUPPORT_WRITEBLOCK | MAL_SUPPORT_READBLOCK,
	
	ee93cx6_drv_init_nb,
	NULL,
	ee93cx6_drv_fini,
	NULL,
	NULL,
	
	NULL, NULL, NULL, NULL,
	
	ee93cx6_drv_eraseall_nb_start,
	NULL,
	ee93cx6_drv_eraseall_waitready,
	ee93cx6_drv_eraseall_nb_end,
	
	ee93cx6_drv_eraseblock_nb_start,
	ee93cx6_drv_eraseblock_nb,
	NULL,
	ee93cx6_drv_eraseblock_waitready,
	ee93cx6_drv_eraseblock_nb_end,
	
	ee93cx6_drv_readblock_nb_start,
	ee93cx6_drv_readblock_nb,
	NULL,
	ee93cx6_drv_readblock_waitready,
	ee93cx6_drv_readblock_nb_end,
	
	ee93cx6_drv_writeblock_nb_start,
	ee93cx6_drv_writeblock_nb,
	NULL,
	ee93cx6_drv_writeblock_waitready,
	ee93cx6_drv_writeblock_nb_end
};

#endif
