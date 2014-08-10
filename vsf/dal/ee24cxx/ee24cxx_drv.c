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

#if DAL_EE24CXX_EN

#include "ee24cxx_drv_cfg.h"
#include "ee24cxx_drv.h"

static vsf_err_t ee24cxx_drv_init_nb(struct dal_info_t *info)
{
	struct ee24cxx_drv_interface_t *ifs = 
								(struct ee24cxx_drv_interface_t *)info->ifs;
	struct ee24cxx_drv_param_t *param = 
								(struct ee24cxx_drv_param_t *)info->param;
	
	if (!param->iic_khz)
	{
		param->iic_khz = 100;
	}
	interfaces->i2c.init(ifs->iic_port);
	interfaces->i2c.config(ifs->iic_port, param->iic_khz, 0, 10000);
	
	return VSFERR_NONE;
}

static vsf_err_t ee24cxx_drv_fini(struct dal_info_t *info)
{
	struct ee24cxx_drv_interface_t *ifs = 
								(struct ee24cxx_drv_interface_t *)info->ifs;
	
	interfaces->i2c.fini(ifs->iic_port);
	return interfaces->peripheral_commit();
}

static vsf_err_t ee24cxx_drv_readblock_nb_start(struct dal_info_t *info, 
								uint64_t address, uint64_t count, uint8_t *buff)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	REFERENCE_PARAMETER(buff);
	return VSFERR_NONE;
}

static vsf_err_t ee24cxx_drv_readblock_nb(struct dal_info_t *info, 
										uint64_t address, uint8_t *buff)
{
	struct ee24cxx_drv_interface_t *ifs = 
								(struct ee24cxx_drv_interface_t *)info->ifs;
	struct ee24cxx_drv_param_t *param = 
								(struct ee24cxx_drv_param_t *)info->param;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	uint16_t addr_word = (uint16_t)address;
	
	addr_word = SYS_TO_BE_U16(addr_word);
	if (interfaces->i2c.write(ifs->iic_port, param->iic_addr, 
								(uint8_t *)&addr_word, 2, 0) || 
		interfaces->i2c.read(ifs->iic_port, param->iic_addr, buff, 
							(uint16_t)mal_info->capacity.block_size, 1, true))
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t ee24cxx_drv_readblock_nb_isready(struct dal_info_t *info, 
												uint64_t address, uint8_t *buff)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	return VSFERR_NONE;
}

static vsf_err_t ee24cxx_drv_readblock_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return VSFERR_NONE;
}

static vsf_err_t ee24cxx_drv_writeblock_nb_start(struct dal_info_t *info, 
								uint64_t address, uint64_t count, uint8_t *buff)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	REFERENCE_PARAMETER(buff);
	return VSFERR_NONE;
}

static vsf_err_t ee24cxx_drv_writeblock_nb(struct dal_info_t *info, 
										uint64_t address, uint8_t *buff)
{
	struct ee24cxx_drv_interface_t *ifs = 
								(struct ee24cxx_drv_interface_t *)info->ifs;
	struct ee24cxx_drv_param_t *param = 
								(struct ee24cxx_drv_param_t *)info->param;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	uint8_t buffe_tmp[32 + 2];
	
	if (mal_info->capacity.block_size != 32)
	{
		return VSFERR_INVALID_PARAMETER;
	}
	
	SET_BE_U16(&buffe_tmp[0], (uint16_t)address);
	memcpy(&buffe_tmp[2], buff, 32);
	
	if (interfaces->i2c.write(ifs->iic_port, param->iic_addr, buffe_tmp,
								2 + 32, 1) || 
		interfaces->delay.delayms(5))
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t ee24cxx_drv_writeblock_nb_isready(struct dal_info_t *info, 
												uint64_t address, uint8_t *buff)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	return VSFERR_NONE;
}

static vsf_err_t ee24cxx_drv_writeblock_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return VSFERR_NONE;
}

#if DAL_INTERFACE_PARSER_EN
static vsf_err_t ee24cxx_drv_parse_interface(struct dal_info_t *info, 
											uint8_t *buff)
{
	struct ee24cxx_drv_interface_t *ifs = 
								(struct ee24cxx_drv_interface_t *)info->ifs;
	
	ifs->iic_port = buff[0];
	return VSFERR_NONE;
}
#endif

const struct mal_driver_t ee24cxx_drv = 
{
	{
		"ee24cxx",
#if DAL_INTERFACE_PARSER_EN
		"iic:%1d",
		ee24cxx_drv_parse_interface,
#endif
	},
	
	MAL_SUPPORT_READBLOCK | MAL_SUPPORT_WRITEBLOCK,
	
	ee24cxx_drv_init_nb,
	NULL,
	ee24cxx_drv_fini,
	NULL,
	NULL,
	
	NULL, NULL, NULL, NULL,
	
	NULL,
	NULL,
	NULL,
	NULL,
	
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	
	ee24cxx_drv_readblock_nb_start,
	ee24cxx_drv_readblock_nb,
	ee24cxx_drv_readblock_nb_isready,
	NULL,
	ee24cxx_drv_readblock_nb_end,
	
	ee24cxx_drv_writeblock_nb_start,
	ee24cxx_drv_writeblock_nb,
	ee24cxx_drv_writeblock_nb_isready,
	NULL,
	ee24cxx_drv_writeblock_nb_end
};

#endif
