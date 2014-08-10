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

#if DAL_DF25XX_EN

#include "df25xx_drv_cfg.h"
#include "df25xx_drv.h"

#define DF25XX_CMD_WRSR						0x01	//write status register 
#define DF25XX_CMD_PGWR						0x02	//page program
#define DF25XX_CMD_PGRD						0x03	//read data
#define DF25XX_CMD_WRDI						0x04	//write disable
#define DF25XX_CMD_RDSR						0x05	//read status register 1(7 to 0)
#define DF25XX_CMD_WREN						0x06	//write enable
#define DF25XX_CMD_ER4K						0x20	//sector erase
#define DF25XX_CMD_CHER						0x60	//chip erase
#define DF25XX_CMD_RDID						0x9F	//jedec id

#define DF25XX_STATE_BUSY					(1 << 0)
#define DF25XX_STATE_WEL					(1 << 1)

static vsf_err_t df25xx_drv_cs_assert(struct df25xx_drv_interface_t *ifs)
{
	if (ifs->cs_port != IFS_DUMMY_PORT)
	{
		return interfaces->gpio.clear(ifs->cs_port, ifs->cs_pin);
	}
	else
	{
		return interfaces->spi.select(ifs->spi_port, (uint8_t)ifs->cs_pin);
	}
}

static vsf_err_t df25xx_drv_cs_deassert(struct df25xx_drv_interface_t *ifs)
{
	if (ifs->cs_port != IFS_DUMMY_PORT)
	{
		interfaces->gpio.set(ifs->cs_port, ifs->cs_pin);
	}
	else
	{
		interfaces->spi.deselect(ifs->spi_port, (uint8_t)ifs->cs_pin);
	}
	// if the cs port is OD with pull-up, need sometime for the signal to rise
	return interfaces->delay.delayus(20);
}

static vsf_err_t df25xx_drv_init_nb(struct dal_info_t *info)
{
	struct df25xx_drv_interface_t *ifs = 
								(struct df25xx_drv_interface_t *)info->ifs;
	struct df25xx_drv_param_t *df25xx_drv_param = 
								(struct df25xx_drv_param_t *)info->param;
	
	if (!df25xx_drv_param->spi_khz)
	{
		df25xx_drv_param->spi_khz = 9000;
	}
	if (ifs->cs_port != IFS_DUMMY_PORT)
	{
		interfaces->gpio.init(ifs->cs_port);
		interfaces->gpio.config(ifs->cs_port, ifs->cs_pin, ifs->cs_pin,
								ifs->cs_pin, ifs->cs_pin);
	}
	interfaces->spi.init(ifs->spi_port);
	return interfaces->spi.config(ifs->spi_port, df25xx_drv_param->spi_khz, 
									SPI_MODE3 | SPI_MSB_FIRST | SPI_MASTER);
}

static vsf_err_t df25xx_drv_getinfo(struct dal_info_t *info)
{
	struct df25xx_drv_interface_t *ifs = 
								(struct df25xx_drv_interface_t *)info->ifs;
	struct df25xx_drv_info_t *pinfo = (struct df25xx_drv_info_t *)info->info;
	uint8_t cmd[4];
	
	df25xx_drv_cs_assert(ifs);
	cmd[0] = DF25XX_CMD_RDID;
	interfaces->spi.io(ifs->spi_port, cmd, cmd, 4);
	df25xx_drv_cs_deassert(ifs);
	if (interfaces->peripheral_commit())
	{
		return VSFERR_FAIL;
	}
	
	pinfo->manufacturer_id = cmd[1];
	pinfo->device_id = GET_BE_U16(&cmd[2]);
	return VSFERR_NONE;
}

static vsf_err_t df25xx_drv_fini(struct dal_info_t *info)
{
	struct df25xx_drv_interface_t *ifs = 
								(struct df25xx_drv_interface_t *)info->ifs;
	
	if (ifs->cs_port != IFS_DUMMY_PORT)
	{
		interfaces->gpio.config(ifs->cs_port, ifs->cs_pin, 0, 0, 0);
		interfaces->gpio.fini(ifs->cs_port);
	}
	return interfaces->spi.fini(ifs->spi_port);
}

static vsf_err_t df25xx_drv_eraseall_nb_start(struct dal_info_t *info)
{
	struct df25xx_drv_interface_t *ifs = 
								(struct df25xx_drv_interface_t *)info->ifs;
	uint8_t cmd[2];
	
	df25xx_drv_cs_assert(ifs);
	cmd[0] = DF25XX_CMD_WREN;
	interfaces->spi.io(ifs->spi_port, cmd, NULL, 1);
	df25xx_drv_cs_deassert(ifs);
	
	df25xx_drv_cs_assert(ifs);
	cmd[0] = DF25XX_CMD_CHER;
	interfaces->spi.io(ifs->spi_port, cmd, NULL, 1);
	return df25xx_drv_cs_deassert(ifs);
}

static vsf_err_t df25xx_drv_eraseall_nb_isready(struct dal_info_t *info)
{
	struct df25xx_drv_interface_t *ifs = 
								(struct df25xx_drv_interface_t *)info->ifs;
	uint8_t cmd[2];
	
	df25xx_drv_cs_assert(ifs);
	cmd[0] = DF25XX_CMD_RDSR;
	interfaces->spi.io(ifs->spi_port, cmd, cmd, 2);
	df25xx_drv_cs_deassert(ifs);
	
	if (interfaces->peripheral_commit())
	{
		return VSFERR_FAIL;
	}
	
	return ((cmd[1] & DF25XX_STATE_BUSY) == 0) ? VSFERR_NONE: VSFERR_NOT_READY;
}

static vsf_err_t df25xx_drv_eraseall_nb_end(struct dal_info_t *info)
{
	struct df25xx_drv_interface_t *ifs = 
								(struct df25xx_drv_interface_t *)info->ifs;
	uint8_t cmd[1];
	
	df25xx_drv_cs_assert(ifs);
	cmd[0] = DF25XX_CMD_WRDI;
	interfaces->spi.io(ifs->spi_port, cmd, NULL, 1);
	return df25xx_drv_cs_deassert(ifs);
}

static vsf_err_t df25xx_drv_eraseblock_nb_start(struct dal_info_t *info, 
											uint64_t address, uint64_t count)
{
	struct df25xx_drv_interface_t *ifs = 
								(struct df25xx_drv_interface_t *)info->ifs;
	uint8_t cmd[1];
	
	REFERENCE_PARAMETER(count);
	REFERENCE_PARAMETER(address);
	
	df25xx_drv_cs_assert(ifs);
	cmd[0] = DF25XX_CMD_WREN;
	interfaces->spi.io(ifs->spi_port, cmd, NULL, 1);
	return df25xx_drv_cs_deassert(ifs);
}

static vsf_err_t df25xx_drv_eraseblock_nb(struct dal_info_t *info, 
											uint64_t address)
{
	struct df25xx_drv_interface_t *ifs = 
								(struct df25xx_drv_interface_t *)info->ifs;
	uint8_t cmd[4];
	
	df25xx_drv_cs_assert(ifs);
	cmd[0] = DF25XX_CMD_ER4K;
	cmd[1] = (address >> 16) & 0xFF;
	cmd[2] = (address >> 8 ) & 0xFF;
	cmd[3] = (address >> 0 ) & 0xFF;
	interfaces->spi.io(ifs->spi_port, cmd, NULL, 4);
	return df25xx_drv_cs_deassert(ifs);
}

static vsf_err_t df25xx_drv_eraseblock_nb_isready(struct dal_info_t *info, 
													uint64_t address)
{
	struct df25xx_drv_interface_t *ifs = 
								(struct df25xx_drv_interface_t *)info->ifs;
	uint8_t cmd[2];
	
	REFERENCE_PARAMETER(address);
	
	df25xx_drv_cs_assert(ifs);
	cmd[0] = DF25XX_CMD_RDSR;
	interfaces->spi.io(ifs->spi_port, cmd, cmd, 2);
	df25xx_drv_cs_deassert(ifs);
	
	if (interfaces->peripheral_commit())
	{
		return VSFERR_FAIL;
	}
	
	return ((cmd[1] & DF25XX_STATE_BUSY) == 0) ? VSFERR_NONE: VSFERR_NOT_READY;
}

static vsf_err_t df25xx_drv_eraseblock_nb_end(struct dal_info_t *info)
{
	struct df25xx_drv_interface_t *ifs = 
								(struct df25xx_drv_interface_t *)info->ifs;
	uint8_t cmd[1];
	
	df25xx_drv_cs_assert(ifs);
	cmd[0] = DF25XX_CMD_WRDI;
	interfaces->spi.io(ifs->spi_port, cmd, NULL, 1);
	return df25xx_drv_cs_deassert(ifs);
}

static vsf_err_t df25xx_drv_readblock_nb_start(struct dal_info_t *info, 
								uint64_t address, uint64_t count, uint8_t *buff)
{
	struct df25xx_drv_interface_t *ifs = 
								(struct df25xx_drv_interface_t *)info->ifs;
	uint8_t cmd[4];
	
	REFERENCE_PARAMETER(count);
	REFERENCE_PARAMETER(buff);
	
	df25xx_drv_cs_assert(ifs);
	cmd[0] = DF25XX_CMD_PGRD;
	cmd[1] = (address >> 16) & 0xFF;
	cmd[2] = (address >> 8 ) & 0xFF;
	cmd[3] = (address >> 0 ) & 0xFF;
	return interfaces->spi.io(ifs->spi_port, cmd, NULL, 4);
}

static vsf_err_t df25xx_drv_readblock_nb(struct dal_info_t *info, 
											uint64_t address, uint8_t *buff)
{
	struct df25xx_drv_interface_t *ifs = 
								(struct df25xx_drv_interface_t *)info->ifs;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	
	REFERENCE_PARAMETER(address);
	return interfaces->spi.io(ifs->spi_port, NULL, buff, 
								(uint16_t)mal_info->read_page_size);
}

static vsf_err_t df25xx_drv_readblock_nb_isready(struct dal_info_t *info, 
												uint64_t address, uint8_t *buff)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	return VSFERR_NONE;
}

static vsf_err_t df25xx_drv_readblock_nb_end(struct dal_info_t *info)
{
	struct df25xx_drv_interface_t *ifs = 
								(struct df25xx_drv_interface_t *)info->ifs;
	
	return df25xx_drv_cs_deassert(ifs);
}

static vsf_err_t df25xx_drv_writeblock_nb_start(struct dal_info_t *info, 
								uint64_t address, uint64_t count, uint8_t *buff)
{
	struct df25xx_drv_interface_t *ifs = 
								(struct df25xx_drv_interface_t *)info->ifs;
	uint8_t cmd[1];
	
	REFERENCE_PARAMETER(count);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	
	df25xx_drv_cs_assert(ifs);
	cmd[0] = DF25XX_CMD_WREN;
	interfaces->spi.io(ifs->spi_port, cmd, NULL, 1);
	return df25xx_drv_cs_deassert(ifs);
}

static vsf_err_t df25xx_drv_writeblock_nb(struct dal_info_t *info, 
											uint64_t address, uint8_t *buff)
{
	struct df25xx_drv_interface_t *ifs = 
								(struct df25xx_drv_interface_t *)info->ifs;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	uint8_t cmd[4];
	
	df25xx_drv_cs_assert(ifs);
	cmd[0] = DF25XX_CMD_PGWR;
	cmd[1] = (address >> 16) & 0xFF;
	cmd[2] = (address >> 8 ) & 0xFF;
	cmd[3] = (address >> 0 ) & 0xFF;
	interfaces->spi.io(ifs->spi_port, cmd, NULL, 4);
	interfaces->spi.io(ifs->spi_port, buff, NULL, 
						(uint16_t)mal_info->write_page_size);
	return df25xx_drv_cs_deassert(ifs);
}

static vsf_err_t df25xx_drv_writeblock_nb_isready(struct dal_info_t *info, 
												uint64_t address, uint8_t *buff)
{
	struct df25xx_drv_interface_t *ifs = 
								(struct df25xx_drv_interface_t *)info->ifs;
	uint8_t cmd[2];
	
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	
	df25xx_drv_cs_assert(ifs);
	cmd[0] = DF25XX_CMD_RDSR;
	interfaces->spi.io(ifs->spi_port, cmd, cmd, 2);
	df25xx_drv_cs_deassert(ifs);
	
	if (interfaces->peripheral_commit())
	{
		return VSFERR_FAIL;
	}
	
	return ((cmd[1] & DF25XX_STATE_BUSY) == 0) ? VSFERR_NONE: VSFERR_NOT_READY;
}

static vsf_err_t df25xx_drv_writeblock_nb_end(struct dal_info_t *info)
{
	struct df25xx_drv_interface_t *ifs = 
								(struct df25xx_drv_interface_t *)info->ifs;
	uint8_t cmd[1];
	
	df25xx_drv_cs_assert(ifs);
	cmd[0] = DF25XX_CMD_WRDI;
	interfaces->spi.io(ifs->spi_port, cmd, NULL, 1);
	return df25xx_drv_cs_deassert(ifs);
}

#if DAL_INTERFACE_PARSER_EN
static vsf_err_t df25xx_drv_parse_interface(struct dal_info_t *info,
											uint8_t *buff)
{
	struct df25xx_drv_interface_t *ifs = 
									(struct df25xx_drv_interface_t *)info->ifs;
	
	ifs->spi_port = buff[0];
	ifs->cs_port = buff[1];
	ifs->cs_pin = *(uint32_t *)&buff[2];
	return VSFERR_NONE;
}
#endif

const struct mal_driver_t df25xx_drv = 
{
	{
		"df25xx",
#if DAL_INTERFACE_PARSER_EN
		"spi:%1dcs:%1d,%4x",
		df25xx_drv_parse_interface,
#endif
	},
	
	MAL_SUPPORT_READBLOCK | MAL_SUPPORT_WRITEBLOCK | MAL_SUPPORT_ERASEBLOCK,
	
	df25xx_drv_init_nb,
	NULL,
	df25xx_drv_fini,
	df25xx_drv_getinfo,
	NULL,
	
	NULL, NULL, NULL, NULL,
	
	df25xx_drv_eraseall_nb_start,
	df25xx_drv_eraseall_nb_isready,
	NULL,
	df25xx_drv_eraseall_nb_end,
	
	df25xx_drv_eraseblock_nb_start,
	df25xx_drv_eraseblock_nb,
	df25xx_drv_eraseblock_nb_isready,
	NULL,
	df25xx_drv_eraseblock_nb_end,
	
	df25xx_drv_readblock_nb_start,
	df25xx_drv_readblock_nb,
	df25xx_drv_readblock_nb_isready,
	NULL,
	df25xx_drv_readblock_nb_end,
	
	df25xx_drv_writeblock_nb_start,
	df25xx_drv_writeblock_nb,
	df25xx_drv_writeblock_nb_isready,
	NULL,
	df25xx_drv_writeblock_nb_end
};

#endif
