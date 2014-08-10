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

#ifndef __SD_SPI_DRV_H_INCLUDED__
#define __SD_SPI_DRV_H_INCLUDED__

struct sd_spi_drv_interface_t
{
	uint8_t cs_port;
	uint32_t cs_pin;
	uint8_t spi_port;
};

struct sd_spi_drv_info_t
{
	enum sd_spi_drv_state_t
	{
		SD_SPI_DRV_WAITRESP,
		SD_SPI_DRV_WAITBUSY,
		SD_SPI_DRV_WAITDATATOK,
		SD_SPI_DRV_WAITDATALASTBUSY,
	} state;
	uint32_t retry;
	uint64_t cur_block;
	uint64_t total_block;
};

extern const struct mal_driver_t sd_spi_drv;

#endif /*  __SD_SPI_DRV_H_INCLUDED__ */

