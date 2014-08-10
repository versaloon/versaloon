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

#ifndef __SST32HFXX_DRV_H_INCLUDED__
#define __SST32HFXX_DRV_H_INCLUDED__

struct sst32hfxx_drv_param_t
{
	struct ebi_sram_psram_nor_info_t nor_info;
	uint32_t base_addr;
	uint16_t delayus;
};

struct sst32hfxx_drv_info_t
{
	uint8_t manufacturer_id;
	uint16_t device_id;
};

struct sst32hfxx_drv_interface_t
{
	uint8_t ebi_port;
	uint8_t nor_index;
};

extern const struct mal_driver_t sst32hfxx_nor_drv;

#endif /* __SST32HFXX_DRV_H_INCLUDED__ */

