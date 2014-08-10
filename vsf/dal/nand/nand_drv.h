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

#ifndef __NAND_DRV_H_INCLUDED__
#define __NAND_DRV_H_INCLUDED__

struct nand_drv_param_t
{
	struct ebi_nand_info_t nand_info;
	bool block_read_en;
	uint8_t col_addr_size;
	uint8_t col_addr_msb;
	uint8_t row_addr_lsb;
	uint8_t row_addr_size;
	// private
	bool addr_loadded;
};

struct nand_drv_info_t
{
	uint8_t manufacturer_id;
	uint16_t device_id[3];
};

struct nand_drv_interface_t
{
	uint8_t ebi_port;
	uint8_t nand_index;
};

extern const struct mal_driver_t nand_drv;

#endif /* __NAND_DRV_H_INCLUDED__ */

