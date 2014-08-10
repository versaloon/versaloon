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

#ifndef __S6B0724_DRV_H_INCLUDED__
#define __S6B0724_DRV_H_INCLUDED__

struct s6b0724_drv_param_t
{
	struct ebi_sram_psram_nor_info_t nor_info;
};

struct s6b0724_drv_interface_t
{
	uint8_t ebi_port;
	uint8_t lcd_index;
	
	uint32_t cmd_addr;
	uint32_t data_addr;
};

extern const struct mal_driver_t s6b0724_drv;

#endif	// __S6B0724_DRV_H_INCLUDED__

