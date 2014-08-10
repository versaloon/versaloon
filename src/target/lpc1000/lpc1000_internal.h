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

#ifndef __LPC1000_INTERNAL_H_INCLUDED__
#define __LPC1000_INTERNAL_H_INCLUDED__

#define LPC1000_FLASH_PAGESIZE				256
#define LPC1000_FLASH_ADDR					0x00000000
#define LPC1000_SRAM_ADDR					0x10000000
#define LPC1000_FLASH_DEFAULT				0xFF

#define LPC1000_JTAG						0
#define LPC1000_SWD							1
#define LPC1000_ISP							2

#define LPC1000_IAP_ENTRY					0x1FFF1FF1

#define LPC1000_IAPCMD_PREPARE_SECTOR		50
#define LPC1000_IAPCMD_RAM_TO_FLASH			51
#define LPC1000_IAPCMD_ERASE_SECTOR			52
#define LPC1000_IAPCMD_BLANK_CHECK			53
#define LPC1000_IAPCMD_READ_ID				54
#define LPC1000_IAPCMD_READ_BOOTVER			55
#define LPC1000_IAPCMD_COMPARE				56
#define LPC1000_IAPCMD_REINVOKE				57
#define LPC1000_IAPCMD_READ_SERIAL			58

#define LPC1000_PARAM_IRC_KHZ				0
#define LPC1000_PARAM_SYSMEMREMAP_ADDR		1
#define LPC1000_PARAM_FLASHREMAP_VALUE		2

uint8_t lpc1000_get_sector_idx_by_addr(struct program_context_t *context,
										uint32_t addr);

#endif /* __LPC1000_INTERNAL_H_INCLUDED__ */

