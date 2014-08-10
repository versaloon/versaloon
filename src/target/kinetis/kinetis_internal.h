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

#ifndef __KINETIS_INTERNAL_H_INCLUDED__
#define __KINETIS_INTERNAL_H_INCLUDED__

#define KINETIS_JTAG				0
#define KINETIS_SWD					1

#define KINETIS_SIM_SDID			0x40048024
#define KINETIS_SIM_FCFG1			0x4004804C

#define KINETIS_SIM_UID				0x40048058

#define KINETIS_SERIESID_KL			0x01

#define KINETIS_FTF_BASE			0x40020000
#define KINETIS_FTF_CMD_ERSALL		(0x44 << 24)

uint32_t kinetis_get_sram_size(uint32_t mcuid);
uint32_t kinetis_get_flash_size(uint32_t fcfg1);
void kinetis_print_device(uint32_t mcuid, uint32_t fcfg1);

#endif /* __KINETIS_INTERNAL_H_INCLUDED__ */

