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

#ifndef __AT91SAM3_INTERNAL_H_INCLUDED__
#define __AT91SAM3_INTERNAL_H_INCLUDED__

#define AT91SAM3_FLASH_PAGESIZE				256
#define AT91SAM3_FLASH_ADDR					0x00400000
#define AT91SAM3_SRAM_ADDR					0x20000000
#define AT91SAM3_FLASH_DEFAULT				0xFF

#define AT91SAM3_CHIPID_CIDR				0x400E0740
#define AT91SAM3_GET_ARCH(id)				(((id) & 0x0FF00000) >> 20)
#define AT91SAM3_ARCH_ATSAM3UxC				0x80
#define AT91SAM3_ARCH_ATSAM3UxE				0x81
#define AT91SAM3_ARCH_ATSAM3AxC				0x83
#define AT91SAM3_ARCH_ATSAM3XxC				0x84
#define AT91SAM3_ARCH_ATSAMXUxE				0x85
#define AT91SAM3_ARCH_ATSAM3XxG				0x86
#define AT91SAM3_ARCH_ATSAM3SxA				0x88
#define AT91SAM3_ARCH_ATSAM3SxB				0x89
#define AT91SAM3_ARCH_ATSAM3SxC				0x8A

#define AT91SAM3_EEFC_FKEY					0x5A000000
#define AT91SAM3_EEFC_FARG(arg)				((arg) << 8)

#define AT91SAM3_EEFC_FCR_OFFSET			0x04
#define AT91SAM3_EEFC_FSR_OFFSET			0x08
#define AT91SAM3_EEFC_FRR_OFFSET			0x0C

#define AT91SAM3_EEFC_CMD_GETD				0x0
#define AT91SAM3_EEFC_CMD_WP				0x1
#define AT91SAM3_EEFC_CMD_WPL				0x2
#define AT91SAM3_EEFC_CMD_EWP				0x3
#define AT91SAM3_EEFC_CMD_EWPL				0x4
#define AT91SAM3_EEFC_CMD_EA				0x5
#define AT91SAM3_EEFC_CMD_SLB				0x8
#define AT91SAM3_EEFC_CMD_CLB				0x9
#define AT91SAM3_EEFC_CMD_GLB				0xA
#define AT91SAM3_EEFC_CMD_SGPB				0xB
#define AT91SAM3_EEFC_CMD_CGPB				0xC
#define AT91SAM3_EEFC_CMD_GGPB				0xD
#define AT91SAM3_EEFC_CMD_STUI				0xE
#define AT91SAM3_EEFC_CMD_SPUI				0xF


#define AT91SAM3_EEFC_CMD_GETD				0x0

#define AT91SAM3_JTAG						0
#define AT91SAM3_SWD						1

#define AT91SAM3_PARAM_PLANE_NUMBER			0
#define AT91SAM3_PARAM_PLANE0_CONTROL		1
#define AT91SAM3_PARAM_PLANE1_CONTROL		2

extern uint8_t at91sam3_wait_state;

#endif /* __AT91SAM3_INTERNAL_H_INCLUDED__ */

