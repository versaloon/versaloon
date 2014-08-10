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

#ifndef __LM3S_INTERNAL_H_INCLUDED__
#define __LM3S_INTERNAL_H_INCLUDED__

#define LM3S_JTAG						0
#define LM3S_SWD						1

#define LM3S_SRAM_ADDR					0x20000000

#define LM3S_SYSCTL_BASE				0x400FE000
#define LM3S_SYSCTL_DID0				(LM3S_SYSCTL_BASE + 0x000)
#define	LM3S_SYSCTL_DID1				(LM3S_SYSCTL_BASE + 0x004)
#define LM3S_SYSCTL_DC0					(LM3S_SYSCTL_BASE + 0x008)
#define LM3S_SYSCTL_DC1					(LM3S_SYSCTL_BASE + 0x010)
#define LM3S_SYSCTL_DC2					(LM3S_SYSCTL_BASE + 0x014)
#define LM3S_SYSCTL_DC3					(LM3S_SYSCTL_BASE + 0x018)
#define LM3S_SYSCTL_FMPRE				(LM3S_SYSCTL_BASE + 0x130)
#define LM3S_SYSCTL_FMPPE				(LM3S_SYSCTL_BASE + 0x134)
#define LM3S_SYSCTL_USECRL				(LM3S_SYSCTL_BASE + 0x140)

#define LM3S_FLASHCTL_BASE				0x400FD000
#define LM3S_FLASHCTL_FMA				(LM3S_FLASHCTL_BASE + 0x000)
#define LM3S_FLASHCTL_FMD				(LM3S_FLASHCTL_BASE + 0x004)
#define LM3S_FLASHCTL_FMC				(LM3S_FLASHCTL_BASE + 0x008)
#define LM3S_FLASHCTL_FCRIS				(LM3S_FLASHCTL_BASE + 0x00C)
#define LM3S_FLASHCTL_FCIM				(LM3S_FLASHCTL_BASE + 0x010)
#define LM3S_FLASHCTL_FCMISC			(LM3S_FLASHCTL_BASE + 0x014)

#define LM3S_FLASHCTL_FMC_KEY			0xA4420000
#define LM3S_FLASHCTL_FMC_WRITE			0x0001
#define LM3S_FLASHCTL_FMC_ERASE			0x0002
#define LM3S_FLASHCTL_FMC_MERASE		0x0004
#define LM3S_FLASHCTL_FMC_COMT			0x0008

#define LM3S_FLASHCTL_INT_PROGRAMMING	0x02
#define LM3S_FLASHCTL_INT_ACCESS		0x01

struct lm3s_device_info_t
{
	uint32_t did0;
	uint32_t did1;
	uint32_t dc0;
	uint32_t dc1;
	uint32_t dc2;
	uint32_t dc3;
};

vsf_err_t lm3s_check_device(struct lm3s_device_info_t *device);

#endif /* __LM3S_INTERNAL_H_INCLUDED__ */

