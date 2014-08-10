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

#ifndef __NUC400_INTERNAL_H_INCLUDED__
#define __NUC400_INTERNAL_H_INCLUDED__

#define NUC400_SWD						0

#define NUC400_REG_GCR_BA				0x40000000
#define NUC400_REG_PDID					(NUC400_REG_GCR_BA + 0x00)
#define NUC400_REG_RSTSRC				(NUC400_REG_GCR_BA + 0x04)
#define NUC400_REG_IPRSTC1				(NUC400_REG_GCR_BA + 0x08)

#define NUC400_REG_IPRSTC1_CUP_RST		((uint32_t)1 << 1)

#define NUC400_REG_REGWRPROT			(NUC400_REG_GCR_BA + 0x100)
#define NUC400_REG_REGWRPROT_D0			0x59
#define NUC400_REG_REGWRPROT_D1			0x16
#define NUC400_REG_REGWRPROT_D2			0x88

#define NUC400_REG_FMC_BA				0x4000C000
#define NUC400_REG_ISPCON				(NUC400_REG_FMC_BA + 0x00)
#define NUC400_REG_ISPADR				(NUC400_REG_FMC_BA + 0x04)
#define NUC400_REG_ISPDAT				(NUC400_REG_FMC_BA + 0x08)
#define NUC400_REG_ISPCMD				(NUC400_REG_FMC_BA + 0x0C)
#define NUC400_REG_ISPTRG				(NUC400_REG_FMC_BA + 0x10)

#define NUC400_REG_ISPCON_ISPFF			((uint32_t)1 << 6)
#define NUC400_REG_ISPCON_LDUEN			((uint32_t)1 << 5)
#define NUC400_REG_ISPCON_CFGUEN		((uint32_t)1 << 4)
#define NUC400_REG_ISPCON_BS_LDROM		((uint32_t)1 << 1)
#define NUC400_REG_ISPCON_BS_APROM		((uint32_t)0 << 1)
#define NUC400_REG_ISPCON_ISPEN			((uint32_t)1 << 0)

#define NUC400_REG_ISPTRG_ISPGO			((uint32_t)1 << 0)

#define NUC400_REG_ISPCMD_READ			0x00
#define NUC400_REG_ISPCMD_PROGRAM		0x21
#define NUC400_REG_ISPCMD_PAGE_ERASE	0x22

#define NUC400_REG_AHBCLK				0x40000204
#define NUC400_REG_AHBCLK_ISPEN			((uint32_t)1 << 2)

#endif /* __NUC400_INTERNAL_H_INCLUDED__ */

