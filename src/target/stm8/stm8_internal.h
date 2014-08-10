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
#ifndef __STM8_INTERNAL_H_INCLUDED__
#define __STM8_INTERNAL_H_INCLUDED__

#define STM8_FUSEPAGE_ADDR			0x004800
#define STM8_FUSEPAGE_SIZE			128

#define	STM8_REG_DM_CSR2			0x007F99
#define STM8_DM_CSR2_STALL			0x08
#define STM8_DM_CSR2_FLUSH			0x01

#define STM8_REG_SWIM_CSR			0x007F80
#define STM8_SWIM_CSR_SAFT_MASK		0x80
#define STM8_SWIM_CSR_SWIM_DM		0x20
#define STM8_SWIM_CSR_HS			0x10
#define STM8_SWIM_CSR_RST			0x04
#define STM8_SWIM_CSR_HSIT			0x02
#define STM8_SWIM_CSR_PRI			0x01

#define STM8_PARAM_IRC				0
#define STM8_PARAM_FLASH_DUKR		1
#define STM8_PARAM_FLASH_PUKR		2
#define STM8_PARAM_FLASH_IAPSR		3
#define STM8_PARAM_FLASH_CR2		4
#define STM8_PARAM_FLASH_NCR2		5
#define STM8_PARAM_CLK_CKDIVR		6
#define STM8_PARAM_CLK_SWIMCCR		7
#define STM8_PARAM_TYPE				8

#define STM8_TYPE_STM8S				0
#define STM8_TYPE_STM8L				1
#define STM8_TYPE_STM8A				2

#define STM8_FLASH_IAPSR_HVOFF		0x40
#define STM8_FLASH_IAPSR_DUL		0x08
#define STM8_FLASH_IAPSR_PUL		0x02
#define STM8_FLASH_IAPSR_EOP		0x04
#define STM8_FLASH_IAPSR_WRPGDIS	0x01

#define STM8_FLASH_CR2_OPT			0x80
#define STM8_FLASH_CR2_WPRG			0x40
#define STM8_FLASH_CR2_ERASE		0x20
#define STM8_FLASH_CR2_FPRG			0x10
#define STM8_FLASH_CR2_PRG			0x01

#define STM8_SWIM					0
#define STM8_ISP					1

#define STM8A_OPT_TMUEN_OFF			13
#define STM8A_OPT_TMUEN_MASK		0x0F
#define STM8A_OPT_TMUEN_ENVAL		0x05
#define STM8A_OPT_TMUKEY_OFF		16
#define STM8A_OPT_TMUMAXATT_OFF		24

#endif /* __STM8_INTERNAL_H_INCLUDED__ */

