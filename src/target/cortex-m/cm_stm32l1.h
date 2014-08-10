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
#ifndef __CM_STM32L1_H_INCLUDED__
#define __CM_STM32L1_H_INCLUDED__

#define STM32L1_IRC_KHZ				8000

/* stm32l1 flash register locations */
#define STM32L1_FLASH_BASE			0x40023C00
#define STM32L1_FLASH_ACR			(STM32L1_FLASH_BASE + 0x00)
#define STM32L1_FLASH_PECR			(STM32L1_FLASH_BASE + 0x04)
#define STM32L1_FLASH_PDKEYR		(STM32L1_FLASH_BASE + 0x08)
#define STM32L1_FLASH_PEKEYR		(STM32L1_FLASH_BASE + 0x0C)
#define STM32L1_FLASH_PRGKEYR		(STM32L1_FLASH_BASE + 0x10)
#define STM32L1_FLASH_OPTKEYR		(STM32L1_FLASH_BASE + 0x14)
#define STM32L1_FLASH_SR			(STM32L1_FLASH_BASE + 0x18)
#define STM32L1_FLASH_OBR			(STM32L1_FLASH_BASE + 0x1C)
#define STM32L1_FLASH_WRPR			(STM32L1_FLASH_BASE + 0x20)

#define STM32L1_FLASH_OBR_RDPRT		0xFF
#define STM32L1_FLASH_OBR_RDPRT_LV0	0xAA
#define STM32L1_FLASH_OBR_RDPRT_LV2	0xCC

/* FLASH_CR register bits */
#define STM32L1_FLASH_PECR_PELOCK	(1 << 0)
#define STM32L1_FLASH_PECR_PRGLOCK	(1 << 1)
#define STM32L1_FLASH_PECR_OPTLOCK	(1 << 2)
#define STM32L1_FLASH_PECR_PROG		(1 << 3)
#define STM32L1_FLASH_PECR_DATA		(1 << 4)
#define STM32L1_FLASH_PECR_FTDW		(1 << 8)
#define STM32L1_FLASH_PECR_ERASE	(1 << 9)
#define STM32L1_FLASH_PECR_FPRG		(1 << 10)

/* FLASH_SR register bits */
#define STM32L1_FLASH_SR_BSY		(1 << 0)
#define STM32L1_FLASH_SR_ERRMSK		0x0F00

#define STM32L1_EE_PECR_UNLOCK_KEY1	0x89ABCDEF
#define STM32L1_EE_PECR_UNLOCK_KEY2	0x02030405
#define STM32L1_FLASH_UNLOCK_KEY1	0x8C9DAEBF
#define STM32L1_FLASH_UNLOCK_KEY2	0x13141516
#define STM32L1_PD_UNLOCK_KEY1		0x04152637
#define STM32L1_PD_UNLOCK_KEY2		0xFAFBFCFD
#define STM32L1_OPT_UNLOCK_KEY1		0xFBEAD9C8
#define STM32L1_OPT_UNLOCK_KEY2		0x24252627

extern const struct program_functions_t stm32l1swj_program_functions;

#endif /* __CM_STM32L1_H_INCLUDED__ */

