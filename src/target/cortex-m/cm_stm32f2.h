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
#ifndef __CM_STM32F2_H_INCLUDED__
#define __CM_STM32F2_H_INCLUDED__

#define STM32F2_IRC_KHZ					4000

/* stm32f2 flash register locations */
#define STM32F2_FLASH_BASE				0x40023C00
#define STM32F2_FLASH_ACR				(STM32F2_FLASH_BASE + 0x00)
#define STM32F2_FLASH_KEYR				(STM32F2_FLASH_BASE + 0x04)
#define STM32F2_FLASH_OPTKEYR			(STM32F2_FLASH_BASE + 0x08)
#define STM32F2_FLASH_SR				(STM32F2_FLASH_BASE + 0x0C)
#define STM32F2_FLASH_CR				(STM32F2_FLASH_BASE + 0x10)
#define STM32F2_FLASH_OPTCR				(STM32F2_FLASH_BASE + 0x14)
#define STM32F2_FLASH_OBR				(STM32F2_FLASH_BASE + 0x1C)

#define STM32F2_FLASH_OPTCR_START		0x00000002
#define STM32F2_FLASH_OPT_MASK			0x0FFFFF00
#define STM32F2_FLASH_OPT_RDP_MASK		0x0000FF00
#define STM32F2_FLASH_OPT_RDP_LVL0		0x0000AA00
#define STM32F2_FLASH_OPT_RDP_LVL1		0x00000000
#define STM32F2_FLASH_OPT_RDP_LVL2		0x0000CC00
#define STM32F2_FLASH_OPT_WRP_MASK		0x0FFF0000

/* FLASH_CR register bits */
#define STM32F2_FLASH_CR_PG				(1 << 0)
#define STM32F2_FLASH_CR_SER			(1 << 1)
#define STM32F2_FLASH_CR_MER			(1 << 2)
#define STM32F2_FLASH_CR_SNB(a)			((a) << 3)
#define STM32F2_FLASH_CR_PSIZE_8		(0 << 8)
#define STM32F2_FLASH_CR_PSIZE_16		(1 << 8)
#define STM32F2_FLASH_CR_PSIZE_32		(2 << 8)
#define STM32F2_FLASH_CR_PSIZE_64		(3 << 8)
#define STM32F2_FLASH_CR_STRT			(1 << 16)
#define STM32F2_FLASH_CR_LOCK			(1 << 31)

/* FLASH_SR register bits */
#define STM32F2_FLASH_SR_BSY			(1 << 16)
#define STM32F2_FLASH_SR_PGSERR			(1 << 7)
#define STM32F2_FLASH_SR_PGPERR			(1 << 6)
#define STM32F2_FLASH_SR_PGAERR			(1 << 5)
#define STM32F2_FLASH_SR_WRPERR			(1 << 4)
#define STM32F2_FLASH_SR_OPERR			(1 << 5)
#define STM32F2_FLASH_SR_ERRMSK			\
	(STM32F2_FLASH_SR_PGSERR | STM32F2_FLASH_SR_PGPERR | \
	STM32F2_FLASH_SR_PGAERR | STM32F2_FLASH_SR_WRPERR | \
	STM32F2_FLASH_SR_OPERR)

#define STM32F2_FLASH_UNLOCK_KEY1		0x45670123
#define STM32F2_FLASH_UNLOCK_KEY2		0xCDEF89AB
#define STM32F2_OPT_UNLOCK_KEY1			0x08192A3B
#define STM32F2_OPT_UNLOCK_KEY2			0x4C5D6E7F

extern const struct program_functions_t stm32f2swj_program_functions;

#endif /* __CM_STM32F2_H_INCLUDED__ */

