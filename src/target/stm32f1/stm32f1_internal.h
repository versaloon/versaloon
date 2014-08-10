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

#ifndef __STM32F1_INTERNAL_H_INCLUDED__
#define __STM32F1_INTERNAL_H_INCLUDED__

#define STM32F1_FLASH_BANK_SIZE		(512 * 1024)

#define STM32F1_JTAG				0
#define STM32F1_SWD					1
#define STM32F1_ISP					2

#define STM32F1_REV_MSK				0xFFFF0000
#define STM32F1_DEN_MSK				0x00000FFF
#define STM32F1_DEN_LOW				0x0412
#define STM32F1_DEN_MEDIUM			0x0410
#define STM32F1_DEN_HIGH			0x0414
#define STM32F1_DEN_CONNECTIVITY	0x0418
#define STM32F1_DEN_VALUELINE		0x0420
#define STM32F1_DEN_XL				0x0430

#define STM32F1_REG_FLASH_RAM_SIZE	0x1FFFF7E0
#define STM32F1_REG_MCU_ID			0xE0042000

#define STM32F1_OB_ADDR				0x1FFFF800
#define STM32F1_OB_SIZE				16
#define STM32F1_OB_RDP				(STM32F1_OB_ADDR + 0)
#define STM32F1_OB_USER				(STM32F1_OB_ADDR + 2)
#define STM32F1_OB_DATA0			(STM32F1_OB_ADDR + 4)
#define STM32F1_OB_DATA1			(STM32F1_OB_ADDR + 6)
#define STM32F1_OB_WRP0				(STM32F1_OB_ADDR + 8)
#define STM32F1_OB_WRP1				(STM32F1_OB_ADDR + 10)
#define STM32F1_OB_WRP2				(STM32F1_OB_ADDR + 12)
#define STM32F1_OB_WRP3				(STM32F1_OB_ADDR + 14)

#define STM32F1_UID_ADDR			(0x1FFFF7E8)

void stm32f1_print_device(uint32_t mcuid);
uint16_t stm32f1_get_flash_size(uint32_t mcuid, uint32_t flash_sram_reg);

#endif /* __STM32F1_INTERNAL_H_INCLUDED__ */

