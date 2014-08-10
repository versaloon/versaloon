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

#ifndef __STM32L1_INTERNAL_H_INCLUDED__
#define __STM32L1_INTERNAL_H_INCLUDED__

#define STM32L1_JTAG				0
#define STM32L1_SWD					1
#define STM32L1_ISP					2

#define STM32L1_REV_MSK				0xFFFF0000
#define STM32L1_DEN_MSK				0x00000FFF
#define STM32L1_DEN_L15				0x0416

#define STM32L1_REG_FLASH_RAM_SIZE	0x1FF8004C
#define STM32L1_REG_MCU_ID			0xE0042000

#define STM32L1_OB_ADDR				0x1FF80000
#define STM32L1_OB_SIZE				16
#define STM32L1_OB_RDP				(STM32L1_OB_ADDR + 0)
#define STM32L1_OB_USER				(STM32L1_OB_ADDR + 4)
#define STM32L1_OB_WRP01			(STM32L1_OB_ADDR + 8)
#define STM32L1_OB_WRP23			(STM32L1_OB_ADDR + 12)

#define STM32L1_UID_ADDR			(0x1FF80050)

void stm32l1_print_device(uint32_t mcuid);
uint16_t stm32l1_get_flash_size(uint32_t mcuid, uint32_t flash_sram_reg);

#endif /* __STM32L1_INTERNAL_H_INCLUDED__ */

