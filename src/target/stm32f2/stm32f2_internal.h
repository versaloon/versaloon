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

#ifndef __STM32F2_INTERNAL_H_INCLUDED__
#define __STM32F2_INTERNAL_H_INCLUDED__

#define STM32F2_JTAG					0
#define STM32F2_SWD						1
#define STM32F2_ISP						2

#define STM32F2_REV_MSK					0xFFFF0000
#define STM32F2_DEN_MSK					0x00000FFF
#define STM32F2_DEN_XL					0x0411

#define STM32F4_DEV_4x5_4x7				0x0413
#define STM32F4_DEV_42xx_43xx			0x0419
#define STM32F4_DEV_401xBC				0x0423
#define STM32F4_DEV_401xDE				0x0433

#define STM32F2_REG_MCU_ID				0xE0042000
#define	STM32F2_REG_FLASH_RAM_SIZE		0x1FFF7A20

#define STM32F2_OB_ADDR					0x1FFFC000
#define STM32F2_OB_SIZE					16

#define STM32F2_UID_ADDR				(0x1FFF7A10)

void stm32f2_print_device(uint32_t mcuid);
void stm32f4_print_device(uint32_t mcuid);

#endif /* __STM32F2_INTERNAL_H_INCLUDED__ */

