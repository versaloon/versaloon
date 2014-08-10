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
#ifndef __APP_CFG_INCLUDED__
#define __APP_CFG_INCLUDED__

#include "compiler.h"

// vss config
#define	VSS_CFG_MAX_LINE_LENGTH				4096
#define VSS_CFG_MAX_ARGC					1024

// system config
#define SYS_CFG_LARGE_MEMORY				1

#define PARAM_CHECK							1

// Target Support Configuration
#define TARGET_AT89S5X_EN					1
#define TARGET_AT91SAM3_EN					1
#define TARGET_AVR32_EN						0
#define TARGET_AVR8_EN						1
#define TARGET_AVRXMEGA_EN					1
#define TARGET_C8051F_EN					1
#define TARGET_HCS08_EN						1
#define TARGET_HCS12_EN						1
#define TARGET_LM3S_EN						1
#define TARGET_LPC1000_EN					1
#define TARGET_LPC900_EN					1
#define TARGET_MSP430_EN					1
#define TARGET_PSOC1_EN						1
#define TARGET_STM32F1_EN					1
#define TARGET_NUC100_EN					1
#define TARGET_STM8_EN						1
#define TARGET_SVF_EN						1
#define TARGET_EE93CX6_EN					1
#define TARGET_EE24CXX_EN					1
#define TARGET_DF25XX_EN					1
#define TARGET_STM32F2_EN					1
#define TARGET_STM32F4_EN					1
#define TARGET_STM32L1_EN					1
#define TARGET_SD_EN						1
#define TARGET_CFI_EN						1
#define TARGET_NAND_EN						1

#define POWER_OUT_EN						1
#define INTERFACE_ADC_EN					1
#define INTERFACE_JTAG_EN					1
#define INTERFACE_IIC_EN					1
#define INTERFACE_GPIO_EN					1
#define INTERFACE_SPI_EN					1
#define INTERFACE_PWM_EN					1
#define INTERFACE_SDIO_EN					1
#define INTERFACE_USART_EN					1
#define INTERFACE_EBI_EN					1
#define INTERFACE_MICROWIRE_EN				1

#define INTERFACE_C2_EN						1
#define INTERFACE_ISSP_EN					1
#define INTERFACE_LPC_ICP_EN				1
#define INTERFACE_MSP430_JTAG_EN			1
#define INTERFACE_MSP430_SBW_EN				1
#define INTERFACE_SWIM_EN					1
#define INTERFACE_SWD_EN					1
#define INTERFACE_BDM_EN					1
#define INTERFACE_DUSI_EN					1

#endif /* __APP_CFG_INCLUDED__ */

