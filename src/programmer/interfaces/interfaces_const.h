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
#ifndef __PROG_INTERFACE_H_INCLUDED__
#define __PROG_INTERFACE_H_INCLUDED__

// interfaces
#define IFS_USART				(1ULL << 0)
#define IFS_SPI					(1ULL << 1)
#define IFS_I2C					(1ULL << 2)
#define IFS_GPIO				(1ULL << 3)
#define IFS_CAN					(1ULL << 4)
#define IFS_CLOCK				(1ULL << 5)
#define IFS_ADC					(1ULL << 6)
#define IFS_DAC					(1ULL << 7)
#define IFS_POWER				(1ULL << 8)
#define IFS_ISSP				(1ULL << 16)
#define IFS_JTAG_LL				(1ULL << 17)
#define IFS_JTAG_HL				(1ULL << 18)
#define IFS_MSP430_SBW			(1ULL << 19)
#define IFS_C2					(1ULL << 20)
#define IFS_MSP430_JTAG			(1ULL << 21)
#define IFS_LPC_ICP				(1ULL << 22)
#define IFS_SWD					(1ULL << 23)
#define IFS_SWIM				(1ULL << 24)
#define IFS_HV					(1ULL << 25)
#define IFS_PDI					(1ULL << 26)
#define IFS_JTAG_RAW			(1ULL << 27)
#define IFS_BDM					(1ULL << 28)
#define IFS_POLL				(1ULL << 29)
#define IFS_DUSI				(1ULL << 30)
#define IFS_MICROWIRE			(1ULL << 31)
#define IFS_PWM					(1ULL << 32)
#define IFS_SDIO				(1ULL << 33)
#define IFS_EBI					(1ULL << 34)
#define IFS_NAND				(1ULL << 35)
#define IFS_INVALID_INTERFACE	(1ULL << 63)


// GPIO
#define GPIO_INFLOAT			0x00
#define GPIO_INPU				0x06
#define GPIO_INPD				0x02
#define GPIO_OUTPP				0x01
#define GPIO_OUTOD				0x05

#define GPIO_SRST				(1 << 0)
#define GPIO_TRST				(1 << 1)
#define GPIO_USR1				(1 << 2)
#define GPIO_USR2				(1 << 3)
#define GPIO_TCK				(1 << 4)
#define GPIO_TDO				(1 << 5)
#define GPIO_TDI				(1 << 6)
#define GPIO_RTCK				(1 << 7)
#define GPIO_TMS				(1 << 8)

// USART
#define USART_MODE0				0x00
#define USART_MODE1				0x04
#define USART_MODE2				0x08
#define USART_MODE3				0x0C
#define USART_CLKEN				0x80
#define USART_STOPBITS_0P5		0x20
#define USART_STOPBITS_1		0x00
#define USART_STOPBITS_1P5		0x60
#define USART_STOPBITS_2		0x40
#define USART_PARITY_NONE		0x00
#define USART_PARITY_ODD		0x03
#define USART_PARITY_EVEN		0x02

// PWM
#define PWM_OUTPP				0x01
#define PWM_OUTPOLARITY			0x02

// SWD
#define SWD_SUCCESS				0x00
#define SWD_RETRY_OUT			0x01
#define SWD_FAULT				0x02
#define SWD_ACK_ERROR			0x03
#define SWD_PARITY_ERROR		0x04

// JTAG
#define JTAG_SRST				GPIO_SRST
#define JTAG_TRST				GPIO_TRST
#define JTAG_USR1				GPIO_USR1
#define JTAG_USR2				GPIO_USR2

// SWIM
#define SWIM_PIN				GPIO_TMS
#define SWIM_RST_PIN			GPIO_SRST

// BDM
#define BDM_PIN					GPIO_TMS

// SPI
#define SPI_MASTER				0x04
#define SPI_SLAVE				0x00
#define SPI_MODE0				0
#define SPI_MODE1				1
#define SPI_MODE2				2
#define SPI_MODE3				3
#define SPI_MSB_FIRST			0x00
#define SPI_LSB_FIRST			0x80

// ISSP for PSoC
#define ISSP_PM_RESET			(1 << 0)
#define ISSP_PM_POWER_ON		(0 << 0)

#define ISSP_VECTOR_END_BIT_1	0x04
#define ISSP_VECTOR_END_BIT_0	0x00
#define ISSP_VECTOR_ATTR_BANK	(1 << 0)
#define ISSP_VECTOR_ATTR_READ	(1 << 1)
#define ISSP_VECTOR_ATTR_0s		(1 << 3)

#define ISSP_VECTOR_0S			(ISSP_VECTOR_ATTR_0s | ISSP_VECTOR_END_BIT_0)
#define ISSP_VECTOR_READ_SRAM	(ISSP_VECTOR_ATTR_READ | ISSP_VECTOR_END_BIT_1)
#define ISSP_VECTOR_WRITE_SRAM	(ISSP_VECTOR_END_BIT_1)
#define ISSP_VECTOR_WRITE_REG	(ISSP_VECTOR_ATTR_BANK | ISSP_VECTOR_END_BIT_1)

#define ISSP_WAP_OK				0x00
#define ISSP_WAP_TIMEOUT		0x01

// EBI
#define EBI_TGTTYP_NOR			0x00
#define EBI_TGTTYP_SRAM			0x10
#define EBI_TGTTYP_PSRAM		0x20
#define EBI_TGTTYP_NAND			0x30

// ADC
#define ADC_ALIGNLEFT			0x08
#define ADC_ALIGNRIGHT			0x00

#endif /* __PROG_INTERFACE_H_INCLUDED__ */

