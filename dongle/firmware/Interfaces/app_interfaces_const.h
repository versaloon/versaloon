/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       interfaces_const.h                                        *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    consts of interface module                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2011-04-08:     created(by SimonQian)                             *
 **************************************************************************/
#ifndef __APP_INTERFACE_CONST_H_INCLUDED__
#define __APP_INTERFACE_CONST_H_INCLUDED__

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
#define IFS_INVALID_INTERFACE	(1ULL << 63)
#define IFS_MASK				(USART | SPI | I2C | GPIO | CAN | CLOCK | ADC \
								 | DAC | POWER | ISSP | JTAG | MSP430_JTAG \
								 | LPC_ICP | MSP430_SBW | SWD | SWIM | HV | BDM\
								 | MICROWIRE | USBD)

// GPIO
#define GPIO_SRST				(1 << 0)
#define GPIO_TRST				(1 << 1)
#define GPIO_USR1				(1 << 2)
#define GPIO_USR2				(1 << 3)
#define GPIO_TCK				(1 << 4)
#define GPIO_TDO				(1 << 5)
#define GPIO_TDI				(1 << 6)
#define GPIO_RTCK				(1 << 7)
#define GPIO_TMS				(1 << 8)

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

#endif /* __APP_INTERFACE_CONST_H_INCLUDED__ */

