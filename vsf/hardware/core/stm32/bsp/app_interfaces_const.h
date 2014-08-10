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
#define IFS_PWM					(1ULL << 32)
#define IFS_SDIO				(1ULL << 33)
#define IFS_EBI					(1ULL << 34)
#define IFS_INVALID_INTERFACE	(1ULL << 63)
#define IFS_MASK				(USART | SPI | I2C | GPIO | CAN | CLOCK | ADC \
								 | DAC | POWER | ISSP | JTAG | MSP430_JTAG \
								 | LPC_ICP | MSP430_SBW | SWD | SWIM | HV | BDM\
								 | MICROWIRE | USBD)

#endif /* __APP_INTERFACE_CONST_H_INCLUDED__ */

