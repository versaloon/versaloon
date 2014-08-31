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
#ifndef __STM32_INTERFACE_CONST_H_INCLUDED__
#define __STM32_INTERFACE_CONST_H_INCLUDED__

#include "stm32f10x.h"
#include "core.h"

#define stm32_SLEEP_WFI				0
#define stm32_SLEEP_PWRDOWN			1

#define stm32_USART_MODE0			0x00
#define stm32_USART_MODE1			0x04
#define stm32_USART_MODE2			0x08
#define stm32_USART_MODE3			0x0C
#define stm32_USART_CLKEN			0x80
#define stm32_USART_STOPBITS_0P5	0x20
#define stm32_USART_STOPBITS_1		0x00
#define stm32_USART_STOPBITS_1P5	0x60
#define stm32_USART_STOPBITS_2		0x40
#define stm32_USART_PARITY_NONE		0x00
#define stm32_USART_PARITY_ODD		0x03
#define stm32_USART_PARITY_EVEN		0x02

#define stm32_SPI_MASTER			0x04
#define stm32_SPI_SLAVE				0x00
#define stm32_SPI_MODE0				0
#define stm32_SPI_MODE1				1
#define stm32_SPI_MODE2				2
#define stm32_SPI_MODE3				3
#define stm32_SPI_MSB_FIRST			0x00
#define stm32_SPI_LSB_FIRST			0x80

#define stm32_ADC_ALIGNLEFT			0x08
#define stm32_ADC_ALIGNRIGHT		0x00

#define stm32_GPIO_INFLOAT			0x04
#define stm32_GPIO_INPU				0x88
#define stm32_GPIO_INPD				0x08
#define stm32_GPIO_INP				0x08
#define stm32_GPIO_OUTPP			0x01
#define stm32_GPIO_OUTOD			0x05
#define stm32_GPIO_ANALOG			0x00
#define stm32_GPIO_AFOD				0x0F
#define stm32_GPIO_AFPP				0x0B

#define stm32_EINT_ONFALL			0x01
#define stm32_EINT_ONRISE			0x02
#define stm32_EINT_INT				0x04
#define stm32_EINT_EVT				0x08

#define stm32_SDIO_RESP_NONE		0x00
#define stm32_SDIO_RESP_SHORT		0x40
#define stm32_SDIO_RESP_LONG		0xC0

#endif	// __STM32_INTERFACE_CONST_H_INCLUDED__
