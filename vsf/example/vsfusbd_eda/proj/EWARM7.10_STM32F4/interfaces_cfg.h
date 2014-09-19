/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       interfaces.h                                              *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    interfaces header file                                    *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

// Core config for clocks, flash, debug...
#ifndef OSC0_FREQ_HZ
#	define OSC0_FREQ_HZ						OSC_HZ
#endif
#ifndef CORE_CLKSRC
#	define CORE_CLKSRC						STM32F4_CLKSRC_PLL
#endif
#ifndef CORE_PLLSRC
#	define CORE_PLLSRC						STM32F4_PLLSRC_HSE
#endif
#ifndef CORE_RTCSRC
#	define CORE_RTCSRC						STM32F4_RTCSRC_LSI
#endif
#ifndef CORE_HSE_TYPE
#	define CORE_HSE_TYPE					STM32F4_HSE_TYPE_CRYSTAL
#endif
#ifndef CORE_PLL_FREQ_HZ
#	define CORE_PLL_FREQ_HZ					(72 * 1000 * 1000)
#endif
#ifndef CORE_AHB_FREQ_HZ
#	define CORE_AHB_FREQ_HZ					(72 * 1000 * 1000)
#endif
#ifndef CORE_APB1_FREQ_HZ
#	define CORE_APB1_FREQ_HZ				(36 * 1000 * 1000)
#endif
#ifndef CORE_APB2_FREQ_HZ
#	define CORE_APB2_FREQ_HZ				(72 * 1000 * 1000)
#endif
#ifndef CORE_FLASH_LATENCY
#	define CORE_FLASH_LATENCY				2
#endif
#ifndef CORE_DEBUG
#	define CORE_DEBUG						STM32F4_DBG_JTAG_SWD
#endif
#ifndef CORE_VECTOR_TABLE
#	define CORE_VECTOR_TABLE				(0x08000000 | FLASH_LOAD_OFFSET)
#endif



#define IFS_FLASH_EN						0
#define IFS_USART_EN						0
#define IFS_SPI_EN							0
#define IFS_ADC_EN							0
#define IFS_GPIO_EN							1
#define IFS_IIC_EN							0
#define IFS_PWM_EN							0
#define IFS_MICROWIRE_EN					0
#define IFS_TIMER_EN						0
#define IFS_EINT_EN							0
#define IFS_EBI_EN							0
#define IFS_SDIO_EN							0
#define IFS_USBD_EN							1

