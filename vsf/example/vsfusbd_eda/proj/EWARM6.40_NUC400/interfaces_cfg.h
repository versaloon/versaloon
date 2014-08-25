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

// Core config for clocks
#ifndef CORE_CLKEN
#	define CORE_CLKEN						NUC400_CLK_HXT
#endif
#ifndef CORE_HCLKSRC
#	define CORE_HCLKSRC						NUC400_HCLKSRC_PLLFOUT
#endif
#ifndef CORE_PCLKSRC
#	define CORE_PCLKSRC						NUC400_PCLKSRC_HCLK
#endif
#ifndef CORE_PLLSRC
#	define CORE_PLLSRC						NUC400_PLLSRC_HXT
#endif
#ifndef OSC0_FREQ_HZ
#	define OSC0_FREQ_HZ						(12 * 1000 * 1000)
#endif
#ifndef OSC32_FREQ_HZ
#	define OSC32_FREQ_HZ					0
#endif
#ifndef CORE_PLL_FREQ_HZ
#	define CORE_PLL_FREQ_HZ					(48 * 1000 * 1000)
#endif
#ifndef CPU_FREQ_HZ
#	define CPU_FREQ_HZ						CORE_PLL_FREQ_HZ
#endif
#ifndef HCLK_FREQ_HZ
#	define HCLK_FREQ_HZ						CORE_PLL_FREQ_HZ
#endif
#ifndef PCLK_FREQ_HZ
#	define PCLK_FREQ_HZ						CORE_PLL_FREQ_HZ
#endif
#ifndef CORE_VECTOR_TABLE
#	define CORE_VECTOR_TABLE				(0x00000000)
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

