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

#define IFS_UNIQUEID_EN						1
#define IFS_FLASH_EN						1
#define IFS_CLKO_EN							1
#define IFS_GPIO_EN							1
#define IFS_TIMER_EN						1
#define IFS_EINT_EN							1
#define IFS_USART_EN						1
#define IFS_SPI_EN							1
#define IFS_ADC_EN							1
#define IFS_IIC_EN							1
#define IFS_USBD_EN							1
#define IFS_PWM_EN							1
#define IFS_MICROWIRE_EN					1
#define IFS_EBI_EN							1
#define IFS_SDIO_EN							1

// Core config for clocks, flash, debug...
#ifndef OSC0_FREQ_HZ
#	define OSC0_FREQ_HZ						OSC_HZ
#endif
#ifndef CORE_CLKSRC
#	define CORE_CLKSRC						STM32_CLKSRC_PLL
#endif
#ifndef CORE_PLLSRC
#	define CORE_PLLSRC						STM32_PLLSRC_HSE
#endif
#ifndef CORE_RTCSRC
#	define CORE_RTCSRC						STM32_RTCSRC_LSI
#endif
#ifndef CORE_HSE_TYPE
#	define CORE_HSE_TYPE					STM32_HSE_TYPE_CRYSTAL
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
#	define CORE_DEBUG						STM32_DBG_JTAG_SWD
#endif
#ifndef CORE_VECTOR_TABLE
#	define CORE_VECTOR_TABLE				(0x08000000 | FLASH_LOAD_OFFSET)
#endif

// SPI config
#define SPI_NUM								2
#define SPI00_ENABLE						0
#	define SPI00_MISO_ENABLE				0
#	define SPI00_MOSI_ENABLE				0
#define SPI10_ENABLE						1
#	define SPI10_MISO_ENABLE				1
#	define SPI10_MOSI_ENABLE				0
#define SPI01_ENABLE						1
#	define SPI01_MISO_ENABLE				1
#	define SPI01_MOSI_ENABLE				1
#define SPI02_ENABLE						0
#	define SPI02_MISO_ENABLE				0
#	define SPI02_MOSI_ENABLE				0

// USART config
#define USART_NUM							3
#define USART0_INT_EN						1
#define USART1_INT_EN						0
#define USART2_INT_EN						0
#define USART00_ENABLE						1
#	define USART00_CTS_ENABLE				0
#	define USART00_RTS_ENABLE				0
#	define USART00_TX_ENABLE				1
#	define USART00_RX_ENABLE				1
#	define USART00_CK_ENABLE				0
#define USART10_ENABLE						0
#	define USART10_CTS_ENABLE				0
#	define USART10_RTS_ENABLE				0
#	define USART10_TX_ENABLE				0
#	define USART10_RX_ENABLE				0
#define USART01_ENABLE						0
#	define USART01_CTS_ENABLE				0
#	define USART01_RTS_ENABLE				0
#	define USART01_TX_ENABLE				0
#	define USART01_RX_ENABLE				0
#define USART11_ENABLE						0
#	define USART11_CTS_ENABLE				0
#	define USART11_RTS_ENABLE				0
#	define USART11_TX_ENABLE				0
#	define USART11_RX_ENABLE				0
#define USART02_ENABLE						0
#	define USART02_CTS_ENABLE				0
#	define USART02_RTS_ENABLE				0
#	define USART02_TX_ENABLE				0
#	define USART02_RX_ENABLE				0
#define USART12_ENABLE						0
#	define USART12_CTS_ENABLE				0
#	define USART12_RTS_ENABLE				0
#	define USART12_TX_ENABLE				0
#	define USART12_RX_ENABLE				0
#define USART32_ENABLE						0
#	define USART32_CTS_ENABLE				0
#	define USART32_RTS_ENABLE				0
#	define USART32_TX_ENABLE				0
#	define USART32_RX_ENABLE				0
#define USART03_ENABLE						0
#	define USART03_CTS_ENABLE				0
#	define USART03_RTS_ENABLE				0
#	define USART03_TX_ENABLE				0
#	define USART03_RX_ENABLE				0
#define USART04_ENABLE						0
#	define USART04_CTS_ENABLE				0
#	define USART04_RTS_ENABLE				0
#	define USART04_TX_ENABLE				0
#	define USART04_RX_ENABLE				0

// FSMC
#define FSMC00_ADDR_LEN						26
#define FSMC00_DATA_LEN						16
