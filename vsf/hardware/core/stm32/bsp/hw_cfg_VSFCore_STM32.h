/**************************************************************************
 *  Copyright (C) 2008 - 2012 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       hw_cfg_VSFCore_STM32.h                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    hardware configuration file for VSFCore_STM32             *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 *      2008-11-22:     rewrite GPIO_Dir(by SimonQian)                    *
 **************************************************************************/

#ifndef HSE_VALUE
#define HSE_VALUE						((uint32_t)12000000)
#endif

#define OSC_HZ							HSE_VALUE

#define _SYS_FREQUENCY					72		// in MHz
#define _SYS_FLASH_VECTOR_TABLE_SHIFT	FLASH_LOAD_OFFSET // From board_defs.mk

/****************************** Abilities ******************************/
#define HW_HAS_USART					1
#define HW_HAS_SPI						1
#define HW_HAS_SDIO						1
#define HW_HAS_EBI						1
#define HW_HAS_IIC						1
#define HW_HAS_GPIO						1
#define HW_HAS_CAN						1
#define HW_HAS_ADC						1
#define HW_HAS_DAC						1

/****************************** USART ******************************/
#define USART_PORT						1
#define USART_RTS_PORT					IFS_DUMMY_PORT
#define USART_RTS_PIN					0
#define USART_DTR_PORT					IFS_DUMMY_PORT
#define USART_DTR_PIN					0

/****************************** SPI ******************************/
#define SPI0_PORT						0x01
#define SPI0_CS_PORT					IFS_DUMMY_PORT
#define SPI0_CS_PIN						0
#define SPI1_PORT						0x10
#define SPI1_CS_PORT					IFS_DUMMY_PORT
#define SPI1_CS_PIN						0

/****************************** IIC ******************************/
#define IIC_PORT						GPIOB
#define IIC_SCL_PIN						GPIO_PIN_10
#define IIC_SDA_PIN						GPIO_PIN_11

#define IIC_PULL_INIT()					

#define IIC_SCL_INIT()					GPIO_SetMode(IIC_PORT, IIC_SCL_PIN, GPIO_MODE_OUT_OD)
#define IIC_SCL_FINI()					GPIO_SetMode(IIC_PORT, IIC_SCL_PIN, GPIO_MODE_IN_FLOATING)
#define IIC_SCL_SET()					GPIO_SetPins(IIC_PORT, IIC_SCL_PIN)
#define IIC_SCL_CLR()					GPIO_ClrPins(IIC_PORT, IIC_SCL_PIN)
#define IIC_SCL_GET()					GPIO_GetInPins(IIC_PORT, IIC_SCL_PIN)

#define IIC_SDA_INIT()					GPIO_SetMode(IIC_PORT, IIC_SDA_PIN, GPIO_MODE_OUT_OD)
#define IIC_SDA_FINI()					GPIO_SetMode(IIC_PORT, IIC_SDA_PIN, GPIO_MODE_IN_FLOATING)
#define IIC_SDA_SET()					GPIO_SetPins(IIC_PORT, IIC_SDA_PIN)
#define IIC_SDA_CLR()					GPIO_ClrPins(IIC_PORT, IIC_SDA_PIN)
#define IIC_SDA_GET()					GPIO_GetInPins(IIC_PORT, IIC_SDA_PIN)

/****************************** dummy *****************************/
#define LED_USB_INIT()
#define LED_USB_ON()
#define LED_USB_OFF()
#define LED_POWER_INIT()
#define LED_STATE_INIT()
#define LED_STATE_R_ON()
#define LED_STATE_G_ON()
#define LED_STATE_B_ON()

/****************************** USB *****************************/
// For USB 2.0, use DP
// For USB 1.1, use DM
#define USB_DP_PORT						0
#define USB_DP_PIN						12
#define USB_PULL_PORT					2
#define USB_PULL_PIN					13

#define USB_Pull_Init()					do{\
											core_interfaces.gpio.init(USB_PULL_PORT);\
											core_interfaces.gpio.clear(USB_PULL_PORT, 1 << USB_PULL_PIN);\
											core_interfaces.gpio.config_pin(USB_PULL_PORT, USB_PULL_PIN, GPIO_OUTPP);\
										} while (0)
#define USB_Connect()					core_interfaces.gpio.set(USB_PULL_PORT, 1 << USB_PULL_PIN)
#define USB_Disconnect()				core_interfaces.gpio.clear(USB_PULL_PORT, 1 << USB_PULL_PIN)

#define USB_Disable()					PowerOff()
#define USB_D_SETOUTPUT()				do {\
											core_interfaces.gpio.init(USB_DP_PORT);\
											core_interfaces.gpio.config_pin(USB_DP_PORT, USB_DP_PIN, GPIO_OUTPP);\
										} while (0)
#define USB_D_SET()						core_interfaces.gpio.set(USB_DP_PORT, 1 << USB_DP_PIN)
#define USB_D_CLR()						core_interfaces.gpio.clear(USB_DP_PORT, 1 << USB_DP_PIN)
