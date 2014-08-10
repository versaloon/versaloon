/**************************************************************************
 *  Copyright (C) 2008 - 2012 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    VSF                                                       *
 *  File:       app_cfg.h                                                 *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    configuration file                                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 *      2011-01-09:     Add stmxx-discovery (by Bingo)                    *
 **************************************************************************/

// hardware config file
#include "hw_cfg_VSFCore_STM32.h"

// compiler config
#include "compiler.h"

#define USB_TO_XXX_EN					1
#define USB_TO_USART_EN					(0 && HW_HAS_USART)
#define USB_TO_SPI_EN					(1 && HW_HAS_SPI)
#define USB_TO_IIC_EN					(1 && HW_HAS_IIC)
#define USB_TO_GPIO_EN					(1 && HW_HAS_GPIO)
#define USB_TO_CAN_EN					(0 && HW_HAS_CAN)
#define USB_TO_PWM_EN					(1 && HW_HAS_PWM)
#define USB_TO_ADC_EN					(1 && HW_HAS_ADC)
#define USB_TO_DAC_EN					(0 && HW_HAS_DAC)
#define USB_TO_EBI_EN					(1 && HW_HAS_EBI)

/***************************** Buffer ****************************/
#define USB_DATA_BUFF_SIZE				(8 * 1024)
#define ASYN_DATA_BUFF_SIZE				(4 * 1024)

/**************************** Checks ****************************/
#define _HARDWARE_VER_STR				make_ver(_HARDWARE_VER)
#define make_ver(v)						make_str(v)
#define make_str(s)						# s

#define INTERFACE_IIC_EN				1
#define INTERFACE_SPI_EN				1
#define INTERFACE_SDIO_EN				1
#define INTERFACE_USART_EN				1
#define INTERFACE_GPIO_EN				1
#define INTERFACE_PWM_EN				0
#define INTERFACE_EBI_EN				1
#define INTERFACE_ADC_EN				1
