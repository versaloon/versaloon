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

#define ASYN_DATA_BUFF_SIZE				1024

#define INTERFACE_IIC_EN				1
#define INTERFACE_SPI_EN				1
#define INTERFACE_SDIO_EN				1
#define INTERFACE_USART_EN				1
#define INTERFACE_GPIO_EN				1
#define INTERFACE_PWM_EN				0
#define INTERFACE_EBI_EN				1
#define INTERFACE_ADC_EN				1
