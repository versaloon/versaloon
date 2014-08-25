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

/****************************** KEY *****************************/
// PB2(Valid_Low) for MiniRelease1
// PC4(Valid_Low) for ProRelease2
// PA4(Valid_Low) for ProRelease3
#define KEY_PORT						1
#define KEY_PIN							9

#define KEY_VALID_LOW					0

/****************************** USB *****************************/
#define USB_PULL_PORT					2
#define USB_PULL_PIN					13

#define USB_Pull_Init()					do{\
											core_interfaces.gpio.init(USB_PULL_PORT);\
											core_interfaces.gpio.clear(USB_PULL_PORT, 1 << USB_PULL_PIN);\
											core_interfaces.gpio.config_pin(USB_PULL_PORT, USB_PULL_PIN, GPIO_OUTPP);\
										} while (0)
#define USB_Connect()					core_interfaces.gpio.set(USB_PULL_PORT, 1 << USB_PULL_PIN)
#define USB_Disconnect()				core_interfaces.gpio.clear(USB_PULL_PORT, 1 << USB_PULL_PIN)
