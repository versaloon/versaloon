/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       GPIO.h                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    GPIO interface header file                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

vsf_err_t stm32_eint_init(uint8_t index);
vsf_err_t stm32_eint_fini(uint8_t index);
vsf_err_t stm32_eint_config(uint8_t index, uint8_t type,
							void (*callback)(void));
vsf_err_t stm32_eint_enable(uint8_t index);
vsf_err_t stm32_eint_disable(uint8_t index);
vsf_err_t stm32_eint_trigger(uint8_t index);
