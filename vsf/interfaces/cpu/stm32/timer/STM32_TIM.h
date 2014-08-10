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

vsf_err_t stm32_timer_init(uint8_t index);
vsf_err_t stm32_timer_fini(uint8_t index);
vsf_err_t stm32_timer_config(uint8_t index, uint32_t kHz, uint32_t mode, 
								void (*overflow)(void));
vsf_err_t stm32_timer_start(uint8_t index);
vsf_err_t stm32_timer_stop(uint8_t index);
vsf_err_t stm32_timer_get_count(uint8_t index, uint32_t *count);
vsf_err_t stm32_timer_set_count(uint8_t index, uint32_t count);
vsf_err_t stm32_timer_config_channel(uint8_t index, uint8_t channel, 
										uint32_t mode, void (*callback)(void));
vsf_err_t stm32_timer_get_channel(uint8_t index, uint8_t channel,
									uint32_t *count);
vsf_err_t stm32_timer_set_channel(uint8_t index, uint8_t channel,
									uint32_t count);
