/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       ADC.h                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    ADC interface header file                                 *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

vsf_err_t adc_init(uint8_t index);
vsf_err_t adc_fini(uint8_t index);
vsf_err_t adc_config(uint8_t index, uint32_t clock_hz, uint8_t mode);
vsf_err_t adc_config_channel(uint8_t index, uint8_t channel, uint8_t cycles);
uint32_t adc_get_max_value(uint8_t index);
vsf_err_t adc_calibrate(uint8_t index, uint8_t channel);
vsf_err_t adc_start(uint8_t index, uint8_t channel);
vsf_err_t adc_isready(uint8_t index, uint8_t channel);
uint32_t adc_get(uint8_t index, uint8_t channel);
vsf_err_t adc_sample(uint8_t index, uint8_t channel, uint32_t *voltage);
