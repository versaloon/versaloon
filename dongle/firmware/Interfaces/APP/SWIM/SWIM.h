/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       SWIM.h                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    SWIM interface header file                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

vsf_err_t swim_init(uint8_t index);
vsf_err_t swim_fini(uint8_t index);
vsf_err_t swim_config(uint8_t index, uint8_t mHz, uint8_t cnt0, uint8_t cnt1);
vsf_err_t swim_srst(uint8_t index);
vsf_err_t swim_wotf(uint8_t index, uint8_t *data, uint16_t bytelen, 
					uint32_t addr);
vsf_err_t swim_rotf(uint8_t index, uint8_t *data, uint16_t bytelen, 
					uint32_t addr);
vsf_err_t swim_sync(uint8_t index, uint8_t mHz);
vsf_err_t swim_enable(uint8_t index);
