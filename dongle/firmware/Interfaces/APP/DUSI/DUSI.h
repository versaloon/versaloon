/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       DUSI.h                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    DUSI interface header file                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

vsf_err_t dusi_init(uint8_t index);
vsf_err_t dusi_fini(uint8_t index);
vsf_err_t dusi_io(uint8_t index, uint8_t *mo, uint8_t *mi, uint8_t *so, 
					uint8_t *si, uint32_t bitlen);
vsf_err_t dusi_config(uint8_t index, uint32_t kHz, uint8_t mode);
