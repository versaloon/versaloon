/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       C2.h                                                      *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    C2 interface header file                                  *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

vsf_err_t c2_init(uint8_t index);
vsf_err_t c2_fini(uint8_t index);
vsf_err_t c2_addr_write(uint8_t index, uint8_t addr);
vsf_err_t c2_addr_read(uint8_t index, uint8_t *data);
vsf_err_t c2_data_read(uint8_t index, uint8_t *data, uint8_t len);
vsf_err_t c2_data_write(uint8_t index, uint8_t *data, uint8_t len);
