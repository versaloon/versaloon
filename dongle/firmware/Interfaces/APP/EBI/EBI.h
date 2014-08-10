/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       SPI.h                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    SPI interface header file                                 *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

vsf_err_t ebi_init(uint8_t index);
vsf_err_t ebi_fini(uint8_t index);
vsf_err_t ebi_config(uint8_t index, uint8_t target_index, void *param);
vsf_err_t ebi_isready(uint8_t index, uint8_t target_index);
vsf_err_t ebi_read(uint8_t index, uint8_t target_index, uint32_t address, 
					uint8_t data_size, uint8_t *buff, uint32_t count);
vsf_err_t ebi_write(uint8_t index, uint8_t target_index, uint32_t address, 
					uint8_t data_size, uint8_t *buff, uint32_t count);
