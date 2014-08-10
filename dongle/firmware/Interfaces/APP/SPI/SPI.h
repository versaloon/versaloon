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

// Only Support Master Mode
vsf_err_t spi_init(uint8_t index);
vsf_err_t spi_fini(uint8_t index);
vsf_err_t spi_io(uint8_t index, uint8_t *out, uint8_t *in, uint32_t len);
vsf_err_t spi_config(uint8_t index, uint32_t kHz, uint8_t mode);
vsf_err_t spi_select(uint8_t index, uint8_t cs);
vsf_err_t spi_deselect(uint8_t index, uint8_t cs);
