/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       MicroWire.h                                               *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    MicroWire interface header file                           *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

vsf_err_t microwire_init(uint8_t index);
vsf_err_t microwire_fini(uint8_t index);
vsf_err_t microwire_config(uint8_t index, uint16_t kHz, uint8_t sel_polarity);
vsf_err_t microwire_transport(uint8_t index, uint32_t opcode,
		uint8_t opcode_bitlen, uint32_t addr, uint8_t addr_bitlen, 
		uint32_t data, uint8_t data_bitlen, uint8_t *ret, uint8_t ret_bitlen);
vsf_err_t microwire_poll(uint8_t index, uint16_t interval_us,
							uint16_t retry_cnt);
