/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       MSP430_JTAG.c                                             *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    MSP430_JTAG interface header file                         *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

vsf_err_t msp430jtag_init(uint8_t index);
vsf_err_t msp430jtag_fini(uint8_t index);
vsf_err_t msp430jtag_config(uint8_t index, uint8_t has_test);
vsf_err_t msp430jtag_ir(uint8_t index, uint8_t *ir, uint8_t want_ret);
vsf_err_t msp430jtag_dr(uint8_t index, uint32_t *dr, uint8_t bitlen,
						uint8_t want_ret);
vsf_err_t msp430jtag_tclk(uint8_t index, uint8_t value);
vsf_err_t msp430jtag_tclk_strobe(uint8_t index, uint16_t cnt);
vsf_err_t msp430jtag_reset(uint8_t index);
vsf_err_t msp430jtag_poll(uint8_t index, uint32_t dr, uint32_t mask,
		uint32_t value, uint8_t len, uint16_t poll_cnt, uint8_t toggle_tclk);
