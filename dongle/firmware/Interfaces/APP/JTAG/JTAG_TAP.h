/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       JTAG_TAP.h                                                *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    JTAG interface header file                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

vsf_err_t jtaghl_init(uint8_t index);
vsf_err_t jtaghl_fini(uint8_t index);
vsf_err_t jtaghl_config_speed(uint8_t index, uint32_t kHz);
vsf_err_t jtaghl_config_daisychain(uint8_t index, struct jtag_pos_t *jtag_pos);
vsf_err_t jtaghl_config(uint8_t index, uint32_t kHz, struct jtag_pos_t *jtag_pos);
vsf_err_t jtaghl_tms(uint8_t index, uint8_t* tms, uint16_t bitlen);
vsf_err_t jtaghl_runtest(uint8_t index, uint32_t cycles);
vsf_err_t jtaghl_ir(uint8_t index, uint8_t *ir, uint16_t bitlen, uint8_t idle, 
					uint8_t want_ret);
vsf_err_t jtaghl_dr(uint8_t index, uint8_t *dr, uint16_t bitlen, uint8_t idle, 
					uint8_t want_ret);
vsf_err_t jtaghl_register_callback(uint8_t index, jtag_callback_t send_callback, 
									jtag_callback_t receive_callback);

vsf_err_t jtagll_init(uint8_t index);
vsf_err_t jtagll_fini(uint8_t index);
vsf_err_t jtagll_config(uint8_t index, uint32_t kHz);
vsf_err_t jtagll_tms(uint8_t index, uint8_t *tms, uint8_t bytelen);
vsf_err_t jtagll_tms_clocks(uint8_t index, uint32_t bytelen, uint8_t tms);
vsf_err_t jtagll_scan(uint8_t index, uint8_t* data, uint16_t bitlen, 
						uint8_t tms_before_valid, uint8_t tms_before, 
						uint8_t tms_after0, uint8_t tms_after1);

vsf_err_t jtagraw_init(uint8_t index);
vsf_err_t jtagraw_fini(uint8_t index);
vsf_err_t jtagraw_config(uint8_t index, uint32_t kHz);
vsf_err_t jtagraw_execute(uint8_t index, uint8_t* tdi, uint8_t* tms, 
							uint8_t *tdo, uint32_t bitlen);
