/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       LPC_ICP.h                                                 *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    LPC_ICP interface header file                             *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#define LPCICP_POLL_ON_SET			0
#define LPCICP_POLL_ON_CLEAR		1
#define LPCICP_POLL_TIME_OUT		2

vsf_err_t lpcicp_init(uint8_t index);
vsf_err_t lpcicp_fini(uint8_t index);
vsf_err_t lpcicp_enter_program_mode(uint8_t index);
vsf_err_t lpcicp_in(uint8_t index, uint8_t *buff, uint16_t len);
vsf_err_t lpcicp_out(uint8_t index, uint8_t *buff, uint16_t len);
vsf_err_t lpcicp_poll_ready(uint8_t index, uint8_t data, uint8_t *ret, 
				uint8_t setmask, uint8_t clearmask, uint16_t pollcnt);
