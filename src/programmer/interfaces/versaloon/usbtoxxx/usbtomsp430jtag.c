/***************************************************************************
 *   Copyright (C) 2009 - 2010 by Simon Qian <SimonQian@SimonQian.com>     *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "compiler.h"

#include "interfaces.h"
#include "usbtoxxx.h"
#include "usbtoxxx_internal.h"

vsf_err_t usbtomsp430jtag_init(uint8_t index)
{
	return usbtoxxx_init_command(USB_TO_MSP430_JTAG, index);
}

vsf_err_t usbtomsp430jtag_fini(uint8_t index)
{
	return usbtoxxx_fini_command(USB_TO_MSP430_JTAG, index);
}

vsf_err_t usbtomsp430jtag_config(uint8_t index, uint8_t has_test)
{
	return usbtoxxx_conf_command(USB_TO_MSP430_JTAG, index,
								 &has_test, 1);
}

vsf_err_t usbtomsp430jtag_ir(uint8_t index, uint8_t *ir,
							uint8_t want_ret)
{
	uint8_t buff[2];
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	buff[0] = 8;
	buff[1] = *ir;
	
	if (want_ret)
	{
		return usbtoxxx_inout_command(USB_TO_MSP430_JTAG, index,
									  buff, 2, 1, ir, 0, 1, 1);
	}
	else
	{
		return usbtoxxx_inout_command(USB_TO_MSP430_JTAG, index,
									  buff, 2, 1, NULL, 0, 0, 1);
	}
}

vsf_err_t usbtomsp430jtag_dr(uint8_t index, uint32_t *dr,
							uint8_t bitlen, uint8_t want_ret)
{
	uint8_t buff[5], byte_len = (bitlen + 7) >> 3;
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	buff[0] = bitlen | 0x80;
	memcpy(buff + 1, dr, byte_len);
	
	if (want_ret)
	{
		return usbtoxxx_inout_command(USB_TO_MSP430_JTAG, index,
				buff, byte_len + 1, byte_len, (uint8_t*)dr, 0, byte_len, 1);
	}
	else
	{
		return usbtoxxx_inout_command(USB_TO_MSP430_JTAG, index,
				buff, byte_len + 1, byte_len, NULL, 0, 0, 1);
	}
}

vsf_err_t usbtomsp430jtag_tclk(uint8_t index, uint8_t value)
{
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	return usbtoxxx_out_command(USB_TO_MSP430_JTAG, index, &value,
								1, 0);
}

vsf_err_t usbtomsp430jtag_tclk_strobe(uint8_t index, uint16_t cnt)
{
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	return usbtoxxx_special_command(USB_TO_MSP430_JTAG, index,
									(uint8_t*)&cnt, 2, 0, NULL, 0, 0, 0);
}

vsf_err_t usbtomsp430jtag_reset(uint8_t index)
{
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	return usbtoxxx_reset_command(USB_TO_MSP430_JTAG, index,
								  NULL, 0);
}

vsf_err_t usbtomsp430jtag_poll(uint8_t index, uint32_t dr,
							uint32_t mask, uint32_t value, uint8_t len,
							uint16_t poll_cnt, uint8_t toggle_tclk)
{
	uint8_t buff[15];
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	buff[0] = len;
	if (toggle_tclk)
	{
		poll_cnt |= 0x8000;
	}
	SET_LE_U16(&buff[1], poll_cnt);
	SET_LE_U32(&buff[3], dr);
	SET_LE_U32(&buff[7], mask);
	SET_LE_U32(&buff[11], value);
	
	return usbtoxxx_poll_command(USB_TO_MSP430_JTAG, index,
								 buff, 15, NULL, 0);
}

