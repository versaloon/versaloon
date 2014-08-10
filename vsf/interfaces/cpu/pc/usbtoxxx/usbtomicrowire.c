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

vsf_err_t usbtomicrowire_init(uint8_t index)
{
	return usbtoxxx_init_command(USB_TO_MICROWIRE, index);
}

vsf_err_t usbtomicrowire_fini(uint8_t index)
{
	return usbtoxxx_fini_command(USB_TO_MICROWIRE, index);
}

vsf_err_t usbtomicrowire_config(uint8_t index, uint16_t kHz,
								uint8_t sel_polarity)
{
	uint8_t conf[3];
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	conf[0] = sel_polarity;
	SET_LE_U16(&conf[1], kHz);
	
	return usbtoxxx_conf_command(USB_TO_MICROWIRE, index, conf, 3);
}

vsf_err_t usbtomicrowire_transport(uint8_t index,
								uint32_t opcode, uint8_t opcode_bitlen,
								uint32_t addr, uint8_t addr_bitlen,
								uint32_t data, uint8_t data_bitlen,
								uint8_t *reply, uint8_t reply_bitlen)
{
	uint8_t reply_bytelen = (reply_bitlen + 7) / 8;
	uint16_t offset;
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
	if ((opcode_bitlen > 32) || (addr_bitlen > 32) ||
		(data_bitlen > 32) || (reply_bitlen > 32))
	{
		return VSFERR_FAIL;
	}
#endif
	
	usbtoxxx_info->cmd_buff[0] = opcode_bitlen;
	usbtoxxx_info->cmd_buff[1] = addr_bitlen;
	usbtoxxx_info->cmd_buff[2] = data_bitlen;
	usbtoxxx_info->cmd_buff[3] = reply_bitlen;
	
	offset = 4;
	if (opcode_bitlen)
	{
		SET_LE_U32(&usbtoxxx_info->cmd_buff[offset], opcode);
		offset += 4;
	}
	if (addr_bitlen)
	{
		SET_LE_U32(&usbtoxxx_info->cmd_buff[offset], addr);
		offset += 4;
	}
	if (data_bitlen)
	{
		SET_LE_U32(&usbtoxxx_info->cmd_buff[offset], data);
		offset += 4;
	}
	
	return usbtoxxx_inout_command(USB_TO_MICROWIRE, index,
		usbtoxxx_info->cmd_buff, offset, reply_bytelen, reply,
		0, reply_bytelen, 1);
}

vsf_err_t usbtomicrowire_poll(uint8_t index, uint16_t interval_us,
							uint16_t retry_cnt)
{
	uint8_t buff[4];
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	SET_LE_U16(&buff[0], interval_us);
	SET_LE_U16(&buff[2], retry_cnt);
	
	return usbtoxxx_poll_command(USB_TO_MICROWIRE, index, buff, 4,
									NULL, 0);
}

