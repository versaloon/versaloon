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

vsf_err_t usbtojtagraw_init(uint8_t index)
{
	return usbtoxxx_init_command(USB_TO_JTAG_RAW, index);
}

vsf_err_t usbtojtagraw_fini(uint8_t index)
{
	return usbtoxxx_fini_command(USB_TO_JTAG_RAW, index);
}

vsf_err_t usbtojtagraw_config(uint8_t index, uint32_t kHz)
{
	uint8_t cfg_buf[4];
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	SET_LE_U32(&cfg_buf[0], kHz);
	
	return usbtoxxx_conf_command(USB_TO_JTAG_RAW, index, cfg_buf, 4);
}

vsf_err_t usbtojtagraw_execute(uint8_t index, uint8_t *tdi,
							uint8_t *tms, uint8_t *tdo, uint32_t bitlen)
{
	uint16_t bytelen;
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	if (bitlen > 8 * 0xFFFF)
	{
		return VSFERR_FAIL;
	}
	bytelen = (uint16_t)((bitlen + 7) >> 3);
	
	SET_LE_U32(&usbtoxxx_info->cmd_buff[0], bitlen);
	memcpy(usbtoxxx_info->cmd_buff + 4, tdi, bytelen);
	memcpy(usbtoxxx_info->cmd_buff + 4 + bytelen, tms, bytelen);
	
	return usbtoxxx_inout_command(USB_TO_JTAG_RAW, index,
			usbtoxxx_info->cmd_buff, 4 + bytelen * 2, bytelen, tdo, 0,
			bytelen, 0);
}

