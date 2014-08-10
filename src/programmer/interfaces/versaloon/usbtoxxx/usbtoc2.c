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

vsf_err_t usbtoc2_init(uint8_t index)
{
	return usbtoxxx_init_command(USB_TO_C2, index);
}

vsf_err_t usbtoc2_fini(uint8_t index)
{
	return usbtoxxx_fini_command(USB_TO_C2, index);
}

vsf_err_t usbtoc2_readaddr(uint8_t index, uint8_t *data)
{
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	if (data != NULL)
	{
		return usbtoxxx_in_command(USB_TO_C2, index, NULL, 0, 1,
									data, 0, 1, 0);
	}
	else
	{
		return usbtoxxx_in_command(USB_TO_C2, index, NULL, 0, 1,
									NULL, 0, 0, 0);
	}
}

vsf_err_t usbtoc2_writeaddr(uint8_t index, uint8_t addr)
{
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	return usbtoxxx_out_command(USB_TO_C2, index, &addr, 1, 0);
}

vsf_err_t usbtoc2_readdata(uint8_t index, uint8_t *buf, uint8_t len)
{
	uint8_t cmdbuf[5];
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
	if ((len > 4) || (0 == len))
	{
		LOG_BUG(ERRMSG_INVALID_VALUE, len, "C2 data length(1..4)");
		return VSFERR_INVALID_PARAMETER;
	}
#endif
	
	cmdbuf[0] = 0x80 | len;
	memset(cmdbuf + 1, 0, len);
	
	return usbtoxxx_inout_command(USB_TO_C2, index, cmdbuf,
									1 + len, len, buf, 0, len, 0);
}

vsf_err_t usbtoc2_writedata(uint8_t index, uint8_t *buf, uint8_t len)
{
	uint8_t cmdbuf[5];
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
	if ((len > 4) || (0 == len))
	{
		LOG_BUG(ERRMSG_INVALID_VALUE, len, "C2 data length(1..4)");
		return VSFERR_INVALID_PARAMETER;
	}
#endif
	
	cmdbuf[0] = len;
	memcpy(cmdbuf + 1, buf, len);
	
	return usbtoxxx_inout_command(USB_TO_C2, index, cmdbuf,
									1 + len, 0, NULL, 0, 0, 0);
}

