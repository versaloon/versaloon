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

vsf_err_t usbtodusi_init(uint8_t index)
{
	return usbtoxxx_init_command(USB_TO_DUSI, index);
}

vsf_err_t usbtodusi_fini(uint8_t index)
{
	return usbtoxxx_fini_command(USB_TO_DUSI, index);
}

vsf_err_t usbtodusi_config(uint8_t index, uint32_t kHz, uint8_t mode)
{
	uint8_t conf[5];
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	conf[0] = mode;
	SET_LE_U32(&conf[1], kHz);
	
	return usbtoxxx_conf_command(USB_TO_DUSI, index, conf, 5);
}

vsf_err_t usbtodusi_io(uint8_t index, uint8_t *mo, uint8_t *mi,
					uint8_t *so, uint8_t *si, uint32_t bitlen)
{
	uint16_t bytelen = (uint16_t)((bitlen + 7) / 8);
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	SET_LE_U32(&usbtoxxx_info->cmd_buff[0], bitlen);
	if (mo != NULL)
	{
		memcpy(&usbtoxxx_info->cmd_buff[2], mo, bytelen);
	}
	else
	{
		memset(&usbtoxxx_info->cmd_buff[2], 0xFF, bytelen);
	}
	if (so != NULL)
	{
		memcpy(&usbtoxxx_info->cmd_buff[2 + bytelen], so, bytelen);
	}
	else
	{
		memset(&usbtoxxx_info->cmd_buff[2 + bytelen], 0xFF, bytelen);
	}
	
	if (mi != NULL)
	{
		if (usbtoxxx_add_want_pos(0, bytelen, mi))
		{
			return VSFERR_FAIL;
		}
	}
	if (si != NULL)
	{
		if (usbtoxxx_add_want_pos(bytelen, bytelen, si))
		{
			return VSFERR_FAIL;
		}
	}
	
	return usbtoxxx_inout_command(USB_TO_DUSI, index,
		usbtoxxx_info->cmd_buff, 2 + 2 * bytelen, 2 * bytelen,
		NULL, 0, 0, 0);
}

