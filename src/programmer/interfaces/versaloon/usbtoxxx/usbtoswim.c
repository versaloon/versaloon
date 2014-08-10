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

vsf_err_t usbtoswim_init(uint8_t index)
{
	return usbtoxxx_init_command(USB_TO_SWIM, index);
}

vsf_err_t usbtoswim_fini(uint8_t index)
{
	return usbtoxxx_fini_command(USB_TO_SWIM, index);
}

vsf_err_t usbtoswim_config(uint8_t index, uint8_t mHz, uint8_t cnt0,
							uint8_t cnt1)
{
	uint8_t buff[3];
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	buff[0] = mHz;
	buff[1] = cnt0;
	buff[2] = cnt1;
	
	return usbtoxxx_conf_command(USB_TO_SWIM, index, buff, 3);
}

vsf_err_t usbtoswim_srst(uint8_t index)
{
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	return usbtoxxx_reset_command(USB_TO_SWIM, index, NULL, 0);
}

vsf_err_t usbtoswim_wotf(uint8_t index, uint8_t *data, uint16_t bytelen,
						uint32_t addr)
{
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	SET_LE_U16(&usbtoxxx_info->cmd_buff[0], bytelen);
	SET_LE_U32(&usbtoxxx_info->cmd_buff[2], addr);
	memcpy(&usbtoxxx_info->cmd_buff[6], data, bytelen);
	
	return usbtoxxx_out_command(USB_TO_SWIM, index,
								usbtoxxx_info->cmd_buff, bytelen + 6, 0);
}

vsf_err_t usbtoswim_rotf(uint8_t index, uint8_t *data, uint16_t bytelen,
						uint32_t addr)
{
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	SET_LE_U16(&usbtoxxx_info->cmd_buff[0], bytelen);
	SET_LE_U32(&usbtoxxx_info->cmd_buff[2], addr);
	memset(&usbtoxxx_info->cmd_buff[6], 0, bytelen);
	
	return usbtoxxx_in_command(USB_TO_SWIM, index,
			usbtoxxx_info->cmd_buff, bytelen + 6, bytelen, data, 0,
			bytelen, 0);
}

vsf_err_t usbtoswim_sync(uint8_t index, uint8_t mHz)
{
	uint8_t buff[1];
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	buff[0] = mHz;
	
	return usbtoxxx_sync_command(USB_TO_SWIM, index, buff, 1,
									0, NULL);
}

vsf_err_t usbtoswim_enable(uint8_t index)
{
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	return usbtoxxx_enable_command(USB_TO_SWIM, index, NULL, 0);
}

