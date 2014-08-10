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

vsf_err_t usbtonand_init(uint8_t index)
{
	return usbtoxxx_init_command(USB_TO_NAND, index);
}

vsf_err_t usbtonand_fini(uint8_t index)
{
	return usbtoxxx_fini_command(USB_TO_NAND, index);
}

vsf_err_t usbtonand_config(uint8_t index, struct nand_info_t *param)
{
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	SET_LE_U32(&usbtoxxx_info->cmd_buff[0], param->clock_hz);
	usbtoxxx_info->cmd_buff[4] = param->ecc.ecc_enable ? 1 : 0;
	SET_LE_U16(&usbtoxxx_info->cmd_buff[5], param->ecc.ecc_page_size);
	usbtoxxx_info->cmd_buff[7] = param->timing.ale_to_re_cycle;
	usbtoxxx_info->cmd_buff[8] = param->timing.cle_to_re_cycle;
	SET_LE_U16(&usbtoxxx_info->cmd_buff[9], param->timing.setup_cycle);
	SET_LE_U16(&usbtoxxx_info->cmd_buff[11], param->timing.wait_cycle);
	usbtoxxx_info->cmd_buff[13] = param->timing.hold_cycle;
	usbtoxxx_info->cmd_buff[14] = param->timing.hiz_cycle;
	SET_LE_U16(&usbtoxxx_info->cmd_buff[15], 
								param->timing.setup_cycle_attr);
	SET_LE_U16(&usbtoxxx_info->cmd_buff[17], 
								param->timing.wait_cycle_attr);
	usbtoxxx_info->cmd_buff[19] = param->timing.hold_cycle_attr;
	usbtoxxx_info->cmd_buff[20] = param->timing.hiz_cycle_attr;
	
	return usbtoxxx_conf_command(USB_TO_NAND, index,
									usbtoxxx_info->cmd_buff, 21);
}

vsf_err_t usbtonand_write_cmd(uint8_t index, uint8_t *cmd, uint8_t bytelen)
{
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	return usbtoxxx_special_command(USB_TO_NAND, index,
									cmd, bytelen, 0, NULL, 0, 0, 0);
}

vsf_err_t usbtonand_write_addr(uint8_t index, uint8_t *addr, uint8_t bytelen)
{
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	return usbtoxxx_inout_command(USB_TO_NAND, index,
									addr, bytelen, 0, NULL, 0, 0, 0);
}

vsf_err_t usbtonand_write_data(uint8_t index, uint8_t *data, uint32_t bytelen)
{
	uint16_t current_len;
	vsf_err_t err = VSFERR_NONE;
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	while (bytelen)
	{
		current_len =
			(uint16_t)min(bytelen, (uint32_t)(usbtoxxx_info->buff_len - 16));
		
		err = usbtoxxx_out_command(USB_TO_NAND, index, data, current_len, 0);
		if (err)
		{
			return err;
		}
		bytelen -= current_len;
		data += current_len;
	}
	return err;
}

vsf_err_t usbtonand_read_data(uint8_t index, uint8_t *data, uint32_t bytelen)
{
	uint16_t current_len;
	vsf_err_t err = VSFERR_NONE;
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	while (bytelen)
	{
		current_len =
			(uint16_t)min(bytelen, (uint32_t)(usbtoxxx_info->buff_len - 16));
		
		err = usbtoxxx_in_command(USB_TO_NAND, index, data, current_len,
									current_len, data, 0, current_len, 0);
		if (err)
		{
			return err;
		}
		bytelen -= current_len;
		data += current_len;
	}
	return err;
}
