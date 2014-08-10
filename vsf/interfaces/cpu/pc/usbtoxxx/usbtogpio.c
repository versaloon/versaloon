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

vsf_err_t usbtogpio_init(uint8_t index)
{
	return usbtoxxx_init_command(USB_TO_GPIO, index);
}

vsf_err_t usbtogpio_fini(uint8_t index)
{
	return usbtoxxx_fini_command(USB_TO_GPIO, index);
}

vsf_err_t usbtogpio_config_pin(uint8_t index, uint8_t pin_idx,
								uint8_t mode)
{
	uint8_t conf[8];
	uint32_t mask = 0, dir_mask = 0, pull_en_mask = 0, out_mask = 0;
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
	if (pin_idx >= 32)
	{
		return VSFERR_FAIL;
	}
#endif
	
	mask = 1 << pin_idx;
	if ((mode & USB_TO_GPIO_DIR_MSK) == USB_TO_GPIO_OUT)
	{
		dir_mask = mask;
	}
	if ((mode & USB_TO_GPIO_PULLEN_MSK) == USB_TO_GPIO_PULLEN)
	{
		pull_en_mask = mask;
	}
	if ((mode & USB_TO_GPIO_OUT_MSK) == USB_TO_GPIO_OUT1)
	{
		out_mask = mask;
	}
	
	dir_mask &= mask;
	SET_LE_U16(&conf[0], mask);
	SET_LE_U16(&conf[2], dir_mask);
	SET_LE_U16(&conf[4], pull_en_mask);
	SET_LE_U16(&conf[6], out_mask);
	
	return usbtoxxx_conf_command(USB_TO_GPIO, index, conf,
									sizeof(conf));
}

vsf_err_t usbtogpio_config(uint8_t index, uint32_t mask,
							uint32_t dir_mask, uint32_t pull_en_mask,
							uint32_t input_pull_mask)
{
	uint8_t conf[8];
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	dir_mask &= mask;
	SET_LE_U16(&conf[0], mask);
	SET_LE_U16(&conf[2], dir_mask);
	SET_LE_U16(&conf[4], pull_en_mask);
	SET_LE_U16(&conf[6], input_pull_mask);
	
	return usbtoxxx_conf_command(USB_TO_GPIO, index, conf,
									sizeof(conf));
}

vsf_err_t usbtogpio_in(uint8_t index, uint32_t mask, uint32_t *value)
{
	uint8_t buf[2];
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	SET_LE_U16(&buf[0], mask);
	
	return usbtoxxx_in_command(USB_TO_GPIO, index, buf, 2, 2,
							   (uint8_t*)value, 0, 2, 0);
}

vsf_err_t usbtogpio_out(uint8_t index, uint32_t mask, uint32_t value)
{
	uint8_t buf[4];
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	SET_LE_U16(&buf[0], mask);
	SET_LE_U16(&buf[2], value);
	
	return usbtoxxx_out_command(USB_TO_GPIO, index, buf, 4, 0);
}

vsf_err_t usbtogpio_set(uint8_t index, uint32_t pin_mask)
{
	return usbtogpio_out(index, pin_mask, pin_mask);
}

vsf_err_t usbtogpio_clear(uint8_t index, uint32_t pin_mask)
{
	return usbtogpio_out(index, pin_mask, 0);
}
