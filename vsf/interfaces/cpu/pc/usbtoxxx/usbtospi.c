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

vsf_err_t usbtospi_init(uint8_t index)
{
	return usbtoxxx_init_command(USB_TO_SPI, index);
}

vsf_err_t usbtospi_fini(uint8_t index)
{
	return usbtoxxx_fini_command(USB_TO_SPI, index);
}

vsf_err_t usbtospi_get_ability(uint8_t index, struct spi_ability_t *ability)
{
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	if (ability != NULL)
	{
		ability->min_freq_hz = 0;
		ability->max_freq_hz = 100 * 1000 * 1000;
	}
	return VSFERR_NONE;
}

vsf_err_t usbtospi_enable(uint8_t index)
{
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	return VSFERR_NONE;
}

vsf_err_t usbtospi_disable(uint8_t index)
{
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	return VSFERR_NONE;
}

vsf_err_t usbtospi_config(uint8_t index, uint32_t kHz, uint8_t mode)
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
	
	return usbtoxxx_conf_command(USB_TO_SPI, index, conf, 5);
}

vsf_err_t usbtospi_select(uint8_t index, uint8_t cs)
{
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	return usbtoxxx_set_command(USB_TO_SPI, index, &cs, 1);
}

vsf_err_t usbtospi_deselect(uint8_t index, uint8_t cs)
{
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	return usbtoxxx_clear_command(USB_TO_SPI, index, &cs, 1);
}

vsf_err_t usbtospi_io(uint8_t index, uint8_t *out, uint8_t *in,
						uint32_t bytelen)
{
	uint8_t *cmd_ptr;
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	if (NULL == out)
	{
		cmd_ptr = usbtoxxx_info->cmd_buff;
		memset(cmd_ptr, 0xFF, bytelen);
	}
	else
	{
		cmd_ptr = out;
	}
	
	return usbtoxxx_inout_command(USB_TO_SPI, index, cmd_ptr,
			(uint16_t)bytelen, (uint16_t)bytelen, in, 0, (uint16_t)bytelen, 1);
}

