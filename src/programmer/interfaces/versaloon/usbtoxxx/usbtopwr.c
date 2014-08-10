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

vsf_err_t usbtopwr_init(uint8_t index)
{
	return usbtoxxx_init_command(USB_TO_POWER, index);
}

vsf_err_t usbtopwr_fini(uint8_t index)
{
	return usbtoxxx_fini_command(USB_TO_POWER, index);
}

vsf_err_t usbtopwr_config(uint8_t index)
{
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	return usbtoxxx_conf_command(USB_TO_POWER, index, NULL, 0);
}

vsf_err_t usbtopwr_set(uint8_t index, uint16_t mV)
{
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	return usbtoxxx_out_command(USB_TO_POWER, index, (uint8_t *)&mV,
								2, 0);
}

vsf_err_t usbtopwr_get(uint8_t index, uint16_t *mV)
{
	uint8_t cmd[2];
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	cmd[0] = cmd[1] = 0;
	return usbtoxxx_in_command(USB_TO_POWER, index, cmd, 2, 2, (uint8_t *)mV,
								0, 2, 0);
}

