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

vsf_err_t usbtoissp_init(uint8_t index)
{
	return usbtoxxx_init_command(USB_TO_ISSP, index);
}

vsf_err_t usbtoissp_fini(uint8_t index)
{
	return usbtoxxx_fini_command(USB_TO_ISSP, index);
}

vsf_err_t usbtoissp_vector(uint8_t index, uint8_t operate,
							uint8_t addr, uint8_t data, uint8_t *buf)
{
	uint8_t cmd_buf[3];
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	cmd_buf[0] = operate;
	cmd_buf[1] = addr;
	cmd_buf[2] = data;
	
	if (operate & ISSP_VECTOR_ATTR_READ)
	{
		return usbtoxxx_inout_command(USB_TO_ISSP, index, cmd_buf,
									  3, 1, buf, 0, 1, 1);
	}
	else
	{
		return usbtoxxx_inout_command(USB_TO_ISSP, index, cmd_buf,
									  3, 0, NULL, 0, 0, 1);
	}
}

vsf_err_t usbtoissp_enter_program_mode(uint8_t index, uint8_t mode)
{
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	return usbtoxxx_in_command(USB_TO_ISSP, index, &mode, 1, 0,
							   NULL, 0, 0, 0);
}

vsf_err_t usbtoissp_leave_program_mode(uint8_t index, uint8_t mode)
{
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	return usbtoxxx_out_command(USB_TO_ISSP, index, &mode, 1, 0);
}

vsf_err_t usbtoissp_wait_and_poll(uint8_t index)
{
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	return usbtoxxx_poll_command(USB_TO_ISSP, index, NULL, 0, NULL,
								 0);
}

