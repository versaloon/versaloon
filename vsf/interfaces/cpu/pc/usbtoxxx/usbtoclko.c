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

vsf_err_t usbtoclko_init(uint8_t index)
{
	return usbtoxxx_init_command(USB_TO_CLKO, index);
}

vsf_err_t usbtoclko_fini(uint8_t index)
{
	return usbtoxxx_fini_command(USB_TO_CLKO, index);
}

vsf_err_t usbtoclko_config(uint8_t index, uint32_t kHz)
{
	uint8_t cmdbuf[4];
	
	SET_LE_U32(cmdbuf, kHz);
	return usbtoxxx_conf_command(USB_TO_CLKO, index, cmdbuf, 4);
}

vsf_err_t usbtoclko_enable(uint8_t index)
{
	return usbtoxxx_enable_command(USB_TO_CLKO, index, NULL, 0);
}

vsf_err_t usbtoclko_disable(uint8_t index)
{
	return usbtoxxx_disable_command(USB_TO_CLKO, index, NULL, 0);
}
