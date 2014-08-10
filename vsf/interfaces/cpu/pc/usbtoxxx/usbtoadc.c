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

vsf_err_t usbtoadc_init(uint8_t index)
{
	return usbtoxxx_init_command(USB_TO_ADC, index);
}

vsf_err_t usbtoadc_fini(uint8_t index)
{
	return usbtoxxx_fini_command(USB_TO_ADC, index);
}

vsf_err_t usbtoadc_config(uint8_t index, uint32_t clock_hz,
							uint8_t mode)
{
	uint8_t cmdbuf[5];
	
	SET_LE_U32(cmdbuf, clock_hz);
	cmdbuf[4] = mode;
	return usbtoxxx_conf_command(USB_TO_ADC, index, cmdbuf, 5);
}

vsf_err_t usbtoadc_config_channel(uint8_t index, uint8_t channel,
									uint8_t cycles)
{
	uint8_t cmdbuf[2];
	
	cmdbuf[0] = channel;
	cmdbuf[1] = cycles;
	return usbtoxxx_out_command(USB_TO_ADC, index, cmdbuf, 2, 0);
}

vsf_err_t usbtoadc_calibrate(uint8_t index, uint8_t channel)
{
	uint8_t cmdbuf[1];
	
	cmdbuf[0] = channel;
	return usbtoxxx_special_command(USB_TO_ADC, index, cmdbuf, 1, 0, 
									NULL, 0, 0, 0);
}

vsf_err_t usbtoadc_sample(uint8_t index, uint8_t channel,
							uint32_t *voltage)
{
	uint8_t cmdbuf[1];
	
	cmdbuf[0] = channel;
	return usbtoxxx_inout_command(USB_TO_ADC, index, cmdbuf, 1, 4, 
									(uint8_t *)voltage, 0, 4, 0);
}
