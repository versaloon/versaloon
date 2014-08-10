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

#ifndef __MIC2826_DRV_H_INCLUDED__
#define __MIC2826_DRV_H_INCLUDED__

#include "dal_cfg.h"
#include "../dal.h"

#define MIC2826_CHANNEL_DCDC				0
#define MIC2826_CHANNEL_LDO1				1
#define MIC2826_CHANNEL_LDO2				2
#define MIC2826_CHANNEL_LDO3				3

struct mic2826_drv_t
{
	struct dal_driver_t driver;
	vsf_err_t (*init)(struct dal_info_t *info);
	vsf_err_t (*fini)(struct dal_info_t *info);
	vsf_err_t (*config)(struct dal_info_t *info, uint16_t DCDC_mV, 
						uint16_t LDO1_mV, uint16_t LDO2_mV, uint16_t LDO3_mV);
};

struct mic2826_drv_interface_t
{
	uint8_t iic_port;
};

struct mic2826_drv_param_t
{
	uint16_t kHz;
};

extern const struct mic2826_drv_t mic2826_drv;

#endif	// __MIC2826_DRV_H_INCLUDED__

