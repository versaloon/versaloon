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

#include <stdlib.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_log.h"
#include "app_err.h"
#include "port.h"

#include "interfaces.h"
#include "scripts.h"
#include "dal/mic2826/mic2826_drv.h"

static struct mic2826_drv_interface_t mic2826_drv_ifs;
static struct mic2826_drv_param_t mic2826_drv_param;
static struct dal_info_t mic2826_dal_info =
{
	&mic2826_drv_ifs,
	&mic2826_drv_param,
	NULL, NULL
};

VSS_HANDLER(mic2826_vss_init)
{
	VSS_CHECK_ARGC(2);
	
	mic2826_drv_ifs.iic_port = 0;
	mic2826_drv_param.kHz = (uint16_t)strtoul(argv[1], NULL, 0);
	return mic2826_drv.init(&mic2826_dal_info);
}

VSS_HANDLER(mic2826_vss_fini)
{
	VSS_CHECK_ARGC(1);
	return mic2826_drv.fini(&mic2826_dal_info);
}

VSS_HANDLER(mic2826_vss_config)
{
	uint16_t DCDC_mV, LDO1_mV, LDO2_mV, LDO3_mV;
	vsf_err_t err;
	
	VSS_CHECK_ARGC(5);
	
	DCDC_mV = (uint16_t)strtoul(argv[1], NULL, 0);
	LDO1_mV = (uint16_t)strtoul(argv[2], NULL, 0);
	LDO2_mV = (uint16_t)strtoul(argv[3], NULL, 0);
	LDO3_mV = (uint16_t)strtoul(argv[4], NULL, 0);
	
	LOG_PUSH();
	LOG_MUTE();
	err = mic2826_drv.config(&mic2826_dal_info, DCDC_mV, LDO1_mV, LDO2_mV,
								LDO3_mV);
	LOG_POP();
	
	return err;
}

