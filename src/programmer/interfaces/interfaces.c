/***************************************************************************
 *   Copyright (C) 2009 - 2010 by Simon Qian <SimonQian@SimonQian.com>     *
 *                                                                         *
 *   This ifsram is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This ifsram is distributed in the hope that it will be useful,        *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this ifsram; if not, write to the                          *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <ctype.h>

#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_log.h"
#include "app_err.h"

#include "interfaces.h"
#include "scripts.h"
#include "target.h"

#include <time.h>
static uint32_t default_tickclk_get_count(void)
{
	return (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
}
// default interfaces supports tickclk only, which will call clock in time.h
// so tickclk will be working without any hardware interface is initialized
static struct interfaces_info_t default_interfaces =
{
	"default", NULL, NULL, false, NULL, 0,
	
	// core
	{NULL},
	// clko
	{NULL},
	// tickclk
	{NULL,  NULL, NULL, NULL, default_tickclk_get_count},
};

#define INTERFACE_DEFAULT				0
struct interfaces_info_t *interfaces_info[] =
{
	// real interfaces
	// versaloon
	&versaloon_interfaces,
	// virtual interfaces
#if TARGET_ARM_ADI_EN
	&vi_stm32_interfaces,
#endif
	// default interfaces
	&default_interfaces,
	NULL
};

struct interfaces_info_t *cur_interface = &default_interfaces;
struct interfaces_info_t *cur_real_interface = NULL;

char* get_interface_name(uint64_t i)
{
#define interface_case(i) case i: return #i
	
	switch (i)
	{
	interface_case(IFS_USART);
	interface_case(IFS_SPI);
	interface_case(IFS_I2C);
	interface_case(IFS_GPIO);
	interface_case(IFS_CAN);
	interface_case(IFS_CLOCK);
	interface_case(IFS_ADC);
	interface_case(IFS_DAC);
	interface_case(IFS_POWER);
	interface_case(IFS_ISSP);
	interface_case(IFS_JTAG_LL);
	interface_case(IFS_JTAG_HL);
	interface_case(IFS_JTAG_RAW);
	interface_case(IFS_C2);
	interface_case(IFS_MSP430_SBW);
	interface_case(IFS_MSP430_JTAG);
	interface_case(IFS_LPC_ICP);
	interface_case(IFS_SWD);
	interface_case(IFS_SWIM);
	interface_case(IFS_HV);
	interface_case(IFS_PDI);
	interface_case(IFS_BDM);
	interface_case(IFS_NAND);
	interface_case(IFS_EBI);
	default:
		return NULL;
	}
}

static struct interfaces_info_t *find_interface_by_name(const char *ifs)
{
	struct interfaces_info_t *interface_tmp;
	uint32_t i;
	
	interface_tmp = NULL;
	if (ifs != NULL)
	{
		for (i = 0; interfaces_info[i] != NULL; i++)
		{
			if (!strcmp(interfaces_info[i]->name, ifs))
			{
				interface_tmp = interfaces_info[i];
				break;
			}
		}
	}
	return interface_tmp;
}

vsf_err_t virtual_interface_init(const char *vifs, const char mode)
{
	struct interfaces_info_t *interface_tmp;
	
	interface_tmp = find_interface_by_name(vifs);
	if ((vifs != NULL) && (NULL == interface_tmp))
	{
		return VSFERR_FAIL;
	}
	
	if (NULL == interface_tmp)
	{
		if (cur_real_interface != NULL)
		{
			cur_interface = cur_real_interface;
		}
	}
	else if (interface_tmp->is_virtual)
	{
		uint32_t i = 0;
		
		if (interface_tmp->mode != NULL)
		{
			while (interface_tmp->mode[i].name != 0)
			{
				if (mode == interface_tmp->mode[i].name)
				{
					break;
				}
				i++;
			}
			if (!interface_tmp->mode[i].name)
			{
				return VSFERR_FAIL;
			}
		}
		
		if (NULL == cur_interface)
		{
			if (interface_init(NULL))
			{
				return VSFERR_FAIL;
			}
		}
		
		if (cur_interface->is_virtual)
		{
			cur_interface = interface_tmp;
		}
		else
		{
			cur_real_interface = cur_interface;
			cur_interface = interface_tmp;
		}
		cur_interface->core.init(&i);
	}
	else
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

vsf_err_t interface_init(const char *ifs)
{
	struct interfaces_info_t *interface_tmp;
	
	interface_tmp = find_interface_by_name(ifs);
	if (NULL == interface_tmp)
	{
		interface_tmp = interfaces_info[INTERFACE_DEFAULT];
	}
	
	if ((interface_tmp != NULL) && (!interface_tmp->is_virtual))
	{
		if ((cur_interface != NULL) && (!cur_interface->is_virtual) &&
			(cur_interface->core.fini != NULL))
		{
			cur_interface->core.fini(cur_interface);
		}
		if ((cur_real_interface != NULL) && (!cur_real_interface->is_virtual) &&
			(cur_real_interface->core.fini != NULL))
		{
			cur_real_interface->core.fini(cur_real_interface);
		}
		cur_interface = NULL;
		cur_real_interface = NULL;
		
		if (interface_tmp->core.init(interface_tmp))
		{
			return VSFERR_FAIL;
		}
		cur_interface = interface_tmp;
		if (interface_tmp->support_mask & IFS_POWER)
		{
			if (vss_run_script("tvcc.get"))
			{
				cur_interface = NULL;
				interface_tmp->core.fini(interface_tmp);
				return VSFERR_FAIL;
			}
		}
		return VSFERR_NONE;
	}
	else
	{
		return VSFERR_FAIL;
	}
}

vsf_err_t interface_assert(struct interfaces_info_t **ifs)
{
	if ((NULL == cur_interface) || (cur_interface == &default_interfaces))
	{
		if (interface_init(NULL) || (NULL == cur_interface))
		{
			cur_interface = NULL;
			vss_set_fatal_error();
			return VSFERR_FAIL;
		}
	}
	if (ifs != NULL)
	{
		*ifs = cur_interface;
	}
	return VSFERR_NONE;
}

void interface_print_list(void)
{
	uint32_t i;
	
	PRINTF("Supported interfaces:\n");
	for (i = 0; interfaces_info[i] != NULL; i++)
	{
		vss_call_notifier(interfaces_info[i]->notifier, "support", NULL);
	}
	PRINTF("\n");
}

void interface_print_help(void)
{
	uint32_t i;
	
	for (i = 0; interfaces_info[i] != NULL; i++)
	{
		vss_call_notifier(interfaces_info[i]->notifier, "help", NULL);
	}
}

