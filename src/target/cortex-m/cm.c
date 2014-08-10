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
#include <ctype.h>

#include "port.h"
#include "app_cfg.h"
#if TARGET_ARM_ADI_EN
#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"
#include "scripts.h"

#include "cm.h"
#include "cm_stm32f1.h"
#include "cm_stm32l1.h"
#include "cm_lpc1000.h"
#include "cm_at91sam3.h"
#include "cm_lm3s.h"
#include "cm_stm32f2.h"
#include "cm_nuc100.h"
#include "cm_nuc400.h"
#include "cm_kinetis.h"

#include "cm_internal.h"

#include "adi_v5p1.h"
#include "cm_common.h"

#define CUR_TARGET_STRING			CM_STRING

ENTER_PROGRAM_MODE_HANDLER(cm);
LEAVE_PROGRAM_MODE_HANDLER(cm);
SWITCH_TARGET_HANDLER(cm);
struct program_functions_t cm_program_functions =
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(cm),
	LEAVE_PROGRAM_MODE_FUNCNAME(cm),
	NULL,
	NULL,
	NULL,
	SWITCH_TARGET_FUNCNAME(cm)
};

const struct cm_param_t cm_chips_param[] = {
#if TARGET_STM32F1_EN
	{
		"cm_stm32f1",					// chip_name
		STM32F1_IRC_KHZ / 6,			// jtag_khz
		{0,1,0,5},						// jtag_pos
		2,								// swd_trn
		0,								// swd_delay
		&stm32f1swj_program_functions	// program_functions
	},
#endif
#if TARGET_STM32F2_EN
	{
		"cm_stm32f2",					// chip_name
		STM32F2_IRC_KHZ / 6,			// jtag_khz
		{0,1,0,5},						// jtag_pos
		2,								// swd_trn
		0,								// swd_delay
		&stm32f2swj_program_functions	// program_functions
	},
#endif
#if TARGET_STM32L1_EN
	{
		"cm_stm32l1",					// chip_name
		STM32L1_IRC_KHZ / 6,			// jtag_khz
		{0,1,0,5},						// jtag_pos
		2,								// swd_trn
		0,								// swd_delay
		&stm32l1swj_program_functions	// program_functions
	},
#endif
#if TARGET_LPC1000_EN
	{
		"cm_lpc1000",					// chip_name
		LPC1000_IRC_KHZ / 6,			// jtag_khz
		{0,0,0,0},						// jtag_pos
		2,								// swd_trn
		1,								// swd_delay
		&lpc1000swj_program_functions	// program_functions
	},
#endif
#if TARGET_AT91SAM3_EN
	{
		"cm_at91sam3",					// chip_name
		AT91SAM3_IRC_KHZ / 6,			// jtag_khz
		{0,0,0,0},						// jtag_pos
		2,								// swd_trn
		0,								// swd_delay
		&at91sam3swj_program_functions	// program_functions
	},
#endif
#if TARGET_LM3S_EN
	{
		"cm_lm3s",						// chip_name
		LM3S_IRC_KHZ / 6,				// jtag_khz
		{0,0,0,0},						// jtag_pos
		2,								// swd_trn
		0,								// swd_delay
		&lm3sswj_program_functions		// program_functions
	},
#endif
#if TARGET_NUC100_EN
	{
		"cm_nuc100",					// chip_name
		0,								// jtag_khz
		{0,0,0,0},						// jtag_pos
		2,								// swd_trn
		1,								// swd_delay
		&nuc100swj_program_functions	// program_functions
	},
#endif
#if TARGET_NUC400_EN
	{
		"cm_nuc400",					// chip_name
		0,								// jtag_khz
		{0,0,0,0},						// jtag_pos
		2,								// swd_trn
		1,								// swd_delay
		&nuc400swj_program_functions	// program_functions
	},
#endif
#if TARGET_KINETIS_EN
	{
		"cm_kinetis",					// chip_name
		KINETIS_IRC_KHZ / 6,			// jtag_khz
		{0,0,0,0},						// jtag_pos
		2,								// swd_trn
		0,								// swd_delay
		&kinetisswj_program_functions	// program_functions
	},
#endif
};

struct cm_param_t * cm_get_param(char *chip_name)
{
	uint8_t i;
	
	for (i = 0; i < dimof(cm_chips_param); i++)
	{
		if (!strcmp(cm_chips_param[i].chip_name, chip_name))
		{
			return (struct cm_param_t *)&cm_chips_param[i];
		}
	}
	return NULL;
}

VSS_HANDLER(cm_chip)
{
	struct cm_info_t *cm;
	uint8_t i;
	
	VSS_CHECK_ARGC(2);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "slot");
		return VSFERR_FAIL;
	}
	cm = (struct cm_info_t *)cur_context->priv;
	
	for (i = 0; i < dimof(cm_chips_param); i++)
	{
		if (!strcmp(cm_chips_param[i].chip_name, argv[1]))
		{
			cm->index = i;
			memcpy(&cm_program_functions,
					cm_chips_param[i].program_functions,
					sizeof(cm_program_functions));
			cm_program_functions.enter_program_mode =
				ENTER_PROGRAM_MODE_FUNCNAME(cm);
			cm_program_functions.leave_program_mode =
				LEAVE_PROGRAM_MODE_FUNCNAME(cm);
			cm_program_functions.switch_target =
				SWITCH_TARGET_FUNCNAME(cm);
			return VSFERR_NONE;
		}
	}
	return VSFERR_FAIL;
}

const struct vss_cmd_t cm_notifier[] =
{
	VSS_CMD(	"chip",
				"select target chip for internal call",
				cm_chip,
				NULL),
	VSS_CMD_END
};

SWITCH_TARGET_HANDLER(cm)
{
	return cm_switch((struct cm_common_info_t *)context->priv);
}

ENTER_PROGRAM_MODE_HANDLER(cm)
{
	struct adi_dpif_t dpif;
	struct program_info_t *pi = context->pi;
	struct cm_info_t *cm = (struct cm_info_t *)context->priv;
	const struct program_functions_t *pf;
	
	if (cm->index >= dimof(cm_chips_param))
	{
		return VSFERR_FAIL;
	}
	pf = cm_chips_param[cm->index].program_functions;
	
	// jtag/swd init
	switch (context->pi->mode_char)
	{
	default:
		LOG_WARNING("debug port not defined, use JTAG by default.");
	case ADI_MODE_CHAR_JTAG:
		dpif.type = ADI_DP_JTAG;
		break;
	case ADI_MODE_CHAR_SWD:
		dpif.type = ADI_DP_SWD;
		break;
	}
	
	switch(dpif.type)
	{
	case ADI_DP_JTAG:
		if (context->pi->frequency)
		{
			dpif.dpif_setting.dpif_jtag_setting.jtag_khz =
				context->pi->frequency;
		}
		else
		{
			dpif.dpif_setting.dpif_jtag_setting.jtag_khz =
				cm_chips_param[cm->index].jtag_khz;
		}
		dpif.dpif_setting.dpif_jtag_setting.jtag_pos.ub =
			cm_chips_param[cm->index].jtag_pos.ub + pi->jtag_pos.ub;
		dpif.dpif_setting.dpif_jtag_setting.jtag_pos.ua =
			cm_chips_param[cm->index].jtag_pos.ua + pi->jtag_pos.ua;
		dpif.dpif_setting.dpif_jtag_setting.jtag_pos.bb =
			cm_chips_param[cm->index].jtag_pos.bb + pi->jtag_pos.bb;
		dpif.dpif_setting.dpif_jtag_setting.jtag_pos.ba =
			cm_chips_param[cm->index].jtag_pos.ba + pi->jtag_pos.ba;
		
		break;
	case ADI_DP_SWD:
		dpif.dpif_setting.dpif_swd_setting.swd_trn =
				cm_chips_param[cm->index].swd_trn;
		if (context->pi->wait_state)
		{
			dpif.dpif_setting.dpif_swd_setting.swd_dly =
				context->pi->wait_state;
		}
		else
		{
			dpif.dpif_setting.dpif_swd_setting.swd_dly =
				cm_chips_param[cm->index].swd_delay;
		}
		dpif.dpif_setting.dpif_swd_setting.swd_retry = 0;
		
		break;
	}
	
	// mode independent
	if (cm_dp_init(context->prog, &dpif))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "initialize cm");
		LOG_ERROR("Maybe your last firmware disable the JTAG/SWD port"
							", try using OpenOCD to erase the firmware.");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	if ((pf->enter_program_mode != NULL)
		&& pf->enter_program_mode(context))
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

LEAVE_PROGRAM_MODE_HANDLER(cm)
{
	struct program_info_t *pi = context->pi;
	struct cm_info_t *cm = (struct cm_info_t *)context->priv;
	const struct program_functions_t *pf;
	vsf_err_t err = VSFERR_NONE;
	
	if (cm->index >= dimof(cm_chips_param))
	{
		return VSFERR_FAIL;
	}
	pf = cm_chips_param[cm->index].program_functions;
	
	if (pf->leave_program_mode != NULL)
	{
		err = pf->leave_program_mode(context, success);
	}
	
	if (pi->execute_flag && success
		&& (context->op->write_operations & APPLICATION))
	{
		uint32_t reg;
		
		if (cm_dp_halt())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "halt cm");
			return ERRCODE_FAILURE_OPERATION;
		}
		if (cm_write_core_register(CM_COREREG_PC, &pi->execute_addr))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "write PC");
			return ERRCODE_FAILURE_OPERATION;
		}
		reg = 0;
		if (cm_read_core_register(CM_COREREG_PC, &reg)
			|| (reg != pi->execute_addr))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "verify written PC");
			return ERRCODE_FAILURE_OPERATION;
		}
		if (cm_dp_resume())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run code");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	
	// jtag/swd fini
	cm_reset();
	cm_dp_fini();
	return err;
}
#endif
