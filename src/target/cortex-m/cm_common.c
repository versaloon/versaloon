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

#include "port.h"
#include "app_cfg.h"
#if TARGET_ARM_ADI_EN
#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "interfaces.h"

#include "adi_v5p1.h"
#include "cm_common.h"

static struct adi_dpif_t *cm_dpif = NULL;
vsf_err_t cm_switch(struct cm_common_info_t *cm_common)
{
	cm_dpif = &cm_common->dpif;
	return adi_switch(&cm_common->adi);
}

vsf_err_t cm_dp_fini(void)
{
	return adi_fini();
}

vsf_err_t cm_dp_init(struct INTERFACES_INFO_T *ifs, struct adi_dpif_t *dp)
{
	uint32_t cpuid, dcb_dhcsr;
	enum adi_dp_target_core_t tgt_core = ADI_DP_INVALID;
	
	*cm_dpif = *dp;
	
	// adi_init will initialize the core type
	if (adi_init(ifs, cm_dpif, &tgt_core))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "initialize cm interface");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// enable debug
	if (adi_memap_read_reg32(CM_DCB_DHCSR, &dcb_dhcsr, 1))
	{
		return VSFERR_FAIL;
	}
	dcb_dhcsr = LE_TO_SYS_U32(dcb_dhcsr);
	
	if (!(dcb_dhcsr & CM_DCB_DHCSR_C_DEBUGEN))
	{
		uint32_t reg0 = 0;
		dcb_dhcsr = (uint32_t)(CM_DCB_DHCSR_DBGKEY | CM_DCB_DHCSR_C_DEBUGEN);
		if (adi_memap_write_reg32(CM_DCB_DHCSR, &dcb_dhcsr, 0) ||
			adi_memap_write_reg32(CM_DCB_DCRDR, &reg0, 1))
		{
			return VSFERR_FAIL;
		}
	}
	
	if (cm_dp_halt() || adi_memap_read_reg32(CM_CPUID, &cpuid, 1))
	{
		return VSFERR_FAIL;
	}
	cpuid = LE_TO_SYS_U32(cpuid);
	// 0xC24 is for CortexM4
	// 0xC23 is for CortexM3
	// 0xC20 is for CortexM0/CortexM0+
	if ((((cpuid >> 4) & 0xC3F) == 0xC24) && (ADI_DP_CM3 == tgt_core))
	{
		tgt_core = ADI_DP_CM4;
		LOG_INFO("CORTEX-M4 r%dp%d processor detected",
					(cpuid >> 20) & 0x0F, (cpuid >> 0) & 0x0F);
	}
	else if ((((cpuid >> 4) & 0xC3F) == 0xC23) && (ADI_DP_CM3 == tgt_core))
	{
		LOG_INFO("CORTEX-M3 r%dp%d processor detected",
					(cpuid >> 20) & 0x0F, (cpuid >> 0) & 0x0F);
	}
	else if ((((cpuid >> 4) & 0xC3F) == 0xC20) && (ADI_DP_CM0 == tgt_core))
	{
		LOG_INFO("CORTEX-M0(+) r%dp%d processor detected",
					(cpuid >> 20) & 0x0F, (cpuid >> 0) & 0x0F);
	}
	else
	{
		LOG_WARNING("Is target a CORTEX-Chip?");
	}
	LOG_INFO(INFOMSG_REG_08X, "CPUID", cpuid);
	
	return VSFERR_NONE;
}

vsf_err_t cm_write_core_register(uint8_t reg_idx, uint32_t *value)
{
	uint32_t dcrdr, reg;
	
	if (adi_memap_read_reg32(CM_DCB_DCRDR, &dcrdr, 1))
	{
		return VSFERR_FAIL;
	}
	
	reg = reg_idx | CM_DCB_DCRSR_WnR;
	adi_memap_write_reg32(CM_DCB_DCRDR, value, 0);
	adi_memap_write_reg32(CM_DCB_DCRSR, &reg, 0);
	
	return adi_memap_write_reg32(CM_DCB_DCRDR, &dcrdr, 1);
}

vsf_err_t cm_read_core_register(uint8_t reg_idx, uint32_t *value)
{
	uint32_t dcrdr, reg;
	
	if (adi_memap_read_reg32(CM_DCB_DCRDR, &dcrdr, 1))
	{
		return VSFERR_FAIL;
	}
	
	reg = reg_idx;
	adi_memap_write_reg32(CM_DCB_DCRSR, &reg, 0);
	adi_memap_read_reg32(CM_DCB_DCRDR, value, 0);
	
	return adi_memap_write_reg32(CM_DCB_DCRDR, &dcrdr, 1);
}

vsf_err_t cm_set_breakpoint(uint8_t bp_idx, uint32_t bp_addr)
{
	uint32_t fp_ctrl, fp_ctrl_enable = 3, compare;
	
	if (adi_memap_read_reg32(CM_FPB_CTRL, &fp_ctrl, 1) ||
		((((fp_ctrl >> 8) & 0x70) | ((fp_ctrl >> 4) & 0x0F)) <= bp_idx))
	{
		return VSFERR_FAIL;
	}
	
	compare = (bp_addr & 0x1FFFFFFC) | ((bp_addr & 2) ? (2 << 30) : (1 << 30)) | 1;
	if (adi_memap_write_reg32(CM_FPB_COMP0 + 4 * bp_idx, &compare, 1))
	{
		return VSFERR_FAIL;
	}
	
	// enable fpb if not enabled
	if (!(fp_ctrl & 1) &&
		adi_memap_write_reg32(CM_FPB_CTRL, &fp_ctrl_enable, 1))
	{
		return VSFERR_FAIL;
	}
	
	return VSFERR_NONE;
}

vsf_err_t cm_clear_breakpoint(uint8_t bp_idx)
{
	uint32_t fp_ctrl, compare = 0;
	
	if (adi_memap_read_reg32(CM_FPB_CTRL, &fp_ctrl, 1) ||
		((((fp_ctrl >> 8) & 0x70) | ((fp_ctrl >> 4) & 0x0F)) <= bp_idx))
	{
		return VSFERR_FAIL;
	}
	
	return adi_memap_write_reg32(CM_FPB_COMP0 + 4 * bp_idx, &compare, 1);
}

uint32_t cm_get_max_block_size(uint32_t address)
{
	return adi_memap_get_max_tar_block_size(address);
}

vsf_err_t cm_reset(void)
{
	uint32_t reg;
	
	// check result should not be enabled here
	// because after reset, dp maybe disabled
	reg = CM_REG_NVIC_AIRCR_VECTKEY | CM_REG_NVIC_AIRCR_SYSRESETREQ;
	if (adi_memap_write_reg32(CM_REG_NVIC_AIRCR, &reg, 0))
	{
		return VSFERR_FAIL;
	}
	return adi_dp_commit();
}

vsf_err_t cm_dp_resume(void)
{
	uint32_t dcb_dhcsr = 0;
	uint8_t wait_halt_clear_delay_in_10ms;
	
	if (adi_memap_read_reg32(CM_DCB_DHCSR, &dcb_dhcsr, 1))
	{
		return VSFERR_FAIL;
	}
	dcb_dhcsr = LE_TO_SYS_U32(dcb_dhcsr);
	if (!(dcb_dhcsr & CM_DCB_DHCSR_C_DEBUGEN))
	{
		return VSFERR_FAIL;
	}
	
	if (dcb_dhcsr & CM_DCB_DHCSR_S_HALT)
	{
		uint32_t corereg20;
		
		// disable exception
		if (cm_read_core_register(CM_COREREG_20, &corereg20))
		{
			return VSFERR_FAIL;
		}
		corereg20 |= CM_PRIMASK_PM;
		if (cm_write_core_register(CM_COREREG_20, &corereg20))
		{
			return VSFERR_FAIL;
		}
		
		// clear halt
		dcb_dhcsr &= ~(0xFFFF0000 | CM_DCB_DHCSR_C_HALT);
		dcb_dhcsr |= CM_DCB_DHCSR_DBGKEY | CM_DCB_DHCSR_C_DEBUGEN;
		adi_memap_write_reg32(CM_DCB_DHCSR, &dcb_dhcsr, 0);
		
		if (adi_memap_read_reg32(CM_DCB_DHCSR, &dcb_dhcsr, 1))
		{
			return VSFERR_FAIL;
		}
		dcb_dhcsr = LE_TO_SYS_U32(dcb_dhcsr);
	}
	// wait halt clear
	wait_halt_clear_delay_in_10ms = 100;	// 1000ms max delay in all
	while ((dcb_dhcsr & CM_DCB_DHCSR_S_HALT) && wait_halt_clear_delay_in_10ms)
	{
		if (adi_memap_read_reg32(CM_DCB_DHCSR, &dcb_dhcsr, 1))
		{
			return VSFERR_FAIL;
		}
		dcb_dhcsr = LE_TO_SYS_U32(dcb_dhcsr);
		wait_halt_clear_delay_in_10ms--;
		sleep_ms(10);
	}
	
	if (dcb_dhcsr & CM_DCB_DHCSR_S_HALT)
	{
		return VSFERR_FAIL;
	}
	else
	{
		return VSFERR_NONE;
	}
}

vsf_err_t cm_dp_halt(void)
{
	uint32_t dcb_dhcsr = 0;
	uint8_t wait_halt_delay_in_10ms;
	
	if (adi_memap_read_reg32(CM_DCB_DHCSR, &dcb_dhcsr, 1))
	{
		return VSFERR_FAIL;
	}
	dcb_dhcsr = LE_TO_SYS_U32(dcb_dhcsr);
	if (!(dcb_dhcsr & CM_DCB_DHCSR_C_DEBUGEN))
	{
		return VSFERR_FAIL;
	}
	
	// halt
	if (!(dcb_dhcsr & CM_DCB_DHCSR_S_HALT))
	{
		dcb_dhcsr &= ~0xFFFF0000;
		dcb_dhcsr |= CM_DCB_DHCSR_DBGKEY | CM_DCB_DHCSR_C_DEBUGEN |
						CM_DCB_DHCSR_C_HALT;
		adi_memap_write_reg32(CM_DCB_DHCSR, &dcb_dhcsr, 0);
		
		if (adi_memap_read_reg32(CM_DCB_DHCSR, &dcb_dhcsr, 1))
		{
			return VSFERR_FAIL;
		}
		dcb_dhcsr = LE_TO_SYS_U32(dcb_dhcsr);
	}
	
	// wait halt
	wait_halt_delay_in_10ms = 100;	// 1000ms max delay in all
	while ((!(dcb_dhcsr & CM_DCB_DHCSR_S_HALT)) && wait_halt_delay_in_10ms)
	{
		if (adi_memap_read_reg32(CM_DCB_DHCSR, &dcb_dhcsr, 1))
		{
			return VSFERR_FAIL;
		}
		dcb_dhcsr = LE_TO_SYS_U32(dcb_dhcsr);
		wait_halt_delay_in_10ms--;
		sleep_ms(10);
	}
	
	if (dcb_dhcsr & CM_DCB_DHCSR_S_HALT)
	{
		return VSFERR_NONE;
	}
	else
	{
		return VSFERR_FAIL;
	}
}

vsf_err_t cm_dump(uint32_t addr, uint32_t size)
{
	uint32_t reg;
	uint8_t i;
	uint8_t *buffer;
	vsf_err_t err = VSFERR_NONE;
	
	buffer = (uint8_t *)malloc(size);
	if (NULL == buffer)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		err = VSFERR_NOT_ENOUGH_RESOURCES;
		goto end;
	}
	
	LOG_INFO("report to author on this message.");
	
	if (cm_dp_halt())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "halt cm");
		err = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	
	for (i = 0; i < 13; i++)
	{
		if (cm_read_core_register(i, &reg))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read register");
			err = ERRCODE_FAILURE_OPERATION;
			goto end;
		}
		reg = LE_TO_SYS_U32(reg);
		LOG_INFO("r%d: %08X", i, reg);
	}
	
	if (cm_read_core_register(CM_COREREG_SP, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read sp");
		err = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	reg = LE_TO_SYS_U32(reg);
	LOG_INFO(INFOMSG_REG_08X, "sp", reg);
	
	if (cm_read_core_register(CM_COREREG_LR, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read lr");
		err = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	reg = LE_TO_SYS_U32(reg);
	LOG_INFO(INFOMSG_REG_08X, "lr", reg);
	
	if (cm_read_core_register(CM_COREREG_PC, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read pc");
		err = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	reg = LE_TO_SYS_U32(reg);
	LOG_INFO(INFOMSG_REG_08X, "pc", reg);
	
	if (cm_read_core_register(CM_COREREG_XPSR, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read xpsr");
		err = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	reg = LE_TO_SYS_U32(reg);
	LOG_INFO(INFOMSG_REG_08X, "xpsr", reg);
	
	if (cm_read_core_register(CM_COREREG_MSP, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read msp");
		err = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	reg = LE_TO_SYS_U32(reg);
	LOG_INFO(INFOMSG_REG_08X, "msp", reg);
	
	if (cm_read_core_register(CM_COREREG_PSP, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read psp");
		err = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	reg = LE_TO_SYS_U32(reg);
	LOG_INFO(INFOMSG_REG_08X, "psp", reg);
	
	if (cm_read_core_register(CM_COREREG_20, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read core_reg 20");
		err = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	reg = LE_TO_SYS_U32(reg);
	LOG_INFO(INFOMSG_REG_02X, "primask", (reg >> 0) & 0xFF);
	LOG_INFO(INFOMSG_REG_02X, "basepri", (reg >> 8) & 0xFF);
	LOG_INFO(INFOMSG_REG_02X, "faultmask", (reg >> 16) & 0xFF);
	LOG_INFO(INFOMSG_REG_02X, "control", (reg >> 24) & 0xFF);
	
	LOG_INFO("SRAM dump at 0x%08X:", addr);
	if (adi_memap_read_buf32(addr, buffer, size))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read sram");
		err = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	LOG_BUF_STD(1, buffer, size, LOG_INFO);
	
end:
	if (buffer != NULL)
	{
		free(buffer);
		buffer = NULL;
	}
	
	return err;
}
#endif
