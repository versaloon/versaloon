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
#ifndef __CM_COMMON_H_INCLUDED__
#define __CM_COMMON_H_INCLUDED__

#define CM_SRAM_ADDR						0x20000000

#define CM_CPUID							0xE000ED00

#define CM_COREREG_R0						0
#define CM_COREREG_R1						1
#define CM_COREREG_R2						2
#define CM_COREREG_R12						12
#define CM_COREREG_SP						13
#define CM_COREREG_LR						14
#define CM_COREREG_PC						15
#define CM_COREREG_XPSR						16
#define CM_COREREG_MSP						17
#define CM_COREREG_PSP						18
#define CM_COREREG_20						20

#define CM_XPSR_T							0x01000000
#define CM_PRIMASK_PM						0x00000001
#define CM_FAULTMASK_FM						0x00010000

/* Debug Control Block */
#define CM_DCB_DHCSR						0xE000EDF0
#define CM_DCB_DCRSR						0xE000EDF4
#define CM_DCB_DCRDR						0xE000EDF8
#define CM_DCB_DEMCR						0xE000EDFC

#define CM_DCB_DCRSR_WnR					(1 << 16)

/* DCB_DHCSR bit and field definitions */
#define CM_DCB_DHCSR_DBGKEY					(0xA05F << 16)
#define CM_DCB_DHCSR_C_DEBUGEN				(1 << 0)
#define CM_DCB_DHCSR_C_HALT					(1 << 1)
#define CM_DCB_DHCSR_C_STEP					(1 << 2)
#define CM_DCB_DHCSR_C_MASKINTS				(1 << 3)
#define CM_DCB_DHCSR_S_REGRDY				(1 << 16)
#define CM_DCB_DHCSR_S_HALT					(1 << 17)
#define CM_DCB_DHCSR_S_SLEEP				(1 << 18)
#define CM_DCB_DHCSR_S_LOCKUP				(1 << 19)
#define CM_DCB_DHCSR_S_RETIRE_ST			(1 << 24)
#define CM_DCB_DHCSR_S_RESET_ST				(1 << 25)

// FPB
#define CM_FPB_CTRL							0xE0002000
#define CM_FPB_REMAP						0xE0002004
#define CM_FPB_COMP0						0xE0002008
#define CM_FPB_COMP1						0xE000200C
#define CM_FPB_COMP2						0xE0002010
#define CM_FPB_COMP3						0xE0002014
#define CM_FPB_COMP4						0xE0002018
#define CM_FPB_COMP5						0xE000201C
#define CM_FPB_COMP6						0xE0002020
#define CM_FPB_COMP7						0xE0002024

#define CM_REG_NVIC_ICTR					0xE000E004
#define CM_REG_NVIC_ISE0					0xE000E100
#define CM_REG_NVIC_ICSR					0xE000ED04
#define CM_REG_NVIC_AIRCR					0xE000ED0C
#define CM_REG_NVIC_SHCSR					0xE000ED24
#define CM_REG_NVIC_CFSR					0xE000ED28
#define CM_REG_NVIC_MMFSRb					0xE000ED28
#define CM_REG_NVIC_BFSRb					0xE000ED29
#define CM_REG_NVIC_USFSRh					0xE000ED2A
#define CM_REG_NVIC_HFSR					0xE000ED2C
#define CM_REG_NVIC_DFSR					0xE000ED30
#define CM_REG_NVIC_MMFAR					0xE000ED34
#define CM_REG_NVIC_BFAR					0xE000ED38

/* NVIC_AIRCR bits */
#define CM_REG_NVIC_AIRCR_VECTKEY			(0x5FA << 16)
#define CM_REG_NVIC_AIRCR_SYSRESETREQ		(1 << 2)
#define CM_REG_NVIC_AIRCR_VECTCLRACTIVE		(1 << 1)
#define CM_REG_NVIC_AIRCR_VECTRESET			(1 << 0)

struct cm_common_info_t
{
	// first member should be same as used in adi_v5p1 module
	// because this class in inherited from adi_info_t
	struct adi_info_t adi;
	
	struct adi_dpif_t dpif;
};
vsf_err_t cm_switch(struct cm_common_info_t *cm_common);

vsf_err_t cm_dp_parameter_init(struct adi_dpif_t *dp);
vsf_err_t cm_dp_fini(void);
vsf_err_t cm_dp_init(struct INTERFACES_INFO_T *ifs, struct adi_dpif_t *interf);

vsf_err_t cm_dp_halt(void);
vsf_err_t cm_dp_resume(void);
vsf_err_t cm_reset(void);

vsf_err_t cm_read_core_register(uint8_t reg_idx, uint32_t *value);
vsf_err_t cm_write_core_register(uint8_t reg_idx, uint32_t *value);

vsf_err_t cm_set_breakpoint(uint8_t bp_idx, uint32_t bp_addr);
vsf_err_t cm_clear_breakpoint(uint8_t bp_idx);

uint32_t cm_get_max_block_size(uint32_t address);
vsf_err_t cm_dump(uint32_t addr, uint32_t size);

#endif	// __CM_COMMON_H_INCLUDED__

