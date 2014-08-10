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
#if TARGET_ARM_ADI_EN && TARGET_LM3S_EN
#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"
#include "scripts.h"

#include "pgbar.h"

#include "cm.h"
#include "cm_lm3s.h"

#include "cm_internal.h"
#include "lm3s_internal.h"

#include "adi_v5p1.h"
#include "cm_common.h"

ENTER_PROGRAM_MODE_HANDLER(lm3sswj);
LEAVE_PROGRAM_MODE_HANDLER(lm3sswj);
ERASE_TARGET_HANDLER(lm3sswj);
WRITE_TARGET_HANDLER(lm3sswj);
READ_TARGET_HANDLER(lm3sswj);
const struct program_functions_t lm3sswj_program_functions =
{
	NULL,
	ENTER_PROGRAM_MODE_FUNCNAME(lm3sswj),
	LEAVE_PROGRAM_MODE_FUNCNAME(lm3sswj),
	ERASE_TARGET_FUNCNAME(lm3sswj),
	WRITE_TARGET_FUNCNAME(lm3sswj),
	READ_TARGET_FUNCNAME(lm3sswj)
};

struct lm3s_fl_t
{
	uint32_t iap_cnt;
};
struct cm_lm3s_t
{
	// first member must be same as used in cm module
	// because this class in inherited from cm_info_t
	struct cm_info_t cm;
	
	struct lm3s_fl_t fl;
	uint8_t tick_tock;
};

#define LM3S_IAP_BASE			LM3S_SRAM_ADDR
#define LM3S_IAP_COMMAND_OFFSET	100
#define LM3S_IAP_COMMAND_ADDR	(LM3S_IAP_BASE + LM3S_IAP_COMMAND_OFFSET)
#define LM3S_IAP_SYNC_ADDR		(LM3S_IAP_BASE + 120)
#define LM3S_IAP_CNT_ADDR		(LM3S_IAP_BASE + 124)
static uint8_t iap_code[] =
{
							// wait_start:
	0x1D, 0x48,				// ldr		r0, [PC, #116]		// load sync
	0x00, 0x28,				// cmp		r0, #0
	0xFC, 0xD0,				// beq 		wait_start
							// init:
	0x18, 0x4A,				// ldr		r2, [PC, #96]		// laod tgt_addr
	0x18, 0x4B,				// ldr		r3, [PC, #96]		// load src_addr
	0x19, 0x4C,				// ldr		r4, [PC, #100]		// load command
	0x19, 0x4D,				// ldr		r5, [PC, #100]		// load cnt
	0x15, 0x4E,				// ldr		r6, [PC, #84]		// laod address of FMA
							// clear_sync:
	0x19, 0xA0,				// add		r0, PC, #100		// load address of sync
	0x00, 0x21,				// mov		r1, #0
	0x01, 0x60,				// str		r1, [r0]
							// do_operation:
	0x30, 0x46,				// mov		r0, r6				// load address of FMA
	0x02, 0x60,				// str		r2, [r0]			// FMA = tgt_addr
	0x00, 0x1D,				// adds		r0, r0, #4			// &FMD = &FMA + 4
	0x19, 0x68,				// ldr		r1, [r3]			// load src_data
	0x01, 0x60,				// str		r1, [r0]			// FMD = *str_data
	0x00, 0x1D,				// adds		r0, r0, #4			// &FMC = &FMD + 4
	0x04, 0x60,				// str		r4, [r0]			// FMC = command
							// wait_operation_finish:
	0x01, 0x68,				// ldr		r1, [r0]			// r1 = FMC
	0x21, 0x40,				// ands		r1, r1, r4			// r1 = r1 & command
	0xFC, 0xD1,				// bne		wait_operation_finish
							// adjust_param:
	0x12, 0x1D,				// adds		r2, r2, #4			// tgt_addr + 4
	0x1B, 0x1D,				// adds		r3, r3, #4			// src_addr + 4
	0x6D, 0x1E,				// subs		r5, r5, #1			// cnt - 1
	0xF1, 0xD1,				// bne		do_operation
							// adjust_iap_cnt:
	0x12, 0xA0,				// add		r0, PC, #72			// load address of iap_cnt
	0x01, 0x68,				// ldr		r1, [r0]
	0x49, 0x1C,				// adds		r1, r1, #1
	0x01, 0x60,				// str		r1, [r0]
							// wait_next_run:
	0xE1, 0xE7,				// b		wait_start
	0xFE, 0xE7,				// b		$
	// 62 bytes above
	// fill
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00,
	// 100 bytes above
	// parameters
	0x00, 0x00, 0x00, 0x00,	// FMA_addr
	0x00, 0x00, 0x00, 0x00,	// tgt_addr
	0x00, 0x00, 0x00, 0x00,	// src_addr
	0x00, 0x00, 0x00, 0x00,	// command
	0x00, 0x00, 0x00, 0x00,	// cnt
	0x00, 0x00, 0x00, 0x00,	// sync
	0x00, 0x00, 0x00, 0x00	// iap_cnt
};

struct lm3sswj_iap_cmd_t
{
	uint32_t FMA_addr;
	uint32_t tgt_addr;
	uint32_t src_addr;
	uint32_t command;
	uint32_t cnt;
};

static vsf_err_t lm3sswj_iap_poll_finish(struct lm3s_fl_t *fl)
{
	uint32_t iap_cnt;
	
	// read busy
	if (adi_memap_read_reg32(LM3S_IAP_CNT_ADDR, &iap_cnt, 1))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read iap sync");
		return ERRCODE_FAILURE_OPERATION;
	}
	iap_cnt = LE_TO_SYS_U32(iap_cnt);
	if (iap_cnt > fl->iap_cnt)
	{
		cm_dump(LM3S_IAP_BASE, sizeof(iap_code));
		return VSFERR_FAIL;
	}
	return (iap_cnt == fl->iap_cnt) ? VSFERR_NONE : VSFERR_NOT_READY;
}

static vsf_err_t lm3sswj_iap_poll_param_taken(struct lm3s_fl_t *fl)
{
	uint32_t sync;
	
	REFERENCE_PARAMETER(fl);
	
	// read sync
	if (adi_memap_read_reg32(LM3S_IAP_SYNC_ADDR, &sync, 1))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read iap sync");
		return ERRCODE_FAILURE_OPERATION;
	}
	sync = LE_TO_SYS_U32(sync);
	return (0 == sync) ? VSFERR_NONE : VSFERR_NOT_READY;
}

static vsf_err_t lm3sswj_iap_wait_param_taken(struct lm3s_fl_t *fl)
{
	vsf_err_t err;
	uint32_t start, end;
	
	start = (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
	while (1)
	{
		err = lm3sswj_iap_poll_param_taken(fl);
		if (!err)
		{
			break;
		}
		if (err < 0)
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "poll iap param taken");
			return err;
		}
		end = (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
		// wait 1s at most
		if ((end - start) > 1000)
		{
			cm_dump(LM3S_IAP_BASE, sizeof(iap_code));
			LOG_ERROR(ERRMSG_TIMEOUT, "wait for iap param taken");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	
	return VSFERR_NONE;
}

static vsf_err_t lm3sswj_iap_wait_finish(struct lm3s_fl_t *fl)
{
	vsf_err_t err;
	uint32_t start, end;
	
	start = (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
	while (1)
	{
		err = lm3sswj_iap_poll_finish(fl);
		if (!err)
		{
			break;
		}
		if (err && (err != VSFERR_NOT_READY))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "poll iap finish");
			return VSFERR_FAIL;
		}
		end = (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
		// wait 1s at most
		if ((end - start) > 1000)
		{
			cm_dump(LM3S_IAP_BASE, sizeof(iap_code));
			LOG_ERROR(ERRMSG_TIMEOUT, "wait for iap finish");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	
	return VSFERR_NONE;
}

static vsf_err_t lm3sswj_iap_run(struct lm3s_fl_t *fl,
									struct lm3sswj_iap_cmd_t * cmd)
{
	uint32_t buff_tmp[6];
	
	if (lm3sswj_iap_wait_param_taken(fl))
	{
		return VSFERR_FAIL;
	}
	
	memset(buff_tmp, 0, sizeof(buff_tmp));
	buff_tmp[0] = cmd->FMA_addr;
	buff_tmp[1] = cmd->tgt_addr;
	buff_tmp[2] = cmd->src_addr;
	buff_tmp[3] = cmd->command;
	buff_tmp[4] = cmd->cnt;
	buff_tmp[5] = 1;				// sync
	
	// write iap command with sync to target SRAM
	// sync is 4-byte AFTER command in sram
	if (adi_memap_write_buf32(LM3S_IAP_COMMAND_ADDR,
										(uint8_t*)buff_tmp, sizeof(buff_tmp)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load iap cmd to SRAM");
		return ERRCODE_FAILURE_OPERATION;
	}
	fl->iap_cnt++;
	
	return VSFERR_NONE;
}

ENTER_PROGRAM_MODE_HANDLER(lm3sswj)
{
	struct cm_lm3s_t *lm3s = (struct cm_lm3s_t *)context->priv;
	struct lm3s_fl_t *fl = &lm3s->fl;
	uint32_t reg;
	uint8_t verify_buff[sizeof(iap_code)];
	
	if (sizeof(*lm3s) > sizeof(context->priv))
	{
		LOG_BUG("context->priv overflows");
		return VSFERR_FAIL;
	}
	
	fl->iap_cnt = 0;
	
	if (cm_dp_halt())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "halt lm3s");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// disable flash programming interrupts
	reg = 0;
	if (adi_memap_write_reg32(LM3S_FLASHCTL_FCIM, &reg, 0))
	{
		return VSFERR_FAIL;
	}
	reg = LM3S_FLASHCTL_INT_PROGRAMMING | LM3S_FLASHCTL_INT_ACCESS;
	if (adi_memap_write_reg32(LM3S_FLASHCTL_FCMISC, &reg, 1))
	{
		return VSFERR_FAIL;
	}
	
	// unlock
	// 0xFFFFFFFF to FMPRE and FMPPE
	reg = 0xFFFFFFFF;
	if (adi_memap_write_reg32(LM3S_SYSCTL_FMPRE, &reg, 0))
	{
		return VSFERR_FAIL;
	}
	if (adi_memap_write_reg32(LM3S_SYSCTL_FMPPE, &reg, 1))
	{
		return VSFERR_FAIL;
	}
	// commit FMPRE
	reg = 0;
	if (adi_memap_write_reg32(LM3S_FLASHCTL_FMA, &reg, 1))
	{
		return VSFERR_FAIL;
	}
	reg = LM3S_FLASHCTL_FMC_COMT;
	if (adi_memap_write_reg32(LM3S_FLASHCTL_FMC, &reg, 1))
	{
		return VSFERR_FAIL;
	}
	// commit EMPPE
	reg = 1;
	if (adi_memap_write_reg32(LM3S_FLASHCTL_FMA, &reg, 1))
	{
		return VSFERR_FAIL;
	}
	reg = LM3S_FLASHCTL_FMC_COMT;
	if (adi_memap_write_reg32(LM3S_FLASHCTL_FMC, &reg, 1))
	{
		return VSFERR_FAIL;
	}
	
	// write iap_code to target SRAM
	if (adi_memap_write_buf32(LM3S_IAP_BASE, (uint8_t*)iap_code,
											sizeof(iap_code)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load iap_code to SRAM");
		return ERRCODE_FAILURE_OPERATION;
	}
	// verify iap_code
	memset(verify_buff, 0, sizeof(iap_code));
	if (adi_memap_read_buf32(LM3S_IAP_BASE, verify_buff,
										sizeof(iap_code)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read flash_loader");
		return ERRCODE_FAILURE_OPERATION;
	}
	if (memcmp(verify_buff, iap_code, sizeof(iap_code)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "verify flash_loader");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// write pc
	reg = LM3S_IAP_BASE + 1;
	if (cm_write_core_register(CM_COREREG_PC, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "write PC");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	if (cm_dp_resume())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run iap");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return VSFERR_NONE;
}

LEAVE_PROGRAM_MODE_HANDLER(lm3sswj)
{
	struct cm_lm3s_t *lm3s = (struct cm_lm3s_t *)context->priv;
	struct lm3s_fl_t *fl = &lm3s->fl;
	
	REFERENCE_PARAMETER(success);
	
	return lm3sswj_iap_wait_finish(fl);
}

ERASE_TARGET_HANDLER(lm3sswj)
{
	struct cm_lm3s_t *lm3s = (struct cm_lm3s_t *)context->priv;
	struct lm3s_fl_t *fl = &lm3s->fl;
	vsf_err_t err = VSFERR_NONE;
	struct lm3sswj_iap_cmd_t cmd;
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		cmd.FMA_addr = LM3S_FLASHCTL_FMA;
		cmd.tgt_addr = 0;
		cmd.src_addr = 0;
		cmd.command = LM3S_FLASHCTL_FMC_MERASE | LM3S_FLASHCTL_FMC_KEY;
		cmd.cnt = 1;
		if (lm3sswj_iap_run(fl, &cmd) ||
			lm3sswj_iap_wait_finish(fl))
		{
			err = VSFERR_FAIL;
		}
		break;
	default:
		err = VSFERR_FAIL;
		break;
	}
	return err;
}

WRITE_TARGET_HANDLER(lm3sswj)
{
	struct cm_lm3s_t *lm3s = (struct cm_lm3s_t *)context->priv;
	struct lm3s_fl_t *fl = &lm3s->fl;
	vsf_err_t err = VSFERR_NONE;
	struct lm3sswj_iap_cmd_t cmd;
	uint32_t page_size = 512;
	uint32_t ram_addr;
	
	switch (area)
	{
	case APPLICATION_CHAR:
		// check alignment
		if ((size % 4) || (addr % 4))
		{
			LOG_ERROR(ERRMSG_INVALID_TARGET, "flash addr and/or size");
			return VSFERR_FAIL;
		}
		
		cmd.FMA_addr = LM3S_FLASHCTL_FMA;
		cmd.command = LM3S_FLASHCTL_FMC_WRITE | LM3S_FLASHCTL_FMC_KEY;
		cmd.cnt = page_size / 4;
		while (size)
		{
			if (lm3s->tick_tock & 1)
			{
				ram_addr = LM3S_SRAM_ADDR + 1024 + page_size;
			}
			else
			{
				ram_addr = LM3S_SRAM_ADDR + 1024;
			}
			cmd.tgt_addr = addr;
			cmd.src_addr = ram_addr;
			if (adi_memap_write_buf32(ram_addr, buff, page_size) ||
				lm3sswj_iap_run(fl, &cmd))
			{
				err = VSFERR_FAIL;
				break;
			}
			lm3s->tick_tock++;
			size -= page_size;
			buff += page_size;
			addr += page_size;
		}
		break;
	default:
		err = VSFERR_FAIL;
		break;
	}
	
	return err;
}

READ_TARGET_HANDLER(lm3sswj)
{
	struct lm3s_device_info_t lm3s_device;
	uint32_t cur_block_size;
	vsf_err_t err = VSFERR_NONE;
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case CHIPID_CHAR:
		if ((adi_memap_read_reg32(LM3S_SYSCTL_DID0, &lm3s_device.did0, 0))
			|| (adi_memap_read_reg32(LM3S_SYSCTL_DID1, &lm3s_device.did1, 0))
			|| (adi_memap_read_reg32(LM3S_SYSCTL_DC0, &lm3s_device.dc0, 0))
			|| (adi_memap_read_reg32(LM3S_SYSCTL_DC1, &lm3s_device.dc1, 0))
			|| (adi_memap_read_reg32(LM3S_SYSCTL_DC2, &lm3s_device.dc2, 0))
			|| (adi_memap_read_reg32(LM3S_SYSCTL_DC3, &lm3s_device.dc3, 1)))
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		lm3s_device.did0 = LE_TO_SYS_U32(lm3s_device.did0);
		lm3s_device.did1 = LE_TO_SYS_U32(lm3s_device.did1);
		lm3s_device.dc0 = LE_TO_SYS_U32(lm3s_device.dc0);
		lm3s_device.dc1 = LE_TO_SYS_U32(lm3s_device.dc1);
		lm3s_device.dc2 = LE_TO_SYS_U32(lm3s_device.dc2);
		lm3s_device.dc3 = LE_TO_SYS_U32(lm3s_device.dc3);
		if (lm3s_check_device(&lm3s_device))
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		*(uint32_t*)buff = (lm3s_device.did1 >> 16) & 0xFFFF;
		break;
	case APPLICATION_CHAR:
		while (size)
		{
			// cm_get_max_block_size return size in dword(4-byte)
			cur_block_size = cm_get_max_block_size(addr);
			if (cur_block_size > (size >> 2))
			{
				cur_block_size = size;
			}
			else
			{
				cur_block_size <<= 2;
			}
			if (adi_memap_read_buf32(addr, buff, cur_block_size))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION_ADDR, "read flash block",
							addr);
				err = ERRCODE_FAILURE_OPERATION_ADDR;
				break;
			}
			
			size -= cur_block_size;
			addr += cur_block_size;
			buff += cur_block_size;
			pgbar_update(cur_block_size);
		}
		break;
	default:
		err = VSFERR_NONE;
		break;
	}
	return err;
}
#endif
