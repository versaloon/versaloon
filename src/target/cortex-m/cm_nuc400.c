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
#if TARGET_ARM_ADI_EN && TARGET_NUC400_EN
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
#include "cm_nuc400.h"

#include "cm_internal.h"
#include "nuc400_internal.h"

#include "adi_v5p1.h"
#include "cm_common.h"

ENTER_PROGRAM_MODE_HANDLER(nuc400swj);
LEAVE_PROGRAM_MODE_HANDLER(nuc400swj);
ERASE_TARGET_HANDLER(nuc400swj);
WRITE_TARGET_HANDLER(nuc400swj);
READ_TARGET_HANDLER(nuc400swj);
const struct program_functions_t nuc400swj_program_functions =
{
	NULL,
	ENTER_PROGRAM_MODE_FUNCNAME(nuc400swj),
	LEAVE_PROGRAM_MODE_FUNCNAME(nuc400swj),
	ERASE_TARGET_FUNCNAME(nuc400swj),
	WRITE_TARGET_FUNCNAME(nuc400swj),
	READ_TARGET_FUNCNAME(nuc400swj)
};

struct nuc400_fl_t
{
	uint32_t iap_cnt;
};
struct cm_nuc400_t
{
	// first member must be same as used in cm module
	// because this class in inherited from cm_info_t
	struct cm_info_t cm;
	
	struct nuc400_fl_t fl;
	bool apmode;
	bool ldmode;
	uint8_t tick_tock;
};

#define NUC400_IAP_BASE				CM_SRAM_ADDR
#define NUC400_IAP_COMMAND_OFFSET	0x60
#define NUC400_IAP_COMMAND_ADDR		(NUC400_IAP_BASE + NUC400_IAP_COMMAND_OFFSET)
#define NUC400_IAP_SYNC_ADDR		(NUC400_IAP_BASE + 0x74)
#define NUC400_IAP_CNT_ADDR			(NUC400_IAP_BASE + 0x78)
#define NUC400_IAP_FAIL_ADDR		(NUC400_IAP_BASE + 0x7C)
static uint8_t iap_code[] =
{
							// wait_start:
	0x1C, 0x48,				// ldr		r0, [PC, #112]		// load sync
	0x00, 0x28,				// cmp		r0, #0
	0xFC, 0xD0,				// beq 		wait_start
							// init:
	0x17, 0x4A,				// ldr		r2, [PC, #92]		// laod tgt_addr
	0x17, 0x4B,				// ldr		r3, [PC, #92]		// load src_addr
	0x18, 0x4C,				// ldr		r4, [PC, #96]		// load command
	0x18, 0x4D,				// ldr		r5, [PC, #96]		// load cnt
	0x14, 0x4E,				// ldr		r6, [PC, #80]		// laod ISP_BASE
							// clear_sync:
	0x18, 0xA0,				// add		r0, PC, #96			// load address of sync
	0x00, 0x21,				// mov		r1, #0
	0x01, 0x60,				// str		r1, [r0]
							// do_operation:
	0x72, 0x60,				// str		r2, [r6, #0x4]		// set ISPADR
	0x12, 0x1D,				// adds		r2, r2, #4			// tgt_addr += 4
	0xF4, 0x60,				// str		r4, [r6, #0xC]		// set ISPCMD
	0x1B, 0x42,				// tst		r3, r3
	0x02, 0xD0,				// beq		isp_go
	0x18, 0x68,				// ldr		r0, [r3]
	0x1B, 0x1D,				// adds		r3, r3, #4			// src_addr ++ 4
	0xB0, 0x60,				// str		r0, [r6, #0x8]		// set ISPDAT
							// isp_go:
	0x01, 0x20,				// mov		r0, #1
	0x30, 0x61,				// str		r0, [r6, #0x10]		// ISPTRG.ISPGO = 1
							// wait_ready:
	0x30, 0x69,				// ldr		r0, [r6, #0x10]
	0x01, 0x21,				// mov		r1, #1
	0x08, 0x40,				// ands		r0, r0, r1			// read ISPTRG.ISPGO
	0xFB, 0xD1,				// bne		wait_ready
	0x30, 0x68,				// ldr		r0, [r6]
	0x40, 0x21,				// mov		r1, #0x40
	0x08, 0x40,				// ands		r0, r0, r1			// read ISPCON.ISPFF
	0x06, 0xD1,				// bne		set_fail
							// adjust_param:
	0x6D, 0x1E,				// subs		r5, r5, #1
	0xEB, 0xD1,				// bne		do_operation
							// adjust_iap_cnt:
	0x0E, 0xA0,				// add		r0, PC, #56
	0x01, 0x68,				// ldr		r1, [r0]
	0x49, 0x1C,				// adds		r1, r1, #1
	0x01, 0x60,				// str		r1, [r0]
	0xDB, 0xE7,				// b		wait_start
							// set_fail:
	0x01, 0x20,				// mov		r0, #1
	0x0C, 0xA1,				// add		r1, PC, #48
	0x08, 0x60,				// str		r0, [r1]
							// dead:
	0xFE, 0xE7,				// b		dead
	// fill
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	// parameters at 0x60
	0x00, 0x00, 0x00, 0x00,	// ISP_BASE
	0x00, 0x00, 0x00, 0x00,	// tgt_addr
	0x00, 0x00, 0x00, 0x00,	// src_addr
	0x00, 0x00, 0x00, 0x00,	// command
	0x00, 0x00, 0x00, 0x00,	// cnt
	0x00, 0x00, 0x00, 0x00,	// sync
	0x00, 0x00, 0x00, 0x00,	// iap_cnt
	0x00, 0x00, 0x00, 0x00	// fail
};

struct nuc400swj_iap_cmd_t
{
	uint32_t tgt_addr;
	uint32_t src_addr;
	uint32_t command;
	uint32_t cnt;
};

static vsf_err_t nuc400swj_iap_poll_finish(struct nuc400_fl_t *fl)
{
	uint32_t iap_cnt, iap_fail;
	
	// read fail
	if (adi_memap_read_reg32(NUC400_IAP_FAIL_ADDR, &iap_fail, 1))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read iap fail");
		return ERRCODE_FAILURE_OPERATION;
	}
	if (iap_fail)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run iap");
		cm_dump(NUC400_IAP_BASE, sizeof(iap_code));
		return VSFERR_FAIL;
	}
	
	// read busy
	if (adi_memap_read_reg32(NUC400_IAP_CNT_ADDR, &iap_cnt, 1))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read iap sync");
		return ERRCODE_FAILURE_OPERATION;
	}
	iap_cnt = LE_TO_SYS_U32(iap_cnt);
	if (iap_cnt > fl->iap_cnt)
	{
		cm_dump(NUC400_IAP_BASE, sizeof(iap_code));
		return VSFERR_FAIL;
	}
	return (iap_cnt == fl->iap_cnt) ? VSFERR_NONE : VSFERR_NOT_READY;
}

static vsf_err_t nuc400swj_iap_poll_param_taken(struct nuc400_fl_t *fl)
{
	uint32_t sync;
	
	REFERENCE_PARAMETER(fl);
	
	// read sync
	if (adi_memap_read_reg32(NUC400_IAP_SYNC_ADDR, &sync, 1))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read iap sync");
		return ERRCODE_FAILURE_OPERATION;
	}
	sync = LE_TO_SYS_U32(sync);
	return (0 == sync) ? VSFERR_NONE : VSFERR_NOT_READY;
}

static vsf_err_t nuc400swj_iap_wait_param_taken(struct nuc400_fl_t *fl)
{
	vsf_err_t err;
	uint32_t start, end;
	
	start = interfaces->tickclk.get_count();
	while (1)
	{
		err = nuc400swj_iap_poll_param_taken(fl);
		if (!err)
		{
			break;
		}
		if (err < 0)
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "poll iap param taken");
			return err;
		}
		end = interfaces->tickclk.get_count();
		// wait 1s at most
		if ((end - start) > 1000)
		{
			cm_dump(NUC400_IAP_BASE, sizeof(iap_code));
			LOG_ERROR(ERRMSG_TIMEOUT, "wait for iap param taken");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	
	return VSFERR_NONE;
}

static vsf_err_t nuc400swj_iap_wait_finish(struct nuc400_fl_t *fl)
{
	vsf_err_t err;
	uint32_t start, end;
	
	if (!fl->iap_cnt)
	{
		return VSFERR_NONE;
	}
	
	start = interfaces->tickclk.get_count();
	while (1)
	{
		err = nuc400swj_iap_poll_finish(fl);
		if (!err)
		{
			break;
		}
		if (err < 0)
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "poll iap finish");
			return VSFERR_FAIL;
		}
		end = interfaces->tickclk.get_count();
		// wait 1s at most
		if ((end - start) > 1000)
		{
			cm_dump(NUC400_IAP_BASE, sizeof(iap_code));
			LOG_ERROR(ERRMSG_TIMEOUT, "wait for iap finish");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	
	return VSFERR_NONE;
}

static vsf_err_t nuc400swj_iap_run(struct nuc400_fl_t *fl,
									struct nuc400swj_iap_cmd_t * cmd)
{
	uint32_t buff_tmp[6];
	
	if (nuc400swj_iap_wait_param_taken(fl))
	{
		return VSFERR_FAIL;
	}
	
	memset(buff_tmp, 0, sizeof(buff_tmp));
	buff_tmp[0] = NUC400_REG_FMC_BA;
	buff_tmp[1] = cmd->tgt_addr;
	buff_tmp[2] = cmd->src_addr;
	buff_tmp[3] = cmd->command;
	buff_tmp[4] = cmd->cnt;
	buff_tmp[5] = 1;				// sync
	
	// write iap command with sync to target SRAM
	// sync is 4-byte AFTER command in sram
	if (adi_memap_write_buf32(NUC400_IAP_COMMAND_ADDR,
										(uint8_t*)buff_tmp, sizeof(buff_tmp)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load iap cmd to SRAM");
		return ERRCODE_FAILURE_OPERATION;
	}
	fl->iap_cnt++;
	
	return VSFERR_NONE;
}

static vsf_err_t nuc400swj_unlock(void)
{
	uint32_t data[3] = {NUC400_REG_REGWRPROT_D0,
						NUC400_REG_REGWRPROT_D1,
						NUC400_REG_REGWRPROT_D2};
	
	if (adi_memap_write_reg32(NUC400_REG_REGWRPROT, &data[0], 0) ||
		adi_memap_write_reg32(NUC400_REG_REGWRPROT, &data[1], 0) ||
		adi_memap_write_reg32(NUC400_REG_REGWRPROT, &data[2], 1))
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t nuc400swj_reset(void)
{
	uint32_t iprstc1 = NUC400_REG_IPRSTC1_CUP_RST;
	
	return adi_memap_write_reg32(NUC400_REG_IPRSTC1, &iprstc1, 1);
}

static vsf_err_t nuc400swj_reset_to_aprom(void)
{
	uint32_t ispcon = NUC400_REG_ISPCON_BS_APROM;
	
	if (adi_memap_write_reg32(NUC400_REG_ISPCON, &ispcon, 1) ||
		nuc400swj_reset())
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t nuc400swj_reset_to_ldrom(void)
{
	uint32_t ispcon = NUC400_REG_ISPCON_BS_LDROM;
	
	if (adi_memap_write_reg32(NUC400_REG_ISPCON, &ispcon, 1) ||
		nuc400swj_reset())
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t nuc400swj_init_iap(void)
{
	uint32_t reg;
	uint8_t verify_buff[sizeof(iap_code)];
	
	if (cm_dp_halt())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "halt nuc400");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// enable isp clock and ispen bit
	reg = NUC400_REG_AHBCLK_ISPEN;
	if (adi_memap_write_reg32(NUC400_REG_AHBCLK, &reg, 0))
	{
		return VSFERR_FAIL;
	}
	reg = NUC400_REG_ISPCON_ISPFF | NUC400_REG_ISPCON_LDUEN |
			NUC400_REG_ISPCON_CFGUEN | NUC400_REG_ISPCON_ISPEN;
	if (adi_memap_write_reg32(NUC400_REG_ISPCON, &reg, 1))
	{
		return VSFERR_FAIL;
	}
	
	// write iap_code
	if (adi_memap_write_buf32(NUC400_IAP_BASE, (uint8_t*)iap_code,
											sizeof(iap_code)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load iap_code to SRAM");
		return ERRCODE_FAILURE_OPERATION;
	}
	// verify iap_code
	memset(verify_buff, 0, sizeof(iap_code));
	if (adi_memap_read_buf32(NUC400_IAP_BASE, verify_buff,
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
	reg = NUC400_IAP_BASE + 1;
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

ENTER_PROGRAM_MODE_HANDLER(nuc400swj)
{
	struct cm_nuc400_t *nuc400 = (struct cm_nuc400_t *)context->priv;
	struct nuc400_fl_t *fl = &nuc400->fl;
	
	if (sizeof(*nuc400) > sizeof(context->priv))
	{
		LOG_BUG("context->priv overflows");
		return VSFERR_FAIL;
	}
	
	fl->iap_cnt = 0;
	nuc400->apmode = false;
	nuc400->ldmode = false;
	
	return VSFERR_NONE;
}

LEAVE_PROGRAM_MODE_HANDLER(nuc400swj)
{
	struct cm_nuc400_t *nuc400 = (struct cm_nuc400_t *)context->priv;
	struct nuc400_fl_t *fl = &nuc400->fl;
	
	REFERENCE_PARAMETER(success);
	
	return nuc400swj_iap_wait_finish(fl);
}

ERASE_TARGET_HANDLER(nuc400swj)
{
	struct cm_nuc400_t *nuc400 = (struct cm_nuc400_t *)context->priv;
	struct nuc400_fl_t *fl = &nuc400->fl;
	vsf_err_t err = VSFERR_NONE;
	struct nuc400swj_iap_cmd_t cmd;
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	
	cmd.command = NUC400_REG_ISPCMD_PAGE_ERASE;
	cmd.src_addr = 0;
	cmd.cnt = 1;
	switch (area)
	{
	case BOOTLOADER_CHAR:
		if (!nuc400->ldmode)
		{
			if (nuc400swj_unlock() || nuc400swj_reset_to_aprom() ||
				nuc400swj_unlock() || nuc400swj_init_iap())
			{
				return VSFERR_FAIL;
			}
			
			nuc400->ldmode = true;
			nuc400->apmode = false;
		}
		
		cmd.tgt_addr = addr;
		if (nuc400swj_iap_run(fl, &cmd) ||
			nuc400swj_iap_wait_finish(fl))
		{
			err = VSFERR_FAIL;
		}
		break;
	case APPLICATION_CHAR:
		if (!nuc400->apmode)
		{
			if (nuc400swj_unlock() || nuc400swj_reset_to_ldrom() ||
				nuc400swj_unlock() || nuc400swj_init_iap())
			{
				return VSFERR_FAIL;
			}
			
			nuc400->apmode = true;
			nuc400->ldmode = false;
		}
		
		cmd.tgt_addr = addr;
		if (nuc400swj_iap_run(fl, &cmd) ||
			nuc400swj_iap_wait_finish(fl))
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

WRITE_TARGET_HANDLER(nuc400swj)
{
	struct cm_nuc400_t *nuc400 = (struct cm_nuc400_t *)context->priv;
	struct nuc400_fl_t *fl = &nuc400->fl;
	vsf_err_t err = VSFERR_NONE;
	struct nuc400swj_iap_cmd_t cmd;
	uint32_t page_size = 512;
	uint32_t ram_addr;
	
	switch (area)
	{
	case APPLICATION_CHAR:
		if (!nuc400->apmode)
		{
			if (nuc400swj_unlock() || nuc400swj_reset_to_ldrom() ||
				nuc400swj_unlock() || nuc400swj_init_iap())
			{
				return VSFERR_FAIL;
			}
			
			nuc400->apmode = true;
			nuc400->ldmode = false;
		}
		
		// check alignment
		if ((size % 4) || (addr % 4))
		{
			LOG_ERROR(ERRMSG_INVALID_TARGET, "flash addr and/or size");
			return VSFERR_FAIL;
		}
		
		cmd.command = NUC400_REG_ISPCMD_PROGRAM;
		cmd.cnt = page_size / 4;
		while (size)
		{
			if (nuc400->tick_tock & 1)
			{
				ram_addr = CM_SRAM_ADDR + 1024 + page_size;
			}
			else
			{
				ram_addr = CM_SRAM_ADDR + 1024;
			}
			cmd.tgt_addr = addr;
			cmd.src_addr = ram_addr;
			if (adi_memap_write_buf32(ram_addr, buff, page_size) ||
				nuc400swj_iap_run(fl, &cmd))
			{
				err = VSFERR_FAIL;
				break;
			}
			nuc400->tick_tock++;
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

READ_TARGET_HANDLER(nuc400swj)
{
	uint32_t cur_block_size;
	vsf_err_t err = VSFERR_NONE;
	uint32_t pdid;
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case CHIPID_CHAR:
		if (adi_memap_read_reg32(NUC400_REG_PDID, &pdid, 1))
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		*(uint32_t*)buff = pdid;
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
