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
#if TARGET_ARM_ADI_EN && TARGET_KINETIS_EN
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
#include "cm_kinetis.h"

#include "adi_v5p1.h"
#include "cm_common.h"

#include "cm_internal.h"
#include "kinetis_internal.h"

ENTER_PROGRAM_MODE_HANDLER(kinetisswj);
LEAVE_PROGRAM_MODE_HANDLER(kinetisswj);
ERASE_TARGET_HANDLER(kinetisswj);
WRITE_TARGET_HANDLER(kinetisswj);
READ_TARGET_HANDLER(kinetisswj);
const struct program_functions_t kinetisswj_program_functions =
{
	NULL,
	ENTER_PROGRAM_MODE_FUNCNAME(kinetisswj),
	LEAVE_PROGRAM_MODE_FUNCNAME(kinetisswj),
	ERASE_TARGET_FUNCNAME(kinetisswj),
	WRITE_TARGET_FUNCNAME(kinetisswj),
	READ_TARGET_FUNCNAME(kinetisswj)
};

struct kinetis_fl_t
{
	uint32_t iap_cnt;
};
struct cm_kinetis_t
{
	// first member must be same as used in cm module
	// because this class in inherited from cm_info_t
	struct cm_info_t cm;
	
	struct kinetis_fl_t fl;
	uint8_t tick_tock;
};

#define KINETIS_IAP_BASE				0x20000000
#define KINETIS_IAP_COMMAND_OFFSET		0x80
#define KINETIS_IAP_COMMAND_ADDR		(KINETIS_IAP_BASE + KINETIS_IAP_COMMAND_OFFSET)
#define KINETIS_IAP_SYNC_ADDR			(KINETIS_IAP_BASE + 0x90)
#define KINETIS_IAP_CNT_ADDR			(KINETIS_IAP_BASE + 0x94)
static uint8_t iap_code[] =
{
							// init
	0x71, 0x24,				// mov		r4, #0x70
	0x80, 0x25,				// mov		r5, #0x80
							// wait_start
	0x22, 0x48,				// ldr		r0, [PC, #0x88]
	0x00, 0x28,				// cmp		r0, #0
	0xFC, 0xD0,				// beq		wait_start
							// update_command
	0x1D, 0x4B,				// ldr		r3, [PC, #0x74]
	0x1D, 0x48,				// ldr		r0, [PC, #0x74]
	0x1E, 0x49,				// ldr		r1, [PC, #0x78]
	0x1E, 0x4A,				// ldr		r2, [PC, #0x78]
							// clear_sync
	0x00, 0x26,				// mov		r6, #0
	0x1E, 0xA7,				// add		r7, PC, #0x78
	0x3E, 0x60,				// str		r6, [r7, #0]
							// do_operation
	0x58, 0x60,				// str		r0, [r3, #4]
	0x99, 0x60,				// str		r1, [r3, #8]
	0xDA, 0x60,				// str		r2, [r3, #12]
	0x1C, 0x70,				// strb		r4, [r3, #0]
	0x1D, 0x70,				// strb		r5, [r3, #0]
							// wait_ready
	0x1E, 0x78,				// ldrb		r6, [r3, #0]
	0xB0, 0x46,				// mov		r8, r6
	0x2E, 0x40,				// and		r6, r6, r5
	0xFB, 0xD0,				// beq		wait_ready
							// check_fail
	0x46, 0x46,				// mov		r6, r8
	0x26, 0x40,				// and		r6, r6, r4
	0x01, 0xD0,				// beq		increase_iap_cnt
							// write_fail
	0x19, 0xA7,				// add		r7, PC, #0x64
	0x3E, 0x60,				// str		r6, [r7, #0]
							// increase_iap_cnt
	0x17, 0xA7,				// add		r7, PC, #0x5C
	0x3E, 0x68,				// ldr		r6, [r7, #0]
	0x76, 0x1C,				// add		r6, r6, #1
	0x3E, 0x60,				// str		r6, [r7, #0]
	0xE2, 0xE7,				// b		wait_start
							// exit
	0xFE, 0xE7,				// b		exit
	
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

	0x00, 0x00, 0x00, 0x00,	// REG_BASE_ADDR
	0x00, 0x00, 0x00, 0x00,	// FCCOB_W0
	0x00, 0x00, 0x00, 0x00,	// FCCOB_W1
	0x00, 0x00, 0x00, 0x00,	// FCCOB_W2
	0x00, 0x00, 0x00, 0x00,	// SYNC
	0x00, 0x00, 0x00, 0x00,	// IAP_CNT
	0x00, 0x00, 0x00, 0x00,	// FAIL
};

struct kinetisswj_iap_cmd_t
{
	uint32_t reg_base;
	uint32_t fccob[3];
};

struct kinetisswj_iap_rpl_t
{
	uint32_t sync;
	uint32_t iap_cnt;
	uint32_t fail;
	uint32_t result;
};

static vsf_err_t kinetisswj_iap_init(struct kinetis_fl_t *fl)
{
	uint8_t verify_buff[sizeof(iap_code)];
	uint32_t reg;
	
	if (cm_dp_halt())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "halt lpc1000");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// write iap_code to target SRAM
	if (adi_memap_write_buf32(KINETIS_IAP_BASE, (uint8_t*)iap_code,
											sizeof(iap_code)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load iap_code to SRAM");
		return ERRCODE_FAILURE_OPERATION;
	}
	// verify iap_code
	memset(verify_buff, 0, sizeof(iap_code));
	if (adi_memap_read_buf32(KINETIS_IAP_BASE, verify_buff,
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
	reg = KINETIS_IAP_BASE + 1;
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
	fl->iap_cnt = 0;
	return VSFERR_NONE;
}

static vsf_err_t kinetisswj_iap_run(struct kinetis_fl_t *fl,
									struct kinetisswj_iap_cmd_t *cmd)
{
	uint32_t buff_tmp[5];
	
	buff_tmp[0] = SYS_TO_LE_U32(cmd->reg_base);		// reg base
	buff_tmp[1] = SYS_TO_LE_U32(cmd->fccob[0]);		// iap parameters
	buff_tmp[2] = SYS_TO_LE_U32(cmd->fccob[1]);
	buff_tmp[3] = SYS_TO_LE_U32(cmd->fccob[2]);
	// sync is word AFTER command in sram
	buff_tmp[4] = SYS_TO_LE_U32(1);					// sync
	
	// write iap command with sync to target SRAM
	if (adi_memap_write_buf32(KINETIS_IAP_COMMAND_ADDR,
										(uint8_t*)buff_tmp, sizeof(buff_tmp)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load iap cmd to SRAM");
		return ERRCODE_FAILURE_OPERATION;
	}
	fl->iap_cnt++;
	
	return VSFERR_NONE;
}

static vsf_err_t kinetisswj_iap_poll_result(struct kinetis_fl_t *fl,
											struct kinetisswj_iap_rpl_t *result)
{
	uint32_t result_buff[4];
	
	REFERENCE_PARAMETER(fl);
	
	if (adi_memap_read_buf32(KINETIS_IAP_SYNC_ADDR, (uint8_t *)result_buff,
										sizeof(result_buff)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read iap sync");
		return ERRCODE_FAILURE_OPERATION;
	}
	result->sync = LE_TO_SYS_U32(result_buff[0]);
	result->iap_cnt = LE_TO_SYS_U32(result_buff[1]);
	result->fail = LE_TO_SYS_U32(result_buff[2]);
	result->result = LE_TO_SYS_U32(result_buff[3]);
	
	return (0 == result->sync) ? VSFERR_NONE : VSFERR_NOT_READY;
}

static vsf_err_t kinetisswj_iap_wait_ready(struct kinetis_fl_t *fl,
								struct kinetisswj_iap_rpl_t *reply, bool last)
{
	vsf_err_t err;
	uint32_t start, end;
	
	start = interfaces->tickclk.get_count();
	while (1)
	{
		err = kinetisswj_iap_poll_result(fl, reply);
		if (!err && (!last || (reply->iap_cnt == fl->iap_cnt)))
		{
			if (reply->fail != 0)
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION_ERRCODE, "run iap",
													reply->fail);
				return VSFERR_FAIL;
			}
			return VSFERR_NONE;
		}
		if (err < 0)
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "poll iap result");
			return VSFERR_FAIL;
		}
		end = interfaces->tickclk.get_count();
		// wait 1s at most
		if ((end - start) > 1000)
		{
			cm_dump(KINETIS_IAP_BASE, sizeof(iap_code));
			LOG_ERROR(ERRMSG_TIMEOUT, "wait for iap ready");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	
	return VSFERR_NONE;
}

static vsf_err_t kinetisswj_iap_call(struct kinetis_fl_t *fl,
								struct kinetisswj_iap_cmd_t *cmd,
								struct kinetisswj_iap_rpl_t *reply, bool last)
{
	if (kinetisswj_iap_run(fl, cmd)
		|| kinetisswj_iap_wait_ready(fl, reply, last))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run iap command");
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

ENTER_PROGRAM_MODE_HANDLER(kinetisswj)
{
	struct cm_kinetis_t *kinetis = (struct cm_kinetis_t *)context->priv;
	struct kinetis_fl_t *fl = &kinetis->fl;
	
	if (sizeof(*kinetis) > sizeof(context->priv))
	{
		LOG_BUG("context->priv overflows");
		return VSFERR_FAIL;
	}
	
	return kinetisswj_iap_init(fl);
}

LEAVE_PROGRAM_MODE_HANDLER(kinetisswj)
{
	struct cm_kinetis_t *kinetis = (struct cm_kinetis_t *)context->priv;
	struct kinetis_fl_t *fl = &kinetis->fl;
	struct kinetisswj_iap_rpl_t reply;
	
	REFERENCE_PARAMETER(success);
	
	return kinetisswj_iap_wait_ready(fl, &reply, true);
}

ERASE_TARGET_HANDLER(kinetisswj)
{
	struct cm_kinetis_t *kinetis = (struct cm_kinetis_t *)context->priv;
	struct kinetis_fl_t *fl = &kinetis->fl;
	struct kinetisswj_iap_cmd_t cmd;
	struct kinetisswj_iap_rpl_t reply;
	struct chip_area_info_t *flash_info = NULL;
	vsf_err_t err = VSFERR_NONE;
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	
	switch (area)
	{
	case FUSE_CHAR:
		break;
	case APPLICATION_CHAR:
		flash_info = target_get_chip_area(context->param, APPLICATION_IDX);
		if (NULL == flash_info)
		{
			return VSFERR_FAIL;
		}
		
		cmd.reg_base = KINETIS_FTF_BASE;
		cmd.fccob[0] = KINETIS_FTF_CMD_ERSALL;
		cmd.fccob[1] = 0;
		cmd.fccob[2] = 0;
		if (kinetisswj_iap_call(fl, &cmd, &reply, true))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "prepare sectors");
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		break;
	default:
		err = VSFERR_FAIL;
		break;
	}
	return err;
}

WRITE_TARGET_HANDLER(kinetisswj)
{
//	struct cm_kinetis_t *kinetis = (struct cm_kinetis_t *)context->priv;
//	struct kinetis_fl_t *fl = &kinetis->fl;
	struct chip_area_info_t *flash_info = NULL;
	vsf_err_t err = VSFERR_NONE;
	
	switch (area)
	{
	case FUSE_CHAR:
		break;
	case APPLICATION_CHAR:
		flash_info = target_get_chip_area(context->param, APPLICATION_IDX);
		if ((NULL == flash_info) || (size != flash_info->page_size))
		{
			return VSFERR_FAIL;
		}
		break;
	default:
		err = VSFERR_FAIL;
		break;
	}
	
	return err;
}

READ_TARGET_HANDLER(kinetisswj)
{
	struct program_info_t *pi = context->pi;
	struct program_area_t *sram_area = NULL, *flash_area = NULL;
	uint32_t mcuid = 0, fcfg1 = 0;
	uint32_t cur_block_size;
	vsf_err_t err = VSFERR_NONE;
	
	switch (area)
	{
	case CHIPID_CHAR:
		if ((adi_memap_read_reg32(KINETIS_SIM_SDID, &mcuid, 1)) ||
			(adi_memap_read_reg32(KINETIS_SIM_FCFG1, &fcfg1, 1)))
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		*(uint32_t *)buff = mcuid;
		kinetis_print_device(mcuid, fcfg1);
		
		sram_area = target_get_program_area(pi, SRAM_IDX);
		if (sram_area != NULL)
		{
			sram_area->size = kinetis_get_sram_size(mcuid);
		}
		
		flash_area = target_get_program_area(pi, APPLICATION_IDX);
		if (flash_area != NULL)
		{
			flash_area->size = kinetis_get_flash_size(fcfg1);
		}
		break;
	case FUSE_CHAR:
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
		}
		break;
	case UNIQUEID_CHAR:
		if (adi_memap_read_buf32(KINETIS_SIM_UID, buff, 12))
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		break;
	default:
		err = VSFERR_FAIL;
		break;
	}
	return err;
}
#endif
