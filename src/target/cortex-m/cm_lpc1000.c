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
#if TARGET_ARM_ADI_EN && TARGET_LPC1000_EN
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
#include "cm_lpc1000.h"
#include "lpc1000.h"

#include "cm_internal.h"
#include "lpc1000_internal.h"

#include "adi_v5p1.h"
#include "cm_common.h"

ENTER_PROGRAM_MODE_HANDLER(lpc1000swj);
LEAVE_PROGRAM_MODE_HANDLER(lpc1000swj);
ERASE_TARGET_HANDLER(lpc1000swj);
WRITE_TARGET_HANDLER(lpc1000swj);
READ_TARGET_HANDLER(lpc1000swj);
const struct program_functions_t lpc1000swj_program_functions =
{
	NULL,
	ENTER_PROGRAM_MODE_FUNCNAME(lpc1000swj),
	LEAVE_PROGRAM_MODE_FUNCNAME(lpc1000swj),
	ERASE_TARGET_FUNCNAME(lpc1000swj),
	WRITE_TARGET_FUNCNAME(lpc1000swj),
	READ_TARGET_FUNCNAME(lpc1000swj)
};

struct lpc1000_fl_t
{
	uint32_t iap_cnt;
};
struct cm_lpc1000_t
{
	// first member must be same as used in cm module
	// because this class in inherited from cm_info_t
	struct cm_info_t cm;
	
	struct lpc1000_fl_t fl;
	uint8_t tick_tock;
};

#define LPC1000_IAP_BASE				LPC1000_SRAM_ADDR
#define LPC1000_IAP_COMMAND_OFFSET		0x80
#define LPC1000_IAP_COMMAND_ADDR		(LPC1000_IAP_BASE + LPC1000_IAP_COMMAND_OFFSET)
#define LPC1000_IAP_RESULT_OFFSET		0x9C
#define LPC1000_IAP_RESULT_ADDR			(LPC1000_IAP_BASE + LPC1000_IAP_RESULT_OFFSET)
#define LPC1000_IAP_ENTRY_OFFSET		0x3C
#define LPC1000_IAP_SYNC_ADDR			(LPC1000_IAP_BASE + 0x98)
#define LPC1000_IAP_CNT_ADDR			(LPC1000_IAP_BASE + 0xB0)
static uint8_t iap_code[] = {
							// wait_start:
	0x25, 0x4C,				// ldr		r4, [PC, #0x94]		// load sync
	0x00, 0x2C,				// cmp		r4, #0
	0xFC, 0xD0,				// beq 		wait_start
							// init:
	0x0D, 0x4A,				// ldr		r2, [PC, #0x38]		// r2: iap_entry
	0x1D, 0xA3,				// add.n	r3, PC, #0x7C		// r3: address of command
	0x24, 0xA1,				// add.n	r1, PC, #0x94		// r4: address of result
	0x0C, 0xA0,				// add.n	r0, PC, #0x38		// r5: address of buff command
	0x22, 0xA6,				// add.n	r6, PC, #0x8C		// r6: address of sync
	0x27, 0xA7,				// add.n	r7, PC, #0xA4		// r7: address of iap_cnt
							// copy command
	0x1C, 0x68,				// ldr		r4, [r3, #0x00]
	0x04, 0x60,				// str		r4, [r0, #0x00]
	0x5C, 0x68,				// ldr		r4, [r3, #0x04]
	0x44, 0x60,				// str		r4, [r0, #0x04]
	0x9C, 0x68,				// ldr		r4, [r3, #0x08]
	0x84, 0x60,				// str		r4, [r0, #0x08]
	0xDC, 0x68,				// ldr		r4, [r3, #0x0C]
	0xC4, 0x60,				// str		r4, [r0, #0x0C]
	0x1C, 0x69,				// ldr		r4, [r3, #0x10]
	0x04, 0x61,				// str		r4, [r0, #0x10]
	0x5C, 0x69,				// ldr		r4, [r3, #0x14]
	0x44, 0x61,				// str		r4, [r0, #0x14]
							// clear_sync:
	0x00, 0x24,				// movs		r4, #0
	0x34, 0x60,				// str		r4, [r6]
							// call_iap:
	0x90, 0x47,				// blx		r2
							// increase_iap_cnt:
	0x3C, 0x68,				// ldr		r4, [r7]
	0x64, 0x1C,				// adds		r4, r4, #1
	0x3C, 0x60,				// str		r4, [r7]
	0xE3, 0xE7,				// b.n		wait_start
							// exit:
							// deadloop:
	0xFE, 0xE7,				// b.n		dead_loop
	0, 0,					// fill
							// parameter
	0, 0, 0, 0,				// address of iap_entry: 0x3C
							// buff command: 0x40
	0, 0, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0,
							// empty
	0, 0, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0,
							// command: 0x80
	0, 0, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0,
							// sync: 0x98
	0, 0, 0, 0,
							// result: 0x9C
	0, 0, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0,
							// iap_cnt: 0xB0
	0, 0, 0, 0
};

static vsf_err_t lpc1000swj_iap_init(struct lpc1000_fl_t *fl)
{
	uint32_t *para_ptr = (uint32_t*)&iap_code[LPC1000_IAP_ENTRY_OFFSET];
	uint8_t verify_buff[sizeof(iap_code)];
	uint32_t reg;
	
	para_ptr[0] = SYS_TO_LE_U32(LPC1000_IAP_ENTRY);
	
	if (cm_dp_halt())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "halt lpc1000");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// write sp
	reg = LPC1000_IAP_BASE + 512;
	if (cm_write_core_register(CM_COREREG_SP, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "write SP");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// write iap_code to target SRAM
	if (adi_memap_write_buf32(LPC1000_IAP_BASE, (uint8_t*)iap_code,
											sizeof(iap_code)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load iap_code to SRAM");
		return ERRCODE_FAILURE_OPERATION;
	}
	// verify iap_code
	memset(verify_buff, 0, sizeof(iap_code));
	if (adi_memap_read_buf32(LPC1000_IAP_BASE, verify_buff,
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
	reg = LPC1000_IAP_BASE + 1;
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

static vsf_err_t lpc1000swj_iap_run(struct lpc1000_fl_t *fl, uint32_t cmd,
									uint32_t param_table[5])
{
	uint32_t buff_tmp[7];
	
	buff_tmp[0] = SYS_TO_LE_U32(cmd);				// iap command
	buff_tmp[1] = SYS_TO_LE_U32(param_table[0]);	// iap parameters
	buff_tmp[2] = SYS_TO_LE_U32(param_table[1]);
	buff_tmp[3] = SYS_TO_LE_U32(param_table[2]);
	buff_tmp[4] = SYS_TO_LE_U32(param_table[3]);
	buff_tmp[5] = SYS_TO_LE_U32(param_table[4]);
	// sync is word AFTER command in sram
	buff_tmp[6] = SYS_TO_LE_U32(1);					// sync
	
	// write iap command with sync to target SRAM
	if (adi_memap_write_buf32(LPC1000_IAP_COMMAND_ADDR,
										(uint8_t*)buff_tmp, sizeof(buff_tmp)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load iap cmd to SRAM");
		return ERRCODE_FAILURE_OPERATION;
	}
	fl->iap_cnt++;
	
	return VSFERR_NONE;
}

static vsf_err_t lpc1000swj_iap_poll_result(struct lpc1000_fl_t *fl,
											uint32_t result_table[7])
{
	uint8_t i;
	
	REFERENCE_PARAMETER(fl);
	
	// read result and sync
	// sync is 4-byte BEFORE result
	if (adi_memap_read_buf32(LPC1000_IAP_SYNC_ADDR, (uint8_t *)result_table, 28))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read iap sync");
		return ERRCODE_FAILURE_OPERATION;
	}
	for (i = 0; i < 7; i++)
	{
		result_table[i] = LE_TO_SYS_U32(result_table[i]);
	}
	return (0 == result_table[0]) ? VSFERR_NONE : VSFERR_NOT_READY;
}

static vsf_err_t lpc1000swj_iap_wait_ready(struct lpc1000_fl_t *fl,
										uint32_t result_table[4], bool last)
{
	vsf_err_t err;
	uint32_t start, end;
	uint32_t buff_tmp[7];
	
	start = (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
	while (1)
	{
		err = lpc1000swj_iap_poll_result(fl, buff_tmp);
		if (!err && (!last || (buff_tmp[6] == fl->iap_cnt)))
		{
			if (buff_tmp[1] != 0)
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION_ERRCODE, "run iap", buff_tmp[1]);
				return VSFERR_FAIL;
			}
			else
			{
				memcpy(result_table, &buff_tmp[2], 16);
			}
			break;
		}
		if (err < 0)
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "poll iap result");
			return VSFERR_FAIL;
		}
		end = (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
		// wait 1s at most
		if ((end - start) > 1000)
		{
			cm_dump(LPC1000_IAP_BASE, sizeof(iap_code));
			LOG_ERROR(ERRMSG_TIMEOUT, "wait for iap ready");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	
	return VSFERR_NONE;
}

static vsf_err_t lpc1000swj_iap_call(struct lpc1000_fl_t *fl, uint32_t cmd,
				uint32_t param_table[5], uint32_t result_table[4], bool last)
{
	if (lpc1000swj_iap_run(fl, cmd, param_table)
		|| lpc1000swj_iap_wait_ready(fl, result_table, last))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run iap command");
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

ENTER_PROGRAM_MODE_HANDLER(lpc1000swj)
{
	struct cm_lpc1000_t *lpc1000 = (struct cm_lpc1000_t *)context->priv;
	struct lpc1000_fl_t *fl = &lpc1000->fl;
	uint32_t reg;
	struct chip_param_t *param = context->param;
	
	if (sizeof(*lpc1000) > sizeof(context->priv))
	{
		LOG_BUG("context->priv overflows");
		return VSFERR_FAIL;
	}
	
	if (lpc1000swj_iap_init(fl))
	{
		return VSFERR_FAIL;
	}
	
	// SYSMEMREMAP should be written to LPC1000_SYSMEMREMAP_USERFLASH
	// to access first sector successfully
	if (param->param[LPC1000_PARAM_SYSMEMREMAP_ADDR])
	{
		reg = param->param[LPC1000_PARAM_FLASHREMAP_VALUE];
		if (adi_memap_write_reg32(
				param->param[LPC1000_PARAM_SYSMEMREMAP_ADDR], &reg, 1))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "write SYSMEMREMAP");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	
	return VSFERR_NONE;
}

LEAVE_PROGRAM_MODE_HANDLER(lpc1000swj)
{
	struct cm_lpc1000_t *lpc1000 = (struct cm_lpc1000_t *)context->priv;
	struct lpc1000_fl_t *fl = &lpc1000->fl;
	uint32_t result_table[4];
	
	REFERENCE_PARAMETER(success);
	
	return lpc1000swj_iap_wait_ready(fl, result_table, true);
}

ERASE_TARGET_HANDLER(lpc1000swj)
{
	struct cm_lpc1000_t *lpc1000 = (struct cm_lpc1000_t *)context->priv;
	struct lpc1000_fl_t *fl = &lpc1000->fl;
	struct program_info_t *pi = context->pi;
	struct program_area_t *flash_area = NULL;
	vsf_err_t err = VSFERR_NONE;
	uint32_t iap_cmd_param[5], iap_reply[4];
	uint32_t sector;
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		flash_area = target_get_program_area(pi, APPLICATION_IDX);
		if (NULL == flash_area)
		{
			return VSFERR_FAIL;
		}
		sector = lpc1000_get_sector_idx_by_addr(context, flash_area->size - 1);
		
		memset(iap_cmd_param, 0, sizeof(iap_cmd_param));
		memset(iap_reply, 0, sizeof(iap_reply));
		iap_cmd_param[0] = 0;				// Start Sector Number
		iap_cmd_param[1] = sector;			// End Sector Number
		iap_cmd_param[2] = pi->kernel_khz;	// CPU Clock Frequency(in kHz)
		if (lpc1000swj_iap_call(fl, LPC1000_IAPCMD_PREPARE_SECTOR,
											iap_cmd_param, iap_reply, false))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "prepare sectors");
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		
		memset(iap_cmd_param, 0, sizeof(iap_cmd_param));
		memset(iap_reply, 0, sizeof(iap_reply));
		iap_cmd_param[0] = 0;				// Start Sector Number
		iap_cmd_param[1] = sector;			// End Sector Number
		iap_cmd_param[2] = pi->kernel_khz;	// CPU Clock Frequency(in kHz)
		if (lpc1000swj_iap_call(fl, LPC1000_IAPCMD_ERASE_SECTOR,
											iap_cmd_param, iap_reply, true))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "erase sectors");
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

WRITE_TARGET_HANDLER(lpc1000swj)
{
	struct cm_lpc1000_t *lpc1000 = (struct cm_lpc1000_t *)context->priv;
	struct lpc1000_fl_t *fl = &lpc1000->fl;
	struct program_info_t *pi = context->pi;
	struct chip_area_info_t *flash_info = NULL;
	uint32_t iap_cmd_param[5], iap_reply[4];
	uint32_t start_sector;
	uint32_t buff_addr;
	uint16_t page_size;
	uint32_t vectors[8];
	
	switch (area)
	{
	case APPLICATION_CHAR:
		flash_info = target_get_chip_area(context->param, APPLICATION_IDX);
		if (NULL == flash_info)
		{
			return VSFERR_FAIL;
		}
		
		page_size = (uint16_t)flash_info->page_size;
		if (size != page_size)
		{
			return VSFERR_FAIL;
		}
		
		memset(iap_cmd_param, 0, sizeof(iap_cmd_param));
		memset(iap_reply, 0, sizeof(iap_reply));
		start_sector = lpc1000_get_sector_idx_by_addr(context, addr);
		iap_cmd_param[0] = start_sector;	// Start Sector Number
		iap_cmd_param[1] = start_sector;	// End Sector Number
		iap_cmd_param[2] = pi->kernel_khz;	// CPU Clock Frequency(in kHz)
		if (lpc1000swj_iap_call(fl, LPC1000_IAPCMD_PREPARE_SECTOR,
											iap_cmd_param, iap_reply, false))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "prepare sectors");
			return ERRCODE_FAILURE_OPERATION;
		}
		
		if (lpc1000->tick_tock & 1)
		{
			buff_addr = LPC1000_IAP_BASE + 512 + page_size;
		}
		else
		{
			buff_addr = LPC1000_IAP_BASE + 512;
		}
		memset(iap_cmd_param, 0, sizeof(iap_cmd_param));
		iap_cmd_param[0] = addr;			// Destination flash address
		iap_cmd_param[1] = buff_addr;		// Source RAM address
		iap_cmd_param[2] = page_size;		// Number of bytes to be written
		iap_cmd_param[3] = pi->kernel_khz;	// CPU Clock Frequency(in kHz)
		if (pi->auto_adjust && !addr)
		{
			uint32_t checksum = 0;
			uint8_t i;
			
			memcpy(vectors, buff, 32);
			for (i = 0; i < 7; i++)
			{
				checksum += GET_LE_U32(&buff[i << 2]);
			}
			SET_LE_U32(&vectors[7], 0 - checksum);
			
			if (adi_memap_write_buf32(buff_addr, (uint8_t *)vectors, 32) ||
				adi_memap_write_buf32(buff_addr + 32, buff + 32, page_size - 32))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run iap");
				return ERRCODE_FAILURE_OPERATION;
			}
		}
		else if (adi_memap_write_buf32(buff_addr, buff, page_size))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run iap");
			return ERRCODE_FAILURE_OPERATION;
		}
		if (lpc1000swj_iap_call(fl, LPC1000_IAPCMD_RAM_TO_FLASH,
											iap_cmd_param, iap_reply, false))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run iap");
			return ERRCODE_FAILURE_OPERATION;
		}
		lpc1000->tick_tock++;
		break;
	default:
		return VSFERR_FAIL;
	}
	
	return VSFERR_NONE;
}

READ_TARGET_HANDLER(lpc1000swj)
{
	struct cm_lpc1000_t *lpc1000 = (struct cm_lpc1000_t *)context->priv;
	struct lpc1000_fl_t *fl = &lpc1000->fl;
	vsf_err_t err = VSFERR_NONE;
	uint32_t cur_block_size;
	uint32_t iap_cmd_param[5], iap_reply[4];
	
	switch (area)
	{
	case CHIPID_CHAR:
		memset(iap_cmd_param, 0, sizeof(iap_cmd_param));
		memset(iap_reply, 0, sizeof(iap_reply));
		if (lpc1000swj_iap_call(fl, LPC1000_IAPCMD_READ_BOOTVER,
											iap_cmd_param, iap_reply, true))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read bootver");
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		LOG_INFO(INFOMSG_BOOTLOADER_VERSION, (iap_reply[0] >> 8) & 0xFF,
					(iap_reply[0] >> 0) & 0xFF);
		
		memset(iap_cmd_param, 0, sizeof(iap_cmd_param));
		memset(iap_reply, 0, sizeof(iap_reply));
		if (lpc1000swj_iap_call(fl, LPC1000_IAPCMD_READ_ID,
											iap_cmd_param, iap_reply, true))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read id");
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		*(uint32_t*)buff = iap_reply[0];
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
	case UNIQUEID_CHAR:
		memset(iap_cmd_param, 0, sizeof(iap_cmd_param));
		if (lpc1000swj_iap_call(fl, LPC1000_IAPCMD_READ_SERIAL,
										iap_cmd_param, (uint32_t *)buff, true))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read serialnum");
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
