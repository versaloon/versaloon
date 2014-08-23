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
#if TARGET_ARM_ADI_EN && TARGET_AT91SAM3_EN
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
#include "cm_at91sam3.h"
#include "at91sam3.h"

#include "cm_internal.h"
#include "at91sam3_internal.h"

#include "adi_v5p1.h"
#include "cm_common.h"

ENTER_PROGRAM_MODE_HANDLER(at91sam3swj);
LEAVE_PROGRAM_MODE_HANDLER(at91sam3swj);
ERASE_TARGET_HANDLER(at91sam3swj);
WRITE_TARGET_HANDLER(at91sam3swj);
READ_TARGET_HANDLER(at91sam3swj);
const struct program_functions_t at91sam3swj_program_functions =
{
	NULL,
	ENTER_PROGRAM_MODE_FUNCNAME(at91sam3swj),
	LEAVE_PROGRAM_MODE_FUNCNAME(at91sam3swj),
	ERASE_TARGET_FUNCNAME(at91sam3swj),
	WRITE_TARGET_FUNCNAME(at91sam3swj),
	READ_TARGET_FUNCNAME(at91sam3swj)
};

#define AT91SAM3_IAP_BASE				AT91SAM3_SRAM_ADDR
#define AT91SAM3_IAP_PARAM_OFFSET		128
#define AT91SAM3_IAP_COMMAND_ADDR		(AT91SAM3_IAP_BASE + 144)
#define AT91SAM3_IAP_SYNC_ADDR			(AT91SAM3_IAP_BASE + 176)
#define AT91SAM3_IAP_DATA_ADDR			(AT91SAM3_IAP_BASE + 180)
#define AT91SAM3_IAP_TYPE				0
static uint8_t iap_code[] = {
							// wait_start:
	0x2B, 0x48,				// ldr		r0, [PC, #XX]		// load sync
	0x00, 0x28,				// cmp		r0, #0
	0xFC, 0xD1,				// bne 		wait_start
	0x00, 0x00,
	// 8 bytes above
							// init:
	0x24, 0x48,				// ldr		r0, [PC, #XX]		// load number of data
	0x00, 0x28,				// cmp		r0, #0
	0x0D, 0xD0,				// beq		run_command
	0x00, 0x00,
	// 16 bytes above
	0x23, 0x49,				// ldr		r1, [PC, #XX]		// load target addr to write data
	0x24, 0x4A,				// ldr		r2, [PC, #XX]		// load address of data
							// copy_data:
	0x13, 0x68,				// ldr		r3, [r2]
	0x0B, 0x60,				// str		r3, [r1]
	0x02, 0xF1, 0x04, 0x02,	// add		r2, r2, #4
	0x01, 0xF1, 0x04, 0x01,	// add		r1, r1, #4
	0xA0, 0xF1, 0x01, 0x00,	// sub		r0, r0, #1
	0x00, 0x28,				// cmp		r0, #0
	0xF5, 0xD1,				// bne		copy_data
	0x00, 0x00,
	// 42 bytes above
							// run_command:
	0x19, 0x4F,				// ldr		r7, [PC, #XX]		// load eefc_base
	0x19, 0x48,				// ldr		r0, [PC, #XX]		// load command
	0x78, 0x60,				// str		r0, [r7, #4]		// write command to eefc_fcr
							// wait_eefc_ready:
	0xB8, 0x68,				// ldr		r0, [r7, #8]		// read eefc_fsr
	0x00, 0xF0, 0x01, 0x01,	// and		r1, r0, #1			// get frdy in eefc_fsr
	0x00, 0x29,				// cmp		r1, #0
	0xFA, 0xD0,				// beq		wait_eefc_ready
	0x00, 0x00,
	// 60 bytes above
	0x10, 0x49,				// ldr		r1, [PC, #XX]		// load address of sync
	0x16, 0x4A,				// ldr		r2, [PC, #XX]		// load number of result
	0x00, 0x2A,				// cmp		r2, #0
	0x0D, 0xD0,				// beq		exit
	0x00, 0x00,
	// 70 bytes above
	0x4F, 0xF0, 0x00, 0x03,	// mov		r3, #0
	0x16, 0x4C,				// ldr		r4, [PC, #XX]		// load address of data
							// read_result:
	0xFD, 0x68,				// ldr		r5, [r7, #0x0C]		// read eefc_frr
	0x4F, 0xEA, 0x83, 0x06,	// lsl		r6, r3, #2
	0x26, 0x44,				// add		r6, r6, r4
	0x35, 0x60,				// str		r5, [r6]
	0x03, 0xF1, 0x01, 0x03,	// add		r3, r3, #1
	0x93, 0x42,				// cmp		r3, r2
	0xF6, 0xD1,				// bne		read_result
	0x00, 0x00,
	// 96 bytes above
							// exit:
	0x08, 0x60,				// str		r0, [r1]			// write iap_result to sync
	
	0xCD, 0xE7,				// b		wait_start
	0xFE, 0xE7,				// b		$
	// 102 bytes above
	0, 0, 0, 0,				// fill 26 bytes
	0, 0, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0,
	0, 0,
	// 128 bytes above
							// parameter
	0, 0, 0, 0,				// address of sync
	0, 0, 0, 0,				// reserved0
	0, 0, 0, 0,				// reserved1
	0, 0, 0, 0,				// reserved2
	// 144 bytes above
							// iap_command parameter:
	0, 0, 0, 0,				// eefc base
	0, 0, 0, 0,				// command
	0, 0, 0, 0,				// number of result
	0, 0, 0, 0,				// number of data
	0, 0, 0, 0,				// target addr to write data
	0, 0, 0, 0,				// address of data
	0, 0, 0, 0,				// reserved0
	0, 0, 0, 0,				// reserved1
	// 176 bytes above
							// iap_reply
	1, 0, 6, 7,				// result status(sync)
	0, 3, 1, 2,				// data from here
};

struct at91sam3swj_iap_command_t
{
	uint32_t eefc_base;
	uint32_t iap_command;
	uint32_t result_num;
	uint32_t data_num;
	uint32_t target_addr;
	uint32_t address_of_data;
};
struct at91sam3swj_iap_reply_t
{
	uint32_t *data;
	uint32_t data_num;
};

#if 0
static vsf_err_t at91sam3swj_iap_run(struct at91sam3swj_iap_command_t *cmd)
{
	uint32_t buff_tmp[9];
	
	memset(buff_tmp, 0, sizeof(buff_tmp));
	buff_tmp[0] = cmd->eefc_base;
	buff_tmp[1] = cmd->iap_command | AT91SAM3_EEFC_FKEY;
	buff_tmp[2] = cmd->result_num;
	buff_tmp[3] = cmd->data_num;
	buff_tmp[4] = cmd->target_addr;
	buff_tmp[5] = cmd->address_of_data;
	buff_tmp[6] = 0;				// reserved0
	buff_tmp[7] = 0;				// reserved1
	buff_tmp[8] = 0;				// sync
	
	// write iap command with sync to target SRAM
	// sync is 4-byte AFTER command in sram
	if (adi_memap_write_buf(AT91SAM3_IAP_COMMAND_ADDR,
										(uint8_t*)buff_tmp, sizeof(buff_tmp)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load iap cmd to SRAM");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return VSFERR_NONE;
}

static vsf_err_t at91sam3swj_iap_poll_result(struct at91sam3swj_iap_reply_t *reply)
{
	uint32_t buff_tmp[256 + 4];
	uint32_t data_size;
	
	if (NULL == fail)
	{
		LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
		return VSFERR_INVALID_PARAMETER;
	}
	
	if ((reply != NULL) && (reply->data_num > (dimof(buff_tmp) - 1)))
	{
		LOG_BUG("buff size is not enough for this call.");
		return VSFERR_FAIL;
	}
	
	data_size = 4;
	if (reply != NULL)
	{
		data_size = (1 + reply->data_num) * sizeof(uint32_t);
	}
	
	// read result and sync
	// sync is 4-byte BEFORE result
	if (adi_memap_read_buf(AT91SAM3_IAP_SYNC_ADDR,
										(uint8_t *)buff_tmp, data_size))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read iap sync");
		return ERRCODE_FAILURE_OPERATION;
	}
	// buff_tmp[0] is sync, which is eefc_frr
	if (buff_tmp[0] != 0)
	{
		if (buff_tmp[0] != 1)
		{
			cm_dump(AT91SAM3_IAP_BASE, sizeof(iap_code) + 256);
			LOG_ERROR(ERRMSG_FAILURE_OPERATION_ERRCODE, "call iap",
						buff_tmp[1]);
			return ERRCODE_FAILURE_OPERATION;
		}
		
		if ((reply != NULL) && (reply->data != NULL))
		{
			memcpy(reply->data, &buff_tmp[1],
					reply->data_num * sizeof(uint32_t));
		}
		return VSFERR_NONE;
	}
	
	return VSFERR_NOT_READY;
}

static vsf_err_t at91sam3swj_iap_wait_ready(struct at91sam3swj_iap_reply_t *reply)
{
	vsf_err_t err;
	uint32_t start, end;
	
	start = interfaces->tickclk.get_count();
	while (1)
	{
		err = at91sam3swj_iap_poll_result(reply);
		if (!err)
		{
			break;
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
			cm_dump(AT91SAM3_IAP_BASE, sizeof(iap_code) + 256);
			LOG_ERROR(ERRMSG_TIMEOUT, "wait for iap ready");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	
	return VSFERR_NONE;
}

static vsf_err_t at91sam3swj_iap_call(struct at91sam3swj_iap_command_t *cmd,
							struct at91sam3swj_iap_reply_t *reply)
{	
	if (at91sam3swj_iap_run(cmd) || at91sam3swj_iap_wait_ready(reply))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run iap command");
		return VSFERR_FAIL;
	}
	
	return VSFERR_NONE;
}
#else
static vsf_err_t at91sam3swj_iap_call(struct at91sam3swj_iap_command_t *cmd,
							struct at91sam3swj_iap_reply_t *reply)
{
	uint32_t reg;
	uint32_t start, end;
	uint32_t i;
	
	reg = cmd->iap_command | AT91SAM3_EEFC_FKEY;
	if (adi_memap_write_reg32(cmd->eefc_base + AT91SAM3_EEFC_FCR_OFFSET,
								&reg, 1))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "write fcr");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	start = interfaces->tickclk.get_count();
	do
	{
		reg = 0;
		if (adi_memap_read_reg32(cmd->eefc_base + AT91SAM3_EEFC_FSR_OFFSET,
									&reg, 1))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read fsr");
			return ERRCODE_FAILURE_OPERATION;
		}
		reg = LE_TO_SYS_U32(reg);
		end = interfaces->tickclk.get_count();
	} while (!(reg & 1) && ((end - start) < 500));
	if (!(reg & 1) || (reg & 0x60))
	{
		return VSFERR_FAIL;
	}
	
	if ((reply != NULL) && (reply->data != NULL) && (reply->data_num > 0))
	{
		for (i = 0; i < reply->data_num; i++)
		{
			if (adi_memap_read_reg32(cmd->eefc_base + AT91SAM3_EEFC_FRR_OFFSET,
										&reply->data[i], 1))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read frr");
				return ERRCODE_FAILURE_OPERATION;
			}
			reply->data[i] = LE_TO_SYS_U32(reply->data[i]);
		}
	}
	
	return VSFERR_NONE;
}
#endif

ENTER_PROGRAM_MODE_HANDLER(at91sam3swj)
{
	struct chip_param_t *param = context->param;
	uint32_t i;
	struct at91sam3swj_iap_command_t command;
	uint32_t *para_ptr = (uint32_t*)&iap_code[AT91SAM3_IAP_PARAM_OFFSET];
	uint32_t reg;
	uint8_t verify_buff[sizeof(iap_code)];
	
	if (cm_dp_halt())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "halt at91sam3");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	para_ptr[0] = SYS_TO_LE_U32(AT91SAM3_IAP_SYNC_ADDR);
	
	// write sp
	reg = AT91SAM3_SRAM_ADDR + 1024;
	if (cm_write_core_register(CM_COREREG_SP, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "write SP");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// write iap_code to target SRAM
	if (adi_memap_write_buf32(AT91SAM3_IAP_BASE, (uint8_t*)iap_code,
							sizeof(iap_code)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load iap_code to SRAM");
		return ERRCODE_FAILURE_OPERATION;
	}
	// verify iap_code
	memset(verify_buff, 0, sizeof(iap_code));
	if (adi_memap_read_buf32(AT91SAM3_IAP_BASE, verify_buff,
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
	reg = AT91SAM3_IAP_BASE + 1;
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
	
	for (i = 0; i < param->param[AT91SAM3_PARAM_PLANE_NUMBER]; i++)
	{
		memset(&command, 0, sizeof(command));
		command.eefc_base = param->param[i + 1];
		
		LOG_PUSH();
		LOG_MUTE();
		reg = 0;
		if (adi_memap_read_reg32(command.eefc_base + AT91SAM3_EEFC_FSR_OFFSET,
									&reg, 1))
		{
			reg = 0;
		}
		reg = LE_TO_SYS_U32(reg);
		LOG_POP();
		if (!(reg & 1))
		{
			// issue stop command
			command.iap_command = AT91SAM3_EEFC_CMD_SPUI;
			if (at91sam3swj_iap_call(&command, NULL))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "stop eefc");
				return ERRCODE_FAILURE_OPERATION;
			}
			
			// try again
			reg = 0;
			if (adi_memap_read_reg32(
						command.eefc_base + AT91SAM3_EEFC_FSR_OFFSET, &reg, 1))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read fsr");
				return ERRCODE_FAILURE_OPERATION;
			}
			reg = LE_TO_SYS_U32(reg);
			if (!(reg & 1))
			{
				LOG_ERROR("eefc is busy");
				return VSFERR_FAIL;
			}
		}
	}
	
	return VSFERR_NONE;
}

LEAVE_PROGRAM_MODE_HANDLER(at91sam3swj)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	return VSFERR_NONE;
}

ERASE_TARGET_HANDLER(at91sam3swj)
{
	struct chip_param_t *param = context->param;
	struct operation_t *op = context->op;
	struct at91sam3swj_iap_command_t command;
	uint32_t eefc_base;
	uint32_t area_mask;
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	
	area_mask = target_area_mask(area);
	if (!area_mask)
	{
		return VSFERR_FAIL;
	}
	
	switch (area)
	{
	case EEPROM_CHAR:
		eefc_base = param->param[AT91SAM3_PARAM_PLANE1_CONTROL];
		goto do_erase;
	case APPLICATION_CHAR:
		eefc_base = param->param[AT91SAM3_PARAM_PLANE0_CONTROL];
do_erase:
		if (0 == (op->write_operations & area_mask))
		{
			command.eefc_base = eefc_base;
			command.iap_command = AT91SAM3_EEFC_CMD_EA;
			command.result_num = 0;
			command.data_num = 0;
			command.target_addr = 0;
			command.address_of_data = AT91SAM3_IAP_DATA_ADDR;
			if (at91sam3swj_iap_call(&command, NULL))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "erase target");
				return ERRCODE_FAILURE_OPERATION;
			}
		}
		else
		{
			// auto-erase
		}
		break;
	default:
		return VSFERR_FAIL;
		break;
	}
	return VSFERR_NONE;
}

WRITE_TARGET_HANDLER(at91sam3swj)
{
	struct at91sam3swj_iap_command_t command;
	uint32_t eefc_base;
	uint32_t eefc_command;
	struct operation_t *op = context->op;
	struct program_info_t *pi = context->pi;
	struct chip_param_t *param = context->param;
	struct chip_area_info_t *area_info = NULL;
	struct program_area_t *lock_area = NULL;
	int8_t area_idx;
	uint16_t page_size;
	uint32_t target_addr;
	uint16_t page_num;
//	uint8_t pingpong = 0;
	
	switch (area)
	{
	case EEPROM_CHAR:
		eefc_base = param->param[AT91SAM3_PARAM_PLANE1_CONTROL];
		goto do_write;
	case APPLICATION_CHAR:
		eefc_base = param->param[AT91SAM3_PARAM_PLANE0_CONTROL];
do_write:
		area_idx = target_area_idx(area);
		if (area_idx < 0)
		{
			return VSFERR_FAIL;
		}
		area_info = target_get_chip_area(param, (uint32_t)area_idx);
		if (NULL == area_info)
		{
			return VSFERR_FAIL;
		}
		
		page_size = (uint16_t)area_info->page_size;
		target_addr = area_info->addr;
		
		lock_area = target_get_program_area(pi, LOCK_IDX);
		if ((op->write_operations & LOCK) &&
			((NULL == lock_area) || (NULL == lock_area->buff)))
		{
			return VSFERR_FAIL;
		}
		if ((op->write_operations & LOCK) && lock_area->buff[0])
		{
			eefc_command = AT91SAM3_EEFC_CMD_EWPL;
		}
		else
		{
			eefc_command = AT91SAM3_EEFC_CMD_EWP;
		}
#if 1
		if (size != 256)
		{
			LOG_ERROR(ERRMSG_INVALID_VALUE, size, "flash_page");
			return VSFERR_FAIL;
		}
		if (addr & 0xFF)
		{
			LOG_ERROR(ERRMSG_INVALID_HEX, addr, "flash_address");
			return VSFERR_FAIL;
		}
		
		if (adi_memap_write_buf32(addr, buff, size))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "write flash buffer");
			return ERRCODE_FAILURE_OPERATION;
		}
		
		page_num = (uint16_t)((addr - target_addr) / page_size);
		command.eefc_base = eefc_base;
		command.iap_command = eefc_command | AT91SAM3_EEFC_FARG(page_num);
		command.result_num = 0;
		command.data_num = 0;
		command.target_addr = addr;
		command.address_of_data = AT91SAM3_IAP_DATA_ADDR;
		if (at91sam3swj_iap_call(&command, NULL))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run iap");
			return ERRCODE_FAILURE_OPERATION;
		}
#else
		// check
		if ((page_size != 256) || (addr % page_size) || (size % page_size))
		{
			LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
			return VSFERR_FAIL;
		}
		
		// write first buff to target SRAM
		if (adi_memap_write_buf(AT91SAM3_IAP_DATA_ADDR, buff, page_size))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load data to SRAM");
			return ERRCODE_FAILURE_OPERATION;
		}
		
		page_num = (uint16_t)((addr - target_addr) / page_size);
		memset(&command, 0, sizeof(command));
		command.eefc_base = eefc_base;
		command.iap_command = eefc_command | AT91SAM3_EEFC_FARG(page_num);
		command.result_num = 0;
		command.data_num = page_size;
		command.target_addr = addr;
		command.address_of_data = AT91SAM3_IAP_DATA_ADDR;
		if (at91sam3swj_iap_run(&command))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run iap");
			return ERRCODE_FAILURE_OPERATION;
		}
		size -= page_size;
		
		while (size)
		{
			buff += page_size;
			addr += page_size;
			pingpong++;
			
			// write buff to target SRAM
			if (pingpong & 1)
			{
				if (adi_memap_write_buf(AT91SAM3_IAP_DATA_ADDR + page_size,
										buff, page_size))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load data to SRAM");
					return ERRCODE_FAILURE_OPERATION;
				}
			}
			else
			{
				if (adi_memap_write_buf(AT91SAM3_IAP_DATA_ADDR, buff,
										page_size))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load data to SRAM");
					return ERRCODE_FAILURE_OPERATION;
				}
			}
			
			// wait ready
			if (at91sam3swj_iap_wait_ready(NULL))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run iap");
				return ERRCODE_FAILURE_OPERATION;
			}
			
			page_num = (uint16_t)((addr - target_addr) / page_size);
			memset(&command, 0, sizeof(command));
			command.eefc_base = eefc_base;
			command.iap_command = eefc_command | AT91SAM3_EEFC_FARG(page_num);
			command.result_num = 0;
			command.data_num = page_size;
			command.target_addr = addr;
			if (pingpong & 1)
			{
				command.address_of_data = AT91SAM3_IAP_DATA_ADDR + page_size;
			}
			else
			{
				command.address_of_data = AT91SAM3_IAP_DATA_ADDR;
			}
			if (at91sam3swj_iap_run(&command))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run iap");
				return ERRCODE_FAILURE_OPERATION;
			}
			
			size -= page_size;
			pgbar_update(page_size);
		}
		// wait ready
		if (at91sam3swj_iap_wait_ready(NULL))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run iap");
			return ERRCODE_FAILURE_OPERATION;
		}
		pgbar_update(page_size);
#endif
		break;
	case LOCK_CHAR:
		break;
	default:
		return VSFERR_FAIL;
	}
	
	return VSFERR_NONE;
}

READ_TARGET_HANDLER(at91sam3swj)
{
	vsf_err_t err = VSFERR_NONE;
	uint32_t cur_block_size;
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case CHIPID_CHAR:
		if (adi_memap_read_reg32(AT91SAM3_CHIPID_CIDR, (uint32_t*)buff, 1))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "address of iap_entry");
			return ERRCODE_FAILURE_OPERATION;
		}
		*buff = LE_TO_SYS_U32(*buff);
		break;
	case EEPROM_CHAR:
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
	case LOCK_CHAR:
		LOG_ERROR(ERRMSG_NOT_SUPPORT, "lock-reading");
		err = VSFERR_FAIL;
		break;
	default:
		err = VSFERR_NONE;
		break;
	}
	return err;
}
#endif
