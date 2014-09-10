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
#if TARGET_ARM_ADI_EN && TARGET_STM32F1_EN
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
#include "cm_stm32f1.h"

#include "adi_v5p1.h"
#include "cm_common.h"

#include "cm_internal.h"
#include "stm32f1_internal.h"
#include "cm_stm32_fl.h"

struct cm_stm32f1_t
{
	// first member must be same as used in cm module
	// because this class in inherited from cm_info_t
	struct cm_info_t cm;
	
	uint32_t page0_addr;
	uint8_t tick_tock;
	struct stm32_fl_t flash_loader;
};

ENTER_PROGRAM_MODE_HANDLER(stm32f1swj);
LEAVE_PROGRAM_MODE_HANDLER(stm32f1swj);
ERASE_TARGET_HANDLER(stm32f1swj);
WRITE_TARGET_HANDLER(stm32f1swj);
READ_TARGET_HANDLER(stm32f1swj);
const struct program_functions_t stm32f1swj_program_functions =
{
	NULL,
	ENTER_PROGRAM_MODE_FUNCNAME(stm32f1swj),
	LEAVE_PROGRAM_MODE_FUNCNAME(stm32f1swj),
	ERASE_TARGET_FUNCNAME(stm32f1swj),
	WRITE_TARGET_FUNCNAME(stm32f1swj),
	READ_TARGET_FUNCNAME(stm32f1swj)
};

ENTER_PROGRAM_MODE_HANDLER(stm32f1swj)
{
	struct cm_stm32f1_t *cm_stm32f1 = (struct cm_stm32f1_t *)context->priv;
	struct chip_area_info_t *sram_info = NULL;
	uint32_t reg, flash_obr, flash_wrpr;
	
	if (sizeof(*cm_stm32f1) > sizeof(context->priv))
	{
		LOG_BUG("context->priv overflows");
		return VSFERR_FAIL;
	}
	
	sram_info = target_get_chip_area(context->param, RAM_IDX);
	if (NULL == sram_info)
	{
		return VSFERR_FAIL;
	}
	cm_stm32f1->flash_loader.base = sram_info->addr;
	cm_stm32f1->page0_addr = sram_info->addr + STM32_FL_BUFFER_OFFSET;
	
	// unlock flash and option bytes
	reg = STM32F1_FLASH_UNLOCK_KEY1;
	adi_memap_write_reg32(STM32F1_FLASH_KEYR, &reg, 0);
	reg = STM32F1_FLASH_UNLOCK_KEY2;
	adi_memap_write_reg32(STM32F1_FLASH_KEYR, &reg, 0);
	reg = STM32F1_OPT_UNLOCK_KEY1;
	adi_memap_write_reg32(STM32F1_FLASH_OPTKEYR, &reg, 0);
	reg = STM32F1_OPT_UNLOCK_KEY2;
	adi_memap_write_reg32(STM32F1_FLASH_OPTKEYR, &reg, 0);
	
	adi_memap_read_reg32(STM32F1_FLASH_WRPR, &flash_wrpr, 0);
	if (adi_memap_read_reg32(STM32F1_FLASH_OBR, &flash_obr, 1))
	{
		return VSFERR_FAIL;
	}
	LOG_INFO(INFOMSG_REG_08X, "FLASH_OBR", flash_obr);
	LOG_INFO(INFOMSG_REG_08X, "FLASH_WRPR", flash_wrpr);
	
	if ((flash_obr & STM32F1_FLASH_OBR_RDPRT) || (flash_wrpr != 0xFFFFFFFF))
	{
		LOG_WARNING("STM32 locked, to unlock, run "
					"vsprog -cstm32f1_XX -mX -oeu -owu -tu0xFFFFFFFFFFFFFFA5");
	}
	
	return stm32swj_fl_init(&cm_stm32f1->flash_loader);
}

LEAVE_PROGRAM_MODE_HANDLER(stm32f1swj)
{
	struct cm_stm32f1_t *cm_stm32f1 = (struct cm_stm32f1_t *)context->priv;
	struct stm32_fl_result_t result;
	
	REFERENCE_PARAMETER(success);
	
	return stm32swj_fl_wait_ready(&cm_stm32f1->flash_loader, &result, true);
}

ERASE_TARGET_HANDLER(stm32f1swj)
{
	struct cm_stm32f1_t *cm_stm32f1 = (struct cm_stm32f1_t *)context->priv;
	struct program_info_t *pi = context->pi;
	struct operation_t *op = context->op;
	struct chip_area_info_t *flash_info = NULL;
	struct program_area_t *fuse_area = NULL;
	vsf_err_t err = VSFERR_NONE;
	struct stm32_fl_cmd_t cmd;
	struct stm32_fl_result_t result;
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	
	cmd.data_unit_round = 1;
	cmd.sr_busy_mask = STM32F1_FLASH_SR_BSY;
	cmd.sr_err_mask = STM32F1_FLASH_SR_ERRMSK;
	switch (area)
	{
	case FUSE_CHAR:
		cmd.cr_addr = STM32F1_FLASH_CR;
		cmd.sr_addr = STM32F1_FLASH_SR;
		cmd.cr_value1 = STM32F1_FLASH_CR_OPTER | STM32F1_FLASH_CR_OPTWRE;
		cmd.cr_value2 = cmd.cr_value1 | STM32F1_FLASH_CR_STRT;
		cmd.data_type = 0;
		cmd.data_round = 1;
		if (stm32swj_fl_call(&cm_stm32f1->flash_loader, &cmd, &result, true))
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		
		// we MUST write a default non-lock value(0xFFFFFFFFFFFFFFA5) to fuse,
		// or STM32 will be locked after fuse erase
		if (!(op->write_operations & FUSE))
		{
			fuse_area = target_get_program_area(pi, FUSE_IDX);
			if ((NULL == fuse_area) || (NULL == fuse_area->buff))
			{
				return VSFERR_FAIL;
			}
			
			SET_LE_U64(fuse_area->buff, 0xFFFFFFFFFFFFFFA5);
			op->write_operations |= FUSE;
		}
		break;
	case APPLICATION_CHAR:
		flash_info = target_get_chip_area(context->param, APPLICATION_IDX);
		if (NULL == flash_info)
		{
			return VSFERR_FAIL;
		}
		
		cmd.cr_addr = STM32F1_FLASH_CR;
		cmd.sr_addr = STM32F1_FLASH_SR;
		cmd.cr_value1 = STM32F1_FLASH_CR_MER;
		cmd.cr_value2 = cmd.cr_value1 | STM32F1_FLASH_CR_STRT;
		cmd.data_type = 0;
		cmd.data_round = 1;
		if (stm32swj_fl_call(&cm_stm32f1->flash_loader, &cmd, &result, true))
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		
		if (flash_info->size > STM32F1_FLASH_BANK_SIZE)
		{
			cmd.cr_addr = STM32F1_FLASH_CR2;
			cmd.sr_addr = STM32F1_FLASH_SR2;
			if (stm32swj_fl_call(&cm_stm32f1->flash_loader, &cmd, &result, true))
			{
				err = ERRCODE_FAILURE_OPERATION;
				break;
			}
		}
		break;
	default:
		err = VSFERR_FAIL;
		break;
	}
	return err;
}

WRITE_TARGET_HANDLER(stm32f1swj)
{
	struct cm_stm32f1_t *cm_stm32f1 = (struct cm_stm32f1_t *)context->priv;
	struct chip_area_info_t *flash_info = NULL;
	uint8_t i;
	uint8_t fuse_buff[STM32F1_OB_SIZE];
	vsf_err_t err = VSFERR_NONE;
	struct stm32_fl_cmd_t cmd;
	struct stm32_fl_result_t result;
	
	cmd.data_unit_round = 1;
	cmd.sr_busy_mask = STM32F1_FLASH_SR_BSY;
	cmd.sr_err_mask = STM32F1_FLASH_SR_ERRMSK;
	switch (area)
	{
	case FUSE_CHAR:
		if (size != STM32F1_OB_SIZE / 2)
		{
			return VSFERR_FAIL;
		}
		
		for (i = 0; i < STM32F1_OB_SIZE / 2; i++)
		{
			fuse_buff[2 * i] = buff[i];
			fuse_buff[2 * i + 1] = ~buff[i];
		}
		
		cmd.cr_addr = STM32F1_FLASH_CR;
		cmd.sr_addr = STM32F1_FLASH_SR;
		cmd.cr_value1 = STM32F1_FLASH_CR_OPTPG | STM32F1_FLASH_CR_OPTWRE;
		cmd.cr_value2 = 0;
		cmd.target_addr = STM32F1_OB_ADDR;
		cmd.ram_addr = cm_stm32f1->page0_addr;
		cmd.data_type = 2;
		cmd.data_round = STM32F1_OB_SIZE / 2;
		if (adi_memap_write_buf32(cmd.ram_addr, fuse_buff, STM32F1_OB_SIZE) ||
			stm32swj_fl_call(&cm_stm32f1->flash_loader, &cmd, &result, true))
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		break;
	case APPLICATION_CHAR:
		flash_info = target_get_chip_area(context->param, APPLICATION_IDX);
		if ((NULL == flash_info) || (size != flash_info->page_size))
		{
			return VSFERR_FAIL;
		}
		
		if (addr >= (flash_info->addr + STM32F1_FLASH_BANK_SIZE))
		{
			cmd.cr_addr = STM32F1_FLASH_CR2;
			cmd.sr_addr = STM32F1_FLASH_SR2;
		}
		else
		{
			cmd.cr_addr = STM32F1_FLASH_CR;
			cmd.sr_addr = STM32F1_FLASH_SR;
		}
		cmd.cr_value1 = STM32F1_FLASH_CR_PG;
		cmd.cr_value2 = 0;
		cmd.target_addr = addr;
		if (cm_stm32f1->tick_tock++ & 1)
		{
			cmd.ram_addr = cm_stm32f1->page0_addr + size;
		}
		else
		{
			cmd.ram_addr = cm_stm32f1->page0_addr;
		}
		cmd.data_type = 2;
		cmd.data_round = (uint16_t)(size / 2);
		if (adi_memap_write_buf32(cmd.ram_addr, buff, size) ||
			stm32swj_fl_call(&cm_stm32f1->flash_loader, &cmd, &result, false))
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

READ_TARGET_HANDLER(stm32f1swj)
{
	struct program_info_t *pi = context->pi;
	struct program_area_t *flash_area = NULL;
	uint8_t option_bytes[STM32F1_OB_SIZE], i;
	uint32_t mcu_id = 0, flash_sram_size, flash_obr;
	uint32_t cur_block_size;
	uint16_t flash_size;
	vsf_err_t err = VSFERR_NONE;
	
	switch (area)
	{
	case CHIPID_CHAR:
		// read MCU ID at STM32F1_REG_MCU_ID
		if (adi_memap_read_reg32(STM32F1_REG_MCU_ID, &mcu_id, 1))
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		mcu_id = LE_TO_SYS_U32(mcu_id);
		stm32f1_print_device(mcu_id);
		mcu_id &= STM32F1_DEN_MSK;
		*(uint32_t *)buff = mcu_id;
		
		if (adi_memap_read_reg32(STM32F1_FLASH_OBR, &flash_obr, 1))
		{
			return VSFERR_FAIL;
		}
		if (flash_obr & STM32F1_FLASH_OBR_RDPRT)
		{
			// read protected, flash size and sram size is not readable
			return VSFERR_NONE;
		}
		
		// read flash and ram size
		if (adi_memap_read_reg32(STM32F1_REG_FLASH_RAM_SIZE, &flash_sram_size, 1))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read stm32f1 flash_ram size");
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		flash_sram_size = LE_TO_SYS_U32(flash_sram_size);
		flash_size = stm32f1_get_flash_size(mcu_id, flash_sram_size);
		flash_area = target_get_program_area(pi, APPLICATION_IDX);
		if (flash_area != NULL)
		{
			flash_area->size = flash_size * 1024;
		}
		
		LOG_INFO("Flash memory size: %i KB", flash_size);
		if ((flash_sram_size >> 16) != 0xFFFF)
		{
			LOG_INFO("SRAM memory size: %i KB", flash_sram_size >> 16);
		}
		break;
	case FUSE_CHAR:
		if (adi_memap_read_buf32(STM32F1_OB_ADDR, option_bytes, STM32F1_OB_SIZE))
		{
			return VSFERR_FAIL;
		}
		for (i = 0; i < size; i++)
		{
			buff[i] = option_bytes[i * 2];
		}
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
		if (adi_memap_read_reg32(STM32F1_UID_ADDR + 0,
									(((uint32_t *)buff) + 0), 0) ||
			adi_memap_read_reg32(STM32F1_UID_ADDR + 4,
									(((uint32_t *)buff) + 1), 0) ||
			adi_memap_read_reg32(STM32F1_UID_ADDR + 8,
									(((uint32_t *)buff) + 2), 1))
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
#endif
