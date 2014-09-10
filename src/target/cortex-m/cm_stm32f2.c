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
#if TARGET_ARM_ADI_EN && (TARGET_STM32F2_EN || TARGET_STM32F4_EN)
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
#include "cm_stm32f2.h"

#include "cm_internal.h"
#include "stm32f2_internal.h"

#include "adi_v5p1.h"
#include "cm_common.h"
#include "cm_stm32_fl.h"

#define STM32F2_FL_PAGE_SIZE	1024
struct cm_stm32f2_t
{
	// first member must be same as used in cm module
	// because this class in inherited from cm_info_t
	struct cm_info_t cm;
	
	uint32_t page0_addr;
	uint32_t page1_addr;
	struct stm32_fl_t flash_loader;
};

ENTER_PROGRAM_MODE_HANDLER(stm32f2swj);
LEAVE_PROGRAM_MODE_HANDLER(stm32f2swj);
ERASE_TARGET_HANDLER(stm32f2swj);
WRITE_TARGET_HANDLER(stm32f2swj);
READ_TARGET_HANDLER(stm32f2swj);
const struct program_functions_t stm32f2swj_program_functions =
{
	NULL,
	ENTER_PROGRAM_MODE_FUNCNAME(stm32f2swj),
	LEAVE_PROGRAM_MODE_FUNCNAME(stm32f2swj),
	ERASE_TARGET_FUNCNAME(stm32f2swj),
	WRITE_TARGET_FUNCNAME(stm32f2swj),
	READ_TARGET_FUNCNAME(stm32f2swj)
};

ENTER_PROGRAM_MODE_HANDLER(stm32f2swj)
{
	struct cm_stm32f2_t *cm_stm32f2 = (struct cm_stm32f2_t *)context->priv;
	struct chip_area_info_t *sram_info = NULL;
	uint32_t reg;
	
	if (sizeof(*cm_stm32f2) > sizeof(context->priv))
	{
		LOG_BUG("context->priv overflows");
		return VSFERR_FAIL;
	}
	
	sram_info = target_get_chip_area(context->param, RAM_IDX);
	if (NULL == sram_info)
	{
		return VSFERR_FAIL;
	}
	cm_stm32f2->flash_loader.base = sram_info->addr;
	cm_stm32f2->page0_addr = sram_info->addr + STM32_FL_BUFFER_OFFSET;
	cm_stm32f2->page1_addr = cm_stm32f2->page0_addr + STM32F2_FL_PAGE_SIZE;
	
	// unlock flash and option bytes
	reg = STM32F2_FLASH_UNLOCK_KEY1;
	adi_memap_write_reg32(STM32F2_FLASH_KEYR, &reg, 0);
	reg = STM32F2_FLASH_UNLOCK_KEY2;
	adi_memap_write_reg32(STM32F2_FLASH_KEYR, &reg, 0);
	reg = STM32F2_OPT_UNLOCK_KEY1;
	adi_memap_write_reg32(STM32F2_FLASH_OPTKEYR, &reg, 0);
	reg = STM32F2_OPT_UNLOCK_KEY2;
	adi_memap_write_reg32(STM32F2_FLASH_OPTKEYR, &reg, 0);
	
	if (adi_memap_read_reg32(STM32F2_FLASH_OPTCR, &reg, 1))
	{
		return VSFERR_FAIL;
	}
	if (((reg & STM32F2_FLASH_OPT_RDP_MASK) != STM32F2_FLASH_OPT_RDP_LVL0) ||
		((reg & STM32F2_FLASH_OPT_WRP_MASK) != STM32F2_FLASH_OPT_WRP_MASK))
	{
		LOG_WARNING("STM32F2 locked, to unlock, run "
					"vsprog -cstm32f2_XX -mX -oeu -owu -tu0xFFFFAAFF");
	}
	
	return stm32swj_fl_init(&cm_stm32f2->flash_loader);
}

LEAVE_PROGRAM_MODE_HANDLER(stm32f2swj)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	return VSFERR_NONE;
}

ERASE_TARGET_HANDLER(stm32f2swj)
{
	struct cm_stm32f2_t *cm_stm32f2 = (struct cm_stm32f2_t *)context->priv;
	vsf_err_t err = VSFERR_NONE;
	struct stm32_fl_cmd_t cmd;
	struct stm32_fl_result_t result;
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	
	cmd.data_unit_round = 1;
	cmd.sr_busy_mask = STM32F2_FLASH_SR_BSY;
	cmd.sr_err_mask = STM32F2_FLASH_SR_ERRMSK;
	switch (area)
	{
	case FUSE_CHAR:
		break;
	case APPLICATION_CHAR:
		cmd.cr_addr = STM32F2_FLASH_CR;
		cmd.sr_addr = STM32F2_FLASH_SR;
		cmd.cr_value1 = STM32F2_FLASH_CR_MER;
		cmd.cr_value2 = cmd.cr_value1 | STM32F2_FLASH_CR_STRT;
		cmd.data_type = 0;
		cmd.data_round = 1;
		if (stm32swj_fl_call(&cm_stm32f2->flash_loader, &cmd, &result, true))
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

WRITE_TARGET_HANDLER(stm32f2swj)
{
	struct cm_stm32f2_t *cm_stm32f2 = (struct cm_stm32f2_t *)context->priv;
	uint8_t tick_tock;
	uint32_t cur_size;
	vsf_err_t err = VSFERR_NONE;
	struct stm32_fl_cmd_t cmd;
	struct stm32_fl_result_t result;
	bool last;
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(buff);
	REFERENCE_PARAMETER(addr);
	
	cmd.data_unit_round = 1;
	switch (area)
	{
	case FUSE_CHAR:
		cmd.cr_addr = STM32F2_FLASH_OPTCR;
		cmd.sr_addr = STM32F2_FLASH_SR;
		cmd.cr_value1 = GET_LE_U32(buff);
		cmd.cr_value2 = cmd.cr_value1 | STM32F2_FLASH_OPTCR_START;
		cmd.target_addr = 0;
		cmd.ram_addr = 0;
		cmd.data_type = 0;
		cmd.data_round = 1;
		if (adi_memap_write_buf32(cmd.ram_addr, buff, 4) ||
			stm32swj_fl_call(&cm_stm32f2->flash_loader, &cmd, &result, true))
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		break;
	case APPLICATION_CHAR:
		last = false;
		if (size <= STM32F2_FL_PAGE_SIZE)
		{
			cur_size = size;
			last = true;
		}
		else
		{
			cur_size = STM32F2_FL_PAGE_SIZE;
		}
		cmd.cr_addr = STM32F2_FLASH_CR;
		cmd.sr_addr = STM32F2_FLASH_SR;
		cmd.cr_value1 = STM32F2_FLASH_CR_PG | STM32F2_FLASH_CR_PSIZE_32;
		cmd.cr_value2 = 0;
		cmd.target_addr = addr;
		cmd.ram_addr = cm_stm32f2->page0_addr;
		cmd.data_type = 4;
		cmd.data_round = (uint16_t)(cur_size / 4);
		if (adi_memap_write_buf32(cmd.ram_addr, buff, cur_size) ||
			stm32swj_fl_call(&cm_stm32f2->flash_loader, &cmd, &result, last))
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		size -= cur_size;
		buff += cur_size;
		addr += cur_size;
		pgbar_update(cur_size);
		tick_tock = 1;
		
		while (size)
		{
			if (size <= STM32F2_FL_PAGE_SIZE)
			{
				cur_size = size;
				last = true;
			}
			else
			{
				cur_size = STM32F2_FL_PAGE_SIZE;
			}
			cmd.target_addr = addr;
			if (tick_tock & 1)
			{
				cmd.ram_addr = cm_stm32f2->page1_addr;
			}
			else
			{
				cmd.ram_addr = cm_stm32f2->page0_addr;
			}
			if (adi_memap_write_buf32(cmd.ram_addr, buff, cur_size) ||
				stm32swj_fl_call(&cm_stm32f2->flash_loader, &cmd, &result, last))
			{
				err = ERRCODE_FAILURE_OPERATION;
				break;
			}
			
			size -= cur_size;
			buff += cur_size;
			addr += cur_size;
			pgbar_update(cur_size);
			tick_tock++;
		}
		break;
	default:
		err = VSFERR_FAIL;
		break;
	}
	
	return err;
}

READ_TARGET_HANDLER(stm32f2swj)
{
	struct program_info_t *pi = context->pi;
	struct program_area_t *flash_area = NULL;
	uint32_t reg;
	uint32_t cur_block_size;
	vsf_err_t err = VSFERR_NONE;
	
	switch (area)
	{
	case CHIPID_CHAR:
		// read MCU ID at STM32_REG_MCU_ID
		if (adi_memap_read_reg32(STM32F2_REG_MCU_ID, &reg, 1))
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		reg = LE_TO_SYS_U32(reg);
		if (!strcmp(pi->chip_type, "stm32f2"))
		{
			stm32f2_print_device(reg);
		}
		else if (!strcmp(pi->chip_type, "stm32f4"))
		{
			stm32f4_print_device(reg);
		}
		reg &= STM32F2_DEN_MSK;
		*(uint32_t *)buff = reg;
		
		// read flash ram size
		if (adi_memap_read_reg32(STM32F2_REG_FLASH_RAM_SIZE, &reg, 1))
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		flash_area = target_get_program_area(pi, APPLICATION_IDX);
		if (flash_area != NULL)
		{
			flash_area->size = (reg >> 16) * 1024;
		}
		LOG_INFO("Flash memory size: %i KB", reg >> 16);
		LOG_INFO("SRAM memory size: %i KB", (reg & 0xFFFF) / 512);
		break;
	case FUSE_CHAR:
		if (adi_memap_read_reg32(STM32F2_FLASH_OPTCR, &reg, 1))
		{
			return VSFERR_FAIL;
		}
		reg &= STM32F2_FLASH_OPT_MASK;
		memcpy(buff, &reg, 4);
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
		if (adi_memap_read_reg32(STM32F2_UID_ADDR + 0,
											(((uint32_t *)buff) + 0), 0) ||
			adi_memap_read_reg32(STM32F2_UID_ADDR + 4,
											(((uint32_t *)buff) + 1), 0) ||
			adi_memap_read_reg32(STM32F2_UID_ADDR + 8,
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
