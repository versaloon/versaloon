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
#if TARGET_ARM_ADI_EN && TARGET_STM32L1_EN
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
#include "cm_stm32l1.h"

#include "adi_v5p1.h"
#include "cm_common.h"

#include "cm_internal.h"
#include "stm32l1_internal.h"
#include "cm_stm32_fl.h"

#define STM32L1_USE_FLASHLOADER					0

#if STM32L1_USE_FLASHLOADER
struct cm_stm32l1_t
{
	// first member must be same as used in cm module
	// because this class in inherited from cm_info_t
	struct cm_info_t cm;
	
	uint32_t page0_addr;
	uint8_t tick_tock;
	struct stm32_fl_t flash_loader;
};
#endif

ENTER_PROGRAM_MODE_HANDLER(stm32l1swj);
LEAVE_PROGRAM_MODE_HANDLER(stm32l1swj);
ERASE_TARGET_HANDLER(stm32l1swj);
WRITE_TARGET_HANDLER(stm32l1swj);
READ_TARGET_HANDLER(stm32l1swj);
const struct program_functions_t stm32l1swj_program_functions =
{
	NULL,
	ENTER_PROGRAM_MODE_FUNCNAME(stm32l1swj),
	LEAVE_PROGRAM_MODE_FUNCNAME(stm32l1swj),
	ERASE_TARGET_FUNCNAME(stm32l1swj),
	WRITE_TARGET_FUNCNAME(stm32l1swj),
	READ_TARGET_FUNCNAME(stm32l1swj)
};

#if !STM32L1_USE_FLASHLOADER
static vsf_err_t stm32l1swj_wait_busy(void)
{
	uint32_t reg;
	
	do {
		if (adi_memap_read_reg32(STM32L1_FLASH_SR, &reg, 1))
		{
			return VSFERR_FAIL;
		}
	} while (reg & STM32L1_FLASH_SR_BSY);
	return (reg & STM32L1_FLASH_SR_ERRMSK) ? VSFERR_FAIL : VSFERR_NONE;
}
#endif

ENTER_PROGRAM_MODE_HANDLER(stm32l1swj)
{
	uint32_t reg, flash_obr, flash_wrpr, flash_pecr;

#if STM32L1_USE_FLASHLOADER
	struct cm_stm32l1_t *cm_stm32l1 = (struct cm_stm32l1_t *)context->priv;
	struct chip_area_info_t *sram_info = NULL;
	
	if (sizeof(*cm_stm32l1) > sizeof(context->priv))
	{
		LOG_BUG("context->priv overflows");
		return VSFERR_FAIL;
	}
	
	sram_info = target_get_chip_area(context->param, SRAM_IDX);
	if (NULL == sram_info)
	{
		return VSFERR_FAIL;
	}
	cm_stm32l1->flash_loader.base = sram_info->addr;
	cm_stm32l1->page0_addr = sram_info->addr + STM32_FL_BUFFER_OFFSET;
#else
	REFERENCE_PARAMETER(context);
#endif
	
	// unlock flash and option bytes
	if (adi_memap_read_reg32(STM32L1_FLASH_PECR, &flash_pecr, 1))
	{
		return VSFERR_FAIL;
	}
	if (flash_pecr & STM32L1_FLASH_PECR_PELOCK)
	{
		reg = STM32L1_EE_PECR_UNLOCK_KEY1;
		adi_memap_write_reg32(STM32L1_FLASH_PEKEYR, &reg, 0);
		reg = STM32L1_EE_PECR_UNLOCK_KEY2;
		adi_memap_write_reg32(STM32L1_FLASH_PEKEYR, &reg, 0);
	}
	if (flash_pecr & STM32L1_FLASH_PECR_PRGLOCK)
	{
		reg = STM32L1_FLASH_UNLOCK_KEY1;
		adi_memap_write_reg32(STM32L1_FLASH_PRGKEYR, &reg, 0);
		reg = STM32L1_FLASH_UNLOCK_KEY2;
		adi_memap_write_reg32(STM32L1_FLASH_PRGKEYR, &reg, 0);
	}
	if (flash_pecr & STM32L1_FLASH_PECR_OPTLOCK)
	{
		reg = STM32L1_OPT_UNLOCK_KEY1;
		adi_memap_write_reg32(STM32L1_FLASH_OPTKEYR, &reg, 0);
		reg = STM32L1_OPT_UNLOCK_KEY2;
		adi_memap_write_reg32(STM32L1_FLASH_OPTKEYR, &reg, 0);
	}
	
	adi_memap_read_reg32(STM32L1_FLASH_WRPR, &flash_wrpr, 0);
	if (adi_memap_read_reg32(STM32L1_FLASH_OBR, &flash_obr, 1))
	{
		return VSFERR_FAIL;
	}
	LOG_INFO(INFOMSG_REG_08X, "FLASH_OBR", flash_obr);
	LOG_INFO(INFOMSG_REG_08X, "FLASH_WRPR", flash_wrpr);
	
	if (((flash_obr & STM32L1_FLASH_OBR_RDPRT) != STM32L1_FLASH_OBR_RDPRT_LV0) ||
		(flash_wrpr != 0))
	{
		LOG_WARNING("STM32L1 locked");
	}
	
#if STM32L1_USE_FLASHLOADER
	return stm32swj_fl_init(&cm_stm32l1->flash_loader);
#else
	return VSFERR_NONE;
#endif
}

LEAVE_PROGRAM_MODE_HANDLER(stm32l1swj)
{
#if STM32L1_USE_FLASHLOADER
	struct cm_stm32l1_t *cm_stm32l1 = (struct cm_stm32l1_t *)context->priv;
	struct stm32_fl_result_t result;
#else
	REFERENCE_PARAMETER(context);
#endif
	
	REFERENCE_PARAMETER(success);
	
#if STM32L1_USE_FLASHLOADER
	return stm32swj_fl_wait_ready(&cm_stm32l1->flash_loader, &result, true);
#else
	return stm32l1swj_wait_busy();
#endif
}

ERASE_TARGET_HANDLER(stm32l1swj)
{
	struct chip_area_info_t *flash_info = NULL;
#if STM32L1_USE_FLASHLOADER
	struct cm_stm32l1_t *cm_stm32l1 = (struct cm_stm32l1_t *)context->priv;
	struct stm32_fl_cmd_t cmd;
	struct stm32_fl_result_t result;
#endif
	vsf_err_t err = VSFERR_NONE;
	uint32_t reg;
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	
#if STM32L1_USE_FLASHLOADER
	cmd.data_unit_round = 1;
	cmd.sr_busy_mask = STM32L1_FLASH_SR_BSY;
	cmd.sr_err_mask = STM32L1_FLASH_SR_ERRMSK;
#endif
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
		
#if STM32L1_USE_FLASHLOADER
		cmd.cr_addr = STM32L1_FLASH_PECR;
		cmd.sr_addr = STM32L1_FLASH_SR;
		cmd.cr_value1 = STM32L1_FLASH_PECR_ERASE;
		cmd.cr_value2 = cmd.cr_value1 | STM32L1_FLASH_PECR_PROG;
		cmd.target_addr = addr;
		if (cm_stm32l1->tick_tock++ & 1)
		{
			cmd.ram_addr = cm_stm32l1->page0_addr + size;
		}
		else
		{
			cmd.ram_addr = cm_stm32l1->page0_addr;
		}
		cmd.data_type = 4;
		cmd.data_round = 1;
		reg = 0;
		if (adi_memap_write_reg32(cmd.ram_addr, &reg, 0)||
			stm32swj_fl_call(&cm_stm32l1->flash_loader, &cmd, &result, true))
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
#else
		if (stm32l1swj_wait_busy())
		{
			return VSFERR_FAIL;
		}
		reg = STM32L1_FLASH_PECR_ERASE;
		adi_memap_write_reg32(STM32L1_FLASH_PECR, &reg, 0);
		reg = STM32L1_FLASH_PECR_ERASE | STM32L1_FLASH_PECR_PROG;
		adi_memap_write_reg32(STM32L1_FLASH_PECR, &reg, 0);
		reg = 0;
		if (adi_memap_write_reg32(addr, &reg, 1))
		{
			return VSFERR_FAIL;
		}
#endif
		break;
	default:
		err = VSFERR_FAIL;
		break;
	}
	return err;
}

WRITE_TARGET_HANDLER(stm32l1swj)
{
	struct chip_area_info_t *flash_info = NULL;
#if STM32L1_USE_FLASHLOADER
	struct cm_stm32l1_t *cm_stm32l1 = (struct cm_stm32l1_t *)context->priv;
	struct stm32_fl_cmd_t cmd;
	struct stm32_fl_result_t result;
	uint32_t round_size;
#else
	uint32_t reg;
#endif
	vsf_err_t err = VSFERR_NONE;
	
#if STM32L1_USE_FLASHLOADER
	cmd.sr_busy_mask = STM32L1_FLASH_SR_BSY;
	cmd.sr_err_mask = STM32L1_FLASH_SR_ERRMSK;
#endif
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
		
#if STM32L1_USE_FLASHLOADER
		cmd.data_unit_round = 32;
		cmd.cr_addr = STM32L1_FLASH_PECR;
		cmd.sr_addr = STM32L1_FLASH_SR;
		cmd.cr_value1 = STM32L1_FLASH_PECR_FPRG;
		cmd.cr_value2 = cmd.cr_value1 | STM32L1_FLASH_PECR_PROG;
		while (size > 0)
		{
			cmd.target_addr = addr;
			if (cm_stm32l1->tick_tock++ & 1)
			{
				cmd.ram_addr = cm_stm32l1->page0_addr + size;
			}
			else
			{
				cmd.ram_addr = cm_stm32l1->page0_addr;
			}
			cmd.data_type = 4;
			cmd.data_round = 1;
			round_size = cmd.data_unit_round * cmd.data_type;
			if (adi_memap_write_buf(cmd.ram_addr, buff, round_size)||
				stm32swj_fl_call(&cm_stm32l1->flash_loader, &cmd, &result, true))
			{
				err = ERRCODE_FAILURE_OPERATION;
				break;
			}
			addr += round_size;
			buff += round_size;
			size -= round_size;
		}
#else
		if (stm32l1swj_wait_busy())
		{
			return VSFERR_FAIL;
		}
		reg = STM32L1_FLASH_PECR_FPRG;
		adi_memap_write_reg32(STM32L1_FLASH_PECR, &reg, 0);
		reg = STM32L1_FLASH_PECR_FPRG | STM32L1_FLASH_PECR_PROG;
		adi_memap_write_reg32(STM32L1_FLASH_PECR, &reg, 0);
		reg = 0;
		if (adi_memap_write_buf32(addr, buff, size / 2))
		{
			return VSFERR_FAIL;
		}
		addr += size / 2;
		buff += size / 2;
		
		if (stm32l1swj_wait_busy())
		{
			return VSFERR_FAIL;
		}
		reg = STM32L1_FLASH_PECR_FPRG;
		adi_memap_write_reg32(STM32L1_FLASH_PECR, &reg, 0);
		reg = STM32L1_FLASH_PECR_FPRG | STM32L1_FLASH_PECR_PROG;
		adi_memap_write_reg32(STM32L1_FLASH_PECR, &reg, 0);
		reg = 0;
		if (adi_memap_write_buf32(addr, buff, size / 2))
		{
			return VSFERR_FAIL;
		}
#endif
		break;
	default:
		err = VSFERR_FAIL;
		break;
	}
	
	return err;
}

READ_TARGET_HANDLER(stm32l1swj)
{
	struct program_info_t *pi = context->pi;
	struct program_area_t *flash_area = NULL;
	uint32_t mcu_id = 0, flash_sram_size, flash_obr;
	uint32_t cur_block_size;
	uint16_t flash_size, sram_size;
	vsf_err_t err = VSFERR_NONE;
	
	switch (area)
	{
	case CHIPID_CHAR:
		// read MCU ID at STM32L1_REG_MCU_ID
		if (adi_memap_read_reg32(STM32L1_REG_MCU_ID, &mcu_id, 1))
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		mcu_id = LE_TO_SYS_U32(mcu_id);
		stm32l1_print_device(mcu_id);
		mcu_id &= STM32L1_DEN_MSK;
		*(uint32_t *)buff = mcu_id;
		
		if (adi_memap_read_reg32(STM32L1_FLASH_OBR, &flash_obr, 1))
		{
			return VSFERR_FAIL;
		}
		if ((flash_obr & STM32L1_FLASH_OBR_RDPRT) !=
				STM32L1_FLASH_OBR_RDPRT_LV0)
		{
			// read protected, flash size and sram size is not readable
			return VSFERR_NONE;
		}
		
		// read flash and ram size
		if (adi_memap_read_reg32(STM32L1_REG_FLASH_RAM_SIZE, &flash_sram_size, 1))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read stm32l1 flash_ram size");
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		flash_sram_size = LE_TO_SYS_U32(flash_sram_size);
		flash_size = stm32l1_get_flash_size(mcu_id, flash_sram_size);
		sram_size = flash_sram_size >> 16;
		flash_area = target_get_program_area(pi, APPLICATION_IDX);
		if (flash_area != NULL)
		{
			flash_area->size = flash_size * 1024;
		}
		
		LOG_INFO("Flash memory size: %i KB", flash_size);
		if ((sram_size != 0xFFFF) && (sram_size != 0))
		{
			LOG_INFO("SRAM memory size: %i KB", flash_sram_size >> 16);
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
			pgbar_update(cur_block_size);
		}
		break;
	case UNIQUEID_CHAR:
		if (adi_memap_read_reg32(STM32L1_UID_ADDR + 0,
									(((uint32_t *)buff) + 0), 0) ||
			adi_memap_read_reg32(STM32L1_UID_ADDR + 4,
									(((uint32_t *)buff) + 1), 0) ||
			adi_memap_read_reg32(STM32L1_UID_ADDR + 8,
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
