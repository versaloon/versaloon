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

#include "app_cfg.h"
#if TARGET_PSOC1_EN

#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"
#include "scripts.h"

#include "pgbar.h"

#include "psoc1.h"
#include "psoc1_internal.h"

#define CUR_TARGET_STRING			PSOC1_STRING

const struct program_area_map_t psoc1_program_area_map[] =
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR | AREA_ATTR_NP},
	{APPLICATION_CHKSUM_CHAR, 1, 0, 0, 0, AREA_ATTR_R},
	{LOCK_CHAR, 1, 0, 0, 0, AREA_ATTR_W | AREA_ATTR_NP},
	{0, 0, 0, 0, 0, AREA_ATTR_NONE}
};

const struct program_mode_t psoc1_program_mode[] =
{
	{'r', "", IFS_ISSP},
	{'p', "", IFS_ISSP},
	{0, NULL, 0}
};

ENTER_PROGRAM_MODE_HANDLER(psoc1);
LEAVE_PROGRAM_MODE_HANDLER(psoc1);
ERASE_TARGET_HANDLER(psoc1);
WRITE_TARGET_HANDLER(psoc1);
READ_TARGET_HANDLER(psoc1);
const struct program_functions_t psoc1_program_functions =
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(psoc1),
	LEAVE_PROGRAM_MODE_FUNCNAME(psoc1),
	ERASE_TARGET_FUNCNAME(psoc1),
	WRITE_TARGET_FUNCNAME(psoc1),
	READ_TARGET_FUNCNAME(psoc1)
};


#define VECTORS_NUM				17
#define VECTORS_TABLE_SIZE		128

VSS_HANDLER(psoc1_help)
{
	VSS_CHECK_ARGC(1);
	PRINTF("Usage of %s:"LOG_LINE_END, CUR_TARGET_STRING);
	PRINTF("  -m,  --mode <MODE>                        set mode<r|p>"LOG_LINE_END);
	PRINTF(LOG_LINE_END);
	return VSFERR_NONE;
}

const struct vss_cmd_t psoc1_notifier[] =
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				psoc1_help,
				NULL),
	VSS_CMD_END
};






static struct INTERFACES_INFO_T *prog = NULL;

#define PSOC1_SSC_CMD_SWBootReset			0x00
#define PSOC1_SSC_CMD_ReadBlock				0x01
#define PSOC1_SSC_CMD_WriteBlock			0x02
#define PSOC1_SSC_CMD_EraseBlock			0x03
#define PSOC1_SSC_CMD_ProtectBlock			0x04
#define PSOC1_SSC_CMD_EraseAll				0x05
#define PSOC1_SSC_CMD_TableRead				0x06
#define PSOC1_SSC_CMD_CheckSum				0x07
#define PSOC1_SSC_CMD_Calibrate0			0x08
#define PSOC1_SSC_CMD_Calibrate1			0x09

#define get_target_voltage(v)				prog->target_voltage.get(0, v)

#define issp_init()							prog->issp.init(0)
#define issp_fini()							prog->issp.fini(0)
#define issp_enter_program_mode(mode)		\
	prog->issp.enter_program_mode(0, mode)
#define issp_leave_program_mode(mode)		\
	prog->issp.leave_program_mode(0, mode)
#define issp_wait_and_poll()				prog->issp.wait_and_poll(0)
#define issp_commit()						prog->peripheral_commit()

#define issp_0s()							\
	prog->issp.vector(0, ISSP_VECTOR_0S, 0x00, 0x00, NULL)
#define issp_read_sram(addr, buf)			\
	prog->issp.vector(0, ISSP_VECTOR_READ_SRAM, (uint8_t)(addr), 0x00, (buf))
#define issp_write_sram(addr, data)			\
	prog->issp.vector(0, ISSP_VECTOR_WRITE_SRAM, (uint8_t)(addr), \
							(uint8_t)(data),NULL)
#define issp_write_reg(addr, data)			\
	prog->issp.vector(0, ISSP_VECTOR_WRITE_REG, (uint8_t)(addr), \
							(uint8_t)(data),NULL)

#define issp_set_cup_a(cmd)					issp_write_reg(0xF0, (cmd))
#define issp_set_cup_sp(sp)					issp_write_reg(0xF6, (sp))
#define issp_set_cpu_f(f)					issp_write_reg(0xF7, (f))

#define issp_ssc_set_key1()					issp_write_sram(0xF8, 0x3A)
#define issp_ssc_set_key2(key2)				issp_write_sram(0xF9, (key2))
#define issp_ssc_set_blockid(id)			issp_write_sram(0xFA, (id))
#define issp_ssc_set_pointer(p)				issp_write_sram(0xFB, (p))
#define issp_ssc_set_clock(c)				issp_write_sram(0xFC, (c))
#define issp_ssc_set_delay(dly)				issp_write_sram(0xFE, (dly))
#define issp_ssc_set_cmd(cmd)				issp_set_cup_a(cmd)
#define issp_ssc_execute()					issp_write_reg(0xFF, 0x12)

#define issp_sel_reg_bank(xio)				issp_set_cpu_f((xio) ? 0x10 : 0x00)
#define issp_set_flash_bank(bank)			issp_write_reg(0xFA, (bank) & 0x03)

#define PSOC1_ISSP_SSC_DEFAULT_SP			0x08
#define PSOC1_ISSP_SSC_DEFAULT_POINTER		0x80
#define PSOC1_ISSP_SSC_DEFAULT_CLOCK_ERASE	0x15
#define PSOC1_ISSP_SSC_DEFAULT_CLOCK_FLASH	0x54
#define PSOC1_ISSP_SSC_DEFAULT_DELAY		0x56
#define PSOC1_ISSP_SSC_RETURN_OK			0x00

vsf_err_t issp_wait_and_poll_with_ret(uint8_t *buf, uint8_t want_ssc_return_value)
{
	uint8_t i;

#ifdef PARAM_CHECK
	if (want_ssc_return_value > 8)
	{
		LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
		return VSFERR_INVALID_PARAMETER;
	}
#endif
	
	if (issp_wait_and_poll())
	{
		return VSFERR_FAIL;
	}
	
	for (i = 0; i < want_ssc_return_value; i++)
	{
		if (issp_read_sram(0xF8 + i, buf + i))
		{
			return VSFERR_FAIL;
		}
	}
	if (issp_commit())
	{
		return VSFERR_FAIL;
	}
	
	return VSFERR_NONE;
}

vsf_err_t issp_init3_half(uint8_t f9_1, uint8_t f9_2)
{
	issp_write_reg(0xF7, 0x00);
	issp_write_reg(0xF4, 0x03);
	issp_write_reg(0xF5, 0x00);
	issp_write_reg(0xF6, 0x08);
	issp_write_reg(0xF8, 0x51);
	issp_write_reg(0xF9, f9_1);
	issp_write_reg(0xFA, 0x30);
	issp_write_reg(0xFF, 0x12);
	issp_0s();
	
	issp_write_reg(0xF7, 0x00);
	issp_write_reg(0xF4, 0x03);
	issp_write_reg(0xF5, 0x00);
	issp_write_reg(0xF6, 0x08);
	issp_write_reg(0xF8, 0x60);
	issp_write_reg(0xF9, f9_2);
	issp_write_reg(0xFA, 0x30);
	issp_write_reg(0xF7, 0x10);
	issp_write_reg(0xFF, 0x12);
	issp_0s();
	
	return VSFERR_NONE;
}

vsf_err_t issp_call_ssc(uint8_t cmd, uint8_t id, uint8_t poll_ready,
						uint8_t *buf, uint8_t want_return)
{
	issp_sel_reg_bank(0x00);
	issp_set_cup_sp(PSOC1_ISSP_SSC_DEFAULT_SP);
	issp_ssc_set_key1();
	issp_ssc_set_key2(PSOC1_ISSP_SSC_DEFAULT_SP + 3);
	issp_write_reg(0xF5, 0x00);
	issp_write_reg(0xF4, 0x03);
	issp_ssc_set_pointer(0x80);
	issp_write_reg(0xF9, 0x30);
	issp_write_reg(0xFA, 0x40);
	issp_ssc_set_blockid(id);
	issp_ssc_set_cmd(cmd);
	issp_write_reg(0xF8, 0x00);
	issp_ssc_execute();
	
	if (poll_ready > 0)
	{
		return issp_wait_and_poll_with_ret(buf, want_return);
	}
	else
	{
		return issp_commit();
	}
}

ENTER_PROGRAM_MODE_HANDLER(psoc1)
{
	struct program_info_t *pi = context->pi;
	uint16_t voltage;
	
	prog = context->prog;
	
	if (get_target_voltage(&voltage))
	{
		return VSFERR_FAIL;
	}
	
	// ISSP Init
	if (issp_init())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "initialize issp");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// enter program mode
	switch (pi->mode)
	{
	case PSOC1_RESET_MODE:
		if (issp_enter_program_mode(ISSP_PM_RESET))
		{
			return ERRCODE_FAILURE_OPERATION;
		}
		break;
	case PSOC1_POWERON_MODE:
		if (voltage > 2000)
		{
			LOG_ERROR("Target should power off in power-on mode");
			return VSFERR_FAIL;
		}
		if (issp_enter_program_mode(ISSP_PM_POWER_ON))
		{
			return ERRCODE_FAILURE_OPERATION;
		}
		break;
	default:
		return VSFERR_FAIL;
		break;
	}
	
	// init1 call_calibrate
	// call calibrate1
	if (issp_call_ssc(PSOC1_SSC_CMD_Calibrate1, 0, 1, NULL, 0))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "call calibrate1");
		return ERRCODE_FAILURE_OPERATION;
	}
	// init2 read table no.1
	if (issp_call_ssc(PSOC1_SSC_CMD_TableRead, 1, 1, NULL, 0))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read table no.1");
		return ERRCODE_FAILURE_OPERATION;
	}
	// init3 do the hell
	if (voltage < 4000)
	{
		// 3.3V
		issp_init3_half(0xF8, 0xEA);
		issp_init3_half(0xF9, 0xE8);
	}
	else
	{
		// 5V
		issp_init3_half(0xFC, 0xEA);
		issp_init3_half(0xFD, 0xE8);
	}
	
	// init sys_clock
	issp_sel_reg_bank(1);
	issp_write_reg(0xE0, 0x02);
	return issp_commit();
}

LEAVE_PROGRAM_MODE_HANDLER(psoc1)
{
	struct program_info_t *pi = context->pi;
	
	REFERENCE_PARAMETER(success);
	
	switch (pi->mode)
	{
	case PSOC1_RESET_MODE:
		if (issp_leave_program_mode(ISSP_PM_RESET))
		{
			return ERRCODE_FAILURE_OPERATION;
		}
		break;
	case PSOC1_POWERON_MODE:
		if (issp_leave_program_mode(ISSP_PM_POWER_ON))
		{
			return ERRCODE_FAILURE_OPERATION;
		}
		break;
	default:
		return VSFERR_FAIL;
		break;
	}
	issp_fini();
	return issp_commit();
}

ERASE_TARGET_HANDLER(psoc1)
{
	uint8_t tmp8;
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(area);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(size);
	
	issp_ssc_set_clock(PSOC1_ISSP_SSC_DEFAULT_CLOCK_ERASE);
	issp_ssc_set_delay(PSOC1_ISSP_SSC_DEFAULT_DELAY);
	
	if (issp_call_ssc(PSOC1_SSC_CMD_EraseAll, 0, 1, &tmp8, 1) ||
		(tmp8 != PSOC1_ISSP_SSC_RETURN_OK))
	{
		return ERRCODE_FAILURE_OPERATION;
	}
	return VSFERR_NONE;
}

WRITE_TARGET_HANDLER(psoc1)
{
	struct chip_param_t *param = context->param;
	struct chip_area_info_t *flash_info = NULL;
	uint8_t bank, bank_num;
	uint16_t block;
	uint32_t page_num, page_size;
	uint32_t i;
	uint32_t size_written;
	uint8_t tmp8;
	vsf_err_t err = VSFERR_NONE;
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	
	flash_info = target_get_chip_area(param, APPLICATION_IDX);
	if (NULL == flash_info)
	{
		return VSFERR_FAIL;
	}
	
	page_size = flash_info->page_size;
	page_num = flash_info->page_num;
	bank_num = (uint8_t)param->param[PSOC1_PARAM_BANK_NUM];
	switch (area)
	{
	case APPLICATION_CHAR:
		for (bank = 0; bank < bank_num; bank++)
		{
			// select bank by write xio in fls_pr1(in reg_bank 1)
			if (param->param[PSOC1_PARAM_BANK_NUM] > 1)
			{
				issp_sel_reg_bank(1);
				issp_set_flash_bank(bank);
				issp_sel_reg_bank(0);
			}
			
			for (block = 0; block < page_num; block++)
			{
				uint32_t block_num = bank * page_num + block;
				uint32_t block_addr = block_num * page_size;
				
				// write data into sram
				for (i = 0; i < page_size; i++)
				{
					issp_write_sram(PSOC1_ISSP_SSC_DEFAULT_POINTER + i,
									buff[block_addr + i]);
				}
				issp_ssc_set_clock(PSOC1_ISSP_SSC_DEFAULT_CLOCK_FLASH);
				issp_ssc_set_delay(PSOC1_ISSP_SSC_DEFAULT_DELAY);
				
				err = issp_call_ssc(PSOC1_SSC_CMD_WriteBlock,
									(uint8_t)(block & 0xFF), 1, &tmp8, 1);
				if (err || (tmp8 != PSOC1_ISSP_SSC_RETURN_OK))
				{
					err = ERRCODE_FAILURE_OPERATION;
					break;
				}
				pgbar_update(page_size);
			}
			if (err)
			{
				break;
			}
		}
		break;
	case LOCK_CHAR:
		size_written = 0;
		for (bank = 0; bank < bank_num; bank++)
		{
			uint32_t lock_bank_addr = bank * (page_num >> 2);
			
			if (bank_num > 1)
			{
				issp_sel_reg_bank(1);
				issp_set_flash_bank(bank);
				issp_sel_reg_bank(0);
			}
			for (i = 0; i < (page_num >> 2); i++)
			{
				issp_write_sram(PSOC1_ISSP_SSC_DEFAULT_POINTER + i,
								buff[lock_bank_addr + i]);
			}
			issp_ssc_set_clock(PSOC1_ISSP_SSC_DEFAULT_CLOCK_FLASH);
			issp_ssc_set_delay(PSOC1_ISSP_SSC_DEFAULT_DELAY);
			
			err = issp_call_ssc(PSOC1_SSC_CMD_ProtectBlock, 0, 1, &tmp8, 1);
			if (err || (tmp8 != PSOC1_ISSP_SSC_RETURN_OK))
			{
				err = ERRCODE_FAILURE_OPERATION;
				break;
			}
			
			pgbar_update(page_num >> 2);
			size_written += page_num >> 2;
		}
		if (size > size_written)
		{
			pgbar_update(size - size_written);
		}
		break;
	}
	return err;
}

READ_TARGET_HANDLER(psoc1)
{
	struct chip_param_t *param = context->param;
	struct chip_area_info_t *flash_info = NULL;
	uint8_t bank, bank_num;
	uint16_t block;
	uint32_t page_num, page_size;
	uint32_t i;
	uint8_t tmp8;
	vsf_err_t err = VSFERR_NONE;
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	
	flash_info = target_get_chip_area(param, APPLICATION_IDX);
	if (NULL == flash_info)
	{
		return VSFERR_FAIL;
	}
	
	page_size = flash_info->page_size;
	page_num = flash_info->page_num;
	bank_num = (uint8_t)param->param[PSOC1_PARAM_BANK_NUM];
	switch (area)
	{
	case CHIPID_CHAR:
		// call table_read no.0 and read 2 bytes from 0xF8 in sram
		memset(buff, 0, 2);
		err = issp_call_ssc(PSOC1_SSC_CMD_TableRead, 0, 1, buff, 2);
		if (err)
		{
			break;
		}
		*(uint16_t *)buff = LE_TO_SYS_U16(*(uint16_t *)buff);
		break;
	case APPLICATION_CHAR:
		for (bank = 0; bank < bank_num; bank++)
		{
			if (bank_num > 1)
			{
				issp_sel_reg_bank(1);
				issp_set_flash_bank(bank);
				issp_sel_reg_bank(0);
			}
			
			for (block = 0; block < page_num; block++)
			{
				uint32_t block_num = bank * page_num + block;
				uint32_t block_addr = block_num * page_size;
				
				err = issp_call_ssc(PSOC1_SSC_CMD_ReadBlock,
									(uint8_t)(block & 0xFF), 1, &tmp8, 1);
				if (err || (tmp8 != PSOC1_ISSP_SSC_RETURN_OK))
				{
					err = ERRCODE_FAILURE_OPERATION;
					break;
				}
				
				for (i = 0; i < page_size; i++)
				{
					issp_read_sram(PSOC1_ISSP_SSC_DEFAULT_POINTER + i,
									buff + block_addr + i);
				}
				
				// commit
				if (issp_commit())
				{
					err = VSFERR_FAIL;
					break;
				}
				
				pgbar_update(page_size);
			}
			if (err)
			{
				break;
			}
		}
		break;
	}
	return err;
}

ADJUST_SETTING_HANDLER(psoc1)
{
	struct chip_area_info_t *flash_checksum_info = NULL;
	
	REFERENCE_PARAMETER(pi);
	REFERENCE_PARAMETER(program_mode);
	
	flash_checksum_info = target_get_chip_area(param, APPLICATION_CHKSUM_IDX);
	if (NULL == flash_checksum_info)
	{
		return VSFERR_FAIL;
	}
	
	// flash checksum of psoc1 is 2 bytes length
	flash_checksum_info->size = 2;
	flash_checksum_info->addr = 0x00200000;
	flash_checksum_info->page_size = 1;
	flash_checksum_info->page_num = flash_checksum_info->size;
	
	return VSFERR_NONE;
}

#endif
