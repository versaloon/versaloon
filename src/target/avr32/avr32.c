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
#include <ctype.h>

#include "app_cfg.h"
#if TARGET_AVR32_EN

#include "compiler.h"
#include "port.h"
#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"
#include "scripts.h"

#include "pgbar.h"

#include "avr32.h"
#include "avr32_internal.h"

#define CUR_TARGET_STRING			AVR32_STRING

struct program_area_map_t avr32_program_area_map[] =
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR | AREA_ATTR_RNP},
	{0, 0, 0, 0, 0, AREA_ATTR_NONE}
};

const struct program_mode_t avr32_program_mode[] =
{
	{'j', SET_FREQUENCY, IFS_JTAG_HL},
	{0, NULL, 0}
};

ENTER_PROGRAM_MODE_HANDLER(avr32jtag);
LEAVE_PROGRAM_MODE_HANDLER(avr32jtag);
ERASE_TARGET_HANDLER(avr32jtag);
WRITE_TARGET_HANDLER(avr32jtag);
READ_TARGET_HANDLER(avr32jtag);
struct program_functions_t avr32_program_functions =
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(avr32jtag),
	LEAVE_PROGRAM_MODE_FUNCNAME(avr32jtag),
	ERASE_TARGET_FUNCNAME(avr32jtag),
	WRITE_TARGET_FUNCNAME(avr32jtag),
	READ_TARGET_FUNCNAME(avr32jtag)
};

VSS_HANDLER(avr32_help)
{
	VSS_CHECK_ARGC(1);
	PRINTF("Usage of %s:"LOG_LINE_END, CUR_TARGET_STRING);
	PRINTF("  -m,  --mode <MODE>                        set mode<j>"LOG_LINE_END);
	PRINTF("  -F,  --frequency <FREQUENCY>              set JTAG frequency, in KHz"LOG_LINE_END);
	PRINTF(LOG_LINE_END);
	return VSFERR_NONE;
}

VSS_HANDLER(avr32_mode)
{
	uint8_t mode;
	
	VSS_CHECK_ARGC(2);
	mode = (uint8_t)strtoul(argv[1], NULL,0);
	switch (mode)
	{
	case AVR32_JTAG:
		break;
	}
	return VSFERR_NONE;
}

const struct vss_cmd_t avr32_notifier[] =
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				avr32_help,
				NULL),
	VSS_CMD(	"mode",
				"set programming mode of target for internal call",
				avr32_mode,
				NULL),
	VSS_CMD_END
};




#define jtag_init()					prog->jtag_hl.init(0)
#define jtag_fini()					prog->jtag_hl.fini(0)
#define jtag_config(kHz,pos)		prog->jtag_hl.config(0, (kHz), (pos))
#define jtag_runtest(len)			prog->jtag_hl.runtest(0, len)
#define jtag_ir_write(i, len)		\
	prog->jtag_hl.ir(0, (uint8_t*)(i), (len), AVR32_JTAG_RTI_CYCLE, 0)
#define jtag_dr_write(d, len)		\
	prog->jtag_hl.dr(0, (uint8_t*)(d), (len), AVR32_JTAG_RTI_CYCLE, 0)
#define jtag_dr_read(d, len)		\
	prog->jtag_hl.dr(0, (uint8_t*)(d), (len), AVR32_JTAG_RTI_CYCLE, 1)
#define jtag_register_callback(s,r)	\
	prog->jtag_hl.register_callback(0, (s), (r))

// retry 1000 times with 0 interval
#define poll_start()				prog->poll.start(1000, 0)
#define poll_end()					prog->poll.end()
#define poll_ok(o, m, v)			\
	prog->poll.checkok(POLL_CHECK_EQU, (o), 1, (m), (v))
#define poll_fail(o, m, v)		\
	prog->poll.checkfail(POLL_CHECK_EQU, (o), 1, (m), (v))

#define delay_ms(ms)				prog->delay.delayms((ms) | 0x8000)
#define delay_us(us)				prog->delay.delayus((us) & 0x7FFF)
#define jtag_commit()				prog->peripheral_commit()

#define avr32jtag_Instr(ir)			jtag_ir_write((ir), AVR32_JTAG_INS_Len)
#define avr32jtag_DataW				jtag_dr_write
#define avr32jtag_DataR				jtag_dr_read

static struct INTERFACES_INFO_T *prog = NULL;

static uint8_t pending_4bytes = 0;
vsf_err_t avr32jtag_receive_callback(uint8_t index, enum jtag_irdr_t cmd,
					uint32_t ir, uint8_t *dest_buffer, uint8_t *src_buffer,
					uint16_t bytelen, uint16_t *processed)
{
	REFERENCE_PARAMETER(index);
	
	if (NULL == src_buffer)
	{
		return VSFERR_FAIL;
	}
	
	switch(cmd)
	{
	case JTAG_SCANTYPE_IR:
		return VSFERR_NONE;
		break;
	case JTAG_SCANTYPE_DR:
		if ((5 == bytelen)
			&& ((AVR32_JTAG_INS_MEMORY_WORD_ACCESS == ir)
				|| (AVR32_JTAG_INS_MEMORY_BLOCK_ACCESS == ir)))
		{
			*processed = 1;
			if (dest_buffer != NULL)
			{
				memcpy(dest_buffer, src_buffer, 4);
			}
		}
		return VSFERR_NONE;
		break;
	}
	
	return VSFERR_FAIL;
}

vsf_err_t avr32jtag_send_callback(uint8_t index, enum jtag_irdr_t cmd,
					uint32_t ir, uint8_t *dest_buffer, uint8_t *src_buffer,
					uint16_t bytelen, uint16_t *processed_len)
{
	REFERENCE_PARAMETER(index);
	
	if ((NULL == src_buffer) || (NULL == dest_buffer))
	{
		return VSFERR_FAIL;
	}
	
	switch(cmd)
	{
	case JTAG_SCANTYPE_IR:
		return VSFERR_NONE;
		break;
	case JTAG_SCANTYPE_DR:
		if ((5 == bytelen)
			&& pending_4bytes
			&& ((AVR32_JTAG_INS_MEMORY_WORD_ACCESS == ir)
				|| (AVR32_JTAG_INS_MEMORY_BLOCK_ACCESS == ir)))
		{
			pending_4bytes = 0;
			*processed_len = 5;
			memset(dest_buffer, 0, 5);
		}
		return VSFERR_NONE;
		break;
	}
	
	return VSFERR_FAIL;
}

static vsf_err_t avr32jtag_sab_word_access(uint8_t slave_addr, uint32_t addr,
											uint8_t *data, uint8_t read)
{
	uint8_t ir;
	uint64_t cmd;
	
	// check
	if ((slave_addr != AVR32_SAB_SLAVE_OCD)
		&& (slave_addr != AVR32_SAB_SLAVE_HSB)
		&& (slave_addr != AVR32_SAB_SLAVE_MSU))
	{
		LOG_ERROR(ERRMSG_INVALID_ADDRESS, slave_addr, "sab slave address");
		return VSFERR_FAIL;
	}
	if (addr & 3)
	{
		LOG_ERROR(ERRMSG_INVALID_ADDRESS, addr, "sab word access");
		return VSFERR_FAIL;
	}
	
	// Phase 1: Write AVR32_JTAG_INS_MEMORY_WORD_ACCESS to IR
	// and poll for BUSY, note that PROTECT and ERROR is the failure bit
	poll_start();
	ir = AVR32_JTAG_INS_MEMORY_WORD_ACCESS;
	avr32jtag_Instr(&ir);
	poll_fail(0, AVR32_JTAG_IRRET_PROTECT, AVR32_JTAG_IRRET_PROTECT);
	poll_fail(0, AVR32_JTAG_IRRET_ERROR, AVR32_JTAG_IRRET_ERROR);
	poll_ok(0, AVR32_JTAG_IRRET_BUSY, 0);
	poll_end();
	
	// Phase 2: Write Address
	cmd = (read > 0) | (addr >> 1) | ((uint64_t)slave_addr << 31);
	avr32jtag_DataW(&cmd, 35);
	
	// Phase 3: Read/Write Data
	if (read)
	{
		poll_start();
		pending_4bytes = 1;
		avr32jtag_DataR(data, 34);
		poll_fail(0, AVR32_JTAG_DRRET_ERROR, AVR32_JTAG_DRRET_ERROR);
		poll_ok(0, AVR32_JTAG_DRRET_BUSY, 0);
		poll_end();
	}
	else
	{
		// no error will occur here
		poll_start();
		avr32jtag_DataW(data, 32);
		poll_fail(3, AVR32_JTAG_IRRET_PROTECT, AVR32_JTAG_IRRET_PROTECT);
		poll_fail(3, AVR32_JTAG_IRRET_ERROR, AVR32_JTAG_IRRET_ERROR);
		poll_ok(3, AVR32_JTAG_DRRET_BUSY, 0);
		poll_end();
		
		// Phase 4: Wait Ready
		poll_start();
		ir = AVR32_JTAG_INS_MEMORY_WORD_ACCESS;
		avr32jtag_Instr(&ir);
		poll_fail(0, AVR32_JTAG_IRRET_PROTECT, AVR32_JTAG_IRRET_PROTECT);
		poll_fail(0, AVR32_JTAG_IRRET_ERROR, AVR32_JTAG_IRRET_ERROR);
		poll_ok(0, AVR32_JTAG_IRRET_BUSY, 0);
		poll_end();
	}
	
	return VSFERR_NONE;
}

static vsf_err_t avr32jtag_sab_access(uint8_t slave_addr, uint32_t addr,
									uint8_t *data, uint8_t read, uint32_t len)
{
	uint8_t ir;
	uint32_t i;
	
	if (0 == len)
	{
		return VSFERR_NONE;
	}
	
	// Phase 1: Read/Write the first Word
	avr32jtag_sab_word_access(slave_addr, addr, data, read);
	if (1 == len)
	{
		return VSFERR_NONE;
	}
	
	// Phase 2: Write AVR32_JTAG_INS_MEMORY_BLOCK_ACCESS to IR
	ir = AVR32_JTAG_INS_MEMORY_BLOCK_ACCESS;
	avr32jtag_Instr(&ir);

	// Phase 3: DR Read Loop
	for (i = 1; i < len; i++)
	{
		if (read)
		{
			poll_start();
			pending_4bytes = 1;
			avr32jtag_DataR(data + 4 * i, 34);
			poll_fail(0, AVR32_JTAG_DRRET_ERROR, AVR32_JTAG_DRRET_ERROR);
			poll_ok(0, AVR32_JTAG_DRRET_BUSY, 0);
			poll_end();
		}
		else
		{
			poll_start();
			avr32jtag_DataW(data + 4 * i, 32);
			poll_fail(3, AVR32_JTAG_IRRET_PROTECT, AVR32_JTAG_IRRET_PROTECT);
			poll_fail(3, AVR32_JTAG_IRRET_ERROR, AVR32_JTAG_IRRET_ERROR);
			poll_ok(3, AVR32_JTAG_IRRET_BUSY, 0);
			poll_end();
		}
	}
	
	return VSFERR_NONE;
}

static vsf_err_t avr32jtag_fcmd_call(uint8_t command, uint16_t pagen)
{
	uint32_t data;
	uint32_t start, end;
	
	data = command | (pagen << 8) | AVR32_FLASHC_FCMD_KEY;
	avr32jtag_sab_access(AVR32_SAB_SLAVE_HSB, AVR32_FLASHC_FCR,
								(uint8_t*)&data, AVR32_JTAG_WRITE, 1);
	
	start = (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
	do
	{
		data = 0;
		avr32jtag_sab_access(AVR32_SAB_SLAVE_HSB, AVR32_FLASHC_FSR,
								(uint8_t*)&data, AVR32_JTAG_READ, 1);
		if (jtag_commit())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read fsr");
			return ERRCODE_FAILURE_OPERATION;
		}
		end = (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
	} while (!(data & 1) && ((end - start) < 5000));
	
	if (!(data & 1) || (data & 0x0C))
	{
		LOG_DEBUG(INFOMSG_REG_08X, "FLASHC_FSR", data);
		return VSFERR_FAIL;
	}
	
	return VSFERR_NONE;
}

ENTER_PROGRAM_MODE_HANDLER(avr32jtag)
{
	struct program_info_t *pi = context->pi;
	
	pending_4bytes = 0;
	prog = context->prog;
	
	if (!pi->frequency)
	{
		pi->frequency = 4500;
	}
	
	// init
	jtag_init();
	jtag_config(pi->frequency, &pi->jtag_pos);
	if (jtag_commit())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "init jtag");
		return VSFERR_FAIL;
	}
	jtag_register_callback(avr32jtag_send_callback,
								   avr32jtag_receive_callback);
	
	return jtag_commit();
}

LEAVE_PROGRAM_MODE_HANDLER(avr32jtag)
{
	vsf_err_t err;
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	jtag_fini();
	err = jtag_commit();
	jtag_register_callback(NULL, NULL);
	return err;
}

ERASE_TARGET_HANDLER(avr32jtag)
{
	struct chip_area_info_t *flash_info = NULL;
	uint32_t i;
	uint32_t data;
	uint16_t pagen;
	
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(size);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		flash_info = target_get_chip_area(context->param, APPLICATION_IDX);
		if (NULL == flash_info)
		{
			return VSFERR_FAIL;
		}
		
		// read fsr to check lock
		data = 0;
		avr32jtag_sab_access(AVR32_SAB_SLAVE_HSB, AVR32_FLASHC_FSR,
								(uint8_t*)&data, AVR32_JTAG_READ, 1);
		if (jtag_commit())
		{
			return VSFERR_FAIL;
		}
		LOG_DEBUG(INFOMSG_REG_08X, "FLASHC_FSR", data);
		for (i = 0; i < 16; i++)
		{
			if (data & (1 << (16 + i)))
			{
				// call AVR32_FLASHC_FCMD_UP
				pagen = (uint16_t)(flash_info->page_num * i / 16);
				if (avr32jtag_fcmd_call(AVR32_FLASHC_FCMD_UP, pagen))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "unlock flash page");
					return ERRCODE_FAILURE_OPERATION;
				}
			}
		}
		
		avr32jtag_fcmd_call(AVR32_FLASHC_FCMD_EGPB, 17);
		avr32jtag_fcmd_call(AVR32_FLASHC_FCMD_EGPB, 18);
		avr32jtag_fcmd_call(AVR32_FLASHC_FCMD_EGPB, 19);
		
		// check BOOTPROT
		data = 0;
		avr32jtag_sab_access(AVR32_SAB_SLAVE_HSB, AVR32_FLASHC_FGPFRHI,
								(uint8_t*)&data, AVR32_JTAG_READ, 1);
		if (jtag_commit())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read fsr");
			return ERRCODE_FAILURE_OPERATION;
		}
		LOG_DEBUG(INFOMSG_REG_08X, "fusehi", data);
		data = 0;
		avr32jtag_sab_access(AVR32_SAB_SLAVE_HSB, AVR32_FLASHC_FGPFRLO,
								(uint8_t*)&data, AVR32_JTAG_READ, 1);
		if (jtag_commit())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read fsr");
			return ERRCODE_FAILURE_OPERATION;
		}
		LOG_DEBUG(INFOMSG_REG_08X, "fuselo", data);
		
		return avr32jtag_fcmd_call(AVR32_FLASHC_FCMD_EA, 0);
		break;
	default:
		return VSFERR_FAIL;
		break;
	}
}

WRITE_TARGET_HANDLER(avr32jtag)
{
	struct chip_area_info_t *flash_info = NULL;
	uint16_t pagen;
	
	switch (area)
	{
	case APPLICATION_CHAR:
		flash_info = target_get_chip_area(context->param, APPLICATION_IDX);
		if (NULL == flash_info)
		{
			return VSFERR_FAIL;
		}
		
		pagen = (uint16_t)((addr - flash_info->addr) / flash_info->page_size);
		avr32jtag_fcmd_call(AVR32_FLASHC_FCMD_CPB, 0);
		avr32jtag_sab_access(AVR32_SAB_SLAVE_HSB, addr,
								buff, AVR32_JTAG_WRITE, size / 4);
		jtag_commit();
		if (avr32jtag_fcmd_call(AVR32_FLASHC_FCMD_WP, pagen))
		{
			return VSFERR_FAIL;
		}
		break;
	default:
		return VSFERR_FAIL;
		break;
	}
	
	return VSFERR_NONE;
}

READ_TARGET_HANDLER(avr32jtag)
{
	struct chip_area_info_t *flash_info = NULL;
	uint32_t current_size;
	uint8_t ir;
	uint32_t dr;
	
	switch (area)
	{
	case CHIPID_CHAR:
		// read jtag_id use IDCODE
		// this should always work
		ir = AVR32_JTAG_INS_IDCODE;
		avr32jtag_Instr(&ir);
		dr = 0;
		jtag_dr_read(&dr, 32);
		if (jtag_commit())
		{
			return VSFERR_FAIL;
		}
		LOG_DEBUG(INFOMSG_REG_08X, "JTAGID", dr);
		
		for (ir = 0; ir < 10; ir++)
		{
			dr = 0;
			avr32jtag_sab_access(AVR32_SAB_SLAVE_HSB, AVR32_FLASHC_FSR,
									(uint8_t*)&dr, AVR32_JTAG_READ, 1);
			if (jtag_commit())
			{
				return VSFERR_FAIL;
			}
			LOG_DEBUG(INFOMSG_REG_08X, "FLASHC_FSR", dr);
			
			dr = 0;
			avr32jtag_sab_access(AVR32_SAB_SLAVE_OCD, 0,
									(uint8_t*)&dr, AVR32_JTAG_READ, 1);
			if (jtag_commit())
			{
				return VSFERR_FAIL;
			}
			LOG_DEBUG(INFOMSG_REG_08X, "OCD_DID", dr);
		}
		
		// clear rev area of id
		dr &= ~0xF0000000;
		memcpy(buff, &dr, 4);
		break;
	case APPLICATION_CHAR:
		// check
		flash_info = target_get_chip_area(context->param, APPLICATION_IDX);
		if ((NULL == flash_info) || (size % flash_info->page_size))
		{
			return VSFERR_FAIL;
		}
		
		current_size = 0;
		while (current_size < size)
		{
			avr32jtag_sab_access(AVR32_SAB_SLAVE_HSB, addr + current_size,
									&buff[current_size], AVR32_JTAG_READ,
									flash_info->page_size / 4);
			current_size += flash_info->page_size;
			pgbar_update(flash_info->page_size);
		}
		return jtag_commit();
		break;
	default:
		return VSFERR_FAIL;
		break;
	}
	
	return VSFERR_NONE;
}

#endif
