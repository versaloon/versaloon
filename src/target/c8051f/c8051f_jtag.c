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
#if TARGET_C8051F_EN

#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"
#include "scripts.h"

#include "c8051f.h"
#include "c8051f_internal.h"

#define CUR_TARGET_STRING		C8051F_STRING
#define CUR_DEFAULT_FREQ		4500

ENTER_PROGRAM_MODE_HANDLER(c8051fjtag);
LEAVE_PROGRAM_MODE_HANDLER(c8051fjtag);
ERASE_TARGET_HANDLER(c8051fjtag);
WRITE_TARGET_HANDLER(c8051fjtag);
READ_TARGET_HANDLER(c8051fjtag);
struct program_functions_t c8051fjtag_program_functions =
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(c8051fjtag),
	LEAVE_PROGRAM_MODE_FUNCNAME(c8051fjtag),
	ERASE_TARGET_FUNCNAME(c8051fjtag),
	WRITE_TARGET_FUNCNAME(c8051fjtag),
	READ_TARGET_FUNCNAME(c8051fjtag)
};

static struct INTERFACES_INFO_T *prog = NULL;


#define jtag_init()					prog->jtag_hl.init(0)
#define jtag_fini()					prog->jtag_hl.fini(0)
#define jtag_config(kHz,pos)		prog->jtag_hl.config(0, (kHz), (pos))
#define jtag_runtest(len)			prog->jtag_hl.runtest(0, len)
#define jtag_ir_write(i, len)		\
	prog->jtag_hl.ir(0, (uint8_t*)(i), (len), 1, 0)
#define jtag_dr_write(d, len)		\
	prog->jtag_hl.dr(0, (uint8_t*)(d), (len), 1, 0)
#define jtag_dr_read(d, len)		\
	prog->jtag_hl.dr(0, (uint8_t*)(d), (len), 1, 1)

#if 0
#define jtag_poll_busy()			c8051f_jtag_poll_busy()
#define jtag_poll_flbusy(dly, int)	c8051f_jtag_poll_flbusy((dly), (int))
#else
#define jtag_poll_busy()			prog->delay.delayus(20)
#define jtag_poll_flbusy(dly, int)	jtag_delay_us((dly) * ((int) + 1))
#endif

#define jtag_delay_us(us)			prog->delay.delayus((us))
#define jtag_delay_ms(ms)			prog->delay.delayms((ms))

#define poll_start(cnt, int)		prog->poll.start((cnt), (int))
#define poll_end()					prog->poll.end()
#define poll_ok(o, m, v)			\
	prog->poll.checkok(POLL_CHECK_EQU, (o), 1, (m), (v))

#define jtag_commit()				prog->peripheral_commit()

vsf_err_t c8051f_jtag_poll_busy(void)
{
	poll_start(C8051F_JTAG_MAX_POLL_COUNT, 0);
	
	jtag_dr_read(NULL, 1);
	poll_ok(0, 0x01, 0x00);
	
	poll_end();
	
	return VSFERR_NONE;
}

// *value read will be little endian
vsf_err_t c8051f_jtag_ind_read(uint8_t addr, uint32_t *value, uint8_t num_bits)
{
	uint16_t ir, dr;

#ifdef PARAM_CHECK
	if ((addr < C8051F_IR_FLASHCON) || (addr > C8051F_IR_FLASHSCL))
	{
		LOG_BUG(ERRMSG_INVALID_ADDRESS, addr, CUR_TARGET_STRING);
		return ERRCODE_INVALID;
	}
#endif
	
	ir = SYS_TO_LE_U16(addr | C8051F_IR_STATECNTL_SUSPEND);
	jtag_ir_write(&ir, C8051F_IR_LEN);
	
	dr = SYS_TO_LE_U16(C8051F_INDOPTCODE_READ);
	jtag_dr_write(&dr, 2);
	
	dr = 0;
	jtag_poll_busy();
	
	jtag_dr_read(value, num_bits + 1);
	
	return VSFERR_NONE;
}

// *value is SYS endian
vsf_err_t c8051f_jtag_ind_write(uint8_t addr, uint32_t *value, uint8_t num_bits)
{
	uint16_t ir;
	
#ifdef PARAM_CHECK
	if ((addr < C8051F_IR_FLASHCON) || (addr > C8051F_IR_FLASHSCL))
	{
		LOG_BUG(ERRMSG_INVALID_ADDRESS, addr, CUR_TARGET_STRING);
		return ERRCODE_INVALID;
	}
#endif
	
	ir = SYS_TO_LE_U16(addr | C8051F_IR_STATECNTL_SUSPEND);
	jtag_ir_write(&ir, C8051F_IR_LEN);
	
	*value |= C8051F_INDOPTCODE_WRITE << num_bits;
	*value = SYS_TO_LE_U32(*value);
	jtag_dr_write(value, num_bits + 2);
	
	jtag_poll_busy();
	
	return VSFERR_NONE;
}

vsf_err_t c8051f_jtag_poll_flbusy(uint16_t poll_cnt, uint16_t interval)
{
	poll_start(poll_cnt, interval);
	
	c8051f_jtag_ind_read(C8051F_IR_FLASHDAT, NULL, 1);
	poll_ok(0, 0x02, 0x00);
	
	poll_end();
	
	return VSFERR_NONE;
}

ENTER_PROGRAM_MODE_HANDLER(c8051fjtag)
{
	struct program_info_t *pi = context->pi;
	uint32_t dr;
	
	if (!pi->frequency)
	{
		context->pi->frequency = CUR_DEFAULT_FREQ;
	}
	prog = context->prog;
	
	jtag_init();
	jtag_config(pi->frequency, &pi->jtag_pos);
	
	// set FLASHSCL based on SYSCLK (2MHx = 0x86)
	dr = 0x86;
	c8051f_jtag_ind_write(C8051F_IR_FLASHSCL, &dr, C8051F_DR_FLASHSCL_LEN);
	
	return jtag_commit();
}

LEAVE_PROGRAM_MODE_HANDLER(c8051fjtag)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	jtag_fini();
	return jtag_commit();
}

ERASE_TARGET_HANDLER(c8051fjtag)
{
	struct chip_param_t *param = context->param;
	uint32_t dr;
	vsf_err_t err = VSFERR_NONE;
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		// set FLASHADR to erase_addr
		dr = param->param[C8051F_PARAM_ERASE_ADDR];
		c8051f_jtag_ind_write(C8051F_IR_FLASHADR, &dr, C8051F_DR_FLASHADR_LEN);
		// set FLASHCON for flash erase operation(0x20)
		dr = 0x20;
		c8051f_jtag_ind_write(C8051F_IR_FLASHCON, &dr, C8051F_DR_FLASHCON_LEN);
		// set FLASHDAT to 0xA5
		dr = 0xA5;
		c8051f_jtag_ind_write(C8051F_IR_FLASHDAT, &dr, C8051F_DR_FLASHDAT_WLEN);
		// set FLASHCON for poll operation
		dr = 0;
		c8051f_jtag_ind_write(C8051F_IR_FLASHCON, &dr, C8051F_DR_FLASHCON_LEN);
		jtag_delay_ms(1500);
		c8051f_jtag_poll_flbusy(1500, 1000);
		if (jtag_commit())
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		
		// read FLBusy and FLFail
		c8051f_jtag_ind_read(C8051F_IR_FLASHDAT, &dr, 2);
		
		if (jtag_commit() || (LE_TO_SYS_U32(dr) & 0x02))
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

WRITE_TARGET_HANDLER(c8051fjtag)
{
	uint32_t dr, read_buf[512];
	uint32_t i;
	vsf_err_t err = VSFERR_NONE;
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		// set FLASHADR to address to write to
		dr = addr;
		c8051f_jtag_ind_write(C8051F_IR_FLASHADR, &dr,
								C8051F_DR_FLASHADR_LEN);
		
		for (i = 0; i < size; i++)
		{
			// set FLASHCON for flash write operation(0x10)
			dr = 0x10;
			c8051f_jtag_ind_write(C8051F_IR_FLASHCON, &dr,
									C8051F_DR_FLASHCON_LEN);
			// initiate the write operation
			dr = buff[i];
			c8051f_jtag_ind_write(C8051F_IR_FLASHDAT, &dr,
									C8051F_DR_FLASHDAT_WLEN);
			// set FLASHCON for poll operation
			dr = 0;
			c8051f_jtag_ind_write(C8051F_IR_FLASHCON, &dr,
									C8051F_DR_FLASHCON_LEN);
			// poll for FLBusy
			jtag_poll_flbusy(60, 0);
			c8051f_jtag_ind_read(C8051F_IR_FLASHDAT, read_buf + i, 2);
		}
		
		if (jtag_commit())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION_ADDR, "program flash", addr);
			err = ERRCODE_FAILURE_OPERATION_ADDR;
			break;
		}
		for (i = 0; i < size; i++)
		{
			if ((read_buf[i] & 0x06) != 0)
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION_ADDR, "program flash",
							addr + i);
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

READ_TARGET_HANDLER(c8051fjtag)
{
	uint16_t ir;
	uint32_t dr, read_buf[512];
	uint32_t i;
	vsf_err_t err = VSFERR_NONE;
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case CHIPID_CHAR:
		ir = SYS_TO_LE_U16(C8051F_IR_STATECNTL_RESET | C8051F_IR_BYPASS);
		jtag_ir_write(&ir, C8051F_IR_LEN);
		ir = SYS_TO_LE_U16(C8051F_IR_STATECNTL_HALT | C8051F_IR_IDCODE);
		jtag_ir_write(&ir, C8051F_IR_LEN);
		dr = SYS_TO_LE_U32(0);
		jtag_dr_read(&dr, C8051F_DR_IDCODE_LEN);
		if (jtag_commit())
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		*(uint32_t*)buff = LE_TO_SYS_U32(dr) & C8051F_JTAG_ID_MASK;
		break;
	case APPLICATION_CHAR:
		// set FLASHADR to address to read from
		dr = addr;
		c8051f_jtag_ind_write(C8051F_IR_FLASHADR, &dr,
								C8051F_DR_FLASHADR_LEN);
		
		for (i = 0; i < size; i++)
		{
			// set FLASHCON for flash read operation(0x02)
			dr = 0x02;
			c8051f_jtag_ind_write(C8051F_IR_FLASHCON, &dr,
									C8051F_DR_FLASHCON_LEN);
			// initiate the read operation, 0-bit
			dr = 0;
			c8051f_jtag_ind_read(C8051F_IR_FLASHDAT, &dr, 0);
			// set FLASHCON for poll operation
			dr = 0;
			c8051f_jtag_ind_write(C8051F_IR_FLASHCON, &dr,
									C8051F_DR_FLASHCON_LEN);
			// poll for FLBusy
			jtag_poll_flbusy(0, 0);
			c8051f_jtag_ind_read(C8051F_IR_FLASHDAT, read_buf + i,
									C8051F_DR_FLASHDAT_RLEN);
		}
		
		if (jtag_commit())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION_ADDR, "read flash", addr);
			err = ERRCODE_FAILURE_OPERATION_ADDR;
			break;
		}
		for (i = 0; i < size; i++)
		{
			if ((read_buf[i] & 0x06) != 0)
			{
				err = ERRCODE_FAILURE_OPERATION;
				break;
			}
			buff[i] = (read_buf[i] >> 3) & 0xFF;
		}
		break;
	default:
		err = VSFERR_FAIL;
		break;
	}
	return err;
}

#endif
