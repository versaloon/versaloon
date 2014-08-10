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

ENTER_PROGRAM_MODE_HANDLER(c8051fc2);
LEAVE_PROGRAM_MODE_HANDLER(c8051fc2);
ERASE_TARGET_HANDLER(c8051fc2);
WRITE_TARGET_HANDLER(c8051fc2);
READ_TARGET_HANDLER(c8051fc2);
struct program_functions_t c8051fc2_program_functions =
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(c8051fc2),
	LEAVE_PROGRAM_MODE_FUNCNAME(c8051fc2),
	ERASE_TARGET_FUNCNAME(c8051fc2),
	WRITE_TARGET_FUNCNAME(c8051fc2),
	READ_TARGET_FUNCNAME(c8051fc2)
};


#define delay_ms(ms)			prog->delay.delayms((ms) | 0x8000)
#define delay_us(us)			prog->delay.delayus((us) & 0x7FFF)

#define c2_init()				prog->c2.init(0)
#define c2_fini()				prog->c2.fini(0)
#define c2_write_ir(ir)			prog->c2.addr_write(0, ir)
#define c2_read_ir(ir)			prog->c2.addr_read(0, ir);
#define c2_write_dr(dr)			prog->c2.data_write(0, &dr, 1)
#define c2_read_dr(dr)			prog->c2.data_read(0, dr, 1)
#define c2_poll_out_ready()		c8051f_c2_addr_poll(0x01, 0x01, 500)
#define c2_poll_in_busy()		c8051f_c2_addr_poll(0x02, 0x00, 500)

#define poll_start(cnt)			prog->poll.start((cnt), 100)
#define poll_end()				prog->poll.end()
#define poll_ok(o, m, v)		\
	prog->poll.checkok(POLL_CHECK_EQU, (o), 1, (m), (v))

#define commit()				prog->peripheral_commit()

static struct INTERFACES_INFO_T *prog = NULL;

vsf_err_t c8051f_c2_addr_poll(uint8_t mask, uint8_t value, uint16_t poll_cnt)
{
	poll_start(poll_cnt * 10);
	
	c2_read_ir(NULL);
	poll_ok(0, mask, value);
	
	poll_end();
	
	return VSFERR_NONE;
}

ENTER_PROGRAM_MODE_HANDLER(c8051fc2)
{
	struct chip_param_t *param = context->param;
	uint8_t dr;
	
	prog = context->prog;
	c2_init();
	// enable flash programming
	c2_write_ir((uint8_t)param->param[C8051F_PARAM_FPCTL_ADDR]);
	dr = 0x02;
	c2_write_dr(dr);
	dr = 0x01;
	c2_write_dr(dr);
	delay_ms(30);
	return commit();
}

LEAVE_PROGRAM_MODE_HANDLER(c8051fc2)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	c2_fini();
	return commit();
}

ERASE_TARGET_HANDLER(c8051fc2)
{
	struct chip_param_t *param = context->param;
	uint8_t dr;
	vsf_err_t err = VSFERR_NONE;
	
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(size);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		c2_write_ir((uint8_t)param->param[C8051F_PARAM_FPDAT_ADDR]);
		dr = C8051F_C2_CMD_DEVICE_ERASE;
		c2_write_dr(dr);
		c2_poll_in_busy();
		
		c2_poll_out_ready();
		c2_read_dr(&dr);
		if (commit() || (dr != C8051F_C2_REP_COMMAND_OK))
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		
		dr = 0xDE;
		c2_write_dr(dr);
		c2_poll_in_busy();
		dr = 0xAD;
		c2_write_dr(dr);
		c2_poll_in_busy();
		dr = 0xA5;
		c2_write_dr(dr);
		c2_poll_in_busy();
		
		c2_poll_out_ready();
		if (commit())
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

WRITE_TARGET_HANDLER(c8051fc2)
{
	struct chip_param_t *param = context->param;
	uint8_t dr;
	uint32_t i;
	vsf_err_t err = VSFERR_NONE;
	
	switch (area)
	{
	case APPLICATION_CHAR:
		c2_write_ir((uint8_t)param->param[C8051F_PARAM_FPDAT_ADDR]);
		
		dr = C8051F_C2_CMD_BLOCK_WRITE;
		c2_write_dr(dr);
		c2_poll_in_busy();
		
		c2_poll_out_ready();
		c2_read_dr(&dr);
		if (commit() || (dr != C8051F_C2_REP_COMMAND_OK))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION_ADDR, "program flash", addr);
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		
		// write address high byte
		dr = (uint8_t)(addr >> 8);
		c2_write_dr(dr);
		c2_poll_in_busy();
		
		// write address low byte
		dr = (uint8_t)(addr & 0xFF);
		c2_write_dr(dr);
		c2_poll_in_busy();
		dr = 0;
		c2_write_dr(dr);
		c2_poll_in_busy();
		
		c2_poll_out_ready();
		c2_read_dr(&dr);
		if (commit() || (dr != C8051F_C2_REP_COMMAND_OK))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION_ADDR, "program flash", addr);
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		
		for (i = 0; i < size; i++)
		{
			c2_write_dr(buff[i]);
			c2_poll_in_busy();
		}
		
		c2_poll_out_ready();
		if (commit())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION_ADDR, "program flash", addr);
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

READ_TARGET_HANDLER(c8051fc2)
{
	struct chip_param_t *param = context->param;
	uint8_t dr;
	uint32_t i;
	vsf_err_t err = VSFERR_NONE;
	
	switch (area)
	{
	case CHIPID_CHAR:
		c2_write_ir(C8051F_C2_DEVICEID);
		c2_read_dr(&dr);
		if (commit())
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		*(uint8_t *)buff = dr;
		break;
	case APPLICATION_CHAR:
		c2_write_ir((uint8_t)param->param[C8051F_PARAM_FPDAT_ADDR]);
		
		dr = C8051F_C2_CMD_BLOCK_READ;
		c2_write_dr(dr);
		c2_poll_in_busy();
		
		c2_poll_out_ready();
		c2_read_dr(&dr);
		if (commit() || (dr != C8051F_C2_REP_COMMAND_OK))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION_ADDR, "read flash", addr);
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		
		// write address high byte
		dr = (uint8_t)(addr >> 8);
		c2_write_dr(dr);
		c2_poll_in_busy();
		// write address low byte
		dr = (uint8_t)(addr & 0xFF);
		c2_write_dr(dr);
		c2_poll_in_busy();
		dr = 0;
		c2_write_dr(dr);
		c2_poll_in_busy();
		
		c2_poll_out_ready();
		c2_read_dr(&dr);
		if (commit() || (dr != C8051F_C2_REP_COMMAND_OK))
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		
		for (i = 0; i < size; i++)
		{
//			c2_poll_out_ready();
			c2_read_dr(&buff[i]);
		}
		
		if (commit())
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
