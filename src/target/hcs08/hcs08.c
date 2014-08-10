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
#if TARGET_HCS08_EN

#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"
#include "scripts.h"

#include "hcs08.h"
#include "hcs08_internal.h"

#define CUR_TARGET_STRING			HCS08_STRING

const struct program_area_map_t hcs08_program_area_map[] =
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR},
	{0, 0, 0, 0, 0, AREA_ATTR_NONE}
};

const struct program_mode_t hcs08_program_mode[] =
{
	{'b', "", IFS_BDM},
	{0, NULL, 0}
};

ENTER_PROGRAM_MODE_HANDLER(hcs08);
LEAVE_PROGRAM_MODE_HANDLER(hcs08);
ERASE_TARGET_HANDLER(hcs08);
WRITE_TARGET_HANDLER(hcs08);
READ_TARGET_HANDLER(hcs08);
const struct program_functions_t hcs08_program_functions =
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(hcs08),
	LEAVE_PROGRAM_MODE_FUNCNAME(hcs08),
	ERASE_TARGET_FUNCNAME(hcs08),
	WRITE_TARGET_FUNCNAME(hcs08),
	READ_TARGET_FUNCNAME(hcs08)
};

VSS_HANDLER(hcs08_help)
{
	VSS_CHECK_ARGC(1);
	PRINTF("Usage of %s:"LOG_LINE_END, CUR_TARGET_STRING);
	PRINTF("  -m,  --mode <MODE>                        set mode<r|p>"LOG_LINE_END);
	PRINTF(LOG_LINE_END);
	return VSFERR_NONE;
}

const struct vss_cmd_t hcs08_notifier[] =
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				hcs08_help,
				NULL),
	VSS_CMD_END
};



#define reset_init()			prog->gpio.init(0)
#define reset_fini()			prog->gpio.fini(0)
#define reset_output()			\
	prog->gpio.config(0, SWIM_RST_PIN, SWIM_RST_PIN, 0, 0)
#define reset_input()			\
	prog->gpio.config(0, SWIM_RST_PIN, 0, SWIM_RST_PIN, SWIM_RST_PIN)
#define reset_set()				reset_input()
#define reset_clr()				reset_output()
#define bdm_output()			\
	prog->gpio.config(0, BDM_PIN, BDM_PIN, 0, 0)
#define bdm_input()			\
	prog->gpio.config(0, BDM_PIN, 0, BDM_PIN, BDM_PIN)
#define bdm_set()				bdm_input()
#define bdm_clr()				bdm_output()

#define bdm_init()				prog->bdm.init(0)
#define bdm_fini()				prog->bdm.fini(0)
#define bdm_sync(khz)			prog->bdm.sync(0, khz)
#define bdm_transact(bo, so, bi, si, d, a)		\
	prog->bdm.transact(0, (bo), (so), (bi), (si), (d), (a))

#define delay_ms(ms)			prog->delay.delayms((ms) | 0x8000)
#define delay_us(us)			prog->delay.delayus((us) & 0x7FFF)

#define poll_start()			prog->poll.start(5000, 100)
#define poll_end()				prog->poll.end()
#define poll_ok(o, m, v)		\
	prog->poll.checkok(POLL_CHECK_EQU, (o), 1, (m), (v))
#define poll_fail(o, m, v)		\
	prog->poll.checkfail(POLL_CHECK_EQU, (o), 1, (m), (v))

#define commit()				prog->peripheral_commit()

static struct INTERFACES_INFO_T *prog = NULL;
static uint8_t hcs08_flash_div = 0;

#define HCS08_BDMCMD_ACKENABLE		0xD5	// NI:D5/d
#define HCS08_BDMCMD_ACKDIABLE		0xD6	// NI:D6/d
#define	HCS08_BDMCMD_BACKGROUND		0x90	// NI:90/d
#define HCS08_BDMCMD_READSTATUS		0xE4	// NI:E4/SS
#define HCS08_BDMCMD_WRITECONTROL	0xC4	// NI:C4/CC
#define HCS08_BDMCMD_READBYTE		0xE0	// NI:E0/AAAA/d/RD
#define HCS08_BDMCMD_READBYTEWS		0xE1	// NI:E1/AAAA/d/SS/RD
#define HCS08_BDMCMD_READLAST		0xE8	// NI:E8/SS/RD
#define HCS08_BDMCMD_WRITEBYTE		0xC0	// NI:C0/AAAA/WD/d
#define HCS08_BDMCMD_WRITEBYTEWS	0xC1	// NI:C1/AAAA/WD/d/SS
#define HCS08_BDMCMD_READBKPT		0xE2	// NI:E2/RBKP
#define HCS08_BDMCMD_WRITEBKPT		0xC2	// NI:C2/WBKP

static vsf_err_t hcs08_ack_enable(void)
{
	uint8_t outbuff[1];
	
	outbuff[0] = HCS08_BDMCMD_ACKENABLE;
	return bdm_transact(outbuff, sizeof(outbuff), NULL, 0, 1, 1);
}
/*
static vsf_err_t hcs08_background(void)
{
	uint8_t outbuff[1];
	
	outbuff[0] = HCS08_BDMCMD_BACKGROUND;
	return bdm_transact(outbuff, sizeof(outbuff), NULL, 0, 0, 0);
}
*/
static vsf_err_t hcs08_read_status(uint8_t *status)
{
	uint8_t outbuff[1];
	
	outbuff[0] = HCS08_BDMCMD_READSTATUS;
	return bdm_transact(outbuff, sizeof(outbuff), status, 1, 0, 0);
}
/*
static vsf_err_t hcs08_write_control(uint8_t control)
{
	uint8_t outbuff[2];
	
	outbuff[0] = HCS08_BDMCMD_WRITECONTROL;
	outbuff[1] = control;
	return bdm_transact(outbuff, sizeof(outbuff), NULL, 0, 0, 0);
}
*/
static vsf_err_t hcs08_read_byte(uint16_t addr, uint8_t *value)
{
	uint8_t outbuff[3];
	
	outbuff[0] = HCS08_BDMCMD_READBYTE;
	SET_BE_U16(&outbuff[1], addr);
	return bdm_transact(outbuff, sizeof(outbuff), value, 1, 1, 1);
}

static vsf_err_t hcs08_write_byte(uint16_t addr, uint8_t value)
{
	uint8_t outbuff[4];
	
	outbuff[0] = HCS08_BDMCMD_WRITEBYTE;
	SET_BE_U16(&outbuff[1], addr);
	outbuff[3] = value;
	return bdm_transact(outbuff, sizeof(outbuff), NULL, 0, 1, 1);
}

static vsf_err_t hcs08_flash_cmd(uint16_t addr, uint8_t value, uint8_t cmd)
{
	if (hcs08_write_byte(addr, value) ||
		hcs08_write_byte(HCS08_FCMD_ADDR, cmd) ||
		hcs08_write_byte(HCS08_FSTAT_ADDR,
				HCS08_FSTAT_FCBEF | HCS08_FSTAT_FPVIOL | HCS08_FSTAT_FACCERR))
	{
		return VSFERR_FAIL;
	}
	
	// poll
	poll_start();
	hcs08_read_byte(HCS08_FSTAT_ADDR, NULL);
	poll_fail(0, HCS08_FSTAT_FPVIOL, HCS08_FSTAT_FPVIOL);
	poll_fail(0, HCS08_FSTAT_FACCERR, HCS08_FSTAT_FACCERR);
	poll_ok(0, HCS08_FSTAT_FCCF, HCS08_FSTAT_FCCF);
	poll_end();
	
	return VSFERR_NONE;
}

ENTER_PROGRAM_MODE_HANDLER(hcs08)
{
	uint16_t kernel_khz;
	uint8_t bdm_status;
	
	prog = context->prog;
	
	reset_init();
	reset_set();
#if 1
	bdm_clr();
	delay_ms(1);
	reset_clr();
	delay_ms(20);
	reset_set();
	delay_ms(1);
	bdm_set();
#endif
	
	bdm_init();
	bdm_sync(&kernel_khz);
	delay_ms(1);
	hcs08_ack_enable();
	
	hcs08_read_status(&bdm_status);
	hcs08_write_byte(HCS08_SOPT_ADDR, HCS08_SOPT_BKGDPE | HCS08_SOPT_RSTPE);
	if (commit())
	{
		return VSFERR_FAIL;
	}
	LOG_INFO("target running at %dkhz", kernel_khz);
	LOG_INFO(INFOMSG_REG_02X, "BDCSCR", bdm_status);
	
	hcs08_flash_div = 0;
	if (kernel_khz > 200 * (HCS08_FCDIV_DIVMASK + 1))
	{
		hcs08_flash_div = HCS08_FCDIV_PRDIV8;
		kernel_khz /= 8;
	}
	if ((kernel_khz % 175) >= (175 / 2))
	{
		hcs08_flash_div |= (kernel_khz / 175);
	}
	else
	{
		hcs08_flash_div |= (kernel_khz / 175) - 1;
	}
	hcs08_write_byte(HCS08_FCDIV_ADDR, hcs08_flash_div);
	hcs08_write_byte(HCS08_FSTAT_ADDR,
						HCS08_FSTAT_FPVIOL | HCS08_FSTAT_FACCERR);
	hcs08_write_byte(HCS08_FPROT_ADDR, 0xFF);
	if (commit())
	{
		return VSFERR_FAIL;
	}
	LOG_INFO("flash running at %dkhz",
				kernel_khz / (1 + (hcs08_flash_div & HCS08_FCDIV_DIVMASK)));
	return VSFERR_NONE;
}

LEAVE_PROGRAM_MODE_HANDLER(hcs08)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	bdm_fini();
	reset_fini();
	
	return commit();
}

ERASE_TARGET_HANDLER(hcs08)
{
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		if (hcs08_flash_cmd((uint16_t)addr, 0, HCS08_FCMD_MMASSERASE) ||
			hcs08_flash_cmd((uint16_t)addr, 0, HCS08_FCMD_MBLANK) ||
			// reprogram security byte to unsecure state
			hcs08_write_byte(HCS08_FCDIV_ADDR, hcs08_flash_div) ||
			hcs08_flash_cmd(HCS08_NVFEOPT_ADDR, 0xFE, HCS08_FCMD_MBYTEPROG) ||
			commit())
		{
			return VSFERR_FAIL;
		}
		return VSFERR_NONE;
	default:
		return VSFERR_FAIL;
	}
}

WRITE_TARGET_HANDLER(hcs08)
{
	uint16_t i;
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(buff);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		for (i = 0; i < size; i++)
		{
			if ((addr + i < HCS08_NONVIOL_REG_START)
				|| (addr + i > HCS08_NONVIOL_REG_END))
			{
				if (hcs08_flash_cmd((uint16_t)addr + i, buff[i],
										HCS08_FCMD_MBURSTPROG))
				{
					return VSFERR_FAIL;
				}
			}
		}
		break;
	default:
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

READ_TARGET_HANDLER(hcs08)
{
	vsf_err_t err = VSFERR_NONE;
	uint16_t i;
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case CHIPID_CHAR:
		hcs08_read_byte(HCS08_DIDH_ADDR, &buff[1]);
		hcs08_read_byte(HCS08_DIDL_ADDR, &buff[0]);
		err = commit();
		if (!err)
		{
			LOG_INFO(INFOMSG_REG_02X, "rev", (buff[1] & 0xF0) >> 4);
			buff[1] &= 0x0F;
		}
		break;
	case APPLICATION_CHAR:
		for (i = 0; i < size; i++)
		{
			hcs08_read_byte((uint16_t)(addr + i), &buff[i]);
		}
		err = commit();
		break;
	default:
		err = VSFERR_FAIL;
		break;
	}
	return err;
}

#endif
