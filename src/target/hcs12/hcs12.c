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
#if TARGET_HCS12_EN

#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"
#include "scripts.h"

#include "hcs12.h"
#include "hcs12_internal.h"

#define CUR_TARGET_STRING			HCS12_STRING

const struct program_area_map_t hcs12_program_area_map[] =
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR},
	{0, 0, 0, 0, 0, AREA_ATTR_NONE}
};

const struct program_mode_t hcs12_program_mode[] =
{
	{'b', "", IFS_BDM},
	{0, NULL, 0}
};

ENTER_PROGRAM_MODE_HANDLER(hcs12);
LEAVE_PROGRAM_MODE_HANDLER(hcs12);
ERASE_TARGET_HANDLER(hcs12);
WRITE_TARGET_HANDLER(hcs12);
READ_TARGET_HANDLER(hcs12);
const struct program_functions_t hcs12_program_functions =
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(hcs12),
	LEAVE_PROGRAM_MODE_FUNCNAME(hcs12),
	ERASE_TARGET_FUNCNAME(hcs12),
	WRITE_TARGET_FUNCNAME(hcs12),
	READ_TARGET_FUNCNAME(hcs12)
};

VSS_HANDLER(hcs12_help)
{
	VSS_CHECK_ARGC(1);
	PRINTF("Usage of %s:"LOG_LINE_END, CUR_TARGET_STRING);
	PRINTF("  -m,  --mode <MODE>                        set mode<r|p>"LOG_LINE_END);
	PRINTF(LOG_LINE_END);
	return VSFERR_NONE;
}

const struct vss_cmd_t hcs12_notifier[] =
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				hcs12_help,
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
static uint8_t hcs12_flash_div = 0;

#define HCS12_BDMCMD_ACKENABLE		0xD5	// NI:D5/d
#define HCS12_BDMCMD_ACKDIABLE		0xD6	// NI:D6/d
#define	HCS12_BDMCMD_BACKGROUND		0x90	// NI:90/d
#define HCS12_BDMCMD_READBDBYTE		0xE4	// NI:E4/AAAA/d/RD16
#define HCS12_BDMCMD_WRITEBDBYTE	0xC4	// NI:C4/AAAA/WR16/d
#define HCS12_BDMCMD_READBDWORD		0xEC	// NI:EC/AAAA/d/RD16
#define HCS12_BDMCMD_WRITEBDWORD	0xCC	// NI:CC/AAAA/d/WR16
#define HCS12_BDMCMD_READBYTE		0xE0	// NI:E0/AAAA/d/RD16
#define HCS12_BDMCMD_WRITEBYTE		0xC0	// NI:C0/AAAA/WD16/d
#define HCS12_BDMCMD_READWORD		0xE8	// NI:E8/AAAA/d/RD16
#define HCS12_BDMCMD_WRITEWORD		0xC8	// NI:C8/WR16/d

static vsf_err_t hcs12_ack_enable(void)
{
	uint8_t outbuff[1];
	
	outbuff[0] = HCS12_BDMCMD_ACKENABLE;
	return bdm_transact(outbuff, 1, NULL, 0, 1, 1);
}
/*
static vsf_err_t hcs12_background(void)
{
	uint8_t outbuff[1];
	
	outbuff[0] = HCS12_BDMCMD_BACKGROUND;
	return bdm_transact(outbuff, 1, NULL, 0, 0, 0);
}
*/
static vsf_err_t hcs12_read(uint8_t cmd, uint16_t addr, uint16_t *data)
{
	uint8_t outbuff[3];
	
	outbuff[0] = cmd;
	SET_BE_U16(&outbuff[1], addr);
	return bdm_transact(outbuff, sizeof(outbuff), (uint8_t *)data, 2, 1, 1);
}

static vsf_err_t hcs12_write(uint8_t cmd, uint16_t addr, uint16_t data)
{
	uint8_t outbuff[5];
	
	outbuff[0] = cmd;
	SET_BE_U16(&outbuff[1], addr);
	SET_BE_U16(&outbuff[3], data);
	return bdm_transact(outbuff, sizeof(outbuff), NULL, 0, 1, 1);
}
/*
static vsf_err_t hcs12_read_bd_word(uint16_t addr, uint16_t *data)
{
	return hcs12_read(HCS12_BDMCMD_READBDWORD, addr, data);
}

static vsf_err_t hcs12_write_bd_word(uint16_t addr, uint16_t data)
{
	return hcs12_write(HCS12_BDMCMD_WRITEBDWORD, addr, data);
}
*/
static vsf_err_t hcs12_read_word(uint16_t addr, uint16_t *data)
{
	return hcs12_read(HCS12_BDMCMD_READWORD, addr, data);
}

static vsf_err_t hcs12_write_word(uint16_t addr, uint16_t data)
{
	return hcs12_write(HCS12_BDMCMD_WRITEWORD, addr, data);
}

static vsf_err_t hcs12_write_byte(uint16_t addr, uint8_t data)
{
	uint16_t data16;
	
	data16 = data;
	if (!(addr & 1))
	{
		data16 <<= 8;
	}
	return hcs12_write(HCS12_BDMCMD_WRITEBYTE, addr, data16);
}

static vsf_err_t hcs12_flash_cmd(uint8_t param_num, uint16_t *param)
{
	uint8_t i;
	
	for (i = 0; i < param_num; i++)
	{
		if (hcs12_write_byte(HCS12_FTMR_CCOBIX_ADDR, i))
		{
			return VSFERR_FAIL;
		}
		
		// param is in little endian, change to big endian first
		if (hcs12_write_word(HCS12_FTMR_FCCOB_ADDR, param[i]))
		{
			return VSFERR_FAIL;
		}
	}
	// launch the command
	if (hcs12_write_byte(HCS12_FTMR_FSTAT_ADDR, HCS12_FTMR_FSTAT_FPVIOL
							| HCS12_FTMR_FSTAT_FACCERR | HCS12_FTMR_FSTAT_CCIF))
	{
		return VSFERR_FAIL;
	}
	
	// poll
	poll_start();
	hcs12_read_word(HCS12_FTMR_FSTAT_ADDR, NULL);
	poll_fail(1, HCS12_FTMR_FSTAT_FPVIOL, HCS12_FTMR_FSTAT_FPVIOL);
	poll_fail(1, HCS12_FTMR_FSTAT_FACCERR, HCS12_FTMR_FSTAT_FACCERR);
	poll_ok(1, HCS12_FTMR_FSTAT_CCIF, HCS12_FTMR_FSTAT_CCIF);
	poll_end();
	
	return VSFERR_NONE;
}

ENTER_PROGRAM_MODE_HANDLER(hcs12)
{
	uint16_t kernel_khz;
	
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
	hcs12_ack_enable();
	
	if (commit())
	{
		return VSFERR_FAIL;
	}
	LOG_INFO("target running at %dkhz", kernel_khz);
	
	hcs12_flash_div = 0;
	if ((kernel_khz % 1000) >= 500)
	{
		hcs12_flash_div |= (kernel_khz / 1000);
	}
	else
	{
		hcs12_flash_div |= (kernel_khz / 1000) - 1;
	}
	hcs12_write_byte(HCS12_FTMR_FCLKDIV_ADDR, hcs12_flash_div);
	hcs12_write_byte(HCS12_FTMR_FSTAT_ADDR,
					HCS12_FTMR_FSTAT_FPVIOL | HCS12_FTMR_FSTAT_FACCERR);
	hcs12_write_byte(HCS12_FTMR_FPROT_ADDR, 0xFF);
	if (commit())
	{
		return VSFERR_FAIL;
	}
	LOG_INFO("flash running at %dkhz",
				kernel_khz / (1 + (hcs12_flash_div & HCS12_FTMR_FCDIV_DIVMASK)));
	return VSFERR_NONE;
}

LEAVE_PROGRAM_MODE_HANDLER(hcs12)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	bdm_fini();
	reset_fini();
	
	return commit();
}

ERASE_TARGET_HANDLER(hcs12)
{
	uint16_t param[8];
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		param[0] = HCS12_FTMR_FCMD_EraseAllBlocks << 8;
		if (hcs12_flash_cmd(1, param))
		{
			return VSFERR_FAIL;
		}
		
		param[0] = (HCS12_FTMR_FCMD_ProgramPFlash << 8)
			| ((HCS12_BDM_BDMGPR_ADDR & 0xFF0000) >> 16);
		param[1] = HCS12_BDM_BDMGPR_ADDR & 0x00FFFF;
		param[2] = 0xFFFF;
		param[3] = 0xFFFF;
		param[4] = 0xFFFF;
		param[5] = 0xFFFE;
		if (hcs12_flash_cmd(6, param) ||
			commit())
		{
			return VSFERR_FAIL;
		}
		return VSFERR_NONE;
	default:
		return VSFERR_FAIL;
	}
}

WRITE_TARGET_HANDLER(hcs12)
{
	vsf_err_t err = VSFERR_NONE;
	uint16_t i;
	uint16_t param[8];
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		if ((addr & 0x07) || (size & 0x07))
		{
			return VSFERR_FAIL;
		}
		
		for (i = 0; i < size; i += 8)
		{
			if ((addr + i < HCS12_BDM_ROM_START)
				|| (addr + i > HCS12_BDM_ROM_END))
			{
				param[0] = (HCS12_FTMR_FCMD_ProgramPFlash << 8)
					| (((addr + i) & 0xFF0000) >> 16);
				param[1] = (addr + i) & 0x00FFFF;
				param[2] = (buff[i + 0] << 8) + buff[i + 1];
				param[3] = (buff[i + 2] << 8) + buff[i + 3];
				param[4] = (buff[i + 4] << 8) + buff[i + 5];
				param[5] = (buff[i + 6] << 8) + buff[i + 7];
				err = hcs12_flash_cmd(6, param);
				if (err)
				{
					break;
				}
			}
		}
		break;
	default:
		err = VSFERR_FAIL;
	}
	return err;
}

READ_TARGET_HANDLER(hcs12)
{
	vsf_err_t err = VSFERR_NONE;
	uint16_t i;
	uint16_t addr16;
	uint8_t ppage;
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case CHIPID_CHAR:
		hcs12_read_word(0x001A, (uint16_t *)&buff[0]);
		err = commit();
		buff[0] &= 0xF0;
		buff[1] &= 0xF0;
		break;
	case APPLICATION_CHAR:
		ppage = (uint8_t)(addr >> 14);
		hcs12_write_byte(HCS12_PPAGE_ADDR, ppage);
		for (i = 0; i < size; i += 2)
		{
			addr16 = (uint16_t)((addr + i) & 0x3FFF) + 0x8000;
			hcs12_read_word(addr16, (uint16_t *)&buff[i]);
		}
		err = commit();
		break;
	default:
		err = VSFERR_FAIL;
		break;
	}
	return err;
}

ADJUST_MAPPING_HANDLER(hcs12)
{
	if (TARGET_MAPPING_FROM_FILE == dir)
	{
		if (!(*address & 0x00FF0000))
		{
			*address |= 0x00FF0000;
		}
		// address:	bit0  .. bit13
		// PPAGE:	bit14 .. bit21
		// bit22:	1
		*address = (*address & 0x3FFF) | ((*address & 0x00FF0000) >> 2)
					| (1 << 22);
	}
	else if (TARGET_MAPPING_TO_FILE == dir)
	{
		*address = (*address & 0xFFFF) | ((*address & 0x003FC000) << 2);
	}
	else
	{
		LOG_BUG(ERRMSG_INVALID_TARGET, "mapping direction");
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

#endif
