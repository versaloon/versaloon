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
#if TARGET_LPC900_EN

#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"
#include "scripts.h"

#include "lpc900.h"
#include "lpc900_internal.h"

#define CUR_TARGET_STRING			LPC900_STRING

const struct program_area_map_t lpc900_program_area_map[] =
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EW | AREA_ATTR_V | AREA_ATTR_RNP},
	{0, 0, 0, 0, 0, AREA_ATTR_NONE}
};

const struct program_mode_t lpc900_program_mode[] =
{
	{'*', "", IFS_LPC_ICP},
	{0, NULL, 0}
};

ENTER_PROGRAM_MODE_HANDLER(lpc900icp);
LEAVE_PROGRAM_MODE_HANDLER(lpc900icp);
ERASE_TARGET_HANDLER(lpc900icp);
WRITE_TARGET_HANDLER(lpc900icp);
READ_TARGET_HANDLER(lpc900icp);
const struct program_functions_t lpc900_program_functions =
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(lpc900icp),
	LEAVE_PROGRAM_MODE_FUNCNAME(lpc900icp),
	ERASE_TARGET_FUNCNAME(lpc900icp),
	WRITE_TARGET_FUNCNAME(lpc900icp),
	READ_TARGET_FUNCNAME(lpc900icp)
};

VSS_HANDLER(lpc900_help)
{
	VSS_CHECK_ARGC(1);
	PRINTF("Usage of %s:"LOG_LINE_END, CUR_TARGET_STRING);
	PRINTF(LOG_LINE_END);
	return VSFERR_NONE;
}

const struct vss_cmd_t lpc900_notifier[] =
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				lpc900_help,
				NULL),
	VSS_CMD_END
};







static struct INTERFACES_INFO_T *prog = NULL;

#define icp_init()					prog->lpcicp.init(0)
#define icp_fini()					prog->lpcicp.fini(0)
#define icp_enter_program_mode()	prog->lpcicp.enter_program_mode(0)
#define icp_leave_program_mode()	prog->lpcicp.leave_program_mode(0)
#define icp_in(buf, len)			prog->lpcicp.in(0, (buf), (len))
#define icp_out(buf, len)			prog->lpcicp.out(0, (buf), (len))
#define icp_poll(dat, ptr, set, clear, cnt)	\
	prog->lpcicp.poll_ready(0, (dat), (ptr), (set), (clear), (cnt))
#define icp_commit()				prog->peripheral_commit()

#define LPCICP_POLL_ON_SET			0
#define LPCICP_POLL_ON_CLEAR		1
#define LPCICP_POLL_TIME_OUT		2

#define ICP_CMD_READ				0x01
#define ICP_CMD_WRITE				0x00

#define ICP_CMD_NOP					0x00
#define	ICP_CMD_FMDATA_I			0x04
#define ICP_CMD_FMADRL				0x08
#define ICP_CMD_FMADRH				0x0A
#define ICP_CMD_FMDATA				0x0C
#define ICP_CMD_FMCON				0x0E
#define ICP_CMD_FMDATA_PG			0x14

#define ICP_FMCMD_LOAD				0x00
#define ICP_FMCMD_PROG				0x48
#define ICP_FMCMD_ERS_G				0x72
#define ICP_FMCMD_ERS_S				0x71
#define ICP_FMCMD_ERS_P				0x70
#define ICP_FMCMD_CONF				0x6C
#define ICP_FMCMD_CRC_G				0x1A
#define ICP_FMCMD_CRC_S				0x19
#define ICP_FMCMD_CCP				0x67

#define ICP_CFG_UCFG1				0x00
#define ICP_CFG_UCFG2				0x01
#define ICP_CFG_BOOTVECTOR			0x02
#define ICP_CFG_STATUSBYTE			0x03
#define ICP_CFG_SEC0				0x08
#define ICP_CFG_SEC1				0x09
#define ICP_CFG_SEC2				0x0A
#define ICP_CFG_SEC3				0x0B
#define ICP_CFG_SEC4				0x0C
#define ICP_CFG_SEC5				0x0D
#define ICP_CFG_SEC6				0x0E
#define ICP_CFG_SEC7				0x0F
#define ICP_CFG_MFGID				0x10
#define ICP_CFG_ID1					0x11
#define ICP_CFG_ID2					0x12

ENTER_PROGRAM_MODE_HANDLER(lpc900icp)
{
	struct program_info_t *pi = context->pi;
	uint32_t device_id;	
	uint8_t tmpbuf[5], retry = 0;
	
	prog = context->prog;
	// ICP Init
ProgramStart:
	if (icp_init())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "initialize icp");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// enter program mode
	if (icp_enter_program_mode())
	{
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// read chip_id
	// call table_read no.0 and read 2 bytes from 0xF8 in sram
	device_id = 0;
	
	tmpbuf[0] = ICP_CMD_WRITE | ICP_CMD_FMCON;
	tmpbuf[1] = ICP_FMCMD_CONF;
	tmpbuf[2] = ICP_CMD_WRITE | ICP_CMD_FMADRL;
	tmpbuf[3] = ICP_CFG_MFGID;
	tmpbuf[4] = ICP_CMD_READ | ICP_CMD_FMDATA;
	icp_out(tmpbuf, 5);
	icp_in((uint8_t*)&device_id + 2, 1);
	
	tmpbuf[0] = ICP_CMD_WRITE | ICP_CMD_FMCON;
	tmpbuf[1] = ICP_FMCMD_CONF;
	tmpbuf[2] = ICP_CMD_WRITE | ICP_CMD_FMADRL;
	tmpbuf[3] = ICP_CFG_ID1;
	tmpbuf[4] = ICP_CMD_READ | ICP_CMD_FMDATA;
	icp_out(tmpbuf, 5);
	icp_in((uint8_t*)&device_id + 1, 1);
	
	tmpbuf[0] = ICP_CMD_WRITE | ICP_CMD_FMCON;
	tmpbuf[1] = ICP_FMCMD_CONF;
	tmpbuf[2] = ICP_CMD_WRITE | ICP_CMD_FMADRL;
	tmpbuf[3] = ICP_CFG_ID2;
	tmpbuf[4] = ICP_CMD_READ | ICP_CMD_FMDATA;
	icp_out(tmpbuf, 5);
	icp_in((uint8_t*)&device_id + 0, 1);
	
	if (icp_commit())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read chip id");
		return ERRCODE_FAILURE_OPERATION;
	}
	device_id = LE_TO_SYS_U32(device_id);
	if ((device_id & 0x00FF0000) != 0x00150000)
	{
		icp_fini();
		if (icp_commit())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATE_DEVICE, "target chip");
			return ERRCODE_FAILURE_OPERATION;
		}
		if (++retry < 10)
		{
			goto ProgramStart;
		}
		else
		{
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	pi->chip_id = device_id;
	return VSFERR_NONE;
}

LEAVE_PROGRAM_MODE_HANDLER(lpc900icp)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	icp_fini();
	if (icp_commit())
	{
		return ERRCODE_FAILURE_OPERATION;
	}
	return VSFERR_NONE;
}

ERASE_TARGET_HANDLER(lpc900icp)
{
	uint8_t tmpbuf[2];
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(area);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(size);
	
	tmpbuf[0] = ICP_CMD_WRITE | ICP_CMD_FMCON;
	tmpbuf[1] = ICP_FMCMD_ERS_G;
	icp_out(tmpbuf, 2);
	icp_poll(ICP_CMD_READ | ICP_CMD_FMDATA_I, tmpbuf, 0x80, 0x00, 10000);
	if (icp_commit())
	{
		return ERRCODE_FAILURE_OPERATION;
	}
	return VSFERR_NONE;
}

WRITE_TARGET_HANDLER(lpc900icp)
{
	uint8_t tmpbuf[256 + 11];
	vsf_err_t err = VSFERR_NONE;
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		tmpbuf[0] = ICP_CMD_WRITE | ICP_CMD_FMCON;
		tmpbuf[1] = ICP_FMCMD_LOAD;
		tmpbuf[2] = ICP_CMD_WRITE | ICP_CMD_FMADRL;
		tmpbuf[3] = 0;
		tmpbuf[4] = ICP_CMD_WRITE | ICP_CMD_FMDATA_PG;
		memcpy(tmpbuf + 5, buff, size);
		tmpbuf[5 + size + 0] = ICP_CMD_WRITE | ICP_CMD_FMADRL;
		tmpbuf[5 + size + 1] = (addr >> 0) & 0xFF;
		tmpbuf[5 + size + 2] = ICP_CMD_WRITE | ICP_CMD_FMADRH;
		tmpbuf[5 + size + 3] = (addr >> 8) & 0xFF;
		tmpbuf[5 + size + 4] = ICP_CMD_WRITE | ICP_CMD_FMCON;
		tmpbuf[5 + size + 5] = ICP_FMCMD_PROG;
		
		icp_out(tmpbuf, (uint16_t)(11 + size));
		icp_poll(ICP_CMD_READ | ICP_CMD_FMCON, tmpbuf,
					0x0F, 0x80, 10000);
		if (icp_commit()
			|| (tmpbuf[0] != LPCICP_POLL_ON_CLEAR))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATE_DEVICE, "target chip");
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

READ_TARGET_HANDLER(lpc900icp)
{
	struct chip_area_info_t *flash_info = NULL;
	uint8_t tmpbuf[5];
	vsf_err_t err = VSFERR_NONE;
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(addr);
	
	switch (area)
	{
	case CHIPID_CHAR:
		tmpbuf[0] = ICP_CMD_WRITE | ICP_CMD_FMCON;
		tmpbuf[1] = ICP_FMCMD_CONF;
		tmpbuf[2] = ICP_CMD_WRITE | ICP_CMD_FMADRL;
		tmpbuf[3] = ICP_CFG_MFGID;
		tmpbuf[4] = ICP_CMD_READ | ICP_CMD_FMDATA;
		icp_out(tmpbuf, 5);
		icp_in(&buff[2], 1);
		
		tmpbuf[0] = ICP_CMD_WRITE | ICP_CMD_FMCON;
		tmpbuf[1] = ICP_FMCMD_CONF;
		tmpbuf[2] = ICP_CMD_WRITE | ICP_CMD_FMADRL;
		tmpbuf[3] = ICP_CFG_ID1;
		tmpbuf[4] = ICP_CMD_READ | ICP_CMD_FMDATA;
		icp_out(tmpbuf, 5);
		icp_in(&buff[1], 1);
		
		tmpbuf[0] = ICP_CMD_WRITE | ICP_CMD_FMCON;
		tmpbuf[1] = ICP_FMCMD_CONF;
		tmpbuf[2] = ICP_CMD_WRITE | ICP_CMD_FMADRL;
		tmpbuf[3] = ICP_CFG_ID2;
		tmpbuf[4] = ICP_CMD_READ | ICP_CMD_FMDATA;
		icp_out(tmpbuf, 5);
		icp_in(&buff[0], 1);
		
		if (icp_commit())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read chip id");
			return ERRCODE_FAILURE_OPERATION;
		}
		*(uint32_t *)buff = LE_TO_SYS_U32(*(uint32_t *)buff);
		break;
	case APPLICATION_CHAR:
		if (context->op->verify_operations & APPLICATION)
		{
			uint32_t crc_in_file, crc_in_chip;
			uint32_t crc_poly = 0x00400007;
			uint32_t crc_tmp = 0x00000000;
			uint8_t crc_msb = 0;
			uint32_t loop;
			
			// CRC verify
			tmpbuf[0] = ICP_CMD_WRITE | ICP_CMD_FMCON;
			tmpbuf[1] = ICP_FMCMD_CRC_G;
			icp_out(tmpbuf, 2);
			icp_poll(ICP_CMD_READ | ICP_CMD_FMCON, tmpbuf, 0x0F, 0x80, 10000);
			if (icp_commit()
				|| (tmpbuf[0] != LPCICP_POLL_ON_CLEAR))
			{
				err = ERRCODE_FAILURE_OPERATION;
				break;
			}
			
			tmpbuf[0] = ICP_CMD_READ | ICP_CMD_FMDATA_I;
			icp_out(tmpbuf, 1);
			icp_in((uint8_t*)&crc_in_chip, 1);
			tmpbuf[0] = ICP_CMD_READ | ICP_CMD_FMDATA_I;
			icp_out(tmpbuf, 1);
			icp_in((uint8_t*)&crc_in_chip + 1, 1);
			tmpbuf[0] = ICP_CMD_READ | ICP_CMD_FMDATA_I;
			icp_out(tmpbuf, 1);
			icp_in((uint8_t*)&crc_in_chip + 2, 1);
			tmpbuf[0] = ICP_CMD_READ | ICP_CMD_FMDATA_I;
			icp_out(tmpbuf, 1);
			icp_in((uint8_t*)&crc_in_chip + 3, 1);
			if (icp_commit())
			{
				err = ERRCODE_FAILURE_OPERATION;
				break;
			}
			crc_in_chip = LE_TO_SYS_U32(crc_in_chip);
			
			// calculate crc in file
			crc_in_file = 0;
			flash_info = target_get_chip_area(context->param, APPLICATION_IDX);
			if (NULL == flash_info)
			{
				return VSFERR_FAIL;
			}
			size = flash_info->size;
			for (loop = 0; loop < size; loop++)
			{
				uint8_t byte = buff[loop];
				
				crc_tmp = 0;
				if (byte & (1 << 0))
				{
					crc_tmp |= (1 << 0);
				}
				if (byte & (1 << 1))
				{
					crc_tmp |= (1 << 3);
				}
				if (byte & (1 << 2))
				{
					crc_tmp |= (1 << 5);
				}
				if (byte & (1 << 3))
				{
					crc_tmp |= (1 << 8);
				}
				if (byte & (1 << 4))
				{
					crc_tmp |= (1 << 10);
				}
				if (byte & (1 << 5))
				{
					crc_tmp |= (1 << 13);
				}
				if (byte & (1 << 6))
				{
					crc_tmp |= (1 << 16);
				}
				if (byte & (1 << 7))
				{
					crc_tmp |= (1 << 18);
				}
				
				crc_msb = (uint8_t)((crc_in_file & 0x80000000) > 0);
				crc_in_file <<= 1;
				crc_in_file ^= crc_tmp;
				if (crc_msb)
				{
					crc_in_file ^= crc_poly;
				}
			}
			
			if (crc_in_file != crc_in_chip)
			{
				err = VSFERR_FAIL;
			}
		}
		else
		{
			LOG_ERROR(ERRMSG_NOT_SUPPORT, "read lpc900 flash");
			err = VSFERR_NOT_SUPPORT;
		}
		break;
	default:
		err = VSFERR_FAIL;
		break;
	}
	return err;
}

#endif
