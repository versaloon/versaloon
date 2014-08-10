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
#if TARGET_AVR8_EN

#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"
#include "scripts.h"

#include "avr8.h"
#include "avr8_internal.h"

#define CUR_TARGET_STRING							AVR8_STRING

ENTER_PROGRAM_MODE_HANDLER(avr8jtag);
LEAVE_PROGRAM_MODE_HANDLER(avr8jtag);
ERASE_TARGET_HANDLER(avr8jtag);
WRITE_TARGET_HANDLER(avr8jtag);
READ_TARGET_HANDLER(avr8jtag);
struct program_functions_t avr8jtag_program_functions =
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(avr8jtag),
	LEAVE_PROGRAM_MODE_FUNCNAME(avr8jtag),
	ERASE_TARGET_FUNCNAME(avr8jtag),
	WRITE_TARGET_FUNCNAME(avr8jtag),
	READ_TARGET_FUNCNAME(avr8jtag)
};

#define AVR_JTAG_INS_LEN							4
// Public Instructions:
#define AVR_JTAG_INS_EXTEST							0x00
#define AVR_JTAG_INS_IDCODE							0x01
#define AVR_JTAG_INS_SAMPLE_PRELOAD					0x02
#define AVR_JTAG_INS_BYPASS							0x0F
// AVR Specified Public Instructions:
#define AVR_JTAG_INS_AVR_RESET						0x0C
#define AVR_JTAG_INS_PROG_ENABLE					0x04
#define AVR_JTAG_INS_PROG_COMMANDS					0x05
#define AVR_JTAG_INS_PROG_PAGELOAD					0x06
#define AVR_JTAG_INS_PROG_PAGEREAD					0x07

// Data Registers:
#define AVR_JTAG_REG_Bypass_Len						1
#define AVR_JTAG_REG_DeviceID_Len					32

#define AVR_JTAG_REG_Reset_Len						1
#define AVR_JTAG_REG_JTAGID_Len						32
#define AVR_JTAG_REG_ProgrammingEnable_Len			16
#define AVR_JTAG_REG_ProgrammingCommand_Len			15
#define AVR_JTAG_REG_FlashDataByte_Len				16

#define AVR_JTAG_RTI_CYCLE							1

#define AVR_JTAG_Reset(r)							(AVR_JTAG_SendIns(AVR_JTAG_INS_AVR_RESET), AVR_JTAG_SendDat((r), AVR_JTAG_REG_Reset_Len))
#define AVR_JTAG_PROG_Enable(v)						(AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_ENABLE), AVR_JTAG_SendDat((v), AVR_JTAG_REG_ProgrammingEnable_Len))

// JTAG Programming Instructions:
#define AVR_JTAG_PROG_OPERATIONCOMPLETE				0x0200
#define AVR_JTAG_PROG_INS(d)						AVR_JTAG_SendDat((d), AVR_JTAG_REG_ProgrammingCommand_Len)
#define AVR_JTAG_PROG_ReadDATA(d, p)				AVR_JTAG_ReadDat((d), (uint16_t*)(p), AVR_JTAG_REG_ProgrammingCommand_Len)
#define AVR_JTAG_PROG_LoadAddrExtendedHighByte(c)	AVR_JTAG_PROG_INS(0xB00 | ((c) & 0xFF))
#define AVR_JTAG_PROG_LoadAddrHighByte(a)			AVR_JTAG_PROG_INS(0x0700 | ((a) & 0xFF))
#define AVR_JTAG_PROG_LoadAddrLowByte(b)			AVR_JTAG_PROG_INS(0x0300 | ((b) & 0xFF))
#define AVR_JTAG_PROG_LoadAddrByte(b)				AVR_JTAG_PROG_LoadAddrLowByte(b)
#define AVR_JTAG_PROG_LoadDataLowByte(i)			AVR_JTAG_PROG_INS(0x1300 | ((i) & 0xFF))
#define AVR_JTAG_PROG_LoadDataHighByte(i)			AVR_JTAG_PROG_INS(0x1700 | ((i) & 0xFF))
#define AVR_JTAG_PROG_LoadDataByte(i)				AVR_JTAG_PROG_LoadDataLowByte(i)
#define AVR_JTAG_PROG_LatchData()					(AVR_JTAG_PROG_INS(0x3700), AVR_JTAG_PROG_INS(0x7700), AVR_JTAG_PROG_INS(0x3700))
// Chip Erase
#define AVR_JTAG_PROG_ChipErase()					(AVR_JTAG_PROG_INS(0x2380), AVR_JTAG_PROG_INS(0x3180), AVR_JTAG_PROG_INS(0x3380), AVR_JTAG_PROG_INS(0x3380))
#define AVR_JTAG_PROG_ChipEraseComplete_CMD			0x3380

// Write Flash
#define AVR_JTAG_PROG_EnterFlashWrite()				AVR_JTAG_PROG_INS(0x2310)
#define AVR_JTAG_PROG_WriteFlashPage()				(AVR_JTAG_PROG_INS(0x3700), AVR_JTAG_PROG_INS(0x3500), AVR_JTAG_PROG_INS(0x3700), AVR_JTAG_PROG_INS(0x3700))
#define AVR_JTAG_PROG_WriteFlashPageComplete_CMD	0x3700

// Read Flash
#define AVR_JTAG_PROG_EnterFlashRead()				AVR_JTAG_PROG_INS(0x2302)

// Write EEPROM
#define AVR_JTAG_PROG_EnterEEPROMWrite()			AVR_JTAG_PROG_INS(0x2311)
#define AVR_JTAG_PROG_WriteEEPROMPage()				(AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3100), AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3300))
#define AVR_JTAG_PROG_WriteEEPROMPageComplete_CMD	0x3300

// Read EEPROM
#define AVR_JTAG_PROG_EnterEEPROMRead()				AVR_JTAG_PROG_INS(0x2303)
#define AVR_JTAG_PROG_ReadEEPROM(a, d)				(AVR_JTAG_PROG_INS(0x3300 | (a)), AVR_JTAG_PROG_INS(0x3200), AVR_JTAG_PROG_ReadDATA(0x3300, &(d)))

// Write Fuses
#define AVR_JTAG_PROG_EnterFuseWrite()				AVR_JTAG_PROG_INS(0x2340)
#define AVR_JTAG_PROG_WriteFuseExtByte()			(AVR_JTAG_PROG_INS(0x3B00), AVR_JTAG_PROG_INS(0x3900), AVR_JTAG_PROG_INS(0x3B00), AVR_JTAG_PROG_INS(0x3B00))
#define AVR_JTAG_PROG_WriteFuseExtByteComplete_CMD	0x3700
#define AVR_JTAG_PROG_WriteFuseHighByte()			(AVR_JTAG_PROG_INS(0x3700), AVR_JTAG_PROG_INS(0x3500), AVR_JTAG_PROG_INS(0x3700), AVR_JTAG_PROG_INS(0x3700))
#define AVR_JTAG_PROG_WriteFuseHighByteComplete_CMD	0x3700
#define AVR_JTAG_PROG_WriteFuseLowByte()			(AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3100), AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3300))
#define AVR_JTAG_PROG_WriteFuseLowByteComplete_CMD	0x3300

// Write Lockbits
#define AVR_JTAG_PROG_EnterLockbitWrite()			AVR_JTAG_PROG_INS(0x2320)
#define AVR_JTAG_PROG_WriteLockbit()				(AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3100), AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3300))
#define AVR_JTAG_PROG_WriteLockbitComplete_CMD		0x3300

// Read Fuses/Lockbits
#define AVR_JTAG_PROG_EnterFuseLockbitRead()		AVR_JTAG_PROG_INS(0x2304)
#define AVR_JTAG_PROG_ReadExtFuseByte(e)			(AVR_JTAG_PROG_INS(0x3A00), AVR_JTAG_PROG_ReadDATA(0x3B00, &(e)))
#define AVR_JTAG_PROG_ReadFuseHighByte(h)			(AVR_JTAG_PROG_INS(0x3E00), AVR_JTAG_PROG_ReadDATA(0x3F00, &(h)))
#define AVR_JTAG_PROG_ReadFuseLowByte(l)			(AVR_JTAG_PROG_INS(0x3200), AVR_JTAG_PROG_ReadDATA(0x3300, &(l)))
#define AVR_JTAG_PROG_ReadLockbit(l)				(AVR_JTAG_PROG_INS(0x3600), AVR_JTAG_PROG_ReadDATA(0x3700, &(l)))

// Read Signature
#define AVR_JTAG_PROG_EnterSignByteRead()			AVR_JTAG_PROG_INS(0x2308)
#define AVR_JTAG_PROG_ReadSignByte(sig)				(AVR_JTAG_PROG_INS(0x3200), AVR_JTAG_PROG_ReadDATA(0x3300, &(sig)))

// Read Calibration Byte
#define AVR_JTAG_PROG_EnterCaliByteRead()			AVR_JTAG_PROG_INS(0x2308)
#define AVR_JTAG_PROG_ReadCaliByte(c)				(AVR_JTAG_PROG_INS(0x3600), AVR_JTAG_PROG_ReadDATA(0x3700, &(c)))

// No Operation Command
#define AVR_JTAG_PROG_LoadNoOperationCommand()		(AVR_JTAG_PROG_INS(0x2300), AVR_JTAG_PROG_INS(0x3300))












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

#define poll_start()				prog->poll.start(20, 500)
#define poll_end()					prog->poll.end()
#define poll_ok(o, m, v)			\
	prog->poll.checkok(POLL_CHECK_EQU, (o), 1, (m), (v))

#define delay_ms(ms)				prog->delay.delayms((ms) | 0x8000)
#define delay_us(us)				prog->delay.delayus((us) & 0x7FFF)
#define jtag_commit()				prog->peripheral_commit()

static struct INTERFACES_INFO_T *prog = NULL;

#define AVR_JTAG_SendIns(i)			(ir = (i), \
									 jtag_ir_write(&ir, AVR_JTAG_INS_LEN))
#define AVR_JTAG_SendDat(d, len)	(dr = SYS_TO_LE_U16(d), jtag_dr_write(&dr, (len)))
void AVR_JTAG_ReadDat(uint16_t w, uint16_t* r, uint8_t len)
{
	*r = SYS_TO_LE_U16(w);
	jtag_dr_read(r, len);
}

void AVR_JTAG_WaitComplete(uint16_t cmd)
{
	uint16_t dr;
	
	poll_start();
	AVR_JTAG_PROG_INS(cmd);
	poll_ok(0, 0x02, 0x02);
	poll_end();
}

ENTER_PROGRAM_MODE_HANDLER(avr8jtag)
{
	struct program_info_t *pi = context->pi;
	uint8_t ir;
	uint16_t dr;
	
	prog = context->prog;
	
	if (!pi->frequency)
	{
		pi->frequency = 4500;
	}
	
	// init
	jtag_init();
	jtag_config(pi->frequency, &pi->jtag_pos);
	
	// enter program mode
	AVR_JTAG_Reset(1);
	AVR_JTAG_PROG_Enable(0xA370);
	return jtag_commit();
}

LEAVE_PROGRAM_MODE_HANDLER(avr8jtag)
{
	uint8_t ir;
	uint16_t dr;
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_LoadNoOperationCommand();
	
	AVR_JTAG_PROG_Enable(0);
	
	AVR_JTAG_Reset(0);
	jtag_fini();
	return jtag_commit();
}

ERASE_TARGET_HANDLER(avr8jtag)
{
	uint8_t ir;
	uint16_t dr;
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(area);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(size);
	
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_ChipErase();
	AVR_JTAG_WaitComplete(AVR_JTAG_PROG_ChipEraseComplete_CMD);
	return jtag_commit();
}

WRITE_TARGET_HANDLER(avr8jtag)
{
	struct chip_param_t *param = context->param;
	struct chip_area_info_t *area_info = NULL;
	uint8_t ir;
	uint16_t dr;
	uint32_t i;
	uint32_t ee_page_size;
	vsf_err_t err = VSFERR_NONE;
	
	switch (area)
	{
	case APPLICATION_CHAR:
		AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
		AVR_JTAG_PROG_EnterFlashWrite();
		
		AVR_JTAG_PROG_LoadAddrHighByte(addr >> 9);
		AVR_JTAG_PROG_LoadAddrLowByte(addr >> 1);
		AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_PAGELOAD);
		
		if (param->param[AVR8_PARAM_JTAG_FULL_BITSTREAM])
		{
			jtag_dr_write(buff, (uint16_t)(size * 8));
		}
		else
		{
			for (i = 0; i < size; i++)
			{
				jtag_dr_write(buff + i, 8);
			}
		}
		AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
		AVR_JTAG_PROG_WriteFlashPage();
		AVR_JTAG_WaitComplete(AVR_JTAG_PROG_WriteFlashPageComplete_CMD);
		if (jtag_commit())
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		break;
	case EEPROM_CHAR:
		area_info = target_get_chip_area(param, EEPROM_IDX);
		if (NULL == area_info)
		{
			return VSFERR_FAIL;
		}
		
		ee_page_size = area_info->page_size;
		AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
		AVR_JTAG_PROG_EnterEEPROMWrite();
		
		while (size > 0)
		{
			AVR_JTAG_PROG_LoadAddrHighByte(addr >> 8);
			for (i = 0; i < ee_page_size; i++)
			{
				AVR_JTAG_PROG_LoadAddrLowByte(addr + i);
				AVR_JTAG_PROG_LoadDataByte(buff[i]);
				AVR_JTAG_PROG_LatchData();
			}
			
			// write page
			AVR_JTAG_PROG_WriteEEPROMPage();
			AVR_JTAG_WaitComplete(AVR_JTAG_PROG_WriteEEPROMPageComplete_CMD);
			
			size -= ee_page_size;
			buff += ee_page_size;
			addr += ee_page_size;
		}
		
		if (jtag_commit())
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		break;
	case FUSE_CHAR:
		area_info = target_get_chip_area(param, FUSE_IDX);
		if (NULL == area_info)
		{
			return VSFERR_FAIL;
		}
		
		// low bits
		if (area_info->size > 0)
		{
			AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
			AVR_JTAG_PROG_EnterFuseWrite();
			AVR_JTAG_PROG_LoadDataLowByte(buff[0]);
			AVR_JTAG_PROG_WriteFuseLowByte();
			AVR_JTAG_WaitComplete(AVR_JTAG_PROG_WriteFuseLowByteComplete_CMD);
		}
		// high bits
		if (area_info->size > 1)
		{
			AVR_JTAG_PROG_LoadDataLowByte(buff[1]);
			AVR_JTAG_PROG_WriteFuseHighByte();
			AVR_JTAG_WaitComplete(AVR_JTAG_PROG_WriteFuseHighByteComplete_CMD);
		}
		// extended bits
		if (area_info->size > 2)
		{
			AVR_JTAG_PROG_LoadDataLowByte(buff[2]);
			AVR_JTAG_PROG_WriteFuseExtByte();
			AVR_JTAG_WaitComplete(AVR_JTAG_PROG_WriteFuseExtByteComplete_CMD);
		}
		if (area_info->size > 0)
		{
			if (jtag_commit())
			{
				err = ERRCODE_FAILURE_OPERATION;
				break;
			}
		}
		else
		{
			LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, "fuse", param->chip_name);
			err = VSFERR_NOT_SUPPORT;
			break;
		}
		break;
	case LOCK_CHAR:
		area_info = target_get_chip_area(param, LOCK_IDX);
		if (NULL == area_info)
		{
			return VSFERR_FAIL;
		}
		
		if (area_info->size > 0)
		{
			AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
			AVR_JTAG_PROG_EnterLockbitWrite();
			AVR_JTAG_PROG_LoadDataByte(buff[0]);
			AVR_JTAG_PROG_WriteLockbit();
			AVR_JTAG_WaitComplete(AVR_JTAG_PROG_WriteLockbitComplete_CMD);
			if (jtag_commit())
			{
				err = ERRCODE_FAILURE_OPERATION;
				break;
			}
		}
		else
		{
			LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, "locks", param->chip_name);
			err = VSFERR_NOT_SUPPORT;
			break;
		}
		break;
	default:
		err = VSFERR_NOT_SUPPORT;
		break;
	}
	return err;
}

READ_TARGET_HANDLER(avr8jtag)
{
	struct chip_param_t *param = context->param;
	struct chip_area_info_t *area_info = NULL;
	uint8_t ir;
	uint16_t dr;
	uint32_t i, j, k;
	uint32_t ee_page_size;
	uint8_t page_buf[256 + 1];
	vsf_err_t err = VSFERR_NONE;
	
	switch (area)
	{
	case CHIPID_CHAR:
		AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
		AVR_JTAG_PROG_EnterSignByteRead();
		
		AVR_JTAG_PROG_LoadAddrByte(2);
		AVR_JTAG_PROG_ReadSignByte(buff[0]);
		AVR_JTAG_PROG_LoadAddrByte(1);
		AVR_JTAG_PROG_ReadSignByte(buff[1]);
		AVR_JTAG_PROG_LoadAddrByte(0);
		AVR_JTAG_PROG_ReadSignByte(buff[2]);
		if (jtag_commit())
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		buff[3] = 0;
		break;
	case APPLICATION_CHAR:
		AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
		AVR_JTAG_PROG_EnterFlashRead();
		
		AVR_JTAG_PROG_LoadAddrHighByte(addr >> 9);
		AVR_JTAG_PROG_LoadAddrLowByte(addr >> 1);
		AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_PAGEREAD);
		
		if (param->param[AVR8_PARAM_JTAG_FULL_BITSTREAM])
		{
			dr = 0;
			jtag_dr_write(&dr, 8);
			jtag_dr_read(page_buf, (uint16_t)(size * 8));
		}
		else
		{
			for (i = 0; i < size; i++)
			{
				jtag_dr_read(page_buf + i, 8);
			}
		}
		
		if (jtag_commit())
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		memcpy(buff, page_buf, size);
		break;
	case EEPROM_CHAR:
		area_info = target_get_chip_area(param, EEPROM_IDX);
		if (NULL == area_info)
		{
			return VSFERR_FAIL;
		}
		
		ee_page_size = area_info->page_size;
		AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
		AVR_JTAG_PROG_EnterEEPROMRead();
		
		j = 0;
		k = size;
		while (k > 0)
		{
			for (i = 0; i < ee_page_size; i++)
			{
				AVR_JTAG_PROG_LoadAddrHighByte(addr >> 8);
				AVR_JTAG_PROG_LoadAddrLowByte(addr + i);
				AVR_JTAG_PROG_ReadEEPROM(addr + i, page_buf[j + i]);
			}
			k -= ee_page_size;
			addr += ee_page_size;
			j += ee_page_size;
		}
		
		if (jtag_commit())
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		memcpy(buff, page_buf, size);
		break;
	case FUSE_CHAR:
		area_info = target_get_chip_area(param, FUSE_IDX);
		if (NULL == area_info)
		{
			return VSFERR_FAIL;
		}
		
		// low bits
		memset(page_buf, 0, 3);
		if (area_info->size > 3)
		{
			LOG_ERROR(ERRMSG_INVALID_VALUE, area_info->size,
						"avr8 fuse size");
			err = ERRCODE_INVALID;
			break;
		}
		if (area_info->size > 0)
		{
			AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
			AVR_JTAG_PROG_EnterFuseLockbitRead();
			AVR_JTAG_PROG_ReadFuseLowByte(page_buf[0]);
		}
		else
		{
			LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, "fuse", param->chip_name);
			err = VSFERR_NOT_SUPPORT;
			break;
		}
		// high bits
		if (area_info->size > 1)
		{
			AVR_JTAG_PROG_ReadFuseHighByte(page_buf[1]);
		}
		// extended bits
		if (area_info->size > 2)
		{
			AVR_JTAG_PROG_ReadExtFuseByte(page_buf[2]);
		}
		
		if (jtag_commit())
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		memcpy(buff, page_buf, area_info->size);
		break;
	case LOCK_CHAR:
		area_info = target_get_chip_area(param, LOCK_IDX);
		if (NULL == area_info)
		{
			return VSFERR_FAIL;
		}
		
		if (area_info->size > 0)
		{
			AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
			AVR_JTAG_PROG_EnterFuseLockbitRead();
			AVR_JTAG_PROG_ReadLockbit(page_buf[0]);
			if (jtag_commit())
			{
				err = ERRCODE_FAILURE_OPERATION;
				break;
			}
			buff[0] = page_buf[0];
		}
		else
		{
			LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, "locks", param->chip_name);
			err = VSFERR_NOT_SUPPORT;
			break;
		}
		break;
	case CALIBRATION_CHAR:
		area_info = target_get_chip_area(param, CALIBRATION_IDX);
		if (NULL == area_info)
		{
			return VSFERR_FAIL;
		}
		
		memset(page_buf, 0, 4);
		if (area_info->size > 4)
		{
			LOG_ERROR(ERRMSG_INVALID_VALUE,
				area_info->size, "avr8 cali size");
			err = ERRCODE_INVALID;
			break;
		}
		if (area_info->size > 0)
		{
			AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
			AVR_JTAG_PROG_EnterCaliByteRead();
			AVR_JTAG_PROG_LoadAddrByte(0);
			AVR_JTAG_PROG_ReadCaliByte(page_buf[0]);
		}
		else
		{
			LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, "calibration", param->chip_name);
			err = VSFERR_NOT_SUPPORT;
			break;
		}
		if (area_info->size > 1)
		{
			AVR_JTAG_PROG_LoadAddrByte(1);
			AVR_JTAG_PROG_ReadCaliByte(page_buf[1]);
		}
		if (area_info->size > 2)
		{
			AVR_JTAG_PROG_LoadAddrByte(2);
			AVR_JTAG_PROG_ReadCaliByte(page_buf[2]);
		}
		if (area_info->size > 3)
		{
			AVR_JTAG_PROG_LoadAddrByte(3);
			AVR_JTAG_PROG_ReadCaliByte(page_buf[3]);
		}
		
		if (jtag_commit())
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		memcpy(buff, page_buf, area_info->size);
		break;
	default:
		err = VSFERR_NOT_SUPPORT;
		break;
	}
	return err;
}

#endif
