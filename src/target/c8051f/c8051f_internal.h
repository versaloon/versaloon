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
#ifndef __C8051F_INTERNAL_H_INCLUDED__
#define __C8051F_INTERNAL_H_INCLUDED__

#define C8051F_FLASH_CHAR				0xFF
#define C8051F_JTAG						0
#define C8051F_C2						1

#define C8051F_PARAM_ERASE_ADDR			0
#define C8051F_PARAM_FPCTL_ADDR			1
#define C8051F_PARAM_FPDAT_ADDR			2

// JTAG
#define C8051F_JTAG_MAX_POLL_COUNT		10

#define C8051F_JTAG_ID_MASK				0x00FFFFFF


#define C8051F_IR_LEN					16

#define C8051F_IR_STATECNTL_NORMAL		0x0000
#define C8051F_IR_STATECNTL_HALT		0x1000
#define C8051F_IR_STATECNTL_RESET		0x2000
#define C8051F_IR_STATECNTL_SUSPEND		0x4000

#define C8051F_IR_EXTEST				0x0000
#define C8051F_IR_SAMPLE_PRELOAD		0x0002
#define C8051F_IR_IDCODE				0x0004
#define C8051F_IR_BYPASS				0x0FFF
#define C8051F_IR_FLASHCON				0x0082
#define C8051F_IR_FLASHDAT				0x0083
#define C8051F_IR_FLASHADR				0x0084
#define C8051F_IR_FLASHSCL				0x0085
#define C8051F_IR_MASK					0x0FFF

#define C8051F_DR_IDCODE_LEN			32
#define C8051F_DR_FLASHCON_LEN			8
#define C8051F_DR_FLASHDAT_RLEN			10
#define C8051F_DR_FLASHDAT_WLEN			8
#define C8051F_DR_FLASHADR_LEN			16
#define C8051F_DR_FLASHSCL_LEN			8

#define C8051F_INDOPTCODE_POLL			0x00
#define C8051F_INDOPTCODE_READ			0x02
#define C8051F_INDOPTCODE_WRITE			0x03

extern struct program_functions_t c8051fjtag_program_functions;


// C2
#define C8051F_C2_DEVICEID				0x00
#define C8051F_C2_REVID					0x01

#define C8051F_C2_CMD_BLOCK_READ		0x06
#define C8051F_C2_CMD_BLOCK_WRITE		0x07
#define C8051F_C2_CMD_PAGE_ERASE		0x08
#define C8051F_C2_CMD_DEVICE_ERASE		0x03

#define C8051F_C2_REP_INVALIE_COMMAND	0x00
#define C8051F_C2_REP_COMMAND_FAILED	0x02
#define C8051F_C2_REP_COMMAND_OK		0x0D

extern struct program_functions_t c8051fc2_program_functions;

#endif /* __C8051F_INTERNAL_H_INCLUDED__ */

