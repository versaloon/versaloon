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
#ifndef __STM32ISP_H_INCLUDED__
#define __STM32ISP_H_INCLUDED__

#define STM32ISP_SYNC					0x7F
#define STM32ISP_ACK					0x79
#define STM32ISP_NACK					0x1F

// get bootloader version and list of supported commands
#define STM32ISP_CMD_GET				0x00
// get bootloader version and (fake in 2.0) read protection status
#define STM32ISP_CMD_GET_VERSION		0x01
// get device ID
#define STM32ISP_CMD_GET_ID				0x02
// read memory
#define STM32ISP_CMD_READ_MEMORY		0x11
// go
#define STM32ISP_CMD_GO					0x21
// write memory
#define STM32ISP_CMD_WRITE_MEMORY		0x31
// erase
#define STM32ISP_CMD_ERASE				0x43
// enable write protect
#define STM32ISP_CMD_WRITE_PROTECT		0x63
// disable write protect
#define STM32ISP_CMD_WRITE_UNPROTECT	0x73
// enable readout protect
#define STM32ISP_CMD_READOUT_PROTECT	0x82
// disable readout protect
#define STM32ISP_CMD_READOUT_UNPROTECT	0x92

extern struct program_functions_t stm32isp_program_functions;

#endif /* __STM32ISP_H_INCLUDED__ */

