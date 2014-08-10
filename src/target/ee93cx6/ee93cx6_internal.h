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
#ifndef __EE93CX6_INTERNAL_H_INCLUDED__
#define __EE93CX6_INTERNAL_H_INCLUDED__

#define EE93CX6_OPCODE_READ					0x02
#define EE93CX6_OPCODE_READ_BITLEN			2
#define EE93CX6_OPCODE_WRITE				0x01
#define EE93CX6_OPCODE_WRITE_BITLEN			2
#define EE93CX6_OPCODE_ERASE				0x03
#define EE93CX6_OPCODE_ERASE_BITLEN			2

#define EE93CX6_OPCODE_WEN					0x03
#define EE93CX6_OPCODE_WEN_BITLEN			4
#define EE93CX6_OPCODE_WDS					0x00
#define EE93CX6_OPCODE_WDS_BITLEN			4
#define EE93CX6_OPCODE_ERAL					0x02
#define EE93CX6_OPCODE_ERAL_BITLEN			4
#define EE93CX6_OPCODE_WRAL					0x01
#define EE93CX6_OPCODE_WRAL_BITLEN			4

#define EE93CX6_PARAM_ADDR_BITLEN			0
#define EE93CX6_PARAM_OPCODE_BITLEN			1

#define EE93CX6_MODE_BYTE					0
#define EE93CX6_MODE_WORD					1

#endif /* __EE93CX6_INTERNAL_H_INCLUDED__ */

