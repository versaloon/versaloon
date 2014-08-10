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

#ifndef __AVRXMEGA_INTERNAL_H_INCLUDED__
#define __AVRXMEGA_INTERNAL_H_INCLUDED__

#define AVRXMEGA_JTAG							0
#define AVRXMEGA_PDI							1

#define AVRXMEGA_JTAG_RTI_CYCLE					0

#define AVRXMEGA_JTAG_INS_Len					4
#define AVRXMEGA_JTAG_INS_IDCODE				0x03
#define AVRXMEGA_JTAG_INS_SAMPLE_PRELOAD		0x02
#define AVRXMEGA_JTAG_INS_EXTEST				0x01
#define AVRXMEGA_JTAG_INS_CLAMP					0x04
#define AVRXMEGA_JTAG_INS_HIGHZ					0x05
#define	AVRXMEGA_JTAG_INS_PDICOM				0x07
#define AVRXMEGA_JTAG_INS_BYPASS				0x0F

#define AVRXMEGA_JTAG_DR_PDICOM_LEN				9

#define AVRXMEGA_JTAG_BREAK						0x01BB
#define AVRXMEGA_JTAG_DELAY						0x01DB
#define AVRXMEGA_JTAG_EMPTY						0x01EB

// PDI Controller
enum pdi_parity_t
{
	PDI_PARITY_EVEN = 0,
	PDI_PARITY_ODD = 1
};

#define PDI_DATASIZE_1BYTE						0
#define PDI_DATASIZE_2BYTES						1
#define PDI_DATASIZE_3BYTES						2
#define PDI_DATASIZE_4BYTES						3

#define PDI_PTR_INDIRECT						0
#define PDI_PTR_INDIRECT_PI						1
#define PDI_PTR_DIRECT							2
#define PDI_PTR_DIRECT_PI						3

#define PDI_INSTR_LDS(sizea, sizeb)				\
					(0x00 | ((((sizea) & 0x03) << 2) | ((sizeb) & 0x03)))
#define PDI_INSTR_STS(sizea, sizeb)				\
					(0x40 | ((((sizea) & 0x03) << 2) | ((sizeb) & 0x03)))
#define PDI_INSTR_LD(ptr, size)					\
					(0x20 | ((((ptr) & 0x03) << 2) | ((size) & 0x03)))
#define PDI_INSTR_ST(ptr, size)					\
					(0x60 | ((((ptr) & 0x03) << 2) | ((size) & 0x03)))
#define PDI_INSTR_LDCS(addr)					(0x80 | (addr & 0x0F))
#define PDI_INSTR_STCS(addr)					(0xC0 | (addr & 0x0F))
#define PDI_INSTR_REPEAT(size)					(0xA0 | (size & 0x03))
#define PDI_INSTR_KEY							0xE0

#define PDI_REG_STATUS							0
#define PDI_REG_RESET							1
#define PDI_REG_CTRL							2

#define PDI_REG_RESET_KEY						0x59

#define PDI_REG_STATUS_NVM						0x02

#define PDI_NVM_KEY								0x1289AB45CDD888FF

#define AVRXMEGA_PDIBUS_APP_BASE				0x00800000
#define AVRXMEGA_PDIBUS_EE_BASE					0x008C0000
#define AVRXMEGA_PDIBUS_PRODSIG_BASE			0x008E0200
#define AVRXMEGA_PDIBUS_USRSIG_BASE				0x008E0400
#define AVRXMEGA_PDIBUS_FUSE_BASE				0x008F0020
#define AVRXMEGA_PDIBUS_DATA_BASE				0x01000000

// NVM Controller
#define AVRXMEGA_NVM_BASE						(AVRXMEGA_PDIBUS_DATA_BASE + 0x1C0)

#define AVRXMEGA_NVM_REG_ADDR0					(AVRXMEGA_NVM_BASE + 0x00)
#define AVRXMEGA_NVM_REG_ADDR1					(AVRXMEGA_NVM_BASE + 0x01)
#define AVRXMEGA_NVM_REG_ADDR2					(AVRXMEGA_NVM_BASE + 0x02)
#define AVRXMEGA_NVM_REG_DATA0					(AVRXMEGA_NVM_BASE + 0x04)
#define AVRXMEGA_NVM_REG_DATA1					(AVRXMEGA_NVM_BASE + 0x05)
#define AVRXMEGA_NVM_REG_DATA2					(AVRXMEGA_NVM_BASE + 0x06)
#define AVRXMEGA_NVM_REG_CMD					(AVRXMEGA_NVM_BASE + 0x0A)
#define AVRXMEGA_NVM_REG_CTRLA					(AVRXMEGA_NVM_BASE + 0x0B)
#define AVRXMEGA_NVM_REG_CTRLB					(AVRXMEGA_NVM_BASE + 0x0C)
#define AVRXMEGA_NVM_REG_INTCTRL				(AVRXMEGA_NVM_BASE + 0x0D)
#define AVRXMEGA_NVM_REG_STATUS					(AVRXMEGA_NVM_BASE + 0x0F)
#define AVRXMEGA_NVM_REG_LOCKBITS				(AVRXMEGA_NVM_BASE + 0x10)

#define AVRXMEGA_NVM_REG_STATUS_BUSY			0x80

#define AVRXMEGA_NVM_REG_CTRLA_CMDEX			0x01

#define AVRXMEGA_NVM_CMD_NOOP					0x00
#define AVRXMEGA_NVM_CMD_CHIPERASE				0x40
#define AVRXMEGA_NVM_CMD_READNVM				0x43
#define AVRXMEGA_NVM_CMD_LOADFLASHPAGEBUFF		0x23
#define AVRXMEGA_NVM_CMD_ERASEFLASHPAGEBUFF		0x26
#define AVRXMEGA_NVM_CMD_ERASEFLASHPAGE			0x2B
#define AVRXMEGA_NVM_CMD_WRITEFLASHPAGE			0x2E
#define AVRXMEGA_NVM_CMD_ERASEWRITEFLASH		0x2F
#define AVRXMEGA_NVM_CMD_FLASHCRC				0x78
#define AVRXMEGA_NVM_CMD_ERASEAPPSEC			0x20
#define AVRXMEGA_NVM_CMD_ERASEAPPSECPAGE		0x22
#define AVRXMEGA_NVM_CMD_WRITEAPPSECPAGE		0x24
#define AVRXMEGA_NVM_CMD_ERASEWRITEAPPSECPAGE	0x25
#define AVRXMEGA_NVM_CMD_APPCRC					0x38
#define AVRXMEGA_NVM_CMD_ERASEBOOTSEC			0x68
#define AVRXMEGA_NVM_CMD_ERASEBOOTSECPAGE		0x2A
#define AVRXMEGA_NVM_CMD_WRITEBOOTSECPAGE		0x2C
#define AVRXMEGA_NVM_CMD_ERASEWRITEBOOTSECPAGE	0x2D
#define AVRXMEGA_NVM_CMD_BOOTCRC				0x39
#define AVRXMEGA_NVM_CMD_READUSERSIG			0x03
#define AVRXMEGA_NVM_CMD_ERASEUSERSIG			0x18
#define AVRXMEGA_NVM_CMD_WRITEUSERSIG			0x1A
#define AVRXMEGA_NVM_CMD_READCALIBRATION		0x02
#define AVRXMEGA_NVM_CMD_READFUSE				0x07
#define AVRXMEGA_NVM_CMD_WRITEFUSE				0x4C
#define AVRXMEGA_NVM_CMD_WRITELOCK				0x08
#define AVRXMEGA_NVM_CMD_LOADEEPROMPAGEBUFF		0x33
#define AVRXMEGA_NVM_CMD_ERASEEEPROMPAGEBUFF	0x36
#define AVRXMEGA_NVM_CMD_ERASEEEPROM			0x30
#define AVRXMEGA_NVM_CMD_ERASEEEPROMPAGE		0x32
#define AVRXMEGA_NVM_CMD_WRITEEEPROMPAGE		0x34
#define AVRXMEGA_NVM_CMD_ERASEWRITEEEPROMPAGE	0x35
#define AVRXMEGA_NVM_CMD_READEEPROM				0x06

#endif /* __AVRXMEGA_INTERNAL_H_INCLUDED__ */

