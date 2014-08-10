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

#ifndef __AVR32_INTERNAL_H_INCLUDED__
#define __AVR32_INTERNAL_H_INCLUDED__

#define AVR32_JTAG								0

#define AVR32_JTAG_INS_Len						5
#define AVR32_JTAG_INS_IDCODE					0x01
#define AVR32_JTAG_INS_SAMPLE_PRELOAD			0x02
#define AVR32_JTAG_INS_EXTEST					0x03
#define AVR32_JTAG_INS_INTEST					0x04
#define AVR32_JTAG_INS_CLAMP					0x06
#define AVR32_JTAG_INS_RESET					0x0C
#define AVR32_JTAG_INS_NEXUS_ACCESS				0x10
#define AVR32_JTAG_INS_MEMORY_WORD_ACCESS		0x11
#define AVR32_JTAG_INS_MEMORY_BLOCK_ACCESS		0x12
#define AVR32_JTAG_INS_CANCEL_ACCESS			0x13
#define AVR32_JTAG_INS_MEMORY_SERVICE			0x14
#define AVR32_JTAG_INS_MEMORY_SIZED_ACCESS		0x15
#define AVR32_JTAG_INS_SYNC						0x17
#define AVR32_JTAG_INS_HALT						0x1C
#define AVR32_JTAG_INS_BYPASS					0x1F

#define AVR32_JTAG_DR_RESET_LEN					5
#define AVR32_JTAG_DR_RESET_CPU					0x01
#define AVR32_JTAG_DR_RESET_APP					0x08
#define AVR32_JTAG_DR_RESET_OCD					0x10
#define AVR32_JTAG_DR_RESET_ALL					0x1F

#define AVR32_JTAG_IRRET_BUSY					0x04
#define AVR32_JTAG_IRRET_ERROR					0x08
#define AVR32_JTAG_IRRET_PROTECT				0x10

#define AVR32_JTAG_DRRET_BUSY					0x01
#define AVR32_JTAG_DRRET_ERROR					0x02

#define AVR32_JTAG_READ							1
#define AVR32_JTAG_WRITE						0

#define AVR32_JTAG_RTI_CYCLE					1

#define AVR32_SAB_SLAVE_OCD						0x1
#define AVR32_SAB_SLAVE_HSB						0x5
#define AVR32_SAB_SLAVE_MSU						0x6

// Device ID
#define AVR32_OCDREG_DID						0x00000000
// Development Control
#define AVR32_OCDREG_DC							0x00000008
// Development Status
#define AVR32_OCDREG_DS							0x00000010
// Read/Write Access Control/Status
#define AVR32_OCDREG_RWCS						0x0000001C

// PM
#define	AVR32_PM_BASE							0xFFFF0C00
#define AVR32_PM_CPUMASK						(AVR32_PM_BASE + 0x08)
#define AVR32_PM_HSBMASK						(AVR32_PM_BASE + 0x0C)
#define AVR32_PM_PBAMASK						(AVR32_PM_BASE + 0x10)
#define AVR32_PM_PBBMASK						(AVR32_PM_BASE + 0x14)

// FLASHC
#define AVR32_FLASHC_BASE						0xFFFE1400
#define AVR32_FLASHC_FCR						(AVR32_FLASHC_BASE + 0x04)
#define AVR32_FLASHC_FSR						(AVR32_FLASHC_BASE + 0x08)
#define AVR32_FLASHC_FGPFRHI					(AVR32_FLASHC_BASE + 0x0C)
#define AVR32_FLASHC_FGPFRLO					(AVR32_FLASHC_BASE + 0x10)

#define AVR32_FLASHC_FCMD_KEY					0xA5000000
#define AVR32_FLASHC_FCMD_NOP					0
#define AVR32_FLASHC_FCMD_WP					1
#define AVR32_FLASHC_FCMD_EP					2
#define AVR32_FLASHC_FCMD_CPB					3
#define AVR32_FLASHC_FCMD_LP					4
#define AVR32_FLASHC_FCMD_UP					5
#define AVR32_FLASHC_FCMD_EA					6
#define AVR32_FLASHC_FCMD_WGPB					7
#define AVR32_FLASHC_FCMD_EGPB					8
#define AVR32_FLASHC_FCMD_SSB					9
#define AVR32_FLASHC_FCMD_PGPFB					10
#define AVR32_FLASHC_FCMD_EAGPF					11
#define AVR32_FLASHC_FCMD_QPR					12
#define AVR32_FLASHC_FCMD_WUP					13
#define AVR32_FLASHC_FCMD_EUP					14
#define AVR32_FLASHC_FCMD_QPRUP					15

#endif /* __AVR32_INTERNAL_H_INCLUDED__ */

