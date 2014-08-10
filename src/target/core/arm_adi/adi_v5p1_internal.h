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

#ifndef __ADI_V5P1_INTERNAL_H_INCLUDED__
#define __ADI_V5P1_INTERNAL_H_INCLUDED__

#define ADI_DP_IR_DPACC						0
#define ADI_DP_IR_APACC						1

#define ADI_SWDDP_REG_DPIDR					0x00
#define ADI_SWDDP_REG_ABORT					0x00
#define ADI_SWDDP_REG_DLCR					0x04
#define ADI_SWDDP_REG_RESEND				0x08

#define ADI_SWDDP_REG_ABORT_DAPABORT		(1<<0)
#define ADI_SWDDP_REG_ABORT_STKCMPCLR		(1<<1)
#define ADI_SWDDP_REG_ABORT_STKERRCLR		(1<<2)
#define ADI_SWDDP_REG_ABORT_WDERRCLR		(1<<3)
#define ADI_SWDDP_REG_ABORT_ORUNERRCLR		(1<<4)

#define ADI_JTAGDP_IRLEN					4
#define ADI_JTAGDP_IR_ABORT					0x04
#define ADI_JTAGDP_IR_ABORT_LEN				32
#define ADI_JTAGDP_IR_DPACC					0x0A
#define ADI_JTAGDP_IR_APACC					0x0B
#define ADI_JTAGDP_IR_APDPACC_LEN			35
#define ADI_JTAGDP_IR_IDCODE				0x0E
#define ADI_JTAGDP_IR_IDCODE_LEN			32
#define ADI_JTAGDP_IR_BYPASS				0x0F

#define ADI_DAP_READ						1
#define ADI_DAP_WRITE						0

#define ADI_SWDDP_ACK_OK					0x01
#define ADI_SWDDP_ACK_WAIT					0x02
#define ADI_SWDDP_ACK_FAIL					0x04

#define ADI_JTAGDP_ACK_WAIT					0x01
#define ADI_JTAGDP_ACK_OK_FAIL				0x02

#define ADI_DP_REG_CTRL_STAT				0x04
#define ADI_DP_REG_SELECT					0x08
#define ADI_DP_REG_RDBUFF					0x0C

#define	ADI_AP_REG_CSW						0x00
#define ADI_AP_REG_TAR						0x04
#define ADI_AP_REG_DRW						0x0C
#define ADI_AP_REG_BD0						0x10
#define ADI_AP_REG_BD1						0x14
#define ADI_AP_REG_BD2						0x18
#define ADI_AP_REG_BD3						0x1C
#define ADI_AP_REG_CFG						0xF4
#define ADI_AP_REG_DBGROMA					0xF8
#define ADI_AP_REG_IDR						0xFC

#define ADI_DP_REG_CTRL_STAT_CORUNDETECT	(uint32_t)(1 << 0)
#define ADI_DP_REG_CTRL_STAT_SSTICKYORUN	(uint32_t)(1 << 1)
#define ADI_DP_REG_CTRL_STAT_SSTICKYERR		(uint32_t)(1 << 5)
#define ADI_DP_REG_CTRL_STAT_WDATAERR		(uint32_t)(1 << 7)
#define ADI_DP_REG_CTRL_STAT_CDBGRSTREQ		(uint32_t)(1 << 26)
#define ADI_DP_REG_CTRL_STAT_CDBGRSTACK		(uint32_t)(1 << 27)
#define ADI_DP_REG_CTRL_STAT_CDBGPWRUPREQ	(uint32_t)(1 << 28)
#define ADI_DP_REG_CTRL_STAT_CDBGPWRUPACK	(uint32_t)(1 << 29)
#define ADI_DP_REG_CTRL_STAT_CSYSPWRUPREQ	(uint32_t)(1 << 30)
#define ADI_DP_REG_CTRL_STAT_CSYSPWRUPACK	(uint32_t)(1 << 31)

#define ADI_AP_REG_CSW_8BIT					0
#define ADI_AP_REG_CSW_16BIT				1
#define ADI_AP_REG_CSW_32BIT				2

#define ADI_AP_REG_CSW_ADDRINC_MASK			(uint32_t)(3 << 4)
#define ADI_AP_REG_CSW_ADDRINC_OFF			(uint32_t)0
#define ADI_AP_REG_CSW_ADDRINC_SINGLE		(uint32_t)(1 << 4)
#define ADI_AP_REG_CSW_ADDRINC_PACKED		(uint32_t)(2 << 4)
#define ADI_AP_REG_CSW_HPROT				(uint32_t)(1 << 25)
#define ADI_AP_REG_CSW_MASTER_DEBUG			(uint32_t)(1 << 29)
#define ADI_AP_REG_CSW_DBGSWENABLE			(uint32_t)(1 << 31)

#endif		// __ADI_V5P1_INTERNAL_H_INCLUDED__

