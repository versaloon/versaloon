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
#ifndef __HCS08_INTERNAL_H_INCLUDED__
#define __HCS08_INTERNAL_H_INCLUDED__

#define HCS08_NONVIOL_REG_START				0xFFB0
#define HCS08_NONVIOL_REG_END				0xFFBF

#define HCS08_DIDH_ADDR						0x1806
#define HCS08_DIDL_ADDR						0x1807

#define HCS08_BDCSCR_ENBDM					0x80
#define HCS08_BDCSCR_BDMACT					0x40
#define HCS08_BDCSCR_BKPTEN					0x20
#define HCS08_BDCSCR_FTS					0x10
#define HCS08_BDCSCR_CLKSW					0x08
#define HCS08_BDCSCR_WS						0x04
#define HCS08_BDCSCR_WSF					0x02
#define HCS08_BDCSCR_DVF					0x01

#define HCS08_FCDIV_ADDR					0x1820
#define HCS08_FCDIV_PRDIV8					0x40
#define HCS08_FCDIV_DIVMASK					0x3F

#define HCS08_FOPT_ADDR						0x1821
#define HCS08_FCNFG_ADDR					0x1823
#define HCS08_FPROT_ADDR					0x1824

#define HCS08_FSTAT_ADDR					0x1825
#define HCS08_FSTAT_FCBEF					0x80
#define HCS08_FSTAT_FCCF					0x40
#define HCS08_FSTAT_FPVIOL					0x20
#define HCS08_FSTAT_FACCERR					0x10
#define HCS08_FSTAT_FBLANK					0x04

#define HCS08_FCMD_ADDR						0x1826
#define HCS08_FCMD_MBLANK					0x05
#define HCS08_FCMD_MBYTEPROG				0x20
#define HCS08_FCMD_MBURSTPROG				0x25
#define HCS08_FCMD_MPAGEERASE				0x40
#define HCS08_FCMD_MMASSERASE				0x41

#define HCS08_SOPT_ADDR						0x1802
#define HCS08_SOPT_COPE						0x80
#define HCS08_SOPT_BKGDPE					0x02
#define HCS08_SOPT_RSTPE					0x01

#define HCS08_NVFEOPT_ADDR					0xFFBF

#endif /* __HCS08_INTERNAL_H_INCLUDED__ */

