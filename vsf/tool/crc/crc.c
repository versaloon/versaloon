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

#include "app_type.h"

#include "crc.h"

uint32_t crc_calc(struct crc_t *crc, void *buff, uint32_t num)
{
	uint8_t bitlen = (uint8_t)crc->bitlen, i;
	uint32_t result = crc->result;
	uint8_t *buff8 = (uint8_t *)buff;
	uint16_t *buff16 = (uint16_t *)buff;
	uint32_t *buff32 = (uint32_t *)buff;
	
	while (num--)
	{
		switch (crc->bitlen)
		{
		case CRC_BITLEN_8:
			result ^= *buff8++;
			break;
		case CRC_BITLEN_16:
			result ^= *buff16++;
			break;
		case CRC_BITLEN_32:
			result ^= *buff32++;
			break;
		}
		for (i = 0; i < bitlen; i++)
		{
			if (result & 0x80)
			{
				result = (result << 1) ^ crc->poly;
			}
			else
			{
				result <<= 1;
			}
		}
	}
	result &= (1 << bitlen) - 1;
	crc->result = result;
	return result;
}
