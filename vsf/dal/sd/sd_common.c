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

#include "app_cfg.h"
#include "app_type.h"

#include "../mal/mal.h"
#include "../mal/mal_driver.h"

#if DAL_SD_SPI_EN || DAL_SD_SDIO_EN

#include "sd_common.h"

uint8_t sd_spi_cmd_chksum(uint8_t *data, uint32_t num)
{
	uint8_t crc, mask, i;
	uint32_t j;
	
	crc = 0;
	for (j = 0; j < num; j++)
	{
		mask = data[j] & 0x80 ? 0x40 : 0x00;
		if ((crc & 0x40) ^ mask)
		{
			crc <<= 1;
			crc ^= SD_CRC7_POLY;
		}
		else
		{
			crc <<= 1;
		}
		crc ^= data[j];
		
		for (i = 0; i < 7; i++)
		{
			if (crc & 0x40)
			{
				crc <<= 1;
				crc ^= SD_CRC7_POLY;
			}
			else
			{
				crc <<= 1;
			}
		}
	}
	return crc & 0x7F;
}

vsf_err_t sd_parse_csd(uint8_t *csd, struct sd_info_t *info)
{
	struct mal_capacity_t tmp_cap;
	uint32_t c_size, c_size_mult, bl_len;
	
	tmp_cap.block_size = 512;
	if (csd[0] & 0x40)
	{
		// csd 2.0
		tmp_cap.block_number = ((((csd[7] & 0xFC) << 16) | 
			((csd[8] & 0xFF) <<  8) | ((csd[9] & 0xFF) <<  0)) + 1) * 1024;
	}
	else
	{
		// csd 1.0
		c_size = csd[6] & 0x03;
		c_size <<= 8;
		c_size += csd[7];
		c_size <<= 2;
		c_size += ((csd[8] & 0xc0) >> 6);
		c_size_mult = csd[9] & 0x03;
		c_size_mult <<= 1;
		c_size_mult += ((csd[10] & 0x80) >> 7);
		bl_len = csd[5] & 0x0F;
		tmp_cap.block_number = (c_size + 1) * (1 << (c_size_mult + 2)) * 
								(1 << bl_len) / tmp_cap.block_size;
	}
	if (info != NULL)
	{
		info->capacity = tmp_cap;
	}
	return VSFERR_NONE;
}

#endif
