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
#ifndef __HEX_H_INCLUDED__
#define __HEX_H_INCLUDED__

vsf_err_t read_hex_file(FILE *hex_file, WRITE_MEMORY_CALLBACK callback,
				void *buffer, uint32_t seg_offset, uint32_t addr_offset);
vsf_err_t write_hex_file(FILE *hex_file, uint32_t file_addr, uint8_t *buff,
				uint32_t buff_size, uint32_t seg_addr, uint32_t start_addr,
				ADJUST_MAPPING_CALLBACK remap);
vsf_err_t write_hex_file_end(FILE *hex_file);

#endif /* __HEX_H_INCLUDED__ */

