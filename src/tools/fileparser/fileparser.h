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

#ifndef __FILE_PARSER_H_INCLUDED__
#define __FILE_PARSER_H_INCLUDED__

#include "filelist.h"

typedef vsf_err_t (*WRITE_MEMORY_CALLBACK)(char *, uint32_t, uint32_t, uint8_t*,
										uint32_t, void *);
typedef vsf_err_t (*ADJUST_MAPPING_CALLBACK)(uint32_t *address, uint8_t dir);

struct file_parser_t
{
	char ext[4];
	vsf_err_t (*parse_file)(FILE *file, WRITE_MEMORY_CALLBACK callback,
							void *buffer, uint32_t seg_offset,
							uint32_t addr_offset);
	vsf_err_t (*save_target_to_file)(FILE *file, uint32_t file_addr,
									uint8_t *buff, uint32_t buff_size,
									uint32_t seg_addr, uint32_t start_addr,
									ADJUST_MAPPING_CALLBACK remap);
	vsf_err_t (*end_file)(FILE *file);
};

extern char *fileparser_cur_ext;

vsf_err_t parse_file(char *file_name, FILE *file, void *para,
						WRITE_MEMORY_CALLBACK callback,
						uint32_t seg_offset, uint32_t addr_offset);
vsf_err_t save_target_to_file(struct filelist *fl, uint8_t *buff,
				uint32_t buff_size, uint32_t seg_addr, uint32_t start_addr,
				int32_t fseg, int32_t faddr, ADJUST_MAPPING_CALLBACK remap);
vsf_err_t end_file(struct filelist *fl);

#endif //__FILE_PARSER_H_INCLUDED__

