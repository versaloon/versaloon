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

#ifndef __FAKEFAT32_H_INCLUDED__
#define __FAKEFAT32_H_INCLUDED__

#define FAKEFAT32_FILEATTR_READONLY			(1 << 0)
#define FAKEFAT32_FILEATTR_HIDDEN			(1 << 1)
#define FAKEFAT32_FILEATTR_SYSTEM			(1 << 2)
#define FKAEFAT32_FILEATTR_VOLUMEID			(1 << 3)
#define FAKEFAT32_FILEATTR_DIRECTORY		(1 << 4)
#define FAKEFAT32_FILEATTR_ARCHIVE			(1 << 5)

struct fakefat32_file_t
{
	char *name;
	char *ext;
	uint8_t attr;
	uint32_t size;
	
	struct fakefat32_file_callback_t
	{
//		vsf_err_t (*init)(struct fakefat32_file_t*file);
//		vsf_err_t (*fini)(struct fakefat32_file_t*file);
		vsf_err_t (*read)(struct fakefat32_file_t*file, uint32_t addr,
									uint8_t *buff, uint32_t page_size);
		vsf_err_t (*read_isready)(struct fakefat32_file_t*file, uint32_t addr,
									uint8_t *buff, uint32_t page_size);
		vsf_err_t (*write)(struct fakefat32_file_t*file, uint32_t addr,
									uint8_t *buff, uint32_t page_size);
		vsf_err_t (*write_isready)(struct fakefat32_file_t*file, uint32_t addr,
									uint8_t *buff, uint32_t page_size);
		vsf_err_t (*change_size)(struct fakefat32_file_t*file, uint32_t size);
	} callback;
	
	// filelist under directory
	struct fakefat32_file_t *filelist;
	
	// can be private
	PACKED_HEAD struct PACKED_MID
	{
		uint8_t CrtTimeTenth;
		uint16_t CrtTime;
		uint16_t CrtData;
		uint16_t LstAccData;
		uint16_t FstClusHI;
		uint16_t WrtTime;
		uint16_t WrtData;
		uint16_t FstClusLO;
	} record; PACKED_TAIL
	
	// private
	uint32_t first_cluster;
	struct fakefat32_file_t *parent;
};

struct fakefat32_param_t
{
	uint16_t sector_size;
	uint32_t sector_number;
	uint8_t sectors_per_cluster;
	
	uint32_t volume_id;
	uint32_t disk_id;
	struct fakefat32_file_t root[2];
};

vsf_err_t fakefat32_dir_read(struct fakefat32_file_t*file, uint32_t addr,
									uint8_t *buff, uint32_t page_size);
vsf_err_t fakefat32_dir_write(struct fakefat32_file_t*file, uint32_t addr,
									uint8_t *buff, uint32_t page_size);
extern struct mal_driver_t fakefat32_drv;

#endif	// __FAKEFAT32_H_INCLUDED__
