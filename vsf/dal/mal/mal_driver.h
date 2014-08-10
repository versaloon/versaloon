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

#define MAL_SUPPORT_ERASEALL				(1 << 0)
#define MAL_SUPPORT_ERASEBLOCK				(1 << 1)
#define MAL_SUPPORT_READBLOCK				(1 << 2)
#define MAL_SUPPORT_WRITEBLOCK				(1 << 3)

struct mal_driver_t
{
	struct dal_driver_t driver;
	
	uint8_t support;
	
	vsf_err_t (*init_nb)(struct dal_info_t *param);
	vsf_err_t (*init_nb_isready)(struct dal_info_t *param);
	vsf_err_t (*fini)(struct dal_info_t *param);
	vsf_err_t (*getinfo)(struct dal_info_t *param);
	vsf_err_t (*poll)(struct dal_info_t *param);

	vsf_err_t (*format_nb_start)(struct dal_info_t *param);
	vsf_err_t (*format_nb_isready)(struct dal_info_t *param);
	vsf_err_t (*format_waitready)(struct dal_info_t *param);
	vsf_err_t (*format_nb_end)(struct dal_info_t *param);
	
	vsf_err_t (*eraseall_nb_start)(struct dal_info_t *param);
	vsf_err_t (*eraseall_nb_isready)(struct dal_info_t *param);
	vsf_err_t (*eraseall_waitready)(struct dal_info_t *param);
	vsf_err_t (*eraseall_nb_end)(struct dal_info_t *param);
	
	vsf_err_t (*eraseblock_nb_start)(struct dal_info_t *param, uint64_t address, 
										uint64_t count);
	vsf_err_t (*eraseblock_nb)(struct dal_info_t *param, uint64_t address);
	vsf_err_t (*eraseblock_nb_isready)(struct dal_info_t *param,
										uint64_t address);
	vsf_err_t (*eraseblock_waitready)(struct dal_info_t *param, 
											uint64_t address);
	vsf_err_t (*eraseblock_nb_end)(struct dal_info_t *param);
	
	vsf_err_t (*readblock_nb_start)(struct dal_info_t *param, uint64_t address, 
										uint64_t count, uint8_t *buff);
	vsf_err_t (*readblock_nb)(struct dal_info_t *param, uint64_t address, 
								uint8_t *buff);
	vsf_err_t (*readblock_nb_isready)(struct dal_info_t *param, uint64_t address, 
										uint8_t *buff);
	vsf_err_t (*readblock_waitready)(struct dal_info_t *param, 
										uint64_t address, uint8_t *buff);
	vsf_err_t (*readblock_nb_end)(struct dal_info_t *param);
	
	vsf_err_t (*writeblock_nb_start)(struct dal_info_t *param, uint64_t address, 
										uint64_t count, uint8_t *buff);
	vsf_err_t (*writeblock_nb)(struct dal_info_t *param, uint64_t address, 
								uint8_t *buff);
	vsf_err_t (*writeblock_nb_isready)(struct dal_info_t *param,
										uint64_t address, uint8_t *buff);
	vsf_err_t (*writeblock_waitready)(struct dal_info_t *param, 
											uint64_t address, uint8_t *buff);
	vsf_err_t (*writeblock_nb_end)(struct dal_info_t *param);
};

