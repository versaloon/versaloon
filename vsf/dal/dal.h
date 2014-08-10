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

#ifndef __DAL_H_INCLUDED__
#define __DAL_H_INCLUDED__

#include "app_cfg.h"
#include "app_type.h"
#include "dal_cfg.h"
#include "interfaces.h"

struct dal_info_t
{
	void *ifs;
	void *param;
	void *info;
	void *extra;
};

struct dal_driver_t
{
	char *name;
#if DAL_INTERFACE_PARSER_EN
	char *ifs_format;
	vsf_err_t (*parse_interface)(struct dal_info_t *info, uint8_t *buff);
#endif
};

extern struct dal_driver_t *dal_drivers[];

#endif	// __DAL_H_INCLUDED__
