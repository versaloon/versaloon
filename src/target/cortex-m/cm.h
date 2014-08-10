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
#ifndef __CM_H_INCLUDED__
#define __CM_H_INCLUDED__

#define CM_STRING						"cm"

extern struct program_functions_t cm_program_functions;
extern const struct vss_cmd_t cm_notifier[];

#include "adi_v5p1.h"
#include "cm_common.h"
struct cm_info_t
{
	// first member should be same as used in cm_common module
	// because this class in inherited from cm_common
	struct cm_common_info_t cm_common;
	
	uint8_t index;
};

#endif /* __CM_H_INCLUDED__ */

