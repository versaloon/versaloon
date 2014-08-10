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
#ifndef __NAND_H_INCLUDED__
#define __NAND_H_INCLUDED__

#define NAND_STRING						"nand"

extern struct program_area_map_t nand_program_area_map[];
extern const struct program_mode_t nand_program_mode[];
extern const struct program_functions_t nand_program_functions;
extern const struct vss_cmd_t nand_notifier[];

ADJUST_SETTING_HANDLER(nand);

#endif /* __NAND_H_INCLUDED__ */

