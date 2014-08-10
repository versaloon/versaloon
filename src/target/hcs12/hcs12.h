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
#ifndef __HCS12_H_INCLUDED__
#define __HCS12_H_INCLUDED__

#define HCS12_STRING				"hcs12"

extern const struct program_area_map_t hcs12_program_area_map[];
extern const struct program_mode_t hcs12_program_mode[];
extern const struct program_functions_t hcs12_program_functions;
extern const struct vss_cmd_t hcs12_notifier[];

ADJUST_MAPPING_HANDLER(hcs12);

#endif /* __HCS12_H_INCLUDED__ */

