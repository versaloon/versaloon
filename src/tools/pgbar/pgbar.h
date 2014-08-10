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

#ifndef __PGBAR_H_INCLUDED__
#define __PGBAR_H_INCLUDED__

#define PROGRESS_STEP			40
#define PROGRESS_CHAR			'='

extern struct vss_cmd_list_t pgbar_cmd_list;

vsf_err_t pgbar_init(char *s, char *e, uint32_t min, uint32_t max,
						uint32_t max_chars, char c);
uint32_t pgbar_fini(void);
void pgbar_set_gui_mode(uint8_t gui_mode);
void pgbar_update(int32_t step);

#endif /* __PGBAR_H_INCLUDED__ */

