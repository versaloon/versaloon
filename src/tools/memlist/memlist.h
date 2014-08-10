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
#ifndef __MEMLIST_H_INCLUDED__
#define __MEMLIST_H_INCLUDED__

#include "list.h"

struct memlist
{
	uint32_t addr;
	uint32_t len;
	uint8_t *buff;
	struct sllist list;
};

#define MEMLIST_GetNext(ml)						\
					sllist_get_container(ml->list.next, struct memlist, list)

uint32_t MEMLIST_CalcAllSize(struct memlist *ml);
vsf_err_t MEMLIST_Add(struct memlist **ml, uint32_t addr, uint32_t len,
						uint32_t page_size, uint8_t *buff);
void MEMLIST_Free(struct memlist **ml);

#endif /* __MEMLIST_H_INCLUDED__ */

