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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "vsf_err.h"

#include "app_type.h"
#include "app_io.h"
#include "app_log.h"
#include "app_err.h"

#include "memlist.h"

#define MEMLIST_AdjustAddr(addr, page_size)		\
						((addr) / (page_size) * (page_size))
#define MEMLIST_AdjustLen(len, page_size)		\
						(((len) + (page_size) - 1) / (page_size) * (page_size))

static vsf_err_t MEMLIST_Merge(struct memlist *ml, uint32_t addr, uint32_t len,
							uint32_t page_size)
{
	struct memlist *ml_tmp = ml;
	uint32_t head = 0, cur_head = 0, tail = 0, cur_tail = 0;
	
	if (NULL == ml_tmp)
	{
		return VSFERR_FAIL;
	}
	
	head = addr;
	tail = addr + len;
	while (ml_tmp != NULL)
	{
		cur_head = ml_tmp->addr;
		cur_tail = ml_tmp->addr + ml_tmp->len;
		
		if (head > cur_tail)
		{
			ml_tmp = MEMLIST_GetNext(ml_tmp);
			continue;
		}
		if (tail < cur_head)
		{
			return VSFERR_FAIL;
		}
		break;
	}
	if (ml_tmp != NULL)
	{
		// merget to ml_tmp
		ml_tmp->addr = MEMLIST_AdjustAddr(min(head, cur_head), page_size);
		ml_tmp->len = MEMLIST_AdjustLen(max(tail, cur_tail) - ml_tmp->addr,
										page_size);
		
		head = ml_tmp->addr;
		tail = ml_tmp->addr + ml_tmp->len;
		// check if next in the list can be merged
		ml = MEMLIST_GetNext(ml_tmp);
		while (ml != NULL)
		{
			struct memlist *ml_swap;
			
			cur_head = ml->addr;
			cur_tail = ml->addr + ml->len;
			
			if (tail < cur_head)
			{
				break;
			}
			
			// merge this ml
			ml_tmp->len = MEMLIST_AdjustLen(max(tail, cur_tail) - head,
												page_size);
			// save next ml in the list
			ml_swap = MEMLIST_GetNext(ml);
			// fix the list
			if (NULL == ml_swap)
			{
				sllist_init_node(ml_tmp->list);
			}
			else
			{
				sllist_insert(ml_tmp->list, ml_swap->list);
			}
			// free this ml
			free(ml);
			// loop from next ml
			ml = ml_swap;
		}
		
		return VSFERR_NONE;
	}
	
	return VSFERR_FAIL;
}

static void MEMLIST_Insert(struct memlist **ml, struct memlist *newitem)
{
	struct memlist *ml_tmp, ml_virtual_head;
	
	if ((NULL == ml) || (NULL == *ml) || (NULL == newitem))
	{
		return;
	}
	
	sllist_insert(ml_virtual_head.list, (*ml)->list);
	ml_tmp = &ml_virtual_head;
	while ((MEMLIST_GetNext(ml_tmp) != NULL)
			&& (newitem->addr > MEMLIST_GetNext(ml_tmp)->addr))
	{
		ml_tmp = MEMLIST_GetNext(ml_tmp);
	}
	
	if (MEMLIST_GetNext(ml_tmp) != NULL)
	{
		sllist_insert(newitem->list, MEMLIST_GetNext(ml_tmp)->list);
		sllist_insert(ml_tmp->list, newitem->list);
		if (ml_tmp == &ml_virtual_head)
		{
			*ml = newitem;
		}
	}
	else
	{
		sllist_insert(ml_tmp->list, newitem->list);
	}
}

uint32_t MEMLIST_CalcAllSize(struct memlist *ml)
{
	uint32_t size = 0;
	
	while (ml != NULL)
	{
		size += ml->len;
		ml = MEMLIST_GetNext(ml);
	}
	
	return size;
}

vsf_err_t MEMLIST_Add(struct memlist **ml, uint32_t addr, uint32_t len,
					uint32_t page_size, uint8_t *buff)
{
	struct memlist *newitem = NULL;
	
	if (NULL == ml)
	{
		return VSFERR_FAIL;
	}
	
	if (NULL == *ml)
	{
		*ml = (struct memlist*)malloc(sizeof(struct memlist));
		if (NULL == *ml)
		{
			LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
			return VSFERR_FAIL;
		}
		
		(*ml)->addr = MEMLIST_AdjustAddr(addr, page_size);
		(*ml)->len = MEMLIST_AdjustLen(len, page_size);
		(*ml)->buff = buff;
		sllist_init_node((*ml)->list);
	}
	else
	{
		if (MEMLIST_Merge(*ml, addr, len, page_size))
		{
			newitem = (struct memlist*)malloc(sizeof(struct memlist));
			if (NULL == newitem)
			{
				LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
				return VSFERR_FAIL;
			}
			
			newitem->addr = MEMLIST_AdjustAddr(addr, page_size);
			newitem->len = MEMLIST_AdjustLen(len, page_size);
			newitem->buff = buff;
			sllist_init_node(newitem->list);
			MEMLIST_Insert(ml, newitem);
		}
	}
	
	return VSFERR_NONE;
}

void MEMLIST_Free(struct memlist **ml)
{
	struct memlist *tmp1, *tmp2;
	
	if (NULL == ml)
	{
		return;
	}
	
	tmp1 = *ml;
	while (tmp1 != NULL)
	{
		tmp2 = tmp1;
		tmp1 = MEMLIST_GetNext(tmp1);
		sllist_init_node(tmp2->list);
		free(tmp2);
	}
	tmp1 = tmp2 = NULL;
	*ml = NULL;
}

