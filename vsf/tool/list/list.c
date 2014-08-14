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

#include "list.h"

int sllist_is_in(struct sllist *head, struct sllist *node)
{
	while (head != (struct sllist *)0)
	{
		if (head->next == node)
		{
			return 1;
		}
		head = head->next;
	}
	return 0;
}

int sllist_remove(struct sllist **head, struct sllist *node)
{
	if (!sllist_is_in(*head, node))
	{
		return -1;
	}
	
	if (*head == node)
	{
		head = &node->next;
		return 0;
	}
	while (*head != (struct sllist *)0)
	{
		if ((*head)->next == node)
		{
			(*head)->next = node->next;
			break;
		}
		*head = (*head)->next;
	}
	return 0;
}
