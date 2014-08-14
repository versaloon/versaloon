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

#include "vsftimer.h"
#include "interfaces.h"
#include "framework/vsfsm/vsfsm.h"

#define VSFSM_EVT_TIMER					8

static struct vsfsm_state_t *
vsftimer_evt_handler(struct vsfsm_t *sm, vsfsm_evt_t evt);

struct vsftimer_t
{
	struct vsftimer_timer_t *timerlist;
	struct vsfsm_t sm;
	
	// private
	vsfsm_evt_t evt_buffer[1];
} static vsftimer =
{
	NULL,
	{
		NULL,							// struct vsfsm_t *super;
		{
			vsftimer.evt_buffer,		// vsfsm_evt_t *evt_buffer;
			dimof(vsftimer.evt_buffer),	// uint16_t evt_buffer_num;
		},								// struct vsfsm_evtqueue_t evt_queue;
		{
			vsftimer_evt_handler
		},								// struct vsfsm_state_t init_state;
	},
};

// vsftimer_callback_int is called in interrupt,
// simply send event to vsftimer SM
static void vsftimer_callback_int(void *param)
{
	struct vsftimer_t *vsftimer = param;
	
	vsfsm_post_evt_int(&vsftimer->sm, VSFSM_EVT_TIMER);
}

static struct vsfsm_state_t *
vsftimer_evt_handler(struct vsfsm_t *sm, vsfsm_evt_t evt)
{
	switch (evt)
	{
	case VSFSM_EVT_INIT:
		vsftimer.timerlist = NULL;
		interfaces->tickclk.set_callback(vsftimer_callback_int, &vsftimer);
		return NULL;
	case VSFSM_EVT_TIMER:
	{
		uint32_t cur_tickcnt = interfaces->tickclk.get_count();
		struct vsftimer_timer_t timer, *ptimer = vsftimer.timerlist;
		
		while (ptimer != NULL)
		{
			// save timer on stack in case event handler of the state machine
			// unregister and free the timer, beause timer on stack maintain
			// the list pointing to the next timer
			timer = *ptimer;
			if (((cur_tickcnt - timer.start_tickcnt) >= timer.interval) &&
				(timer.sm != NULL) && (timer.evt != VSFSM_EVT_INVALID))
			{
				vsfsm_post_evt(timer.sm, timer.evt);
			}
			ptimer = sllist_get_container(timer.list.next,
											struct vsftimer_timer_t, list);
		}
		return NULL;
	}
	case VSFSM_EVT_ENTER:
	default:
		return NULL;
	}
}

vsf_err_t vsftimer_init(void)
{
	return vsfsm_init(&vsftimer.sm);
}

vsf_err_t vsftimer_poll(void)
{
	return vsfsm_poll(&vsftimer.sm);
}

vsf_err_t vsftimer_register(struct vsftimer_timer_t *timer)
{
	timer->start_tickcnt = interfaces->tickclk.get_count();
	if (NULL == vsftimer.timerlist)
	{
		vsftimer.timerlist = timer;
		return VSFERR_NONE;
	}
	if (!sllist_is_in(&vsftimer.timerlist->list, &timer->list))
	{
		if (vsftimer.timerlist != NULL)
		{
			sllist_insert(timer->list, vsftimer.timerlist->list);
		}
		vsftimer.timerlist = timer;
	}
	return VSFERR_NONE;
}

vsf_err_t vsftimer_unregister(struct vsftimer_timer_t *timer)
{
	if (vsftimer.timerlist == timer)
	{
		vsftimer.timerlist = sllist_get_container(timer->list.next,
								struct vsftimer_timer_t, list);
		return VSFERR_NONE;
	}
	return sllist_remove(&vsftimer.timerlist->list.next, &timer->list);
}
