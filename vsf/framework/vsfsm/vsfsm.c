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

#include "compiler.h"
#include "vsfsm.h"

// internal event queue processors
static vsf_err_t vsfsm_evtq_init(struct vsfsm_evtqueue_t *evtq)
{
	evtq->head = evtq->tail = evtq->count = 0;
	return VSFERR_NONE;
}

static vsfsm_evt_t vsfsm_evtq_get(struct vsfsm_evtqueue_t *evtq)
{
	vsfsm_evt_t evt;
	
	if (0 == evtq->count)
	{
		return VSFSM_EVT_INVALID;
	}
	evt = evtq->evt_buffer[evtq->tail];
	
	vsf_enter_critical();
	
	if (++evtq->tail >= evtq->evt_buffer_num)
	{
		evtq->tail = 0;
	}
	--evtq->count;
	
	vsf_leave_critical();
	
	return evt;
}

static vsf_err_t vsfsm_evtq_post(struct vsfsm_evtqueue_t *evtq,
									vsfsm_evt_t evt)
{
	vsf_enter_critical();
	
	if (evtq->count >= evtq->evt_buffer_num)
	{
		vsf_leave_critical();
		return VSFERR_NOT_ENOUGH_RESOURCES;
	}
	
	evtq->evt_buffer[evtq->head] = evt;
	if (++evtq->head >= evtq->evt_buffer_num)
	{
		evtq->head = 0;
	}
	++evtq->count;
	
	vsf_leave_critical();
	
	return VSFERR_NONE;
}

static vsf_err_t vsfsm_dispatch_evt(struct vsfsm_t *sm, vsfsm_evt_t evt)
{
	struct vsfsm_state_t *next = sm->cur_state->evt_handler(sm, evt);
	if (NULL == next)
	{
		return VSFERR_NONE;
	}
	if (evt >= VSFSM_EVT_USER_LOCAL)
	{
		// local event, can not transmit
		return VSFERR_BUG;
	}
	
	// need to transmit
	
}

vsf_err_t vsfsm_init(struct vsfsm_t *sm)
{
	vsfsm_evtq_init(&sm->evtq);
	sm->cur_state = &sm->init_state;
	vsfsm_post_evt(sm, VSFSM_EVT_INIT);
	vsfsm_post_evt(sm, VSFSM_EVT_ENTER);
	return VSFERR_NONE;
}

vsf_err_t vsfsm_fini(struct vsfsm_t *sm)
{
	return VSFERR_NONE;
}

vsf_err_t vsfsm_poll(struct vsfsm_t *sm)
{
	if (sm->evtq.count)
	{
		vsfsm_evt_t evt = vsfsm_evtq_get(&sm->evtq);
		return vsfsm_dispatch_evt(sm, evt);
	}
	return VSFERR_NONE;
}

vsf_err_t vsfsm_post_evt(struct vsfsm_t *sm, vsfsm_evt_t evt)
{
	if (0 == sm->evtq.count)
	{
		// if interrupt occur here and post event to the same statemachine,
		// current event will be dispatched first
		return vsfsm_dispatch_evt(sm, evt);
	}
	else
	{
		return vsfsm_evtq_post(&sm->evtq, evt);
	}
}

vsf_err_t vsfsm_post_evt_int(struct vsfsm_t *sm, vsfsm_evt_t evt)
{
	return vsfsm_evtq_post(&sm->evtq, evt);
}
