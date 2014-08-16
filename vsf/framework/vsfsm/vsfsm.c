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

static volatile uint32_t vsfsm_event_pending = 0;
uint32_t vsfsm_get_event_pending(void)
{
	uint32_t ret;
	
	vsf_enter_critical();
	ret = vsfsm_event_pending;
	vsf_leave_critical();
	
	return ret;
}

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
	vsfsm_event_pending--;
	
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
	vsfsm_event_pending++;
	
	vsf_leave_critical();
	
	return VSFERR_NONE;
}

static bool vsfsm_is_in(struct vsfsm_state_t *s, struct vsfsm_state_t *t)
{
	while (t != NULL)
	{
		if (s == t)
		{
			return true;
		}
		t = t->super;
	}
	return false;
}

static vsf_err_t vsfsm_dispatch_evt(struct vsfsm_t *sm, vsfsm_evt_t evt)
{
	struct vsfsm_state_t *temp_state = NULL, *lca_state;
	struct vsfsm_state_t *processor_state = sm->cur_state;
	struct vsfsm_state_t *target_state = processor_state->evt_handler(sm, evt);
	struct vsfsm_state_t *temp_processor_state, *temp_target_state;
	
	// local event can not transmit or be passed to superstate
	if (evt >= VSFSM_EVT_LOCAL)
	{
		return VSFERR_NONE;
	}
	
	// superstate
	while (target_state == (struct vsfsm_state_t *)-1)
	{
		processor_state = sm->cur_state->super;
		if (processor_state != NULL)
		{
			target_state = processor_state->evt_handler(sm, evt);
		}
	}
	
	if (NULL == target_state)
	{
		// handled, or even topstate can not handle this event
		return VSFERR_NONE;
	}
	
	// need to transmit
	// 1. exit to processor_state
	for (temp_state = sm->cur_state; temp_state != processor_state;)
	{
		temp_state->evt_handler(sm, VSFSM_EVT_EXIT);
		temp_state = temp_state->super;
	}
	// 2. some simple transition which happens in most cases
	if ((processor_state == target_state) ||
		(processor_state->super == target_state->super))
	{
		processor_state->evt_handler(sm, VSFSM_EVT_EXIT);
		target_state->evt_handler(sm, VSFSM_EVT_ENTER);
		goto update_cur_state;
	}
	if (processor_state->super == target_state)
	{
		processor_state->evt_handler(sm, VSFSM_EVT_EXIT);
		goto update_cur_state;
	}
	if (processor_state == target_state->super)
	{
		target_state->evt_handler(sm, VSFSM_EVT_ENTER);
		goto update_cur_state;
	}
	// 3. find the LCA
	lca_state = NULL;
	temp_processor_state = processor_state;
	temp_target_state = target_state;
	do
	{
		if (temp_processor_state != NULL)
		{
			if (vsfsm_is_in(temp_processor_state, target_state))
			{
				lca_state = temp_processor_state;
				break;
			}
			temp_processor_state = temp_processor_state->super;
		}
		if (temp_target_state != NULL)
		{
			if (vsfsm_is_in(temp_target_state, processor_state))
			{
				lca_state = temp_target_state;
				break;
			}
			temp_target_state = temp_target_state->super;
		}
		if ((NULL == temp_processor_state) && (NULL == temp_target_state))
		{
			return VSFERR_BUG;
		}
	} while (NULL == lca_state);
	// 4. exit from processor_state to lca
	for (temp_state = processor_state; temp_state != lca_state;)
	{
		temp_state->evt_handler(sm, VSFSM_EVT_EXIT);
		temp_state = temp_state->super;
	}
	// 5. enter from lca to target_state
	for (temp_state = lca_state; temp_state != target_state;)
	{
		temp_state->evt_handler(sm, VSFSM_EVT_ENTER);
		temp_state = temp_state->super;
	}
	// 6. update cur_state
update_cur_state:
	sm->cur_state = target_state;
	// 7. send VSFSM_EVT_INIT to target_state
	return vsfsm_post_evt(sm, VSFSM_EVT_INIT);
}

static struct vsfsm_state_t *
vsfsm_top_handler(struct vsfsm_t *sm, vsfsm_evt_t evt)
{
	REFERENCE_PARAMETER(sm);
	REFERENCE_PARAMETER(evt);
	return NULL;
}
struct vsfsm_state_t vsfsm_top = {NULL, vsfsm_top_handler,};

vsf_err_t vsfsm_init(struct vsfsm_t *sm)
{
#if VSFSM_CFG_SEM_EN
	sm->pending_next = NULL;
#endif
	vsfsm_evtq_init(&sm->evtq);
	sm->cur_state = &sm->init_state;
	// ignore any state transition on VSFSM_EVT_ENTER
	sm->cur_state->evt_handler(sm, VSFSM_EVT_ENTER);
	// set active so that sm can accept events
	vsfsm_set_active(sm, true);
	// process state transition on VSFSM_EVT_INIT
	return vsfsm_post_evt(sm, VSFSM_EVT_INIT);
}

vsf_err_t vsfsm_poll(struct vsfsm_t *sm)
{
	vsfsm_evt_t evt;
	vsf_err_t err = VSFERR_NONE, err_temp;
	
	if (sm->evtq.count)
	{
		evt = vsfsm_evtq_get(&sm->evtq);
		return vsfsm_dispatch_evt(sm, evt);
	}
	// poll subsm in cur_state
	if (sm->cur_state->subsm != NULL)
	{
		uint32_t i;
		for (i = 0; sm->cur_state->subsm[i] != NULL; i++)
		{
			err_temp = vsfsm_poll(sm->cur_state->subsm[i]);
			if (!err)
			{
				err = err_temp;
			}
		}
	}
	return err;
}

vsf_err_t vsfsm_set_active(struct vsfsm_t *sm, bool active)
{
	vsf_enter_critical();
	sm->active = active;
	vsf_leave_critical();
	return VSFERR_NONE;
}

vsf_err_t vsfsm_post_evt(struct vsfsm_t *sm, vsfsm_evt_t evt)
{
	return (!sm->active) ? VSFERR_FAIL :
			(evt >= VSFSM_EVT_LOCAL_INSTANT) || (0 == sm->evtq.count) ?
				vsfsm_dispatch_evt(sm, evt) : vsfsm_evtq_post(&sm->evtq, evt);
}

// pending event will be forced to be sent to event queue
vsf_err_t vsfsm_post_evt_pending(struct vsfsm_t *sm, vsfsm_evt_t evt)
{
	return (sm->active) ? vsfsm_evtq_post(&sm->evtq, evt) : VSFERR_FAIL;
}

#if VSFSM_CFG_SEM_EN
// vsfsm_sem_t
vsf_err_t vsfsm_sem_init(struct vsfsm_sem_t *sem)
{
	sem->num_accessing = 0;
	sem->sm_pending = NULL;
	return VSFERR_NONE;
}

static bool vsfsm_sem_in_pending(struct vsfsm_t *sm, struct vsfsm_sem_t *sem)
{
	struct vsfsm_t *sm_pending = sem->sm_pending;
	while (sm_pending != NULL)
	{
		if (sm_pending == sm)
		{
			return true;
		}
		sm_pending = sm_pending->pending_next;
	}
	return false;
}

static void vsfsm_sem_append_sm(struct vsfsm_t *sm, struct vsfsm_sem_t *sem)
{
	sm->pending_next = NULL;
	if (NULL == sem->sm_pending)
	{
		sem->sm_pending = sm;
	}
	else
	{
		struct vsfsm_t *sm_pending = sem->sm_pending;
		while (sm_pending->pending_next != NULL)
		{
			sm_pending = sm_pending->pending_next;
		}
		sm_pending->pending_next = sm;
	}
}

static void vsfsm_sem_remove_sm(struct vsfsm_t *sm, struct vsfsm_sem_t *sem)
{
	if (vsfsm_sem_in_pending(sm, sem))
	{
		if (sem->sm_pending == sm)
		{
			sem->sm_pending = sm->pending_next;
		}
		else
		{
			struct vsfsm_t *sm_pending = sem->sm_pending;
			while (sm_pending->pending_next != sm)
			{
				sm_pending = sm_pending->pending_next;
			}
			sm_pending->pending_next = sm->pending_next;
		}
	}
}

bool vsfsm_sem_acquire(struct vsfsm_t *sm, struct vsfsm_sem_t *sem)
{
	if (sem->num_accessing < sem->num_accessable)
	{
		sem->num_accessing++;
		return true;
	}
	
	vsfsm_sem_append_sm(sm, sem);
	return false;
}

vsf_err_t vsfsm_sem_cancel(struct vsfsm_t *sm, struct vsfsm_sem_t *sem)
{
	vsfsm_sem_remove_sm(sm, sem);
	return VSFERR_NONE;
}

vsf_err_t vsfsm_sem_release(struct vsfsm_sem_t *sem)
{
	if (!sem->num_accessing)
	{
		return VSFERR_BUG;
	}
	
	if (sem->sm_pending != NULL)
	{
		if (vsfsm_post_evt(sem->sm_pending, VSFSM_EVT_SEM))
		{
			// need to increase the evtq buffer size
			return VSFERR_BUG;
		}
		sem->sm_pending = sem->sm_pending->pending_next;
	}
	else
	{
		sem->num_accessing--;
	}
	return VSFERR_NONE;
}
#endif	// VSFSM_CFG_SEM_EN
