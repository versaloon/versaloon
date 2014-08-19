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
// vsfsm_get_event_pending should be called with interrupt disabled
uint32_t vsfsm_get_event_pending(void)
{
	return vsfsm_event_pending;
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

#if VSFSM_CFG_HSM_EN
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
#endif

static vsf_err_t vsfsm_dispatch_evt(struct vsfsm_t *sm, vsfsm_evt_t evt)
{
#if VSFSM_CFG_HSM_EN
	struct vsfsm_state_t *temp_state = NULL, *lca_state;
	struct vsfsm_state_t *temp_processor_state, *temp_target_state;
#endif
	struct vsfsm_state_t *processor_state = sm->cur_state;
	struct vsfsm_state_t *target_state = processor_state->evt_handler(sm, evt);
	
	// local event can not transmit or be passed to superstate
	if (evt >= VSFSM_EVT_LOCAL)
	{
		return VSFERR_NONE;
	}
	
#if VSFSM_CFG_HSM_EN
	// superstate
	while (target_state == (struct vsfsm_state_t *)-1)
	{
		processor_state = sm->cur_state->super;
		if (processor_state != NULL)
		{
			target_state = processor_state->evt_handler(sm, evt);
		}
	}
#endif
	
	if ((NULL == target_state)
#if !VSFSM_CFG_HSM_EN
		|| ((struct vsfsm_state_t *)-1 == target_state)
#endif
		)
	{
		// handled, or even topstate can not handle this event
		return VSFERR_NONE;
	}
	
	// need to transmit
#if VSFSM_CFG_HSM_EN
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
#else
	sm->cur_state->evt_handler(sm, VSFSM_EVT_EXIT);
	sm->cur_state = target_state;
	sm->cur_state->evt_handler(sm, VSFSM_EVT_ENTER);
#endif
	return vsfsm_post_evt(sm, VSFSM_EVT_INIT);
}

#if VSFSM_CFG_HSM_EN
static struct vsfsm_state_t *
vsfsm_top_handler(struct vsfsm_t *sm, vsfsm_evt_t evt)
{
	REFERENCE_PARAMETER(sm);
	REFERENCE_PARAMETER(evt);
	return NULL;
}
struct vsfsm_state_t vsfsm_top = {vsfsm_top_handler};
#endif

static bool vsfsm_subsm_exists(struct vsfsm_state_t *state, struct vsfsm_t *sm)
{
	struct vsfsm_t *sm_temp = state->subsm;
	while (sm_temp != NULL)
	{
		if (sm_temp == sm)
		{
			return true;
		}
		sm_temp = sm_temp->next;
	}
	return false;
}

vsf_err_t vsfsm_add_subsm(struct vsfsm_state_t *state, struct vsfsm_t *sm)
{
	if (!vsfsm_subsm_exists(state, sm))
	{
		if (NULL == state->subsm)
		{
			sm->next = NULL;
		}
		else
		{
			sm->next = state->subsm;
		}
		state->subsm = sm;
	}
	return VSFERR_NONE;
}

vsf_err_t vsfsm_remove_subsm(struct vsfsm_state_t *state, struct vsfsm_t *sm)
{
	struct vsfsm_t *sm_temp = state->subsm;
	if (sm_temp == sm)
	{
		state->subsm = sm->next;
	}
	else
	{
		while (sm_temp != NULL)
		{
			if (sm_temp->next == sm)
			{
				sm_temp->next = sm->next;
				break;
			}
			sm_temp = sm_temp->next;
		}
	}
	return VSFERR_NONE;
}

vsf_err_t vsfsm_init(struct vsfsm_t *sm)
{
#if VSFSM_CFG_SYNC_EN
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
	struct vsfsm_t *sm_temp, *sm_next;
	
	if (sm->evtq.count)
	{
		evt = vsfsm_evtq_get(&sm->evtq);
		return vsfsm_dispatch_evt(sm, evt);
	}
	// poll subsm in cur_state
	// save sm_next incase sm_temp remove itself from the list
	sm_temp = sm->cur_state->subsm;
	sm_next = (NULL == sm_temp) ? NULL : sm_temp->next;
	while (sm_temp != NULL)
	{
		err_temp = vsfsm_poll(sm_temp);
		if (!err)
		{
			err = err_temp;
		}
		sm_temp = sm_next;
		sm_next = (NULL == sm_next) ? NULL : sm_next->next;
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
			((evt >= VSFSM_EVT_INSTANT) &&
				(evt <= VSFSM_EVT_INSTANT_END)) ||
			((evt >= VSFSM_EVT_LOCAL_INSTANT) &&
				(evt <= VSFSM_EVT_LOCAL_INSTANT_END)) ||
			(0 == sm->evtq.count) ?
				vsfsm_dispatch_evt(sm, evt) : vsfsm_evtq_post(&sm->evtq, evt);
}

// pending event will be forced to be sent to event queue
vsf_err_t vsfsm_post_evt_pending(struct vsfsm_t *sm, vsfsm_evt_t evt)
{
	return (sm->active) ? vsfsm_evtq_post(&sm->evtq, evt) : VSFERR_FAIL;
}

#if VSFSM_CFG_SYNC_EN
// vsfsm_sync_t
vsf_err_t vsfsm_sync_init(struct vsfsm_sync_t *sync, uint32_t cur_value,
				uint32_t max_value, vsfsm_evt_t evt, bool valid_on_increase)
{
	sync->cur_value = cur_value;
	sync->max_value = max_value;
	sync->valid_on_increase = valid_on_increase;
	sync->sm_pending = NULL;
	return VSFERR_NONE;
}

static void vsfsm_sync_append_sm(struct vsfsm_t *sm, struct vsfsm_sync_t *sync)
{
	struct vsfsm_t sm_temp, *sm_pending;
	sm_temp.next = sync->sm_pending;
	sm_pending = &sm_temp;
	
	while (sm_pending->next != NULL)
	{
		sm_pending->next = sm_pending->pending_next;
	}
	sm->pending_next = NULL;
	sm_pending->pending_next = sm;
}

vsf_err_t vsfsm_sync_cancel(struct vsfsm_t *sm, struct vsfsm_sync_t *sync)
{
	struct vsfsm_t sm_temp, *sm_pending;
	sm_temp.next = sync->sm_pending;
	sm_pending = &sm_temp;
	
	while (sm_pending != NULL)
	{
		if (sm_pending->next == sm)
		{
			sm_pending->next = sm->pending_next;
			break;
		}
		sm_pending = sm_pending->pending_next;
	}
	return VSFERR_NONE;
}

vsf_err_t vsfsm_sync_increase(struct vsfsm_t *sm, struct vsfsm_sync_t *sync)
{
	if (sync->cur_value >= sync->max_value)
	{
		return VSFERR_BUG;
	}
	
	if (sync->valid_on_increase)
	{
		if (sync->sm_pending)
		{
			if (vsfsm_post_evt(sync->sm_pending, sync->evt))
			{
				// should increase the evtq buffer size
				return VSFERR_BUG;
			}
			sync->sm_pending = sync->sm_pending->pending_next;
		}
		else
		{
			sync->cur_value++;
		}
		return VSFERR_NONE;
	}
	else
	{
		if (sync->cur_value < sync->max_value)
		{
			sync->cur_value++;
			return VSFERR_NONE;
		}
		vsfsm_sync_append_sm(sm, sync);
		return VSFERR_NOT_READY;
	}
}

vsf_err_t vsfsm_sync_decrease(struct vsfsm_t *sm, struct vsfsm_sync_t *sync)
{
	if (!sync->cur_value)
	{
		return VSFERR_BUG;
	}
	
	if (!sync->valid_on_increase)
	{
		if (sync->sm_pending)
		{
			if (vsfsm_post_evt(sync->sm_pending, sync->evt))
			{
				// should increase the evtq buffer size
				return VSFERR_BUG;
			}
			sync->sm_pending = sync->sm_pending->pending_next;
		}
		else
		{
			sync->cur_value--;
		}
		return VSFERR_NONE;
	}
	else
	{
		if (sync->cur_value < sync->max_value)
		{
			sync->cur_value--;
			return VSFERR_NONE;
		}
		vsfsm_sync_append_sm(sm, sync);
		return VSFERR_NOT_READY;
	}
}
#endif	// VSFSM_CFG_SYNC_EN
