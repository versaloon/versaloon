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

#ifndef __VSFSM_H_INCLUDED__
#define __VSFSM_H_INCLUDED__

#include "app_type.h"

#define VSFSM_EVT_INVALID				-1
#define VSFSM_EVT_DUMMY					0
#define VSFSM_EVT_INIT					1
#define VSFSM_EVT_FINI					2
#define VSFSM_EVT_ENTER					3
#define VSFSM_EVT_EXIT					4
#define VSFSM_EVT_USER					0x10
#define VSFSM_EVT_USER_LOCAL			0x4000

typedef int vsfsm_evt_t;

struct vsfsm_evtqueue_t
{
	vsfsm_evt_t *evt_buffer;
	uint16_t evt_buffer_num;
	
	// private
	uint16_t head;
	uint16_t tail;
	uint16_t count;
};

struct vsfsm_t;
struct vsfsm_state_t
{
	struct vsfsm_state_t * (*evt_handler)(struct vsfsm_t *sm, vsfsm_evt_t evt);
};

struct vsfsm_t
{
	struct vsfsm_state_t *super;
	struct vsfsm_evtqueue_t evtq;
	struct vsfsm_state_t init_state;
	// user_data point to the user specified data for the sm
	void *user_data;
	// sm_extra is used for specific sm type
	// for MSM, sm_extra point to the transmit table
	void *sm_extra;
	
	// private
	struct vsfsm_state_t *cur_state;
};

vsf_err_t vsfsm_init(struct vsfsm_t *sm);
vsf_err_t vsfsm_fini(struct vsfsm_t *sm);
vsf_err_t vsfsm_poll(struct vsfsm_t *sm);
vsf_err_t vsfsm_post_evt(struct vsfsm_t *sm, vsfsm_evt_t evt);
vsf_err_t vsfsm_post_evt_int(struct vsfsm_t *sm, vsfsm_evt_t evt);

#endif	// #ifndef __VSFSM_H_INCLUDED__