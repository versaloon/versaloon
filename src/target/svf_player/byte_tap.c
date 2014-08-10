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

#include "compiler.h"

#include "app_cfg.h"
#if TARGET_SVF_EN

#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "interfaces.h"

#include "byte_tap.h"

const char *tap_state_name[TAP_NUM_OF_STATE] =
{
	"RESET",
	"IDLE",
	"DRSHIFT",
	"DRPAUSE",
	"IRSHIFT",
	"IRPAUSE",
	"DRSELECT",
	"DRCAPTURE",
	"DREXIT1",
	"DREXIT2",
	"DRUPDATE",
	"IRSELECT",
	"IRCAPTURE",
	"IREXTI1",
	"IREXIT2",
	"IRUPDATE",
};

struct tap_path_info_t
{
	enum tap_state_t next_state_by0;
	enum tap_state_t next_state_by1;
};

static const struct tap_path_info_t tap_path[TAP_NUM_OF_STATE] =
{
	{IDLE, RESET},			// RESET
	{IDLE, DRSELECT},		// IDLE
	{DRSHIFT, DREXIT1},		// DRSHIFT
	{DRPAUSE, DREXIT2},		// DRAPUSE
	{IRSHIFT, IREXIT1},		// IRSHIFT
	{IRPAUSE, IREXIT2},		// IRPAUSE
	{DRCAPTURE, IRSELECT},	// DRSELECT
	{DRSHIFT, DREXIT1},		// DRCAPTURE
	{DRPAUSE, DRUPDATE},	// DREXIT1
	{DRSHIFT, DRUPDATE},	// DREXIT2
	{IDLE, DRSELECT},		// DRUPDATE
	{IRCAPTURE, RESET},		// IRSELECT
	{IRSHIFT, IREXIT1},		// IRCAPTURE
	{IRPAUSE, IRUPDATE},	// IREXIT1
	{IRSHIFT, IRUPDATE},	// IREXIT2
	{IDLE, DRSELECT}		// IRUPDATE
};

struct tap_move_info_t
{
	uint8_t tms;
	uint8_t insert_pos;
	uint8_t insert_value;
};

// tap_move[from_state][to_state]
static const struct tap_move_info_t tap_move[6][6] =
{
//	RESET,		IDLE,		DRSHIFT,	DRPAUSE,	IRSHIFT,	IRPAUSE
{{0xff,0,1},{0x7f,0,1},	{0x2f,0,1},	{0x0a,0,1},	{0x37,0,1},	{0x16,0,1}},// RESET
{{0xff,0,1},{0x00,0,0},	{0x45,4,0},	{0x05,4,0},	{0x4b,4,0},	{0x0b,4,0}},// IDLE
{{0x00,0,0},{0x00,0,0},	{0x00,0,0},	{0x00,0,0},	{0x00,0,0},	{0x00,0,0}},// DRSHIFT
{{0xff,0,1},{0x60,0,0},	{0x40,0,0},	{0x5c,0,0},	{0x3c,0,0},	{0x5e,0,0}},// DRPAUSE
{{0x00,0,0},{0x00,0,0},	{0x00,0,0},	{0x00,0,0},	{0x00,0,0},	{0x00,0,0}},// IRSHIFT
{{0xff,0,1},{0x60,0,0},	{0x38,0,0},	{0x5c,0,0},	{0x40,0,0},	{0x5e,0,0}}// IRPAUSE
};

static enum tap_state_t end_state = IDLE;
static enum tap_state_t cur_state = IDLE;
static uint8_t tap_tms_remain_cycles = 0;
static uint8_t tap_tms_remain;

static struct INTERFACES_INFO_T *prog = NULL;

vsf_err_t jtag_init(void)
{
	return prog->jtag_ll.init(0);
}
vsf_err_t jtag_fini(void)
{
	return prog->jtag_ll.fini(0);
}
vsf_err_t jtag_config(uint16_t kHz)
{
	return prog->jtag_ll.config(0, kHz);
}
vsf_err_t jtag_tms(uint8_t *tms, uint8_t bytelen)
{
	return prog->jtag_ll.tms(0, tms, bytelen);
}
vsf_err_t jtag_tms_clocks(uint32_t bytelen, uint8_t tms)
{
	return prog->jtag_ll.tms_clocks(0, bytelen, tms);
}
vsf_err_t jtag_xr(uint8_t *data, uint16_t bitlen, uint8_t tms_before_valid,
				uint8_t tms_before, uint8_t tms_after0, uint8_t tms_after1)
{
	return prog->jtag_ll.scan(0, data, bitlen, tms_before_valid,
									tms_before, tms_after0, tms_after1);
}
vsf_err_t jtag_commit(void)
{
	return prog->peripheral_commit();
}

vsf_err_t jtag_trst_init(void)
{
	return prog->gpio.init(0);
}
vsf_err_t jtag_trst_fini(void)
{
	return prog->gpio.fini(0);
}
vsf_err_t jtag_trst_output(uint8_t value)
{
	return prog->gpio.config(0, JTAG_TRST, JTAG_TRST,
									0, value ? JTAG_TRST : 0);
}
vsf_err_t jtag_trst_input(void)
{
	return prog->gpio.config(0, JTAG_TRST, 0, JTAG_TRST, JTAG_TRST);
}
vsf_err_t jtag_trst_1(void)
{
	return prog->gpio.out(0, JTAG_TRST, JTAG_TRST);
}
vsf_err_t jtag_trst_0(void)
{
	return prog->gpio.out(0, JTAG_TRST, 0);
}





bool tap_state_is_stable(enum tap_state_t state)
{
	return ((RESET == state) || (IDLE == state)
			|| (DRPAUSE == state) || (IRPAUSE == state));
}

bool tap_state_is_valid(enum tap_state_t state)
{
	return (state < TAP_NUM_OF_STATE);
}

vsf_err_t tap_end_state(enum tap_state_t state)
{
	if (tap_state_is_valid(state))
	{
		if (state < 6)
		{
			end_state = state;
			return VSFERR_NONE;
		}
		else
		{
			LOG_BUG("can not shift to %s", tap_state_name[state]);
			return VSFERR_FAIL;
		}
	}
	else
	{
		LOG_BUG("%d is not a valid state", state);
		return VSFERR_FAIL;
	}
}

vsf_err_t tap_state_move(void)
{
	const struct tap_move_info_t *tm;
	uint16_t tms_16bit;

	if ((cur_state == DRSHIFT) || (cur_state == IRSHIFT))
	{
		LOG_BUG("move from %s is invalid", tap_state_name[cur_state]);
		return VSFERR_FAIL;
	}
	
	if (tap_tms_remain_cycles > 0)
	{
		tm = &tap_move[cur_state][end_state];
		tms_16bit = ((1 << tap_tms_remain_cycles) - 1) & tap_tms_remain;
		tms_16bit |= (tm->tms & ((1 << tm->insert_pos) - 1))
						<< tap_tms_remain_cycles;
		if (tm->insert_value)
		{
			tms_16bit |= ((1 << (8 - tap_tms_remain_cycles)) - 1)
							<< (tap_tms_remain_cycles + tm->insert_pos);
		}
		tms_16bit |= (tm->tms >> tm->insert_pos) << (8 + tm->insert_pos);
		
		cur_state = end_state;
		tap_tms_remain_cycles = 0;
		return jtag_tms((uint8_t*)&tms_16bit, 2);
	}
	else
	{
		if (jtag_tms((uint8_t*)&tap_move[cur_state][end_state].tms, 1))
		{
			return VSFERR_FAIL;
		}
		cur_state = end_state;
	}
	
	return VSFERR_NONE;
}

vsf_err_t tap_path_move(uint32_t num_states, enum tap_state_t *path)
{
	uint8_t tms;
	uint32_t i;
	uint8_t remain_cycles;
	
	tms = tap_tms_remain_cycles > 0 ?
			tap_tms_remain & ((1 << tap_tms_remain_cycles) - 1) : 0;
	remain_cycles = (tap_tms_remain_cycles + num_states) % 8;
	
	for (i = 0; i < num_states; i++)
	{
		if (!tap_state_is_valid(path[i]))
		{
			LOG_BUG("%d is not a valid state", path[i]);
			return VSFERR_FAIL;
		}
		
		if (path[i] == tap_path[cur_state].next_state_by0)
		{
			tms &= ~(1 << ((i + tap_tms_remain_cycles) % 8));
		}
		else if (path[i] == tap_path[cur_state].next_state_by1)
		{
			tms |= 1 << ((i + tap_tms_remain_cycles) % 8);
		}
		else
		{
			LOG_ERROR("can not shift to %s from %s",
						tap_state_name[cur_state], tap_state_name[path[i]]);
			return VSFERR_FAIL;
		}
		cur_state = path[i];
		
		if (((i + tap_tms_remain_cycles) % 8) == 7)
		{
			if (jtag_tms(&tms, 1))
			{
				return VSFERR_FAIL;
			}
			tap_tms_remain_cycles = 0;
			tms = 0;
		}
	}
	
	tap_tms_remain_cycles = remain_cycles;
	tap_tms_remain = tms;
	
	return VSFERR_NONE;
}

vsf_err_t tap_runtest(enum tap_state_t run_state, enum tap_state_t end_state,
				   uint32_t num_cycles)
{
	uint8_t tms;
	
	if ((IDLE == run_state) || (DRPAUSE == run_state)
		|| (IRPAUSE == run_state))
	{
		tms = 0;
	}
	else if (RESET == run_state)
	{
		tms = 1;
	}
	else
	{
		if (tap_state_is_valid(run_state))
		{
			LOG_ERROR("unstable run_state: %s", tap_state_name[run_state]);
		}
		else
		{
			LOG_ERROR("invalid run_state: %d", run_state);
		}
		
		return VSFERR_FAIL;
	}
	if (!tap_state_is_stable(end_state))
	{
		if (tap_state_is_valid(end_state))
		{
			LOG_ERROR("unstable end_state: %s", tap_state_name[end_state]);
		}
		else
		{
			LOG_ERROR("invalid end_state: %d", end_state);
		}
		
		return VSFERR_FAIL;
	}
	
	if (cur_state != run_state)
	{
		if (tap_end_state(run_state) || tap_state_move())
		{
			return VSFERR_FAIL;
		}
	}
	else if (tap_tms_remain_cycles)
	{
		uint8_t tms_tmp = tap_tms_remain & ((1 << tap_tms_remain_cycles) - 1);
		
		if (tms)
		{
			uint8_t bits1;
			
			if ((tap_tms_remain_cycles + num_cycles) >= 8)
			{
				bits1 = 8 - tap_tms_remain_cycles;
			}
			else
			{
				bits1 = (uint8_t)num_cycles;
			}
			
			tms_tmp |= ((1 << bits1) - 1) << tap_tms_remain_cycles;
		}
		
		if ((tap_tms_remain_cycles + num_cycles) >= 8)
		{
			num_cycles -= 8 - tap_tms_remain_cycles;
			tap_tms_remain_cycles = 0;
			if (jtag_tms(&tms_tmp, 1))
			{
				return VSFERR_FAIL;
			}
		}
		else
		{
			tap_tms_remain_cycles =
						(uint8_t)(tap_tms_remain_cycles + num_cycles);
			tap_tms_remain = tms_tmp;
			num_cycles = 0;
		}
	}
	
	if (num_cycles)
	{
		if (jtag_tms_clocks(num_cycles >> 3, tms))
		{
			return VSFERR_FAIL;
		}
		if (num_cycles > 0xFFFF)
		{
			// it will take to much time, commit here
			if (tap_commit())
			{
				return VSFERR_FAIL;
			}
		}
		num_cycles &= 7;
		
		tap_tms_remain_cycles = (uint8_t)num_cycles;
		if (tms)
		{
			tap_tms_remain = (1 << tap_tms_remain_cycles) - 1;
		}
		else
		{
			tap_tms_remain = 0x00;
		}
	}
	
	if (end_state != run_state)
	{
		// runtest in IDLE, but end_state is not IDLE
		if (tap_end_state(end_state) || tap_state_move())
		{
			return VSFERR_FAIL;
		}
	}
	cur_state = end_state;
	
	return VSFERR_NONE;
}

vsf_err_t tap_scan_ir(uint8_t *buffer, uint32_t bit_size)
{
	enum tap_state_t last_end_state = end_state;
	
	if (tap_end_state(IRSHIFT) || tap_state_move() ||
		tap_end_state(last_end_state))
	{
		return VSFERR_FAIL;
	}
	
	if (end_state != IRPAUSE)
	{
		if (jtag_xr(buffer, (uint16_t)bit_size, 0, 0,
				1 << ((bit_size - 1) % 8), tap_move[IRPAUSE][end_state].tms))
		{
			return VSFERR_FAIL;
		}
	}
	else
	{
		if (jtag_xr(buffer, (uint16_t)bit_size, 0, 0,
						1 << ((bit_size - 1) % 8), 0))
		{
			return VSFERR_FAIL;
		}
	}
	cur_state = end_state;
	
	return VSFERR_NONE;
}

vsf_err_t tap_scan_dr(uint8_t *buffer, uint32_t bit_size)
{
	enum tap_state_t last_end_state = end_state;
	
	if (tap_end_state(DRSHIFT) || tap_state_move() ||
		tap_end_state(last_end_state))
	{
		return VSFERR_FAIL;
	}
	
	if (end_state != DRPAUSE)
	{
		if (jtag_xr(buffer, (uint16_t)bit_size, 0, 0,
				1 << ((bit_size - 1) % 8), tap_move[DRPAUSE][end_state].tms))
		{
			return VSFERR_FAIL;
		}
	}
	else
	{
		if (jtag_xr(buffer, (uint16_t)bit_size, 0, 0,
						1 << ((bit_size - 1) % 8), 0))
		{
			return VSFERR_FAIL;
		}
	}
	cur_state = end_state;
	
	return VSFERR_NONE;
}

vsf_err_t tap_commit(void)
{
	if (tap_tms_remain_cycles)
	{
		// append last tms
		tap_tms_remain &= (1 << tap_tms_remain_cycles) - 1;
		if (tap_tms_remain & (1 << (tap_tms_remain_cycles - 1)))
		{
			tap_tms_remain |= ((1 << (8 - tap_tms_remain_cycles)) - 1)
								<< tap_tms_remain_cycles;
		}
		tap_tms_remain_cycles = 0;
		if (jtag_tms(&tap_tms_remain, 1))
		{
			return VSFERR_FAIL;
		}
	}
	if (jtag_commit())
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

vsf_err_t tap_init(struct INTERFACES_INFO_T *ifs)
{
	prog = ifs;
	
	if (jtag_init() || jtag_config(1000) || jtag_trst_init() ||
		tap_end_state(RESET) || tap_state_move() || jtag_commit())
	{
		return VSFERR_FAIL;
	}
	
	return VSFERR_NONE;
}

vsf_err_t tap_fini(void)
{
	if (jtag_fini() || jtag_trst_fini())
	{
		return VSFERR_FAIL;
	}
	
	return VSFERR_NONE;
}

#endif
