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
#ifndef __CM_INTERNAL_H_INCLUDED__
#define __CM_INTERNAL_H_INCLUDED__

struct cm_param_t
{
	const char *chip_name;
	uint16_t jtag_khz;
	struct jtag_pos_t jtag_pos;
	uint8_t swd_trn;
	uint16_t swd_delay;
	const struct program_functions_t *program_functions;
};

extern const struct cm_param_t cm_chips_param[];
extern uint8_t cm_execute_flag;
extern uint32_t cm_execute_addr;

struct cm_param_t * cm_get_param(char *chip_name);

#endif /* __CM_INTERNAL_H_INCLUDED__ */

