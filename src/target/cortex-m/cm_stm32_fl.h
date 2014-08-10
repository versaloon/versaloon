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

#ifndef __CM3_STM32_FL_H_INCLUDED__
#define __CM3_STM32_FL_H_INCLUDED__

#define STM32_FL_BUFFER_OFFSET		1024

struct stm32_fl_t
{
	uint32_t base;
	uint32_t cnt;
};

struct stm32_fl_cmd_t
{
	uint32_t cr_addr;
	uint32_t cr_value1;
	uint32_t cr_value2;
	
	uint32_t sr_addr;
	uint32_t sr_busy_mask;
	uint32_t sr_err_mask;
	
	uint32_t target_addr;
	uint32_t ram_addr;
	
	uint8_t data_type;
	uint16_t data_round;
	uint16_t data_unit_round;
};

struct stm32_fl_result_t
{
	uint32_t result;
};

vsf_err_t stm32swj_fl_init(struct stm32_fl_t *fl);
vsf_err_t stm32swj_fl_run(struct stm32_fl_t *fl, struct stm32_fl_cmd_t *cmd);
vsf_err_t stm32swj_fl_poll_result(struct stm32_fl_t *fl,
									struct stm32_fl_result_t *result);
vsf_err_t stm32swj_fl_wait_ready(struct stm32_fl_t *fl,
								struct stm32_fl_result_t *result, bool last);
vsf_err_t stm32swj_fl_call(struct stm32_fl_t *fl, struct stm32_fl_cmd_t *cmd,
							struct stm32_fl_result_t *result, bool last);

#endif	// __CM3_STM32_FL_H_INCLUDED__

