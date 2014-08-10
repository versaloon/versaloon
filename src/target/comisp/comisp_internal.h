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
#ifndef __COMISP_INTERNAL_H_INCLUDED__
#define __COMISP_INTERNAL_H_INCLUDED__

#define COMM_PARAMETER_UNSURE			'*'

#define COMM_AUXPIN_DISABLE				'N'
#define COMM_AUXPIN_ENABLE				'A'

#define COMM_DEFAULT_BAUDRATE			115200
#define COMM_DEFAULT_DATALENGTH			8
#define COMM_DEFAULT_PARITYBIT			COMM_PARITYBIT_NONE
#define COMM_DEFAULT_STOPBIT			COMM_STOPBIT_1
#define COMM_DEFAULT_HANDSHAKE			COMM_HANDSHAKE_NONE
#define COMM_DEFAULT_AUXPIN				COMM_AUXPIN_DISABLE

struct com_mode_t
{
	char comport[21];
	int32_t baudrate;
	int8_t datalength;
	char paritybit;
	char stopbit;
	char handshake;
	char auxpin;
};

struct comisp_param_t
{
	const char *chip_name;
	struct com_mode_t com_mode;
	struct program_functions_t *program_functions;
};

extern struct com_mode_t com_mode;
extern uint8_t comisp_execute_flag;
extern uint32_t comisp_execute_addr;

#endif /* __COMISP_INTERNAL_H_INCLUDED__ */

