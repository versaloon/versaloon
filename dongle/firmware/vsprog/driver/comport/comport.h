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
#ifndef __COMPORT_H_INCLUDED__
#define __COMPORT_H_INCLUDED__

#define COMM_STOPBIT_1					'1'
#define COMM_STOPBIT_1P5				'P'
#define COMM_STOPBIT_2					'2'

#define COMM_PARITYBIT_NONE				'N'
#define COMM_PARITYBIT_ODD				'O'
#define COMM_PARITYBIT_EVEN				'E'

#define COMM_HANDSHAKE_NONE				'N'
#define COMM_HANDSHAKE_SOFTWARE			'S'
#define COMM_HANDSHAKE_HARDWARE			'H'

struct comm_func_t
{
	vsf_err_t (*comm_open)(char *comport, uint32_t baudrate, uint8_t datalength,
							char paritybit, char stopbit, char handshake);
	void (*comm_close)(void);
	int32_t (*comm_read)(uint8_t *buffer, uint32_t num_of_bytes);
	int32_t (*comm_write)(uint8_t *buffer, uint32_t num_of_bytes);
	int32_t (*comm_ctrl)(uint8_t dtr, uint8_t rts);
	int32_t (*comm_flush)(void);
};

void comm_close(void);
vsf_err_t comm_open(char *comport, uint32_t baudrate, uint8_t datalength,
				 char paritybit, char stopbit, char handshake);
int32_t comm_read(uint8_t *buffer, uint32_t num_of_bytes);
int32_t comm_write(uint8_t *buffer, uint32_t num_of_bytes);
int32_t comm_ctrl(uint8_t dtr, uint8_t rts);
int32_t comm_flush(void);

#endif /* __COMPORT_H_INCLUDED__ */

