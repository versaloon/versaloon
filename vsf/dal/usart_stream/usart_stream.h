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

#ifndef __USART_STREAM_H_INCLUDED__
#define __USART_STREAM_H_INCLUDED__

#include "dal/stream/stream.h"

struct usart_info_t
{
	uint32_t baudrate;
	uint8_t datalength;
	uint8_t mode;
};

struct usart_stream_info_t
{
	uint8_t usart_index;
	uint32_t int_priority;
	struct vsf_stream_t stream_tx;
	struct vsf_stream_t stream_rx;
	struct
	{
		void *param;
		void (*ontx_empty_int)(void *param);
		void (*ontx_int)(void *param);
		void (*onrx_int)(void *param);
	} callback;
	struct usart_info_t usart_info;
	
	// private
	bool txing;
};

vsf_err_t usart_stream_init(struct usart_stream_info_t *usart_stream);
vsf_err_t usart_stream_fini(struct usart_stream_info_t *usart_stream);
vsf_err_t usart_stream_config(struct usart_stream_info_t *usart_stream);
uint32_t usart_stream_rx(struct usart_stream_info_t *usart_stream, 
							struct vsf_buffer_t *buffer);
uint32_t usart_stream_tx(struct usart_stream_info_t *usart_stream, 
							struct vsf_buffer_t *buffer);

#endif	// __USART_STREAM_H_INCLUDED__
