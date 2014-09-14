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

#include "app_cfg.h"
#include "app_type.h"

#include "interfaces.h"
#include "usart_stream.h"

static void usart_stream_send_byte(struct usart_stream_info_t *usart_stream)
{
	uint8_t data;
	struct vsf_buffer_t buffer;
	
	buffer.buffer = &data;
	buffer.size = 1;
	if (stream_rx(&usart_stream->stream_tx, &buffer) == buffer.size)
	{
		usart_stream->txing = true;
		core_interfaces.usart.tx(usart_stream->usart_index, (uint16_t)data);
	}
}

static void usart_stream_ontx_int(void *p)
{
	struct usart_stream_info_t *usart_stream = (struct usart_stream_info_t *)p;
	
	if (usart_stream->callback.ontx_int != NULL)
	{
		usart_stream->callback.ontx_int(usart_stream->callback.param);
	}
	
	if (stream_get_data_size(&usart_stream->stream_tx))
	{
		usart_stream_send_byte(usart_stream);
	}
	else if (usart_stream->callback.ontx_empty_int != NULL)
	{
		usart_stream->txing = false;
		usart_stream->callback.ontx_empty_int(usart_stream->callback.param);
	}
}

static void usart_stream_onrx_int(void *p, uint16_t data)
{
	struct usart_stream_info_t *usart_stream = (struct usart_stream_info_t *)p;
	struct vsf_buffer_t buffer;
	uint8_t byte = (uint8_t)data;
	
	buffer.buffer = &byte;
	buffer.size = 1;
	stream_tx(&usart_stream->stream_rx, &buffer);
	if (usart_stream->callback.onrx_int != NULL)
	{
		usart_stream->callback.onrx_int(usart_stream->callback.param);
	}
}

vsf_err_t usart_stream_init(struct usart_stream_info_t *usart_stream)
{
	usart_stream->txing = false;
	stream_init(&usart_stream->stream_tx);
	stream_init(&usart_stream->stream_rx);
	if (usart_stream->usart_index != IFS_DUMMY_PORT)
	{
		core_interfaces.usart.init(usart_stream->usart_index);
		core_interfaces.usart.config_callback(usart_stream->usart_index,
					usart_stream->int_priority, (void *)usart_stream,
					usart_stream_ontx_int, usart_stream_onrx_int);
	}
	return VSFERR_NONE;
}

vsf_err_t usart_stream_fini(struct usart_stream_info_t *usart_stream)
{
	if (usart_stream->usart_index != IFS_DUMMY_PORT)
	{
		core_interfaces.usart.config_callback(usart_stream->usart_index,
					usart_stream->int_priority, NULL, NULL, NULL);
		core_interfaces.usart.fini(usart_stream->usart_index);
	}
	return VSFERR_NONE;
}

vsf_err_t usart_stream_config(struct usart_stream_info_t *usart_stream)
{
	if ((usart_stream->usart_index != IFS_DUMMY_PORT) &&
		core_interfaces.usart.config(usart_stream->usart_index,
										usart_stream->usart_info.baudrate,
										usart_stream->usart_info.datalength,
										usart_stream->usart_info.mode))
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

uint32_t usart_stream_rx(struct usart_stream_info_t *usart_stream,
							struct vsf_buffer_t *buffer)
{
	return stream_rx(&usart_stream->stream_rx, buffer);
}

uint32_t usart_stream_tx(struct usart_stream_info_t *usart_stream,
							struct vsf_buffer_t *buffer)
{
	uint32_t result = stream_tx(&usart_stream->stream_tx, buffer);
	if (!usart_stream->txing && (usart_stream->usart_index != IFS_DUMMY_PORT))
	{
		usart_stream_send_byte(usart_stream);
	}
	return result;
}
