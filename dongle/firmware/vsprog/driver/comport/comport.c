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

#include "compiler.h"

#include "app_type.h"
#include "app_io.h"
#include "app_log.h"
#include "app_err.h"

#include "interfaces.h"
#include "dal/usart_stream/usart_stream.h"
#include "tool/buffer/buffer.h"

#include "port.h"
#include "comport.h"

extern struct usart_stream_info_t usart_stream_p0;

void comm_close_usbtocomm(void);
vsf_err_t comm_open_usbtocomm(char *comport, uint32_t baudrate,
			uint8_t datalength, char paritybit, char stopbit, char handshake);
int32_t comm_read_usbtocomm(uint8_t *buffer, uint32_t num_of_bytes);
int32_t comm_write_usbtocomm(uint8_t *buffer, uint32_t num_of_bytes);
int32_t comm_ctrl_usbtocomm(uint8_t dtr, uint8_t rts);
int32_t comm_flush_usbtocomm(void);

struct comm_func_t comm_func[] =
{
	{
		comm_open_usbtocomm,
		comm_close_usbtocomm,
		comm_read_usbtocomm,
		comm_write_usbtocomm,
		comm_ctrl_usbtocomm,
		comm_flush_usbtocomm
	}
};

uint32_t comm_idx = 0;

static uint8_t usbtocomm_open = 0;
static struct INTERFACES_INFO_T *prog = NULL;
void comm_close_usbtocomm(void)
{
	if (!usbtocomm_open)
	{
		return;
	}
	
	usart_stream_fini(&usart_stream_p0);
	usbtocomm_open = 0;
}

vsf_err_t comm_open_usbtocomm(char *comport, uint32_t baudrate,
			uint8_t datalength, char paritybit, char stopbit, char handshake)
{
	REFERENCE_PARAMETER(comport);
	REFERENCE_PARAMETER(handshake);
	
	// paritybit
	usart_stream_p0.usart_info.baudrate = baudrate;
	usart_stream_p0.usart_info.mode = 0;
	switch (paritybit)
	{
	default:
	case COMM_PARITYBIT_NONE:
		usart_stream_p0.usart_info.mode |= USART_PARITY_NONE;
		usart_stream_p0.usart_info.datalength = 8;
		break;
	case COMM_PARITYBIT_ODD:
		usart_stream_p0.usart_info.mode |= USART_PARITY_ODD;
		usart_stream_p0.usart_info.datalength = 9;
		break;
	case COMM_PARITYBIT_EVEN:
		usart_stream_p0.usart_info.mode |= USART_PARITY_EVEN;
		usart_stream_p0.usart_info.datalength = 9;
		break;
	}
	
	// stopbit
	switch (stopbit)
	{
	default:
	case COMM_STOPBIT_1:
		usart_stream_p0.usart_info.mode |= USART_STOPBITS_1;
		break;
	case COMM_STOPBIT_1P5:
		usart_stream_p0.usart_info.mode |= USART_STOPBITS_1P5;
		break;
	case COMM_STOPBIT_2:
		usart_stream_p0.usart_info.mode |= USART_STOPBITS_2;
		break;
	}
	
	// initialize usbtocomm
	if (usart_stream_init(&usart_stream_p0) ||
		usart_stream_config(&usart_stream_p0))
	{
		return -1;
	}
	
	usbtocomm_open = 1;
	return VSFERR_NONE;
}

int32_t comm_read_usbtocomm(uint8_t *buffer, uint32_t num_of_bytes)
{
	struct vsf_buffer_t sbuffer;
	uint32_t start, end;
	int32_t data_read;
	
	if (!usbtocomm_open)
	{
		return -1;
	}
	
	data_read = 0;
	start = (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
	while (data_read < num_of_bytes)
	{
		sbuffer.size = num_of_bytes - data_read;
		sbuffer.buffer = buffer + data_read;
		usart_stream_poll(&usart_stream_p0);
		if (usart_stream_rx(&usart_stream_p0, &sbuffer))
		{
			return -1;
		}
		end = (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
		if (sbuffer.size)
		{
			data_read += sbuffer.size;
			start = end;
		}
		else if ((end - start) > 3000)
		{
			break;
		}
	}
	
	return data_read;
}

int32_t comm_write_usbtocomm(uint8_t *buffer, uint32_t num_of_bytes)
{
	struct vsf_buffer_t sbuffer;
	int32_t data_write;
	
	if (!usbtocomm_open)
	{
		return -1;
	}
	
	data_write = 0;
	while (data_write < num_of_bytes)
	{
		sbuffer.size = num_of_bytes - data_write;
		sbuffer.buffer = buffer + data_write;
		usart_stream_poll(&usart_stream_p0);
		if (usart_stream_tx(&usart_stream_p0, &sbuffer))
		{
			return -1;
		}
		if (sbuffer.size)
		{
			data_write += sbuffer.size;
		}
	}
	while (vsf_fifo_get_data_length(&usart_stream_p0.stream_tx.fifo) > 0)
	{
		usart_stream_poll(&usart_stream_p0);
	}
	
	return data_write;
}

int32_t comm_flush_usbtocomm(void)
{
	if (!usbtocomm_open)
	{
		return -1;
	}
	
	while (vsf_fifo_get_data_length(&usart_stream_p0.stream_tx.fifo) > 0)
	{
		usart_stream_poll(&usart_stream_p0);
	}
	while (vsf_fifo_get_data_length(&usart_stream_p0.stream_rx.fifo) > 0)
	{
		vsf_fifo_pop8(&usart_stream_p0.stream_rx.fifo);
		usart_stream_poll(&usart_stream_p0);
	}
	return 0;
}

int32_t comm_ctrl_usbtocomm(uint8_t dtr, uint8_t rts)
{
	REFERENCE_PARAMETER(dtr);
	REFERENCE_PARAMETER(rts);
	
	if ((NULL == prog) || !usbtocomm_open)
	{
		return -1;
	}
	
	return 0;
}

void comm_close(void)
{
	comm_func[comm_idx].comm_close();
}

vsf_err_t comm_open(char *comport, uint32_t baudrate, uint8_t datalength,
				 char paritybit, char stopbit, char handshake)
{
	comm_idx = 0;
	
	return comm_func[comm_idx].comm_open(comport, baudrate, datalength, paritybit,
											stopbit, handshake);
}

int32_t comm_read(uint8_t *buffer, uint32_t num_of_bytes)
{
	return comm_func[comm_idx].comm_read(buffer, num_of_bytes);
}

int32_t comm_write(uint8_t *buffer, uint32_t num_of_bytes)
{
	return comm_func[comm_idx].comm_write(buffer, num_of_bytes);
}

int32_t comm_ctrl(uint8_t dtr, uint8_t rts)
{
	return comm_func[comm_idx].comm_ctrl(dtr, rts);
}

int32_t comm_flush(void)
{
	return comm_func[comm_idx].comm_flush();
}

