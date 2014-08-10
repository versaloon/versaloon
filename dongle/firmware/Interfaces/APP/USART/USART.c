/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USART.c                                                   *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    USART interface implementation file                       *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_USART_EN

#include "app_interfaces.h"
#include "tool/buffer/buffer.h"
#include "dal/usart_stream/usart_stream.h"

#include "USART.h"

extern uint8_t asyn_rx_buf[ASYN_DATA_BUFF_SIZE];
struct usart_stream_info_t usart_stream_p0 = 
{
	USART_PORT,							// usart_index
	{
		{{asyn_rx_buf, 1024}},			// fifo
	},									// struct vsf_stream_t stream_tx;
	{
		{{asyn_rx_buf + 1024, 1024}},	// fifo
	}									// struct vsf_stream_t stream_rx;
};

vsf_err_t usart_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		return usart_stream_init(&usart_stream_p0);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t usart_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		return usart_stream_fini(&usart_stream_p0);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t usart_config(uint8_t index, uint32_t baudrate, uint8_t datalength, 
						uint8_t mode)
{
	switch (index)
	{
	case 0:
		usart_stream_p0.usart_info.datalength = datalength;
		usart_stream_p0.usart_info.baudrate = baudrate;
		usart_stream_p0.usart_info.mode = mode;
		return usart_stream_config(&usart_stream_p0);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t usart_send(uint8_t index, uint8_t *buf, uint16_t len)
{
	struct vsf_buffer_t buffer;
	
	switch (index)
	{
	case 0:
		if (NULL == buf)
		{
			return VSFERR_INVALID_PTR;
		}
		
		buffer.buffer = buf;
		buffer.size = len;
		return usart_stream_tx(&usart_stream_p0, &buffer);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t usart_receive(uint8_t index, uint8_t *buf, uint16_t len)
{
	struct vsf_buffer_t buffer;
	
	switch (index)
	{
	case 0:
		if (NULL == buf)
		{
			return VSFERR_INVALID_PTR;
		}
		
		buffer.buffer = buf;
		buffer.size = len;
		return usart_stream_rx(&usart_stream_p0, &buffer);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t usart_status(uint8_t index, struct usart_status_t *status)
{
	struct vsf_fifo_t *fifo_tx, *fifo_rx;
	
	switch (index)
	{
	case 0:
		if (NULL == status)
		{
			return VSFERR_INVALID_PTR;
		}
		
		fifo_tx = &usart_stream_p0.stream_tx.fifo;
		fifo_rx = &usart_stream_p0.stream_rx.fifo;
		status->tx_buff_avail = vsf_fifo_get_avail_length(fifo_tx);
		status->tx_buff_size = vsf_fifo_get_data_length(fifo_tx);
		status->rx_buff_avail = vsf_fifo_get_avail_length(fifo_rx);
		status->rx_buff_size = vsf_fifo_get_data_length(fifo_rx);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t usart_poll(uint8_t index)
{
	switch (index)
	{
	case 0:
		return usart_stream_poll(&usart_stream_p0);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

#endif
