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

#ifndef __BUFFER_H_INCLUDED__
#define __BUFFER_H_INCLUDED__

struct vsf_buffer_t
{
	uint8_t *buffer;
	uint32_t size;
};

struct vsf_transaction_buffer_t
{
	struct vsf_buffer_t buffer;
	uint32_t position;
};

// fifo
struct vsf_fifo_t
{
	struct vsf_buffer_t buffer;
	uint32_t head;
	uint32_t tail;
};
vsf_err_t vsf_fifo_init(struct vsf_fifo_t *fifo);
uint32_t vsf_fifo_push8(struct vsf_fifo_t *fifo, uint8_t data);
uint8_t vsf_fifo_pop8(struct vsf_fifo_t *fifo);
uint32_t vsf_fifo_push(struct vsf_fifo_t *fifo, uint32_t size, uint8_t *data);
uint32_t vsf_fifo_pop(struct vsf_fifo_t *fifo, uint32_t size, uint8_t *data);
uint32_t vsf_fifo_peek(struct vsf_fifo_t *fifo, uint32_t size, uint8_t *data);
uint32_t vsf_fifo_peek_consequent(struct vsf_fifo_t *fifo, uint32_t size, 
								uint8_t *data);
uint32_t vsf_fifo_get_data_length(struct vsf_fifo_t *fifo);
uint32_t vsf_fifo_get_avail_length(struct vsf_fifo_t *fifo);

// multi_buffer
struct vsf_multibuf_t
{
	uint16_t count;
	uint32_t size;
	uint8_t **buffer_list;
	
	uint16_t head;
	uint16_t tail;
	uint16_t length;
};

vsf_err_t vsf_multibuf_init(struct vsf_multibuf_t *mbuffer);
uint8_t* vsf_multibuf_get_empty(struct vsf_multibuf_t *mbuffer);
vsf_err_t vsf_multibuf_push(struct vsf_multibuf_t *mbuffer);
uint8_t* vsf_multibuf_get_payload(struct vsf_multibuf_t *mbuffer);
vsf_err_t vsf_multibuf_pop(struct vsf_multibuf_t *mbuffer);

// buffer_manager
struct vsf_bufmgr_t
{
	struct vsf_buffer_t buffer;
	uint16_t num_of_buffer;
};
void vsf_bufmgr_init(struct vsf_bufmgr_t *bufmgr);
void* vsf_bufmgr_malloc(struct vsf_bufmgr_t *bufmgr, uint32_t size);
void vsf_bufmgr_free(struct vsf_bufmgr_t *bufmgr, void *ptr);

#endif	// __BUFFER_H_INCLUDED__

