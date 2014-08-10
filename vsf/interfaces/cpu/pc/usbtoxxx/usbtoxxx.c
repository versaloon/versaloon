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

#include "interfaces.h"
#include "usbtoxxx.h"
#include "usbtoxxx_internal.h"

#define N_A		"n/a"
static const char* types_name[96] =
{
"usbtousart", "usbtospi", "usbtoi2c", "usbtogpio", "usbtocan", "usbtopwm",
													"usbtoadc", "usbtodac",
"usbtomicrowire", "usbtoswim", "usbtodusi", "usbtoebi", N_A, N_A, "usbtopower", 
																"usbtodelay",
N_A, N_A, N_A, N_A, N_A, N_A, N_A, N_A, N_A,
N_A, N_A, N_A, N_A, N_A, N_A, N_A,
"usbtojtagll", "usbtojtaghl", "usbtoissp", "usbtoc2", "usbtosbw",
									"usbtolpcicp", "usbtoswd", "usbtojtagraw",
"usbtobdm", N_A, N_A, N_A, N_A, N_A, N_A, N_A,
N_A, N_A, N_A, N_A, N_A, N_A, N_A, N_A,
"usbtomsp430jtag", N_A, N_A, N_A, N_A, N_A, N_A, N_A,
"usbtopower", "usbtodelay", "usbtopoll", N_A, N_A, N_A, N_A, N_A,
N_A, N_A, N_A, N_A, N_A, N_A, N_A, "usbtoall"
};

struct usbtoxxx_info_t *usbtoxxx_info;

#define usbtoxxx_get_type_name(type)	\
			types_name[((type) - USBTOXXX_CFG_CMD_START) \
					   % (sizeof(types_name) / sizeof(types_name[0]))]

void usbtoxxx_set_pending_id(uint32_t id)
{
	usbtoxxx_info->pending_id = id;
}
void usbtoxxx_set_callback(usbtoxxx_callback_t callback)
{
	usbtoxxx_info->callback = callback;
}
void usbtoxxx_set_extra_data(void * p)
{
	usbtoxxx_info->extra_data = p;
}

vsf_err_t usbtoxxx_add_want_pos(uint16_t offset, uint16_t size,
										uint8_t *buff)
{
	struct usbtoxxx_want_pos_t *new_pos = NULL;
	
	new_pos = (struct usbtoxxx_want_pos_t *)malloc(sizeof(*new_pos));
	if (NULL == new_pos)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return VSFERR_NOT_ENOUGH_RESOURCES;
	}
	new_pos->offset = offset;
	new_pos->size = size;
	new_pos->buff = buff;
	new_pos->next = NULL;
	
	if (NULL == usbtoxxx_info->want_pos)
	{
		usbtoxxx_info->want_pos = new_pos;
	}
	else
	{
		struct usbtoxxx_want_pos_t *tmp = usbtoxxx_info->want_pos;
		
		while (tmp->next != NULL)
		{
			tmp = tmp->next;
		}
		tmp->next = new_pos;
	}
	
	return VSFERR_NONE;
}

static void usbtoxxx_free_want_pos(void)
{
	uint16_t i;
	struct usbtoxxx_want_pos_t *tmp, *free_tmp;
	
	tmp = usbtoxxx_info->want_pos;
	while (tmp != NULL)
	{
		free_tmp = tmp;
		tmp = tmp->next;
		free(free_tmp);
	}
	usbtoxxx_info->want_pos = NULL;
	
	for (i = 0; i < dimof(usbtoxxx_info->pending); i++)
	{
		tmp = usbtoxxx_info->pending[i].pos;
		while (tmp != NULL)
		{
			free_tmp = tmp;
			tmp = tmp->next;
			free(free_tmp);
		}
		usbtoxxx_info->pending[i].pos = NULL;
	}
}

vsf_err_t usbtoxxx_add_pending(uint8_t type, uint8_t cmd,
	uint16_t actual_szie, uint16_t want_pos, uint16_t want_size,
	uint8_t *buffer, uint8_t collect)
{
#if PARAM_CHECK
	if (usbtoxxx_info->pending_idx >= USBTOXXX_CFG_MAX_PENDING_NUMBER)
	{
		LOG_BUG(ERRMSG_INVALID_INDEX, usbtoxxx_info->pending_idx,
					"usbtoxxx pending data");
		return VSFERR_FAIL;
	}
#endif
	
	usbtoxxx_info->pending[usbtoxxx_info->pending_idx].type = type;
	usbtoxxx_info->pending[usbtoxxx_info->pending_idx].cmd = cmd;
	usbtoxxx_info->pending[usbtoxxx_info->pending_idx].actual_data_size = actual_szie;
	usbtoxxx_info->pending[usbtoxxx_info->pending_idx].want_data_pos = want_pos;
	usbtoxxx_info->pending[usbtoxxx_info->pending_idx].want_data_size = want_size;
	usbtoxxx_info->pending[usbtoxxx_info->pending_idx].data_buffer = buffer;
	usbtoxxx_info->pending[usbtoxxx_info->pending_idx].collect = collect;
	usbtoxxx_info->pending[usbtoxxx_info->pending_idx].id = usbtoxxx_info->pending_id;
	usbtoxxx_info->pending_id = 0;
	usbtoxxx_info->pending[usbtoxxx_info->pending_idx].extra_data = usbtoxxx_info->extra_data;
	usbtoxxx_info->extra_data = NULL;
	usbtoxxx_info->pending[usbtoxxx_info->pending_idx].callback = usbtoxxx_info->callback;
	usbtoxxx_info->callback = NULL;
	usbtoxxx_info->pending[usbtoxxx_info->pending_idx].pos = usbtoxxx_info->want_pos;
	usbtoxxx_info->want_pos = NULL;
	usbtoxxx_info->pending_idx++;
	
	return (USBTOXXX_CFG_MAX_PENDING_NUMBER > 1) ? VSFERR_NONE : usbtoxxx_execute_command();
}

static void usbtoxxx_save_context(struct usbtoxxx_context_t *c)
{
	c->type_pre = usbtoxxx_info->type_pre;
	c->usbtoxxx_buffer = usbtoxxx_info->usbtoxxx_buffer;
	c->usbtoxxx_buffer_index = usbtoxxx_info->buffer_index;
	c->usbtoxxx_current_cmd_index = usbtoxxx_info->current_cmd_index;
	c->usbtoxxx_pending_idx = usbtoxxx_info->pending_idx;
}

static void usbtoxxx_pop_context(struct usbtoxxx_context_t *c)
{
	usbtoxxx_info->type_pre = c->type_pre;
	usbtoxxx_info->usbtoxxx_buffer = c->usbtoxxx_buffer;
	usbtoxxx_info->buffer_index = c->usbtoxxx_buffer_index;
	usbtoxxx_info->current_cmd_index = c->usbtoxxx_current_cmd_index;
	usbtoxxx_info->pending_idx = c->usbtoxxx_pending_idx;
}

static vsf_err_t usbtoxxx_validate_current_command_type(void)
{
	if (usbtoxxx_info->type_pre > 0)
	{
		// not the first command
		if (NULL == usbtoxxx_info->usbtoxxx_buffer)
		{
			LOG_BUG(ERRMSG_INVALID_BUFFER, TO_STR(usbtoxxx_info->usbtoxxx_buffer));
			return ERRCODE_INVALID_BUFFER;
		}
		
		usbtoxxx_info->usbtoxxx_buffer[0] = usbtoxxx_info->type_pre;
		SET_LE_U16(&usbtoxxx_info->usbtoxxx_buffer[1], usbtoxxx_info->current_cmd_index);
		
		usbtoxxx_info->buffer_index += usbtoxxx_info->current_cmd_index;
	}
	else
	{
		// first command
		usbtoxxx_info->buffer_index = 3;
	}
	
	// prepare for next command
	usbtoxxx_info->current_cmd_index = 3;
	usbtoxxx_info->usbtoxxx_buffer = usbtoxxx_info->buff + usbtoxxx_info->buffer_index;
	
	usbtoxxx_info->collect_index = 0;
	usbtoxxx_info->collect_cmd = 0;
	
	return VSFERR_NONE;
}



vsf_err_t usbtoxxx_execute_command(void)
{
	uint16_t i;
	uint16_t inlen;
	vsf_err_t err = VSFERR_NONE;
	
	if (usbtoxxx_info->poll_nesting)
	{
		LOG_BUG(ERRMSG_INVALID_USAGE, "USB_TO_POLL");
		usbtoxxx_free_want_pos();
		return VSFERR_FAIL;
	}
	
	if (usbtoxxx_validate_current_command_type())
	{
		LOG_BUG(ERRMSG_FAILURE_OPERATION, "validate previous commands");
		usbtoxxx_free_want_pos();
		return ERRCODE_FAILURE_OPERATION;
	}
	if (3 == usbtoxxx_info->buffer_index)
	{
		usbtoxxx_free_want_pos();
		return VSFERR_NONE;
	}
	
	usbtoxxx_info->buff[0] = USB_TO_ALL;
	SET_LE_U16(&usbtoxxx_info->buff[1], usbtoxxx_info->buffer_index);
	
	inlen = usbtoxxx_info->buff_len;
	if (usbtoxxx_info->comm->transact(usbtoxxx_info->buff,
			usbtoxxx_info->buffer_index, usbtoxxx_info->buff, &inlen))
	{
		usbtoxxx_free_want_pos();
		return VSFERR_FAIL;
	}
	
	// process return data
	usbtoxxx_info->buffer_index = 0;
	for (i = 0; i < usbtoxxx_info->pending_idx; i++)
	{
		// check result
		if ((0 == i) || !((usbtoxxx_info->pending[i].collect)
							&& (usbtoxxx_info->pending[i - 1].collect)
							&& (usbtoxxx_info->pending[i].cmd
								== usbtoxxx_info->pending[i - 1].cmd)))
		{
			if (USB_TO_XXX_CMD_NOT_SUPPORT
				== usbtoxxx_info->buff[usbtoxxx_info->buffer_index])
			{
				LOG_ERROR(ERRMSG_NOT_SUPPORT_BY,
							usbtoxxx_get_type_name(usbtoxxx_info->pending[i].type),
							"current dongle");
				err = VSFERR_FAIL;
				break;
			}
			else if (USB_TO_XXX_OK !=
						usbtoxxx_info->buff[usbtoxxx_info->buffer_index])
			{
				LOG_ERROR("%s command 0x%02x failed with 0x%02x",
					usbtoxxx_get_type_name(usbtoxxx_info->pending[i].type),
					usbtoxxx_info->pending[i].cmd,
					usbtoxxx_info->buff[usbtoxxx_info->buffer_index]);
				err = VSFERR_FAIL;
				break;
			}
			usbtoxxx_info->buffer_index++;
		}
		
		// get result data
		if (usbtoxxx_info->pending[i].pos != NULL)
		{
			uint8_t processed = 0;
			
			if (usbtoxxx_info->pending[i].callback != NULL)
			{
				usbtoxxx_info->pending[i].callback(&usbtoxxx_info->pending[i],
					usbtoxxx_info->buff + usbtoxxx_info->buffer_index, &processed);
			}
			if (!processed)
			{
				struct usbtoxxx_want_pos_t *tmp;
				
				tmp = usbtoxxx_info->pending[i].pos;
				while (tmp != NULL)
				{
					struct usbtoxxx_want_pos_t *free_tmp;
					
					if ((tmp->buff != NULL) && (tmp->size > 0))
					{
						memcpy(tmp->buff,
							usbtoxxx_info->buff + usbtoxxx_info->buffer_index
							+ tmp->offset, tmp->size);
					}
					free_tmp = tmp;
					tmp = tmp->next;
					free(free_tmp);
				}
				usbtoxxx_info->pending[i].pos = NULL;
			}
		}
		else if ((usbtoxxx_info->pending[i].want_data_size > 0)
			&& (usbtoxxx_info->pending[i].data_buffer != NULL))
		{
			uint8_t processed = 0;
			
			if (usbtoxxx_info->pending[i].callback != NULL)
			{
				usbtoxxx_info->pending[i].callback(&usbtoxxx_info->pending[i],
					usbtoxxx_info->buff + usbtoxxx_info->buffer_index, &processed);
			}
			if (!processed)
			{
				memcpy(usbtoxxx_info->pending[i].data_buffer,
					   usbtoxxx_info->buff + usbtoxxx_info->buffer_index
							+ usbtoxxx_info->pending[i].want_data_pos,
					   usbtoxxx_info->pending[i].want_data_size);
			}
		}
		usbtoxxx_info->buffer_index += usbtoxxx_info->pending[i].actual_data_size;
		if (usbtoxxx_info->buffer_index > inlen)
		{
			LOG_BUG("%s command 0x%02x process error",
					usbtoxxx_get_type_name(usbtoxxx_info->pending[i].type),
					usbtoxxx_info->pending[i].cmd);
			err = VSFERR_FAIL;
			break;
		}
	}
	
	// data is not the right size
	if (inlen != usbtoxxx_info->buffer_index)
	{
		LOG_ERROR(ERRMSG_INVALID_TARGET, "length of return data");
		err = VSFERR_FAIL;
	}
	
	if (usbtoxxx_info->pending_idx > 0)
	{
		usbtoxxx_info->pending_idx = 0;
	}
	else
	{
		// no receive data, avoid collision
		sleep_ms(10);
	}
	
	usbtoxxx_info->type_pre = 0;
	usbtoxxx_info->collect_cmd = 0;
	usbtoxxx_info->collect_index = 0;
	usbtoxxx_free_want_pos();
	return err;
}

vsf_err_t usbtoxxx_init(void)
{
	if (0 == usbtoxxx_info->buff_len)
	{
		return VSFERR_INVALID_PARAMETER;
	}
	usbtoxxx_info->buff_allocated = false;
	usbtoxxx_info->cmd_buff_allocated = false;
	
	memset(usbtoxxx_info->pending, 0, sizeof(usbtoxxx_info->pending));
	usbtoxxx_info->pending_idx = 0;
	
	if (NULL == usbtoxxx_info->buff)
	{
		usbtoxxx_info->buff = (uint8_t *)malloc(usbtoxxx_info->buff_len);
		if (NULL == usbtoxxx_info->buff)
		{
			return VSFERR_NOT_ENOUGH_RESOURCES;
		}
		usbtoxxx_info->buff_allocated = true;
	}
	if (NULL == usbtoxxx_info->cmd_buff)
	{
		usbtoxxx_info->cmd_buff = (uint8_t *)malloc(usbtoxxx_info->buff_len);
		if (NULL == usbtoxxx_info->cmd_buff)
		{
			return VSFERR_NOT_ENOUGH_RESOURCES;
		}
		usbtoxxx_info->cmd_buff_allocated = true;
	}
	
	if (usbtoinfo_get_abilities(usbtoxxx_info->abilities) ||
		usbtoxxx_execute_command())
	{
		return VSFERR_FAIL;
	}
	LOG_INFO("USB_TO_XXX abilities: 0x%08X:0x%08X:0x%08X",
		GET_LE_U32(&usbtoxxx_info->abilities[0]),
		GET_LE_U32(&usbtoxxx_info->abilities[4]),
		GET_LE_U32(&usbtoxxx_info->abilities[8]));
	return VSFERR_NONE;
}

vsf_err_t usbtoxxx_fini(void)
{
	if (usbtoxxx_info->buff_allocated && (usbtoxxx_info->buff != NULL))
	{
		free(usbtoxxx_info->buff);
		usbtoxxx_info->buff = NULL;
	}
	if (usbtoxxx_info->cmd_buff_allocated && (usbtoxxx_info->cmd_buff != NULL))
	{
		free(usbtoxxx_info->cmd_buff);
		usbtoxxx_info->cmd_buff = NULL;
	}
	usbtoxxx_free_want_pos();
	usbtoxxx_info->usbtoxxx_buffer = NULL;
	usbtoxxx_info->type_pre = 0;
	return VSFERR_NONE;
}

bool usbtoxxx_interface_supported(uint8_t cmd)
{
	if ((cmd < USBTOXXX_CFG_CMD_START) ||
		(cmd >= (USBTOXXX_CFG_CMD_START + USBTOXXX_CMD_NUM)))
	{
		return false;
	}
	
	cmd -= USBTOXXX_CFG_CMD_START;
	return (usbtoxxx_info->abilities[cmd  / 8] & (1 << (cmd % 8))) > 0;
}



vsf_err_t usbtoxxx_ensure_buffer_size(uint16_t cmdlen)
{
	// check free space, commit if not enough
	if (((usbtoxxx_info->buffer_index + usbtoxxx_info->current_cmd_index + cmdlen)
			>= usbtoxxx_info->buff_len)
		|| (usbtoxxx_info->pending_idx >= USBTOXXX_CFG_MAX_PENDING_NUMBER))
	{
		struct usbtoxxx_context_t context_tmp;
		uint8_t poll_nesting_tmp = 0;
		
		memset(&context_tmp, 0, sizeof(context_tmp));
		if (usbtoxxx_info->poll_nesting)
		{
			if (0 == usbtoxxx_info->poll_context.type_pre)
			{
				LOG_BUG("USB_TO_POLL toooooo long");
				return VSFERR_NONE;
			}
			
			usbtoxxx_save_context(&context_tmp);
			usbtoxxx_pop_context(&usbtoxxx_info->poll_context);
			poll_nesting_tmp = usbtoxxx_info->poll_nesting;
			usbtoxxx_info->poll_nesting = 0;
		}
		
		if (usbtoxxx_execute_command())
		{
			return VSFERR_FAIL;
		}
		
		if (poll_nesting_tmp)
		{
			uint16_t newlen, oldlen;
			
			newlen = context_tmp.usbtoxxx_pending_idx
									- usbtoxxx_info->poll_context.usbtoxxx_pending_idx;
			memcpy(&usbtoxxx_info->pending[0],
					&usbtoxxx_info->pending[usbtoxxx_info->poll_context.usbtoxxx_pending_idx],
					sizeof(usbtoxxx_info->pending[0]) * newlen);
			context_tmp.usbtoxxx_pending_idx = newlen;
			oldlen = usbtoxxx_info->poll_context.usbtoxxx_buffer_index
									+ usbtoxxx_info->poll_context.usbtoxxx_current_cmd_index;
			newlen = context_tmp.usbtoxxx_buffer_index
									+ context_tmp.usbtoxxx_current_cmd_index;
			memcpy(usbtoxxx_info->buff + 3, usbtoxxx_info->buff + oldlen,
					newlen - oldlen);
			oldlen -= 3;
			context_tmp.usbtoxxx_buffer -= oldlen;
			context_tmp.usbtoxxx_buffer_index -= oldlen;
			usbtoxxx_pop_context(&context_tmp);
			usbtoxxx_info->poll_nesting = poll_nesting_tmp;
		}
	}
	return VSFERR_NONE;
}

vsf_err_t usbtoxxx_add_command(uint8_t type, uint8_t cmd, uint8_t *cmdbuf,
							uint16_t cmdlen, uint16_t retlen, uint8_t *wantbuf,
							uint16_t wantpos, uint16_t wantlen, uint8_t collect)
{
	uint16_t len_tmp;
	
	// 3 more bytes by usbtoxxx_validate_current_command_type
	// 3 more bytes when ((0 == collect_index) || (collect_cmd != cmd))
	if (usbtoxxx_ensure_buffer_size(cmdlen + 6))
	{
		return VSFERR_FAIL;
	}
	
	if ((usbtoxxx_info->type_pre != type) || (NULL == usbtoxxx_info->usbtoxxx_buffer))
	{
		if (usbtoxxx_validate_current_command_type())
		{
			LOG_BUG(ERRMSG_FAILURE_OPERATION, "validate previous commands");
			return ERRCODE_FAILURE_OPERATION;
		}
		usbtoxxx_info->type_pre = type;
	}
	
	if ((0 == usbtoxxx_info->collect_index) || (usbtoxxx_info->collect_cmd != cmd))
	{
		usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->current_cmd_index++] = cmd;
		
		if (collect)
		{
			usbtoxxx_info->collect_index = usbtoxxx_info->current_cmd_index;
			usbtoxxx_info->collect_cmd = cmd;
		}
		else
		{
			usbtoxxx_info->collect_index = 0;
			usbtoxxx_info->collect_cmd = 0;
		}
		SET_LE_U16(&usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->current_cmd_index], cmdlen);
		usbtoxxx_info->current_cmd_index += 2;
	}
	else
	{
		len_tmp = GET_LE_U16(&usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->collect_index]) + cmdlen;
		SET_LE_U16(&usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->collect_index], len_tmp);
	}
	
	if (cmdbuf != NULL)
	{
		memcpy(usbtoxxx_info->usbtoxxx_buffer + usbtoxxx_info->current_cmd_index, cmdbuf, cmdlen);
		usbtoxxx_info->current_cmd_index += cmdlen;
	}
	
	return usbtoxxx_add_pending(type, cmd, retlen, wantpos, wantlen,
								 wantbuf, collect);
}





vsf_err_t usbtoinfo_get_abilities(uint8_t abilities[USB_TO_XXX_ABILITIES_LEN])
{
	if (usbtoxxx_ensure_buffer_size(3))
	{
		return VSFERR_FAIL;
	}
	
	if (usbtoxxx_validate_current_command_type())
	{
		LOG_BUG(ERRMSG_FAILURE_OPERATION, "validate previous commands");
		return ERRCODE_FAILURE_OPERATION;
	}
	usbtoxxx_info->type_pre = USB_TO_INFO;
	
	return usbtoxxx_add_pending(USB_TO_INFO, 0, USB_TO_XXX_ABILITIES_LEN, 0,
									USB_TO_XXX_ABILITIES_LEN, abilities, 0);
}




vsf_err_t usbtopoll_start(uint16_t retry_cnt, uint16_t interval_us)
{
	if (usbtoxxx_ensure_buffer_size(3 + 5))
	{
		return VSFERR_FAIL;
	}
	if (!usbtoxxx_info->poll_nesting)
	{
		usbtoxxx_save_context(&usbtoxxx_info->poll_context);
	}
	
	if (usbtoxxx_validate_current_command_type())
	{
		LOG_BUG(ERRMSG_FAILURE_OPERATION, "validate previous commands");
		return ERRCODE_FAILURE_OPERATION;
	}
	usbtoxxx_info->poll_nesting++;
	usbtoxxx_info->type_pre = USB_TO_POLL;
	
	usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->current_cmd_index++] = USB_TO_POLL_START;
	SET_LE_U16(&usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->current_cmd_index], retry_cnt);
	usbtoxxx_info->current_cmd_index += 2;
	SET_LE_U16(&usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->current_cmd_index], interval_us);
	usbtoxxx_info->current_cmd_index += 2;
	
	return usbtoxxx_add_pending(USB_TO_POLL, 0, 0, 0, 0, NULL, 0);
}

vsf_err_t usbtopoll_end(void)
{
	if (!usbtoxxx_info->poll_nesting)
	{
		LOG_BUG(ERRMSG_FAILURE_OPERATION, "check poll nesting");
		return ERRCODE_FAILURE_OPERATION;
	}
	if (usbtoxxx_ensure_buffer_size(3 + 1))
	{
		return VSFERR_FAIL;
	}
	
	if (usbtoxxx_validate_current_command_type())
	{
		LOG_BUG(ERRMSG_FAILURE_OPERATION, "validate previous commands");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	usbtoxxx_info->poll_nesting--;
	usbtoxxx_info->type_pre = USB_TO_POLL;
	
	usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->current_cmd_index++] = USB_TO_POLL_END;
	
	return usbtoxxx_add_pending(USB_TO_POLL, 0, 0, 0, 0, NULL, 0);
}

vsf_err_t usbtopoll_checkok(uint8_t equ, uint16_t offset, uint8_t size,
							uint32_t mask, uint32_t value)
{
	uint8_t i;
	
	if (size > 4)
	{
		LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
		return VSFERR_INVALID_PARAMETER;
	}
	if (!usbtoxxx_info->poll_nesting)
	{
		LOG_BUG(ERRMSG_FAILURE_OPERATION, "check poll nesting");
		return ERRCODE_FAILURE_OPERATION;
	}
	if (usbtoxxx_ensure_buffer_size(3 + 4 + 2 * size))
	{
		return VSFERR_FAIL;
	}
	
	if (usbtoxxx_validate_current_command_type())
	{
		LOG_BUG(ERRMSG_FAILURE_OPERATION, "validate previous commands");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	usbtoxxx_info->type_pre = USB_TO_POLL;
	
	usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->current_cmd_index++] = USB_TO_POLL_CHECKOK;
	SET_LE_U16(&usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->current_cmd_index], offset);
	usbtoxxx_info->current_cmd_index += 2;
	usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->current_cmd_index++] = size;
	usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->current_cmd_index++] = equ;
	for (i =0; i < size; i++)
	{
		usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->current_cmd_index++] =
			(mask >> (8 * i)) & 0xFF;
	}
	for (i =0; i < size; i++)
	{
		usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->current_cmd_index++] =
			(value >> (8 * i)) & 0xFF;
	}
	
	return VSFERR_NONE;
}

vsf_err_t usbtopoll_checkfail(uint8_t equ, uint16_t offset, uint8_t size,
							uint32_t mask, uint32_t value)
{
	uint8_t i;
	
	if (size > 4)
	{
		LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
		return VSFERR_INVALID_PARAMETER;
	}
	if (!usbtoxxx_info->poll_nesting)
	{
		LOG_BUG(ERRMSG_FAILURE_OPERATION, "check poll nesting");
		return ERRCODE_FAILURE_OPERATION;
	}
	if (usbtoxxx_ensure_buffer_size(3 + 4 + 2 * size))
	{
		return VSFERR_FAIL;
	}
	
	if (usbtoxxx_validate_current_command_type())
	{
		LOG_BUG(ERRMSG_FAILURE_OPERATION, "validate previous commands");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	usbtoxxx_info->type_pre = USB_TO_POLL;
	
	usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->current_cmd_index++] = USB_TO_POLL_CHECKFAIL;
	SET_LE_U16(&usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->current_cmd_index], offset);
	usbtoxxx_info->current_cmd_index += 2;
	usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->current_cmd_index++] = size;
	usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->current_cmd_index++] = equ;
	for (i =0; i < size; i++)
	{
		usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->current_cmd_index++] =
			(mask >> (8 * i)) & 0xFF;
	}
	for (i =0; i < size; i++)
	{
		usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->current_cmd_index++] =
			(value >> (8 * i)) & 0xFF;
	}
	
	return VSFERR_NONE;
}

vsf_err_t usbtopoll_verifybuff(uint16_t offset, uint16_t size, uint8_t *buff)
{
	if (!usbtoxxx_info->poll_nesting)
	{
		LOG_BUG(ERRMSG_FAILURE_OPERATION, "check poll nesting");
		return ERRCODE_FAILURE_OPERATION;
	}
	if (usbtoxxx_ensure_buffer_size(3 + 5 + size))
	{
		return VSFERR_FAIL;
	}
	
	if (usbtoxxx_validate_current_command_type())
	{
		LOG_BUG(ERRMSG_FAILURE_OPERATION, "validate previous commands");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	usbtoxxx_info->type_pre = USB_TO_POLL;
	
	usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->current_cmd_index++] = USB_TO_POLL_VERIFYBUFF;
	SET_LE_U16(&usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->current_cmd_index], offset);
	usbtoxxx_info->current_cmd_index += 2;
	SET_LE_U16(&usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->current_cmd_index], size);
	usbtoxxx_info->current_cmd_index += 2;
	memcpy(&usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->current_cmd_index], buff, size);
	usbtoxxx_info->current_cmd_index += size;
	
	return VSFERR_NONE;
}




static vsf_err_t usbtodelay_delay(uint16_t dly)
{
	if (usbtoxxx_ensure_buffer_size(3 + 2))
	{
		return VSFERR_FAIL;
	}
	
	if (usbtoxxx_validate_current_command_type())
	{
		LOG_BUG(ERRMSG_FAILURE_OPERATION, "validate previous commands");
		return ERRCODE_FAILURE_OPERATION;
	}
	usbtoxxx_info->type_pre = USB_TO_DELAY;
	
	SET_LE_U16(&usbtoxxx_info->usbtoxxx_buffer[usbtoxxx_info->current_cmd_index], dly);
	usbtoxxx_info->current_cmd_index += 2;
	
	return usbtoxxx_add_pending(USB_TO_DELAY, 0, 0, 0, 0, NULL, 0);
}

vsf_err_t usbtodelay_init(void)
{
	return VSFERR_NONE;
}

vsf_err_t usbtodelay_delayms(uint16_t ms)
{
	return usbtodelay_delay(ms | 0x8000);
}

vsf_err_t usbtodelay_delayus(uint16_t us)
{
	return usbtodelay_delay(us & 0x7FFF);
}

