/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_ISSP.c                                             *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_ISSP                       *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_ISSP_EN

#include "USB_TO_XXX.h"
#include "app_interfaces.h"

void USB_TO_ISSP_ProcessCmd(uint8_t *dat, uint16_t len)
{
	uint16_t index, length;
	uint8_t command, device_idx;
	
	bool fail;
	uint8_t operate, addr, data;
	uint16_t rlen, i;
	uint16_t vector_num;
	
	index = 0;
	while(index < len)
	{
		command = dat[index] & USB_TO_XXX_CMDMASK;
		device_idx = dat[index] & USB_TO_XXX_IDXMASK;
		length = GET_LE_U16(&dat[index + 1]);
		index += 3;
		
		switch(command)
		{
		case USB_TO_XXX_INIT:
			if (app_interfaces.issp.init(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_FINI:
			if (app_interfaces.issp.fini(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_ISSP_Vector:
			if(length % 3 != 0)
			{
				buffer_reply[rep_len++] = USB_TO_XXX_INVALID_PARA;
				return;
			}
			vector_num = length / 3;
			
			fail = false;
			rlen = 0;
			
			for(i = 0; i < vector_num; i++)
			{
				operate = dat[index + i * 3];
				addr = dat[index + i * 3 + 1];
				data = dat[index + i * 3 + 2];
				if (app_interfaces.issp.vector(device_idx, operate, addr, data,
											&buffer_reply[rep_len + 1 + rlen]))
				{
					fail = true;
					break;
				}
				else
				{
					rlen++;
				}
			}
			if (fail)
			{
				buffer_reply[rep_len] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len] = USB_TO_XXX_OK;
			}
			rep_len += 1 + rlen;
			break;
		case USB_TO_ISSP_EnterProgMode:
			if (app_interfaces.issp.enter_program_mode(device_idx, dat[index]))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_ISSP_LeaveProgMode:
			if (app_interfaces.issp.leave_program_mode(device_idx, dat[index]))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_ISSP_WaitAndPoll:
			if (app_interfaces.issp.wait_and_poll(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		default:
			buffer_reply[rep_len++] = USB_TO_XXX_CMD_NOT_SUPPORT;
			break;
		}
		index += length;
	}
}

#endif
