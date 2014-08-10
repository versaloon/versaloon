/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_LPCICP.c                                           *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_LPCICP                     *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_LPCICP_EN

#include "USB_TO_XXX.h"
#include "app_interfaces.h"

void USB_TO_LPCICP_ProcessCmd(uint8_t *dat, uint16_t len)
{
	uint16_t index, length;
	uint8_t command, device_idx;
	
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
			if (app_interfaces.lpcicp.init(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_FINI:
			if (app_interfaces.lpcicp.fini(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_LPCICP_EnterProgMode:
			if (app_interfaces.lpcicp.enter_program_mode(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_LPCICP_In:
			if (app_interfaces.lpcicp.in(device_idx, &buffer_reply[rep_len + 1],
											length))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			rep_len += length;
			break;
		case USB_TO_LPCICP_Out:
			if (app_interfaces.lpcicp.out(device_idx, &dat[index], length))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_LPCICP_PollRdy:
			if (app_interfaces.lpcicp.poll_ready(device_idx, dat[index + 0], 
					&buffer_reply[rep_len + 1], dat[index + 1], 
					dat[index + 2], GET_LE_U16(&dat[index + 3])))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			rep_len++;
			break;
		default:
			buffer_reply[rep_len++] = USB_TO_XXX_CMD_NOT_SUPPORT;
			break;
		}
		index += length;
	}
}

#endif
