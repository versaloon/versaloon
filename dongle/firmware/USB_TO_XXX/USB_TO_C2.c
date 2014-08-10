/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_C2.c                                               *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_C2                         *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_C2_EN

#include "USB_TO_XXX.h"
#include "app_interfaces.h"

void USB_TO_C2_ProcessCmd(uint8_t *dat, uint16_t len)
{
	uint16_t index, length;
	uint8_t command, device_idx;
	
	uint8_t tmp;
	
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
			if (app_interfaces.c2.init(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_FINI:
			if (app_interfaces.c2.fini(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_C2_Data:
			tmp = dat[index + 0] & 0x07;
			
			if(dat[index + 0] & 0x80)
			{
				if (app_interfaces.c2.data_read(device_idx,
											&buffer_reply[rep_len + 1], tmp))
				{
					buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
				}
				else
				{
					buffer_reply[rep_len++] = USB_TO_XXX_OK;
				}
				rep_len += tmp;
			}
			else
			{
				if (app_interfaces.c2.data_write(device_idx, &dat[index + 1],
													tmp))
				{
					buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
				}
				else
				{
					buffer_reply[rep_len++] = USB_TO_XXX_OK;
				}
			}
			break;
		case USB_TO_C2_WriteAddr:
			if (app_interfaces.c2.addr_write(device_idx, dat[index]))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_C2_ReadAddr:
			if (app_interfaces.c2.addr_read(device_idx,
											&buffer_reply[rep_len + 1]))
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
			buffer_reply[rep_len++] = USB_TO_XXX_INVALID_CMD;
			break;
		}
		index += length;
	}
}

#endif
