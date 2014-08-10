/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_GPIO.c                                             *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_GPIO                       *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_GPIO_EN

#include "USB_TO_XXX.h"
#include "app_interfaces.h"

void USB_TO_GPIO_ProcessCmd(uint8_t *dat, uint16_t len)
{
	uint16_t index, length;
	uint8_t command, device_idx;
	
	uint32_t port_data, mask_data, io_data, pull_en_mask;
	
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
			if (app_interfaces.gpio.init(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_CONFIG:
			mask_data = GET_LE_U16(&dat[index + 0]);
			io_data   = GET_LE_U16(&dat[index + 2]);
			pull_en_mask  = GET_LE_U16(&dat[index + 4]);
			port_data = GET_LE_U16(&dat[index + 6]);
			io_data  &= mask_data;
			
			if (app_interfaces.gpio.config(device_idx, mask_data, io_data,
											pull_en_mask, port_data))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_FINI:
			if (app_interfaces.gpio.fini(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_IN:
			mask_data = GET_LE_U16(&dat[index]);
			
			if (app_interfaces.gpio.in(device_idx, mask_data, &port_data))
			{
				buffer_reply[rep_len] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len] = USB_TO_XXX_OK;
				SET_LE_U16(&buffer_reply[rep_len + 1], port_data);
			}
			rep_len += 3;
			break;
		case USB_TO_XXX_OUT:
			mask_data = GET_LE_U16(&dat[index + 0]);
			port_data = GET_LE_U16(&dat[index + 2]);
			
			if (app_interfaces.gpio.out(device_idx, mask_data, port_data))
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
