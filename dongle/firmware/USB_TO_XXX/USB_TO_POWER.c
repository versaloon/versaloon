/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_POWER.c                                            *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_POWER                      *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_POWER_EN

#include "USB_TO_XXX.h"
#include "app_interfaces.h"

void USB_TO_POWER_ProcessCmd(uint8_t *dat, uint16_t len)
{
	uint16_t index, length;
	uint8_t command, device_idx;
	
	uint16_t voltage;
	
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
			if (0 == device_idx)
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_CONFIG:
			if (0 == device_idx)
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_FINI:
			if (0 == device_idx)
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_IN:
			if (app_interfaces.target_voltage.get(device_idx, &voltage))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
				voltage = 0;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			SET_LE_U16(&buffer_reply[rep_len], voltage);
			rep_len += 2;
			break;
		case USB_TO_XXX_OUT:
			voltage = GET_LE_U16(&dat[index]);
			if (app_interfaces.target_voltage.set(device_idx, voltage))
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
