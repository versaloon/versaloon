/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_SWD.c                                              *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_SWD                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_SWD_EN

#include "USB_TO_XXX.h"
#include "app_interfaces.h"

void USB_TO_SWD_ProcessCmd(uint8_t *dat, uint16_t len)
{
	uint16_t index, length;
	uint8_t command, device_idx;
	
	uint32_t swd_data;
	uint16_t bitlen;
	
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
			if (app_interfaces.swd.init(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_CONFIG:
			if (app_interfaces.swd.config(device_idx, dat[index + 0], 
					GET_LE_U16(&dat[index + 1]), GET_LE_U16(&dat[index + 3])))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_FINI:
			if (app_interfaces.swd.fini(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_SWD_SEQOUT:
			bitlen = GET_LE_U16(&dat[index]);
			if (app_interfaces.swd.seqout(device_idx, &dat[index + 2], bitlen))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_SWD_SEQIN:
			bitlen = GET_LE_U16(&dat[index]);
			if (app_interfaces.swd.seqin(device_idx, &buffer_reply[rep_len + 1],
											bitlen))
			{
				buffer_reply[rep_len] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len] = USB_TO_XXX_OK;
			}
			rep_len += 1 + ((bitlen + 7) >> 3);
			break;
		case USB_TO_SWD_Transact:
			swd_data = GET_LE_U32(&dat[index + 1]);
			if (app_interfaces.swd.transact(device_idx, dat[index], &swd_data, 
												&buffer_reply[rep_len + 1]))
			{
				buffer_reply[rep_len] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len] = USB_TO_XXX_OK;
				SET_LE_U32(buffer_reply + rep_len + 2, swd_data);
			}
			rep_len += 1 + 1 + 4;
			break;
		default:
			buffer_reply[rep_len++] = USB_TO_XXX_CMD_NOT_SUPPORT;
			break;
		}
		index += length;
	}
}

#endif
