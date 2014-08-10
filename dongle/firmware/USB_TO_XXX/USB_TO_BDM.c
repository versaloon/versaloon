/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_BDM.c                                              *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_BDM                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_BDM_EN

#include "USB_TO_XXX.h"
#include "app_interfaces.h"
#include "BDM/BDM.h"

void USB_TO_BDM_ProcessCmd(uint8_t *dat, uint16_t len)
{
	uint16_t index, length;
	uint8_t command, device_idx;
	
	uint16_t token;
	uint16_t processed_len;
	uint16_t rindex;
	bool fail;
	
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
			if (app_interfaces.bdm.init(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_FINI:
			if (app_interfaces.bdm.fini(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_BDM_TRANSACT:
			rindex = rep_len++;
			fail = false;
			processed_len = 0;
			
			while (processed_len < length)
			{
				token = GET_LE_U16(&dat[index + processed_len]);
				processed_len += 2;
				
				if (app_interfaces.bdm.transact(device_idx, 
									&dat[index + processed_len], BDM_OUT_LEN(token), 
									&buffer_reply[rep_len], BDM_IN_LEN(token), 
									BDM_OUT_DLY_CNT(token), BDM_ACK(token)))
				{
					fail = true;
					break;
				}
				processed_len += BDM_OUT_LEN(token);
				rep_len += BDM_IN_LEN(token);
			}
			if (fail)
			{
				buffer_reply[rindex] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rindex] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_SYNC:
			if (app_interfaces.bdm.sync(device_idx, &processed_len))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
				SET_LE_U16(&buffer_reply[rep_len], processed_len);
			}
			rep_len += 2;
			break;
		default:
			buffer_reply[rep_len++] = USB_TO_XXX_INVALID_CMD;
			break;
		}
		index += length;
	}
}

#endif
