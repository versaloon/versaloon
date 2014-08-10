/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_JTAG_LL.c                                          *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_JTAG_LL                    *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_JTAG_LL_EN

#include "USB_TO_XXX.h"
#include "app_interfaces.h"

void USB_TO_JTAG_LL_ProcessCmd(uint8_t *dat, uint16_t len)
{
	uint16_t index, length;
	uint8_t command, device_idx;
	
	uint8_t para;
	uint32_t cur_dat_len;
	
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
			if (app_interfaces.jtag_ll.init(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_CONFIG:
			if (app_interfaces.jtag_ll.config(device_idx,
												GET_LE_U32(&dat[index])))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_FINI:
			if (app_interfaces.jtag_ll.fini(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_JTAG_LL_SCAN:
			cur_dat_len = GET_LE_U16(&dat[index]);
			para = cur_dat_len >> 15;
			cur_dat_len &= 0x7FFF;
			
			if (app_interfaces.jtag_ll.scan(device_idx, &dat[index + 2 + para],
					cur_dat_len * 8, para, dat[index + 2],
					dat[index + 2 + cur_dat_len + para],
					dat[index + 2 + cur_dat_len + para + 1]))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
				memcpy(&buffer_reply[rep_len], &dat[index + 2 + para],
						cur_dat_len);
			}
			rep_len += cur_dat_len;
			break;
		case USB_TO_JTAG_LL_TMS:
			if (app_interfaces.jtag_ll.tms(device_idx, &dat[index], length))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_JTAG_LL_TMS_CLOCKS:
			para = dat[index];
			cur_dat_len = GET_LE_U32(&dat[index + 1]);
			
			if (app_interfaces.jtag_ll.tms_clocks(device_idx, cur_dat_len,
													para))
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
