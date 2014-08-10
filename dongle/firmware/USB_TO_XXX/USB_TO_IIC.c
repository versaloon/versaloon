/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_IIC.c                                              *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_IIC                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_IIC_EN

#include "USB_TO_XXX.h"
#include "app_interfaces.h"

void USB_TO_IIC_ProcessCmd(uint8_t *dat, uint16_t len)
{
	uint16_t index, length;
	uint8_t command, device_idx;
	
	uint8_t chip_addr, stop;
	uint16_t data_len;
	bool nacklast;
	
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
			if (app_interfaces.i2c.init(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_CONFIG:
			if (app_interfaces.i2c.config(device_idx,
					GET_LE_U16(&dat[index + 0]), GET_LE_U16(&dat[index + 2]),
					GET_LE_U16(&dat[index + 4])))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_FINI:
			if (app_interfaces.i2c.fini(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_IIC_Read:
			chip_addr = dat[index + 0];
			data_len = GET_LE_U16(&dat[index + 1]);
			stop = dat[index + 3];
			nacklast = dat[index + 4] > 0;
			
			if (app_interfaces.i2c.read(device_idx, chip_addr, 
						&buffer_reply[rep_len + 1], data_len, stop, nacklast))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			rep_len += data_len;
			break;
		case USB_TO_IIC_Write:
			if (app_interfaces.i2c.write(device_idx, dat[index + 0], 
					&dat[index + 4], GET_LE_U16(&dat[index + 1]), 
					dat[index + 3]))
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
