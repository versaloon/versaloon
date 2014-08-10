/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_USART.c                                            *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_USART                      *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_USART_EN

#include "USB_TO_XXX.h"
#include "app_interfaces.h"

void USB_TO_USART_ProcessCmd(uint8_t *dat, uint16_t len)
{
	uint16_t index, length;
	uint8_t command, device_idx;
	
	uint32_t data_len;
	uint32_t baudrate;
	uint8_t datalength, paritybit, stopbit;
	struct usart_status_t status;
	
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
			if (app_interfaces.usart.init(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_CONFIG:
			baudrate = GET_LE_U32(&dat[index]);
			datalength = dat[index + 4];
			paritybit = dat[index + 5];
			stopbit = dat[index + 6];
			
			if (app_interfaces.usart.config(device_idx, baudrate, 
								datalength, paritybit, stopbit, 0))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_FINI:
			if (app_interfaces.usart.fini(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_IN:
			data_len = GET_LE_U16(&dat[index]);
			
			if (app_interfaces.usart.receive(device_idx, &buffer_reply[rep_len + 1], data_len))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
				rep_len += data_len;
			}
			break;
		case USB_TO_XXX_OUT:
			data_len = GET_LE_U16(&dat[index]);
			
			if (app_interfaces.usart.send(device_idx, &dat[index + 2], data_len))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_STATUS:
			if (app_interfaces.usart.status(device_idx, &status))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
				
				SET_LE_U32(&buffer_reply[rep_len +  0], status.tx_buff_avail);
				SET_LE_U32(&buffer_reply[rep_len +  4], status.tx_buff_size);
				SET_LE_U32(&buffer_reply[rep_len +  8], status.rx_buff_avail);
				SET_LE_U32(&buffer_reply[rep_len + 12], status.rx_buff_size);
			}
			rep_len += 16;
			break;
		default:
			buffer_reply[rep_len++] = USB_TO_XXX_CMD_NOT_SUPPORT;
			break;
		}
		index += length;
	}
}

#endif
