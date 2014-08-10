/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_ADC.c                                              *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_ADC                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_ADC_EN

#include "USB_TO_XXX.h"
#include "app_interfaces.h"

void USB_TO_ADC_ProcessCmd(uint8_t *dat, uint16_t len)
{
	uint16_t index, length;
	uint8_t command, device_idx;
	
	uint32_t clock_hz;
	uint8_t mode;
	uint8_t channel;
	uint8_t cycles;
	uint32_t adc_result;
	
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
				interfaces->adc.init(0);
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
				clock_hz = GET_LE_U32(&dat[index]);
				mode = dat[index + 4];
				interfaces->adc.config(0, clock_hz, mode);
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
				interfaces->adc.fini(0);
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_IN_OUT:
			if (0 == device_idx)
			{
				channel = dat[index + 0];
				interfaces->adc.sample(0, channel, &adc_result);
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
				SET_LE_U32(&buffer_reply[rep_len], adc_result);
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			rep_len += 4;
			break;
		case USB_TO_XXX_OUT:
			if (0 == device_idx)
			{
				channel = dat[index + 0];
				cycles = dat[index + 1];
				interfaces->adc.config_channel(0, channel, cycles);
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_SPECIAL:
			if (0 == device_idx)
			{
				channel = dat[index + 0];
				interfaces->adc.calibrate(0, channel);
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
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
