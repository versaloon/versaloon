/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_MSP430_JTAG.c                                      *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_MSP430_JTAG                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_MSP430_JTAG_EN

#include "USB_TO_XXX.h"
#include "app_interfaces.h"

void USB_TO_MSP430_JTAG_ProcessCmd(uint8_t *dat, uint16_t len)
{
	uint16_t index, length;
	uint8_t command, device_idx;
	
	uint32_t data, mask, value;
	uint16_t len_tmp;
	uint8_t byte_len, bit_len;
	bool fail;
	uint16_t rindex;
	
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
			if (app_interfaces.msp430jtag.init(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_CONFIG:
			if (app_interfaces.msp430jtag.config(device_idx, dat[index]))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_FINI:
			if (app_interfaces.msp430jtag.fini(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_MSP430_JTAG_TCLK:
			if (app_interfaces.msp430jtag.tclk(device_idx, dat[index]))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_MSP430_JTAG_IRDR:
			fail = false;
			len_tmp = 0;
			rindex = rep_len++;
			
			while(len_tmp < length)
			{
				bit_len = dat[index + len_tmp + 0];
				byte_len = ((bit_len & 0x7F) + 7) >> 3;
				
				if(bit_len & 0x80)
				{
					// DR
					data = 0;
					memcpy((uint8_t*)&data, dat + index + len_tmp + 1,
							byte_len);
					
					if (app_interfaces.msp430jtag.dr(device_idx, &data,
														bit_len & 0x7F, 1))
					{
						fail = true;
						break;
					}
					else
					{
						memcpy(buffer_reply + rep_len, &data, byte_len);
					}
					rep_len += byte_len;
					len_tmp += 1 + byte_len;
				}
				else
				{
					// IR
					if (app_interfaces.msp430jtag.ir(device_idx,
												&dat[index + len_tmp + 1], 1))
					{
						fail = true;
						break;
					}
					else
					{
						buffer_reply[rep_len++] = dat[index + len_tmp + 1];
					}
					len_tmp += 2;
				}
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
		case USB_TO_MSP430_JTAG_Poll:
			len_tmp = GET_LE_U16(&dat[index + 1]);
			data = GET_LE_U32(&dat[index + 3]);
			mask = GET_LE_U32(&dat[index + 7]);
			value = GET_LE_U32(&dat[index + 11]);
			
			if (app_interfaces.msp430jtag.poll(device_idx, data, mask, value, 
					dat[index], len_tmp & 0x7FFF, (len_tmp & 0x8000) > 0))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_MSP430_JTAG_TCLK_STROBE:
			if (app_interfaces.msp430jtag.tclk_strobe(device_idx,
													GET_LE_U16(&dat[index])))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_MSP430_JTAG_Reset:
			if (app_interfaces.msp430jtag.reset(device_idx))
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
