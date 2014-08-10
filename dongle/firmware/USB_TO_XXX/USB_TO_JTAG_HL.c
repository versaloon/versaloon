/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_JTAG_HL.c                                          *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_JTAG_HL                    *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_JTAG_HL_EN

#include "USB_TO_XXX.h"
#include "app_interfaces.h"

void USB_TO_JTAG_HL_ProcessCmd(uint8_t *dat, uint16_t len)
{
	uint16_t index, length;
	uint8_t command, device_idx;
	
	struct jtag_pos_t jtag_pos;
	uint32_t jtag_khz;
	uint16_t cur_dat_len, i, len_tmp;
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
			if (app_interfaces.jtag_hl.init(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_CONFIG:
			jtag_khz = GET_LE_U32(&dat[index]);
			jtag_pos.ub = dat[index + 4];
			jtag_pos.ua = dat[index + 5];
			jtag_pos.bb = GET_LE_U16(&dat[index + 6]);
			jtag_pos.ba = GET_LE_U16(&dat[index + 8]);
			if (app_interfaces.jtag_hl.config(device_idx, jtag_khz, &jtag_pos))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_FINI:
			if (app_interfaces.jtag_hl.fini(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_JTAG_HL_IR_DR:
			fail = false;
			rindex = rep_len++;
			len_tmp = 0;
			
			while(len_tmp < length)
			{
				i = GET_LE_U16(&dat[index + len_tmp]);				// in bit
				cur_dat_len = i & 0x7FFF;
				
				if(i & 0x8000)
				{
					if (app_interfaces.jtag_hl.ir(device_idx,
							&dat[index + len_tmp + 3], cur_dat_len, 
							dat[index + len_tmp + 2], 1))
					{
						fail = true;
						break;
					}
					else
					{
						memcpy(&buffer_reply[rep_len],
								&dat[index + len_tmp + 3],
								(cur_dat_len + 7) >> 3);
					}
				}
				else
				{
					if (app_interfaces.jtag_hl.dr(device_idx,
							&dat[index + len_tmp + 3], cur_dat_len,
							dat[index + len_tmp + 2], 1))
					{
						fail = true;
						break;
					}
					else
					{
						memcpy(&buffer_reply[rep_len],
								&dat[index + len_tmp + 3],
								(cur_dat_len + 7) >> 3);
					}
				}
				
				cur_dat_len = (cur_dat_len + 7) >> 3;
				rep_len += cur_dat_len;
				len_tmp += cur_dat_len + 3;
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
		case USB_TO_JTAG_HL_TMS:
			if (app_interfaces.jtag_hl.tms(device_idx, &dat[index + 1],
											dat[index + 0] + 1))
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
