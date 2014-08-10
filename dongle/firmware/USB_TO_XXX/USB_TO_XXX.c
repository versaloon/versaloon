/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_xxx.c                                              *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_XXX                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_XXX_EN

#include "USB_TO_XXX.h"
#include "app_interfaces.h"

#if USB_TO_POLL_EN
typedef struct
{
	uint8_t *buffer_reply_save;
	uint32_t rep_len_save;
	uint16_t cmd_index;
	uint16_t poll_retry;
	uint16_t poll_interval;
	uint8_t poll_result;
} USB_TO_POLL_Context_t;
USB_TO_POLL_Context_t USB_TO_POLL_Context[USB_TO_POLL_NUM];
static const uint8_t *USB_TO_POLL_buffer_reply[USB_TO_POLL_NUM];
int8_t USB_TO_POLL_Index;
#endif

uint8_t *buffer_reply;

#define USB_TO_XXX_ABILITIES_LEN			\
	((VERSALOON_USB_TO_XXX_CMD_END + 1 - VERSALOON_USB_TO_XXX_CMD_START) / 8)

static void USB_TO_XXX_AddAbility(uint8_t abilities[USB_TO_XXX_ABILITIES_LEN], uint8_t cmd)
{
	if ((cmd < VERSALOON_USB_TO_XXX_CMD_START) || 
		(cmd > VERSALOON_USB_TO_XXX_CMD_END))
	{
		return;
	}

	cmd -= VERSALOON_USB_TO_XXX_CMD_START;
	abilities[cmd / 8] |= 1 << (cmd % 8);
}

void USB_TO_XXX_Init(uint8_t *poll_buff)
{
#if USB_TO_POLL_EN
	uint32_t i;
	
	memset(USB_TO_POLL_buffer_reply, 0, sizeof(USB_TO_POLL_buffer_reply));
	if (poll_buff != NULL)
	{
		for (i = 0; i < dimof(USB_TO_POLL_buffer_reply); i++)
		{
			USB_TO_POLL_buffer_reply[i] = poll_buff + 1024 * i;
		}
	}
#endif
}

void USB_TO_XXX_ProcessCmd(uint8_t *dat, uint16_t len)
{
	uint16_t USB_TO_XXX_CmdIdx;
	uint16_t USB_TO_XXX_CmdLen_tmp;
	uint16_t dly;
	uint8_t USB_TO_XXX_Abilities[USB_TO_XXX_ABILITIES_LEN];

	// for USB_TO_ALL command, data area(from the 3rd byte) is the real command to execute
	if(dat[0] == USB_TO_ALL)
	{
		USB_TO_XXX_CmdIdx = 3;
	}
	else
	{
		USB_TO_XXX_CmdIdx = 0;
	}

	// Poll for all the data received
	while(USB_TO_XXX_CmdIdx < len)
	{
		// get command length
		USB_TO_XXX_CmdLen_tmp = GET_LE_U16(&dat[USB_TO_XXX_CmdIdx + 1]) - 3;
		// poll command type and call different module
		switch(dat[USB_TO_XXX_CmdIdx])
		{
		case USB_TO_INFO:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			memset(USB_TO_XXX_Abilities, 0, sizeof(USB_TO_XXX_Abilities));
#if USB_TO_USART_EN
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_USART);
#endif
#if USB_TO_SPI_EN
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_SPI);
#endif
#if USB_TO_EBI_EN
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_EBI);
#endif
#if USB_TO_IIC_EN
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_IIC);
#endif
#if USB_TO_GPIO_EN
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_GPIO);
#endif
#if USB_TO_CAN_EN
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_CAN);
#endif
#if USB_TO_PWM_EN
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_PWM);
#endif
#if USB_TO_ADC_EN
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_ADC);
#endif
#if USB_TO_CLKO_EN
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_CLKO);
#endif
#if USB_TO_DAC_EN
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_DAC);
#endif
#if USB_TO_SWIM_EN
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_SWIM);
#endif
#if USB_TO_DUSI_EN
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_DUSI);
#endif
#if USB_TO_MICROWIRE_EN
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_MICROWIRE);
#endif
#if USB_TO_JTAG_LL_EN
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_JTAG_LL);
#endif
#if USB_TO_JTAG_RAW_EN
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_JTAG_RAW);
#endif
#if USB_TO_JTAG_HL_EN
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_JTAG_HL);
#endif
#if USB_TO_ISSP_EN
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_ISSP);
#endif
#if USB_TO_C2_EN
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_C2);
#endif
#if USB_TO_LPCICP_EN
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_LPCICP);
#endif
#if USB_TO_SWD_EN
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_SWD);
#endif
#if USB_TO_MSP430_JTAG_EN
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_MSP430_JTAG);
#endif
#if USB_TO_BDM_EN
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_BDM);
#endif
#if USB_TO_POWER_EN
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_POWER);
#endif
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_DELAY);
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_INFO);
			USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_ALL);
#if USB_TO_POLL_EN
			if (USB_TO_POLL_buffer_reply[0] != NULL)
			{
				USB_TO_XXX_AddAbility(USB_TO_XXX_Abilities, USB_TO_POLL);
			}
#endif
			memcpy(&buffer_reply[rep_len], USB_TO_XXX_Abilities, USB_TO_XXX_ABILITIES_LEN);
			rep_len += USB_TO_XXX_ABILITIES_LEN;
		break;
/****************************** Page0 ******************************/
#if USB_TO_USART_EN
		case USB_TO_USART:
			USB_TO_USART_ProcessCmd(dat + USB_TO_XXX_CmdIdx + 3, USB_TO_XXX_CmdLen_tmp);
			break;
#endif
#if USB_TO_SPI_EN
		case USB_TO_SPI:
			USB_TO_SPI_ProcessCmd(dat + USB_TO_XXX_CmdIdx + 3, USB_TO_XXX_CmdLen_tmp);
			break;
#endif
#if USB_TO_EBI_EN
		case USB_TO_EBI:
			USB_TO_EBI_ProcessCmd(dat + USB_TO_XXX_CmdIdx + 3, USB_TO_XXX_CmdLen_tmp);
			break;
#endif
#if USB_TO_IIC_EN
		case USB_TO_IIC:
			USB_TO_IIC_ProcessCmd(dat + USB_TO_XXX_CmdIdx + 3, USB_TO_XXX_CmdLen_tmp);
			break;
#endif
#if USB_TO_GPIO_EN
		case USB_TO_GPIO:
			USB_TO_GPIO_ProcessCmd(dat + USB_TO_XXX_CmdIdx + 3, USB_TO_XXX_CmdLen_tmp);
			break;
#endif
#if USB_TO_CAN_EN
		case USB_TO_CAN:
			USB_TO_CAN_ProcessCmd(dat + USB_TO_XXX_CmdIdx + 3, USB_TO_XXX_CmdLen_tmp);
			break;
#endif
#if USB_TO_PWM_EN
		case USB_TO_PWM:
			USB_TO_PWM_ProcessCmd(dat + USB_TO_XXX_CmdIdx + 3, USB_TO_XXX_CmdLen_tmp);
			break;
#endif
#if USB_TO_ADC_EN
		case USB_TO_ADC:
			USB_TO_ADC_ProcessCmd(dat + USB_TO_XXX_CmdIdx + 3, USB_TO_XXX_CmdLen_tmp);
			break;
#endif
#if USB_TO_CLKO_EN
		case USB_TO_CLKO:
			USB_TO_CLKO_ProcessCmd(dat + USB_TO_XXX_CmdIdx + 3, USB_TO_XXX_CmdLen_tmp);
			break;
#endif
#if USB_TO_DAC_EN
		case USB_TO_DAC:
			USB_TO_DAC_ProcessCmd(dat + USB_TO_XXX_CmdIdx + 3, USB_TO_XXX_CmdLen_tmp);
			break;
#endif
#if USB_TO_SWIM_EN
		case USB_TO_SWIM:
			USB_TO_SWIM_ProcessCmd(dat + USB_TO_XXX_CmdIdx + 3, USB_TO_XXX_CmdLen_tmp);
			break;
#endif
#if USB_TO_DUSI_EN
		case USB_TO_DUSI:
			USB_TO_DUSI_ProcessCmd(dat + USB_TO_XXX_CmdIdx + 3, USB_TO_XXX_CmdLen_tmp);
			break;
#endif
#if USB_TO_MICROWIRE_EN
		case USB_TO_MICROWIRE:
			USB_TO_MICROWIRE_ProcessCmd(dat + USB_TO_XXX_CmdIdx + 3, USB_TO_XXX_CmdLen_tmp);
			break;
#endif

/****************************** Page1 ******************************/
#if USB_TO_JTAG_LL_EN
		case USB_TO_JTAG_LL:
			USB_TO_JTAG_LL_ProcessCmd(dat + USB_TO_XXX_CmdIdx + 3, USB_TO_XXX_CmdLen_tmp);
			break;
#endif
#if USB_TO_JTAG_HL_EN
		case USB_TO_JTAG_HL:
			USB_TO_JTAG_HL_ProcessCmd(dat + USB_TO_XXX_CmdIdx + 3, USB_TO_XXX_CmdLen_tmp);
			break;
#endif
#if USB_TO_JTAG_RAW_EN
		case USB_TO_JTAG_RAW:
			USB_TO_JTAG_RAW_ProcessCmd(dat + USB_TO_XXX_CmdIdx + 3, USB_TO_XXX_CmdLen_tmp);
			break;
#endif
#if USB_TO_ISSP_EN
		case USB_TO_ISSP:
			USB_TO_ISSP_ProcessCmd(dat + USB_TO_XXX_CmdIdx + 3, USB_TO_XXX_CmdLen_tmp);
			break;
#endif
#if USB_TO_C2_EN
		case USB_TO_C2:
			USB_TO_C2_ProcessCmd(dat + USB_TO_XXX_CmdIdx + 3, USB_TO_XXX_CmdLen_tmp);
			break;
#endif
#if USB_TO_LPCICP_EN
		case USB_TO_LPCICP:
			USB_TO_LPCICP_ProcessCmd(dat + USB_TO_XXX_CmdIdx + 3, USB_TO_XXX_CmdLen_tmp);
			break;
#endif
#if USB_TO_SWD_EN
		case USB_TO_SWD:
			USB_TO_SWD_ProcessCmd(dat + USB_TO_XXX_CmdIdx + 3, USB_TO_XXX_CmdLen_tmp);
			break;
#endif
#if USB_TO_MSP430_JTAG_EN
		case USB_TO_MSP430_JTAG:
			USB_TO_MSP430_JTAG_ProcessCmd(dat + USB_TO_XXX_CmdIdx + 3, USB_TO_XXX_CmdLen_tmp);
			break;
#endif
#if USB_TO_BDM_EN
		case USB_TO_BDM:
			USB_TO_BDM_ProcessCmd(dat + USB_TO_XXX_CmdIdx + 3, USB_TO_XXX_CmdLen_tmp);
			break;
#endif

/****************************** Page2 ******************************/
#if USB_TO_POWER_EN
		case USB_TO_POWER:
			USB_TO_POWER_ProcessCmd(dat + USB_TO_XXX_CmdIdx + 3, USB_TO_XXX_CmdLen_tmp);
			break;
#endif
		case USB_TO_DELAY:
			dly = GET_LE_U16(&dat[USB_TO_XXX_CmdIdx + 3]);

			if(dly & 0x8000)
			{
				app_interfaces.delay.delayms(dly & 0x7FFF);
			}
			else
			{
				app_interfaces.delay.delayus(dly);
			}

			buffer_reply[rep_len++] = USB_TO_XXX_OK;
			break;
#if USB_TO_POLL_EN
		case USB_TO_POLL:
			switch(dat[USB_TO_XXX_CmdIdx + 3])
			{
			case USB_TO_POLL_START:
				USB_TO_POLL_Index++;
				if ((USB_TO_POLL_Index >= USB_TO_POLL_NUM) || (USB_TO_POLL_Index < 0))
				{
					buffer_reply[rep_len++] = USB_TO_XXX_INVALID_INDEX;
					return;
				}
				else
				{
					buffer_reply[rep_len++] = USB_TO_XXX_OK;

					USB_TO_POLL_Context[USB_TO_POLL_Index].buffer_reply_save = buffer_reply;
					USB_TO_POLL_Context[USB_TO_POLL_Index].rep_len_save = rep_len;
					USB_TO_POLL_Context[USB_TO_POLL_Index].cmd_index = 
						USB_TO_XXX_CmdIdx + USB_TO_XXX_CmdLen_tmp + 3;
					USB_TO_POLL_Context[USB_TO_POLL_Index].poll_retry = 
						GET_LE_U16(&dat[USB_TO_XXX_CmdIdx + 4]);
					USB_TO_POLL_Context[USB_TO_POLL_Index].poll_interval = 
						GET_LE_U16(&dat[USB_TO_XXX_CmdIdx + 6]);
					USB_TO_POLL_Context[USB_TO_POLL_Index].poll_result = 1;

					buffer_reply = (uint8_t *)USB_TO_POLL_buffer_reply[USB_TO_POLL_Index];
					rep_len = 0;
				}
				break;
			case USB_TO_POLL_END:
				if (USB_TO_POLL_Index < 0)
				{
					buffer_reply[rep_len++] = USB_TO_XXX_INVALID_CMD;
					return;
				}
				else if (USB_TO_POLL_Index >= USB_TO_POLL_NUM)
				{
					buffer_reply[rep_len++] = USB_TO_XXX_INVALID_INDEX;
					USB_TO_POLL_Index--;
					return;
				}
				else
				{
					if (USB_TO_POLL_Context[USB_TO_POLL_Index].poll_result)
					{
						// poll success
						memcpy(USB_TO_POLL_Context[USB_TO_POLL_Index].buffer_reply_save + 
							   USB_TO_POLL_Context[USB_TO_POLL_Index].rep_len_save, 
							   buffer_reply, rep_len);

						buffer_reply = USB_TO_POLL_Context[USB_TO_POLL_Index].buffer_reply_save;
						rep_len += USB_TO_POLL_Context[USB_TO_POLL_Index].rep_len_save;

						buffer_reply[rep_len++] = USB_TO_XXX_OK;
						USB_TO_POLL_Index--;
					}
					else
					{
						// poll fail
						if (!USB_TO_POLL_Context[USB_TO_POLL_Index].poll_retry)
						{
							// timeout
							memcpy(USB_TO_POLL_Context[USB_TO_POLL_Index].buffer_reply_save + 
								   USB_TO_POLL_Context[USB_TO_POLL_Index].rep_len_save, 
								   buffer_reply, rep_len);

							buffer_reply = USB_TO_POLL_Context[USB_TO_POLL_Index].buffer_reply_save;
							rep_len += USB_TO_POLL_Context[USB_TO_POLL_Index].rep_len_save;

							buffer_reply[rep_len++] = USB_TO_XXX_TIME_OUT;
							USB_TO_POLL_Index--;
							return;
						}
						else
						{
							// retry
							USB_TO_POLL_Context[USB_TO_POLL_Index].poll_retry--;
							if (USB_TO_POLL_Context[USB_TO_POLL_Index].poll_interval)
							{
								dly = USB_TO_POLL_Context[USB_TO_POLL_Index].poll_interval;
								if(dly & 0x8000)
								{
									app_interfaces.delay.delayms(dly & 0x7FFF);
								}
								else
								{
									app_interfaces.delay.delayus(dly);
								}
							}

							USB_TO_POLL_Context[USB_TO_POLL_Index].poll_result = 1;

							buffer_reply = (uint8_t *)USB_TO_POLL_buffer_reply[USB_TO_POLL_Index];
							rep_len = 0;
							USB_TO_XXX_CmdIdx = USB_TO_POLL_Context[USB_TO_POLL_Index].cmd_index - USB_TO_XXX_CmdLen_tmp - 3;
						}
					}
				}
				break;
			case USB_TO_POLL_CHECKOK:
				if (USB_TO_POLL_Index < USB_TO_POLL_NUM)
				{
					uint8_t equ;
					uint8_t size, i;
					uint16_t offset;
					uint32_t mask, value, data;

					offset = GET_LE_U16(&dat[USB_TO_XXX_CmdIdx + 4]);
					size = dat[USB_TO_XXX_CmdIdx + 6];
					if (size > 4)
					{
						size = 4;
					}
					equ = dat[USB_TO_XXX_CmdIdx + 7];
					mask = 0;
					value = 0;
					for (i = 0; i < size; i++)
					{
						mask += dat[USB_TO_XXX_CmdIdx + 8 + i] << (8 * i);
						value += dat[USB_TO_XXX_CmdIdx + 8 + size + i] << (8 * i);
					}
					data = 0;
					for (i = 0; i < size; i++)
					{
						data += buffer_reply[rep_len - 1 - offset + i] << (8 * i);
					}
					if ((equ && !((data & mask) == value)) 
						|| (!equ && ((data & mask) == value)))
					{
						USB_TO_POLL_Context[USB_TO_POLL_Index].poll_result = 0;
					}
				}
				break;
			case USB_TO_POLL_CHECKFAIL:
				if (USB_TO_POLL_Index < USB_TO_POLL_NUM)
				{
					uint8_t equ;
					uint8_t size, i;
					uint16_t offset;
					uint32_t mask, value, data;

					offset = GET_LE_U16(&dat[USB_TO_XXX_CmdIdx + 4]);
					size = dat[USB_TO_XXX_CmdIdx + 6];
					if (size > 4)
					{
						size = 4;
					}
					equ = dat[USB_TO_XXX_CmdIdx + 7];
					mask = 0;
					value = 0;
					for (i = 0; i < size; i++)
					{
						mask += dat[USB_TO_XXX_CmdIdx + 8 + i] << (8 * i);
						value += dat[USB_TO_XXX_CmdIdx + 8 + size + i] << (8 * i);
					}
					data = 0;
					for (i = 0; i < size; i++)
					{
						data += buffer_reply[rep_len - 1 - offset + i] << (8 * i);
					}
					if ((equ && ((data & mask) == value)) 
						|| (!equ && !((data & mask) == value)))
					{
						USB_TO_POLL_Context[USB_TO_POLL_Index].poll_result = 0;
						USB_TO_POLL_Context[USB_TO_POLL_Index].poll_retry = 0;
					}
				}
				break;
			case USB_TO_POLL_VERIFYBUFF:
				if (USB_TO_POLL_Index < USB_TO_POLL_NUM)
				{
					uint16_t size, offset;

					offset = GET_LE_U16(&dat[USB_TO_XXX_CmdIdx + 4]);
					size = GET_LE_U16(&dat[USB_TO_XXX_CmdIdx + 6]);
					if (memcmp(&buffer_reply[rep_len - 1 - offset], &dat[USB_TO_XXX_CmdIdx + 8], size))
					{
						USB_TO_POLL_Context[USB_TO_POLL_Index].poll_result = 0;
					}
				}
				break;
			default:
				break;
			}
			break;
#endif
		default:
			buffer_reply[rep_len++] = USB_TO_XXX_CMD_NOT_SUPPORT;
			break;
		}

		// add command length to the USB_TO_XXX_CmdIdx
		// 3 more bytes are command char(1 byte), command length(2 bytes)
		USB_TO_XXX_CmdIdx += USB_TO_XXX_CmdLen_tmp + 3;
	}
}

#endif		// #if USB_TO_XXX_EN
