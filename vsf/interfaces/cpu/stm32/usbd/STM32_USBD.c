/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       BDM.c                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    BDM interface implementation file                         *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2011-05-09:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_type.h"
#include "compiler.h"
#include "interfaces.h"

// TODO: remove MACROs below to stm32_reg.h
#define STM32_RCC_CFGR_USBPRE				((uint32_t)1 << 22)

#define STM32_RCC_APB1ENR_USBEN				((uint32_t)1 << 23)

#if IFS_USBD_EN

#include "STM32_USBD.h"

#include "usb_regs.h"
#include "usb_mem.h"

#define STM32_USBD_EP_NUM					8
const uint8_t stm32_usbd_ep_num = STM32_USBD_EP_NUM;
struct interface_usbd_callback_t stm32_usbd_callback;
static uint16_t EP_Cfg_Ptr = 0x200;

uint16_t stm32_usbd_IN_epsize[STM32_USBD_EP_NUM];
uint16_t stm32_usbd_OUT_epsize[STM32_USBD_EP_NUM];
bool stm32_usbd_IN_dbuffer[STM32_USBD_EP_NUM];
bool stm32_usbd_OUT_dbuffer[STM32_USBD_EP_NUM];
int8_t stm32_usbd_epaddr[STM32_USBD_EP_NUM];

vsf_err_t stm32_usbd_init(void)
{
	struct stm32_info_t *stm32_info;
	
	memset(stm32_usbd_IN_epsize, 0, sizeof(stm32_usbd_IN_epsize));
	memset(stm32_usbd_OUT_epsize, 0, sizeof(stm32_usbd_OUT_epsize));
	memset(stm32_usbd_IN_dbuffer, 0, sizeof(stm32_usbd_IN_dbuffer));
	memset(stm32_usbd_OUT_dbuffer, 0, sizeof(stm32_usbd_OUT_dbuffer));
	memset(stm32_usbd_epaddr, -1, sizeof(stm32_usbd_epaddr));
	
	if (stm32_interface_get_info(&stm32_info))
	{
		return VSFERR_FAIL;
	}
	switch (stm32_info->sys_freq_hz)
	{
	case 72 * 1000 * 1000:
		RCC->CFGR &= ~STM32_RCC_CFGR_USBPRE;
		break;
	case 48 * 1000 * 1000:
		RCC->CFGR |= STM32_RCC_CFGR_USBPRE;
		break;
	default:
		return VSFERR_INVALID_PARAMETER;
	}
	RCC->APB1ENR |= STM32_RCC_APB1ENR_USBEN;
	
	// reset
	SetCNTR(CNTR_FRES);
	SetCNTR(0);
	
	// It seems that there MUST be at least 8 clock cycles
	// between clear FRES and clear ISTR, or RESET flash can't be cleared
	__asm("nop");
	__asm("nop");
	__asm("nop");
	__asm("nop");
	__asm("nop");
	__asm("nop");
	__asm("nop");
	SetISTR(0);
	SetCNTR(CNTR_CTRM | CNTR_WKUPM | CNTR_SUSPM | CNTR_ERRM | CNTR_RESETM);
	SetBTABLE(0);
	return VSFERR_NONE;
}

vsf_err_t stm32_usbd_fini(void)
{
	// reset
	SetCNTR(CNTR_FRES);
	SetISTR(0);
	
	SetCNTR(CNTR_FRES + CNTR_PDWN);
	return VSFERR_NONE;
}

vsf_err_t stm32_usbd_reset(void)
{
	return VSFERR_NONE;
}

void USB_Istr(void);
vsf_err_t stm32_usbd_poll(void)
{
	USB_Istr();
	return VSFERR_NONE;
}

vsf_err_t stm32_usbd_connect(void)
{
	return VSFERR_NONE;
}

vsf_err_t stm32_usbd_disconnect(void)
{
	return VSFERR_NONE;
}

vsf_err_t stm32_usbd_set_address(uint8_t address)
{
	SetDADDR(address | DADDR_EF);
	return VSFERR_NONE;
}

uint8_t stm32_usbd_get_address(void)
{
	return (_GetDADDR() & 0x7F);
}

vsf_err_t stm32_usbd_suspend(void)
{
	return VSFERR_NONE;
}

vsf_err_t stm32_usbd_resume(void)
{
	return VSFERR_NONE;
}

vsf_err_t stm32_usbd_lowpower(uint8_t level)
{
	return VSFERR_NONE;
}

uint32_t stm32_usbd_get_frame_number(void)
{
	return GetFNR() & 0x7FF;
}

vsf_err_t stm32_usbd_get_setup(uint8_t *buffer)
{
	if (8 != stm32_usbd_ep_get_OUT_count(0))
	{
		return VSFERR_FAIL;
	}
	
	return stm32_usbd_ep_read_OUT_buffer(0, buffer, 8);
}

vsf_err_t stm32_usbd_prepare_buffer(void)
{
	EP_Cfg_Ptr = 0x200;
	return VSFERR_NONE;
}

static int8_t stm32_usbd_ep(uint8_t idx)
{
	uint8_t i;
	
	for (i = 0; i < sizeof(stm32_usbd_epaddr); i++)
	{
		if (idx == stm32_usbd_epaddr[i])
		{
			return (int8_t)i;
		}
	}
	return -1;
}

static int8_t stm32_usbd_get_ep(uint8_t idx)
{
	int8_t i;
	
	i = stm32_usbd_ep(idx);
	if (i >= 0)
	{
		return i;
	}
	
	for (i = 0; i < sizeof(stm32_usbd_epaddr); i++)
	{
		if (-1 == stm32_usbd_epaddr[i])
		{
			stm32_usbd_epaddr[i] = idx;
			SetEPAddress(i, idx);
			return i;
		}
	}
	return -1;
}

vsf_err_t stm32_usbd_ep_reset(uint8_t idx)
{
	return VSFERR_NONE;
}

vsf_err_t stm32_usbd_ep_set_type(uint8_t idx, enum interface_usbd_eptype_t type)
{
	int8_t index;
	
	index = stm32_usbd_get_ep(idx);
	if (index < 0)
	{
		return VSFERR_FAIL;
	}
	idx = (uint8_t)index;
	
	switch (type)
	{
	case USB_EP_TYPE_CONTROL:
		SetEPType(idx, EP_CONTROL);
		Clear_Status_Out(idx);
		break;
	case USB_EP_TYPE_INTERRUPT:
		SetEPType(idx, EP_INTERRUPT);
		break;
	case USB_EP_TYPE_BULK:
		SetEPType(idx, EP_BULK);
		ClearEPDoubleBuff(idx);
		break;
	case USB_EP_TYPE_ISO:
		SetEPType(idx, EP_ISOCHRONOUS);
		break;
	default:
		return VSFERR_INVALID_PARAMETER;
	}
	return VSFERR_NONE;
}

vsf_err_t stm32_usbd_ep_set_IN_dbuffer(uint8_t idx)
{
	uint16_t epsize = stm32_usbd_ep_get_IN_epsize(idx);
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return VSFERR_FAIL;
	}
	idx = (uint8_t)index;
	
	if ((EP_Cfg_Ptr - epsize) < STM32_USBD_EP_NUM * 8)
	{
		return VSFERR_NOT_ENOUGH_RESOURCES;
	}
	EP_Cfg_Ptr -= epsize;
	
	SetEPDoubleBuff(idx);
	SetEPDblBuffAddr(idx, GetEPTxAddr(idx), EP_Cfg_Ptr);
	SetEPDblBuffCount(idx, EP_DBUF_IN, 0);
	ClearDTOG_RX(idx);
	ClearDTOG_TX(idx);
	SetEPRxStatus(idx, EP_RX_DIS);
	SetEPTxStatus(idx, EP_TX_NAK);
	stm32_usbd_IN_dbuffer[idx] = true;
	return VSFERR_NONE;
}

bool stm32_usbd_ep_is_IN_dbuffer(uint8_t idx)
{
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return false;
	}
	idx = (uint8_t)index;
	return stm32_usbd_IN_dbuffer[idx];
}

vsf_err_t stm32_usbd_ep_switch_IN_buffer(uint8_t idx)
{
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return VSFERR_FAIL;
	}
	idx = (uint8_t)index;
	FreeUserBuffer(idx, EP_DBUF_IN);
	return VSFERR_NONE;
}

vsf_err_t stm32_usbd_ep_set_IN_epsize(uint8_t idx, uint16_t epsize)
{
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return VSFERR_FAIL;
	}
	idx = (uint8_t)index;
	
	if ((EP_Cfg_Ptr - epsize) < STM32_USBD_EP_NUM * 8)
	{
		return VSFERR_NOT_ENOUGH_RESOURCES;
	}
	
	stm32_usbd_IN_epsize[idx] = epsize;
	SetEPTxCount(idx, epsize);
	// fix for 16-bit aligned memory
	EP_Cfg_Ptr -= epsize & 1 ? epsize + 1 : epsize;
	SetEPTxAddr(idx, EP_Cfg_Ptr);
	SetEPTxStatus(idx, EP_TX_NAK);
	return VSFERR_NONE;
}

uint16_t stm32_usbd_ep_get_IN_epsize(uint8_t idx)
{
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return 0;
	}
	idx = (uint8_t)index;
	return stm32_usbd_IN_epsize[idx];
}

vsf_err_t stm32_usbd_ep_set_IN_stall(uint8_t idx)
{
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return VSFERR_FAIL;
	}
	idx = (uint8_t)index;
	SetEPTxStatus(idx, EP_TX_STALL);
	return VSFERR_NONE;
}

vsf_err_t stm32_usbd_ep_clear_IN_stall(uint8_t idx)
{
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return VSFERR_FAIL;
	}
	idx = (uint8_t)index;
	SetEPTxStatus(idx, EP_TX_NAK);
	return VSFERR_NONE;
}

bool stm32_usbd_ep_is_IN_stall(uint8_t idx)
{
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return false;
	}
	idx = (uint8_t)index;
	
	return (EP_TX_STALL == GetEPTxStatus(idx));
}

vsf_err_t stm32_usbd_ep_reset_IN_toggle(uint8_t idx)
{
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return VSFERR_FAIL;
	}
	idx = (uint8_t)index;
	ClearDTOG_TX(idx);
	return VSFERR_NONE;
}

vsf_err_t stm32_usbd_ep_toggle_IN_toggle(uint8_t idx)
{
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return VSFERR_FAIL;
	}
	idx = (uint8_t)index;
	ToggleDTOG_TX(idx);
	return VSFERR_NONE;
}


vsf_err_t stm32_usbd_ep_set_IN_count(uint8_t idx, uint16_t size)
{
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return VSFERR_FAIL;
	}
	idx = (uint8_t)index;
	
	if (stm32_usbd_IN_dbuffer[idx])
	{
		if(GetENDPOINT(idx) & EP_DTOG_RX)
		{
			SetEPDblBuf1Count(idx, EP_DBUF_IN, size);
		}
		else
		{
			SetEPDblBuf0Count(idx, EP_DBUF_IN, size);
		}
	}
	else
	{
		SetEPTxCount(idx, size);
	}
	SetEPTxStatus(idx, EP_TX_VALID);
	return VSFERR_NONE;
}

vsf_err_t stm32_usbd_ep_write_IN_buffer(uint8_t idx, uint8_t *buffer,
										uint16_t size)
{
	uint32_t PMA_ptr;
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return VSFERR_FAIL;
	}
	idx = (uint8_t)index;
	
	if (stm32_usbd_IN_dbuffer[idx])
	{
		if(GetENDPOINT(idx) & EP_DTOG_RX)
		{
			PMA_ptr = GetEPDblBuf1Addr(idx);
		}
		else
		{
			PMA_ptr = GetEPDblBuf0Addr(idx);
		}
	}
	else
	{
		PMA_ptr = GetEPTxAddr(idx);
	}
	UserToPMABufferCopy(buffer, PMA_ptr, size);
	return VSFERR_NONE;
}

vsf_err_t stm32_usbd_ep_set_OUT_dbuffer(uint8_t idx)
{
	uint16_t epsize = stm32_usbd_ep_get_OUT_epsize(idx);
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return VSFERR_FAIL;
	}
	idx = (uint8_t)index;
	
	if ((EP_Cfg_Ptr - epsize) < STM32_USBD_EP_NUM * 8)
	{
		return VSFERR_NOT_ENOUGH_RESOURCES;
	}
	EP_Cfg_Ptr -= epsize;
	
	SetEPDoubleBuff(idx);
	SetEPDblBuffAddr(idx, GetEPRxAddr(idx), EP_Cfg_Ptr);
	SetEPDblBuffCount(idx, EP_DBUF_OUT, epsize);
	ClearDTOG_RX(idx);
	ClearDTOG_TX(idx);
	ToggleDTOG_TX(idx);
	SetEPRxStatus(idx, EP_RX_VALID);
	SetEPTxStatus(idx, EP_TX_DIS);
	stm32_usbd_OUT_dbuffer[idx] = true;
	return VSFERR_NONE;
}

bool stm32_usbd_ep_is_OUT_dbuffer(uint8_t idx)
{
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return false;
	}
	idx = (uint8_t)index;
	return stm32_usbd_OUT_dbuffer[idx];
}

vsf_err_t stm32_usbd_ep_switch_OUT_buffer(uint8_t idx)
{
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return VSFERR_FAIL;
	}
	idx = (uint8_t)index;
	FreeUserBuffer(idx, EP_DBUF_OUT);
	return VSFERR_NONE;
}

vsf_err_t stm32_usbd_ep_set_OUT_epsize(uint8_t idx, uint16_t epsize)
{
	bool ep0;
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return VSFERR_FAIL;
	}
	ep0 = 0 == idx;
	idx = (uint8_t)index;
	
	if ((EP_Cfg_Ptr - epsize) < STM32_USBD_EP_NUM * 8)
	{
		return VSFERR_NOT_ENOUGH_RESOURCES;
	}
	
	stm32_usbd_OUT_epsize[idx] = epsize;
	SetEPRxCount(idx, epsize);
	// fix for 16-bit aligned memory
	EP_Cfg_Ptr -= epsize & 1 ? epsize + 1 : epsize;
	SetEPRxAddr(idx, EP_Cfg_Ptr);
	if (ep0)
	{
		SetEPRxStatus(idx, EP_RX_VALID);
	}
	else
	{
		SetEPRxStatus(idx, EP_RX_NAK);
	}
	return VSFERR_NONE;
}

uint16_t stm32_usbd_ep_get_OUT_epsize(uint8_t idx)
{
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return 0;
	}
	idx = (uint8_t)index;
	return stm32_usbd_OUT_epsize[idx];
}

vsf_err_t stm32_usbd_ep_set_OUT_stall(uint8_t idx)
{
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return VSFERR_FAIL;
	}
	idx = (uint8_t)index;
	SetEPRxStatus(idx, EP_RX_STALL);
	return VSFERR_NONE;
}

vsf_err_t stm32_usbd_ep_clear_OUT_stall(uint8_t idx)
{
	int8_t index;
	bool ep0;
	
	index = stm32_usbd_ep(idx);
	ep0 = 0 == idx;
	if (index < 0)
	{
		return VSFERR_FAIL;
	}
	idx = (uint8_t)index;
	if (ep0)
	{
		SetEPRxStatus(idx, EP_RX_VALID);
	}
	else
	{
		SetEPRxStatus(idx, EP_RX_NAK);
	}
	return VSFERR_NONE;
}

bool stm32_usbd_ep_is_OUT_stall(uint8_t idx)
{
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return false;
	}
	idx = (uint8_t)index;
	
	return (EP_RX_STALL == GetEPRxStatus(idx));
}

vsf_err_t stm32_usbd_ep_reset_OUT_toggle(uint8_t idx)
{
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return VSFERR_FAIL;
	}
	idx = (uint8_t)index;
	ClearDTOG_RX(idx);
	return VSFERR_NONE;
}

vsf_err_t stm32_usbd_ep_toggle_OUT_toggle(uint8_t idx)
{
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return VSFERR_FAIL;
	}
	idx = (uint8_t)index;
	ToggleDTOG_RX(idx);
	return VSFERR_NONE;
}

uint16_t stm32_usbd_ep_get_OUT_count(uint8_t idx)
{
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return 0;
	}
	idx = (uint8_t)index;
	
	if (stm32_usbd_OUT_dbuffer[idx])
	{
		if(GetENDPOINT(idx) & EP_DTOG_TX)
		{
			return GetEPDblBuf1Count(idx);
		}
		else
		{
			return GetEPDblBuf0Count(idx);
		}
	}
	else
	{
		return GetEPRxCount(idx);
	}
}

vsf_err_t stm32_usbd_ep_read_OUT_buffer(uint8_t idx, uint8_t *buffer,
										uint16_t size)
{
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return VSFERR_FAIL;
	}
	idx = (uint8_t)index;
	
	if (stm32_usbd_OUT_dbuffer[idx])
	{
		if(GetENDPOINT(idx) & EP_DTOG_TX)
		{
			PMAToUserBufferCopy(buffer, GetEPDblBuf1Addr(idx), size);
		}
		else
		{
			PMAToUserBufferCopy(buffer, GetEPDblBuf0Addr(idx), size);
		}
	}
	else
	{
		PMAToUserBufferCopy(buffer, GetEPRxAddr(idx), size);
	}
	return VSFERR_NONE;
}

vsf_err_t stm32_usbd_ep_enable_OUT(uint8_t idx)
{
	int8_t index;
	
	index = stm32_usbd_ep(idx);
	if (index < 0)
	{
		return VSFERR_FAIL;
	}
	idx = (uint8_t)index;
	
	SetEPRxStatus(idx, EP_RX_VALID);
	return VSFERR_NONE;
}






void CTR_LP(void)
{
	uint8_t EPindex;
	int8_t epaddr;
	uint16_t wIstr;
	volatile uint16_t wEPVal = 0;
	
	while (((wIstr = _GetISTR()) & ISTR_CTR) != 0)
	{
		EPindex = (uint8_t)(wIstr & ISTR_EP_ID);
		epaddr = stm32_usbd_epaddr[EPindex];
		
		if (epaddr == 0)
		{
			if ((wIstr & ISTR_DIR) == 0)
			{
				_ClearEP_CTR_TX(ENDP0);
				if (stm32_usbd_callback.on_in != NULL)
				{
					stm32_usbd_callback.on_in(stm32_usbd_callback.param,
												epaddr);
				}
				return;
			}
			else
			{
				wEPVal = _GetENDPOINT(ENDP0);
				_ClearEP_CTR_RX(ENDP0);
				if ((wEPVal & EP_SETUP) != 0)
				{
					if (stm32_usbd_callback.on_setup != NULL)
					{
						stm32_usbd_callback.on_setup(stm32_usbd_callback.param);
					}
				}
				else if ((wEPVal & EP_CTR_RX) != 0)
				{
					if (stm32_usbd_callback.on_out != NULL)
					{
						stm32_usbd_callback.on_out(stm32_usbd_callback.param,
													epaddr);
					}
				}
				return;
			}
		}
		else
		{
			wEPVal = _GetENDPOINT(EPindex);
			if ((wEPVal & EP_CTR_RX) != 0)
			{
				_ClearEP_CTR_RX(EPindex);
				if ((stm32_usbd_callback.on_out != NULL) && (epaddr >= 0))
				{
					stm32_usbd_callback.on_out(stm32_usbd_callback.param,
												epaddr);
				}
			}
			if ((wEPVal & EP_CTR_TX) != 0)
			{
				_ClearEP_CTR_TX(EPindex);
				if ((stm32_usbd_callback.on_in != NULL) && (epaddr >= 0))
				{
					stm32_usbd_callback.on_in(stm32_usbd_callback.param,
												epaddr);
				}
			}
		}
	}
}

void CTR_HP(void)
{
	uint8_t EPindex;
	int8_t epaddr;
	uint16_t wIstr;
	uint32_t wEPVal = 0;
	
	while (((wIstr = _GetISTR()) & ISTR_CTR) != 0)
	{
		_SetISTR((uint16_t)CLR_CTR);
		
		EPindex = (uint8_t)(wIstr & ISTR_EP_ID);
		epaddr = stm32_usbd_epaddr[EPindex];
		wEPVal = _GetENDPOINT(EPindex);
		if ((wEPVal & EP_CTR_RX) != 0)
		{
			_ClearEP_CTR_RX(EPindex);
			if ((stm32_usbd_callback.on_out != NULL) && (epaddr >= 0))
			{
				stm32_usbd_callback.on_out(stm32_usbd_callback.param, epaddr);
			}
		}
		else if ((wEPVal & EP_CTR_TX) != 0)
		{
			_ClearEP_CTR_TX(EPindex);
			if ((stm32_usbd_callback.on_in != NULL) && (epaddr >= 0))
			{
				stm32_usbd_callback.on_in(stm32_usbd_callback.param, epaddr);
			}
		}
	}
}

void USB_Istr(void)
{
	uint16_t wIstr = _GetISTR();
	
	if (wIstr & ISTR_RESET)
	{
		_SetISTR((uint16_t)CLR_RESET);
		if (stm32_usbd_callback.on_reset != NULL)
		{
			stm32_usbd_callback.on_reset(stm32_usbd_callback.param);
		}
	}
	if (wIstr & ISTR_DOVR)
	{
		_SetISTR((uint16_t)CLR_DOVR);
	}
	if (wIstr & ISTR_ERR)
	{
		_SetISTR((uint16_t)CLR_ERR);
		if (stm32_usbd_callback.on_error != NULL)
		{
			stm32_usbd_callback.on_error(stm32_usbd_callback.param,
											USBERR_ERROR);
		}
	}
	if (wIstr & ISTR_WKUP)
	{
		_SetISTR((uint16_t)CLR_WKUP);
		if (stm32_usbd_callback.on_wakeup != NULL)
		{
			stm32_usbd_callback.on_wakeup(stm32_usbd_callback.param);
		}
	}
	if (wIstr & ISTR_SUSP)
	{
		if (stm32_usbd_callback.on_suspend != NULL)
		{
			stm32_usbd_callback.on_suspend(stm32_usbd_callback.param);
		}
		_SetISTR((uint16_t)CLR_SUSP);
	}
	if (wIstr & ISTR_SOF)
	{
		_SetISTR((uint16_t)CLR_SOF);
		if (stm32_usbd_callback.on_sof != NULL)
		{
			stm32_usbd_callback.on_sof(stm32_usbd_callback.param);
		}
	}
	if (wIstr & ISTR_ESOF)
	{
		_SetISTR((uint16_t)CLR_ESOF);
		if (stm32_usbd_callback.on_error != NULL)
		{
			stm32_usbd_callback.on_error(stm32_usbd_callback.param,
											USBERR_SOF_TO);
		}
	}
	if (wIstr & ISTR_CTR)
	{
		CTR_LP();
	}
}

ROOTFUNC void USB_LP_CAN1_RX0_IRQHandler(void)
{
	USB_Istr();
}

ROOTFUNC void USB_HP_CAN1_TX_IRQHandler(void)
{
	CTR_HP();
}

ROOTFUNC void USBWakeUp_IRQHandler(void)
{
}

#endif
