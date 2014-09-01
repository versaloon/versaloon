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
#define STM32_RCC_CFGR_USBPRE			((uint32_t)1 << 22)
#define STM32_RCC_APB1ENR_USBEN			((uint32_t)1 << 23)

// USB registers
#define RegBase							0x40005C00L
#define PMAAddr							0x40006000L
#define CNTR							((volatile unsigned *)(RegBase + 0x40))
#define ISTR							((volatile unsigned *)(RegBase + 0x44))
#define FNR								((volatile unsigned *)(RegBase + 0x48))
#define DADDR							((volatile unsigned *)(RegBase + 0x4C))
#define BTABLE							((volatile unsigned *)(RegBase + 0x50))
#define EP0REG							((volatile unsigned *)(RegBase))
#	define EPREG_MASK					(USB_EP0R_CTR_RX | USB_EP0R_SETUP | USB_EP0R_EP_TYPE |\
											USB_EP0R_EP_KIND | USB_EP0R_CTR_TX | USB_EP0R_EA)
#	define GetEPReg(ep)					(EP0REG[(ep)] & EPREG_MASK)
#	define SetEPReg(ep, value)			(EP0REG[(ep)] = (value) | USB_EP0R_CTR_RX | USB_EP0R_CTR_TX)
#		define ClearEP_CTR_RX(ep)		(EP0REG[(ep)] = (GetEPReg(ep) & ~USB_EP0R_CTR_RX) | USB_EP0R_CTR_TX)
#		define ToggleDTOG_RX(ep)		SetEPReg((ep), GetEPReg(ep) | USB_EP0R_DTOG_RX)
#		define ClearDTOG_RX(ep)			\
			do {\
				if((EP0REG[(ep)] & USB_EP0R_DTOG_RX) != 0) ToggleDTOG_RX(ep);\
			} while (0)

//#	define USB_EP0R_STAT_RX				0x3000
#		define USB_EPRX_STAT_DIS		0x0000
#		define USB_EPRX_STAT_STALL		0x1000
#		define USB_EPRX_STAT_NAK		0x2000
#		define USB_EPRX_STAT_VALID		0x3000
#		define USB_EPRX_STAT_DTOG1		0x1000
#		define USB_EPRX_STAT_DTOG2		0x2000
//#	define USB_EP0R_EP_TYPE				0x0600
#		define EP_TYPE_MASK				0x0600
#		define EP_BULK					0x0000
#		define EP_CONTROL				0x0200
#		define EP_ISOCHRONOUS			0x0400
#		define EP_INTERRUPT				0x0600

#		define SetEP_KIND(ep)			SetEPReg((ep), GetEPReg(ep) | USB_EP0R_EP_KIND)
#		define ClearEP_KIND(ep)			SetEPReg((ep), GetEPReg(ep) & ~USB_EP0R_EP_KIND)
#		define ClearStatusOut(ep)		ClearEP_KIND(ep)
#		define ClearEPDoubleBuff(ep)	ClearEP_KIND(ep)
#		define SetEPDoubleBuff(ep)		SetEP_KIND(ep)
#		define ClearEP_CTR_TX(ep)		(EP0REG[(ep)] = (GetEPReg(ep) & ~USB_EP0R_CTR_TX) | USB_EP0R_CTR_RX)
#		define ToggleDTOG_TX(ep)		SetEPReg((ep), GetEPReg(ep) | USB_EP0R_DTOG_TX)
#		define ClearDTOG_TX(ep)			\
			do {\
				if((EP0REG[(ep)] & USB_EP0R_DTOG_TX) != 0) ToggleDTOG_TX(ep);\
			} while (0)

//#	define USB_EP0R_STAT_TX				0x0030
#		define USB_EPTX_STAT_DIS		0x0000
#		define USB_EPTX_STAT_STALL		0x0010
#		define USB_EPTX_STAT_NAK		0x0020
#		define USB_EPTX_STAT_VALID		0x0030
#		define USB_EPTX_STAT_DTOG1		0x0010
#		define USB_EPTX_STAT_DTOG2		0x0020

#	define SetEPAddress(ep, addr)		SetEPReg((ep), GetEPReg(ep) | (addr))
#	define SetEPType(ep, type)			SetEPReg((ep), ((GetEPReg(ep) & ~EP_TYPE_MASK) | (type)))

// endpoint buffer
#	define _pEPTxAddr(ep)				((uint32_t *)(((uint16_t)*BTABLE + (ep) * 8 + 0) * 2 + PMAAddr))
#	define _pEPTxCount(ep)				((uint32_t *)(((uint16_t)*BTABLE + (ep) * 8 + 2) * 2 + PMAAddr))
#	define _pEPRxAddr(ep)				((uint32_t *)(((uint16_t)*BTABLE + (ep) * 8 + 4) * 2 + PMAAddr))
#	define _pEPRxCount(ep)				((uint32_t *)(((uint16_t)*BTABLE + (ep) * 8 + 6) * 2 + PMAAddr))

#	define GetEPTxAddr(ep)				((uint16_t)*_pEPTxAddr(ep))
#	define GetEPRxAddr(ep)				((uint16_t)*_pEPRxAddr(ep))
#	define GetEPDblBuf0Addr(ep)			GetEPTxAddr(ep)
#	define GetEPDblBuf1Addr(ep)			GetEPRxAddr(ep)

#define SetEPCountRxReg(reg, count)	\
		do {\
			register uint16_t block;\
			if((count) > 62) {\
				block = (((count) >> 1) + ((count) & 1 ? 1 : 0)) << 10;\
			} else {\
				block = ((((count) >> 5) - ((count) & 0x1F ? 0 : 1)) << 10) | 0x8000;\
			}\
			*(reg) = block;\
		} while (0)
#define SetEPTxCount(ep, count)			(*_pEPTxCount(ep) = (count))
#define SetEPRxCount(ep, count)			SetEPCountRxReg(_pEPRxCount(ep), (count))

#define SetEPTxAddr(ep, addr)			(*_pEPTxAddr(ep) = (((addr) >> 1) << 1))
#define SetEPRxAddr(ep, addr)			(*_pEPRxAddr(ep) = (((addr) >> 1) << 1))

#define SetEPDblBuf0Addr(ep, buf0addr)	SetEPTxAddr(ep, buf0addr)
#define SetEPDblBuf1Addr(ep, buf1addr)	SetEPRxAddr(ep, buf1addr)
#define SetEPDblBuffAddr(ep, buf0addr, buf1addr)\
			do {\
				SetEPDblBuf0Addr(ep, buf0addr);\
				SetEPDblBuf1Addr(ep, buf1addr);\
			} while (0)

#define GetEPTxCount(ep)				((uint16_t)(*_pEPTxCount(ep)) & 0x3ff)
#define GetEPRxCount(ep)				((uint16_t)(*_pEPRxCount(ep)) & 0x3ff)
#define GetEPDblBuf0Count(ep)			GetEPTxCount(ep)
#define GetEPDblBuf1Count(ep)			GetEPRxCount(ep)

#define SetEPDblBuf0Count(ep, in, count)\
		do {\
			if (in)\
				*_pEPTxCount(ep) = (uint32_t)(count);\
			else\
				SetEPCountRxReg(_pEPTxCount(ep), (count));\
		} while (0)
#define SetEPDblBuf1Count(ep, in, count)\
		do {\
			if (in)\
				*_pEPRxCount(ep) = (uint32_t)(count);\
			else\
				SetEPRxCount((ep), (count));\
		} while (0)
#define SetEPDblBuffCount(ep, in, count)\
		do {\
			SetEPDblBuf0Count((ep), (in), (count));\
			SetEPDblBuf1Count((ep), (in), (count));\
		} while (0)

#define SetEPTxStatus(ep, state)		\
		do {\
			register uint16_t _wRegVal = EP0REG[(ep)] & (EPREG_MASK | USB_EP0R_STAT_TX);\
			if((USB_EPTX_STAT_DTOG1 & state) != 0)\
				_wRegVal ^= USB_EPTX_STAT_DTOG1;\
			if((USB_EPTX_STAT_DTOG2 & state)!= 0)\
				_wRegVal ^= USB_EPTX_STAT_DTOG2;\
			SetEPReg(ep, _wRegVal);\
		} while (0)

#define SetEPRxStatus(ep, state)		\
		do {\
			register uint16_t _wRegVal = EP0REG[(ep)] & (EPREG_MASK | USB_EP0R_STAT_RX);\
			if((USB_EPRX_STAT_DTOG1 & state)!= 0) \
				_wRegVal ^= USB_EPRX_STAT_DTOG1;  \
			if((USB_EPRX_STAT_DTOG2 & state)!= 0) \
				_wRegVal ^= USB_EPRX_STAT_DTOG2;  \
			SetEPReg(ep, _wRegVal);\
		} while (0)

#define FreeUserBuffer(ep, in)			\
			do {\
				if (in)\
					ToggleDTOG_RX(ep);\
				else\
					ToggleDTOG_TX(ep);\
			} while (0)

#if IFS_USBD_EN

#include "STM32_USBD.h"

#define STM32_USBD_EP_NUM					8
const uint8_t stm32_usbd_ep_num = STM32_USBD_EP_NUM;
struct interface_usbd_callback_t stm32_usbd_callback;
static uint16_t EP_Cfg_Ptr = 0x200;

uint16_t stm32_usbd_IN_epsize[STM32_USBD_EP_NUM];
uint16_t stm32_usbd_OUT_epsize[STM32_USBD_EP_NUM];
bool stm32_usbd_IN_dbuffer[STM32_USBD_EP_NUM];
bool stm32_usbd_OUT_dbuffer[STM32_USBD_EP_NUM];
int8_t stm32_usbd_epaddr[STM32_USBD_EP_NUM];

vsf_err_t stm32_usbd_init(uint32_t int_priority)
{
	uint8_t irqn;
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
	
	irqn = USB_HP_CAN1_TX_IRQn;
	NVIC->IP[irqn] = int_priority;
	NVIC->ISER[irqn >> 0x05] = 1UL << (irqn & 0x1F);
	irqn = USB_LP_CAN1_RX0_IRQn;
	NVIC->IP[irqn] = int_priority;
	NVIC->ISER[irqn >> 0x05] = 1UL << (irqn & 0x1F);
	
	// reset
	*CNTR = USB_CNTR_FRES;
	*CNTR = 0;
	
	// It seems that there MUST be at least 8 clock cycles
	// between clear FRES and clear ISTR, or RESET flash can't be cleared
	__asm("nop");
	__asm("nop");
	__asm("nop");
	__asm("nop");
	__asm("nop");
	__asm("nop");
	__asm("nop");
	*ISTR = 0;
	*CNTR = USB_CNTR_CTRM | USB_CNTR_WKUPM | USB_CNTR_SUSPM | USB_CNTR_ERRM |
				USB_CNTR_RESETM;
	*BTABLE = 0;
	return VSFERR_NONE;
}

vsf_err_t stm32_usbd_fini(void)
{
	uint8_t irqn;
	// reset
	*CNTR = USB_CNTR_FRES;
	*ISTR = 0;
	
	*CNTR = USB_CNTR_FRES | USB_CNTR_PDWN;
	irqn = USB_HP_CAN1_TX_IRQn;
	NVIC->ICER[irqn >> 0x05] = 1UL << (irqn & 0x1F);
	irqn = USB_LP_CAN1_RX0_IRQn;
	NVIC->ICER[irqn >> 0x05] = 1UL << (irqn & 0x1F);
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
	*DADDR = address | USB_DADDR_EF;
	return VSFERR_NONE;
}

uint8_t stm32_usbd_get_address(void)
{
	return *DADDR & USB_DADDR_ADD;
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
	return *FNR & 0x7FF;
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
		ClearStatusOut(idx);
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
	SetEPDblBuffCount(idx, true, 0);
	ClearDTOG_RX(idx);
	ClearDTOG_TX(idx);
	SetEPRxStatus(idx, USB_EPRX_STAT_DIS);
	SetEPTxStatus(idx, USB_EPTX_STAT_NAK);
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
	FreeUserBuffer(idx, true);
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
	SetEPTxStatus(idx, USB_EPTX_STAT_NAK);
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
	SetEPTxStatus(idx, USB_EPTX_STAT_STALL);
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
	SetEPTxStatus(idx, USB_EPTX_STAT_NAK);
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
	
	return (USB_EPTX_STAT_STALL == (EP0REG[idx] & USB_EP0R_STAT_TX));
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
		if(EP0REG[idx] & USB_EP0R_DTOG_RX)
		{
			SetEPDblBuf1Count(idx, true, size);
		}
		else
		{
			SetEPDblBuf0Count(idx, true, size);
		}
	}
	else
	{
		SetEPTxCount(idx, size);
	}
	SetEPTxStatus(idx, USB_EPTX_STAT_VALID);
	return VSFERR_NONE;
}

static void stm32_usr2pma_copy(uint8_t *usr, uint16_t pma_addr, uint16_t bytes)
{
	uint16_t i;
	uint16_t *pma_ptr;
	
	pma_ptr = (uint16_t *)(pma_addr * 2 + PMAAddr);
	for (i = ((bytes + 1) >> 1); i > 0; i--)
	{
		*pma_ptr = usr[0] + (usr[1] << 8);
		pma_ptr += 2;
		usr += 2;
	}
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
		if(EP0REG[idx] & USB_EP0R_DTOG_RX)
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
	stm32_usr2pma_copy(buffer, PMA_ptr, size);
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
	SetEPDblBuffCount(idx, false, epsize);
	ClearDTOG_RX(idx);
	ClearDTOG_TX(idx);
	ToggleDTOG_TX(idx);
	SetEPRxStatus(idx, USB_EPRX_STAT_VALID);
	SetEPTxStatus(idx, USB_EPTX_STAT_DIS);
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
	FreeUserBuffer(idx, false);
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
		SetEPRxStatus(idx, USB_EPRX_STAT_VALID);
	}
	else
	{
		SetEPRxStatus(idx, USB_EPRX_STAT_NAK);
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
	SetEPRxStatus(idx, USB_EPRX_STAT_STALL);
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
		SetEPRxStatus(idx, USB_EPRX_STAT_VALID);
	}
	else
	{
		SetEPRxStatus(idx, USB_EPRX_STAT_NAK);
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
	
	return (USB_EPRX_STAT_STALL == (EP0REG[idx] & USB_EP0R_STAT_RX));
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
		if(EP0REG[idx] & USB_EP0R_DTOG_TX)
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

static void stm32_pma2usr_copy(uint8_t *usr, uint16_t pma_addr, uint16_t bytes)
{
	uint16_t i;
	uint32_t *pma_ptr;
	
	pma_ptr = (uint32_t *)(pma_addr * 2 + PMAAddr);
	for (i = ((bytes + 1) >> 1); i > 0; i--)
	{
		*(uint16_t*)usr = *pma_ptr++;
		usr += 2;
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
		if(EP0REG[idx] & USB_EP0R_DTOG_TX)
		{
			stm32_pma2usr_copy(buffer, GetEPDblBuf1Addr(idx), size);
		}
		else
		{
			stm32_pma2usr_copy(buffer, GetEPDblBuf0Addr(idx), size);
		}
	}
	else
	{
		stm32_pma2usr_copy(buffer, GetEPRxAddr(idx), size);
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
	
	SetEPRxStatus(idx, USB_EPRX_STAT_VALID);
	return VSFERR_NONE;
}






void CTR_LP(void)
{
	uint8_t EPindex;
	int8_t epaddr;
	uint16_t wIstr;
	volatile uint16_t wEPVal = 0;
	
	while (((wIstr = *ISTR) & USB_ISTR_CTR) != 0)
	{
		EPindex = (uint8_t)(wIstr & USB_ISTR_EP_ID);
		epaddr = stm32_usbd_epaddr[EPindex];
		
		if (epaddr == 0)
		{
			if ((wIstr & USB_ISTR_DIR) == 0)
			{
				ClearEP_CTR_TX(0);
				if (stm32_usbd_callback.on_in != NULL)
				{
					stm32_usbd_callback.on_in(stm32_usbd_callback.param,
												epaddr);
				}
				return;
			}
			else
			{
				wEPVal = EP0REG[0];
				ClearEP_CTR_RX(0);
				if ((wEPVal & USB_EP0R_SETUP) != 0)
				{
					if (stm32_usbd_callback.on_setup != NULL)
					{
						stm32_usbd_callback.on_setup(stm32_usbd_callback.param);
					}
				}
				else if ((wEPVal & USB_EP0R_CTR_RX) != 0)
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
			wEPVal = EP0REG[EPindex];
			if ((wEPVal & USB_EP0R_CTR_RX) != 0)
			{
				ClearEP_CTR_RX(EPindex);
				if ((stm32_usbd_callback.on_out != NULL) && (epaddr >= 0))
				{
					stm32_usbd_callback.on_out(stm32_usbd_callback.param,
												epaddr);
				}
			}
			if ((wEPVal & USB_EP0R_CTR_TX) != 0)
			{
				ClearEP_CTR_TX(EPindex);
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
	
	while (((wIstr = *ISTR) & USB_ISTR_CTR) != 0)
	{
		*ISTR = ~USB_ISTR_CTR;
		
		EPindex = (uint8_t)(wIstr & USB_ISTR_EP_ID);
		epaddr = stm32_usbd_epaddr[EPindex];
		wEPVal = EP0REG[EPindex];
		if ((wEPVal & USB_EP0R_CTR_RX) != 0)
		{
			ClearEP_CTR_RX(EPindex);
			if ((stm32_usbd_callback.on_out != NULL) && (epaddr >= 0))
			{
				stm32_usbd_callback.on_out(stm32_usbd_callback.param, epaddr);
			}
		}
		else if ((wEPVal & USB_EP0R_CTR_TX) != 0)
		{
			ClearEP_CTR_TX(EPindex);
			if ((stm32_usbd_callback.on_in != NULL) && (epaddr >= 0))
			{
				stm32_usbd_callback.on_in(stm32_usbd_callback.param, epaddr);
			}
		}
	}
}

void USB_Istr(void)
{
	uint16_t wIstr = *ISTR;
	
	if (wIstr & USB_ISTR_RESET)
	{
		*ISTR = ~USB_ISTR_RESET;
		if (stm32_usbd_callback.on_reset != NULL)
		{
			stm32_usbd_callback.on_reset(stm32_usbd_callback.param);
		}
	}
	if (wIstr & USB_ISTR_PMAOVR)
	{
		*ISTR = ~USB_ISTR_PMAOVR;
	}
	if (wIstr & USB_ISTR_ERR)
	{
		*ISTR = ~USB_ISTR_ERR;
		if (stm32_usbd_callback.on_error != NULL)
		{
			stm32_usbd_callback.on_error(stm32_usbd_callback.param,
											USBERR_ERROR);
		}
	}
	if (wIstr & USB_ISTR_WKUP)
	{
		*ISTR = ~USB_ISTR_WKUP;
		if (stm32_usbd_callback.on_wakeup != NULL)
		{
			stm32_usbd_callback.on_wakeup(stm32_usbd_callback.param);
		}
	}
	if (wIstr & USB_ISTR_SUSP)
	{
		if (stm32_usbd_callback.on_suspend != NULL)
		{
			stm32_usbd_callback.on_suspend(stm32_usbd_callback.param);
		}
		*ISTR = ~USB_ISTR_SUSP;
	}
	if (wIstr & USB_ISTR_SOF)
	{
		*ISTR = ~USB_ISTR_SOF;
		if (stm32_usbd_callback.on_sof != NULL)
		{
			stm32_usbd_callback.on_sof(stm32_usbd_callback.param);
		}
	}
	if (wIstr & USB_ISTR_ESOF)
	{
		*ISTR = ~USB_ISTR_ESOF;
		if (stm32_usbd_callback.on_error != NULL)
		{
			stm32_usbd_callback.on_error(stm32_usbd_callback.param,
											USBERR_SOF_TO);
		}
	}
	if (wIstr & USB_ISTR_CTR)
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
