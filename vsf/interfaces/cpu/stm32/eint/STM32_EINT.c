/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       GPIO.h                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    GPIO interface header file                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_type.h"
#include "compiler.h"
#include "interfaces.h"

#if IFS_EINT_EN

#include "STM32_EINT.h"

#define STM32_EINT_NUM					16
static void (*stm32_enit_callback[STM32_EINT_NUM])(void);

vsf_err_t stm32_eint_init(uint8_t index)
{
	uint32_t eint_idx = (index & 0x0F) >> 0;
	uint32_t port_idx = (index & 0xF0) >> 4;
	
#if __VSF_DEBUG__
	if (eint_idx >= STM32_EINT_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	AFIO->EXTICR[eint_idx >> 2] &= 0x0F << ((eint_idx & 0x03) << 2);
	AFIO->EXTICR[eint_idx >> 2] |= port_idx << ((eint_idx & 0x03) << 2);
	return VSFERR_NONE;
}

vsf_err_t stm32_eint_fini(uint8_t index)
{
	uint8_t eint_idx = index & 0x0F;
	uint32_t mask = 1 << eint_idx;
	
#if __VSF_DEBUG__
	if (eint_idx >= STM32_EINT_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	EXTI->IMR &= ~mask;
	EXTI->EMR &= ~mask;
	EXTI->PR |= ~mask;
	return VSFERR_NONE;
}

vsf_err_t stm32_eint_config(uint8_t index, uint8_t type, void (*callback)(void))
{
	uint8_t eint_idx = index & 0x0F;
	uint32_t mask = 1 << eint_idx;
	uint8_t on_fall = type & EINT_ONFALL, 
			on_rise = type & EINT_ONRISE, 
			interrupt = type & EINT_INT, 
			event = type & EINT_EVT;
	
#if __VSF_DEBUG__
	if (eint_idx >= STM32_EINT_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	if (on_fall || on_rise)
	{
		if (on_fall)
		{
			EXTI->FTSR |= mask;
		}
		else
		{
			EXTI->FTSR &= ~mask;
		}
		if (on_rise)
		{
			EXTI->RTSR |= mask;
		}
		else
		{
			EXTI->RTSR &= ~mask;
		}
		stm32_enit_callback[eint_idx] = callback;
		if (interrupt)
		{
			EXTI->EMR |= mask;
		}
		if (event)
		{
			EXTI->IMR |= mask;
		}
	}
	else
	{
		EXTI->IMR &= ~mask;
		EXTI->EMR &= ~mask;
		stm32_enit_callback[eint_idx] = NULL;
	}
	
	return VSFERR_NONE;
}

vsf_err_t stm32_eint_enable(uint8_t index)
{
	uint8_t eint_idx = index & 0x0F;
	uint32_t mask = 1 << eint_idx;
	
#if __VSF_DEBUG__
	if (eint_idx >= STM32_EINT_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	EXTI->IMR |= mask;
	return VSFERR_NONE;
}

vsf_err_t stm32_eint_disable(uint8_t index)
{
	uint8_t eint_idx = index & 0x0F;
	uint32_t mask = 1 << eint_idx;
	
#if __VSF_DEBUG__
	if (eint_idx >= STM32_EINT_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	EXTI->IMR &= ~mask;
	return VSFERR_NONE;
}

vsf_err_t stm32_eint_trigger(uint8_t index)
{
	uint8_t eint_idx = index & 0x0F;
	uint32_t mask = 1 << eint_idx;
	
#if __VSF_DEBUG__
	if (eint_idx >= STM32_EINT_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	EXTI->SWIER |= mask;
	return VSFERR_NONE;
}

static void stm32_eint_call(uint8_t index)
{
	if (stm32_enit_callback[index] != NULL)
	{
		stm32_enit_callback[index]();
	}
	EXTI->PR = 1 << index;
}

ROOTFUNC void EXTI0_IRQHandler(void)
{
	stm32_eint_call(0);
}

ROOTFUNC void EXTI1_IRQHandler(void)
{
	stm32_eint_call(1);
}

ROOTFUNC void EXTI2_IRQHandler(void)
{
	stm32_eint_call(2);
}

ROOTFUNC void EXTI3_IRQHandler(void)
{
	stm32_eint_call(3);
}

ROOTFUNC void EXTI4_IRQHandler(void)
{
	stm32_eint_call(4);
}

ROOTFUNC void EXTI9_5_IRQHandler(void)
{
	uint8_t i;
	
	for (i = 5; i <= 9; i++)
	{
		if (EXTI->IMR & (1 << i))
		{
			if (EXTI->PR & (1 << i))
			{
				stm32_eint_call(i);
			}
		}
	}
}

ROOTFUNC void EXTI15_10_IRQHandler(void)
{
	uint8_t i;
	
	for (i = 10; i <= 15; i++)
	{
		if (EXTI->IMR & (1 << i))
		{
			if (EXTI->PR & (1 << i))
			{
				stm32_eint_call(i);
			}
		}
	}
}

#endif
