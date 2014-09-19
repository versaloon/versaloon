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
#include "interfaces.h"

#if IFS_GPIO_EN

#include "STM32F4_GPIO.h"

#define STM32F4_GPIO_NUM					11

vsf_err_t stm32f4_gpio_init(uint8_t index)
{
#if __VSF_DEBUG__
	if (index >= STM32F4_GPIO_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	RCC->AHB1ENR |= RCC_AHB1ENR_GPIOAEN << index;
	return VSFERR_NONE;
}

vsf_err_t stm32f4_gpio_fini(uint8_t index)
{
#if __VSF_DEBUG__
	if (index >= STM32F4_GPIO_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	RCC->AHB1ENR &= ~(RCC_AHB1ENR_GPIOAEN << index);
	return VSFERR_NONE;
}

vsf_err_t stm32f4_gpio_config_pin(uint8_t index, uint8_t pin_idx, uint8_t mode)
{
	GPIO_TypeDef *gpio;
	
#if __VSF_DEBUG__
	if (index >= STM32F4_GPIO_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	gpio = (GPIO_TypeDef *)(GPIOA_BASE + ((uint32_t)index << 10));
	return VSFERR_NONE;
}

vsf_err_t stm32f4_gpio_config(uint8_t index, uint32_t pin_mask, uint32_t io, 
							uint32_t pull_en_mask, uint32_t input_pull_mask)
{
	GPIO_TypeDef *gpio;
	
#if __VSF_DEBUG__
	if (index >= STM32F4_GPIO_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	gpio = (GPIO_TypeDef *)(GPIOA_BASE + ((uint32_t)index << 10));
	return VSFERR_NONE;
}

vsf_err_t stm32f4_gpio_set(uint8_t index, uint32_t pin_mask)
{
	GPIO_TypeDef *gpio;
	
#if __VSF_DEBUG__
	if (index >= STM32F4_GPIO_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	gpio = (GPIO_TypeDef *)(GPIOA_BASE + ((uint32_t)index << 10));
	return VSFERR_NONE;
}

vsf_err_t stm32f4_gpio_clear(uint8_t index, uint32_t pin_mask)
{
	GPIO_TypeDef *gpio;
	
#if __VSF_DEBUG__
	if (index >= STM32F4_GPIO_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	gpio = (GPIO_TypeDef *)(GPIOA_BASE + ((uint32_t)index << 10));
	return VSFERR_NONE;
}

vsf_err_t stm32f4_gpio_out(uint8_t index, uint32_t pin_mask, uint32_t value)
{
	GPIO_TypeDef *gpio;
	
#if __VSF_DEBUG__
	if (index >= STM32F4_GPIO_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	gpio = (GPIO_TypeDef *)(GPIOA_BASE + ((uint32_t)index << 10));
	return VSFERR_NONE;
}

vsf_err_t stm32f4_gpio_in(uint8_t index, uint32_t pin_mask, uint32_t *value)
{
	GPIO_TypeDef *gpio;
	
#if __VSF_DEBUG__
	if (index >= STM32F4_GPIO_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	gpio = (GPIO_TypeDef *)(GPIOA_BASE + ((uint32_t)index << 10));
	return VSFERR_NONE;
}

uint32_t stm32f4_gpio_get(uint8_t index, uint32_t pin_mask)
{
	GPIO_TypeDef *gpio;
	
#if __VSF_DEBUG__
	if (index >= STM32F4_GPIO_NUM)
	{
		return 0;
	}
#endif
	
	gpio = (GPIO_TypeDef *)(GPIOA_BASE + ((uint32_t)index << 10));
	return 0;
}

#endif
