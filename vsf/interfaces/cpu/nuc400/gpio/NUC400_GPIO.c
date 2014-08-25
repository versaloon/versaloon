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

#include "NUC400_GPIO.h"
#include "NUC472_442.h"

#define NUC400_GPIO_NUM					7

vsf_err_t nuc400_gpio_init(uint8_t index)
{
#if __VSF_DEBUG__
	if (index >= NUC400_GPIO_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	return VSFERR_NONE;
}

vsf_err_t nuc400_gpio_fini(uint8_t index)
{
#if __VSF_DEBUG__
	if (index >= NUC400_GPIO_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	return VSFERR_NONE;
}

vsf_err_t nuc400_gpio_config_pin(uint8_t index, uint8_t pin_idx, uint8_t mode)
{
	GPIO_T *gpio;
	uint32_t tmpreg = mode & 0x03;
	
#if __VSF_DEBUG__
	if ((index >= NUC400_GPIO_NUM) || (pin_idx >= 16))
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	gpio = (GPIO_T *)(GPIOA_BASE + ((uint32_t)index << 6));
	gpio->MODE = (gpio->MODE & ~(((uint32_t)0x03) << (pin_idx << 1))) |
						tmpreg << (pin_idx << 1);
	gpio->DINOFF &= ~(0x100UL << pin_idx);
	return VSFERR_NONE;
}

vsf_err_t nuc400_gpio_config(uint8_t index, uint32_t pin_mask, uint32_t io, 
							uint32_t pull_en_mask, uint32_t input_pull_mask)
{
	uint32_t i, mask;
	uint8_t mode;
	
#if __VSF_DEBUG__
	if (index >= NUC400_GPIO_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	for (i = 0; i < 16; i++)
	{
		mask = 1UL << i;
		if (pin_mask & mask)
		{
			mode = !(io & mask) ? nuc400_GPIO_INFLOAT :
				(pull_en_mask & input_pull_mask & mask) ? nuc400_GPIO_OUTOD :
				nuc400_GPIO_OUTPP;
			nuc400_gpio_config_pin(index, i, mode);
		}
	}
	return VSFERR_NONE;
}

vsf_err_t nuc400_gpio_set(uint8_t index, uint32_t pin_mask)
{
	uint32_t *gpio;
	uint32_t i;
	
#if __VSF_DEBUG__
	if (index >= NUC400_GPIO_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	gpio = (uint32_t *)(GPIO_PIN_DATA_BASE + ((uint32_t)index << 6));
	for (i = 0; i < 16; i++)
	{
		if (pin_mask & (1UL << i))
		{
			gpio[i] = 1;
		}
	}
	return VSFERR_NONE;
}

vsf_err_t nuc400_gpio_clear(uint8_t index, uint32_t pin_mask)
{
	uint32_t *gpio;
	uint32_t i;
	
#if __VSF_DEBUG__
	if (index >= NUC400_GPIO_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	gpio = (uint32_t *)(GPIO_PIN_DATA_BASE + ((uint32_t)index << 6));
	for (i = 0; i < 16; i++)
	{
		if (pin_mask & (1UL << i))
		{
			gpio[i] = 0;
		}
	}
	return VSFERR_NONE;
}

vsf_err_t nuc400_gpio_out(uint8_t index, uint32_t pin_mask, uint32_t value)
{
	uint32_t *gpio;
	uint32_t i;
	
#if __VSF_DEBUG__
	if (index >= NUC400_GPIO_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	gpio = (uint32_t *)(GPIO_PIN_DATA_BASE + ((uint32_t)index << 6));
	for (i = 0; i < 16; i++)
	{
		if (pin_mask & (1UL << i))
		{
			gpio[i] = (value & (1UL << i)) >> i;
		}
	}
	return VSFERR_NONE;
}

vsf_err_t nuc400_gpio_in(uint8_t index, uint32_t pin_mask, uint32_t *value)
{
	GPIO_T *gpio;
	
#if __VSF_DEBUG__
	if (index >= NUC400_GPIO_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	gpio = (GPIO_T *)(GPIOA_BASE + ((uint32_t)index << 6));
	*value = gpio->PIN & pin_mask;
	return VSFERR_NONE;
}

uint32_t nuc400_gpio_get(uint8_t index, uint32_t pin_mask)
{
	GPIO_T *gpio;
	
#if __VSF_DEBUG__
	if (index >= NUC400_GPIO_NUM)
	{
		return 0;
	}
#endif
	
	gpio = (GPIO_T *)(GPIOA_BASE + ((uint32_t)index << 6));
	return gpio->PIN & pin_mask;
}

#endif
