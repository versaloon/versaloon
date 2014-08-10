/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       interfaces.c                                              *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    interfaces implementation file                            *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-12-05:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#include "app_interfaces.h"

#if INTERFACE_USART_EN
#	include "USART/USART.h"
#endif
#if INTERFACE_SPI_EN
#	include "SPI/SPI.h"
#endif
#if INTERFACE_EBI_EN
#	include "EBI/EBI.h"
#endif
#if INTERFACE_GPIO_EN
#	include "GPIO/GPIO.h"
#endif
#if INTERFACE_IIC_EN
#	include "IIC/IIC.h"
#endif
#if INTERFACE_PWM_EN
#	include "PWM/PWM.h"
#endif
#if INTERFACE_SDIO_EN
#	include "SDIO/SDIO.h"
#endif
#if INTERFACE_ADC_EN
#	include "ADC/ADC.h"
#endif

// GPIO
void GPIO_SetMode(GPIO_TypeDef* GPIOx, uint8_t pin, uint8_t mode)
{
	uint32_t tmp_reg;

	if(pin < 8)
	{
		tmp_reg = GPIOx->CRL;
		tmp_reg &= ~(((uint32_t)0x0F) << ((pin - 0) * 4));
		tmp_reg |= (uint32_t)(mode & 0x0F) << ((pin - 0) * 4);
		GPIOx->CRL = tmp_reg;
	}
	else
	{
		tmp_reg = GPIOx->CRH;
		tmp_reg &= ~(((uint32_t)0x0F) << ((pin - 8) * 4));
		tmp_reg |= (uint32_t)(mode & 0x0F) << ((pin - 8) * 4);
		GPIOx->CRH = tmp_reg;
	}

	if(mode & 0x20)
	{
		if(mode & 0x10)
		{
			GPIOx->BSRR = (((uint32_t)0x01) << pin);
		}
		else
		{
			GPIOx->BRR = (((uint32_t)0x01) << pin);
		}
	}
}

// delay
static vsf_err_t delay_init(void)
{
	return core_interfaces.delay.init();
}
static vsf_err_t delay_delayms(uint16_t ms)
{
	ms &= 0x7FFF;
	return core_interfaces.delay.delayms(ms);
}
static vsf_err_t delay_delayus(uint16_t us)
{
	us &= 0x7FFF;
	return core_interfaces.delay.delayus(us);
}

static vsf_err_t tickclk_init(void)
{
	return core_interfaces.tickclk.init();
}
static vsf_err_t tickclk_fini(void)
{
	return core_interfaces.tickclk.fini();
}
static vsf_err_t tickclk_start(void)
{
	return core_interfaces.tickclk.start();
}
static vsf_err_t tickclk_stop(void)
{
	return core_interfaces.tickclk.stop();
}
static uint32_t tickclk_get_count(void)
{
	return core_interfaces.tickclk.get_count();
}

static vsf_err_t app_interface_init(void *p)
{
	return core_interfaces.core.init(p);
}

static vsf_err_t app_interface_fini(void *p)
{
	return core_interfaces.core.fini(p);
}

static vsf_err_t app_interface_reset(void *p)
{
	return core_interfaces.core.reset(p);
}

static vsf_err_t app_peripheral_commit(void)
{
	return VSFERR_NONE;
}

const struct app_interfaces_info_t app_interfaces = 
{
	"versaloon",
	
	app_peripheral_commit,
	
	0
#if INTERFACE_USART_EN
	| IFS_USART 
#endif
#if INTERFACE_SPI_EN
	| IFS_SPI 
#endif
#if INTERFACE_IIC_EN
	| IFS_I2C 
#endif
#if INTERFACE_GPIO_EN
	| IFS_GPIO 
#endif
#if INTERFACE_PWM_EN
	| IFS_PWM
#endif
#if INTERFACE_ADC_EN
	| IFS_ADC
#endif
	
	,{
		// core
		app_interface_init,
		app_interface_fini,
		app_interface_reset
	}
	
#if INTERFACE_GPIO_EN
	,{
		// gpio
		gpio_init,
		gpio_fini,
		NULL,
		gpio_config,
		gpio_set,
		gpio_clear,
		gpio_out,
		gpio_in,
		NULL
	}
#endif
#if INTERFACE_USART_EN
	,{
		// usart
		usart_init,
		usart_fini,
		usart_config,
		usart_send,
		usart_receive,
		usart_status,
		usart_poll
	}
#endif
#if INTERFACE_SPI_EN
	,{
		// spi
		spi_init,
		spi_fini,
		NULL, NULL, NULL,
		spi_config,
		spi_select,
		spi_deselect,
		NULL, NULL, NULL, NULL,
		spi_io,
		NULL, NULL, NULL
	}
#endif
#if INTERFACE_EBI_EN
	,{
		ebi_init,
		ebi_fini,
		ebi_config, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
		ebi_get_base_addr,
		ebi_isready,
		ebi_read,
		ebi_write,
		NULL, NULL, NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	}
#endif
#if INTERFACE_IIC_EN
	,{
		// i2c
		iic_init,
		iic_fini,
		iic_config,
		iic_read,
		iic_write
	}
#endif
#if INTERFACE_PWM_EN
	,{
		// pwm
		pwm_init,
		pwm_fini,
		pwm_config_mode,
		pwm_config_freq,
		pwm_out,
		pwm_in
	}
#endif
#if INTERFACE_SDIO_EN
	,{
		// sdio
		sdio_init,
		sdio_fini,
		sdio_config,
		sdio_start,
		sdio_stop,
		sdio_send_cmd,
		sdio_send_cmd_isready,
		sdio_get_resp,
		sdio_data_tx,
		sdio_data_tx_isready,
		sdio_data_rx,
		sdio_data_rx_isready,
	}
#endif
#if INTERFACE_ADC_EN
	,{
		adc_init,
		adc_fini,
		adc_config,
		adc_config_channel,
		adc_calibrate,
		adc_start,
		adc_isready,
		adc_get,
		adc_sample
	}
#endif
	,{
		// tickclk
		tickclk_init,
		tickclk_fini,
		tickclk_start,
		tickclk_stop,
		tickclk_get_count
	}
	,{
		// delay
		delay_init,
		delay_delayms,
		delay_delayus
	}
};

struct app_interfaces_info_t *interfaces = 
								(struct app_interfaces_info_t *)&app_interfaces;
