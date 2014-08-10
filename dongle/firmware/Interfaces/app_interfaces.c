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

#if INTERFACE_CLKO_EN
#	include "CLKO/CLKO.h"
#endif
#if INTERFACE_USART_EN
#	include "USART/USART.h"
#endif
#if POWER_OUT_EN
#	include "PowerExt/PowerExt.h"
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
#if INTERFACE_ISSP_EN
#	include "ISSP/ISSP.h"
#endif
#if INTERFACE_SWD_EN
#	include "SWD/SWD.h"
#endif
#if INTERFACE_JTAG_EN
#	include "JTAG/JTAG_TAP.h"
#endif
#if INTERFACE_MSP430_JTAG_EN
#	include "MSP430_JTAG/MSP430_JTAG.h"
#endif
#if INTERFACE_C2_EN
#	include "C2/C2.h"
#endif
#if INTERFACE_IIC_EN
#	include "IIC/IIC.h"
#endif
#if INTERFACE_LPC_ICP_EN
#	include "LPC_ICP/LPC_ICP.h"
#endif
#if INTERFACE_SWIM_EN
#	include "SWIM/SWIM.h"
#endif
#if INTERFACE_BDM_EN
#	include "BDM/BDM.h"
#endif
#if INTERFACE_DUSI_EN
#	include "DUSI/DUSI.h"
#endif
#if INTERFACE_MICROWIRE_EN
#	include "MicroWire/MicroWire.h"
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

char* get_interface_name(uint64_t i)
{
#define interface_case(i) case i: return #i
	
	switch (i)
	{
		interface_case(IFS_USART);
		interface_case(IFS_SPI);
		interface_case(IFS_I2C);
		interface_case(IFS_GPIO);
		interface_case(IFS_CAN);
		interface_case(IFS_CLOCK);
		interface_case(IFS_ADC);
		interface_case(IFS_DAC);
		interface_case(IFS_POWER);
		interface_case(IFS_ISSP);
		interface_case(IFS_JTAG_LL);
		interface_case(IFS_JTAG_HL);
		interface_case(IFS_JTAG_RAW);
		interface_case(IFS_C2);
		interface_case(IFS_MSP430_SBW);
		interface_case(IFS_MSP430_JTAG);
		interface_case(IFS_LPC_ICP);
		interface_case(IFS_SWD);
		interface_case(IFS_SWIM);
		interface_case(IFS_HV);
		interface_case(IFS_PDI);
		interface_case(IFS_BDM);
	default:
		return NULL;
	}
}

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
	return VSFERR_NONE;
}

static vsf_err_t app_interface_fini(void)
{
	return VSFERR_NONE;
}

static vsf_err_t app_peripheral_commit(void)
{
	return VSFERR_NONE;
}

vsf_err_t interface_assert(struct app_interfaces_info_t **ifs)
{
	if (ifs != NULL)
	{
		*ifs = (struct app_interfaces_info_t *)&app_interfaces;
	}
	return VSFERR_NONE;
}

const struct app_interfaces_info_t app_interfaces = 
{
	"versaloon",
	
	app_interface_init,
	app_interface_fini,
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
#if INTERFACE_CLKO_EN
	| IFS_CLOCK
#endif
#if POWER_OUT_EN
	| IFS_POWER 
#endif
#if INTERFACE_ISSP_EN
	| IFS_ISSP 
#endif
#if INTERFACE_JTAG_EN
	| IFS_JTAG_LL | IFS_JTAG_HL | IFS_JTAG_RAW 
#endif
#if INTERFACE_SWIM_EN
	| IFS_SWIM 
#endif
#if INTERFACE_C2_EN
	| IFS_C2 
#endif
#if INTERFACE_MSP430_JTAG_EN
	| IFS_MSP430_JTAG 
#endif
#if INTERFACE_MSP430_SBW_EN
	| IFS_MSP430_SBW
#endif
#if INTERFACE_LPC_ICP_EN
	| IFS_LPC_ICP 
#endif
#if INTERFACE_SWD_EN
	| IFS_SWD 
#endif
#if INTERFACE_BDM_EN
	| IFS_BDM 
#endif
#if INTERFACE_DUSI_EN
	| IFS_DUSI 
#endif
#if INTERFACE_MICROWIRE_EN
	| IFS_MICROWIRE 
#endif
#if INTERFACE_PWM_EN
	| IFS_PWM
#endif
#if INTERFACE_ADC_EN
	| IFS_ADC
#endif
#if INTERFACE_EBI_EN
	| IFS_EBI
#endif
#if INTERFACE_SDIO_EN
	| IFS_SDIO
#endif
	
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
#if INTERFACE_CLKO_EN
	,{
		clko_init,
		clko_fini,
		clko_config,
		clko_enable,
		clko_disable
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
		NULL,
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
#if POWER_OUT_EN
	,{
		// target_voltage
		target_voltage_get,
		target_voltage_set,
		target_voltage_poll
	}
#endif
#if INTERFACE_ISSP_EN
	,{
		// issp
		issp_init,
		issp_fini,
		issp_enter_program_mode,
		issp_leave_program_mode,
		issp_wait_and_poll,
		issp_vector
	}
#endif
#if INTERFACE_SWD_EN
	,{
		// swd
		swd_init,
		swd_fini,
		swd_config,
		swd_seqout,
		swd_seqin,
		swd_transact
	}
#endif
#if INTERFACE_JTAG_EN
	,{
		// jtag_hl
		jtaghl_init,
		jtaghl_fini,
		jtaghl_config_speed,
		jtaghl_config_daisychain,
		jtaghl_config,
		jtaghl_tms,
		jtaghl_runtest,
		jtaghl_ir,
		jtaghl_dr,
		jtaghl_register_callback
	}
#endif
#if INTERFACE_JTAG_EN
	,{
		// jtag_ll
		jtagll_init,
		jtagll_fini,
		jtagll_config,
		jtagll_tms,
		jtagll_tms_clocks,
		jtagll_scan
	}
#endif
#if INTERFACE_JTAG_EN
	,{
		// jtag_raw
		jtagraw_init,
		jtagraw_fini,
		jtagraw_config,
		jtagraw_execute
	}
#endif
#if INTERFACE_MSP430_JTAG_EN
	,{
		// msp430jtag
		msp430jtag_init,
		msp430jtag_fini,
		msp430jtag_config,
		msp430jtag_ir,
		msp430jtag_dr,
		msp430jtag_tclk,
		msp430jtag_tclk_strobe,
		msp430jtag_reset,
		msp430jtag_poll
	}
#endif
#if INTERFACE_MSP430_SBW_EN
	,{
		// msp430sbw
		NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
	}
#endif
#if INTERFACE_C2_EN
	,{
		// c2
		c2_init,
		c2_fini,
		c2_addr_write,
		c2_addr_read,
		c2_data_write,
		c2_data_read
	}
#endif
#if INTERFACE_LPC_ICP_EN
	,{
		// lpcicp
		lpcicp_init,
		lpcicp_fini,
		lpcicp_enter_program_mode,
		lpcicp_in,
		lpcicp_out,
		lpcicp_poll_ready
	}
#endif
#if INTERFACE_SWIM_EN
	,{
		// swim
		swim_init,
		swim_fini,
		swim_config,
		swim_srst,
		swim_wotf,
		swim_rotf,
		swim_sync,
		swim_enable
	}
#endif
#if INTERFACE_BDM_EN
	,{
		// bdm
		bdm_init,
		bdm_fini,
		bdm_sync,
		bdm_transact
	}
#endif
#if INTERFACE_DUSI_EN
	,{
		// dusi
		dusi_init,
		dusi_fini,
		dusi_config,
		dusi_io
	}
#endif
#if INTERFACE_MICROWIRE_EN
	,{
		// microwire
		microwire_init,
		microwire_fini,
		microwire_config,
		microwire_transport,
		microwire_poll
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
		adc_get_max_value,
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
	,{
		// poll
		NULL, NULL, NULL, NULL, NULL
	}
};

struct app_interfaces_info_t *interfaces = 
								(struct app_interfaces_info_t *)&app_interfaces;
