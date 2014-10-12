/***************************************************************************
 *   Copyright (C) 2009 - 2010 by Simon Qian <SimonQian@SimonQian.com>     *
 *                                                                         *
 *   This ifsram is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This ifsram is distributed in the hope that it will be useful,        *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this ifsram; if not, write to the                          *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "app_type.h"
#include "app_cfg.h"
#if TARGET_ARM_ADI_EN
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "../../interfaces.h"
#include "vsprog.h"
#include "target.h"
#include "scripts.h"

#include "adi_v5p1.h"
#include "cm_common.h"
#include "cm_internal.h"

#define VI_STM32_STRING						"vi_stm32"

VSS_HANDLER(vi_stm32_support);
VSS_HANDLER(vi_stm32_help);
const struct vss_cmd_t vi_stm32_notifier[] =
{
	VSS_CMD(	"support",
				"print support information, format: support/S",
				vi_stm32_support,
				NULL),
	VSS_CMD(	"S",
				"print support information, format: support/S",
				vi_stm32_support,
				NULL),
	VSS_CMD(	"help",
				"print help information, format: help/h",
				vi_stm32_help,
				NULL),
	VSS_CMD(	"h",
				"print help information, format: help/h",
				vi_stm32_help,
				NULL),
	VSS_CMD_END
};

#define VI_STM32_MODE_JTAG					0
#define VI_STM32_MODE_SWD					1
const struct program_mode_t vi_stm32_mode[] =
{
	{'j', SET_FREQUENCY, IFS_JTAG_HL},
	{'s', "", IFS_SWD},
	{0, NULL, 0}
};

VSS_HANDLER(vi_stm32_help)
{
	VSS_CHECK_ARGC(1);
		PRINTF("Usage of %s:\n\n", VI_STM32_STRING);
	return VSFERR_NONE;
}

VSS_HANDLER(vi_stm32_support)
{
	VSS_CHECK_ARGC(1);
	PRINTF("%s: virtual programmer on stm32.\n", VI_STM32_STRING);
	return VSFERR_NONE;
}



struct cm_common_info_t vi_stm32_cm_info;
static vsf_err_t vi_stm32_init(void *p)
{
	struct program_info_t *pi = cur_context->pi;
	uint32_t mode = *(uint32_t *)p;
	struct adi_dpif_t dp;
	struct cm_param_t *param;
	
	param = cm_get_param("cm_stm32f1");
	if (NULL == param)
	{
		return VSFERR_FAIL;
	}
	
	switch (mode)
	{
	case VI_STM32_MODE_JTAG:
		// JTAG;
		dp.type = ADI_DP_JTAG;
		dp.dpif_setting.dpif_jtag_setting.jtag_khz = param->jtag_khz;
		dp.dpif_setting.dpif_jtag_setting.jtag_pos.ub =
								param->jtag_pos.ub + pi->jtag_pos.ub;
		dp.dpif_setting.dpif_jtag_setting.jtag_pos.ua =
								param->jtag_pos.ua + pi->jtag_pos.ua;
		dp.dpif_setting.dpif_jtag_setting.jtag_pos.bb =
								param->jtag_pos.bb + pi->jtag_pos.bb;
		dp.dpif_setting.dpif_jtag_setting.jtag_pos.ba =
								param->jtag_pos.ba + pi->jtag_pos.ba;
		break;
	case VI_STM32_MODE_SWD:
		// SWD
		dp.type = ADI_DP_SWD;
		dp.dpif_setting.dpif_swd_setting.swd_trn = param->swd_trn;
		dp.dpif_setting.dpif_swd_setting.swd_dly = param->swd_delay;
		dp.dpif_setting.dpif_swd_setting.swd_retry = 0;
		break;
	default:
		return VSFERR_FAIL;
	}
	cm_switch(&vi_stm32_cm_info);
	if (cm_dp_init(cur_real_interface, &dp))
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t vi_stm32_fini(void *p)
{
	REFERENCE_PARAMETER(p);
	return cur_real_interface->core.fini(cur_real_interface);
}

static vsf_err_t vi_stm32_reset(void *p)
{
	REFERENCE_PARAMETER(p);
	return VSFERR_NONE;
}

// GPIO
vsf_err_t vi_stm32_gpio_init(uint8_t index)
{
	REFERENCE_PARAMETER(index);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_gpio_fini(uint8_t index)
{
	REFERENCE_PARAMETER(index);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_gpio_config_pin(uint8_t index, uint8_t pin_idx, uint8_t mode)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(pin_idx);
	REFERENCE_PARAMETER(mode);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_gpio_config(uint8_t index, uint32_t mask,
								uint32_t dir_mask, uint32_t pull_en_mask,
								uint32_t input_pull_mask)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(mask);
	REFERENCE_PARAMETER(dir_mask);
	REFERENCE_PARAMETER(pull_en_mask);
	REFERENCE_PARAMETER(input_pull_mask);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_gpio_in(uint8_t index, uint32_t mask, uint32_t *value)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(mask);
	REFERENCE_PARAMETER(value);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_gpio_set(uint8_t index, uint32_t mask)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(mask);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_gpio_clear(uint8_t index, uint32_t mask)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(mask);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_gpio_out(uint8_t index, uint32_t mask, uint32_t value)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(mask);
	REFERENCE_PARAMETER(value);
	return VSFERR_NONE;
}

// USART
vsf_err_t vi_stm32_usart_init(uint8_t index)
{
	REFERENCE_PARAMETER(index);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_usart_fini(uint8_t index)
{
	REFERENCE_PARAMETER(index);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_usart_config(uint8_t index, uint32_t baudrate,
								uint8_t datalength, uint8_t mode)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(baudrate);
	REFERENCE_PARAMETER(datalength);
	REFERENCE_PARAMETER(mode);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_usart_receive(uint8_t index, uint8_t *buf, uint16_t len)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(buf);
	REFERENCE_PARAMETER(len);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_usart_send(uint8_t index, uint8_t *buf, uint16_t len)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(buf);
	REFERENCE_PARAMETER(len);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_usart_status(uint8_t index, struct usart_status_t *status)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(status);
	return VSFERR_NONE;
}

// SPI
vsf_err_t vi_stm32_spi_init(uint8_t index)
{
	REFERENCE_PARAMETER(index);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_spi_fini(uint8_t index)
{
	REFERENCE_PARAMETER(index);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_spi_get_ability(uint8_t index, struct spi_ability_t *ability)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(ability);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_spi_enable(uint8_t index)
{
	REFERENCE_PARAMETER(index);
	return VSFERR_NONE;
}
vsf_err_t vs_stm32_spi_disable(uint8_t index)
{
	REFERENCE_PARAMETER(index);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_spi_config(uint8_t index, uint32_t kHz, uint8_t mode)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(kHz);
	REFERENCE_PARAMETER(mode);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_spi_select(uint8_t index, uint8_t cs)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(cs);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_spi_deselect(uint8_t index, uint8_t cs)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(cs);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_spi_io(uint8_t index, uint8_t *out, uint8_t *in,
							uint32_t bytelen)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(out);
	REFERENCE_PARAMETER(in);
	REFERENCE_PARAMETER(bytelen);
	return VSFERR_NONE;
}

// IIC
vsf_err_t vi_stm32_iic_init(uint8_t index)
{
	REFERENCE_PARAMETER(index);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_iic_fini(uint8_t index)
{
	REFERENCE_PARAMETER(index);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_iic_config(uint8_t index, uint16_t kHz,
								uint16_t byte_interval, uint16_t max_dly)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(kHz);
	REFERENCE_PARAMETER(byte_interval);
	REFERENCE_PARAMETER(max_dly);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_iic_read(uint8_t index, uint16_t chip_addr,
							uint8_t *data, uint16_t data_len, uint8_t stop,
							bool nacklast)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(chip_addr);
	REFERENCE_PARAMETER(data);
	REFERENCE_PARAMETER(data_len);
	REFERENCE_PARAMETER(stop);
	REFERENCE_PARAMETER(nacklast);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_iic_write(uint8_t index, uint16_t chip_addr,
								uint8_t *data, uint16_t data_len, uint8_t stop)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(chip_addr);
	REFERENCE_PARAMETER(data);
	REFERENCE_PARAMETER(data_len);
	REFERENCE_PARAMETER(stop);
	return VSFERR_NONE;
}

// PWM
vsf_err_t vi_stm32_pwm_init(uint8_t index)
{
	REFERENCE_PARAMETER(index);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_pwm_fini(uint8_t index)
{
	REFERENCE_PARAMETER(index);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_pwm_config_mode(uint8_t index, uint8_t mode)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(mode);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_pwm_config_freq(uint8_t index, uint16_t kHz)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(kHz);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_pwm_out(uint8_t index, uint16_t count, uint16_t *rate)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(count);
	REFERENCE_PARAMETER(rate);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_pwm_in(uint8_t index, uint16_t count, uint16_t *rate)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(count);
	REFERENCE_PARAMETER(rate);
	return VSFERR_NONE;
}

// clko
vsf_err_t vi_stm32_clko_init(uint8_t index)
{
	REFERENCE_PARAMETER(index);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_clko_fini(uint8_t index)
{
	REFERENCE_PARAMETER(index);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_clko_config(uint8_t index, uint32_t kHz)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(kHz);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_clko_enable(uint8_t index)
{
	REFERENCE_PARAMETER(index);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_clko_disable(uint8_t index)
{
	REFERENCE_PARAMETER(index);
	return VSFERR_NONE;
}

// tickclk
vsf_err_t vi_stm32_tickclk_init(void)
{
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_tickclk_fini(void)
{
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_tickclk_start(void)
{
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_tickclk_stop(void)
{
	return VSFERR_NONE;
}

uint32_t vi_stm32_tickclk_get_count(void)
{
	return 0;
}

// delay
vsf_err_t vi_stm32_delayinit(void)
{
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_delayms(uint16_t ms)
{
	REFERENCE_PARAMETER(ms);
	return VSFERR_NONE;
}

vsf_err_t vi_stm32_delayus(uint16_t us)
{
	REFERENCE_PARAMETER(us);
	return VSFERR_NONE;
}

static vsf_err_t vi_stm32_peripheral_commit(void)
{
	return VSFERR_NONE;
}

struct interfaces_info_t vi_stm32_interfaces =
{
	VI_STM32_STRING,
	vi_stm32_notifier,
	NULL,
	
	true,
	vi_stm32_mode,
	
	IFS_GPIO | IFS_USART | IFS_SPI | IFS_I2C | IFS_PWM,
	
	{	// core
		vi_stm32_init,
		vi_stm32_fini,
		vi_stm32_reset
	},
	{
		vi_stm32_clko_init,
		vi_stm32_clko_fini,
		vi_stm32_clko_config,
		vi_stm32_clko_enable,
		vi_stm32_clko_disable,
	},
	{
		vi_stm32_tickclk_init,
		vi_stm32_tickclk_fini,
		vi_stm32_tickclk_start,
		vi_stm32_tickclk_stop,
		vi_stm32_tickclk_get_count
	},
	{	// delay
		vi_stm32_delayinit,
		vi_stm32_delayms,
		vi_stm32_delayus
	},
	{	// gpio
		vi_stm32_gpio_init,
		vi_stm32_gpio_fini,
		vi_stm32_gpio_config_pin,
		vi_stm32_gpio_config,
		vi_stm32_gpio_set,
		vi_stm32_gpio_clear,
		vi_stm32_gpio_out,
		vi_stm32_gpio_in
	},
	{	// usart
		vi_stm32_usart_init,
		vi_stm32_usart_fini,
		vi_stm32_usart_config,
		vi_stm32_usart_send,
		vi_stm32_usart_receive,
		vi_stm32_usart_status
	},
	{	// spi
		vi_stm32_spi_init,
		vi_stm32_spi_fini,
		vi_stm32_spi_get_ability,
		vi_stm32_spi_enable,
		vs_stm32_spi_disable,
		vi_stm32_spi_config,
		vi_stm32_spi_select,
		vi_stm32_spi_deselect,
		vi_stm32_spi_io
	},
	{
		// nand
		NULL, NULL, NULL, NULL, NULL, NULL, NULL
	},
	{
		// ebi
		NULL, NULL, NULL, NULL, NULL, NULL
	},
	{	// i2c
		vi_stm32_iic_init,
		vi_stm32_iic_fini,
		vi_stm32_iic_config,
		vi_stm32_iic_read,
		vi_stm32_iic_write
	},
	{	// pwm
		vi_stm32_pwm_init,
		vi_stm32_pwm_fini,
		vi_stm32_pwm_config_mode,
		vi_stm32_pwm_config_freq,
		vi_stm32_pwm_out,
		vi_stm32_pwm_in
	},
	{	// microwire
		NULL, NULL, NULL, NULL, NULL
	},
	{	// target_voltage
		NULL, NULL
	},
	{
		// adc
		NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
	},
	{	// issp
		NULL, NULL, NULL, NULL, NULL, NULL
	},
	{	// swd
		NULL, NULL, NULL, NULL, NULL, NULL
	},
	{	// jtag_hl
		NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
	},
	{	// jtag_ll
		NULL, NULL, NULL, NULL, NULL, NULL
	},
	{	// jtag_raw
		NULL, NULL, NULL, NULL
	},
	{	// msp430_jtag
		NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
	},
	{	// msp430_sbw
		NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
	},
	{	// c2
		NULL, NULL, NULL, NULL, NULL, NULL
	},
	{	// lpcicp
		NULL, NULL, NULL, NULL, NULL, NULL
	},
	{	// swim
		NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
	},
	{	// bdm
		NULL, NULL, NULL, NULL
	},
	{	// dusi
		NULL, NULL, NULL, NULL
	},
	{	// poll
		NULL, NULL, NULL, NULL, NULL
	},
	vi_stm32_peripheral_commit
};
#endif
