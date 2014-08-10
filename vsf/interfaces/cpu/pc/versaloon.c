/***************************************************************************
 *   Copyright (C) 2009 - 2010 by Simon Qian <SimonQian@SimonQian.com>     *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <time.h>
#include "compiler.h"

#include "versaloon_include.h"
#include "versaloon.h"
#include "versaloon_internal.h"
#include "usbtoxxx/usbtoxxx.h"
#include "usbtoxxx/usbtoxxx_internal.h"

#include "versaloon_libusb.h"

// usbtoxxx transact structure
static struct usbtoxxx_info_t versaloon_usbtoxxx_info =
{
	NULL, NULL, 0, NULL
};







// Interfaces:
// Core
static vsf_err_t versaloon_fini(void *p)
{
	struct interfaces_info_t *t = interfaces;
	
	usbtoxxx_fini();
	usbtoxxx_info = NULL;
	return t->comm->fini();
}

#define VERSALOON_RETRY_CNT				10
static vsf_err_t versaloon_init(void *p)
{
	struct interfaces_info_t *t = interfaces;
	uint16_t ret = 0;
	uint8_t retry;
	vsf_err_t err = VSFERR_NONE;
	
	if (t->comm->init())
	{
		return VSFERR_FAIL;
	}
	
	// malloc temporary buffer
	if (!versaloon_usbtoxxx_info.buff_len)
	{
		versaloon_usbtoxxx_info.buff_len = 256;
	}
	versaloon_usbtoxxx_info.buff =
						(uint8_t *)malloc(versaloon_usbtoxxx_info.buff_len);
	if (NULL == versaloon_usbtoxxx_info.buff)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return VSFERR_NOT_ENOUGH_RESOURCES;
	}
	
	sleep_ms(100);
	
	// connect to versaloon
	LOG_PUSH();
	LOG_MUTE();
	// not output error message when connectting
	// 500ms delay when connect
	t->comm->set_timeout(100);
	for (retry = 0; retry < VERSALOON_RETRY_CNT; retry++)
	{
		versaloon_usbtoxxx_info.buff[0] = VERSALOON_GET_INFO;
		ret = versaloon_usbtoxxx_info.buff_len;
		if (!t->comm->transact(versaloon_usbtoxxx_info.buff, 1,
								versaloon_usbtoxxx_info.buff, &ret) &&
			(ret >= 3))
		{
			break;
		}
	}
	LOG_POP();
	t->comm->set_timeout(VERSALOON_TIMEOUT);
	if (VERSALOON_RETRY_CNT == retry)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "communicate with versaloon");
		err = ERRCODE_FAILURE_OPERATION;
		goto versaloon_init_fail;
	}
	
	versaloon_usbtoxxx_info.buff[ret] = 0;
	versaloon_usbtoxxx_info.buff_len =
								GET_LE_U16(&versaloon_usbtoxxx_info.buff[0]);
	LOG_INFO("%s", &versaloon_usbtoxxx_info.buff[2]);
	
	// free temporary buffer
	free(versaloon_usbtoxxx_info.buff);
	versaloon_usbtoxxx_info.buff = NULL;
	
	usbtoxxx_info = &versaloon_usbtoxxx_info;
	usbtoxxx_info->comm = t->comm;
	if (usbtoxxx_init())
	{
		err = VSFERR_FAIL;
		goto versaloon_init_fail;
	}
	
	return VSFERR_NONE;
versaloon_init_fail:
	versaloon_fini(t);
	return err;
}

static vsf_err_t versaloon_reset(void *p)
{
	REFERENCE_PARAMETER(p);
	return VSFERR_NONE;
}

// Commit
static vsf_err_t versaloon_peripheral_commit(void)
{
	return usbtoxxx_execute_command();
}

// tick clock
vsf_err_t versaloon_tickclk_init(void)
{
	return VSFERR_NONE;
}

vsf_err_t versaloon_tickclk_fini(void)
{
	return VSFERR_NONE;
}

vsf_err_t versaloon_tickclk_start(void)
{
	return VSFERR_NONE;
}

vsf_err_t versaloon_tickclk_stop(void)
{
	// not supported
	return VSFERR_FAIL;
}

uint32_t versaloon_tickclk_get_count(void)
{
	return (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
}

struct interfaces_info_t versaloon_interfaces =
{
	&versaloon_libusb_comm,
	
	{	// core
		versaloon_init,
		versaloon_fini,
		versaloon_reset
	},
#if IFS_FLASH_EN
	{
	},
#endif
#if IFS_GPIO_EN
	{	// gpio
		usbtogpio_init,
		usbtogpio_fini,
		usbtogpio_config_pin,
		usbtogpio_config,
		usbtogpio_set,
		usbtogpio_clear,
		usbtogpio_out,
		usbtogpio_in
	},
#endif
#if IFS_TIMER_EN
	{	// timer
	},
#endif
#if IFS_EINT_EN
	{	// eint
	},
#endif
#if IFS_USART_EN
	{	// usart
		usbtousart_init,
		usbtousart_fini,
		usbtousart_config,
		usbtousart_send,
		usbtousart_receive,
		usbtousart_status
	},
#endif
#if IFS_SPI_EN
	{	// spi
		usbtospi_init,
		usbtospi_fini,
		usbtospi_get_ability,
		usbtospi_enable,
		usbtospi_disable,
		usbtospi_config,
		usbtospi_select,
		usbtospi_deselect,
		NULL, NULL, NULL, NULL,
		usbtospi_io,
		NULL, NULL, NULL
	},
#endif
#if IFS_ADC_EN
	{
		// adc
		usbtoadc_init,
		usbtoadc_fini,
		usbtoadc_config,
		usbtoadc_config_channel,
		usbtoadc_calibrate,
		NULL, NULL, NULL,
		usbtoadc_sample
	},
#endif
#if IFS_IIC_EN
	{	// i2c
		usbtoi2c_init,
		usbtoi2c_fini,
		usbtoi2c_config,
		usbtoi2c_read,
		usbtoi2c_write
	},
#endif
#if IFS_USBD_EN
	{	// usbd
	},
#endif
#if IFS_PWM_EN
	{	// pwm
		usbtopwm_init,
		usbtopwm_fini,
		usbtopwm_config_mode,
		usbtopwm_config_freq,
		usbtopwm_out,
		usbtopwm_in
	},
#endif
#if IFS_MICROWIRE_EN
	{	// microwire
		usbtomicrowire_init,
		usbtomicrowire_fini,
		usbtomicrowire_config,
		usbtomicrowire_transport,
		usbtomicrowire_poll
	},
#endif
#if IFS_NAND_EN
	{
		// nand
		usbtonand_init,
		usbtonand_fini,
		usbtonand_write_cmd,
		usbtonand_write_addr,
		usbtonand_write_data,
		usbtonand_read_data
	},
#endif
#if IFS_EBI_EN
	{
		// ebi
		usbtoebi_init,
		usbtoebi_fini,
		usbtoebi_config,
		NULL, NULL, NULL, NULL, NULL, NULL, NULL,
		NULL,
		usbtoebi_isready,
		usbtoebi_read,
		usbtoebi_write,
		NULL, NULL, NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	},
#endif
#if IFS_SDIO_EN
	{	// sdio
	},
#endif
	{	// tickclk
		versaloon_tickclk_init,
		versaloon_tickclk_fini,
		versaloon_tickclk_start,
		versaloon_tickclk_stop,
		versaloon_tickclk_get_count,
	},
	{	// delay
		usbtodelay_init,
		usbtodelay_delayms,
		usbtodelay_delayus
	},
	versaloon_peripheral_commit
};

