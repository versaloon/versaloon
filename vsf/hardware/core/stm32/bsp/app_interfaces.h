/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       interfaces.h                                              *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    interfaces header file                                    *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#ifndef __APP_INTERFACES_H_INCLUDED__
#define __APP_INTERFACES_H_INCLUDED__

#include "app_cfg.h"
#include "interfaces.h"
#include "app_interfaces_const.h"

#define GPIO_CNF_IN_ANALOG			(0x00 << 2)
#define GPIO_CNF_IN_FLOAT			(0x01 << 2)
#define GPIO_CNF_IN_PULL			(0x02 << 2)
#define GPIO_CNF_OUT_PUSHPULL		(0x00 << 2)
#define GPIO_CNF_OUT_OPENDRAIN		(0x01 << 2)
#define GPIO_CNF_OUT_AF_PUSHPULL	(0x02 << 2)
#define GPIO_CNF_OUT_AF_OPENDRAIN	(0x03 << 2)
#define GPIO_MODE_IN				(0x00 << 0)
#define GPIO_MODE_OUT_10M			(0x01 << 0)
#define GPIO_MODE_OUT_2M			(0x02 << 0)
#define GPIO_MODE_OUT_50M			(0x03 << 0)
#define GPIO_INPUT_PULLUP			0x30
#define GPIO_INPUT_PULLDOWN			0x20

#define GPIO_MODE_IN_FLOATING		(GPIO_MODE_IN | GPIO_CNF_IN_FLOAT)
#define GPIO_MODE_AIN				(GPIO_MODE_IN | GPIO_CNF_IN_ANALOG)
#define GPIO_MODE_IPD				(GPIO_MODE_IN | GPIO_CNF_IN_PULL | GPIO_INPUT_PULLDOWN)
#define GPIO_MODE_IPU				(GPIO_MODE_IN | GPIO_CNF_IN_PULL | GPIO_INPUT_PULLUP)
#define GPIO_MODE_OUT_OD			(GPIO_CNF_OUT_OPENDRAIN | GPIO_MODE_OUT_50M)
#define GPIO_MODE_OUT_PP			(GPIO_CNF_OUT_PUSHPULL | GPIO_MODE_OUT_50M)
#define GPIO_MODE_AF_OD				(GPIO_CNF_OUT_AF_OPENDRAIN | GPIO_MODE_OUT_50M)
#define GPIO_MODE_AF_PP				(GPIO_CNF_OUT_AF_PUSHPULL | GPIO_MODE_OUT_50M)

#define GPIO_PIN_0					0
#define GPIO_PIN_1					1
#define GPIO_PIN_2					2
#define GPIO_PIN_3					3
#define GPIO_PIN_4					4
#define GPIO_PIN_5					5
#define GPIO_PIN_6					6
#define GPIO_PIN_7					7
#define GPIO_PIN_8					8
#define GPIO_PIN_9					9
#define GPIO_PIN_10					10
#define GPIO_PIN_11					11
#define GPIO_PIN_12					12
#define GPIO_PIN_13					13
#define GPIO_PIN_14					14
#define GPIO_PIN_15					15
#define GPIO_PIN_GetMask(p)			(((uint32_t)1) << (p))

#define GPIO_SetPins(port, pin)		(port)->BSRR = GPIO_PIN_GetMask(pin)
#define GPIO_ClrPins(port, pin)		(port)->BRR = GPIO_PIN_GetMask(pin)
#define GPIO_GetOutPins(port, pin)	((port)->ODR & GPIO_PIN_GetMask(pin))
#define GPIO_GetInPins(port, pin)	((port)->IDR & GPIO_PIN_GetMask(pin))
void GPIO_SetMode(GPIO_TypeDef* GPIOx, uint8_t pin, uint8_t mode);

struct usart_status_t
{
	uint32_t tx_buff_avail;
	uint32_t tx_buff_size;
	uint32_t rx_buff_avail;
	uint32_t rx_buff_size;
};
struct app_interface_usart_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint32_t baudrate, uint8_t datalength, 
						uint8_t mode);
	vsf_err_t (*send)(uint8_t index, uint8_t *buf, uint16_t len);
	vsf_err_t (*receive)(uint8_t index, uint8_t *buf, uint16_t len);
	vsf_err_t (*status)(uint8_t index, struct usart_status_t *status);
	vsf_err_t (*poll)(uint8_t index);
};

struct app_interfaces_info_t
{
	char *name;
	
	vsf_err_t (*peripheral_commit)(void);
	
	uint64_t support_mask;
	struct interface_core_t core;
#if	INTERFACE_GPIO_EN
	struct interface_gpio_t gpio;
#endif
#if	INTERFACE_USART_EN
	struct app_interface_usart_t usart;
#endif
#if	INTERFACE_SPI_EN
	struct interface_spi_t spi;
#endif
#if INTERFACE_EBI_EN
	struct interface_ebi_t ebi;
#endif
#if	INTERFACE_IIC_EN
	struct interface_i2c_t i2c;
#endif
#if INTERFACE_PWM_EN
	struct interface_pwm_t pwm;
#endif
#if INTERFACE_SDIO_EN
	struct interface_sdio_t sdio;
#endif
#if INTERFACE_ADC_EN
	struct interface_adc_t adc;
#endif
	struct interface_tickclk_t tickclk;
	struct interface_delay_t delay;
};

extern const struct app_interfaces_info_t app_interfaces;
extern struct app_interfaces_info_t *interfaces;

#endif	// __APP_INTERFACES_H_INCLUDED__
