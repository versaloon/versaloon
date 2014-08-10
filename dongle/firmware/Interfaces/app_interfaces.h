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

#include "PowerExt/PowerExt.h"

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

struct interface_issp_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*enter_program_mode)(uint8_t index, uint8_t mode);
	vsf_err_t (*leave_program_mode)(uint8_t index, uint8_t mode);
	vsf_err_t (*wait_and_poll)(uint8_t index);
	vsf_err_t (*vector)(uint8_t index, uint8_t operate, uint8_t addr, 
						uint8_t data, uint8_t *buf);
};

struct interface_swd_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint8_t trn, uint16_t retry, uint16_t dly);
	vsf_err_t (*seqout)(uint8_t index, uint8_t *data, uint16_t bitlen);
	vsf_err_t (*seqin)(uint8_t index, uint8_t *data, uint16_t bitlen);
	vsf_err_t (*transact)(uint8_t index, uint8_t request, uint32_t *data, 
							uint8_t *ack);
};

struct jtag_pos_t
{
	uint8_t ub;		// units before
	uint8_t ua;		// bits before
	uint16_t bb;	// units after
	uint16_t ba;	// bits after
};

enum jtag_irdr_t
{
	JTAG_SCANTYPE_IR,
	JTAG_SCANTYPE_DR
};
typedef vsf_err_t (*jtag_callback_t)(uint8_t index, enum jtag_irdr_t cmd, 
										uint32_t ir, uint8_t *dest_buffer, 
										uint8_t *src_buffer, uint16_t bytelen, 
										uint16_t *processed);
struct interface_jtag_hl_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config_speed)(uint8_t index, uint32_t kHz);
	vsf_err_t (*config_daisychain)(uint8_t index, struct jtag_pos_t *pos);
	vsf_err_t (*config)(uint8_t index, uint32_t kHz, struct jtag_pos_t *pos);
	vsf_err_t (*tms)(uint8_t index, uint8_t* tms, uint16_t bitlen);
	vsf_err_t (*runtest)(uint8_t index, uint32_t cycles);
	vsf_err_t (*ir)(uint8_t index, uint8_t *ir, uint16_t bitlen, uint8_t idle, 
					uint8_t want_ret);
	vsf_err_t (*dr)(uint8_t index, uint8_t *dr, uint16_t bitlen, uint8_t idle, 
					uint8_t want_ret);
	vsf_err_t (*register_callback)(uint8_t index, jtag_callback_t send_callback, 
									jtag_callback_t receive_callback);
};

struct interface_jtag_ll_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint32_t kHz);
	vsf_err_t (*tms)(uint8_t index, uint8_t *tms, uint8_t bytelen);
	vsf_err_t (*tms_clocks)(uint8_t index, uint32_t bytelen, uint8_t tms);
	vsf_err_t (*scan)(uint8_t index, uint8_t* data, uint16_t bitlen, 
						uint8_t tms_before_valid, uint8_t tms_before, 
						uint8_t tms_after0, uint8_t tms_after1);
};

struct interface_jtag_raw_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint32_t kHz);
	vsf_err_t (*execute)(uint8_t index, uint8_t* tdi, uint8_t* tms, 
							uint8_t *tdo, uint32_t bitlen);
};

struct interface_msp430jtag_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint8_t has_test);
	vsf_err_t (*ir)(uint8_t index, uint8_t *ir, uint8_t want_ret);
	vsf_err_t (*dr)(uint8_t index, uint32_t *dr, uint8_t bitlen, 
					uint8_t want_ret);
	vsf_err_t (*tclk)(uint8_t index, uint8_t value);
	vsf_err_t (*tclk_strobe)(uint8_t index, uint16_t cnt);
	vsf_err_t (*reset)(uint8_t index);
	vsf_err_t (*poll)(uint8_t index, uint32_t dr, uint32_t mask, uint32_t value, 
						uint8_t len, uint16_t poll_cnt, uint8_t toggle_tclk);
};

struct interface_msp430sbw_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint8_t has_test);
	vsf_err_t (*ir)(uint8_t index, uint8_t *ir, uint8_t want_ret);
	vsf_err_t (*dr)(uint8_t index, uint32_t *dr, uint8_t len, uint8_t want_ret);
	vsf_err_t (*tclk)(uint8_t index, uint8_t value);
	vsf_err_t (*tclk_strobe)(uint8_t index, uint16_t cnt);
	vsf_err_t (*reset)(uint8_t index);
	vsf_err_t (*poll)(uint8_t index, uint32_t dr, uint32_t mask, uint32_t value, 
						uint8_t len, uint16_t poll_cnt, uint8_t toggle_tclk);
};

struct interface_c2_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*addr_write)(uint8_t index, uint8_t addr);
	vsf_err_t (*addr_read)(uint8_t index, uint8_t *data);
	vsf_err_t (*data_write)(uint8_t index, uint8_t *data, uint8_t len);
	vsf_err_t (*data_read)(uint8_t index, uint8_t *data, uint8_t len);
};

struct interface_lpcicp_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*enter_program_mode)(uint8_t index);
	vsf_err_t (*in)(uint8_t index, uint8_t *buff, uint16_t len);
	vsf_err_t (*out)(uint8_t index, uint8_t *buff, uint16_t len);
	vsf_err_t (*poll_ready)(uint8_t index, uint8_t data, uint8_t *ret, 
							uint8_t setmask, uint8_t clearmask, 
							uint16_t pollcnt);
};

struct interface_swim_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint8_t mHz, uint8_t cnt0, uint8_t cnt1);
	vsf_err_t (*srst)(uint8_t index);
	vsf_err_t (*wotf)(uint8_t index, uint8_t *data, uint16_t bytelen, 
						uint32_t addr);
	vsf_err_t (*rotf)(uint8_t index, uint8_t *data, uint16_t bytelen, 
						uint32_t addr);
	vsf_err_t (*sync)(uint8_t index, uint8_t mHz);
	vsf_err_t (*enable)(uint8_t index);
};

struct interface_bdm_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*sync)(uint8_t index, uint16_t *khz);
	vsf_err_t (*transact)(uint8_t index, uint8_t *out, uint8_t outlen, 
							uint8_t *in, uint8_t inlen, uint8_t delay, 
							uint8_t ack);
};

struct interface_dusi_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint32_t kHz, uint8_t mode);
	vsf_err_t (*io)(uint8_t index, uint8_t *mo, uint8_t *mi, uint8_t *so, 
					uint8_t *si, uint32_t bitlen);
};

struct interface_target_voltage_t
{
	vsf_err_t (*get)(uint8_t index, uint16_t *voltage);
	vsf_err_t (*set)(uint8_t index, uint16_t voltage);
	vsf_err_t (*poll)(uint8_t index);
};

enum poll_check_type_t
{
	POLL_CHECK_EQU,
	POLL_CHECK_UNEQU
};
struct interface_poll_t
{
	vsf_err_t (*start)(uint16_t retry, uint16_t interval_us);
	vsf_err_t (*end)(void);
	vsf_err_t (*checkok)(enum poll_check_type_t type, uint16_t offset,

							uint8_t size, uint32_t mask, uint32_t value);
	vsf_err_t (*checkfail)(enum poll_check_type_t type, uint16_t offset,

							uint8_t size, uint32_t mask, uint32_t value);
	vsf_err_t (*verifybuff)(uint16_t offset, uint16_t size, uint8_t *buff);
};

struct app_interfaces_info_t
{
	char *name;
	
	vsf_err_t (*init)(void *p);
	vsf_err_t (*fini)(void);
	vsf_err_t (*peripheral_commit)(void);
	
	uint64_t support_mask;
#if	INTERFACE_GPIO_EN
	struct interface_gpio_t gpio;
#endif
#if INTERFACE_CLKO_EN
	struct interface_clko_t clko;
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
#if POWER_OUT_EN
	struct interface_target_voltage_t target_voltage;
#endif
#if	INTERFACE_ISSP_EN
	struct interface_issp_t issp;
#endif
#if	INTERFACE_SWD_EN
	struct interface_swd_t swd;
#endif
#if	INTERFACE_JTAG_EN
	struct interface_jtag_hl_t jtag_hl;
#endif
#if	INTERFACE_JTAG_EN
	struct interface_jtag_ll_t jtag_ll;
#endif
#if	INTERFACE_JTAG_EN
	struct interface_jtag_raw_t jtag_raw;
#endif
#if	INTERFACE_MSP430_JTAG_EN
	struct interface_msp430jtag_t msp430jtag;
#endif
#if	INTERFACE_MSP430_SBW_EN
	struct interface_msp430sbw_t msp430sbw;
#endif
#if	INTERFACE_C2_EN
	struct interface_c2_t c2;
#endif
#if	INTERFACE_LPC_ICP_EN
	struct interface_lpcicp_t lpcicp;
#endif
#if	INTERFACE_SWIM_EN
	struct interface_swim_t swim;
#endif
#if	INTERFACE_BDM_EN
	struct interface_bdm_t bdm;
#endif
#if	INTERFACE_DUSI_EN
	struct interface_dusi_t dusi;
#endif
#if INTERFACE_MICROWIRE_EN
	struct interface_microwire_t microwire;
#endif
#if INTERFACE_SDIO_EN
	struct interface_sdio_t sdio;
#endif
#if INTERFACE_ADC_EN
	struct interface_adc_t adc;
#endif
	struct interface_tickclk_t tickclk;
	struct interface_delay_t delay;
	struct interface_poll_t poll;
};

extern const struct app_interfaces_info_t app_interfaces;
extern struct app_interfaces_info_t *interfaces;

vsf_err_t interface_assert(struct app_interfaces_info_t **ifs);
char* get_interface_name(uint64_t i);

#endif	// __APP_INTERFACES_H_INCLUDED__
