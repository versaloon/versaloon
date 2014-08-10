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
#ifndef __USBTOXXX_H_INCLUDED__
#define __USBTOXXX_H_INCLUDED__

#include "usbtoxxx_cfg.h"
#include "interfaces.h"

#define USB_TO_XXX_ABILITIES_LEN			12

typedef vsf_err_t (*usbtoxxx_callback_t)(void *, uint8_t *, uint8_t *);
struct usbtoxxx_want_pos_t
{
	uint16_t offset;
	uint16_t size;
	uint8_t *buff;
	struct usbtoxxx_want_pos_t *next;
};
struct usbtoxxx_pending_t
{
	uint8_t type;
	uint8_t cmd;
	uint16_t want_data_pos;
	uint16_t want_data_size;
	uint16_t actual_data_size;
	uint8_t *data_buffer;
	uint8_t collect;
	uint32_t id;
	struct usbtoxxx_want_pos_t *pos;
	void *extra_data;
	usbtoxxx_callback_t callback;
};

struct usbtoxxx_context_t
{
	uint8_t type_pre;
	uint8_t *usbtoxxx_buffer;
	uint16_t usbtoxxx_current_cmd_index;
	uint16_t usbtoxxx_buffer_index;
	uint16_t usbtoxxx_pending_idx;
};

struct usbtoxxx_info_t
{
	// buffer
	uint8_t *buff;
	uint8_t *cmd_buff;
	uint16_t buff_len;
	
	// transact
	struct interfaces_comm_t *comm;
	
	// private
	uint8_t abilities[USB_TO_XXX_ABILITIES_LEN];
	uint8_t type_pre;
	uint16_t buffer_index;
	uint16_t current_cmd_index;
	uint8_t *usbtoxxx_buffer;
	
	uint16_t collect_index;
	uint8_t collect_cmd;
	uint8_t poll_nesting;
	
	struct usbtoxxx_context_t poll_context;
	
	struct usbtoxxx_pending_t pending[USBTOXXX_CFG_MAX_PENDING_NUMBER];
	uint16_t pending_idx;
	
	uint32_t pending_id;
	usbtoxxx_callback_t callback;
	void *extra_data;
	struct usbtoxxx_want_pos_t *want_pos;
	
	bool buff_allocated;
	bool cmd_buff_allocated;
};

extern struct usbtoxxx_info_t *usbtoxxx_info;
vsf_err_t usbtoxxx_init(void);
vsf_err_t usbtoxxx_fini(void);
vsf_err_t usbtoxxx_execute_command(void);

bool usbtoxxx_interface_supported(uint8_t cmd);

// USB_TO_INFO
vsf_err_t usbtoinfo_get_abilities(uint8_t abilities[USB_TO_XXX_ABILITIES_LEN]);

// USB_TO_DELAY
vsf_err_t usbtodelay_init(void);
vsf_err_t usbtodelay_delayms(uint16_t ms);
vsf_err_t usbtodelay_delayus(uint16_t us);



// ADC
vsf_err_t usbtoadc_init(uint8_t index);
vsf_err_t usbtoadc_fini(uint8_t index);
vsf_err_t usbtoadc_config(uint8_t index, uint32_t clock_hz,
							uint8_t mode);
vsf_err_t usbtoadc_config_channel(uint8_t index, uint8_t channel,
									uint8_t cycles);
vsf_err_t usbtoadc_calibrate(uint8_t index, uint8_t channel);
vsf_err_t usbtoadc_sample(uint8_t index, uint8_t channel,
							uint32_t *voltage);



// USB_TO_USART
// TODO: make USB_TO_USART compatible with APIs in firmware

// USB_TO_SPI
vsf_err_t usbtospi_init(uint8_t index);
vsf_err_t usbtospi_fini(uint8_t index);
vsf_err_t usbtospi_get_ability(uint8_t index, struct spi_ability_t *ability);
vsf_err_t usbtospi_enable(uint8_t index);
vsf_err_t usbtospi_disable(uint8_t index);
vsf_err_t usbtospi_config(uint8_t index, uint32_t kHz, uint8_t mode);
vsf_err_t usbtospi_select(uint8_t index, uint8_t cs);
vsf_err_t usbtospi_deselect(uint8_t index, uint8_t cs);
vsf_err_t usbtospi_io(uint8_t index, uint8_t *out, uint8_t *in,
						uint32_t bytelen);



// USB_TO_EBI
vsf_err_t usbtoebi_init(uint8_t index);
vsf_err_t usbtoebi_fini(uint8_t index);
vsf_err_t usbtoebi_config(uint8_t index, uint8_t target_index, void *param);
vsf_err_t usbtoebi_isready(uint8_t index, uint8_t target_index);
vsf_err_t usbtoebi_read(uint8_t index, uint8_t target_index,
			uint32_t address, uint8_t data_size, uint8_t *buff, uint32_t count);
vsf_err_t usbtoebi_write(uint8_t index, uint8_t target_index,
			uint32_t address, uint8_t data_size, uint8_t *buff, uint32_t count);



// USB_TO_GPIO
vsf_err_t usbtogpio_init(uint8_t index);
vsf_err_t usbtogpio_fini(uint8_t index);
vsf_err_t usbtogpio_config_pin(uint8_t index, uint8_t pin_idx, uint8_t mode);
vsf_err_t usbtogpio_config(uint8_t index, uint32_t mask,
							uint32_t dir_mask, uint32_t pull_en_mask,
							uint32_t input_pull_mask);
vsf_err_t usbtogpio_set(uint8_t index, uint32_t mask);
vsf_err_t usbtogpio_clear(uint8_t index, uint32_t mask);
vsf_err_t usbtogpio_in(uint8_t index, uint32_t mask, uint32_t *value);
vsf_err_t usbtogpio_out(uint8_t index, uint32_t mask, uint32_t value);




// USB_TO_I2C
vsf_err_t usbtoi2c_init(uint8_t index);
vsf_err_t usbtoi2c_fini(uint8_t index);
vsf_err_t usbtoi2c_config(uint8_t index, uint16_t kHz,
							uint16_t byte_interval, uint16_t max_dly);
vsf_err_t usbtoi2c_read(uint8_t index, uint16_t chip_addr,
						uint8_t *data, uint16_t data_len, uint8_t stop,
						bool nacklast);
vsf_err_t usbtoi2c_write(uint8_t index, uint16_t chip_addr,
							uint8_t *data, uint16_t data_len, uint8_t stop);




// USB_TO_MICROWIRE
vsf_err_t usbtomicrowire_init(uint8_t index);
vsf_err_t usbtomicrowire_fini(uint8_t index);
vsf_err_t usbtomicrowire_config(uint8_t index, uint16_t kHz,
								uint8_t sel_polarity);
vsf_err_t usbtomicrowire_transport(uint8_t index,
									uint32_t opcode, uint8_t opcode_bitlen,
									uint32_t addr, uint8_t addr_bitlen,
									uint32_t data, uint8_t data_bitlen,
									uint8_t *reply, uint8_t reply_bitlen);
vsf_err_t usbtomicrowire_poll(uint8_t index, uint16_t interval_us,
								uint16_t retry_cnt);



// USB_TO_PWM
vsf_err_t usbtopwm_init(uint8_t index);
vsf_err_t usbtopwm_fini(uint8_t index);
vsf_err_t usbtopwm_config_mode(uint8_t index, uint8_t mode);
vsf_err_t usbtopwm_config_freq(uint8_t index, uint16_t kHz);
vsf_err_t usbtopwm_out(uint8_t index, uint16_t count, uint16_t *rate);
vsf_err_t usbtopwm_in(uint8_t index, uint16_t count, uint16_t *rate);



// USB_TO_NAND
vsf_err_t usbtonand_init(uint8_t index);
vsf_err_t usbtonand_fini(uint8_t index);
vsf_err_t usbtonand_config(uint8_t index, struct nand_info_t *param);
vsf_err_t usbtonand_write_cmd(uint8_t index, uint8_t *cmd, uint8_t bytelen);
vsf_err_t usbtonand_write_addr(uint8_t index, uint8_t *addr, uint8_t bytelen);
vsf_err_t usbtonand_write_data(uint8_t index, uint8_t *data, uint32_t bytelen);
vsf_err_t usbtonand_read_data(uint8_t index, uint8_t *data, uint32_t bytelen);

#endif /* __USBTOXXX_H_INCLUDED__ */

