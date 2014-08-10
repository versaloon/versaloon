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


// USB_TO_CLKO
vsf_err_t usbtoclko_init(uint8_t index);
vsf_err_t usbtoclko_fini(uint8_t index);
vsf_err_t usbtoclko_config(uint8_t index, uint32_t kHz);
vsf_err_t usbtoclko_enable(uint8_t index);
vsf_err_t usbtoclko_disable(uint8_t index);




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
vsf_err_t usbtousart_init(uint8_t index);
vsf_err_t usbtousart_fini(uint8_t index);
vsf_err_t usbtousart_config(uint8_t index, uint32_t baudrate,
							uint8_t datalength, uint8_t mode);
vsf_err_t usbtousart_send(uint8_t index, uint8_t *buf, uint16_t len);
vsf_err_t usbtousart_receive(uint8_t index, uint8_t *buf,
								uint16_t len);
vsf_err_t usbtousart_status(uint8_t index,
							struct usart_status_t *status);

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



// USB_TO_ISSP
vsf_err_t usbtoissp_init(uint8_t index);
vsf_err_t usbtoissp_fini(uint8_t index);
vsf_err_t usbtoissp_enter_program_mode(uint8_t index, uint8_t mode);
vsf_err_t usbtoissp_leave_program_mode(uint8_t index, uint8_t mode);
vsf_err_t usbtoissp_wait_and_poll(uint8_t index);
vsf_err_t usbtoissp_vector(uint8_t index, uint8_t operate,
							uint8_t addr,uint8_t data, uint8_t *buf);



// USB_TO_LPCICP
vsf_err_t usbtolpcicp_init(uint8_t index);
vsf_err_t usbtolpcicp_fini(uint8_t index);
vsf_err_t usbtolpcicp_config(uint8_t index);
vsf_err_t usbtolpcicp_enter_program_mode(uint8_t index);
vsf_err_t usbtolpcicp_in(uint8_t index, uint8_t *buff, uint16_t len);
vsf_err_t usbtolpcicp_out(uint8_t index, uint8_t *buff, uint16_t len);
vsf_err_t usbtolpcicp_poll_ready(uint8_t index, uint8_t data,
		uint8_t *ret, uint8_t setmask, uint8_t clearmask, uint16_t pollcnt);



// USB_TO_JTAG_LL
vsf_err_t usbtojtagll_init(uint8_t index);
vsf_err_t usbtojtagll_fini(uint8_t index);
vsf_err_t usbtojtagll_config(uint8_t index, uint32_t kHz);
vsf_err_t usbtojtagll_tms(uint8_t index, uint8_t *tms,
							uint8_t bytelen);
vsf_err_t usbtojtagll_tms_clocks(uint8_t index, uint32_t bytelen,
									uint8_t tms);
vsf_err_t usbtojtagll_scan(uint8_t index, uint8_t* data,
							uint16_t bitlen, uint8_t tms_before_valid,
							uint8_t tms_before, uint8_t tms_after0,
							uint8_t tms_after1);



// USB_TO_JTAG_HL
vsf_err_t usbtojtaghl_init(uint8_t index);
vsf_err_t usbtojtaghl_fini(uint8_t index);
vsf_err_t usbtojtaghl_config(uint8_t index, uint32_t kHz,
								struct jtag_pos_t *pos);
vsf_err_t usbtojtaghl_ir(uint8_t index, uint8_t *ir, uint16_t bitlen,
						  uint8_t idle, uint8_t want_ret);
vsf_err_t usbtojtaghl_dr(uint8_t index, uint8_t *dr, uint16_t bitlen,
						  uint8_t idle, uint8_t want_ret);
vsf_err_t usbtojtaghl_tms(uint8_t index, uint8_t *tms, uint16_t bitlen);
vsf_err_t usbtojtaghl_runtest(uint8_t index, uint32_t cycles);
vsf_err_t usbtojtaghl_register_callback(uint8_t index,
			jtag_callback_t send_callback, jtag_callback_t receive_callback);



// USB_TO_JTAG_RAW
vsf_err_t usbtojtagraw_init(uint8_t index);
vsf_err_t usbtojtagraw_fini(uint8_t index);
vsf_err_t usbtojtagraw_config(uint8_t index, uint32_t kHz);
vsf_err_t usbtojtagraw_execute(uint8_t index, uint8_t *tdi,
								uint8_t *tms, uint8_t *tdo, uint32_t bitlen);




// USB_TO_C2
vsf_err_t usbtoc2_init(uint8_t index);
vsf_err_t usbtoc2_fini(uint8_t index);
vsf_err_t usbtoc2_writeaddr(uint8_t index, uint8_t addr);
vsf_err_t usbtoc2_readaddr(uint8_t index, uint8_t *data);
vsf_err_t usbtoc2_writedata(uint8_t index, uint8_t *buf, uint8_t len);
vsf_err_t usbtoc2_readdata(uint8_t index, uint8_t *buf, uint8_t len);



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



// USB_TO_MSP430_JTAG
vsf_err_t usbtomsp430jtag_init(uint8_t index);
vsf_err_t usbtomsp430jtag_fini(uint8_t index);
vsf_err_t usbtomsp430jtag_config(uint8_t index, uint8_t has_test);
vsf_err_t usbtomsp430jtag_ir(uint8_t index, uint8_t *ir,
								uint8_t want_ret);
vsf_err_t usbtomsp430jtag_dr(uint8_t index, uint32_t *dr,
								uint8_t bitlen, uint8_t want_ret);
vsf_err_t usbtomsp430jtag_tclk(uint8_t index, uint8_t value);
vsf_err_t usbtomsp430jtag_tclk_strobe(uint8_t index, uint16_t cnt);
vsf_err_t usbtomsp430jtag_reset(uint8_t index);
vsf_err_t usbtomsp430jtag_poll(uint8_t index, uint32_t dr,
								uint32_t mask, uint32_t value, uint8_t len,
								uint16_t poll_cnt, uint8_t toggle_tclk);



// USB_TO_MSP430_SBW
vsf_err_t usbtomsp430sbw_init(uint8_t index);
vsf_err_t usbtomsp430sbw_fini(uint8_t index);
vsf_err_t usbtomsp430sbw_config(uint8_t index, uint8_t has_test);
vsf_err_t usbtomsp430sbw_ir(uint8_t index, uint8_t *ir,
							uint8_t want_ret);
vsf_err_t usbtomsp430sbw_dr(uint8_t index, uint32_t *dr,
							uint8_t bitlen, uint8_t want_ret);
vsf_err_t usbtomsp430sbw_tclk(uint8_t index, uint8_t value);
vsf_err_t usbtomsp430sbw_tclk_strobe(uint8_t index, uint16_t cnt);
vsf_err_t usbtomsp430sbw_reset(uint8_t index);
vsf_err_t usbtomsp430sbw_poll(uint8_t index, uint32_t dr,
								uint32_t mask, uint32_t value, uint8_t len,
								uint16_t poll_cnt, uint8_t toggle_tclk);



// USB_TO_POWER
vsf_err_t usbtopwr_init(uint8_t index);
vsf_err_t usbtopwr_fini(uint8_t index);
vsf_err_t usbtopwr_config(uint8_t index);
vsf_err_t usbtopwr_set(uint8_t index, uint16_t mV);
vsf_err_t usbtopwr_get(uint8_t index, uint16_t *mV);



// USB_TO_POLL
vsf_err_t usbtopoll_start(uint16_t retry_cnt, uint16_t interval_us);
vsf_err_t usbtopoll_end(void);
vsf_err_t usbtopoll_checkok(uint8_t equ, uint16_t offset, uint8_t size,
							uint32_t mask, uint32_t value);
vsf_err_t usbtopoll_checkfail(uint8_t equ, uint16_t offset, uint8_t size,
								uint32_t mask, uint32_t value);
vsf_err_t usbtopoll_verifybuff(uint16_t offset, uint16_t size, uint8_t *buff);



// USB_TO_SWD
vsf_err_t usbtoswd_init(uint8_t index);
vsf_err_t usbtoswd_fini(uint8_t index);
vsf_err_t usbtoswd_config(uint8_t index, uint8_t trn, uint16_t retry,
						   uint16_t dly);
vsf_err_t usbtoswd_seqout(uint8_t index, uint8_t *data,
							uint16_t bitlen);
vsf_err_t usbtoswd_seqin(uint8_t index, uint8_t *data,
							uint16_t bitlen);
vsf_err_t usbtoswd_transact(uint8_t index, uint8_t request,
							uint32_t *data, uint8_t *ack);



// USB_TO_SWIM
vsf_err_t usbtoswim_init(uint8_t index);
vsf_err_t usbtoswim_fini(uint8_t index);
vsf_err_t usbtoswim_config(uint8_t index, uint8_t mHz, uint8_t cnt0,
							uint8_t cnt1);
vsf_err_t usbtoswim_srst(uint8_t index);
vsf_err_t usbtoswim_wotf(uint8_t index, uint8_t *data,
							uint16_t bytelen, uint32_t addr);
vsf_err_t usbtoswim_rotf(uint8_t index, uint8_t *data,
							uint16_t bytelen, uint32_t addr);
vsf_err_t usbtoswim_sync(uint8_t index, uint8_t mHz);
vsf_err_t usbtoswim_enable(uint8_t index);




// USB_TO_BDM
vsf_err_t usbtobdm_init(uint8_t index);
vsf_err_t usbtobdm_fini(uint8_t index);
vsf_err_t usbtobdm_sync(uint8_t index, uint16_t *khz);
vsf_err_t usbtobdm_transact(uint8_t index, uint8_t *out,
	uint8_t outlen, uint8_t *in, uint8_t inlen, uint8_t delay, uint8_t ack);



// USB_TO_DUSI
vsf_err_t usbtodusi_init(uint8_t index);
vsf_err_t usbtodusi_fini(uint8_t index);
vsf_err_t usbtodusi_config(uint8_t index, uint32_t kHz, uint8_t mode);
vsf_err_t usbtodusi_io(uint8_t index, uint8_t *mo, uint8_t *mi,
						uint8_t *so, uint8_t *si, uint32_t bitlen);



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

