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

#ifndef __INTERFACES_H_INCLUDED__
#define __INTERFACES_H_INCLUDED__

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdint.h>

#include "app_type.h"
#include "vsf_err.h"
#include "interfaces_const.h"
#include "versaloon/versaloon.h"
#include "virtualinterface/vi_stm32/vi_stm32.h"

#define IFS_DUMMY_PORT						0xFF

char* get_interface_name(uint64_t i);

struct interface_core_t
{
	vsf_err_t (*init)(void *p);
	vsf_err_t (*fini)(void *p);
	vsf_err_t (*reset)(void *p);
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

struct usart_status_t
{
	uint32_t tx_buff_avail;
	uint32_t tx_buff_size;
	uint32_t rx_buff_avail;
	uint32_t rx_buff_size;
};

struct interface_usart_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint32_t baudrate, uint8_t datalength,
						uint8_t mode);
	vsf_err_t (*send)(uint8_t index, uint8_t *buf, uint16_t len);
	vsf_err_t (*receive)(uint8_t index, uint8_t *buf, uint16_t len);
	vsf_err_t (*status)(uint8_t index, struct usart_status_t *status);
};

struct spi_ability_t
{
	uint32_t max_freq_hz;
	uint32_t min_freq_hz;
};
struct interface_spi_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*get_ability)(uint8_t index, struct spi_ability_t *ability);
	vsf_err_t (*enable)(uint8_t index);
	vsf_err_t (*disable)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint32_t kHz, uint8_t mode);
	vsf_err_t (*select)(uint8_t index, uint8_t cs);
	vsf_err_t (*deselect)(uint8_t index, uint8_t cs);
	vsf_err_t (*io)(uint8_t index, uint8_t *out, uint8_t *in, uint32_t bytelen);
};

struct ebi_info_t
{
	uint8_t data_width;
	enum wait_signal_t
	{
		EBI_WAIT_NONE = 0,
		EBI_WAIT_POLHIGH_VI = 1,
		EBI_WAIT_POLHIGH_VN = 2,
		EBI_WAIT_POLLOW_VI = 3,
		EBI_WAIT_POLLOW_VN = 4
	} wait_signal;
	
	// mux_addr_mask is used when a multiplexer(eg. 74LS138) is used.
	// If no multiplexer is used, set mux_addr_mask to 0.
	// mux_addr_mask defines the address mask to select current chip.
	uint32_t mux_addr_mask;
};
struct ebi_sram_psram_nor_param_t
{
	// A0-15 == D0-15 with ALE
	bool addr_multiplex;
	
	struct ebi_sram_psram_nor_timing_t
	{
		uint16_t address_setup_cycle_r;
		uint16_t address_hold_cycle_r;
		uint16_t data_setup_cycle_r;
		uint32_t clock_hz_r;
		uint16_t address_setup_cycle_w;
		uint16_t address_hold_cycle_w;
		uint16_t data_setup_cycle_w;
		uint32_t clock_hz_w;
	} timing;
};
struct ebi_sram_psram_nor_info_t
{
	struct ebi_info_t common_info;
	struct ebi_sram_psram_nor_param_t param;
};
struct ebi_nand_info_t
{
	struct ebi_info_t common_info;
	struct ebi_nand_param_t
	{
		uint32_t clock_hz;
		struct ebi_nand_ecc_t
		{
			bool ecc_enable;
			uint16_t ecc_page_size;
		} ecc;
		struct ebi_nand_timing_t
		{
			uint8_t ale_to_re_cycle;
			uint8_t cle_to_re_cycle;
			uint16_t setup_cycle;
			uint16_t wait_cycle;
			uint8_t hold_cycle;
			uint8_t hiz_cycle;
			uint16_t setup_cycle_attr;
			uint16_t wait_cycle_attr;
			uint8_t hold_cycle_attr;
			uint8_t hiz_cycle_attr;
		} timing;
		struct ebi_nand_addr_t
		{
			uint32_t cmd;
			uint32_t addr;
			uint32_t data;
		} addr;
	} param;
};
struct interface_ebi_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint8_t target_index, void *param);
	vsf_err_t (*isready)(uint8_t index, uint8_t target_index);
	vsf_err_t (*read)(uint8_t index, uint8_t target_index, uint32_t address,
						uint8_t data_size, uint8_t *buff, uint32_t count);
	vsf_err_t (*write)(uint8_t index, uint8_t target_index, uint32_t address,
						uint8_t data_size, uint8_t *buff, uint32_t count);
};

struct nand_info_t
{
	uint32_t clock_hz;
	struct nand_ecc_t
	{
		bool ecc_enable;
		uint16_t ecc_page_size;
	} ecc;
	struct nand_timing_t
	{
		uint8_t ale_to_re_cycle;
		uint8_t cle_to_re_cycle;
		uint16_t setup_cycle;
		uint16_t wait_cycle;
		uint8_t hold_cycle;
		uint8_t hiz_cycle;
		uint16_t setup_cycle_attr;
		uint16_t wait_cycle_attr;
		uint8_t hold_cycle_attr;
		uint8_t hiz_cycle_attr;
	} timing;
};
struct interface_nand_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, struct nand_info_t *param);
	vsf_err_t (*write_cmd)(uint8_t index, uint8_t *cmd, uint8_t bytelen);
	vsf_err_t (*write_addr)(uint8_t index, uint8_t *addr, uint8_t bytelen);
	vsf_err_t (*write_data)(uint8_t index, uint8_t *data, uint32_t bytelen);
	vsf_err_t (*read_data)(uint8_t index, uint8_t *data, uint32_t bytelen);
};

struct interface_gpio_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config_pin)(uint8_t index, uint8_t pin_idx, uint8_t mode);
	vsf_err_t (*config)(uint8_t index, uint32_t pin_mask, uint32_t io,
						uint32_t pull_en_mask, uint32_t input_pull_mask);
	vsf_err_t (*set)(uint8_t index, uint32_t pin_mask);
	vsf_err_t (*clear)(uint8_t index, uint32_t pin_mask);
	vsf_err_t (*out)(uint8_t index, uint32_t pin_mask, uint32_t value);
	vsf_err_t (*in)(uint8_t index, uint32_t pin_mask, uint32_t *value);
};

struct interface_gpio_pin_t
{
	uint8_t port;
	uint32_t pin;
};

struct interface_delay_t
{
	vsf_err_t (*init)(void);
	vsf_err_t (*delayms)(uint16_t ms);
	vsf_err_t (*delayus)(uint16_t us);
};

struct interface_tickclk_t
{
	vsf_err_t (*init)(void);
	vsf_err_t (*fini)(void);
	vsf_err_t (*start)(void);
	vsf_err_t (*stop)(void);
	uint32_t (*get_count)(void);
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
	vsf_err_t (*config)(uint8_t index, uint8_t trn, uint16_t retry,
						uint16_t dly);
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

struct interface_jtag_hl_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
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
	vsf_err_t (*data_read)(uint8_t index, uint8_t *data, uint8_t len);
	vsf_err_t (*data_write)(uint8_t index, uint8_t *data, uint8_t len);
};

struct interface_i2c_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint16_t kHz, uint16_t byte_interval,
						uint16_t max_dly);
	vsf_err_t (*read)(uint8_t index, uint16_t chip_addr, uint8_t *data,
						uint16_t data_len, uint8_t stop, bool nacklast);
	vsf_err_t (*write)(uint8_t index, uint16_t chip_addr, uint8_t *data,
						uint16_t data_len, uint8_t stop);
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

struct interface_microwire_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint16_t kHz, uint8_t sel_polarity);
	vsf_err_t (*transport)(uint8_t index,
							uint32_t opcode, uint8_t opcode_bitlen,
							uint32_t addr, uint8_t addr_bitlen,
							uint32_t data, uint8_t data_bitlen,
							uint8_t *reply, uint8_t reply_bitlen);
	vsf_err_t (*poll)(uint8_t index, uint16_t interval_us, uint16_t retry_cnt);
};

struct interface_pwm_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config_mode)(uint8_t index, uint8_t mode);
	vsf_err_t (*config_freq)(uint8_t index, uint16_t kHz);
	vsf_err_t (*out)(uint8_t index, uint16_t count, uint16_t *rate);
	vsf_err_t (*in)(uint8_t index, uint16_t count, uint16_t *rate);
};

struct interface_adc_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint32_t clock_hz, uint8_t mode);
	vsf_err_t (*config_channel)(uint8_t index, uint8_t channel, uint8_t cycles);
	vsf_err_t (*calibrate)(uint8_t index, uint8_t channel);
	vsf_err_t (*start)(uint8_t index, uint8_t channel);
	vsf_err_t (*isready)(uint8_t index, uint8_t channel);
	uint32_t (*get)(uint8_t index, uint8_t channel);
	vsf_err_t (*sample)(uint8_t index, uint8_t channel, uint32_t *voltage);
};

struct interface_target_voltage_t
{
	vsf_err_t (*get)(uint8_t index, uint16_t *voltage);
	vsf_err_t (*set)(uint8_t index, uint16_t voltage);
};

struct interface_clko_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint32_t kHz);
	vsf_err_t (*enable)(uint8_t index);
	vsf_err_t (*disable)(uint8_t index);
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

struct interfaces_comm_t
{
	vsf_err_t (*init)(void);
	vsf_err_t (*fini)(void);
	void (*set_timeout)(uint32_t ms);
	vsf_err_t (*transact)(uint8_t *buffer_out, uint16_t out_len,
							uint8_t *buffer_in, uint16_t *in_len);
	
	vsf_err_t (*display_all)(void);
};

struct interfaces_info_t
{
	char *name;
	const struct vss_cmd_t *notifier;
	
	struct interfaces_comm_t *comm;
	
	// virtual interface support
	bool is_virtual;
	const struct program_mode_t *mode;
	
	uint64_t support_mask;
	struct interface_core_t core;
	struct interface_clko_t clko;
	struct interface_tickclk_t tickclk;
	struct interface_delay_t delay;
	struct interface_gpio_t gpio;
	struct interface_usart_t usart;
	struct interface_spi_t spi;
	struct interface_nand_t nand;
	struct interface_ebi_t ebi;
	struct interface_i2c_t i2c;
	struct interface_pwm_t pwm;
	struct interface_microwire_t microwire;
	struct interface_target_voltage_t target_voltage;
	struct interface_adc_t adc;
	struct interface_issp_t issp;
	struct interface_swd_t swd;
	struct interface_jtag_hl_t jtag_hl;
	struct interface_jtag_ll_t jtag_ll;
	struct interface_jtag_raw_t jtag_raw;
	struct interface_msp430jtag_t msp430jtag;
	struct interface_msp430sbw_t msp430sbw;
	struct interface_c2_t c2;
	struct interface_lpcicp_t lpcicp;
	struct interface_swim_t swim;
	struct interface_bdm_t bdm;
	struct interface_dusi_t dusi;
	struct interface_poll_t poll;
	vsf_err_t (*peripheral_commit)(void);
};

#ifdef DEFAULT_INTERFACES
#	define interfaces				(&DEFAULT_INTERFACES)
#else
#	define interfaces				cur_interface
#endif
#define INTERFACES_INFO_T		interfaces_info_t
extern struct interfaces_info_t *cur_interface;
extern struct interfaces_info_t *cur_real_interface;
extern struct interfaces_info_t *interfaces_info[];

void interface_print_list(void);
void interface_print_help(void);
vsf_err_t interface_init(const char *ifs);
vsf_err_t virtual_interface_init(const char *vifs, const char mode);
vsf_err_t interface_assert(struct interfaces_info_t **ifs);

#endif	// __INTERFACES_H_INCLUDED__

