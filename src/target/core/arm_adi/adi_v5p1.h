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

#ifndef __ADI_V5P1_H_INCLUDED__
#define __ADI_V5P1_H_INCLUDED__

#define ADI_MODE_CHAR_JTAG					'j'
#define ADI_MODE_CHAR_SWD					's'

enum adi_dpif_type_t
{
	ADI_DP_JTAG = 0,
	ADI_DP_SWD = 1
};

enum adi_dp_target_core_t
{
	ADI_DP_INVALID,
	ADI_DP_CM0,
	ADI_DP_CM3,
	ADI_DP_CM4
};

struct adi_dpif_t
{
	enum adi_dpif_type_t type;
	union
	{
		struct
		{
			uint16_t jtag_khz;
			struct jtag_pos_t jtag_pos;
		} dpif_jtag_setting;
		struct
		{
			uint8_t swd_trn;
			uint16_t swd_retry;
			uint16_t swd_dly;
		} dpif_swd_setting;
	} dpif_setting;
};

struct adi_dp_t
{
	uint8_t cur_ir;
	uint8_t ack;
	uint32_t dp_ctrl_stat;
	uint32_t ap_sel_value;
	uint32_t ap_bank_value;
	uint32_t ap_csw_value;
	uint32_t ap_tar_value;
};

struct adi_dp_info_t
{
	enum adi_dpif_type_t type;
	enum adi_dp_target_core_t core;
	uint8_t memaccess_tck;
	uint32_t tar_autoincr_block;
	
	uint32_t if_id;
	uint32_t config;			// MEM-AP register: CFG
	uint32_t rom_address;		// MEM-AP register: BASE
	uint32_t ahb_ap_id;			// MEM-AP register: IDR
	struct adi_dp_t dp_state;
	
	uint8_t ack_value;
	uint8_t adi_dp_first3bits;
};

struct adi_info_t
{
	struct INTERFACES_INFO_T *prog;
	struct adi_dpif_t dpif;
	struct adi_dp_t dp;
	struct adi_dp_info_t dp_info;
	uint8_t ack_value;
	uint8_t dp_first3bits;
};

vsf_err_t adi_switch(struct adi_info_t *adi_info);
vsf_err_t adi_init(struct INTERFACES_INFO_T *ifs, struct adi_dpif_t *interf,
					enum adi_dp_target_core_t *core);
vsf_err_t adi_fini(void);
vsf_err_t adi_dp_commit(void);

void adi_ap_select(uint8_t apsel);

vsf_err_t adi_dp_read_reg(uint8_t reg_addr, uint32_t *value,
								uint8_t check_result);
vsf_err_t adi_dp_write_reg(uint8_t reg_addr, uint32_t *value,
								uint8_t check_result);
vsf_err_t adi_ap_read_reg(uint8_t reg_addr, uint32_t *value,
								uint8_t check_result);
vsf_err_t adi_ap_write_reg(uint8_t reg_addr, uint32_t *value,
								uint8_t check_result);

uint32_t adi_memap_get_max_tar_block_size(uint32_t address);

vsf_err_t adi_memap_read_reg8(uint32_t address, uint8_t *reg,
								uint8_t check_result);
vsf_err_t adi_memap_write_reg8(uint32_t address, uint8_t *reg,
								uint8_t check_result);
vsf_err_t adi_memap_read_reg16(uint32_t address, uint16_t *reg,
								uint8_t check_result);
vsf_err_t adi_memap_write_reg16(uint32_t address, uint16_t *reg,
								uint8_t check_result);
vsf_err_t adi_memap_read_reg32(uint32_t address, uint32_t *reg,
								uint8_t check_result);
vsf_err_t adi_memap_write_reg32(uint32_t address, uint32_t *reg,
								uint8_t check_result);
vsf_err_t adi_memap_read_buf8(uint32_t address, uint8_t *buffer, uint32_t len);
vsf_err_t adi_memap_write_buf8(uint32_t address, uint8_t *buffer, uint32_t len);
vsf_err_t adi_memap_read_buf32(uint32_t address, uint8_t *buffer, uint32_t len);
vsf_err_t adi_memap_write_buf32(uint32_t address, uint8_t *buffer, uint32_t len);

#endif		// __ADI_V5P1_H_INCLUDED__

