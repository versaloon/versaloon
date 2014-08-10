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

#ifndef __NRF24L01_DRV_H_INCLUDED__
#define __NRF24L01_DRV_H_INCLUDED__

#include "dal_cfg.h"
#include "../dal.h"

#include "../../tool/buffer/buffer.h"

enum nrf24l01_drv_dir_t
{
	NRF24L01_DIR_TX		= 0,
	NRF24L01_DIR_RX		= 1,
};

enum nrf24l01_drv_aw_t
{
	NRF24L01_AW_3		= 1,
	NRF24L01_AW_4		= 2,
	NRF24L01_AW_5		= 3,
};

enum nrf24l01_drv_datarate_t
{
	NRF24L01_DR_250K,
	NRF24L01_DR_1M,
	NRF24L01_DR_2M,
};

enum nrf24l01_drv_txpwr_t
{
	NRF24L01_TXPWR_M18DBM	= 0,
	NRF24L01_TXPWR_M12DBM	= 2,
	NRF24L01_TXPWR_M6DBM	= 4,
	NRF24L01_TXPWR_0DBM		= 12,
};

enum nrf24l01_drv_crclen_t
{
	NRF24L01_CRCLEN_1		= 0,
	NRF24L01_CRCLEN_2		= 1,
};

struct nrf24l01_drv_user_callback
{
	vsf_err_t (*on_tx)(struct dal_info_t *info);
	vsf_err_t (*on_rx)(struct dal_info_t *info, uint8_t channel, 
						struct vsf_buffer_t *buffer);
	vsf_err_t (*on_to)(struct dal_info_t *info);
	vsf_err_t (*on_poll)(struct dal_info_t *info);
};

struct nrf24l01_drv_t
{
	struct dal_driver_t driver;
	vsf_err_t (*init)(struct dal_info_t *info);
	vsf_err_t (*fini)(struct dal_info_t *info);
	vsf_err_t (*config)(struct dal_info_t *info);
	vsf_err_t (*config_channel)(struct dal_info_t *info, uint8_t channel, 
								bool enable, uint8_t *addr, bool enaa);
	vsf_err_t (*power)(struct dal_info_t *info, bool power_down);
	
	vsf_err_t (*tx_fifo_avail)(struct dal_info_t *info);
	vsf_err_t (*tx_fifo_write)(struct dal_info_t *info, 
								struct vsf_buffer_t *buffer);
	vsf_err_t (*ack_fifo_avail)(struct dal_info_t *info);
	vsf_err_t (*ack_fifo_write)(struct dal_info_t *info, uint8_t channel, 
								struct vsf_buffer_t *buffer);
	vsf_err_t (*rx_fifo_avail)(struct dal_info_t *info);
	vsf_err_t (*rx_fifo_read)(struct dal_info_t *info, 
								struct vsf_buffer_t *buffer);
	vsf_err_t (*send)(struct dal_info_t *info);
	
	vsf_err_t (*tx_flush)(struct dal_info_t *info);
	vsf_err_t (*rx_flush)(struct dal_info_t *info);
	
	vsf_err_t (*get_rx_power)(struct dal_info_t *info, uint8_t *pwr);
	vsf_err_t (*poll)(struct dal_info_t *info);
	
	struct nrf24l01_drv_callback_t
	{
		vsf_err_t (*on_interrupt)(struct dal_info_t *info);
	} callback;
};

struct nrf24l01_drv_interface_t
{
	uint8_t csn_port;
	uint32_t csn_pin;
	uint8_t ce_port;
	uint32_t ce_pin;
	uint8_t spi_port;
};

struct nrf24l01_drv_param_t
{
	uint16_t kHz;
	
	enum nrf24l01_drv_dir_t dir;
	enum nrf24l01_drv_crclen_t crclen;
	enum nrf24l01_drv_aw_t aw;
	uint16_t rf_channel_mHz;
	enum nrf24l01_drv_datarate_t dr;
	struct nrf24l01_drv_user_callback user_callback;
	enum nrf24l01_drv_txpwr_t txpwr;
	
	// necessary only in tx mode
	uint16_t ard_us;
	uint8_t arc;
};

extern const struct nrf24l01_drv_t nrf24l01_drv;

#endif	// __NRF24L01_DRV_H_INCLUDED__

