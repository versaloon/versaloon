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

#include "app_cfg.h"
#include "app_type.h"

#include "interfaces.h"
#include "../dal.h"

#if DAL_NRF24L01_EN

#include "nrf24l01_drv_cfg.h"
#include "nrf24l01_drv.h"

#define NRF24L01_DRV_BASE_MHZ				2400
#define NRF24L01_DRV_MAX_PKGSIZE			32
#define NRF24L01_DRV_MAX_CHANNEL			5

#define NRF24L01_CMD_R_REGISTER(r)			(0x00 + (r))
#define NRF24L01_CMD_W_REGISTER(r)			(0x20 + (r))
#define NRF24L01_CMD_R_RX_PAYLOAD			0x61
#define NRF24L01_CMD_W_TX_PAYLOAD			0xA0
#define NRF24L01_CMD_FLUSH_TX				0xE1
#define NRF24L01_CMD_FLUSH_RX				0xE2
#define NRF24L01_CMD_REUSE_TX_PL			0xE3
#define NRF24L01_CMD_R_RX_PL_WID			0x60
#define NRF24L01_CMD_W_ACK_PAYLOAD(c)		(0xA8 + (c))
#define NRF24L01_CMD_W_TX_PAYLOAD_NOACK		0xB0
#define NRF24L01_CMD_NOP					0xFF

#define NRF24L01_REG_CONFIG					0x00
#define NRF24L01_REG_CONFIG_MASK_RX_DR		(1 << 6)
#define NRF24L01_REG_CONFIG_MASK_TX_DS		(1 << 5)
#define NRF24L01_REG_CONFIG_MASK_MAX_RT		(1 << 4)
#define NRF24L01_REG_CONFIG_EN_CRC			(1 << 3)
#define NRF24L01_REG_CONFIG_CRCO			(1 << 2)
#define NRF24L01_REG_CONFIG_PWR_UP			(1 << 1)
#define NRF24L01_REG_CONFIG_PRIM_RX			(1 << 0)
#define NRF24L01_REG_EN_AA					0x01
#define NRF24L01_REG_EN_RXADDR				0x02
#define NRF24L01_REG_SETUP_AW				0x03
#define NRF24L01_REG_SETUP_PETR				0x04
#define NRF24L01_REG_RF_CH					0x05
#define NRF24L01_REG_RF_SETUP				0x06
#define NRF24L01_REG_RF_SETUP_250K			0x20
#define NRF24L01_REG_RF_SETUP_1M			0x00
#define NRF24L01_REG_RF_SETUP_2M			0x08
#define NRF24L01_REG_STATUS					0x07
#define NRF24L01_REG_STATUS_RX_DR			(1 << 6)
#define NRF24L01_REG_STATUS_TX_DS			(1 << 5)
#define NRF24L01_REG_STATUS_MAX_RT			(1 << 4)
#define NRF24L01_REG_OBSERVE_TX				0x08
#define NRF24L01_REG_RPD					0x09
#define NRF24L01_REG_RX_ADDR_P(n)			(0x0A + (n))
#define NRF24L01_REG_TX_ADDR				0x10
#define NRF24L01_REG_RX_PW_P(n)				(0x11 + (n))
#define NRF24L01_REG_FIFO_STATUS			0x17
#define NRF24L01_REG_DYNPD					0x1C
#define NRF24L01_REG_FEATURE				0x1D
#define NRF24L01_REG_FEATURE_EN_DPL			(1 << 2)
#define NRF24L01_REG_FEATURE_EN_ACK_PAY		(1 << 1)
#define NRF24L01_REG_FEATURE_EN_DYN_ACK		(1 << 0)

static vsf_err_t nrf24l01_drv_transact(struct nrf24l01_drv_interface_t *ifs, 
		uint8_t *out, uint8_t outlen, uint8_t *in, uint8_t inlen)
{
	interfaces->gpio.out(ifs->csn_port, ifs->csn_pin, 0);
	if (out != NULL)
	{
		interfaces->spi.io(ifs->spi_port, out, NULL, outlen);
	}
	if (in != NULL)
	{
		interfaces->spi.io(ifs->spi_port, NULL, in, inlen);
	}
	interfaces->gpio.out(ifs->csn_port, ifs->csn_pin, ifs->csn_pin);
	return interfaces->peripheral_commit();
}

static vsf_err_t nrf24l01_drv_write_reg(struct nrf24l01_drv_interface_t *ifs, 
			uint8_t reg, uint8_t *value, uint8_t size)
{
	uint8_t cmd_buff[6];
	
	if (size > 5)
	{
		return VSFERR_INVALID_PARAMETER;
	}
	
	cmd_buff[0] = NRF24L01_CMD_W_REGISTER(reg);
	memcpy(&cmd_buff[1], value, size);
	return nrf24l01_drv_transact(ifs, cmd_buff, 1 + size, NULL, 0);
}

static vsf_err_t nrf24l01_drv_read_reg(struct nrf24l01_drv_interface_t *ifs, 
			uint8_t reg, uint8_t *value, uint8_t size)
{
	uint8_t cmd_buff[1];
	
	if (size > 5)
	{
		return VSFERR_INVALID_PARAMETER;
	}
	
	cmd_buff[0] = NRF24L01_CMD_R_REGISTER(reg);
	return nrf24l01_drv_transact(ifs, cmd_buff, 1, value, size);
}

static vsf_err_t nrf24l01_drv_init(struct dal_info_t *info)
{
	struct nrf24l01_drv_interface_t *ifs = 
								(struct nrf24l01_drv_interface_t *)info->ifs;
	
	interfaces->spi.init(ifs->spi_port);
	interfaces->gpio.init(ifs->ce_port);
	interfaces->gpio.config(ifs->ce_port, ifs->ce_pin, ifs->ce_pin, 0, 0);
	interfaces->gpio.init(ifs->csn_port);
	return interfaces->gpio.config(ifs->csn_port, ifs->csn_pin, ifs->csn_pin, 0,
									ifs->csn_pin);
}

static vsf_err_t nrf24l01_drv_fini(struct dal_info_t *info)
{
	struct nrf24l01_drv_interface_t *ifs = 
								(struct nrf24l01_drv_interface_t *)info->ifs;
	
	interfaces->spi.fini(ifs->spi_port);
	interfaces->gpio.fini(ifs->ce_port);
	return interfaces->gpio.fini(ifs->csn_port);
}

static vsf_err_t nrf24l01_drv_config(struct dal_info_t *info)
{
	struct nrf24l01_drv_interface_t *ifs = 
								(struct nrf24l01_drv_interface_t *)info->ifs;
	struct nrf24l01_drv_param_t *param = 
								(struct nrf24l01_drv_param_t *)info->param;
	uint8_t config;
	
	interfaces->spi.config(ifs->spi_port, param->kHz, 
							SPI_MODE0 | SPI_MSB_FIRST | SPI_MASTER);
	
	config = (uint8_t)param->aw;
	if (nrf24l01_drv_write_reg(ifs, NRF24L01_REG_SETUP_AW, &config, 1))
	{
		return VSFERR_FAIL;
	}
	config = (uint8_t)(param->rf_channel_mHz - NRF24L01_DRV_BASE_MHZ);
	if (nrf24l01_drv_write_reg(ifs, NRF24L01_REG_RF_CH, &config, 1))
	{
		return VSFERR_FAIL;
	}
	config = NRF24L01_REG_FEATURE_EN_DPL | NRF24L01_REG_FEATURE_EN_ACK_PAY | 
			NRF24L01_REG_FEATURE_EN_DYN_ACK;
	if (nrf24l01_drv_write_reg(ifs, NRF24L01_REG_FEATURE, &config, 1))
	{
		return VSFERR_FAIL;
	}
	config = (uint8_t)(param->txpwr << 1);
	switch (param->dr)
	{
	case NRF24L01_DR_250K:
		config |= NRF24L01_REG_RF_SETUP_250K;
		break;
	case NRF24L01_DR_1M:
		config |= NRF24L01_REG_RF_SETUP_1M;
		break;
	case NRF24L01_DR_2M:
		config |= NRF24L01_REG_RF_SETUP_2M;
		break;
	default:
		return VSFERR_INVALID_PARAMETER;
	}
	if (nrf24l01_drv_write_reg(ifs, NRF24L01_REG_RF_SETUP, &config, 1))
	{
		return VSFERR_FAIL;
	}
	if (NRF24L01_DIR_TX == param->dir)
	{
		config = param->arc + (uint8_t)(((param->ard_us / 250) - 1) << 4);
		if (nrf24l01_drv_write_reg(ifs, NRF24L01_REG_SETUP_PETR, &config, 1))
		{
			return VSFERR_FAIL;
		}
	}
	config = NRF24L01_REG_CONFIG_MASK_RX_DR | NRF24L01_REG_CONFIG_MASK_TX_DS | 
			NRF24L01_REG_CONFIG_MASK_MAX_RT | NRF24L01_REG_CONFIG_EN_CRC | 
			NRF24L01_REG_CONFIG_PWR_UP | (uint8_t)(param->crclen << 2) | 
			(uint8_t)(param->dir);
	if (nrf24l01_drv_write_reg(ifs, NRF24L01_REG_CONFIG, &config, 1))
	{
		return VSFERR_FAIL;
	}
	if (NRF24L01_DIR_RX == param->dir)
	{
		interfaces->gpio.out(ifs->ce_port, ifs->ce_pin, ifs->ce_pin);
	}
	return interfaces->peripheral_commit();
}

static vsf_err_t nrf24l01_drv_config_channel(struct dal_info_t *info, 
					uint8_t channel, bool enable, uint8_t *addr, bool enaa)
{
	struct nrf24l01_drv_interface_t *ifs = 
								(struct nrf24l01_drv_interface_t *)info->ifs;
	struct nrf24l01_drv_param_t *param = 
								(struct nrf24l01_drv_param_t *)info->param;
	uint8_t config, size;
	
	if (channel > NRF24L01_DRV_MAX_CHANNEL)
	{
		return VSFERR_INVALID_PARAMETER;
	}
	
	switch (param->dir)
	{
	case NRF24L01_DIR_TX:
		if (nrf24l01_drv_write_reg(ifs, NRF24L01_REG_TX_ADDR, addr,
									param->aw) || 
			nrf24l01_drv_write_reg(ifs, NRF24L01_REG_RX_ADDR_P(0), addr,
									param->aw))
		{
			return VSFERR_FAIL;
		}
		break;
	case NRF24L01_DIR_RX:
		if (nrf24l01_drv_read_reg(ifs, NRF24L01_REG_EN_RXADDR, &config, 1))
		{
			return VSFERR_FAIL;
		}
		if (enable)
		{
			config |= (1 << channel);
		}
		if (nrf24l01_drv_write_reg(ifs, NRF24L01_REG_EN_RXADDR, &config, 1))
		{
			return VSFERR_FAIL;
		}
		if (!enable)
		{
			break;
		}
		
		if (nrf24l01_drv_read_reg(ifs, NRF24L01_REG_DYNPD, &config, 1))
		{
			return VSFERR_FAIL;
		}
		config |= (1 << channel);
		if (nrf24l01_drv_write_reg(ifs, NRF24L01_REG_DYNPD, &config, 1))
		{
			return VSFERR_FAIL;
		}
		
		if (nrf24l01_drv_read_reg(ifs, NRF24L01_REG_EN_AA, &config, 1))
		{
			return VSFERR_FAIL;
		}
		if (enaa)
		{
			config |= (1 << channel);
		}
		if (nrf24l01_drv_write_reg(ifs, NRF24L01_REG_EN_AA, &config, 1))
		{
			return VSFERR_FAIL;
		}
		
		if (channel < 2)
		{
			size = param->aw;
		}
		else
		{
			size = 1;
		}
		if (nrf24l01_drv_write_reg(ifs, NRF24L01_REG_RX_ADDR_P(channel), addr,
									size))
		{
			return VSFERR_FAIL;
		}
		break;
	default:
		return VSFERR_INVALID_PARAMETER;
	}
	return interfaces->peripheral_commit();
}

static vsf_err_t nrf24l01_drv_power(struct dal_info_t *info, bool power_down)
{
	struct nrf24l01_drv_interface_t *ifs = 
								(struct nrf24l01_drv_interface_t *)info->ifs;
	uint8_t config;
	
	if (nrf24l01_drv_read_reg(ifs, NRF24L01_REG_CONFIG, &config, 1))
	{
		return VSFERR_FAIL;
	}
	
	if (power_down)
	{
		config &= ~NRF24L01_REG_CONFIG_PWR_UP;
	}
	else
	{
		config |= NRF24L01_REG_CONFIG_PWR_UP;
	}
	return nrf24l01_drv_write_reg(ifs, NRF24L01_REG_CONFIG, &config, 1);
}

static vsf_err_t nrf24l01_drv_tx_fifo_avail(struct dal_info_t *info)
{
	struct nrf24l01_drv_interface_t *ifs = 
								(struct nrf24l01_drv_interface_t *)info->ifs;
	struct nrf24l01_drv_param_t *param = 
								(struct nrf24l01_drv_param_t *)info->param;
	uint8_t status;
	
	if (param->dir != NRF24L01_DIR_TX)
	{
		return VSFERR_INVALID_PARAMETER;
	}
	
	if (nrf24l01_drv_read_reg(ifs, NRF24L01_REG_STATUS, &status, 1))
	{
		return VSFERR_FAIL;
	}
	return !(status & 1) ? VSFERR_NONE : VSFERR_NOT_AVAILABLE;
}

static vsf_err_t nrf24l01_drv_tx_fifo_write(struct dal_info_t *info, 
											struct vsf_buffer_t *buffer)
{
	struct nrf24l01_drv_interface_t *ifs = 
								(struct nrf24l01_drv_interface_t *)info->ifs;
	struct nrf24l01_drv_param_t *param = 
								(struct nrf24l01_drv_param_t *)info->param;
	uint8_t cmd_buff[33];
	
	if ((NULL == buffer) || (NULL == buffer->buffer) || (buffer->size > 32) || 
		(param->dir != NRF24L01_DIR_TX))
	{
		return VSFERR_INVALID_PARAMETER;
	}
	
	cmd_buff[0] = NRF24L01_CMD_W_TX_PAYLOAD;
	memcpy(&cmd_buff[1], buffer->buffer, buffer->size);
	if (nrf24l01_drv_transact(ifs, cmd_buff, (uint8_t)(1 + buffer->size), NULL,
								0))
	{
		return VSFERR_FAIL;
	}
	return interfaces->peripheral_commit();
}

static vsf_err_t nrf24l01_drv_ack_fifo_avail(struct dal_info_t *info)
{
	struct nrf24l01_drv_interface_t *ifs = 
								(struct nrf24l01_drv_interface_t *)info->ifs;
	uint8_t status;
	
	if (nrf24l01_drv_read_reg(ifs, NRF24L01_REG_STATUS, &status, 1))
	{
		return VSFERR_FAIL;
	}
	return !(status & 1) ? VSFERR_NONE : VSFERR_NOT_AVAILABLE;
}

static vsf_err_t nrf24l01_drv_ack_fifo_write(struct dal_info_t *info, 
								uint8_t channel, struct vsf_buffer_t *buffer)
{
	struct nrf24l01_drv_interface_t *ifs = 
								(struct nrf24l01_drv_interface_t *)info->ifs;
	uint8_t cmd_buff[33];
	
	if ((NULL == buffer) || (NULL == buffer->buffer) || (buffer->size > 32))
	{
		return VSFERR_INVALID_PARAMETER;
	}
	
	cmd_buff[0] = NRF24L01_CMD_W_ACK_PAYLOAD(channel);
	memcpy(&cmd_buff[1], buffer->buffer, buffer->size);
	return nrf24l01_drv_transact(ifs, cmd_buff, (uint8_t)(1 + buffer->size), 
									NULL, 0);
}

static vsf_err_t nrf24l01_drv_rx_fifo_avail(struct dal_info_t *info)
{
	struct nrf24l01_drv_interface_t *ifs = 
								(struct nrf24l01_drv_interface_t *)info->ifs;
	uint8_t status;
	
	if (nrf24l01_drv_read_reg(ifs, NRF24L01_REG_STATUS, &status, 1))
	{
		return VSFERR_FAIL;
	}
	return ((status >> 1) & 0x07) != 0x07 ? VSFERR_NONE : VSFERR_NOT_AVAILABLE;
}

static vsf_err_t nrf24l01_drv_rx_fifo_read(struct dal_info_t *info, 
						struct vsf_buffer_t *buffer)
{
	struct nrf24l01_drv_interface_t *ifs = 
								(struct nrf24l01_drv_interface_t *)info->ifs;
	uint8_t cmd_buff[1];
	
	if ((NULL == buffer) || (NULL == buffer->buffer) || (buffer->size > 32))
	{
		return VSFERR_INVALID_PARAMETER;
	}
	
	cmd_buff[0] = NRF24L01_CMD_R_RX_PAYLOAD;
	memcpy(&cmd_buff[1], buffer->buffer, buffer->size);
	return nrf24l01_drv_transact(ifs, cmd_buff, 1, buffer->buffer, 
									(uint8_t)buffer->size);
}

static vsf_err_t nrf24l01_drv_send(struct dal_info_t *info)
{
	struct nrf24l01_drv_interface_t *ifs = 
								(struct nrf24l01_drv_interface_t *)info->ifs;
	struct nrf24l01_drv_param_t *param = 
								(struct nrf24l01_drv_param_t *)info->param;
	
	if (param->dir != NRF24L01_DIR_TX)
	{
		return VSFERR_INVALID_PARAMETER;
	}
	
	interfaces->gpio.out(ifs->ce_port, ifs->ce_pin, ifs->ce_pin);
	interfaces->delay.delayus(10);
	interfaces->gpio.out(ifs->ce_port, ifs->ce_pin, 0);
	return interfaces->peripheral_commit();
}

static vsf_err_t nrf24l01_drv_tx_flush(struct dal_info_t *info)
{
	struct nrf24l01_drv_interface_t *ifs = 
								(struct nrf24l01_drv_interface_t *)info->ifs;
	uint8_t cmd_buff[1];
	
	cmd_buff[0] = NRF24L01_CMD_FLUSH_TX;
	return nrf24l01_drv_transact(ifs, cmd_buff, 1, NULL, 0);
}

static vsf_err_t nrf24l01_drv_rx_flush(struct dal_info_t *info)
{
	struct nrf24l01_drv_interface_t *ifs = 
								(struct nrf24l01_drv_interface_t *)info->ifs;
	uint8_t cmd_buff[1];
	
	cmd_buff[0] = NRF24L01_CMD_FLUSH_RX;
	return nrf24l01_drv_transact(ifs, cmd_buff, 1, NULL, 0);
}

static vsf_err_t nrf24l01_drv_get_rx_power(struct dal_info_t *info, uint8_t *pwr)
{
	struct nrf24l01_drv_interface_t *ifs = 
								(struct nrf24l01_drv_interface_t *)info->ifs;
	
	return nrf24l01_drv_read_reg(ifs, NRF24L01_REG_RPD, pwr, 1);
}

static vsf_err_t nrf24l01_drv_poll(struct dal_info_t *info)
{
	struct nrf24l01_drv_param_t *param = 
								(struct nrf24l01_drv_param_t *)info->param;
	
	if (param->user_callback.on_poll != NULL)
	{
		return param->user_callback.on_poll(info);
	}
	return VSFERR_NONE;
}

static vsf_err_t nrf24l01_drv_on_interrupt(struct dal_info_t *info)
{
	struct nrf24l01_drv_interface_t *ifs = 
								(struct nrf24l01_drv_interface_t *)info->ifs;
	struct nrf24l01_drv_param_t *param = 
								(struct nrf24l01_drv_param_t *)info->param;
	uint8_t status, channel;
	
	if (nrf24l01_drv_read_reg(ifs, NRF24L01_REG_STATUS, &status, 1))
	{
		return VSFERR_FAIL;
	}
	
	channel = (status >> 1) & 0x07;
	if (status & NRF24L01_REG_STATUS_RX_DR)
	{
		status &= ~NRF24L01_REG_STATUS_RX_DR;
		if (param->user_callback.on_rx != NULL)
		{
			uint8_t cmd_buff[1], size, buff[NRF24L01_DRV_MAX_PKGSIZE];
			struct vsf_buffer_t buffer;
			
			cmd_buff[0] = NRF24L01_CMD_R_RX_PAYLOAD;
			if (nrf24l01_drv_read_reg(ifs, NRF24L01_REG_RX_PW_P(channel),
										&size, 1) || 
				(!size) || nrf24l01_drv_transact(ifs, cmd_buff, 1, buff, size))
			{
				return VSFERR_FAIL;
			}
			
			buffer.size = size;
			buffer.buffer = buff;
			return param->user_callback.on_rx(info, channel, &buffer);
		}
	}
	if (status & NRF24L01_REG_STATUS_TX_DS)
	{
		status &= ~NRF24L01_REG_STATUS_TX_DS;
		if (param->user_callback.on_tx != NULL)
		{
			return param->user_callback.on_tx(info);
		}
	}
	if (status & NRF24L01_REG_STATUS_MAX_RT)
	{
		status &= ~NRF24L01_REG_STATUS_MAX_RT;
		if (param->user_callback.on_to != NULL)
		{
			return param->user_callback.on_to(info);
		}
	}
	
	return nrf24l01_drv_write_reg(ifs, NRF24L01_REG_STATUS, &status, 1);
}

#if DAL_INTERFACE_PARSER_EN
static vsf_err_t nrf24l01_drv_parse_interface(struct dal_info_t *info, 
											uint8_t *buff)
{
	struct nrf24l01_drv_interface_t *ifs = 
								(struct nrf24l01_drv_interface_t *)info->ifs;
	
	ifs->spi_port = buff[0];
	ifs->csn_port = buff[1];
	ifs->csn_pin = *(uint32_t *)&buff[2];
	ifs->ce_port = buff[6];
	ifs->ce_pin = *(uint32_t *)&buff[7];
	return VSFERR_NONE;
}
#endif

const struct nrf24l01_drv_t nrf24l01_drv = 
{
	{
		"nrf24l01",
#if DAL_INTERFACE_PARSER_EN
		"spi:%1dcsn:%1d,%4xce:%1d,%4x",
		nrf24l01_drv_parse_interface,
#endif
	},
	nrf24l01_drv_init,
	nrf24l01_drv_fini,
	nrf24l01_drv_config,
	nrf24l01_drv_config_channel,
	nrf24l01_drv_power,
	
	nrf24l01_drv_tx_fifo_avail,
	nrf24l01_drv_tx_fifo_write,
	nrf24l01_drv_ack_fifo_avail,
	nrf24l01_drv_ack_fifo_write,
	nrf24l01_drv_rx_fifo_avail,
	nrf24l01_drv_rx_fifo_read,
	nrf24l01_drv_send,
	
	nrf24l01_drv_tx_flush,
	nrf24l01_drv_rx_flush,
	
	nrf24l01_drv_get_rx_power,
	nrf24l01_drv_poll,
	
	{
		nrf24l01_drv_on_interrupt,
	}
};

#endif
