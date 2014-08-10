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
#include "../mal/mal.h"
#include "../mal/mal_driver.h"

#if DAL_SD_SPI_EN

#include "sd_common.h"
#include "sd_spi_drv_cfg.h"
#include "sd_spi_drv.h"

// RESP
#define  SD_RESP_EMPTY						0xFF

#define  SD_DATATOKEN_RESP_ACCEPTED			0x05
#define  SD_DATATOKEN_RESP_CRC_REJECTED		0x0B
#define  SD_DATATOKEN_RESP_WR_REJECTED		0x0D
#define  SD_DATATOKEN_RESP_MASK				0x1F

#define  SD_DATATOKEN_START_BLK_MULT		0xFC
#define  SD_DATATOKEN_STOP_TRAN				0xFD
#define  SD_DATATOKEN_START_BLK				0xFE

#define  SD_DATATOKEN_ERR_ERROR				0x01
#define  SD_DATATOKEN_ERR_CC_ERROR			0x02
#define  SD_DATATOKEN_ERR_CARD_ECC_ERROR	0x04
#define  SD_DATATOKEN_ERR_OUT_OF_RANGE		0x08

// Card Status
#define SD_CS8_NONE							0x00
#define SD_CS8_IN_IDLE_STATE				0x01
#define SD_CS8_ERASE_RESET					0x02
#define SD_CS8_ILLEGAL_COMMAND				0x04
#define SD_CS8_COM_CRC_ERROR				0x08
#define SD_CS8_ERASE_SEQUENCE_ERROR			0x10
#define SD_CS8_ADDRESS_ERROR				0x20
#define SD_CS8_PARAMETER_ERROR				0x40
#define SD_CS8_ERROR_MASK					\
	(SD_CS8_ERASE_RESET | SD_CS8_ILLEGAL_COMMAND | SD_CS8_COM_CRC_ERROR | \
	SD_CS8_ERASE_SEQUENCE_ERROR | SD_CS8_ADDRESS_ERROR | SD_CS8_PARAMETER_ERROR)

static vsf_err_t sd_spi_drv_cs_assert(struct sd_spi_drv_interface_t *ifs)
{
	if (ifs->cs_port != IFS_DUMMY_PORT)
	{
		return interfaces->gpio.clear(ifs->cs_port, ifs->cs_pin);
	}
	else
	{
		return interfaces->spi.select(ifs->spi_port, (uint8_t)ifs->cs_pin);
	}
}

static vsf_err_t sd_spi_drv_cs_deassert(struct sd_spi_drv_interface_t *ifs)
{
	if (ifs->cs_port != IFS_DUMMY_PORT)
	{
		return interfaces->gpio.set(ifs->cs_port, ifs->cs_pin);
	}
	else
	{
		return interfaces->spi.deselect(ifs->spi_port, (uint8_t)ifs->cs_pin);
	}
}

static vsf_err_t sd_spi_drv_send_empty_bytes(struct sd_spi_drv_interface_t *ifs, 
											uint8_t cnt)
{
	uint8_t byte_0xFF = 0xFF;
	
	while (cnt--)
	{
		interfaces->spi.io(ifs->spi_port, &byte_0xFF, NULL, 1);
	}
	return VSFERR_NONE;
}

static vsf_err_t sd_spi_transact_init(struct sd_spi_drv_interface_t *ifs)
{
	if (ifs->cs_port != IFS_DUMMY_PORT)
	{
		interfaces->gpio.init(ifs->cs_port);
		interfaces->gpio.config(ifs->cs_port, ifs->cs_pin, ifs->cs_pin,
								ifs->cs_pin, ifs->cs_pin);
	}
	// use slowest spi speed when initializing
	interfaces->spi.init(ifs->spi_port);
	interfaces->spi.config(ifs->spi_port, 400, 
							SPI_MODE3 | SPI_MSB_FIRST | SPI_MASTER);
	return sd_spi_drv_send_empty_bytes(ifs, 20);
}

static vsf_err_t sd_spi_transact_fini(struct sd_spi_drv_interface_t *ifs)
{
	if (ifs->cs_port != IFS_DUMMY_PORT)
	{
		interfaces->gpio.config(ifs->cs_port, ifs->cs_pin, 0, 0, 0);
		interfaces->gpio.fini(ifs->cs_port);
	}
	return interfaces->spi.fini(ifs->spi_port);
}

static vsf_err_t sd_spi_transact_start(struct sd_spi_drv_interface_t *ifs)
{
	sd_spi_drv_cs_assert(ifs);
	return sd_spi_drv_send_empty_bytes(ifs, 1);
}

static vsf_err_t sd_spi_transact_cmd(struct sd_spi_drv_interface_t *ifs,
	struct sd_spi_drv_info_t *drv_info, uint16_t token,
	uint8_t cmd, uint32_t arg)
{
	uint8_t cmd_pkt[6];
	
	REFERENCE_PARAMETER(token);
	
	if (token & SD_TRANSTOKEN_RESP_CHECKBUSY)
	{
		drv_info->state = SD_SPI_DRV_WAITBUSY;
		drv_info->retry = 312500;
	}
	else
	{
		drv_info->state = SD_SPI_DRV_WAITRESP;
		drv_info->retry = 32;
	}
	drv_info->cur_block = 0;
	
	cmd_pkt[0] = cmd | 0x40;
	cmd_pkt[1] = (arg >> 24) & 0xFF;
	cmd_pkt[2] = (arg >> 16) & 0xFF;
	cmd_pkt[3] = (arg >>  8) & 0xFF;
	cmd_pkt[4] = (arg >>  0) & 0xFF;
	cmd_pkt[5] = (sd_spi_cmd_chksum(&cmd_pkt[0], 5) << 1) | 0x01;
	return interfaces->spi.io(ifs->spi_port, cmd_pkt, NULL, 6);
}

static vsf_err_t sd_spi_transact_cmd_isready(struct sd_spi_drv_interface_t *ifs,
	struct sd_spi_drv_info_t *drv_info, uint16_t token,
	uint8_t *resp1, uint8_t *resp)
{
	uint8_t tmp8 = 0xFF;
	
	interfaces->spi.io(ifs->spi_port, &tmp8, &tmp8, 1);
	if (interfaces->peripheral_commit())
	{
		return VSFERR_FAIL;
	}
	
	if (SD_SPI_DRV_WAITRESP == drv_info->state)
	{
		if (!(tmp8 & 0x80))
		{
			uint8_t len;
			
			*resp1 = tmp8;
			if (!(token & SD_TRANSTOKEN_RESP_CHECKBUSY) &&
				(tmp8 & SD_CS8_ERROR_MASK))
			{
				return VSFERR_FAIL;
			}
			
			len = token & SD_TRANSTOKEN_RESP_LENMSK;
			if (len)
			{
				interfaces->spi.io(ifs->spi_port, NULL, resp, len);
			}
			
			if (!(token & SD_TRANSTOKEN_RESP_CHECKBUSY))
			{
				return VSFERR_NONE;
			}
			drv_info->state = SD_SPI_DRV_WAITBUSY;
			drv_info->retry = 312500;
		}
		else if (!--drv_info->retry)
		{
			return VSFERR_FAIL;
		}
	}
	else if (SD_SPI_DRV_WAITBUSY == drv_info->state)
	{
		if (tmp8 != 0)
		{
			return VSFERR_NONE;
		}
		else if (!--drv_info->retry)
		{
			return VSFERR_FAIL;
		}
	}
	return VSFERR_NOT_READY;
}

static vsf_err_t sd_spi_transact_cmd_waitready(
	struct sd_spi_drv_interface_t *ifs, struct sd_spi_drv_info_t *drv_info,
	uint16_t token, uint8_t *resp1, uint8_t *resp)
{
	vsf_err_t err;
	
	do {
		err = sd_spi_transact_cmd_isready(ifs, drv_info, token, resp1, resp);
	} while (err > 0);
	return err;
}

static vsf_err_t sd_spi_transact_datablock_init(
	struct sd_spi_drv_interface_t *ifs, struct sd_spi_drv_info_t *drv_info,
	uint16_t token, uint32_t block_num, uint32_t block_size)
{
	REFERENCE_PARAMETER(block_size);
	
	if (token & SD_TRANSTOKEN_DATA_IN)
	{
		drv_info->retry = 312500;
		return VSFERR_NONE;
	}
	else
	{
		uint8_t data_token;
		
		if (block_num > 1)
		{
			data_token = SD_DATATOKEN_START_BLK_MULT;
		}
		else
		{
			data_token = SD_DATATOKEN_START_BLK;
		}
		interfaces->spi.io(ifs->spi_port, &data_token, NULL, 1);
	}
	return VSFERR_NONE;
}

static vsf_err_t sd_spi_transact_datablock(
	struct sd_spi_drv_interface_t *ifs, struct sd_spi_drv_info_t *drv_info,
	uint16_t token, uint8_t *buffer, uint32_t block_size)
{
	uint16_t dummy_crc16;
	
	if (token & SD_TRANSTOKEN_DATA_IN)
	{
		interfaces->spi.io(ifs->spi_port, NULL, buffer, (uint16_t)block_size);
		interfaces->spi.io(ifs->spi_port, NULL, (uint8_t *)&dummy_crc16, 2);
		drv_info->cur_block++;
	}
	else
	{
		interfaces->spi.io(ifs->spi_port, buffer, NULL, (uint16_t)block_size);
		interfaces->spi.io(ifs->spi_port, (uint8_t *)&dummy_crc16, NULL, 2);
		drv_info->state = SD_SPI_DRV_WAITDATATOK;
	}
	return interfaces->peripheral_commit();
}

static vsf_err_t sd_spi_transact_datablock_isready(
	struct sd_spi_drv_interface_t *ifs, struct sd_spi_drv_info_t *drv_info,
	uint16_t token)
{
	uint8_t resp;
	
	resp = 0xFF;
	interfaces->spi.io(ifs->spi_port, &resp, &resp, 1);
	if (interfaces->peripheral_commit())
	{
		return VSFERR_FAIL;
	}
	
	if (token & SD_TRANSTOKEN_DATA_IN)
	{
		if (resp != 0xFF)
		{
			if (SD_DATATOKEN_START_BLK == resp)
			{
				return VSFERR_NONE;
			}
			return VSFERR_FAIL;
		}
		else if (!--drv_info->retry)
		{
			return VSFERR_FAIL;
		}
		else
		{
			return VSFERR_NOT_READY;
		}
	}
	else
	{
		if (SD_SPI_DRV_WAITDATATOK == drv_info->state)
		{
			if ((resp & SD_DATATOKEN_RESP_MASK) != SD_DATATOKEN_RESP_ACCEPTED)
			{
				return VSFERR_FAIL;
			}
			drv_info->state = SD_SPI_DRV_WAITBUSY;
			drv_info->retry = 312500;
			return VSFERR_NOT_READY;
		}
		else if (SD_SPI_DRV_WAITBUSY == drv_info->state)
		{
			if (0xFF == resp)
			{
				drv_info->cur_block++;
				if (drv_info->cur_block >= drv_info->total_block)
				{
					resp = SD_DATATOKEN_STOP_TRAN;
					interfaces->spi.io(ifs->spi_port, &resp, NULL, 1);
					drv_info->state = SD_SPI_DRV_WAITDATALASTBUSY;
					drv_info->retry = 312500;
					return VSFERR_NOT_READY;
				}
				return VSFERR_NONE;
			}
			else if (!--drv_info->retry)
			{
				return VSFERR_FAIL;
			}
			return VSFERR_NOT_READY;
		}
		else if (SD_SPI_DRV_WAITDATALASTBUSY == drv_info->state)
		{
			if (0xFF == resp)
			{
				return VSFERR_NONE;
			}
			else if (!--drv_info->retry)
			{
				return VSFERR_FAIL;
			}
			return VSFERR_NOT_READY;
		}
		return VSFERR_FAIL;
	}
}

static vsf_err_t sd_spi_transact_datablock_waitready(
	struct sd_spi_drv_interface_t *ifs, struct sd_spi_drv_info_t *drv_info,
	uint16_t token)
{
	vsf_err_t err;
	
	do {
		err = sd_spi_transact_datablock_isready(ifs, drv_info, token);
	} while (err > 0);
	return err;
}

static vsf_err_t sd_spi_transact_datablock_fini(
	struct sd_spi_drv_interface_t *ifs, struct sd_spi_drv_info_t *drv_info,
	uint16_t token)
{
	REFERENCE_PARAMETER(ifs);
	REFERENCE_PARAMETER(drv_info);
	REFERENCE_PARAMETER(token);
	return VSFERR_NONE;
}

static vsf_err_t sd_spi_transact_end(struct sd_spi_drv_interface_t *ifs)
{
	sd_spi_drv_cs_deassert(ifs);
	return sd_spi_drv_send_empty_bytes(ifs, 1);
}

static vsf_err_t sd_spi_transact_do(
	struct sd_spi_drv_interface_t *ifs, struct sd_spi_drv_info_t *drv_info,
	uint16_t token, uint8_t cmd, uint32_t arg, uint8_t *resp1, uint8_t *resp,
	uint8_t *data, uint32_t block_num, uint32_t block_size)
{
	if (sd_spi_transact_cmd(ifs, drv_info, token, cmd, arg) ||
		sd_spi_transact_cmd_waitready(ifs, drv_info, token, resp1, resp))
	{
		return VSFERR_FAIL;
	}
	
	if ((token & (SD_TRANSTOKEN_DATA_OUT | SD_TRANSTOKEN_DATA_IN)) &&
		(data != NULL) && (block_size))
	{
		if (sd_spi_transact_datablock_init(ifs, drv_info, token,
											block_num, block_size))
		{
			return VSFERR_FAIL;
		}
		
		if (token & SD_TRANSTOKEN_DATA_IN)
		{
			if (sd_spi_transact_datablock_waitready(ifs, drv_info, token) ||
				sd_spi_transact_datablock(ifs, drv_info, token, data,
											block_size))
			{
				return VSFERR_FAIL;
			}
		}
		else
		{
			if (sd_spi_transact_datablock(ifs, drv_info, token, data,
											block_size) ||
				sd_spi_transact_datablock_waitready(ifs, drv_info, token))
			{
				return VSFERR_FAIL;
			}
		}
		
		if (sd_spi_transact_datablock_fini(ifs, drv_info, token))
		{
			return VSFERR_FAIL;
		}
	}
	return VSFERR_NONE;
}

static vsf_err_t sd_spi_transact(
	struct sd_spi_drv_interface_t *ifs, struct sd_spi_drv_info_t *drv_info,
	uint16_t token, uint8_t cmd, uint32_t arg, uint8_t *resp1, uint8_t *resp,
	uint8_t *data, uint32_t block_num, uint32_t block_size)
{
	vsf_err_t err = VSFERR_NONE;
	
	if (sd_spi_transact_start(ifs) || 
		sd_spi_transact_do(ifs, drv_info, token, cmd, arg, resp1, resp,
							data, block_num, block_size))
	{
		err = VSFERR_FAIL;
	}
	sd_spi_transact_end(ifs);
	interfaces->peripheral_commit();
	return err;
}

static vsf_err_t sd_spi_drv_init_nb(struct dal_info_t *info)
{
	struct sd_spi_drv_info_t *drv_info = (struct sd_spi_drv_info_t *)info->info;
	struct sd_spi_drv_interface_t *ifs =
								(struct sd_spi_drv_interface_t *)info->ifs;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	struct sd_info_t *sd_info = (struct sd_info_t *)mal_info->extra;
	uint8_t resp_r1 = 0xFF, resp_r7[4];
	
	if (sd_spi_transact_init(ifs))
	{
		return VSFERR_FAIL;
	}
	
	// SD Init
	sd_info->cardtype = SD_CARDTYPE_NONE;
	sd_spi_transact(ifs, drv_info, SD_TRANSTOKEN_RESP_R1,
				SD_CMD_GO_IDLE_STATE, 0, &resp_r1, NULL, NULL, 0, 0);
	
	// detect card type
	// send CMD8 to get card op
	if (sd_spi_transact(ifs, drv_info, SD_TRANSTOKEN_RESP_R7 | 4,
				SD_CMD_SEND_IF_COND, SD_CMD8_VHS_27_36_V | SD_CMD8_CHK_PATTERN,
				&resp_r1, resp_r7, NULL, 0, 0))
	{
		return VSFERR_FAIL;
	}
	if ((resp_r1 == SD_CS8_IN_IDLE_STATE) && 
		(resp_r7[3] == SD_CMD8_CHK_PATTERN))
	{
		sd_info->cardtype = SD_CARDTYPE_SD_V2;
	}
	else
	{
		sd_info->cardtype = SD_CARDTYPE_SD_V1;
	}
	
	drv_info->retry = 1000;
	return VSFERR_NONE;
}

static vsf_err_t sd_spi_drv_init_nb_isready(struct dal_info_t *info)
{
	struct sd_spi_drv_info_t *drv_info = (struct sd_spi_drv_info_t *)info->info;
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	struct sd_param_t *param = (struct sd_param_t *)info->param;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	struct sd_info_t *sd_info = (struct sd_info_t *)mal_info->extra;
	uint8_t resp_r1, resp_r7[4];
	uint32_t ocr;
	
	// send acmd41 to get card status
	if (sd_spi_transact(ifs, drv_info, SD_TRANSTOKEN_RESP_R1, 
			SD_CMD_APP_CMD, 0, &resp_r1, NULL, NULL, 0, 0) || 
		sd_spi_transact(ifs, drv_info, SD_TRANSTOKEN_RESP_R1, 
			SD_ACMD_SD_SEND_OP_COND, SD_ACMD41_HCS, &resp_r1, NULL, NULL, 0, 0))
	{
		return VSFERR_FAIL;
	}
	if (resp_r1 != SD_CS8_NONE)
	{
		return !--drv_info->retry ? VSFERR_FAIL : VSFERR_NOT_READY;
	}
	
	if (sd_spi_transact(ifs, drv_info, SD_TRANSTOKEN_RESP_R7 | 4,
			SD_CMD_READ_OCR, 0x40000000, &resp_r1, resp_r7, NULL, 0, 0))
	{
		return VSFERR_FAIL;
	}
	ocr = GET_BE_U32(resp_r7);
	if (SD_CS8_NONE == resp_r1)
	{
		if ((ocr & SD_OCR_BUSY) && (ocr & SD_OCR_CCS))
		{
			if (SD_CARDTYPE_SD_V2 == sd_info->cardtype)
			{
				sd_info->cardtype = SD_CARDTYPE_SD_V2HC;
			}
			else
			{
				sd_info->cardtype = SD_CARDTYPE_MMC_HC;
			}
		}
	}
	
	if ((NULL == sd_spi_drv.getinfo) || 
		(NULL == sd_info) || 
		sd_spi_drv.getinfo(info) || 
		interfaces->spi.config(ifs->spi_port, param->kHz,
								SPI_MODE3 | SPI_MSB_FIRST | SPI_MASTER) || 
		interfaces->peripheral_commit() || 
		sd_spi_transact(ifs, drv_info, SD_TRANSTOKEN_RESP_R1,
				SD_CMD_SET_BLOCKLEN, 512, &resp_r1, NULL, NULL, 0, 0))
	{
		return VSFERR_FAIL;
	}
	mal_info->capacity = sd_info->capacity;
	return VSFERR_NONE;
}

static vsf_err_t sd_spi_drv_fini(struct dal_info_t *info)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	
	return sd_spi_transact_fini(ifs);
}

static vsf_err_t sd_spi_getinfo(struct dal_info_t *info)
{
	struct sd_spi_drv_info_t *drv_info = (struct sd_spi_drv_info_t *)info->info;
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	struct sd_info_t *sd_info = (struct sd_info_t *)mal_info->extra;
	uint16_t token;
	uint8_t csd[16];
	uint8_t resp_r1, resp_r7[4];
	
	drv_info->total_block = 1;
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_IN;
	if ((NULL == sd_info) || 
		sd_spi_transact(ifs, drv_info, token, SD_CMD_SEND_CSD, 0,
						&resp_r1, resp_r7, csd, 1, 16) || 
		sd_parse_csd(csd, sd_info) || 
		sd_spi_transact(ifs, drv_info, token, SD_CMD_SEND_CID, 0,
						&resp_r1, resp_r7, sd_info->cid, 1, 16))
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t sd_spi_drv_readblock_nb_start(struct dal_info_t *info, 
								uint64_t address, uint64_t count, uint8_t *buff)
{
	struct sd_spi_drv_info_t *drv_info = (struct sd_spi_drv_info_t *)info->info;
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	struct sd_info_t *sd_info = (struct sd_info_t *)mal_info->extra;
	uint16_t token;
	uint32_t arg;
	uint8_t resp;
	
	REFERENCE_PARAMETER(buff);
	if (SD_CARDTYPE_SD_V2HC == sd_info->cardtype)
	{
		arg = (uint32_t)(address >> 9);
	}
	else
	{
		arg = (uint32_t)address;
	}
	
	drv_info->total_block = count;
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_IN;
	if (sd_spi_transact_start(ifs) || 
		sd_spi_transact_cmd(ifs, drv_info, token, SD_CMD_READ_MULTIPLE_BLOCK,
							arg) || 
		sd_spi_transact_cmd_waitready(ifs, drv_info, token, &resp, NULL) || 
		(resp != SD_CS8_NONE))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return VSFERR_FAIL;
	}
	
	return sd_spi_transact_datablock_init(ifs, drv_info, token,
										(uint32_t)drv_info->total_block, 512);
}

static vsf_err_t sd_spi_drv_readblock_nb(struct dal_info_t *info, 
										uint64_t address, uint8_t *buff)
{
	struct sd_spi_drv_info_t *drv_info = (struct sd_spi_drv_info_t *)info->info;
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	uint16_t token;
	
	REFERENCE_PARAMETER(address);
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_IN;
	if (sd_spi_transact_datablock(ifs, drv_info, token, buff, 512) ||
		sd_spi_transact_datablock_fini(ifs, drv_info, token))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return VSFERR_FAIL;
	}
	
	if (drv_info->cur_block < drv_info->total_block)
	{
		sd_spi_transact_datablock_init(ifs, drv_info, token,
										(uint32_t)drv_info->total_block, 512);
	}
	return interfaces->peripheral_commit();
}

static vsf_err_t sd_spi_drv_readblock_nb_isready(struct dal_info_t *info, 
												uint64_t address, uint8_t *buff)
{
	struct sd_spi_drv_info_t *drv_info = (struct sd_spi_drv_info_t *)info->info;
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	uint16_t token;
	vsf_err_t err;
	
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_IN;
	err = sd_spi_transact_datablock_isready(ifs, drv_info, token);
	if (err < 0)
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return err;
	}
	
	return err;
}

static vsf_err_t sd_spi_drv_readblock_nb_end(struct dal_info_t *info)
{
	struct sd_spi_drv_info_t *drv_info = (struct sd_spi_drv_info_t *)info->info;
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	uint16_t token;
	uint8_t resp;
	
	token = SD_TRANSTOKEN_RESP_R1B;
	if (sd_spi_transact_cmd(ifs, drv_info, token, SD_CMD_STOP_TRANSMISSION,
							0) || 
		sd_spi_transact_cmd_waitready(ifs, drv_info, token, &resp, NULL))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return VSFERR_FAIL;
	}
	
	sd_spi_transact_end(ifs);
	interfaces->peripheral_commit();
	return VSFERR_NONE;
}

static vsf_err_t sd_spi_drv_writeblock_nb_start(struct dal_info_t *info, 
								uint64_t address, uint64_t count, uint8_t *buff)
{
	struct sd_spi_drv_info_t *drv_info = (struct sd_spi_drv_info_t *)info->info;
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	struct sd_info_t *sd_info = (struct sd_info_t *)mal_info->extra;
	uint16_t token;
	uint32_t arg;
	uint8_t resp;
	
	REFERENCE_PARAMETER(buff);
	if (SD_CARDTYPE_SD_V2HC == sd_info->cardtype)
	{
		arg = (uint32_t)(address >> 9);
	}
	else
	{
		arg = (uint32_t)address;
	}
	
	drv_info->total_block = count;
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_OUT;
	if (sd_spi_transact_start(ifs) || 
		sd_spi_transact_cmd(ifs, drv_info, token, SD_CMD_WRITE_MULTIPLE_BLOCK,
							arg) || 
		sd_spi_transact_cmd_waitready(ifs, drv_info, token, &resp, NULL) || 
		(resp != SD_CS8_NONE))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return VSFERR_FAIL;
	}
	
	return interfaces->peripheral_commit();
}

static vsf_err_t sd_spi_drv_writeblock_nb(struct dal_info_t *info, 
										uint64_t address, uint8_t *buff)
{
	struct sd_spi_drv_info_t *drv_info = (struct sd_spi_drv_info_t *)info->info;
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	uint16_t token;
	
	REFERENCE_PARAMETER(address);
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_OUT;
	// force using 2 as block_num, because SD_CMD_WRITE_MULTIPLE_BLOCK is used
	if (sd_spi_transact_datablock_init(ifs, drv_info, token, 2, 512) || 
		sd_spi_transact_datablock(ifs, drv_info, token, buff, 512))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return VSFERR_FAIL;
	}
	
	return interfaces->peripheral_commit();
}

static vsf_err_t sd_spi_drv_writeblock_nb_isready(struct dal_info_t *info, 
												uint64_t address, uint8_t *buff)
{
	struct sd_spi_drv_info_t *drv_info = (struct sd_spi_drv_info_t *)info->info;
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	uint16_t token;
	vsf_err_t err;
	
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_OUT;
	err = sd_spi_transact_datablock_isready(ifs, drv_info, token);
	if (err < 0)
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return err;
	}
	if (!err)
	{
		sd_spi_transact_datablock_fini(ifs, drv_info, token);
	}
	
	return err;
}

static vsf_err_t sd_spi_drv_writeblock_nb_end(struct dal_info_t *info)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	
	sd_spi_transact_end(ifs);
	interfaces->peripheral_commit();
	return VSFERR_NONE;
}

#if DAL_INTERFACE_PARSER_EN
static vsf_err_t sd_spi_drv_parse_interface(struct dal_info_t *info, uint8_t *buff)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;	
	
	ifs->spi_port = buff[0];
	ifs->cs_port = buff[1];
	ifs->cs_pin = *(uint32_t *)&buff[2];
	return VSFERR_NONE;
}
#endif

const struct mal_driver_t sd_spi_drv = 
{
	{
		"sd_spi",
#if DAL_INTERFACE_PARSER_EN
		"spi:%1dcs:%1d,%4x",
		sd_spi_drv_parse_interface,
#endif
	},
	
	MAL_SUPPORT_READBLOCK | MAL_SUPPORT_WRITEBLOCK,
	
	sd_spi_drv_init_nb,
	sd_spi_drv_init_nb_isready,
	sd_spi_drv_fini,
	sd_spi_getinfo,
	NULL,
	
	NULL, NULL, NULL, NULL,
	
	NULL,
	NULL,
	NULL,
	NULL,
	
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	
	sd_spi_drv_readblock_nb_start,
	sd_spi_drv_readblock_nb,
	sd_spi_drv_readblock_nb_isready,
	NULL,
	sd_spi_drv_readblock_nb_end,
	
	sd_spi_drv_writeblock_nb_start,
	sd_spi_drv_writeblock_nb,
	sd_spi_drv_writeblock_nb_isready,
	NULL,
	sd_spi_drv_writeblock_nb_end
};

#endif
