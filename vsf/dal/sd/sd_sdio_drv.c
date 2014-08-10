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

#if DAL_SD_SDIO_EN

#include "sd_common.h"
#include "sd_sdio_drv.h"


static vsf_err_t sd_sdio_transact_init(struct sd_sdio_drv_interface_t *ifs)
{
	interfaces->sdio.init(ifs->sdio_port);
	// use slowest sdio speed when initializing
	interfaces->sdio.config(ifs->sdio_port, 400, 1);
	return interfaces->sdio.start(ifs->sdio_port);
}

static vsf_err_t sd_sdio_transact_fini(struct sd_sdio_drv_interface_t *ifs)
{
	interfaces->sdio.stop(ifs->sdio_port);
	return interfaces->sdio.fini(ifs->sdio_port);
}

static vsf_err_t sd_sdio_transact_start(struct sd_sdio_drv_interface_t *ifs)
{
	return VSFERR_NONE;
}

static vsf_err_t sd_sdio_transact_cmd(struct sd_sdio_drv_interface_t *ifs,
	struct sd_sdio_drv_info_t *drv_info, uint16_t token,
	uint8_t cmd, uint32_t arg)
{
	uint8_t resp_token = SDIO_RESP_NONE;
	
	if (token & SD_TRANSTOKEN_RESP_LONG)
	{
		resp_token = SDIO_RESP_LONG;
	}
	else if (token & SD_TRANSTOKEN_RESP_SHORT)
	{
		resp_token = SDIO_RESP_SHORT;
	}
	return interfaces->sdio.send_cmd(ifs->sdio_port, cmd, arg, resp_token);
}

static vsf_err_t sd_sdio_transact_cmd_isready(
	struct sd_sdio_drv_interface_t *ifs, struct sd_sdio_drv_info_t *drv_info,
	uint16_t token, uint8_t cmd, uint32_t *resp)
{
	vsf_err_t err;
	uint8_t resp_token = SDIO_RESP_NONE;
	
	if (token & SD_TRANSTOKEN_RESP_LONG)
	{
		resp_token = SDIO_RESP_LONG;
	}
	else if (token & SD_TRANSTOKEN_RESP_SHORT)
	{
		resp_token = SDIO_RESP_SHORT;
	}
	
	err = interfaces->sdio.send_cmd_isready(ifs->sdio_port, resp_token);
	if (!err)
	{
		uint8_t cresp;
		uint8_t resp_num = token & SD_TRANSTOKEN_RESP_LENMSK;
		
		if (interfaces->sdio.get_resp(ifs->sdio_port, &cresp, resp, resp_num) ||
			((token & SD_TRANSTOKEN_RESP_CMD) && (cresp != cmd)))
		{
			return VSFERR_FAIL;
		}
	}
	return err;
}

static vsf_err_t sd_sdio_transact_cmd_waitready(
	struct sd_sdio_drv_interface_t *ifs, struct sd_sdio_drv_info_t *drv_info,
	uint16_t token, uint8_t cmd, uint32_t *resp)
{
	vsf_err_t err;
	
	do {
		err = sd_sdio_transact_cmd_isready(ifs, drv_info, token, cmd, resp);
	} while (err > 0);
	return err;
}

static vsf_err_t sd_sdio_transact_datablock_init(
	struct sd_sdio_drv_interface_t *ifs, struct sd_sdio_drv_info_t *drv_info,
	uint16_t token, uint32_t block_num, uint32_t block_size)
{
	if (token & SD_TRANSTOKEN_DATA_IN)
	{
		return interfaces->sdio.data_rx(ifs->sdio_port, 1000,
							block_num * block_size, block_size);
	}
	else if (token & SD_TRANSTOKEN_DATA_OUT)
	{
		return interfaces->sdio.data_tx(ifs->sdio_port, 1000,
							block_num * block_size, block_size);
	}
	
	return VSFERR_NONE;
}

static vsf_err_t sd_sdio_transact_datablock_isready(
	struct sd_sdio_drv_interface_t *ifs, struct sd_sdio_drv_info_t *drv_info,
	uint16_t token, uint32_t size, uint8_t *buffer)
{
	if (token & SD_TRANSTOKEN_DATA_IN)
	{
		return interfaces->sdio.data_rx_isready(ifs->sdio_port, size, buffer);
	}
	else if (token & SD_TRANSTOKEN_DATA_OUT)
	{
		return interfaces->sdio.data_tx_isready(ifs->sdio_port, size, buffer);
	}
	else
	{
		return VSFERR_FAIL;
	}
}

static vsf_err_t sd_sdio_transact_datablock_waitready(
	struct sd_sdio_drv_interface_t *ifs, struct sd_sdio_drv_info_t *drv_info,
	uint16_t token, uint32_t size, uint8_t *buffer)
{
	vsf_err_t err;
	
	do {
		err = sd_sdio_transact_datablock_isready(ifs, drv_info, token,
													size, buffer);
	} while (err > 0);
	return err;
}

static vsf_err_t sd_sdio_transact_end(struct sd_sdio_drv_interface_t *ifs)
{
	return VSFERR_NONE;
}

static vsf_err_t sd_sdio_transact_do(
	struct sd_sdio_drv_interface_t *ifs, struct sd_sdio_drv_info_t *drv_info,
	uint16_t token, uint8_t cmd, uint32_t arg, uint32_t *resp,
	uint8_t *data, uint32_t block_num, uint32_t block_size)
{
	if ((token & (SD_TRANSTOKEN_DATA_OUT | SD_TRANSTOKEN_DATA_IN)) &&
		(data != NULL) && (block_size))
	{
		if (sd_sdio_transact_datablock_init(ifs, drv_info, token,
											block_num, block_size))
		{
			return VSFERR_FAIL;
		}
	}
	
	if (sd_sdio_transact_cmd(ifs, drv_info, token, cmd, arg) ||
		sd_sdio_transact_cmd_waitready(ifs, drv_info, token, cmd, resp))
	{
		return VSFERR_FAIL;
	}
	
	if ((token & (SD_TRANSTOKEN_DATA_OUT | SD_TRANSTOKEN_DATA_IN)) &&
		(data != NULL) && (block_size))
	{
		if (sd_sdio_transact_datablock_waitready(ifs, drv_info, token,
												block_num * block_size, data))
		{
			return VSFERR_FAIL;
		}
	}
	return VSFERR_NONE;
}

static vsf_err_t sd_sdio_transact(
	struct sd_sdio_drv_interface_t *ifs, struct sd_sdio_drv_info_t *drv_info,
	uint16_t token, uint8_t cmd, uint32_t arg, uint32_t *resp,
	uint8_t *data, uint32_t block_num, uint32_t block_size)
{
	vsf_err_t err = VSFERR_NONE;
	
	if (sd_sdio_transact_start(ifs) || 
		sd_sdio_transact_do(ifs, drv_info, token, cmd, arg, resp, data,
							block_num, block_size))
	{
		err = VSFERR_FAIL;
	}
	sd_sdio_transact_end(ifs);
	interfaces->peripheral_commit();
	return err;
}

static vsf_err_t sd_sdio_drv_init(struct dal_info_t *info)
{
	struct sd_sdio_drv_info_t *drv_info =
								(struct sd_sdio_drv_info_t *)info->info;
	struct sd_sdio_drv_interface_t *ifs =
								(struct sd_sdio_drv_interface_t *)info->ifs;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	struct sd_info_t *sd_info = (struct sd_info_t *)mal_info->extra;
	uint32_t resp;
	vsf_err_t err;
	
	if (sd_sdio_transact_init(ifs))
	{
		return VSFERR_FAIL;
	}
	
	// SD Init
	sd_info->cardtype = SD_CARDTYPE_NONE;
	
	sd_sdio_transact(ifs, drv_info, SD_TRANSTOKEN_RESP_NONE,
						SD_CMD_GO_IDLE_STATE, 0, NULL, NULL, 0, 0);
	
	// detect card type
	// send CMD8 to get card op
	err = sd_sdio_transact(ifs, drv_info, SD_TRANSTOKEN_RESP_R7 | 1,
				SD_CMD_SEND_IF_COND, SD_CMD8_VHS_27_36_V | SD_CMD8_CHK_PATTERN,
				&resp, NULL, 0, 0);
	if (!err && ((resp & 0xFF) == SD_CMD8_CHK_PATTERN))
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

static vsf_err_t sd_sdio_drv_init_isready(struct dal_info_t *info)
{
	struct sd_sdio_drv_info_t *drv_info =
								(struct sd_sdio_drv_info_t *)info->info;
	struct sd_sdio_drv_interface_t *ifs =
								(struct sd_sdio_drv_interface_t *)info->ifs;
	struct sd_param_t *param = (struct sd_param_t *)info->param;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	struct sd_info_t *sd_info = (struct sd_info_t *)mal_info->extra;
	uint32_t ocr;
	
	// send acmd41 to get card status
	if (sd_sdio_transact(ifs, drv_info, SD_TRANSTOKEN_RESP_R1,
			SD_CMD_APP_CMD, 0, NULL, NULL, 0, 0) ||
		sd_sdio_transact(ifs, drv_info, SD_TRANSTOKEN_RESP_R3 | 1,
			SD_ACMD_SD_SEND_OP_COND, SD_ACMD41_HCS | SD_OCR_32_33V,
			&ocr, NULL, 0, 0))
	{
		return VSFERR_FAIL;
	}
	if (!(ocr & SD_OCR_BUSY))
	{
		return !--drv_info->retry ? VSFERR_FAIL : VSFERR_NOT_READY;
	}
	if (ocr & SD_OCR_CCS)
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
	
	if ((NULL == sd_sdio_drv.getinfo) ||
		(NULL == sd_info) ||
		sd_sdio_drv.getinfo(info) ||
		interfaces->sdio.config(ifs->sdio_port, param->kHz, 1) ||
		interfaces->peripheral_commit() ||
		// select card
		sd_sdio_transact(ifs, drv_info, SD_TRANSTOKEN_RESP_R1,
				SD_CMD_SEL_DESEL_CARD, drv_info->rca << 16, NULL, NULL, 0, 0))
	{
		return VSFERR_FAIL;
	}
	
	// read scr
	if (sd_sdio_transact(ifs, drv_info, SD_TRANSTOKEN_RESP_R1,
				SD_CMD_SET_BLOCKLEN, 8, NULL, NULL, 0, 0) ||
		sd_sdio_transact(ifs, drv_info, SD_TRANSTOKEN_RESP_R1,
				SD_CMD_APP_CMD, drv_info->rca << 16, NULL, NULL, 0, 0) ||
		sd_sdio_transact(ifs, drv_info,
				SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_IN,
				SD_ACMD_SEND_SCR, 0, NULL, (uint8_t *)&drv_info->scr, 1, 8))
	{
		return VSFERR_FAIL;
	}
	drv_info->scr = GET_BE_U64(&drv_info->scr);
	
	// enable wide bus
	if ((sd_info->cardtype != SD_CARDTYPE_MMC) &&
		(sd_info->cardtype != SD_CARDTYPE_MMC_HC))
	{
		if (8 == ifs->sdio_bus_width)
		{
		}
		if (4 == ifs->sdio_bus_width)
		{
			if (drv_info->scr & SD_SCR_BUSWIDTH_4BIT)
			{
				if (sd_sdio_transact(ifs, drv_info, SD_TRANSTOKEN_RESP_R1,
						SD_CMD_APP_CMD, drv_info->rca << 16, NULL, NULL, 0, 0) ||
					sd_sdio_transact(ifs, drv_info, SD_TRANSTOKEN_RESP_R1,
						SD_ACMD_BUS_WIDTH, 0x02, NULL, NULL, 0, 0) ||
					interfaces->sdio.config(ifs->sdio_port, param->kHz, 4))
				{
					return VSFERR_FAIL;
				}
			}
			else
			{
				ifs->sdio_bus_width = 1;
			}
		}
		if (1 == ifs->sdio_bus_width)
		{
			if (drv_info->scr & SD_SCR_BUSWIDTH_1BIT)
			{
				if (sd_sdio_transact(ifs, drv_info, SD_TRANSTOKEN_RESP_R1,
						SD_CMD_APP_CMD, drv_info->rca << 16, NULL, NULL, 0, 0) ||
					sd_sdio_transact(ifs, drv_info, SD_TRANSTOKEN_RESP_R1,
						SD_ACMD_BUS_WIDTH, 0x00, NULL, NULL, 0, 0) ||
					interfaces->sdio.config(ifs->sdio_port, param->kHz, 1))
				{
					return VSFERR_FAIL;
				}
			}
			else
			{
				return VSFERR_FAIL;
			}
		}
	}
	
	if (sd_sdio_transact(ifs, drv_info, SD_TRANSTOKEN_RESP_R1,
							SD_CMD_SET_BLOCKLEN, 512, NULL, NULL, 0, 0))
	{
		return VSFERR_FAIL;
	}
	mal_info->capacity = sd_info->capacity;
	return VSFERR_NONE;
}

static vsf_err_t sd_sdio_drv_fini(struct dal_info_t *info)
{
	struct sd_sdio_drv_interface_t *ifs =
								(struct sd_sdio_drv_interface_t *)info->ifs;
	
	return sd_sdio_transact_fini(ifs);
}

static vsf_err_t sd_sdio_getinfo(struct dal_info_t *info)
{
	struct sd_sdio_drv_info_t *drv_info =
								(struct sd_sdio_drv_info_t *)info->info;
	struct sd_sdio_drv_interface_t *ifs =
								(struct sd_sdio_drv_interface_t *)info->ifs;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	struct sd_info_t *sd_info = (struct sd_info_t *)mal_info->extra;
	uint32_t csd[4];
	uint32_t rca = 1, tmp32;
	
	if ((NULL == sd_info) ||
		sd_sdio_transact(ifs, drv_info, SD_TRANSTOKEN_RESP_R2 | 4,
				SD_CMD_ALL_SEND_CID, 0, (uint32_t *)sd_info->cid, NULL, 0, 0) ||
		sd_sdio_transact(ifs, drv_info, SD_TRANSTOKEN_RESP_R6 | 1,
				SD_CMD_SEND_RELATIVE_ADDR, 0, &rca, NULL, 0, 0) ||
		sd_sdio_transact(ifs, drv_info, SD_TRANSTOKEN_RESP_R2 | 4,
				SD_CMD_SEND_CSD, rca & 0xFFFF0000, csd, NULL, 0, 0))
	{
		return VSFERR_FAIL;
	}
	
	drv_info->rca = rca >> 16;
	
	tmp32 = GET_BE_U32(&sd_info->cid[0]);
	SET_LE_U32(&sd_info->cid[0], tmp32);
	tmp32 = GET_BE_U32(&sd_info->cid[4]);
	SET_LE_U32(&sd_info->cid[4], tmp32);
	tmp32 = GET_BE_U32(&sd_info->cid[8]);
	SET_LE_U32(&sd_info->cid[8], tmp32);
	tmp32 = GET_BE_U32(&sd_info->cid[12]);
	SET_LE_U32(&sd_info->cid[12], tmp32);
	
	tmp32 = GET_BE_U32(&csd[0]);
	SET_LE_U32(&csd[0], tmp32);
	tmp32 = GET_BE_U32(&csd[1]);
	SET_LE_U32(&csd[1], tmp32);
	tmp32 = GET_BE_U32(&csd[2]);
	SET_LE_U32(&csd[2], tmp32);
	tmp32 = GET_BE_U32(&csd[3]);
	SET_LE_U32(&csd[3], tmp32);
	sd_parse_csd((uint8_t *)csd, sd_info);
	return VSFERR_NONE;
}

static vsf_err_t sd_sdio_drv_readblock_nb_start(struct dal_info_t *info, 
								uint64_t address, uint64_t count, uint8_t *buff)
{
	struct sd_sdio_drv_info_t *drv_info =
								(struct sd_sdio_drv_info_t *)info->info;
	struct sd_sdio_drv_interface_t *ifs =
								(struct sd_sdio_drv_interface_t *)info->ifs;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	struct sd_info_t *sd_info = (struct sd_info_t *)mal_info->extra;
	uint16_t token;
	uint32_t arg;
	
	REFERENCE_PARAMETER(buff);
	if (SD_CARDTYPE_SD_V2HC == sd_info->cardtype)
	{
		arg = (uint32_t)(address >> 9);
	}
	else
	{
		arg = (uint32_t)address;
	}
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_IN;
	if (sd_sdio_transact_start(ifs) ||
		sd_sdio_transact_datablock_init(ifs, drv_info, token, count, 512) ||
		sd_sdio_transact_cmd(ifs, drv_info, token,
											SD_CMD_READ_MULTIPLE_BLOCK, arg) ||
		sd_sdio_transact_cmd_waitready(ifs, drv_info, token,
											SD_CMD_READ_MULTIPLE_BLOCK, NULL))
	{
		sd_sdio_transact_end(ifs);
		interfaces->peripheral_commit();
		return VSFERR_FAIL;
	}
	
	drv_info->total_size = count * 512;
	return interfaces->peripheral_commit();
}

static vsf_err_t sd_sdio_drv_readblock_nb(struct dal_info_t *info, 
										uint64_t address, uint8_t *buff)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	return VSFERR_NONE;
}

static vsf_err_t sd_sdio_drv_readblock_nb_isready(struct dal_info_t *info, 
												uint64_t address, uint8_t *buff)
{
	struct sd_sdio_drv_info_t *drv_info =
								(struct sd_sdio_drv_info_t *)info->info;
	struct sd_sdio_drv_interface_t *ifs =
								(struct sd_sdio_drv_interface_t *)info->ifs;
	uint16_t token;
	vsf_err_t err;
	
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_IN;
	err = sd_sdio_transact_datablock_isready(ifs, drv_info, token, 512, buff);
	if (err < 0)
	{
		sd_sdio_transact_end(ifs);
		interfaces->peripheral_commit();
		return err;
	}
	
	return err;
}

static vsf_err_t sd_sdio_drv_readblock_nb_end(struct dal_info_t *info)
{
	struct sd_sdio_drv_info_t *drv_info =
								(struct sd_sdio_drv_info_t *)info->info;
	struct sd_sdio_drv_interface_t *ifs =
								(struct sd_sdio_drv_interface_t *)info->ifs;
	uint16_t token;
	
	token = SD_TRANSTOKEN_RESP_R1B;
	if (sd_sdio_transact_cmd(ifs, drv_info, token,
											SD_CMD_STOP_TRANSMISSION, 0) || 
		sd_sdio_transact_cmd_waitready(ifs, drv_info, token,
											SD_CMD_STOP_TRANSMISSION, NULL))
	{
		sd_sdio_transact_end(ifs);
		interfaces->peripheral_commit();
		return VSFERR_FAIL;
	}
	
	sd_sdio_transact_end(ifs);
	interfaces->peripheral_commit();
	return VSFERR_NONE;
}
/*
static vsf_err_t sd_sdio_drv_writeblock_nb_start(struct dal_info_t *info, 
								uint64_t address, uint64_t count, uint8_t *buff)
{
	struct sd_sdio_drv_info_t *drv_info =
								(struct sd_sdio_drv_info_t *)info->info;
	struct sd_sdio_drv_interface_t *ifs =
								(struct sd_sdio_drv_interface_t *)info->ifs;
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
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_OUT;
	if (sd_sdio_transact_start(ifs) || 
		sd_sdio_transact_cmd(ifs, token, SD_CMD_WRITE_MULTIPLE_BLOCK, arg,
							(uint32_t)count) || 
		sd_sdio_transact_cmd_waitready(ifs, token, &resp, NULL) || 
		(resp != SD_CS8_NONE))
	{
		sd_sdio_transact_end(ifs);
		interfaces->peripheral_commit();
		return VSFERR_FAIL;
	}
	
	return interfaces->peripheral_commit();
}

static vsf_err_t sd_sdio_drv_writeblock_nb(struct dal_info_t *info, 
										uint64_t address, uint8_t *buff)
{
	struct sd_sdio_drv_info_t *drv_info =
								(struct sd_sdio_drv_info_t *)info->info;
	struct sd_sdio_drv_interface_t *ifs =
								(struct sd_sdio_drv_interface_t *)info->ifs;
	uint16_t token;
	
	REFERENCE_PARAMETER(address);
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_OUT;
	if (sd_sdio_transact_datablock_init(ifs, token, buff, 
											SD_DATATOKEN_START_BLK_MULT) || 
		sd_sdio_transact_datablock(ifs, token, buff, 512))
	{
		sd_sdio_transact_end(ifs);
		interfaces->peripheral_commit();
		return VSFERR_FAIL;
	}
	
	return interfaces->peripheral_commit();
}

static vsf_err_t sd_sdio_drv_writeblock_nb_isready(struct dal_info_t *info, 
												uint64_t address, uint8_t *buff)
{
	struct sd_sdio_drv_info_t *drv_info =
								(struct sd_sdio_drv_info_t *)info->info;
	struct sd_sdio_drv_interface_t *ifs =
								(struct sd_sdio_drv_interface_t *)info->ifs;
	uint16_t token;
	vsf_err_t err;
	
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_OUT;
	err = sd_sdio_transact_datablock_isready(ifs, token);
	if (err < 0)
	{
		sd_sdio_transact_end(ifs);
		interfaces->peripheral_commit();
		return err;
	}
	
	return err;
}

static vsf_err_t sd_sdio_drv_writeblock_nb_end(struct dal_info_t *info)
{
	struct sd_sdio_drv_info_t *drv_info =
								(struct sd_sdio_drv_info_t *)info->info;
	struct sd_sdio_drv_interface_t *ifs =
								(struct sd_sdio_drv_interface_t *)info->ifs;
	uint8_t datatoken, resp;
	
	datatoken = SD_DATATOKEN_STOP_TRAN;
	if (interfaces->spi.io(ifs->spi_port, &datatoken, NULL, 1) || 
		sd_sdio_transact_cmd_waitready(ifs, SD_TRANSTOKEN_RESP_CHECKBUSY,
											&resp, NULL))
	{
		sd_sdio_transact_end(ifs);
		interfaces->peripheral_commit();
		return VSFERR_FAIL;
	}
	
	sd_sdio_transact_end(ifs);
	interfaces->peripheral_commit();
	return VSFERR_NONE;
}
*/
#if DAL_INTERFACE_PARSER_EN
static vsf_err_t sd_sdio_drv_parse_interface(struct dal_info_t *info, uint8_t *buff)
{
	struct sd_sdio_drv_interface_t *ifs = 
								(struct sd_sdio_drv_interface_t *)info->ifs;	
	
	ifs->sdio_port = buff[0];
	ifs->sdio_bus_width = buff[1];
	return VSFERR_NONE;
}
#endif

const struct mal_driver_t sd_sdio_drv = 
{
	{
		"sd_sdio",
#if DAL_INTERFACE_PARSER_EN
		"sdio:%1dbuswidth:%1d",
		sd_sdio_drv_parse_interface,
#endif
	},
	
	MAL_SUPPORT_READBLOCK | MAL_SUPPORT_WRITEBLOCK,
	
	sd_sdio_drv_init,
	sd_sdio_drv_init_isready,
	sd_sdio_drv_fini,
	sd_sdio_getinfo,
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
	
	sd_sdio_drv_readblock_nb_start,
	sd_sdio_drv_readblock_nb,
	sd_sdio_drv_readblock_nb_isready,
	NULL,
	sd_sdio_drv_readblock_nb_end,
	
//	sd_sdio_drv_writeblock_nb_start,
//	sd_sdio_drv_writeblock_nb,
//	sd_sdio_drv_writeblock_nb_isready,
	NULL,
//	sd_sdio_drv_writeblock_nb_end
};

#endif
