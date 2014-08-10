/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       JTAG_TAP.c                                                *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    JTAG interface implementation file                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_JTAG_EN

#include "app_interfaces.h"
#include "JTAG_TAP.h"

#define JTAG_TAP_DR_IR_PREPARE_DLY()	

#define JTAG_TAP_TMS_2TLR				0xFF
#define JTAG_TAP_TMS_2RTI				0x7F
#define JTAG_TAP_TMS_E12UPDATE			0xC0

#define JTAG_TAP_TMS_UPDATERTI2SD		0x01
#define JTAG_TAP_TMS_UPDATERTI2SD_LEN	3
#define JTAG_TAP_TMS_UPDATERTI2SI		0x03
#define JTAG_TAP_TMS_UPDATERTI2SI_LEN	4

#define JTAG_TAP_ASYN					0
#define JTAG_TAP_RAW					1

#define JTAG_TAP_Reset_ASYN()			\
	JTAG_TAP_WriteTMSByte_ASYN(JTAG_TAP_TMS_2RTI)
#define JTAG_TAP_WriteTMSByte_ASYN(tms)	JTAG_TAP_Operate_Asyn(0, tms)

static uint32_t JTAG_TAP_UnitsBefore, JTAG_TAP_UnitsAfter,
				JTAG_TAP_BitsBefore, JTAG_TAP_BitsAfter;
void (*JTAG_TAP_Operate_RAW)(uint32_t bit_len, uint8_t *tdi, uint8_t *tms,
								uint8_t *tdo);
uint16_t (*JTAG_TAP_Operate_Asyn)(uint16_t tdi, uint16_t tms);

static int32_t JTAG_kHz = 0xFFFFFFFF;

static void JTAG_TAP_RTCK_Wait(uint8_t signal)
{
	uint32_t retry = 1000;	// 1000us = 1ms, min clock is 1K

	while (retry--)
	{
		if ((signal && JTAG_TAP_RTCK_GET()) 
			|| (!signal && !JTAG_TAP_RTCK_GET()))
		{
			break;
		}
		app_interfaces.delay.delayus(1);
	}
}

static void JTAG_TAP_TCK_Toggle(void)
{
	if (JTAG_TAP_TCK_GET())
	{
		JTAG_TAP_TCK_CLR();
		if (JTAG_kHz)
		{
			app_interfaces.delay.delayus(500 / JTAG_kHz);
		}
		else
		{
			JTAG_TAP_RTCK_Wait(0);
		}
	}
	else
	{
		JTAG_TAP_TCK_SET();
		if (JTAG_kHz)
		{
			app_interfaces.delay.delayus(500 / JTAG_kHz);
		}
		else
		{
			JTAG_TAP_RTCK_Wait(1);
		}
	}
}

static uint16_t JTAG_TAP_GPIO_Operate_Asyn(uint16_t tdi, uint16_t tms)
{
	static uint16_t tdo = 0;
	uint16_t result = tdo;
	uint8_t i;

	tdo = 0;
	for (i = 0; i < 8; i++)
	{
		tdo >>= 1;

		if (tdi & 1)
		{
			JTAG_TAP_TDI_SET();
		}
		else
		{
			JTAG_TAP_TDI_CLR();
		}
		if (tms & 1)
		{
			JTAG_TAP_TMS_SET();
		}
		else
		{
			JTAG_TAP_TMS_CLR();
		}

		JTAG_TAP_TCK_Toggle();
		JTAG_TAP_TCK_Toggle();
		if (JTAG_TAP_TDO_GET())
		{
			tdo |= 0x80;
		}

		tdi >>= 1;
		tms >>= 1;
	}

	return result;
}

static void JTAG_TAP_GPIO_Operate_RAW(uint32_t bit_len, uint8_t *tdi,
										uint8_t *tms, uint8_t *tdo)
{
	uint32_t offset;
	uint8_t mask;
	uint32_t i;

	for (i = 0; i < bit_len; i++)
	{
		offset = i >> 3;
		mask = 1 << (i & 7);
		if (1 == mask)
		{
			tdo[offset] = 0;
		}

		if (tdi[offset] & mask)
		{
			JTAG_TAP_TDI_SET();
		}
		else
		{
			JTAG_TAP_TDI_CLR();
		}
		if (tms[offset] & mask)
		{
			JTAG_TAP_TMS_SET();
		}
		else
		{
			JTAG_TAP_TMS_CLR();
		}

		JTAG_TAP_TCK_Toggle();
		JTAG_TAP_TCK_Toggle();
		if (JTAG_TAP_TDO_GET())
		{
			tdo[offset] |= mask;
		}
	}
}

static void JTAG_TAP_HS_Operate_RAW_DMA(uint32_t bit_len, uint8_t *tdi,
										uint8_t *tms, uint8_t *tdo)
{
	uint16_t i, byte_len = bit_len >> 3;

	if (byte_len)
	{
		JTAG_TAP_HS_SPI_M_RX_DMA_LEN(byte_len);
		JTAG_TAP_HS_SPI_M_RX_DMA_ADDR((uint32_t)tdo);
		JTAG_TAP_HS_SPI_M_RX_DMA_EN();
		JTAG_TAP_HS_SPI_S_TX_DMA_LEN(byte_len);
		JTAG_TAP_HS_SPI_S_TX_DMA_ADDR((uint32_t)tms);
		JTAG_TAP_HS_SPI_S_TX_DMA_EN();

		for(i = 0; i < byte_len; i++)
		{
			JTAG_TAP_HS_WaitTxReady();
			JTAG_TAP_HS_TDI_Out(tdi[i]);
		}

		JTAG_TAP_HS_SPI_M_RX_DMA_WAIT();
		JTAG_TAP_HS_SPI_S_TX_DMA_WAIT();

		JTAG_TAP_HS_SPI_M_RX_DMA_DIS();
		JTAG_TAP_HS_SPI_S_TX_DMA_DIS();
	}
	tdi += byte_len;
	tms += byte_len;
	tdo += byte_len;
	if (bit_len & 7)
	{
		JTAG_TAP_HS_SPIS_Disable();
		JTAG_TAP_TCK_SET();
		JTAG_TAP_TCK_SETOUTPUT();
		JTAG_TAP_TDO_SETINPUT();
		JTAG_TAP_TDI_SETOUTPUT();
		JTAG_TAP_TMS_SETOUTPUT();
		
		JTAG_TAP_GPIO_Operate_RAW(bit_len & 7, tdi, tms, tdo);

		JTAG_TAP_HS_SPIS_Enable();
		JTAG_TAP_HS_PortIOInit();
	}
}

static uint16_t JTAG_TAP_HS_Operate_Asyn(uint16_t tdi, uint16_t tms)
{
	uint16_t tdo;

	JTAG_TAP_HS_WaitRxReady();
	tdo = JTAG_TAP_HS_In();
	JTAG_TAP_HS_Out(tms, tdi);

	return tdo;
}





static void JTAG_TAP_RW(uint8_t *tdo, uint8_t *tdi, uint8_t tms_before,
				uint8_t tms_after0, uint8_t tms_after1, uint16_t dat_byte_len)
{
	uint8_t tdo_tmp;
	uint16_t ret_len = 0, cur_pos = 0;

	if(dat_byte_len & 0x8000)
	{
		JTAG_TAP_Operate_Asyn(tdi[cur_pos++], tms_before);
		ret_len++;
		dat_byte_len &= 0x7FFF;
		dat_byte_len--;
	}

	JTAG_TAP_DR_IR_PREPARE_DLY();
	dat_byte_len--;
	while(dat_byte_len-- > 0)
	{
		tdo_tmp = JTAG_TAP_Operate_Asyn(tdi[cur_pos++], 0);
		if(ret_len > 0)
		{
			tdo[ret_len - 1] = tdo_tmp;
		}
		ret_len++;
	}
	tdo_tmp = JTAG_TAP_Operate_Asyn(tdi[cur_pos], tms_after0);
	if(ret_len > 0)
	{
		tdo[ret_len - 1] = tdo_tmp;
	}
	tdo_tmp = JTAG_TAP_Operate_Asyn(0, tms_after1);
	tdo[ret_len] = tdo_tmp;
}
/*
static void JTAG_TAP_R(uint8_t *tdo, uint8_t tms_before, uint8_t tms_after0,
						uint8_t tms_after1, uint16_t dat_byte_len)
{
	uint8_t tdo_tmp;
	uint16_t ret_len = 0;

	if(dat_byte_len & 0x8000)
	{
		JTAG_TAP_Operate_Asyn(0, tms_before);
		ret_len++;
		dat_byte_len &= 0x7FFF;
		dat_byte_len--;
	}

	JTAG_TAP_DR_IR_PREPARE_DLY();
	dat_byte_len--;
	while(dat_byte_len-- > 0)
	{
		tdo_tmp = JTAG_TAP_Operate_Asyn(0, 0);
		if(ret_len > 0)
		{
			tdo[ret_len - 1] = tdo_tmp;
		}
		ret_len++;
	}
	tdo_tmp = JTAG_TAP_Operate_Asyn(0, tms_after0);
	if(ret_len > 0)
	{
		tdo[ret_len - 1] = tdo_tmp;
	}
	tdo_tmp = JTAG_TAP_Operate_Asyn(0, tms_after1);
	tdo[ret_len] = tdo_tmp;
}

static void JTAG_TAP_W(uint8_t *tdi, uint8_t tms_before, uint8_t tms_after0,
						uint8_t tms_after1, uint16_t dat_byte_len)
{
	uint16_t cur_pos = 0;

	if(dat_byte_len & 0x8000)
	{
		JTAG_TAP_Operate_Asyn(tdi[cur_pos++], tms_before);
		dat_byte_len &= 0x7FFF;
		dat_byte_len--;
	}

	JTAG_TAP_DR_IR_PREPARE_DLY();
	dat_byte_len--;
	while(dat_byte_len-- > 0)
	{
		JTAG_TAP_Operate_Asyn(tdi[cur_pos++], 0);
	}
	JTAG_TAP_Operate_Asyn(tdi[cur_pos], tms_after0);
	JTAG_TAP_Operate_Asyn(0, tms_after1);
}
*/











uint8_t JTAG_TAP_1s[] = {0x00, 0x01, 0x03, 0x07, 0x0F, 0x1F, 0x3F, 0x7F, 0xFF};
uint8_t JTAG_TAP_TMS_scrap = 0;
uint8_t JTAG_TAP_TMS_scraplen = 0;
static void JTAG_TAP_TMS_Bit(uint8_t* tms, uint8_t bit_len)
{
	while (bit_len >= 8)
	{
		JTAG_TAP_WriteTMSByte_ASYN(*tms);
		tms++;
		bit_len -= 8;
	}
	if (bit_len)
	{
		JTAG_TAP_TMS_scraplen = bit_len;
		JTAG_TAP_TMS_scrap = *tms;
	}
}

static void JTAG_TAP_ProcessDataRW(uint8_t *tdo, uint8_t *tdi,
	uint8_t tms_before, uint8_t tms_len_before, uint16_t bit_len,
	uint8_t len_of_1s_before, uint8_t len_of_1s_after, uint8_t idle)
{
	uint8_t tdi_tmp, tdo_tmp, tms_tmp, len_tmp;
	uint8_t offset, Rec_offset, last_mask;
	uint16_t iSend = 0, iReceive = 0, iTmp = 0, bit_len_remain, receiveFromByte;

	bit_len_remain = bit_len;
	receiveFromByte = len_of_1s_before + tms_len_before;
	Rec_offset = receiveFromByte & 0x07;
	receiveFromByte = (receiveFromByte >> 3) + 1;
	last_mask = (bit_len_remain & 7) ? (1 << (bit_len_remain & 7)) - 1 :
		((bit_len_remain > 0) ? 0xFF : 0x00);

	// process TMS scrap
	if (JTAG_TAP_TMS_scraplen)
	{
		if ((tms_len_before + JTAG_TAP_TMS_scraplen) > 8)
		{
			JTAG_TAP_TMS_scrap |= tms_before << JTAG_TAP_TMS_scraplen;
			JTAG_TAP_WriteTMSByte_ASYN(JTAG_TAP_TMS_scrap);

			tms_before >>= 8 - JTAG_TAP_TMS_scraplen;
			tms_len_before -= 8 - JTAG_TAP_TMS_scraplen;
			JTAG_TAP_TMS_scraplen = 0;
		}
		else
		{
			tms_before |= JTAG_TAP_TMS_scrap << tms_len_before;
			tms_len_before += JTAG_TAP_TMS_scraplen;
			JTAG_TAP_TMS_scraplen = 0;
		}
	}

	tms_tmp = tms_before;
	tdi_tmp = 0;
	len_tmp = tms_len_before;
	offset = tms_len_before;

	while(len_of_1s_before + bit_len + len_of_1s_after > 0)
	{
		if(len_of_1s_before > 0)
		{
			if(len_tmp > 0)
			{
				if(len_of_1s_before >= 8 - len_tmp)
				{
					tdi_tmp = JTAG_TAP_1s[8 - len_tmp] << offset;
				}
				else
				{
					tdi_tmp = JTAG_TAP_1s[len_of_1s_before] << offset;
				}
			}
			else
			{
				if(len_of_1s_before >= 8)
				{
					tdi_tmp = 0xFF;
				}
				else
				{
					tdi_tmp = JTAG_TAP_1s[len_of_1s_before];
				}
			}
			if((len_of_1s_before + len_tmp) <= 8)
			{
				len_tmp += len_of_1s_before;
				len_of_1s_before = 0;
				offset = len_tmp;
				if(offset == 8)
				{
					offset = 0;
				}
			}
			else
			{
				len_of_1s_before -= 8 - len_tmp;
				len_tmp = 8;
			}
		}
		if((bit_len > 0) && (len_tmp < 8))
		{
			if(tdi != NULL)
			{
				if(iSend > 0)
				{
					tdi_tmp = tdi[iSend - 1] >> (8 - offset);
					if(bit_len > offset)
					{
						tdi_tmp |= tdi[iSend] << offset;
					}
				}
				else
				{
					tdi_tmp |= tdi[iSend] << offset;
				}
				iSend++;
			}
			if((bit_len + len_tmp) <= 8)
			{
				len_tmp += bit_len;
				bit_len = 0;
				offset = len_tmp;
				if(offset == 8)
				{
					offset = 0;
				}
			}
			else
			{
				bit_len -= 8 - len_tmp;
				len_tmp = 8;
			}
		}
		if((len_of_1s_after > 0) && (len_tmp < 8))
		{
			if(len_tmp > 0)
			{
				if(len_of_1s_after >= 8 - len_tmp)
				{
					tdi_tmp |= JTAG_TAP_1s[8 - len_tmp] << offset;
				}
				else
				{
					tdi_tmp |= JTAG_TAP_1s[len_of_1s_after] << offset;
				}
			}
			else
			{
				if(len_of_1s_after >= 8)
				{
					tdi_tmp = 0xFF;
				}
				else
				{
					tdi_tmp = JTAG_TAP_1s[len_of_1s_after];
				}
			}
			if((len_of_1s_after + len_tmp) <= 8)
			{
				len_tmp += len_of_1s_after;
				len_of_1s_after = 0;
			}
			else
			{
				len_of_1s_after -= 8 - len_tmp;
				len_tmp = 8;
			}
		}

		if((len_of_1s_before + bit_len + len_of_1s_after) == 0)
		{
			tms_tmp |= 1 << (len_tmp - 1);
		}
		tdo_tmp = JTAG_TAP_Operate_Asyn(tdi_tmp, tms_tmp);
		len_tmp = 0;
		tms_tmp = 0;

		if(tdo != NULL)
		{
			if((bit_len_remain > 0) && (iReceive >= receiveFromByte))
			{
				iTmp = iReceive - receiveFromByte;
				if(iTmp > 0)
				{
					tdo[iTmp - 1] |= tdo_tmp << (8 - Rec_offset);
					if (bit_len_remain > Rec_offset)
					{
						bit_len_remain -= Rec_offset;
					}
					else
					{
						bit_len_remain = 0;
						tdo[iTmp - 1] &= last_mask;
					}
				}
				if((iTmp == 0) || ((iTmp > 0) && (bit_len_remain > Rec_offset)))
				{
					tdo[iTmp] = tdo_tmp >> Rec_offset;
					if (bit_len_remain > (8 - Rec_offset))
					{
						bit_len_remain -= 8 - Rec_offset;
					}
					else
					{
						bit_len_remain = 0;
						tdo[iTmp] &= last_mask;
					}
				}
			}
			iReceive++;
		}
	}

	len_tmp = idle & 0x07;
	if(len_tmp < 6)
	{
		tdo_tmp = JTAG_TAP_Operate_Asyn(0, JTAG_TAP_TMS_E12UPDATE >> idle);
	}
	else
	{
		tdo_tmp = JTAG_TAP_Operate_Asyn(0, JTAG_TAP_TMS_E12UPDATE << (8 - idle));
		JTAG_TAP_Operate_Asyn(0, JTAG_TAP_TMS_E12UPDATE >> idle);
	}
	if((tdo != NULL) && (bit_len_remain > 0))
	{
		iTmp = iReceive - receiveFromByte;
		if(iTmp > 0)
		{
			tdo[iTmp - 1] |= tdo_tmp << (8 - Rec_offset);
			if(bit_len_remain > Rec_offset)
			{
				tdo[iTmp] = tdo_tmp >> Rec_offset;
				tdo[iTmp] &= last_mask;
			}
			else
			{
				tdo[iTmp - 1] &= last_mask;
			}
		}
		else
		{
			tdo[iTmp] = tdo_tmp >> Rec_offset;
			tdo[iTmp] &= last_mask;
		}
	}
	len_tmp = idle >> 3;
	while(len_tmp--)
	{
		JTAG_TAP_Operate_Asyn(0, 0);
	}
}
/*
static uint32_t JTAG_TAP_Instr(uint32_t instr, uint8_t bit_len, uint8_t idle)
{
	uint32_t ret;

	JTAG_TAP_ProcessDataRW((uint8_t*)&ret,
						   (uint8_t*)&instr,
						   JTAG_TAP_TMS_UPDATERTI2SI,
						   JTAG_TAP_TMS_UPDATERTI2SI_LEN,
						   bit_len,
						   JTAG_TAP_BitsBefore,
						   JTAG_TAP_BitsAfter,
						   idle);

	return ret;
}
*/
static void JTAG_TAP_InstrPtr(uint8_t *instr, uint8_t *tdo, uint16_t bit_len,
								uint8_t idle)
{
	JTAG_TAP_ProcessDataRW(tdo,
						   instr,
						   JTAG_TAP_TMS_UPDATERTI2SI,
						   JTAG_TAP_TMS_UPDATERTI2SI_LEN,
						   bit_len,
						   JTAG_TAP_BitsBefore,
						   JTAG_TAP_BitsAfter,
						   idle);
}
/*
static void JTAG_TAP_InstrOutPtr(uint8_t *instr, uint16_t bit_len, uint8_t idle)
{
	JTAG_TAP_ProcessDataRW(NULL,
						   instr,
						   JTAG_TAP_TMS_UPDATERTI2SI,
						   JTAG_TAP_TMS_UPDATERTI2SI_LEN,
						   bit_len,
						   JTAG_TAP_BitsBefore,
						   JTAG_TAP_BitsAfter,
						   idle);
}

static void JTAG_TAP_DataOutPtr(uint8_t *tdi, uint16_t bit_len, uint8_t idle)
{
	JTAG_TAP_ProcessDataRW(NULL,
						   tdi,
						   JTAG_TAP_TMS_UPDATERTI2SD,
						   JTAG_TAP_TMS_UPDATERTI2SD_LEN,
						   bit_len,
						   JTAG_TAP_UnitsBefore,
						   JTAG_TAP_UnitsAfter,
						   idle);
}

static void JTAG_TAP_DataInPtr(uint8_t *tdo, uint16_t bit_len, uint8_t idle)
{
	JTAG_TAP_ProcessDataRW(tdo,
						   NULL,
						   JTAG_TAP_TMS_UPDATERTI2SD,
						   JTAG_TAP_TMS_UPDATERTI2SD_LEN,
						   bit_len,
						   JTAG_TAP_UnitsBefore,
						   JTAG_TAP_UnitsAfter,
						   idle);
}
*/
static void JTAG_TAP_DataPtr(uint8_t *tdi, uint8_t *tdo, uint16_t bit_len,
								uint8_t idle)
{
	JTAG_TAP_ProcessDataRW(tdo,
						   tdi,
						   JTAG_TAP_TMS_UPDATERTI2SD,
						   JTAG_TAP_TMS_UPDATERTI2SD_LEN,
						   bit_len,
						   JTAG_TAP_UnitsBefore,
						   JTAG_TAP_UnitsAfter,
						   idle);
}
/*
static uint32_t JTAG_TAP_Data(uint32_t tdi, uint16_t bit_len, uint8_t idle)
{
	uint32_t tdo;

	JTAG_TAP_ProcessDataRW((uint8_t*)&tdo,
						   (uint8_t*)&tdi,
						   JTAG_TAP_TMS_UPDATERTI2SD,
						   JTAG_TAP_TMS_UPDATERTI2SD_LEN,
						   bit_len,
						   JTAG_TAP_UnitsBefore,
						   JTAG_TAP_UnitsAfter,
						   idle);

	return tdo;
}

static void JTAG_TAP_DataOut(uint32_t tdi, uint16_t bit_len, uint8_t idle)
{
	JTAG_TAP_ProcessDataRW(NULL,
						   (uint8_t*)&tdi,
						   JTAG_TAP_TMS_UPDATERTI2SD,
						   JTAG_TAP_TMS_UPDATERTI2SD_LEN,
						   bit_len,
						   JTAG_TAP_UnitsBefore,
						   JTAG_TAP_UnitsAfter,
						   idle);
}

static uint32_t JTAG_TAP_DataIn(uint16_t bit_len, uint8_t idle)
{
	uint32_t tdo;

	JTAG_TAP_ProcessDataRW((uint8_t*)&tdo,
						   NULL,
						   JTAG_TAP_TMS_UPDATERTI2SD,
						   JTAG_TAP_TMS_UPDATERTI2SD_LEN,
						   bit_len,
						   JTAG_TAP_UnitsBefore,
						   JTAG_TAP_UnitsAfter,
						   idle);

	return tdo;
}
*/


static void JTAG_TAP_SetDaisyChainPos(uint32_t ub, uint32_t ua, uint32_t bb,
										uint32_t ba)
{
	JTAG_TAP_UnitsBefore	= ub;
	JTAG_TAP_UnitsAfter		= ua;
	JTAG_TAP_BitsBefore		= bb;
	JTAG_TAP_BitsAfter		= ba;
}

static vsf_err_t JTAG_TAP_Fini(void)
{
	JTAG_TAP_HS_DMA_FINI();

	JTAG_kHz = 0xFFFFFFFF;
	core_interfaces.spi.fini(JTAG_TAP_HS_SPI_M_PORT);
	core_interfaces.spi.fini(JTAG_TAP_HS_SPI_S_PORT);
	return VSFERR_NONE;
}

static vsf_err_t JTAG_TAP_Init(uint32_t kHz, uint8_t mode)
{
	struct spi_ability_t spis_ability, spim_ability;
	uint32_t min_khz;
	
	if (core_interfaces.spi.get_ability(JTAG_TAP_HS_SPI_M_PORT, &spim_ability) || 
		core_interfaces.spi.get_ability(JTAG_TAP_HS_SPI_S_PORT, &spis_ability) || 
		(spis_ability.max_freq_hz < spim_ability.min_freq_hz) || 
		(spis_ability.min_freq_hz > spim_ability.max_freq_hz))
	{
		return VSFERR_INVALID_PARAMETER;
	}
	min_khz = (spim_ability.min_freq_hz > spis_ability.min_freq_hz ? 
				spim_ability.min_freq_hz : spis_ability.min_freq_hz) / 1000;
	
	JTAG_TAP_Fini();
	if (kHz >= min_khz)
	{
		core_interfaces.spi.init(JTAG_TAP_HS_SPI_M_PORT);
		core_interfaces.spi.init(JTAG_TAP_HS_SPI_S_PORT);
	}
	else
	{
		JTAG_TAP_TCK_SET();
		JTAG_TAP_TCK_SETOUTPUT();
		JTAG_TAP_TDO_SETINPUT();
		JTAG_TAP_TDI_SETOUTPUT();
		JTAG_TAP_TMS_SETOUTPUT();
		JTAG_TAP_RTCK_SETINPUT();
	}
	
	JTAG_kHz = kHz;
	if (JTAG_kHz >= min_khz)
	{
		core_interfaces.spi.config(JTAG_TAP_HS_SPI_M_PORT, kHz, 
								SPI_MODE3 | SPI_LSB_FIRST | SPI_MASTER);
		core_interfaces.spi.config(JTAG_TAP_HS_SPI_S_PORT, 
								spis_ability.max_freq_hz / 1000, 
								SPI_MODE3 | SPI_LSB_FIRST | SPI_SLAVE);
	}
	else if (!JTAG_kHz)
	{
		// Wait RTCK
		JTAG_TAP_RTCK_Wait(1);
	}

	if(mode == JTAG_TAP_ASYN)
	{
		JTAG_TAP_HS_Out(JTAG_TAP_TMS_2RTI, 0);
		if (kHz >= min_khz)
		{
			JTAG_TAP_Operate_Asyn = JTAG_TAP_HS_Operate_Asyn;
		}
		else
		{
			JTAG_TAP_Operate_Asyn = JTAG_TAP_GPIO_Operate_Asyn;
		}
	}
	else if(mode == JTAG_TAP_RAW)
	{
		if (kHz >= min_khz)
		{
			// DMA Init
			JTAG_TAP_HS_DMA_INIT();

			JTAG_TAP_Operate_RAW = JTAG_TAP_HS_Operate_RAW_DMA;
		}
		else
		{
			JTAG_TAP_Operate_RAW = JTAG_TAP_GPIO_Operate_RAW;
		}
	}
	else
	{
		JTAG_TAP_Operate_RAW = JTAG_TAP_GPIO_Operate_RAW;
		JTAG_TAP_Operate_Asyn = JTAG_TAP_GPIO_Operate_Asyn;
	}
	return VSFERR_NONE;
}

vsf_err_t jtaghl_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t jtaghl_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		return JTAG_TAP_Fini();
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t jtaghl_config_speed(uint8_t index, uint32_t kHz)
{
	switch (index)
	{
	case 0:
		return JTAG_TAP_Init(kHz, JTAG_TAP_ASYN);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t jtaghl_config_daisychain(uint8_t index, struct jtag_pos_t *jtag_pos)
{
	switch (index)
	{
	case 0:
		JTAG_TAP_SetDaisyChainPos(jtag_pos->ub, jtag_pos->ua, jtag_pos->bb, jtag_pos->ba);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t jtaghl_config(uint8_t index, uint32_t kHz, struct jtag_pos_t *jtag_pos)
{
	switch (index)
	{
	case 0:
		if (jtaghl_config_speed(index, kHz) || 
			jtaghl_config_daisychain(index, jtag_pos))
		{
			return VSFERR_FAIL;
		}
		else
		{
			return VSFERR_NONE;
		}
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t jtaghl_tms(uint8_t index, uint8_t* tms, uint16_t bitlen)
{
	switch (index)
	{
	case 0:
		JTAG_TAP_TMS_Bit(tms, bitlen);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t jtaghl_runtest(uint8_t index, uint32_t cycles)
{
	uint8_t tms[256 / 8];
	uint16_t cur_cycles;
	
	switch (index)
	{
	case 0:
		memset(tms, 0, sizeof(tms));
		while (cycles > 0)
		{
			if (cycles > 256)
			{
				cur_cycles = 256;
			}
			else
			{
				cur_cycles = (uint8_t)cycles;
			}
			if (jtaghl_tms(index, tms, cur_cycles))
			{
				return VSFERR_FAIL;
			}
			cycles -= cur_cycles;
		}
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

static jtag_callback_t jtaghl_receive_callback = NULL;
static jtag_callback_t jtaghl_send_callback = NULL;
static uint32_t jtaghl_ir_backup;
// IMP: if callback function is used, sizeof(jtaghl_temp_buff) should be large
// enough to hold the temperory ir/dr data
static uint8_t jtaghl_temp_buff[128];
vsf_err_t jtaghl_register_callback(uint8_t index, jtag_callback_t send_callback,
									jtag_callback_t receive_callback)
{
	switch (index)
	{
	case 0:
		jtaghl_receive_callback = receive_callback;
		jtaghl_send_callback = send_callback;
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t jtaghl_ir(uint8_t index, uint8_t *ir, uint16_t bitlen, uint8_t idle,
					uint8_t want_ret)
{
	uint16_t bytelen = (bitlen + 7) >> 3;
	uint8_t *pir;
	uint16_t processed_len;
	
	switch (index)
	{
	case 0:
		jtaghl_ir_backup = 0;
		if (bytelen > 4)
		{
			memcpy(&jtaghl_ir_backup, ir, 4);
		}
		else
		{
			memcpy(&jtaghl_ir_backup, ir, bytelen);
		}
		
		processed_len = 0;
		if (jtaghl_send_callback != NULL)
		{
			if (jtaghl_send_callback(index, JTAG_SCANTYPE_IR, 
							jtaghl_ir_backup, jtaghl_temp_buff, ir, bytelen, 
							&processed_len))
			{
				return VSFERR_FAIL;
			}
		}
		if (!processed_len)
		{
			pir = ir;
		}
		else
		{
			pir = jtaghl_temp_buff;
		}
		
		JTAG_TAP_InstrPtr(pir, pir, bitlen, idle);
		
		if (want_ret)
		{
			processed_len = 0;
			if (jtaghl_receive_callback != NULL)
			{
				if (jtaghl_receive_callback(index, JTAG_SCANTYPE_IR, 
							jtaghl_ir_backup, ir, pir, bytelen, &processed_len))
				{
					return VSFERR_FAIL;
				}
			}
			if (!processed_len)
			{
				memcpy(ir, pir, bytelen);
			}
		}
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t jtaghl_dr(uint8_t index, uint8_t *dr, uint16_t bitlen, uint8_t idle,
					uint8_t want_ret)
{
	uint16_t bytelen = (bitlen + 7) >> 3;
	uint8_t *pdr;
	uint16_t processed_len;
	
	switch (index)
	{
	case 0:
		processed_len = 0;
		if (jtaghl_send_callback != NULL)
		{
			if (jtaghl_send_callback(index, JTAG_SCANTYPE_DR, 
						jtaghl_ir_backup, jtaghl_temp_buff, dr, bytelen, 
						&processed_len))
			{
				return VSFERR_FAIL;
			}
		}
		if (!processed_len)
		{
			pdr = dr;
		}
		else
		{
			pdr = jtaghl_temp_buff;
		}
		
		JTAG_TAP_DataPtr(pdr, pdr, bitlen, idle);
		
		if (want_ret)
		{
			processed_len = 0;
			if (jtaghl_receive_callback != NULL)
			{
				if (jtaghl_receive_callback(index, JTAG_SCANTYPE_DR, 
							jtaghl_ir_backup, dr, pdr, bytelen, &processed_len))
				{
					return VSFERR_FAIL;
				}
			}
			if (!processed_len)
			{
				memcpy(dr, pdr, bytelen);
			}
		}
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t jtagll_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t jtagll_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		return JTAG_TAP_Fini();
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t jtagll_config(uint8_t index, uint32_t kHz)
{
	switch (index)
	{
	case 0:
		return JTAG_TAP_Init(kHz, JTAG_TAP_ASYN);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t jtagll_tms(uint8_t index, uint8_t *tms, uint8_t bytelen)
{
	uint16_t i;
	
	switch (index)
	{
	case 0:
		for(i = 0; i < bytelen; i++)
		{
			JTAG_TAP_WriteTMSByte_ASYN(tms[i]);
		}
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t jtagll_tms_clocks(uint8_t index, uint32_t bytelen, uint8_t tms)
{
	switch (index)
	{
	case 0:
		while(bytelen--)
		{
			JTAG_TAP_WriteTMSByte_ASYN(tms);
		}
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t jtagll_scan(uint8_t index, uint8_t* data, uint16_t bitlen, 
					uint8_t tms_before_valid, uint8_t tms_before, 
					uint8_t tms_after0, uint8_t tms_after1)
{
	uint16_t bytelen = (bitlen + 7) >> 3;
	
	switch (index)
	{
	case 0:
		if (NULL == data)
		{
			return VSFERR_INVALID_PTR;
		}
		
		if (tms_before_valid)
		{
			bytelen |= 0x8000;
		}
		JTAG_TAP_RW(data, data, tms_before, tms_after0, tms_after1, bytelen);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t jtagraw_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t jtagraw_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		return JTAG_TAP_Fini();
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t jtagraw_config(uint8_t index, uint32_t kHz)
{
	switch (index)
	{
	case 0:
		return JTAG_TAP_Init(kHz, JTAG_TAP_RAW);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t jtagraw_execute(uint8_t index, uint8_t* tdi, uint8_t* tms,
							uint8_t *tdo, uint32_t bitlen)
{
	switch (index)
	{
	case 0:
		if ((NULL == tdi) || (NULL == tms) || (NULL == tdo))
		{
			return VSFERR_INVALID_PTR;
		}
		
		JTAG_TAP_Operate_RAW(bitlen, tdi, tms, tdo);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

#endif
