/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       SWD.c                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    SWD interface implementation file                         *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_SWD_EN

#include "app_interfaces.h"
#include "SWD.h"

#define SWD_SUCCESS				0x00
#define SWD_FAULT				0x80
#define SWD_RETRY_OUT			0x40
#define SWD_ACK_ERROR			0x20
#define SWD_PARITY_ERROR		0x10

#define SWD_ACK_OK				0x01
#define SWD_ACK_WAIT			0x02
#define SWD_ACK_FAULT			0x04

#define SWD_TRANS_RnW			(1 << 2)

uint8_t SWD_Trn = 1;
uint16_t SWD_Retry = 0;
uint16_t SWD_Delay = 0;

#define SWD_Delay()		app_interfaces.delay.delayus(SWD_Delay)

uint8_t (*SWD_SeqIn)(uint8_t *seq, uint16_t num_of_bits);
uint8_t (*SWD_SeqOut)(uint8_t *seq, uint16_t num_of_bits);

uint8_t SWD_SeqIn_NoDelay(uint8_t *seq, uint16_t num_of_bits)
{
	uint16_t i;
	uint8_t parity = 0;

	for (i = 0; i < num_of_bits; i++)
	{
		SWD_SWCLK_SET();
		SWD_SWCLK_CLR();
		if (SWD_SWDIO_GET())
		{
			seq[i / 8] |= 1 << (i % 8);
			parity++;
		}
		else
		{
			seq[i / 8] &= ~(1 << (i % 8));
		}
	}
	return parity & 1;
}

uint8_t SWD_SeqOut_NoDelay(uint8_t *seq, uint16_t num_of_bits)
{
	uint16_t i;
	uint8_t parity = 0;

	for (i = 0; i < num_of_bits; i++)
	{
		if (seq[i / 8] & (1 << (i % 8)))
		{
			SWD_SWDIO_SET();
			parity++;
		}
		else
		{
			SWD_SWDIO_CLR();
		}
		SWD_SWCLK_SET();
		SWD_SWCLK_CLR();
	}

	return parity & 1;
}

uint8_t SWD_SeqIn_Delay(uint8_t *seq, uint16_t num_of_bits)
{
	uint16_t i;
	uint8_t parity = 0;

	for (i = 0; i < num_of_bits; i++)
	{
		SWD_SWCLK_SET();
		SWD_Delay();
		SWD_SWCLK_CLR();
		if (SWD_SWDIO_GET())
		{
			seq[i / 8] |= 1 << (i % 8);
			parity++;
		}
		else
		{
			seq[i / 8] &= ~(1 << (i % 8));
		}
		SWD_Delay();
	}
	return parity & 1;
}

uint8_t SWD_SeqOut_Delay(uint8_t *seq, uint16_t num_of_bits)
{
	uint16_t i;
	uint8_t parity = 0;

	for (i = 0; i < num_of_bits; i++)
	{
		if (seq[i / 8] & (1 << (i % 8)))
		{
			SWD_SWDIO_SET();
			parity++;
		}
		else
		{
			SWD_SWDIO_CLR();
		}
		SWD_Delay();
		SWD_SWCLK_SET();
		SWD_Delay();
		SWD_SWCLK_CLR();
	}

	return parity & 1;
}

void SWD_StopClock(void)
{
	uint32_t null = 0;

	// shift in at least 8 bits
	SWD_SeqOut((uint8_t*)&null, 8);
}

uint8_t SWD_Transaction(uint8_t request, uint32_t *buff)
{
	uint32_t reply, dummy, data;
	uint8_t read = request & SWD_TRANS_RnW, parity;
	uint16_t retry = 0;

SWD_RETRY:
	// set swdio output to output request
	SWD_SWDIO_SET();
	SWD_SWDIO_SETOUTPUT();

	// send out request
	SWD_SeqOut(&request, 8);

	// set swdio input to receive reply
	SWD_SWDIO_SETINPUT();

	SET_LE_U32(&data, *buff);

	if (read)
	{
		// receive 3-bit reply
		SWD_SeqIn((uint8_t*)&reply, 3);
		// receive data and parity
		parity = SWD_SeqIn((uint8_t*)&data, 32);
		parity += SWD_SeqIn((uint8_t*)&dummy, 1);
		// trn
		SWD_SeqIn((uint8_t*)&dummy, SWD_Trn);

		// set swdio output to output stop clock
		SWD_SWDIO_SET();
		SWD_SWDIO_SETOUTPUT();
	}
	else
	{
		// receive trn and 3-bit reply and then trn
		SWD_SeqIn((uint8_t*)&reply, SWD_Trn + 3);

		// set swdio output to output data
		SWD_SWDIO_SET();
		SWD_SWDIO_SETOUTPUT();

		// send data and parity
		parity = SWD_SeqOut((uint8_t*)&data, 32);
		parity += SWD_SeqOut(&parity, 1);
	}
	SWD_StopClock();
	// set swdio input after clock is stopped
	SWD_SWDIO_SETINPUT();

	if (read)
	{
		*buff = GET_LE_U32(&data);
	}

	reply &= 0x07; 
	switch (reply)
	{
	case SWD_ACK_OK:
		if (parity & 1)
		{
			return SWD_PARITY_ERROR | reply;
		}
		else
		{
			return SWD_SUCCESS | reply;
		}
	case SWD_ACK_WAIT:
		retry++;
		if (retry < SWD_Retry)
		{
			goto SWD_RETRY;
		}
		else
		{
			return SWD_RETRY_OUT | reply;
		}
	case SWD_ACK_FAULT:
		return SWD_FAULT | reply;
	default:
		return SWD_ACK_ERROR | reply;
	}
}

void SWD_Init(void)
{
	SWD_SWDIO_SETINPUT();
	SWD_SWCLK_SET();
	SWD_SWCLK_SETOUTPUT();
}

void SWD_Fini(void)
{
	SWD_SWDIO_SETINPUT();
	SWD_SWCLK_SETINPUT();
}

void SWD_SetDelay(uint16_t dly)
{
	if (!dly)
	{
		SWD_SeqIn = SWD_SeqIn_NoDelay;
		SWD_SeqOut = SWD_SeqOut_NoDelay;
	}
	else
	{
		SWD_SeqIn = SWD_SeqIn_Delay;
		SWD_SeqOut = SWD_SeqOut_Delay;
		SWD_Delay = dly - 1;
	}
}

void SWD_SetTurnaround(uint8_t cycles)
{
	if (cycles <= 14)
	{
		SWD_Trn = cycles;
	}
}

void SWD_SetRetryCount(uint16_t retry)
{
	SWD_Retry = retry;
}

vsf_err_t swd_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t swd_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		SWD_Fini();
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t swd_config(uint8_t index, uint8_t trn, uint16_t retry, uint16_t dly)
{
	switch (index)
	{
	case 0:
		SWD_SetTurnaround(trn);
		SWD_SetRetryCount(retry);
		SWD_SetDelay(dly);
		SWD_Init();
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t swd_seqout(uint8_t index, uint8_t *data, uint16_t bitlen)
{
	switch (index)
	{
	case 0:
		if (data == NULL)
		{
			return VSFERR_INVALID_PTR;
		}
		
		SWD_SWDIO_SET();
		SWD_SWDIO_SETOUTPUT();
		SWD_SeqOut(data, bitlen);
		SWD_SWDIO_SETINPUT();
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t swd_seqin(uint8_t index, uint8_t *data, uint16_t bitlen)
{
	switch (index)
	{
	case 0:
		if (data == NULL)
		{
			return VSFERR_INVALID_PTR;
		}
		
		SWD_SeqIn(data, bitlen);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t swd_transact(uint8_t index, uint8_t request, uint32_t *data, 
						uint8_t *ack)
{
	uint8_t parity;
	uint8_t ack_tmp;
	
	switch (index)
	{
	case 0:
		if (data == NULL)
		{
			return VSFERR_INVALID_PTR;
		}
		
		parity = (request >> 1) & 1;
		parity += (request >> 2) & 1;
		parity += (request >> 3) & 1;
		parity += (request >> 4) & 1;
		parity &= 1;
		request = (request | 0x81 | (parity << 5)) & ~0x40;
		ack_tmp = SWD_Transaction(request, data);
		if (ack != NULL)
		{
			*ack = ack_tmp;
		}
		return (ack_tmp == (SWD_SUCCESS | SWD_ACK_OK)) ?
					VSFERR_NONE : VSFERR_FAIL;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

#endif
