/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       MicroWire.c                                               *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    MicroWire interface implementation file                   *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_MICROWIRE_EN

#include "app_interfaces.h"
#include "MicroWire.h"

static uint16_t MicroWire_DelayUS = 0;
static uint8_t MicroWire_SelPolarity = 0;

static void MicroWire_Select(void)
{
	if (MicroWire_SelPolarity)
	{
		MICROWIRE_SEL_SET();
	}
	else
	{
		MICROWIRE_SEL_CLR();
	}
}

static void MicroWire_Release(void)
{
	if (MicroWire_SelPolarity)
	{
		MICROWIRE_SEL_CLR();
	}
	else
	{
		MICROWIRE_SEL_SET();
	}
}

static void MicroWire_OutBit(uint32_t out)
{
	if (out)
	{
		MICROWIRE_SO_SET();
	}
	else
	{
		MICROWIRE_SO_CLR();
	}
	app_interfaces.delay.delayus(MicroWire_DelayUS);
	MICROWIRE_SK_SET();
	app_interfaces.delay.delayus(MicroWire_DelayUS * 2);
	MICROWIRE_SK_CLR();
	app_interfaces.delay.delayus(MicroWire_DelayUS);
}

static uint8_t MicroWire_InBit(void)
{
	uint8_t si;
	
	app_interfaces.delay.delayus(MicroWire_DelayUS);
	MICROWIRE_SK_SET();
	app_interfaces.delay.delayus(MicroWire_DelayUS * 2);
	MICROWIRE_SK_CLR();
	si = MICROWIRE_SI_GET() > 0;
	app_interfaces.delay.delayus(MicroWire_DelayUS);
	return si;
}

static void MicroWire_Init(uint8_t sel_polarity)
{
	MicroWire_SelPolarity = sel_polarity;
	MicroWire_Release();
	MICROWIRE_SEL_SETOUTPUT();
	
	MICROWIRE_SK_CLR();
	MICROWIRE_SK_SETOUTPUT();
	MICROWIRE_SO_SETOUTPUT();
	MICROWIRE_SI_SETINPUT();
}

static void MicroWire_Fini(void)
{
	MICROWIRE_SEL_SETINPUT();
	MICROWIRE_SK_SETINPUT();
	MICROWIRE_SO_SETINPUT();
	MICROWIRE_SI_SETINPUT();
}

static void MicroWire_Config(uint16_t kHz)
{
	if (!kHz)
	{
		kHz = 1;
	}
	
	MicroWire_DelayUS = (1000 / 4) / kHz;
}

static vsf_err_t MicroWire_Poll(uint16_t interval_us, uint16_t retry_cnt)
{
	MicroWire_Select();
	
	while (!MICROWIRE_SI_GET() && --retry_cnt)
	{
		app_interfaces.delay.delayus(interval_us);
	}
	
	MicroWire_Release();
	
	if (retry_cnt)
	{
		return VSFERR_NONE;
	}
	else
	{
		return VSFERR_FAIL;
	}
}

static void MicroWire_Transact(uint32_t opcode, uint8_t opcode_bitlen, 
					uint32_t addr, uint8_t addr_bitlen, uint32_t data,
					uint8_t data_bitlen, uint8_t *ret, uint8_t ret_bitlen)
{
	uint16_t i;
	uint32_t reply;
	
	MicroWire_Select();
	
	MicroWire_OutBit(1);
	
	for (i = 0; i < opcode_bitlen; i++)
	{
		MicroWire_OutBit(opcode & (1UL << (opcode_bitlen - 1 - i)));
	}
	for (i = 0; i < addr_bitlen; i++)
	{
		MicroWire_OutBit(addr & (1UL << (addr_bitlen - 1 - i)));
	}
	for (i = 0; i < data_bitlen; i++)
	{
		MicroWire_OutBit(data & (1UL << (data_bitlen - 1 - i)));
	}
	
	reply = 0;
	for (i = 0; i < ret_bitlen; i++)
	{
		if (MicroWire_InBit())
		{
			reply |= 1UL << (ret_bitlen - 1 - i);
		}
	}
	memcpy(ret, &reply, (ret_bitlen + 7) / 8);
	
	MicroWire_Release();
}

vsf_err_t microwire_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t microwire_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		MicroWire_Fini();
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t microwire_config(uint8_t index, uint16_t kHz, uint8_t sel_polarity)
{
	switch (index)
	{
	case 0:
		MicroWire_Init(sel_polarity);
		MicroWire_Config(kHz);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t microwire_transport(uint8_t index, uint32_t opcode,
		uint8_t opcode_bitlen, uint32_t addr, uint8_t addr_bitlen, 
		uint32_t data, uint8_t data_bitlen, uint8_t *ret, uint8_t ret_bitlen)
{
	switch (index)
	{
	case 0:
		MicroWire_Transact(opcode, opcode_bitlen, 
						   addr, addr_bitlen, 
						   data, data_bitlen, 
						   ret, ret_bitlen);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t microwire_poll(uint8_t index, uint16_t interval_us,
							uint16_t retry_cnt)
{
	switch (index)
	{
	case 0:
		return MicroWire_Poll(interval_us, retry_cnt);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

#endif
