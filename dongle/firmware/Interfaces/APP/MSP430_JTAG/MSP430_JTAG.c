/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       MSP430_JTAG.c                                             *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    MSP430_JTAG interface implementation file                 *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_MSP430_JTAG_EN

#include "app_interfaces.h"
#include "MSP430_JTAG.h"

#define MSP430_JTAG_DELAY()				app_interfaces.delay.delayus(0)
#define MSP430_JTAG_IR_LEN				8

static void MSP430_JTAG_Init(uint8_t has_test)
{
	JTAG_TAP_TMS_CLR();
	JTAG_TAP_TMS_SETOUTPUT();
	JTAG_TAP_TDI_CLR();
	JTAG_TAP_TDI_SETOUTPUT();
	JTAG_TAP_TDO_SETINPUT();
	JTAG_TAP_TCK_CLR();
	JTAG_TAP_TCK_SETOUTPUT();
	app_interfaces.delay.delayms(50);

	JTAG_TAP_TMS_SET();
	JTAG_TAP_TDI_SET();
	JTAG_TAP_TCK_SET();

	if(has_test)
	{
		// enter 4-wire JTAG mode
		// TEST :        _______         _________
		//       _______|       |_______|         
		// /RST :___________                ______
		//                  |______________|______
		{
			MSP430_JTAG_TEST_CLR();
			MSP430_JTAG_TEST_SETOUTPUT();
			JTAG_TAP_SRST_SET();
			JTAG_TAP_SRST_SETOUTPUT();
			app_interfaces.delay.delayms(1);		// wait for SRST stable

			// now TEST low, SRST high
			MSP430_JTAG_TEST_SET();	// BSL disabled
			app_interfaces.delay.delayms(1);
			JTAG_TAP_SRST_CLR();
			app_interfaces.delay.delayms(1);
			MSP430_JTAG_TEST_CLR();
			app_interfaces.delay.delayms(1);
			MSP430_JTAG_TEST_SET();
			app_interfaces.delay.delayms(1);
		}

		JTAG_TAP_SRST_SETINPUT();	// release SRST
		app_interfaces.delay.delayms(1);
	}
	else
	{
		JTAG_TAP_SRST_SET();
		JTAG_TAP_SRST_SETINPUT();
	}
}

static void MSP430_JTAG_Fini(void)
{
#if 1
	MSP430_JTAG_TEST_CLR();
	app_interfaces.delay.delayus(50);
	JTAG_TAP_TMS_CLR();
	JTAG_TAP_TDI_CLR();
	JTAG_TAP_TCK_CLR();
#else
	JTAG_TAP_TMS_SETINPUT();
	JTAG_TAP_TDI_SETINPUT();
	JTAG_TAP_TDO_SETINPUT();
	JTAG_TAP_TCK_SETINPUT();
	MSP430_JTAG_TEST_CLR();
	MSP430_JTAG_TEST_SETINPUT();
	JTAG_TAP_SRST_SETINPUT();
#endif
}

static void MSP430_JTAG_TCLK(uint8_t tclk)
{
	if(tclk)
	{
		JTAG_TAP_TDI_SET();
	}
	else
	{
		JTAG_TAP_TDI_CLR();
	}
}

static void MSP430_JTAG_TCLK_STROKE(uint16_t cnt)
{
	while(cnt--)
	{
		MSP430_JTAG_TCLK(1);
		MSP430_JTAG_DELAY();
		MSP430_JTAG_TCLK(0);
	}
}

static uint32_t MSP430_JTAG_Shift(uint32_t data, uint8_t len)
{
	uint32_t tclk = JTAG_TAP_TDI_GET();
	uint32_t tdo = 0, mask = 1 << (len - 1);
	uint8_t i;

	for(i = len; i > 0; i--)
	{
		if((data & mask) > 0)
		{
			JTAG_TAP_TDI_SET();
		}
		else
		{
			JTAG_TAP_TDI_CLR();
		}
		if(1 == i)
		{
			JTAG_TAP_TMS_SET();
		}
		JTAG_TAP_TCK_CLR();
		data <<= 1;
		tdo <<= 1;
		MSP430_JTAG_DELAY();
		JTAG_TAP_TCK_SET();
		MSP430_JTAG_DELAY();

		if(JTAG_TAP_TDO_GET() > 0)
		{
			tdo |= 1;
		}
	}

	if(tclk > 0)
	{
		JTAG_TAP_TDI_SET();
	}
	else
	{
		JTAG_TAP_TDI_CLR();
	}

	// ED
	JTAG_TAP_TCK_CLR();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TCK_SET();
	MSP430_JTAG_DELAY();
	// UD
	JTAG_TAP_TMS_CLR();
	JTAG_TAP_TCK_CLR();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TCK_SET();
	MSP430_JTAG_DELAY();
	// RTI

	return tdo;
}

static uint32_t MSP430_JTAG_DR(uint32_t dr, uint8_t len)
{
	// RTI
	JTAG_TAP_TMS_SET();
	JTAG_TAP_TCK_CLR();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TCK_SET();
	MSP430_JTAG_DELAY();
	// SDS
	JTAG_TAP_TMS_CLR();
	JTAG_TAP_TCK_CLR();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TCK_SET();
	MSP430_JTAG_DELAY();
	// CD
	JTAG_TAP_TCK_CLR();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TCK_SET();
	MSP430_JTAG_DELAY();

	return MSP430_JTAG_Shift(dr, len);
}

static uint8_t MSP430_JTAG_IR(uint8_t ir, uint8_t len)
{
	// RTI
	JTAG_TAP_TMS_SET();
	JTAG_TAP_TCK_CLR();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TCK_SET();
	MSP430_JTAG_DELAY();
	// SDS
	JTAG_TAP_TCK_CLR();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TCK_SET();
	MSP430_JTAG_DELAY();
	// SIS
	JTAG_TAP_TMS_CLR();
	JTAG_TAP_TCK_CLR();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TCK_SET();
	MSP430_JTAG_DELAY();
	// CD
	JTAG_TAP_TCK_CLR();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TCK_SET();
	MSP430_JTAG_DELAY();

	return (uint8_t)MSP430_JTAG_Shift(ir, len);
}

static void MSP430_JTAG_Reset(void)
{
	uint8_t i;

	JTAG_TAP_TDI_SET();
	JTAG_TAP_TMS_SET();

	// shift out 6 tms '1'
	for(i = 7; i > 0; i--)
	{
		JTAG_TAP_TCK_SET();
		MSP430_JTAG_DELAY();
		JTAG_TAP_TCK_CLR();
		MSP430_JTAG_DELAY();
	}

	JTAG_TAP_TMS_CLR();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TCK_SET();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TMS_SET();
	MSP430_JTAG_DELAY();

	// check fuse
	JTAG_TAP_TMS_CLR();
	app_interfaces.delay.delayms(1);
	JTAG_TAP_TMS_SET();
	app_interfaces.delay.delayus(5);
	JTAG_TAP_TMS_CLR();
	app_interfaces.delay.delayms(1);
	JTAG_TAP_TMS_SET();

	for(i = 7; i > 0; i--)
	{
		JTAG_TAP_TCK_SET();
		MSP430_JTAG_DELAY();
		JTAG_TAP_TCK_CLR();
		MSP430_JTAG_DELAY();
	}
	JTAG_TAP_TMS_CLR();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TCK_SET();
	MSP430_JTAG_DELAY();
}

static uint8_t MSP430_JTAG_Poll(uint32_t data, uint32_t mask, uint32_t value,
								uint8_t len, uint16_t poll_cnt)
{
	uint8_t toggle_tclk = (poll_cnt & 0x8000) > 0;

	poll_cnt &= 0x7FFF;
	while(poll_cnt-- > 0)
	{
		if((MSP430_JTAG_DR(data, len) & mask) == value)
		{
			return 0;
		}
		if(toggle_tclk)
		{
			JTAG_TAP_TDI_CLR();
			JTAG_TAP_TDI_SET();
		}
	}
	return 1;
}

vsf_err_t msp430jtag_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t msp430jtag_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		MSP430_JTAG_Fini();
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}
vsf_err_t msp430jtag_config(uint8_t index, uint8_t has_test)
{
	switch (index)
	{
	case 0:
		MSP430_JTAG_Init(has_test);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t msp430jtag_ir(uint8_t index, uint8_t *ir, uint8_t want_ret)
{
	uint8_t ir_tmp;
	
	switch (index)
	{
	case 0:
		if (NULL == ir)
		{
			return VSFERR_INVALID_PTR;
		}
		
		ir_tmp = MSP430_JTAG_IR(*ir, MSP430_JTAG_IR_LEN);
		if (want_ret)
		{
			*ir = ir_tmp;
		}
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t msp430jtag_dr(uint8_t index, uint32_t *dr, uint8_t bitlen,
						uint8_t want_ret)
{
	uint32_t dr_tmp;
	
	switch (index)
	{
	case 0:
		if (NULL == dr)
		{
			return VSFERR_INVALID_PTR;
		}
		
		dr_tmp = MSP430_JTAG_DR(*dr, bitlen);
		if (want_ret)
		{
			*dr = dr_tmp;
		}
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t msp430jtag_tclk(uint8_t index, uint8_t value)
{
	switch (index)
	{
	case 0:
		MSP430_JTAG_TCLK(value);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t msp430jtag_tclk_strobe(uint8_t index, uint16_t cnt)
{
	switch (index)
	{
	case 0:
		MSP430_JTAG_TCLK_STROKE(cnt);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t msp430jtag_reset(uint8_t index)
{
	switch (index)
	{
	case 0:
		MSP430_JTAG_Reset();
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t msp430jtag_poll(uint8_t index, uint32_t dr, uint32_t mask,
			uint32_t value, uint8_t len, uint16_t poll_cnt, uint8_t toggle_tclk)
{
	switch (index)
	{
	case 0:
		if(MSP430_JTAG_Poll(dr, mask, value, len,
							poll_cnt | (toggle_tclk ? 0x8000 : 0)))
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

#endif
