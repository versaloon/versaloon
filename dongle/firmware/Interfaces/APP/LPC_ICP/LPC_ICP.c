/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       LPC_ICP.c                                                 *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    LPC_ICP interface implementation file                     *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_LPC_ICP_EN

#include "app_interfaces.h"
#include "LPC_ICP.h"
#if POWER_OUT_EN
#	include "../PowerExt/PowerExt.h"
#endif

#define LPCICP_POWERON_DELAY			10
#define LPCICP_RST_TOGGLE_DELAY			1
#define LPCICP_POST_ENTERPROGMODE_DELAY	100
#define LPCICP_SHIFT_DELAY_SHORT		0
#define LPCICP_SHIFT_DELAY_LONG			0

static void LPCICP_Init(void)
{
	LPCICP_RST_CLR();
	LPCICP_RST_SETOUTPUT();

	LPCICP_PCL_SET();
	LPCICP_PCL_SETOUTPUT();

	LPCICP_PDA_SET();
	LPCICP_PDA_SETOUTPUT();
}

static void LPCICP_Fini(void)
{
	LPCICP_RST_SETINPUT();
	LPCICP_PDA_SETINPUT();
	LPCICP_PCL_SETINPUT();
}

static void LPCICP_LeavrProgMode(void)
{
	PWREXT_Release();
}

static void LPCICP_EnterProgMode(void)
{
	uint8_t toggle_count;

	PWREXT_Acquire();
	app_interfaces.delay.delayms(LPCICP_POWERON_DELAY);

	for (toggle_count = 0; toggle_count < 7; toggle_count++)
	{
		LPCICP_RST_SET();
		app_interfaces.delay.delayus(LPCICP_RST_TOGGLE_DELAY);		// Trh
		LPCICP_RST_CLR();
		app_interfaces.delay.delayus(LPCICP_RST_TOGGLE_DELAY);		// Trl
	}
	LPCICP_RST_SET();

	app_interfaces.delay.delayms(LPCICP_POST_ENTERPROGMODE_DELAY);	// Trp
	LPCICP_PDA_SETINPUT();
}

static void LPCICP_In(uint8_t *buff, uint16_t len)
{
	uint32_t i;

	for (i = 0; i < len * 8; i++)
	{
		LPCICP_PCL_CLR();
		app_interfaces.delay.delayus(LPCICP_SHIFT_DELAY_SHORT);
		LPCICP_PCL_SET();

		if (LPCICP_PDA_GET())
		{
			buff[i / 8] |= 1 << (i % 8);
		}
		else
		{
			buff[i / 8] &= ~(1 << (i % 8));
		}
		app_interfaces.delay.delayus(LPCICP_SHIFT_DELAY_LONG);
	}
}

static void LPCICP_Out(uint8_t *buff, uint16_t len)
{
	uint32_t i;

	LPCICP_PDA_SETOUTPUT();

	for (i = 0; i < len * 8; i++)
	{
		LPCICP_PCL_CLR();

		if (buff[i / 8] & (1 << (i % 8)))
		{
			LPCICP_PDA_SET();
		}
		else
		{
			LPCICP_PDA_CLR();
		}

		app_interfaces.delay.delayus(LPCICP_SHIFT_DELAY_LONG);
		LPCICP_PCL_SET();
		app_interfaces.delay.delayus(LPCICP_SHIFT_DELAY_SHORT);
	}

	LPCICP_PDA_SETINPUT();
}

static uint8_t LPCICP_Poll(uint8_t out, uint8_t setbit, uint8_t clearbit, uint16_t pollcnt)
{
	uint8_t tmp;

	while (pollcnt-- > 0)
	{
		LPCICP_Out(&out, 1);
		LPCICP_In(&tmp, 1);

		if (setbit && ((tmp & setbit) == setbit))
		{
			return LPCICP_POLL_ON_SET;
		}
		if (clearbit && ((tmp & clearbit) == 0))
		{
			return LPCICP_POLL_ON_CLEAR;
		}
	}

	return LPCICP_POLL_TIME_OUT;
}

vsf_err_t lpcicp_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		LPCICP_Init();
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t lpcicp_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		LPCICP_LeavrProgMode();
		LPCICP_Fini();
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t lpcicp_enter_program_mode(uint8_t index)
{
	uint16_t voltage;
	
	switch (index)
	{
	case 0:
		if (app_interfaces.target_voltage.get(0, &voltage) || 
			(voltage > TVCC_SAMPLE_MIN_POWER))
		{
			// No power should be applied on the target
			return VSFERR_FAIL;
		}
		else
		{
			LPCICP_EnterProgMode();
			return VSFERR_NONE;
		}
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t lpcicp_in(uint8_t index, uint8_t *buff, uint16_t len)
{
	switch (index)
	{
	case 0:
		LPCICP_In(buff, len);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t lpcicp_out(uint8_t index, uint8_t *buff, uint16_t len)
{
	switch (index)
	{
	case 0:
		LPCICP_Out(buff, len);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t lpcicp_poll_ready(uint8_t index, uint8_t data, uint8_t *ret, 
					uint8_t setmask, uint8_t clearmask, uint16_t pollcnt)
{
	uint8_t ret_tmp;
	
	switch (index)
	{
	case 0:
		ret_tmp = LPCICP_Poll(data, setmask, clearmask, pollcnt);
		if (ret != NULL)
		{
			*ret = ret_tmp;
		}
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

#endif	// INTERFACE_LPC_ICP_EN
