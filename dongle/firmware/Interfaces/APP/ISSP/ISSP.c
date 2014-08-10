/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       ISSP.c                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    ISSP interface implementation file                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_ISSP_EN

#include "app_interfaces.h"
#include "ISSP.h"

#if POWER_OUT_EN
#	include "../PowerExt/PowerExt.h"
#endif

#define ISSP_VECTOR_BITNUM		22

// read or write
#define ISSP_VECTOR_R			1
#define ISSP_VECTOR_W			0

// target
#define ISSP_VECTOR_SRAM		0
#define ISSP_VECTOR_CPUBANK		1

// Prog Mode
#define ISSP_PM_RESET			(1 << 0)
#define ISSP_PM_POWER_ON		(0 << 0)

// wap result
#define ISSP_WAP_OK				0x00
#define ISSP_WAP_TIMEOUT		0x01

#define ISSP_Delay()			app_interfaces.delay.delayus(0)

static void ISSP_Out_Bit(uint8_t bit)
{
	if(bit)
	{
		ISSP_SDATA_SET();
	}
	else
	{
		ISSP_SDATA_CLR();
	}

	ISSP_SCLK_SET();

	ISSP_Delay();

	ISSP_SCLK_CLR();

	ISSP_Delay();
}

static uint8_t ISSP_In_Bit(void)
{
	uint8_t ret;

	ISSP_SCLK_SET();

	ret = (ISSP_SDATA_GET() > 0);

	ISSP_Delay();

	ISSP_SCLK_CLR();

	ISSP_Delay();

	return ret;
}

uint8_t ISSP_Vector(uint8_t bank, uint8_t addr, uint8_t data, uint8_t r, 
					uint8_t append_bit)
{
	uint8_t i;

	ISSP_SDATA_SETOUTPUT();

	// Output header
	ISSP_Out_Bit(1);
	ISSP_Out_Bit(bank);
	ISSP_Out_Bit(r);

	// Output address
	for(i = 0; i < 8; i++)
	{
		ISSP_Out_Bit(addr & 0x80);
		addr <<= 1;
	}

	// Read or Write data
	if(r)
	{
		ISSP_SDATA_SETINPUT();

		data = 0;
		for(i = 0; i < 10; i++)
		{
			data <<= 1;
			data |= ISSP_In_Bit();
		}

		ISSP_SDATA_SETOUTPUT();
	}
	else
	{
		for(i = 0; i < 8; i++)
		{
			ISSP_Out_Bit(data & 0x80);
			data <<= 1;
		}
	}

	//append bit
	i = r ? 1 : 3;
	while(i--)
	{
		ISSP_Out_Bit(append_bit);
	}

	ISSP_SDATA_SETINPUT();

	return data;
}

void ISSP_Vector_0s(void)
{
	uint8_t i;

	ISSP_SDATA_SETOUTPUT();

	for(i = 0; i < ISSP_VECTOR_BITNUM; i++)
	{
		ISSP_Out_Bit(0);
	}

	ISSP_SDATA_SETINPUT();
}

vsf_err_t issp_enter_program_mode(uint8_t index, uint8_t mode)
{
	uint16_t to = 1000;
	uint16_t voltage;

	switch (index)
	{
	case 0:
		if(app_interfaces.target_voltage.get(0, &voltage) || 
			((mode == ISSP_PM_POWER_ON) && (voltage > TVCC_SAMPLE_MIN_POWER)))
		{
			return VSFERR_FAIL;
		}

		ISSP_SDATA_SETOUTPUT();

		if(mode == ISSP_PM_RESET)
		{
			PWREXT_Acquire();
			app_interfaces.delay.delayms(1);

			ISSP_XRES_SET();
			ISSP_XRES_SETOUTPUT();
			app_interfaces.delay.delayms(1);
			ISSP_XRES_CLR();
		}
		else if(mode == ISSP_PM_POWER_ON)
		{
			ISSP_SDATA_SETINPUT();

			ISSP_PowerOff();
			app_interfaces.delay.delayms(1);
			ISSP_PowerOn();
			app_interfaces.delay.delayms(5);

			while(ISSP_SDATA_GET() && --to)
			{
				app_interfaces.delay.delayus(10);
			}

			ISSP_SDATA_SETOUTPUT();
		}

		ISSP_SCLK_CLR();
		ISSP_SCLK_SETOUTPUT();

		ISSP_Vector(1, 0x50, 0x00, 0, 0);
		ISSP_Vector_0s();
		ISSP_Vector_0s();
		ISSP_Vector_0s();
		ISSP_Vector_0s();
		ISSP_Vector_0s();

		ISSP_SDATA_SETINPUT();
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t issp_leave_program_mode(uint8_t index, uint8_t mode)
{
	switch (index)
	{
	case 0:
		if(mode == ISSP_PM_RESET)
		{
			ISSP_XRES_SET();
			app_interfaces.delay.delayms(1);
			ISSP_XRES_SETINPUT();
			ISSP_SCLK_SETINPUT();

			PWREXT_Release();
		}
		else if(mode == ISSP_PM_POWER_ON)
		{
			ISSP_PowerOff();
			ISSP_SCLK_SETINPUT();
		}
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t issp_wait_and_poll(uint8_t index)
{
	uint8_t i;
	uint16_t dly;

	switch (index)
	{
	case 0:
		ISSP_SDATA_SETINPUT();
		ISSP_In_Bit();
		dly = 100;
		while(!ISSP_SDATA_GET())
		{
			app_interfaces.delay.delayus(10);
			if(--dly == 0)
			{
				return VSFERR_FAIL;
			}
		}

		dly = 10000;
		while(ISSP_SDATA_GET())
		{
			app_interfaces.delay.delayus(10);
			if(--dly == 0)
			{
				return VSFERR_FAIL;
			}
		}

		ISSP_SDATA_CLR();
		ISSP_SDATA_SETOUTPUT();

		for(i = 0; i < 40; i++)
		{
			ISSP_Out_Bit(0);
		}
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t issp_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		return issp_fini(index);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t issp_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		ISSP_XRES_CLR();
		ISSP_XRES_SETINPUT();
		ISSP_SDATA_SETINPUT();
		ISSP_SCLK_SETINPUT();
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t issp_vector(uint8_t index, uint8_t operate, uint8_t addr, 
					 uint8_t data, uint8_t *buf)
{
	switch (index)
	{
	case 0:
		if(operate & ISSP_OPERATE_0s)
		{
			ISSP_Vector_0s();
		}
		else
		{
			if(operate & ISSP_OPERATE_READ)
			{
				uint8_t buf_tmp;
				// Read
				buf_tmp = ISSP_Vector(operate & ISSP_OPERATE_BANK, addr, data, 
										1,operate & ISSP_OPERATE_APPENDBIT);
				if (buf != NULL)
				{
					*buf = buf_tmp;
				}
			}
			else
			{
				// Write
				ISSP_Vector(operate & ISSP_OPERATE_BANK, addr, data, 0,
								operate & ISSP_OPERATE_APPENDBIT);
			}
		}
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

#endif
