/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       PowerExt.c                                                *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    Power output interface implementation file                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2009-06-20:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if POWER_OUT_EN

#include "app_interfaces.h"
#include "PowerExt.h"

static uint8_t PWREXT_EnableCount = 0;
static uint16_t PWREXT_Vtarget = 0;
static uint16_t PWREXT_FSMState = 0;
uint8_t PWREXT_PowerState = 0;

void PWREXT_Acquire(void)
{
	if(PWREXT_Vtarget < TVCC_SAMPLE_MIN_POWER)
	{
		if(!PWREXT_EnableCount)
		{
			PWREXT_ENABLE();
		}
	}
	PWREXT_EnableCount++;
}

void PWREXT_Release(void)
{
	if(PWREXT_EnableCount)
	{
		PWREXT_EnableCount--;
		if(!PWREXT_EnableCount)
		{
			PWREXT_DISABLE();
		}
	}
}

void PWREXT_ForceRelease(void)
{
	while(PWREXT_EnableCount)
	{
		PWREXT_Release();
	}
}

uint8_t PWREXT_GetState(void)
{
	return PWREXT_EnableCount;
}

vsf_err_t target_voltage_set(uint8_t index, uint16_t voltage)
{
	uint8_t i;
	
	switch (index)
	{
	case 0:
		if (!PWREXT_GetState())
		{
			PWREXT_PowerState = 0;
		}
		
		if(voltage == 3300)
		{
			// only support 3.3V
			if (!PWREXT_PowerState)
			{
				PWREXT_PowerState = 1;
				PWREXT_Acquire();
			}
		}
		else if(voltage == 0)
		{
			if (PWREXT_PowerState)
			{
				PWREXT_PowerState = 0;
				PWREXT_ForceRelease();
			}
		}
		else
		{
			return VSFERR_FAIL;
		}
		
		app_interfaces.delay.delayms(10);
		// sample 10 times to ensure 
		// a valid voltage after change before next operation
		while (PWREXT_FSMState < 2)
		{
			target_voltage_poll(index);
		}
		for (i = 0; i < 10; i++)
		{
			PWREXT_FSMState = 0;
			while (PWREXT_FSMState < 2)
			{
				target_voltage_poll(index);
			}
		}
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t target_voltage_get(uint8_t index, uint16_t *voltage)
{
	switch (index)
	{
	case 0:
		if (voltage != NULL)
		{
			*voltage = PWREXT_Vtarget;
		}
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t target_voltage_poll(uint8_t index)
{
	switch (index)
	{
	case 0:
		switch (PWREXT_FSMState)
		{
		case 0:
			core_interfaces.adc.start(TVCC_ADC_PORT, TVCC_ADC_CHANNEL);
			PWREXT_FSMState++;
			break;
		case 1:
			if (!core_interfaces.adc.isready(TVCC_ADC_PORT, TVCC_ADC_CHANNEL))
			{
				PWREXT_Vtarget = 
					core_interfaces.adc.get(TVCC_ADC_PORT, TVCC_ADC_CHANNEL) * 
						TVCC_SAMPLE_DIV * 3300 / 4096;
				
				if(PWREXT_Vtarget > TVCC_SAMPLE_MIN_POWER)
				{
					LED_POWER_ON();
				}
				else
				{
					LED_POWER_OFF();
				}
				if((PWREXT_Vtarget < TVCC_SAMPLE_MIN_POWER))
				{
					PWREXT_ForceRelease();
				}
				PWREXT_FSMState++;
			}
			break;
		default:
			if (++PWREXT_FSMState > 0x3FFF)
			{
				PWREXT_FSMState = 0;
			}
			break;
		}
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

#endif /* USB_TO_POWER_EN || POWER_OUT_EN*/
