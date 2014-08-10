/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       PWM.c                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    PWM header file                                           *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2011-03-06:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_PWM_EN

#include "app_interfaces.h"
#include "PWM.h"

static uint16_t pwm0_cycle = 0;
static uint16_t pwm0_cur_rate = 0;

vsf_err_t pwm_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		return VSFERR_NONE;
	case 1:
		SYNCSWPWM_IN_TIMER_INIT();
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t pwm_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		SYNCSWPWM_PORT_ODPP_FINI();
		SYNCSWPWM_OUT_TIMER_FINI(SYNCSWPWM_OUT_TIMER_DMA_COMPARE);
		return VSFERR_NONE;
	case 1:
		SYNCSWPWM_IN_TIMER_FINI();
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t pwm_config_freq(uint8_t index, uint16_t kHz)
{
	switch (index)
	{
	case 0:
		pwm0_cycle = SYNCSWPWM_OUT_TIMER_MHZ * 1000 / kHz;
		if (pwm0_cycle >= 0xFFFF)
		{
			pwm0_cycle = 0xFFFE;
		}
		SYNCSWPWM_OUT_TIMER_SetCycle(pwm0_cycle);
		if (pwm0_cur_rate)
		{
			SYNCSWPWM_OUT_TIMER_SetRate(pwm0_cur_rate * pwm0_cycle / 0xFFFF);
		}
		return VSFERR_NONE;
	case 1:
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t pwm_config_mode(uint8_t index, uint8_t mode)
{
	switch (index)
	{
	case 0:
		SYNCSWPWM_OUT_TIMER_INIT(SYNCSWPWM_OUT_TIMER_DMA_COMPARE, mode & PWM_OUTPOLARITY);
		if (mode & PWM_OUTPP)
		{
			SYNCSWPWM_PORT_PP_INIT();
		}
		else
		{
			SYNCSWPWM_PORT_OD_INIT();
		}
		return VSFERR_NONE;
	case 1:
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t pwm_out(uint8_t index, uint16_t count, uint16_t *rate)
{
	uint16_t i;
	
	switch (index)
	{
	case 0:
		if (NULL == rate)
		{
			return VSFERR_INVALID_PARAMETER;
		}
		
		pwm0_cur_rate = rate[count - 1];
		for (i = 0; i < count; i++)
		{
			rate[i] = rate[i] * (pwm0_cycle + 1) / 0xFFFF;
		}
		SYNCSWPWM_OUT_TIMER_DMA_INIT(SYNCSWPWM_OUT_TIMER_DMA_COMPARE, count, rate);
		SYNCSWPWM_OUT_TIMER_DMA_COMPARE_WAIT();
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t pwm_in(uint8_t index, uint16_t count, uint16_t *rate)
{
	uint32_t dly;
	
	switch (index)
	{
	case 1:
		if (NULL == rate)
		{
			return VSFERR_INVALID_PARAMETER;
		}
		
		SYNCSWPWM_IN_TIMER_DMA_INIT(count, rate, rate + count);
		dly = 0xFFFFFFFF;
		SYNCSWPWM_IN_TIMER_RISE_DMA_WAIT(dly);
		return dly ? VSFERR_NONE : VSFERR_FAIL;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

#endif
