/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       IIC.c                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    IIC interface implementation file                         *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_IIC_EN

#include "app_interfaces.h"
#include "IIC_MOD_Common.h"
#include "EMIIC_MOD.h"
#include "IIC.h"

DEFINE_EMIIC_MOD(USBTOXXX, IIC_SCL_CLR, IIC_SCL_SET, IIC_SCL_GET, IIC_SDA_CLR, IIC_SDA_SET, IIC_SDA_GET, app_interfaces.delay.delayus, uint16_t)

uint16_t IIC_Delay;

vsf_err_t iic_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		IIC_SCL_INIT();
		IIC_SDA_INIT();
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t iic_fini(uint8_t index)
{
	vsf_err_t err;
	
	switch (index)
	{
	case 0:
		if (IIC_MOD_ACK == EMIIC_USBTOXXX_DeInit())
		{
			err = VSFERR_NONE;
		}
		else
		{
			err = VSFERR_FAIL;
		}
		IIC_SCL_FINI();
		IIC_SDA_FINI();
		return err;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t iic_config(uint8_t index, uint16_t kHz, uint16_t byte_interval, 
				 uint16_t max_dly)
{
	uint16_t clock_cycle = 1000 / kHz;
	
	switch (index)
	{
	case 0:
		IIC_PULL_INIT();
		if ((IIC_MOD_ACK != EMIIC_USBTOXXX_SetParameter(clock_cycle, max_dly, 1, byte_interval)) ||
			(IIC_MOD_ACK != EMIIC_USBTOXXX_Init()))
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

vsf_err_t iic_read(uint8_t index, uint16_t chip_addr, uint8_t *data, 
			   uint16_t data_len, uint8_t stop, bool nacklast)
{
	IIC_STOP_t iic_stop;
	uint16_t actual_len;
	
	switch (index)
	{
	case 0:
		if (stop)
		{
			iic_stop = IIC_FORCESTOP;
		}
		else
		{
			iic_stop = IIC_NOSTOP;
		}
		
		if (IIC_MOD_ACK == EMIIC_USBTOXXX_Receive(chip_addr, data, data_len, iic_stop, nacklast, &actual_len))
		{
			return VSFERR_NONE;
		}
		else
		{
			return VSFERR_FAIL;
		}
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t iic_write(uint8_t index, uint16_t chip_addr, uint8_t *data, 
					uint16_t data_len, uint8_t stop)
{
	IIC_STOP_t iic_stop;
	uint16_t actual_len;
	
	switch (index)
	{
	case 0:
		if (stop)
		{
			iic_stop = IIC_FORCESTOP;
		}
		else
		{
			iic_stop = IIC_NOSTOP;
		}
		
		if (IIC_MOD_ACK == EMIIC_USBTOXXX_Send(chip_addr, data, data_len, iic_stop, &actual_len))
		{
			return VSFERR_NONE;
		}
		else
		{
			return VSFERR_FAIL;
		}
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

#endif
