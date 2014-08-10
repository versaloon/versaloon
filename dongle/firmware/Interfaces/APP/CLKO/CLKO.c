/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       ADC.c                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    SPI interface implementation file                         *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_CLKO_EN

#include "app_interfaces.h"
#include "CLKO.h"

vsf_err_t clko_init(uint8_t index)
{
	switch (index)
	{
	case 0:		
		return core_interfaces.clko.init(0);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t clko_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		return core_interfaces.clko.fini(0);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t clko_config(uint8_t index, uint32_t kHz)
{
	switch (index)
	{
	case 0:
		return core_interfaces.clko.config(0, kHz);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t clko_enable(uint8_t index)
{
	switch (index)
	{
	case 0:
		return core_interfaces.clko.enable(0);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t clko_disable(uint8_t index)
{
	switch (index)
	{
	case 0:
		return core_interfaces.clko.disable(0);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

#endif
