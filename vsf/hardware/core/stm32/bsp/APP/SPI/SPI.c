/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       SPI.c                                                     *
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
#if INTERFACE_SPI_EN

#include "app_interfaces.h"
#include "SPI.h"

vsf_err_t spi_init(uint8_t index)
{
	switch (index)
	{
	case 0:
	case 1:
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t spi_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		return core_interfaces.spi.fini(SPI0_PORT);
	case 1:
		return core_interfaces.spi.fini(SPI1_PORT);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t spi_config(uint8_t index, uint32_t kHz, uint8_t mode)
{
	switch (index)
	{
	case 0:
		core_interfaces.spi.init(SPI0_PORT);
		return core_interfaces.spi.config(SPI0_PORT, kHz, mode | SPI_MASTER);
	case 1:
		core_interfaces.spi.init(SPI1_PORT);
		return core_interfaces.spi.config(SPI1_PORT, kHz, mode | SPI_MASTER);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t spi_select(uint8_t index, uint8_t cs)
{
	switch (index)
	{
	case 0:
		return core_interfaces.spi.select(SPI0_PORT, cs);
	case 1:
		return core_interfaces.spi.select(SPI1_PORT, cs);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t spi_deselect(uint8_t index, uint8_t cs)
{
	switch (index)
	{
	case 0:
		return core_interfaces.spi.deselect(SPI0_PORT, cs);
	case 1:
		return core_interfaces.spi.deselect(SPI1_PORT, cs);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t spi_io(uint8_t index, uint8_t *out, uint8_t *in, uint32_t len)
{
	switch (index)
	{
	case 0:
		core_interfaces.spi.io(SPI0_PORT, out, in, len);
		return VSFERR_NONE;
	case 1:
		core_interfaces.spi.io(SPI1_PORT, out, in, len);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

#endif
