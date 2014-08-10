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
#if INTERFACE_EBI_EN

#include "app_interfaces.h"
#include "EBI.h"

vsf_err_t ebi_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		return core_interfaces.ebi.init(0);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t ebi_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		return core_interfaces.ebi.fini(0);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t ebi_config(uint8_t index, uint8_t target_index, void *param)
{
	switch (index)
	{
	case 0:
		return core_interfaces.ebi.config(0, target_index, param);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t ebi_read(uint8_t index, uint8_t target_index, uint32_t address, 
					uint8_t data_size, uint8_t *buff, uint32_t count)
{
	switch (index)
	{
	case 0:
		return core_interfaces.ebi.read(0, target_index, address, data_size,
										buff, count);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t ebi_write(uint8_t index, uint8_t target_index, uint32_t address, 
					uint8_t data_size, uint8_t *buff, uint32_t count)
{
	switch (index)
	{
	case 0:
		return core_interfaces.ebi.write(0, target_index, address, data_size,
											buff, count);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t ebi_isready(uint8_t index, uint8_t target_index)
{
	switch (index)
	{
	case 0:
		return core_interfaces.ebi.isready(0, target_index);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

#endif
