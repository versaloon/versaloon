/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       DUSI.c                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    DUSI interface implementation file                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_DUSI_EN

#include "app_interfaces.h"
#include "../SPI/SPI.h"
#include "DUSI.h"

vsf_err_t dusi_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		core_interfaces.spi.init(JTAG_TAP_HS_SPI_M_PORT);
		core_interfaces.spi.init(JTAG_TAP_HS_SPI_S_PORT);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t dusi_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		core_interfaces.spi.fini(JTAG_TAP_HS_SPI_M_PORT);
		core_interfaces.spi.fini(JTAG_TAP_HS_SPI_S_PORT);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t dusi_config(uint8_t index, uint32_t kHz, uint8_t mode)
{
	struct spi_ability_t spis_ability, spim_ability;
	
	switch (index)
	{
	case 0:
		if (core_interfaces.spi.get_ability(JTAG_TAP_HS_SPI_M_PORT, &spim_ability) || 
			core_interfaces.spi.get_ability(JTAG_TAP_HS_SPI_S_PORT, &spis_ability) || 
			(spis_ability.max_freq_hz < spim_ability.min_freq_hz) || 
			(spis_ability.min_freq_hz > spim_ability.max_freq_hz))
		{
			return VSFERR_INVALID_PARAMETER;
		}
		
		core_interfaces.spi.config(JTAG_TAP_HS_SPI_M_PORT, kHz, mode | SPI_MASTER);
		core_interfaces.spi.config(JTAG_TAP_HS_SPI_S_PORT, 
							spis_ability.max_freq_hz / 1000, mode | SPI_SLAVE);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t dusi_io(uint8_t index, uint8_t *mo, uint8_t *mi, uint8_t *so, uint8_t *si, 
			   uint32_t bitlen)
{
	uint32_t i;
	uint8_t tmp;
	
	switch (index)
	{
	case 0:
		// currently support byte mode ONLY
		bitlen /= 8;
		for(i = 0; i < bitlen; i++)
		{
			if (so != NULL)
			{
				DUSI_SlaveOutBytePtr(so);
				so++;
			}
			if (mo != NULL)
			{
				tmp = *mo;
				mo++;
			}
			else
			{
				tmp = 0;
			}
			DUSI_MasterOutByte(tmp);
			
			JTAG_TAP_HS_WaitReady();
			
			if (si != NULL)
			{
				*si = DUSI_SlaveInByte();
				si++;
			}
			if (mi != NULL)
			{
				*mi = DUSI_MasterInByte();
				mi++;
			}
		}
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

#endif
