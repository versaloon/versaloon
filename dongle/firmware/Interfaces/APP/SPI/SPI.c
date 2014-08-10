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

static uint8_t SPI_Emu = 0, SPI_Emu_Mode = 0, SPI_Emu_MSB_First = 0;
static uint32_t SPI_Dly;

static void SPI_Delay(uint8_t dly)
{
	app_interfaces.delay.delayus(dly);
}

static vsf_err_t SPI_Config_Emu(uint32_t freq_hz, uint8_t mode)
{
	SPI_Dly = 250000 / freq_hz;
	SPI_Emu_MSB_First = !(mode & SPI_LSB_FIRST);
	SPI_Emu_Mode = mode & 0x03;
	
	if (SPI_Emu_Mode & 0x02)
	{
		SPI_SCK_SET();
	}
	else
	{
		SPI_SCK_CLR();
	}
	
	SPI_SCK_SETOUTPUT();
	SPI_MOSI_SETOUTPUT();
	SPI_MISO_SETINPUT();
	return VSFERR_NONE;
}

#define SPI_DATA_LEN			8
#define SPI_MSB					(1 << (SPI_DATA_LEN - 1))
#define SPI_LSB					(1)

static uint8_t SPI_RW_Emu_GetData(uint8_t *data)
{
	uint8_t ret;
	
	if (SPI_Emu_MSB_First)
	{
		ret = *data & SPI_MSB;
		*data <<= 1;
	}
	else
	{
		ret = *data & SPI_LSB;
		*data >>= 1;
	}
	return ret;
}

static void SPI_RW_Emu_OutBit(uint8_t *data)
{
	if (SPI_RW_Emu_GetData(data))
	{
		SPI_MOSI_SET();
	}
	else
	{
		SPI_MOSI_CLR();
	}
}

static void SPI_RW_Emu_SetData(uint8_t *data, uint8_t bit)
{
	if (SPI_Emu_MSB_First)
	{
		*data <<= 1;
		if (bit)
		{
			*data |= SPI_LSB;
		}
		else
		{
			*data &= ~SPI_LSB;
		}
	}
	else
	{
		*data >>= 1;
		if (bit)
		{
			*data |= SPI_MSB;
		}
		else
		{
			*data &= ~SPI_MSB;
		}
	}
}

static void SPI_RW_Emu_InBit(uint8_t *data)
{
	if (SPI_MISO_GET())
	{
		SPI_RW_Emu_SetData(data, 1);
	}
	else
	{
		SPI_RW_Emu_SetData(data, 0);
	}
}

static uint8_t SPI_RW_Emu(uint8_t data)
{
	uint8_t i, ret = 0;
	
	for(i = SPI_DATA_LEN; i; i--)
	{
		if (!(SPI_Emu_Mode & 1))
		{
			// CPHA = 0, sample on first edge
			// output data first
			SPI_RW_Emu_OutBit(&data);
		}
		
		SPI_Delay(SPI_Dly);
		if (SPI_Emu_Mode & 2)
		{
			// CPOL = 1, SCK high on idle
			// fisrt SCK edge is falling edge
			SPI_SCK_CLR();
		}
		else
		{
			// CPOL = 0, SCK low on idle
			// first SCK edge is rising edge
			SPI_SCK_SET();
		}
		if (!(SPI_Emu_Mode & 1))
		{
			// CPHA = 0, sample on first edge
			// input data after first SCK edge
			SPI_RW_Emu_InBit(&ret);
		}
		SPI_Delay(SPI_Dly);
		
		if (SPI_Emu_Mode & 1)
		{
			// CPHA = 1, sample on seconde edge
			// output data after first SCK edge
			SPI_RW_Emu_OutBit(&data);
		}
		
		SPI_Delay(SPI_Dly);
		if (SPI_Emu_Mode & 2)
		{
			// CPOL = 1, SCK high on idle
			// second SCK edge is rising edge
			SPI_SCK_SET();
		}
		else
		{
			// CPOL = 0, SCK low on idle
			// second SCK edge is falling edge
			SPI_SCK_CLR();
		}
		if (SPI_Emu_Mode & 1)
		{
			// CPHA = 1, sample on seconde edge
			// input data after second SCK edge
			SPI_RW_Emu_InBit(&ret);
		}
		SPI_Delay(SPI_Dly);
	}
	return ret;
}

vsf_err_t spi_init(uint8_t index)
{
	switch (index)
	{
	case 0:
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
		return core_interfaces.spi.fini(SPI_PORT);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t spi_config(uint8_t index, uint32_t kHz, uint8_t mode)
{
	struct spi_ability_t spi_ability;
	uint32_t min_khz;
	
	if (core_interfaces.spi.get_ability(SPI_PORT, &spi_ability))
	{
		return VSFERR_FAIL;
	}
	min_khz = spi_ability.min_freq_hz / 1000;
	
	switch (index)
	{
	case 0:
		if(kHz < min_khz)
		{
			SPI_Emu = 1;
			core_interfaces.spi.fini(SPI_PORT);
			return SPI_Config_Emu(kHz * 1000, mode);
		}
		else
		{
			SPI_Emu = 0;
			core_interfaces.spi.init(SPI_PORT);
			return core_interfaces.spi.config(SPI_PORT, kHz, mode | SPI_MASTER);
		}
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t spi_select(uint8_t index, uint8_t cs)
{
	switch (index)
	{
	case 0:
		return core_interfaces.spi.select(SPI_PORT, cs);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t spi_deselect(uint8_t index, uint8_t cs)
{
	switch (index)
	{
	case 0:
		return core_interfaces.spi.deselect(SPI_PORT, cs);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t spi_io(uint8_t index, uint8_t *out, uint8_t *in, uint32_t len)
{
	uint32_t i;

	switch (index)
	{
	case 0:
		if(SPI_Emu)
		{
			if (NULL == out)
			{
				if (NULL == in)
				{
					for(i = 0; i < len; i++)
					{
						SPI_RW_Emu(0xFF);
					}
				}
				else
				{
					for(i = 0; i < len; i++)
					{
						in[i] = SPI_RW_Emu(0xFF);
					}
				}
			}
			else
			{
				if (NULL == in)
				{
					for(i = 0; i < len; i++)
					{
						SPI_RW_Emu(out[i]);
					}
				}
				else
				{
					for(i = 0; i < len; i++)
					{
						in[i] = SPI_RW_Emu(out[i]);
					}
				}
			}
		}
		else
		{
			core_interfaces.spi.io(SPI_PORT, out, in, len);
		}
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

#endif
