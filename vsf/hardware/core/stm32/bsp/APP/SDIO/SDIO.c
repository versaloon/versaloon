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
 *      2012-01-24:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_SDIO_EN

#include "app_interfaces.h"
#include "SDIO.h"

vsf_err_t sdio_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		return core_interfaces.sdio.init(index);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t sdio_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		return core_interfaces.sdio.fini(index);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t sdio_config(uint8_t index, uint16_t kHz, uint8_t buswidth)
{
	switch (index)
	{
	case 0:
		return core_interfaces.sdio.config(index, kHz, buswidth);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t sdio_start(uint8_t index)
{
	switch (index)
	{
	case 0:
		return core_interfaces.sdio.start(index);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t sdio_stop(uint8_t index)
{
	switch (index)
	{
	case 0:
		return core_interfaces.sdio.stop(index);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t sdio_send_cmd(uint8_t index, uint8_t cmd, uint32_t arg, uint8_t resp)
{
	switch (index)
	{
	case 0:
		return core_interfaces.sdio.send_cmd(index, cmd, arg, resp);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t sdio_send_cmd_isready(uint8_t index, uint8_t resp)
{
	switch (index)
	{
	case 0:
		return core_interfaces.sdio.send_cmd_isready(index, resp);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t sdio_get_resp(uint8_t index, uint8_t *cresp, uint32_t *resp,
						uint8_t resp_num)
{
	switch (index)
	{
	case 0:
		return core_interfaces.sdio.get_resp(index, cresp, resp, resp_num);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t sdio_data_tx(uint8_t index, uint32_t timeout, uint32_t size,
						uint32_t block_size)
{
	switch (index)
	{
	case 0:
		return core_interfaces.sdio.data_tx(index, timeout, size, block_size);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t sdio_data_tx_isready(uint8_t index, uint32_t size, uint8_t *buffer)
{
	switch (index)
	{
	case 0:
		return core_interfaces.sdio.data_tx_isready(index, size, buffer);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t sdio_data_rx(uint8_t index, uint32_t timeout, uint32_t size,
						uint32_t block_size)
{
	switch (index)
	{
	case 0:
		return core_interfaces.sdio.data_rx(index, timeout, size, block_size);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t sdio_data_rx_isready(uint8_t index, uint32_t size, uint8_t *buffer)
{
	switch (index)
	{
	case 0:
		return core_interfaces.sdio.data_rx_isready(index, size, buffer);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

#endif		// INTERFACE_SDIO_EN
