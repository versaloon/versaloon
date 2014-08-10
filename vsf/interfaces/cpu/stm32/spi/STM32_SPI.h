/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       GPIO.h                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    GPIO interface header file                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

vsf_err_t stm32_spi_init(uint8_t index);
vsf_err_t stm32_spi_fini(uint8_t index);
vsf_err_t stm32_spi_get_ability(uint8_t index, struct spi_ability_t *ability);
vsf_err_t stm32_spi_enable(uint8_t index);
vsf_err_t stm32_spi_disable(uint8_t index);
vsf_err_t stm32_spi_config(uint8_t index, uint32_t kHz, uint8_t mode);
vsf_err_t stm32_spi_select(uint8_t index, uint8_t cs);
vsf_err_t stm32_spi_deselect(uint8_t index, uint8_t cs);
vsf_err_t stm32_spi_io_tx(uint8_t index, uint8_t out);
vsf_err_t stm32_spi_io_tx_isready(uint8_t index);
vsf_err_t stm32_spi_io_rx_isready(uint8_t index);
uint8_t stm32_spi_io_rx(uint8_t index);
vsf_err_t stm32_spi_io(uint8_t index, uint8_t *out, uint8_t *in, uint32_t len);
vsf_err_t stm32_spi_io_dma_start(uint8_t index, uint8_t *out, uint8_t *in, 
									uint32_t len);
vsf_err_t stm32_spi_io_dma_isready(uint8_t index);
vsf_err_t stm32_spi_io_dma_end(uint8_t index);
