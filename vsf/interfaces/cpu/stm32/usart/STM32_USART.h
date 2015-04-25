/***************************************************************************
 *   Copyright (C) 2009 - 2010 by Simon Qian <SimonQian@SimonQian.com>     *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

vsf_err_t stm32_usart_init(uint8_t index);
vsf_err_t stm32_usart_fini(uint8_t index);
vsf_err_t stm32_usart_config(uint8_t index, uint32_t baudrate, 
								uint8_t datalength, uint8_t mode);
vsf_err_t stm32_usart_config_callback(uint8_t index, void *p, 
						void (*ontx)(void *), void (*onrx)(void *, uint16_t));
vsf_err_t stm32_usart_tx(uint8_t index, uint16_t data);
vsf_err_t stm32_usart_tx_isready(uint8_t index);
uint16_t stm32_usart_rx(uint8_t index);
vsf_err_t stm32_usart_rx_isready(uint8_t index);
