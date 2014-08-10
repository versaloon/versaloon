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
#ifndef __PROG_INTERFACE_H_INCLUDED__
#define __PROG_INTERFACE_H_INCLUDED__

// GPIO
#define usbtoxxx_GPIO_DIR_MSK			0x01
#define usbtoxxx_GPIO_OUT				usbtoxxx_GPIO_DIR_MSK
#define usbtoxxx_GPIO_IN				0
#define usbtoxxx_GPIO_PULLEN_MSK		0x02
#define usbtoxxx_GPIO_PULLEN			usbtoxxx_GPIO_PULLEN_MSK
#define usbtoxxx_GPIO_PULLDIS			0x00
#define usbtoxxx_GPIO_OUT_MSK			0x04
#define usbtoxxx_GPIO_OUT1				usbtoxxx_GPIO_OUT_MSK
#define usbtoxxx_GPIO_OUT0				0x00

#define usbtoxxx_GPIO_INFLOAT			0
#define usbtoxxx_GPIO_INPU				0x06
#define usbtoxxx_GPIO_INPD				0x02
#define usbtoxxx_GPIO_OUTPP				0x01
#define usbtoxxx_GPIO_OUTOD				0x05

// ADC
#define usbtoxxx_ADC_ALIGNLEFT			0x08
#define usbtoxxx_ADC_ALIGNRIGHT			0x00

// SPI
#define usbtoxxx_SPI_MASTER				0x04
#define usbtoxxx_SPI_SLAVE				0x00
#define usbtoxxx_SPI_MODE0				0
#define usbtoxxx_SPI_MODE1				1
#define usbtoxxx_SPI_MODE2				2
#define usbtoxxx_SPI_MODE3				3
#define usbtoxxx_SPI_MSB_FIRST			0x00
#define usbtoxxx_SPI_LSB_FIRST			0x80

#endif /* __PROG_INTERFACE_H_INCLUDED__ */

