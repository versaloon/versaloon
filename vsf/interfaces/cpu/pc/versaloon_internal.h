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
#ifndef __VERSALOON_INTERNAL_H_INCLUDED__
#define __VERSALOON_INTERNAL_H_INCLUDED__

#define VERSALOON_TIMEOUT				5000
#define VERSALOON_TIMEOUT_LONG			60000

// USB Commands
// Common Commands
#define VERSALOON_COMMON_CMD_START		0x00
#define VERSALOON_COMMON_CMD_END		0x0F

#define VERSALOON_GET_INFO				0x00
#define VERSALOON_FW_UPDATE				0x0F
#define VERSALOON_FW_UPDATE_KEY			0xAA

// USB_TO_XXX Command
#define VERSALOON_USB_TO_XXX_CMD_START	0x20
#define VERSALOON_USB_TO_XXX_CMD_END	0x7F

#endif /* __VERSALOON_INTERNAL_H_INCLUDED__ */

