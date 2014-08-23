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
#ifndef __PORT_H_INCLUDED__
#define __PORT_H_INCLUDED__

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if IS_WIN32

#include <windows.h>

#define FILE_SEPARATOR			'\\'
#define sleep_ms(ms)			Sleep(ms)

#else /* !IS_WIN32 */

#include <unistd.h>
#include <fcntl.h>
#include <termios.h>

#define FILE_SEPARATOR			'/'
#define sleep_ms(ms)			usleep((ms) * 1000)

#endif /* IS_WIN32 */





#ifndef MAX_PATH
#	define MAX_PATH				260
#endif

#endif /* __PORT_H_INCLUDED__ */

