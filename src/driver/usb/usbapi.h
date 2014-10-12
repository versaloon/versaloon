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
#ifndef __USBAPI_H_INCLUDED__
#define __USBAPI_H_INCLUDED__

#include "libusb-1.0/libusb.h"

extern struct vss_cmd_list_t usbapi_cmd_list;

struct usbapi_param_t
{
	uint8_t valid;
	uint16_t vid;
	uint16_t pid;
	uint8_t epin;
	uint8_t epout;
	uint8_t interface;
	char typestring[256];
	char serialstring[256];
};
extern struct usbapi_param_t usb_param;

uint8_t usb_param_valid(void);
uint16_t usb_param_vid(void);
uint16_t usb_param_pid(void);
uint8_t usb_param_epin(void);
uint8_t usb_param_epout(void);
uint8_t usb_param_interface(void);
char *usb_param_type(void);
char *usb_param_serial(void);
void usb_set_param(uint16_t vid, uint16_t pid, uint8_t epin, uint8_t epout,
					uint8_t interface);
uint32_t print_usb_devices(uint16_t VID, uint16_t PID, int8_t serialindex,
							char *serialstring, int8_t productindex,
							char *productstring);
struct libusb_device_handle* find_usb_device(uint16_t VID, uint16_t PID,
							uint8_t interface, int8_t serialindex,
							char *serialstring, int8_t productindex,
							char *productstring);

#endif /* __USBAPI_H_INCLUDED__ */

