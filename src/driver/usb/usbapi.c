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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "compiler.h"

#include "vsf_err.h"

#include "app_type.h"
#include "app_io.h"
#include "app_log.h"
#include "app_err.h"

#include "usbapi.h"

struct usbapi_param_t usb_param;

// usb_set_param will not ser serialstring
// because serialstring can be different for same usb_device type
// to set serialstring, please use --usb or -U
void usb_set_param(uint16_t vid, uint16_t pid, uint8_t epin, uint8_t epout,
					uint8_t interface)
{
	usb_param.vid = vid;
	usb_param.pid = pid;
	usb_param.epin = epin | 0x80;
	usb_param.epout = epout & 0x7F;
	usb_param.interface = interface;
	usb_param.valid = 1;
}

uint8_t usb_param_valid(void)
{
	return usb_param.valid;
}

uint16_t usb_param_vid(void)
{
	return usb_param.vid;
}

uint16_t usb_param_pid(void)
{
	return usb_param.pid;
}

uint8_t usb_param_epin(void)
{
	return usb_param.epin;
}

uint8_t usb_param_epout(void)
{
	return usb_param.epout;
}

uint8_t usb_param_interface(void)
{
	return usb_param.interface;
}

char *usb_param_type(void)
{
	if (strlen(usb_param.typestring) > 0)
	{
		return usb_param.typestring;
	}
	else
	{
		return NULL;
	}
}

char *usb_param_serial(void)
{
	if (strlen(usb_param.serialstring) > 0)
	{
		return usb_param.serialstring;
	}
	else
	{
		return NULL;
	}
}

static bool usb_check_string(struct libusb_device_handle *dev,
							uint8_t stringidx, char * string,
							char * buff, uint16_t buf_size)
{
	bool alloced = false;
	bool ret = true;
	
	if (NULL == buff)
	{
		buf_size = 256;
		buff = (char*)malloc(buf_size);
		if (NULL == buff)
		{
			ret = 0;
			goto free_and_return;
		}
		alloced = true;
	}
	
	strcpy(buff, "");
	
	if (libusb_get_string_descriptor_ascii(dev, stringidx,
										(unsigned char *)buff, buf_size) < 0)
	{
		ret = false;
		goto free_and_return;
	}
	
	if ((string != NULL) && strcmp((const char *)buff, string))
	{
		ret = false;
		goto free_and_return;
	}
	
free_and_return:
	if (alloced && (buff != NULL))
	{
		free(buff);
		buff = NULL;
	}
	return ret;
}

static struct libusb_context *libusb_ctx = NULL;

uint32_t print_usb_devices(uint16_t VID, uint16_t PID, int8_t serialindex,
							char *serialstring, int8_t productindex,
							char *productstring)
{
	ssize_t usb_devices_num;
	libusb_device **usb_devices;
	int c = 0;
	uint8_t buf[256];
	
	if (NULL == libusb_ctx)
	{
		libusb_init(&libusb_ctx);
	}
	
	usb_devices_num = libusb_get_device_list(libusb_ctx, &usb_devices);
	if (usb_devices_num > 0)
	{
		ssize_t i;
		libusb_device *dev;
		struct libusb_device_descriptor device_desc;
		struct libusb_device_handle *dev_handle = NULL;
		
		for (i = 0; i < usb_devices_num; i++)
		{
			dev = usb_devices[i];
			if (libusb_get_device_descriptor(dev, &device_desc) ||
				(device_desc.idVendor != VID) ||
				(device_desc.idProduct != PID) ||
				libusb_open(dev, &dev_handle))
			{
				continue;
			}
			
			if (((productstring != NULL) && (productindex >= 0) &&
					!usb_check_string(dev_handle, productindex, productstring,
										NULL, 0))
			    || ((serialindex >= 0) &&
					!usb_check_string(dev_handle, serialindex, serialstring,
										(char*)buf, sizeof(buf))))
			{
				libusb_close(dev_handle);
				dev_handle = NULL;
				continue;
			}
			
			// print current device
			if (strlen((char *)buf) > 0)
			{
				PRINTF("%s%d: 0x%04X:0x%04X:%s.\n",
						productstring, c, VID, PID, buf);
			}
			else
			{
				PRINTF("%s%d: 0x%04X:0x%04X.\n", productstring, c, VID, PID);
			}
			c++;
			
			libusb_close(dev_handle);
		}
	}
	
	libusb_free_device_list(usb_devices, 1);
	return c;
}

struct libusb_device_handle* find_usb_device(uint16_t VID, uint16_t PID,
							uint8_t interface, int8_t serialindex,
							char *serialstring, int8_t productindex,
							char *productstring)
{
	ssize_t usb_devices_num;
	libusb_device **usb_devices;
	struct libusb_device_handle *dev_handle = NULL;
	uint8_t buf[256];
	
	if (NULL == libusb_ctx)
	{
		libusb_init(&libusb_ctx);
	}
	
	usb_devices_num = libusb_get_device_list(libusb_ctx, &usb_devices);
	if (usb_devices_num > 0)
	{
		ssize_t i;
		libusb_device *dev;
		struct libusb_device_descriptor device_desc;
		
		for (i = 0; i < usb_devices_num; i++)
		{
			dev = usb_devices[i];
			if (libusb_get_device_descriptor(dev, &device_desc) ||
				(device_desc.idVendor != VID) ||
				(device_desc.idProduct != PID) ||
				libusb_open(dev, &dev_handle))
			{
				continue;
			}
			
			if (((productstring != NULL) && (productindex >= 0) &&
					!usb_check_string(dev_handle, productindex, productstring,
										NULL, 0))
			    || ((serialindex >= 0) &&
					!usb_check_string(dev_handle, serialindex, serialstring,
										(char*)buf, sizeof(buf))))
			{
				libusb_close(dev_handle);
				dev_handle = NULL;
				continue;
			}
			
			if (libusb_claim_interface(dev_handle, interface))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "claim interface");
				libusb_close(dev_handle);
				continue;
			}
			
			break;
		}
	}
	
	libusb_free_device_list(usb_devices, 1);
	return dev_handle;
}

