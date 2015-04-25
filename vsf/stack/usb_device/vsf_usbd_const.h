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

#ifndef __VSF_USBD_CONST_H_INCLUDED__
#define __VSF_USBD_CONST_H_INCLUDED__

#define USB_SETUP_PKG_SIZE			8

#define USB_DESC_SIZE_DEVICE		18
#define USB_DESC_SIZE_CONFIGURATION	9
#define USB_DESC_SIZE_INTERFACE		7
#define USB_DESC_SIZE_ENDPOINT		7

#define USB_STATUS_SIZE				2
#define USB_CONFIGURATION_SIZE		1
#define USB_ALTERNATE_SETTING_SIZE	1

#define USB_DESC_DEVICE_OFF_EP0SIZE	7
#define USB_DESC_DEVICE_OFF_CFGNUM	17
#define USB_DESC_CONFIG_OFF_CFGVAL	5
#define USB_DESC_CONFIG_OFF_BMATTR	7
#define USB_DESC_CONFIG_OFF_IFNUM	4
#define USB_DESC_EP_OFF_EPADDR		2
#define USB_DESC_EP_OFF_EPATTR		3
#define USB_DESC_EP_OFF_EPSIZE		4

// description type
enum usb_description_type_t
{
	USB_DESC_TYPE_DEVICE			= 0x01,
	USB_DESC_TYPE_CONFIGURATION		= 0x02,
	USB_DESC_TYPE_STRING			= 0x03,
	USB_DESC_TYPE_INTERFACE			= 0x04,
	USB_DESC_TYPE_ENDPOINT			= 0x05,
	USB_DESC_TYPE_IAD				= 0x0B,
};

// request
enum usb_stdreq_t
{
	// standard request
	USB_REQ_GET_STATUS				= 0x00,
	USB_REQ_CLEAR_FEATURE			= 0x01,
	USB_REQ_SET_FEATURE				= 0x03,
	USB_REQ_SET_ADDRESS				= 0x05,
	USB_REQ_GET_DESCRIPTOR			= 0x06,
	USB_REQ_SET_DESCRIPTOR			= 0x07,
	USB_REQ_GET_CONFIGURATION		= 0x08,
	USB_REQ_SET_CONFIGURATION		= 0x09,
	USB_REQ_GET_INTERFACE			= 0x0A,
	USB_REQ_SET_INTERFACE			= 0x0B,
};

#define USB_REQ_TYPE_STANDARD		0x00
#define USB_REQ_TYPE_CLASS			0x20
#define USB_REQ_TYPE_VENDOR			0x40
#define USB_REQ_TYPE_MASK			0x60
#define USB_REQ_GET_TYPE(req)		((req) & USB_REQ_TYPE_MASK)

#define USB_REQ_DIR_HTOD			0x00
#define USB_REQ_DIR_DTOH			0x80
#define USB_REQ_DIR_MASK			0x80
#define USB_REQ_GET_DIR(req)		((req) & USB_REQ_DIR_MASK)

#define USB_REQ_RECP_DEVICE			0x00
#define USB_REQ_RECP_INTERFACE		0x01
#define USB_REQ_RECP_ENDPOINT		0x02
#define USB_REQ_RECP_OTHER			0x03
#define USB_REQ_RECP_MASK			0x1F
#define USB_REQ_GET_RECP(req)		((req) & USB_REQ_RECP_MASK)

// feature
enum usb_feature_cmd_t
{
	USB_EP_FEATURE_CMD_HALT				= 0x00,
	USB_DEV_FEATURE_CMD_REMOTE_WAKEUP	= 0x01,
};

// attributes
enum usb_config_attributes_t
{
	USB_CFGATTR_SELFPOWERED			= 0x40,
	USB_CFGATTR_REMOTE_WEAKUP		= 0x20,
};

// device feature
enum usb_device_feature_t
{
	USB_DEV_FEATURE_SELFPOWERED		= 0x01,
	USB_DEV_FEATURE_REMOTE_WEAKUP	= 0x02,
};

#endif	// __VSF_USBD_CONST_H_INCLUDED__

