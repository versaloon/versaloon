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

#include <string.h>

#include "app_type.h"
#include "app_io.h"
#include "app_log.h"
#include "app_err.h"

#include "scripts.h"
#include "strparser.h"

#include "usbapi.h"

VSS_HANDLER(usbapi_param);
static const struct vss_cmd_t usbapi_cmd[] =
{
	VSS_CMD(	"usb",
				"set usb device, format: "
				"usb/U [VID_PID_EPIN_EPOUT_INTERFACE|TYPESTRING]SERIALSTRING",
				usbapi_param,
				NULL),
	VSS_CMD(	"U",
				"set usb device, format: "
				"usb/U [VID_PID_EPIN_EPOUT_INTERFACE|TYPESTRING]SERIALSTRING",
				usbapi_param,
				NULL),
	VSS_CMD_END
};
struct vss_cmd_list_t usbapi_cmd_list = VSS_CMD_LIST("usbapi", usbapi_cmd);

VSS_HANDLER(usbapi_param)
{
	// vid: 2 bytes
	// pid: 2 bytes
	// epin: 1 byte
	// epout: 1 byte
	// interface: 1 byte
	// serialstring: 256 bytes
	uint8_t usb_setting[2 * 256 + 7], *ptr;
	vsf_err_t err;
	uint8_t i;
	char* formats[] =
	{
		// VID_PID_EPIN_EPOUT_INTERFACE_SERIALSTRING
		// vid(2d):pid(2d):epin(1d):epout(1d):interface(1d):serialstring(s)
		"%2d%2d%1d%1d%1d%s",
		// typestring:vid(2d):pid(2d):epin(1d):epout(1d)
		"%2d%2d%1d%1d",
		// typestring:serialstring(s)
		"%s%s",
		// serialstring(s)
		"%s"
	};
	
	VSS_CHECK_ARGC(2);
	
	err = VSFERR_FAIL;
	for (i = 0; i < dimof(formats); i++)
	{
		memset(usb_setting, 0, sizeof(usb_setting));
		err = strparser_parse((char*)argv[1], formats[i],
									usb_setting, sizeof(usb_setting));
		if (!err)
		{
			break;
		}
	}
	
	if (err)
	{
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);
		vss_print_help(argv[0]);
		return ERRCODE_INVALID_OPTION;
	}
	
	ptr = usb_setting;
	if (3 == i)
	{
		strncpy(usb_param.serialstring, (char*)&ptr[0],
					sizeof(usb_param.serialstring));
	}
	else if (2 == i)
	{
		strncpy(usb_param.typestring, (char*)&ptr[0],
					sizeof(usb_param.typestring));
		strncpy(usb_param.serialstring,
					(char*)&ptr[0] + strlen(usb_param.typestring) + 1,
					sizeof(usb_param.serialstring));
	}
	else if (1 == i)
	{
		usb_param.valid = 1;
		usb_param.vid = ptr[0] + (ptr[1] << 8);
		usb_param.pid = ptr[2] + (ptr[3] << 8);
		usb_param.epin = ptr[4] | 0x80;
		usb_param.epout = ptr[5] & 0x7F;
	}
	else if (0 == i)
	{
		usb_param.valid = 1;
		usb_param.vid = ptr[0] + (ptr[1] << 8);
		usb_param.pid = ptr[2] + (ptr[3] << 8);
		usb_param.epin = ptr[4] | 0x80;
		usb_param.epout = ptr[5] & 0x7F;
		usb_param.interface = ptr[6];
		strncpy(usb_param.serialstring, (char*)&ptr[7],
					sizeof(usb_param.serialstring));
	}
	
	if (strlen(usb_param.serialstring) > 0)
	{
		LOG_DEBUG("usb_device is on 0x%04X:0x%04X(0x%02x_0x%02X):%s.",
			usb_param.vid, usb_param.pid, usb_param.epin, usb_param.epout,
			usb_param.serialstring);
	}
	else
	{
		LOG_DEBUG("usb_device is on 0x%04X:0x%04X(0x%02x_0x%02X).",
			usb_param.vid, usb_param.pid, usb_param.epin, usb_param.epout);
	}
	return VSFERR_NONE;
}

