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

#include "app_cfg.h"
#include "interfaces.h"

#include "tool/buffer/buffer.h"

#include "vsfusbd_cfg.h"
#include "../usb_common.h"
#include "vsfusbd.h"

// events for vsfusbd
#define VSFUSBD_INTEVT_BASE				VSFSM_EVT_USER_LOCAL
enum vsfusbd_evt_t
{
	VSFUSBD_INTEVT_RESET = VSFUSBD_INTEVT_BASE + 0,
	VSFUSBD_INTEVT_SUSPEND = VSFUSBD_INTEVT_BASE + 1,
	VSFUSBD_INTEVT_RESUME = VSFUSBD_INTEVT_BASE + 2,
	VSFUSBD_INTEVT_WAKEUP = VSFUSBD_INTEVT_BASE + 3,
	VSFUSBD_INTEVT_DETACH = VSFUSBD_INTEVT_BASE + 4,
	VSFUSBD_INTEVT_ATTACH = VSFUSBD_INTEVT_BASE + 5,
	VSFUSBD_INTEVT_SOF = VSFUSBD_INTEVT_BASE + 6,
	VSFUSBD_INTEVT_SETUP = VSFUSBD_INTEVT_BASE + 7,
	VSFUSBD_INTEVT_IN = VSFUSBD_INTEVT_BASE + 0x10,
	VSFUSBD_INTEVT_OUT = VSFUSBD_INTEVT_BASE + 0x20,
	VSFUSBD_EVT_DATAIO_IN = VSFUSBD_INTEVT_BASE + 0x30,
	VSFUSBD_EVT_DATAIO_OUT = VSFUSBD_INTEVT_BASE + 0x40,
	VSFUSBD_INTEVT_ERR = VSFUSBD_INTEVT_BASE + 0x100,
};
#define VSFUSBD_INTEVT_INEP(ep)			(VSFUSBD_INTEVT_IN + (ep))
#define VSFUSBD_INTEVT_OUTEP(ep)		(VSFUSBD_INTEVT_OUT + (ep))
#define VSFUSBD_INTEVT_INOUT_MASK		~0xF
#define VSFUSBD_INTEVT_ERR_MASK			~0xFF

vsf_err_t vsfusbd_device_get_descriptor(struct vsfusbd_device_t *device, 
		struct vsfusbd_desc_filter_t *filter, uint8_t type, uint8_t index, 
		uint16_t lanid, struct vsf_buffer_t *buffer)
{
	while (((filter->buffer.buffer != NULL) && (filter->buffer.size != 0)) || 
			(filter->read != NULL))
	{
		if ((filter->type == type) && (filter->index == index) && 
			(filter->lanid == lanid))
		{
			buffer->size = filter->buffer.size;
			buffer->buffer = filter->buffer.buffer;
			
			if (filter->read != NULL)
			{
				return filter->read(device, buffer);
			}
			return VSFERR_NONE;
		}
		filter++;
	}
	return VSFERR_FAIL;
}

vsf_err_t vsfusbd_set_IN_handler(struct vsfusbd_device_t *device,
		uint8_t ep, vsf_err_t (*handler)(struct vsfusbd_device_t*, uint8_t))
{
	if (ep >= VSFUSBD_CFG_MAX_IN_EP)
	{
		return VSFERR_INVALID_PARAMETER;
	}
	
	device->IN_handler[ep] = handler;
	return VSFERR_NONE;
}

vsf_err_t vsfusbd_set_OUT_handler(struct vsfusbd_device_t *device,
		uint8_t ep, vsf_err_t (*handler)(struct vsfusbd_device_t*, uint8_t))
{
	if (ep >= VSFUSBD_CFG_MAX_OUT_EP)
	{
		return VSFERR_INVALID_PARAMETER;
	}
	
	device->OUT_handler[ep] = handler;
	return VSFERR_NONE;
}

static uint8_t* vsfusbd_data_io(struct vsfusbd_transact_t *transact)
{
	struct vsf_transaction_buffer_t *tbuffer = &transact->tbuffer;
	if (tbuffer->buffer.buffer != NULL)
	{
		return &tbuffer->buffer.buffer[tbuffer->position];
	}
	else if (transact->callback.data_io != NULL)
	{
		return transact->callback.data_io(transact->callback.param);
	}
	return NULL;
}

vsf_err_t vsfusbd_ep_receive_nb(struct vsfusbd_device_t *device, uint8_t ep)
{
	struct vsfusbd_transact_t *transact = &device->OUT_transact[ep];
	struct vsf_transaction_buffer_t *tbuffer = &transact->tbuffer;
	uint16_t size;
	
	transact->pkt.out.isshort = false;
	tbuffer->position = 0;
	size = tbuffer->buffer.size;
	
	if (size > 0)
	{
		if (vsfusbd_data_io(transact) != NULL)
		{
			transact->need_poll = false;
			return device->drv->ep.enable_OUT(ep);
		}
		else
		{
			transact->need_poll = true;
		}
	}
	else
	{
		return device->drv->ep.enable_OUT(ep);
	}
	return VSFERR_NONE;
}

vsf_err_t vsfusbd_ep_send_nb(struct vsfusbd_device_t *device, uint8_t ep)
{
	struct vsfusbd_transact_t *transact = &device->IN_transact[ep];
	struct vsf_transaction_buffer_t *tbuffer = &transact->tbuffer;
	uint8_t *buffer_ptr;
	uint32_t remain_size;
	uint16_t pkg_size, ep_size;
	
	tbuffer->position = 0;
	
	remain_size = tbuffer->buffer.size;
	ep_size = device->drv->ep.get_IN_epsize(ep);
	transact->pkt.in.num = (remain_size + ep_size - 1) / ep_size;
	if (transact->pkt.in.zlp && !(tbuffer->buffer.size % ep_size))
	{
		transact->pkt.in.num++;
	}
	pkg_size = min(remain_size, ep_size);
	
	if (pkg_size > 0)
	{
		buffer_ptr = vsfusbd_data_io(transact);
		transact->need_poll = (NULL == buffer_ptr);
		if (transact->need_poll)
		{
			return VSFERR_NONE;
		}
		device->drv->ep.write_IN_buffer(ep, buffer_ptr, pkg_size);
	}
	device->drv->ep.set_IN_count(ep, pkg_size);
	tbuffer->position = pkg_size;
	remain_size -= pkg_size;
	
#if VSFUSBD_CFG_DBUFFER_EN
	if (device->drv->ep.is_IN_dbuffer(ep))
	{
		pkg_size = min(remain_size, ep_size);
		device->drv->ep.switch_IN_buffer(ep);
		
		if (pkg_size)
		{
			buffer_ptr = vsfusbd_data_io(transact);
			if (NULL == buffer_ptr)
			{
				return VSFERR_NONE;
			}
			device->drv->ep.write_IN_buffer(ep, buffer_ptr, pkg_size);
		}
		device->drv->ep.set_IN_count(ep, pkg_size);
		tbuffer->position += pkg_size;
	}
#endif
	
	return VSFERR_NONE;
}

// standard request handlers
static vsf_err_t vsfusbd_stdreq_get_device_status_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	struct vsfusbd_ctrl_handler_t *ctrl_handler = &device->ctrl_handler;
	struct usb_ctrl_request_t *request = &ctrl_handler->request;
	
	if ((request->value != 0) || (request->index != 0))
	{
		return VSFERR_FAIL;
	}
	
	ctrl_handler->ctrl_reply_buffer[0] = device->feature;
	ctrl_handler->ctrl_reply_buffer[1] = 0;
	buffer->buffer = ctrl_handler->ctrl_reply_buffer;
	buffer->size = USB_STATUS_SIZE;
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_stdreq_get_interface_status_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	struct vsfusbd_ctrl_handler_t *ctrl_handler = &device->ctrl_handler;
	struct usb_ctrl_request_t *request = &ctrl_handler->request;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	
	if ((request->value != 0) || 
		(request->index >= config->num_of_ifaces))
	{
		return VSFERR_FAIL;
	}
	
	ctrl_handler->ctrl_reply_buffer[0] = 0;
	ctrl_handler->ctrl_reply_buffer[1] = 0;
	buffer->buffer = ctrl_handler->ctrl_reply_buffer;
	buffer->size = USB_STATUS_SIZE;
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_stdreq_get_endpoint_status_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	struct vsfusbd_ctrl_handler_t *ctrl_handler = &device->ctrl_handler;
	struct usb_ctrl_request_t *request = &ctrl_handler->request;
	uint8_t ep_num = request->index & 0x7F;
	uint8_t ep_dir = request->index & 0x80;
	
	if ((request->value != 0) || 
		(request->index >= *device->drv->ep.num_of_ep))
	{
		return VSFERR_FAIL;
	}
	
	if ((ep_dir && device->drv->ep.is_IN_stall(ep_num)) || 
		(!ep_dir && device->drv->ep.is_OUT_stall(ep_num)))
	{
		ctrl_handler->ctrl_reply_buffer[0] = 1;
	}
	else
	{
		ctrl_handler->ctrl_reply_buffer[0] = 0;
	}
	ctrl_handler->ctrl_reply_buffer[1] = 0;
	buffer->buffer = ctrl_handler->ctrl_reply_buffer;
	buffer->size = USB_STATUS_SIZE;
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_stdreq_clear_device_feature_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	struct usb_ctrl_request_t *request = &device->ctrl_handler.request;
	
	if ((request->index != 0) || 
		(request->value != USB_DEV_FEATURE_CMD_REMOTE_WAKEUP))
	{
		return VSFERR_FAIL;
	}
	
	device->feature &= ~USB_CFGATTR_REMOTE_WEAKUP;
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_stdreq_clear_interface_feature_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_stdreq_clear_endpoint_feature_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	struct usb_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t ep_num = request->index & 0x7F;
	uint8_t ep_dir = request->index & 0x80;
	
	if ((request->value != USB_EP_FEATURE_CMD_HALT) || 
		(ep_num >= *device->drv->ep.num_of_ep))
	{
		return VSFERR_FAIL;
	}
	
	if (ep_dir)
	{
		device->drv->ep.reset_IN_toggle(ep_num);
		device->drv->ep.clear_IN_stall(ep_num);
	}
	else
	{
		device->drv->ep.reset_OUT_toggle(ep_num);
		device->drv->ep.clear_OUT_stall(ep_num);
		device->drv->ep.enable_OUT(ep_num);
	}
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_stdreq_set_device_feature_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	struct usb_ctrl_request_t *request = &device->ctrl_handler.request;
	
	if ((request->index != 0) || 
		(request->value != USB_DEV_FEATURE_CMD_REMOTE_WAKEUP))
	{
		return VSFERR_FAIL;
	}
	
	device->feature |= USB_CFGATTR_REMOTE_WEAKUP;
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_stdreq_set_interface_feature_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_stdreq_set_endpoint_feature_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	return VSFERR_FAIL;
}

static vsf_err_t vsfusbd_stdreq_set_address_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	struct usb_ctrl_request_t *request = &device->ctrl_handler.request;
	
	if ((request->value > 127) || (request->index != 0) || 
		(device->configuration != 0))
	{
		return VSFERR_FAIL;
	}
	
	return VSFERR_NONE;
}
static vsf_err_t vsfusbd_stdreq_set_address_process(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct usb_ctrl_request_t *request = &device->ctrl_handler.request;
	
	device->address = (uint8_t)request->value;
	return device->drv->set_address(device->address);
}

static vsf_err_t vsfusbd_stdreq_get_device_descriptor_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	struct usb_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t type = (request->value >> 8) & 0xFF, index = request->value & 0xFF;
	uint16_t lanid = request->index;
	
	return vsfusbd_device_get_descriptor(device, device->desc_filter, type, 
											index, lanid, buffer);
}

static vsf_err_t vsfusbd_stdreq_get_interface_descriptor_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	struct usb_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	uint8_t type = (request->value >> 8) & 0xFF, index = request->value & 0xFF;
	uint16_t iface = request->index;
	struct vsfusbd_class_protocol_t *protocol = 
										config->iface[iface].class_protocol;
	
	if ((iface > config->num_of_ifaces) || (NULL == protocol) || 
		((NULL == protocol->desc_filter) && (NULL == protocol->get_desc)))
	{
		return VSFERR_FAIL;
	}
	
	if (((NULL == protocol->desc_filter) ||
			vsfusbd_device_get_descriptor(device, 
				protocol->desc_filter, type, index, 0, buffer)) && 
		((NULL == protocol->get_desc) || 
		 	protocol->get_desc(device, type, index, 0, buffer)))
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_stdreq_get_configuration_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	struct vsfusbd_ctrl_handler_t *ctrl_handler = &device->ctrl_handler;
	struct usb_ctrl_request_t *request = &ctrl_handler->request;
	
	if ((request->value != 0) || (request->index != 0))
	{
		return VSFERR_FAIL;
	}
	
	ctrl_handler->ctrl_reply_buffer[0] = 
					device->config[device->configuration].configuration_value;
	buffer->buffer = ctrl_handler->ctrl_reply_buffer;
	buffer->size = USB_CONFIGURATION_SIZE;
	return VSFERR_NONE;
}

static int16_t vsfusbd_get_config(struct vsfusbd_device_t *device,
									uint8_t value)
{
	uint8_t i;
	
	for (i = 0; i < device->num_of_configuration; i++)
	{
		if (value == device->config[i].configuration_value)
		{
			return i;
		}
	}
	return -1;
}

static vsf_err_t vsfusbd_stdreq_set_configuration_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	struct usb_ctrl_request_t *request = &device->ctrl_handler.request;
	
	if ((request->index != 0) || 
		(vsfusbd_get_config(device, request->value) < 0))
	{
		return VSFERR_FAIL;
	}
	
	// remove original sub-statemachine
	if (device->configured)
	{
		int16_t config_idx = vsfusbd_get_config(device, device->configuration);
		uint8_t i;
		
		device->configured = false;
		for (i = 0; i < device->config[config_idx].num_of_ifaces; i++)
		{
			vsfsm_remove_subsm(&device->sm.init_state,
					&device->config[config_idx].iface[i].sm);
		}
	}
	
	return VSFERR_NONE;
}

#if VSFUSBD_CFG_AUTOSETUP
static vsf_err_t vsfusbd_on_OUT_do(struct vsfusbd_device_t *device, uint8_t ep);
static vsf_err_t vsfusbd_on_IN_do(struct vsfusbd_device_t *device, uint8_t ep);

static vsf_err_t vsfusbd_auto_init(struct vsfusbd_device_t *device)
{
	struct vsfusbd_config_t *config;
	struct vsf_buffer_t desc = {NULL, 0};
	enum interface_usbd_eptype_t ep_type;
	uint16_t pos;
	uint8_t attr, feature;
	uint16_t ep_size, ep_addr, ep_index, ep_attr;
	int16_t cur_iface;
	
	config = &device->config[device->configuration];
	
	if (vsfusbd_device_get_descriptor(device, device->desc_filter, 
											USB_DESC_TYPE_DEVICE, 0, 0, &desc)
#if __VSF_DEBUG__
		|| (NULL == desc.buffer) || (desc.size != USB_DESC_SIZE_DEVICE)
		|| (desc.buffer[0] != desc.size) 
		|| (desc.buffer[1] != USB_DESC_TYPE_DEVICE)
		|| (device->num_of_configuration != 
										desc.buffer[USB_DESC_DEVICE_OFF_CFGNUM])
#endif
		)
	{
		return VSFERR_FAIL;
	}
	
	// config ep0
	ep_size = desc.buffer[USB_DESC_DEVICE_OFF_EP0SIZE];
	if (device->drv->prepare_buffer() || 
		device->drv->ep.set_IN_epsize(0, ep_size) || 
		device->drv->ep.set_OUT_epsize(0, ep_size))
	{
		return VSFERR_FAIL;
	}
	
	// config other eps according to descriptors
	if (vsfusbd_device_get_descriptor(device, device->desc_filter, 
				USB_DESC_TYPE_CONFIGURATION, device->configuration, 0, &desc)
#if __VSF_DEBUG__
		|| (NULL == desc.buffer) || (desc.size <= USB_DESC_SIZE_CONFIGURATION)
		|| (desc.buffer[0] != USB_DESC_SIZE_CONFIGURATION)
		|| (desc.buffer[1] != USB_DESC_TYPE_CONFIGURATION)
		|| (config->num_of_ifaces != desc.buffer[USB_DESC_CONFIG_OFF_IFNUM])
#endif
		)
	{
		return VSFERR_FAIL;
	}
	
	// initialize device feature according to 
	// bmAttributes field in configuration descriptor
	attr = desc.buffer[USB_DESC_CONFIG_OFF_BMATTR];
	feature = 0;
	if (attr & USB_CFGATTR_SELFPOWERED)
	{
		feature |= USB_DEV_FEATURE_SELFPOWERED;
	}
	if (attr & USB_CFGATTR_REMOTE_WEAKUP)
	{
		feature |= USB_DEV_FEATURE_REMOTE_WEAKUP;
	}
	
#if __VSF_DEBUG__
	num_iface = desc.buffer[USB_DESC_CONFIG_OFF_IFNUM];
	num_endpoint = 0;
#endif
	
	cur_iface = -1;
	pos = USB_DESC_SIZE_CONFIGURATION;
	while (desc.size > pos)
	{
#if __VSF_DEBUG__
		if ((desc.buffer[pos] < 2) || (desc.size < (pos + desc.buffer[pos])))
		{
			return VSFERR_FAIL;
		}
#endif
		switch (desc.buffer[pos + 1])
		{
		case USB_DESC_TYPE_INTERFACE:
#if __VSF_DEBUG__
			if (num_endpoint)
			{
				return VSFERR_FAIL;
			}
			num_endpoint = desc.buffer[pos + 4];
			num_iface--;
#endif
			cur_iface = desc.buffer[pos + 2];
			break;
		case USB_DESC_TYPE_ENDPOINT:
			ep_addr = desc.buffer[pos + USB_DESC_EP_OFF_EPADDR];
			ep_attr = desc.buffer[pos + USB_DESC_EP_OFF_EPATTR];
			ep_size = desc.buffer[pos + USB_DESC_EP_OFF_EPSIZE];
			ep_index = ep_addr & 0x0F;
#if __VSF_DEBUG__
			num_endpoint--;
			if (ep_index > (*device->drv->ep.num_of_ep - 1))
			{
				return VSFERR_FAIL;
			}
#endif
			switch (ep_attr & 0x03)
			{
			case 0x00:
				ep_type = USB_EP_TYPE_CONTROL;
				break;
			case 0x01:
				ep_type = USB_EP_TYPE_ISO;
				break;
			case 0x02:
				ep_type = USB_EP_TYPE_BULK;
				break;
			case 0x03:
				ep_type = USB_EP_TYPE_INTERRUPT;
				break;
			default:
				return VSFERR_FAIL;
			}
			device->drv->ep.set_type(ep_index, ep_type);
			if (ep_addr & 0x80)
			{
				// IN ep
				device->drv->ep.set_IN_epsize(ep_index, ep_size);
				vsfusbd_set_IN_handler(device, ep_index, vsfusbd_on_IN_do);
				config->ep_IN_iface_map[ep_index] = cur_iface;
			}
			else
			{
				// OUT ep
				device->drv->ep.set_OUT_epsize(ep_index, ep_size);
				vsfusbd_set_OUT_handler(device, ep_index, vsfusbd_on_OUT_do);
				config->ep_OUT_iface_map[ep_index] = cur_iface;
			}
			break;
		}
		pos += desc.buffer[pos];
	}
#if __VSF_DEBUG__
	if (num_iface || num_endpoint || (desc.size != pos))
	{
		return VSFERR_FAIL;
	}
#endif
	return VSFERR_NONE;
}
#endif

static vsf_err_t vsfusbd_stdreq_set_configuration_process(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct usb_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_config_t *config;
	int16_t config_idx;
	uint8_t i;
#if __VSF_DEBUG__
	uint8_t num_iface, num_endpoint;
#endif
	
	config_idx = vsfusbd_get_config(device, request->value);
	if (config_idx < 0)
	{
		return VSFERR_FAIL;
	}
	device->configuration = (uint8_t)config_idx;
	config = &device->config[device->configuration];
	
#if VSFUSBD_CFG_AUTOSETUP
	if (vsfusbd_auto_init(device))
	{
		return VSFERR_FAIL;
	}
#endif
	
	// call user initialization
	if ((config->init != NULL) && config->init(device))
	{
		return VSFERR_FAIL;
	}
	
	for (i = 0; i < config->num_of_ifaces; i++)
	{
		config->iface[i].alternate_setting = 0;
		
		if (((config->iface[i].class_protocol != NULL) && 
				(config->iface[i].class_protocol->init != NULL) && 
				config->iface[i].class_protocol->init(i, device)) ||
			vsfsm_add_subsm(&device->sm.init_state, &config->iface[i].sm))
		{
			return VSFERR_FAIL;
		}
	}
	
	device->configured = true;
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_stdreq_get_interface_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	struct vsfusbd_ctrl_handler_t *ctrl_handler = &device->ctrl_handler;
	struct usb_ctrl_request_t *request = &ctrl_handler->request;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	
	if ((request->value != 0) || 
		(iface >= device->config[device->configuration].num_of_ifaces))
	{
		return VSFERR_FAIL;
	}
	
	ctrl_handler->ctrl_reply_buffer[0] = config->iface[iface].alternate_setting;
	buffer->buffer = ctrl_handler->ctrl_reply_buffer;
	buffer->size = USB_ALTERNATE_SETTING_SIZE;
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_stdreq_set_interface_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	struct usb_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t iface_idx = request->index;
	uint8_t alternate_setting = request->value;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	
	if (iface_idx >= device->config[device->configuration].num_of_ifaces)
	{
		return VSFERR_FAIL;
	}
	
	config->iface[iface_idx].alternate_setting = alternate_setting;
	if (device->callback.on_set_interface != NULL)
	{
		return device->callback.on_set_interface(iface_idx, alternate_setting);
	}
	return VSFERR_NONE;
}

static const struct vsfusbd_setup_filter_t vsfusbd_standard_req_filter[] = 
{
	// USB_REQ_GET_STATUS
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_DEVICE,
		USB_REQ_GET_STATUS,
		vsfusbd_stdreq_get_device_status_prepare,
		NULL
	},
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_INTERFACE,
		USB_REQ_GET_STATUS,
		vsfusbd_stdreq_get_interface_status_prepare,
		NULL
	},
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_ENDPOINT,
		USB_REQ_GET_STATUS,
		vsfusbd_stdreq_get_endpoint_status_prepare,
		NULL
	},
	// USB_REQ_CLEAR_FEATURE
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_DEVICE,
		USB_REQ_CLEAR_FEATURE,
		vsfusbd_stdreq_clear_device_feature_prepare,
		NULL
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_INTERFACE,
		USB_REQ_CLEAR_FEATURE,
		vsfusbd_stdreq_clear_interface_feature_prepare,
		NULL
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_ENDPOINT,
		USB_REQ_CLEAR_FEATURE,
		vsfusbd_stdreq_clear_endpoint_feature_prepare,
		NULL
	},
	// USB_REQ_SET_FEATURE
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_DEVICE,
		USB_REQ_SET_FEATURE,
		vsfusbd_stdreq_set_device_feature_prepare,
		NULL
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_INTERFACE,
		USB_REQ_SET_FEATURE,
		vsfusbd_stdreq_set_interface_feature_prepare,
		NULL
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_ENDPOINT,
		USB_REQ_SET_FEATURE,
		vsfusbd_stdreq_set_endpoint_feature_prepare,
		NULL
	},
	// USB_REQ_SET_ADDRESS
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_DEVICE,
		USB_REQ_SET_ADDRESS,
		vsfusbd_stdreq_set_address_prepare,
		vsfusbd_stdreq_set_address_process
	},
	// USB_REQ_GET_DESCRIPTOR
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_DEVICE,
		USB_REQ_GET_DESCRIPTOR,
		vsfusbd_stdreq_get_device_descriptor_prepare,
		NULL
	},
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_INTERFACE,
		USB_REQ_GET_DESCRIPTOR,
		vsfusbd_stdreq_get_interface_descriptor_prepare,
		NULL
	},
	// USB_REQ_SET_DESCRIPTOR, not supported
	// USB_REQ_GET_CONFIGURATION
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_DEVICE,
		USB_REQ_GET_CONFIGURATION,
		vsfusbd_stdreq_get_configuration_prepare,
		NULL
	},
	// USB_REQ_SET_CONFIGURATION
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_DEVICE,
		USB_REQ_SET_CONFIGURATION,
		vsfusbd_stdreq_set_configuration_prepare,
		vsfusbd_stdreq_set_configuration_process
	},
	// USB_REQ_GET_INTERFACE
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_INTERFACE,
		USB_REQ_GET_INTERFACE,
		vsfusbd_stdreq_get_interface_prepare,
		NULL
	},
	// USB_REQ_SET_INTERFACE
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_INTERFACE,
		USB_REQ_SET_INTERFACE,
		vsfusbd_stdreq_set_interface_prepare,
		NULL
	},
	VSFUSBD_SETUP_NULL
};

static struct vsfusbd_setup_filter_t *vsfusbd_get_request_filter_do(
		struct vsfusbd_device_t *device, struct vsfusbd_setup_filter_t *list)
{
	struct usb_ctrl_request_t *request = &device->ctrl_handler.request;
	
	if (list != NULL)
	{
		while (list->type != VSFUSBD_SETUP_INVALID_TYPE)
		{
			if ((list->type == request->type) && 
				(list->request == request->request))
			{
				return list;
			}
			list++;
		}
	}
	return NULL;
}

struct vsfusbd_setup_filter_t *vsfusbd_get_class_request_filter(
		struct vsfusbd_device_t *device,
		struct vsfusbd_class_protocol_t *class_protocol)
{
	struct vsfusbd_setup_filter_t *result =
			vsfusbd_get_request_filter_do(device, class_protocol->req_filter);
	
	if ((NULL == result) && (class_protocol->get_request_filter != NULL))
	{
		// try call class_protocol->get_request_filter
		result = class_protocol->get_request_filter(device);
	}
	return result;
}

static struct vsfusbd_setup_filter_t *vsfusbd_get_request_filter(
		struct vsfusbd_device_t *device, int8_t *iface)
{
	struct usb_ctrl_request_t *request = &device->ctrl_handler.request;
	
	if (USB_REQ_GET_TYPE(request->type) == USB_REQ_TYPE_STANDARD)
	{
		*iface = -1;
		return vsfusbd_get_request_filter_do(device, 
				(struct vsfusbd_setup_filter_t *)vsfusbd_standard_req_filter);
	}
	else
	{
		struct vsfusbd_config_t *config = 
										&device->config[device->configuration];
		
		if (USB_REQ_GET_RECP(request->type) == USB_REQ_RECP_INTERFACE)
		{
			*iface = (int8_t)request->index;
		}
		else if (USB_REQ_GET_RECP(request->type) == USB_REQ_RECP_OTHER)
		{
			// find an interface which will claim this request
			struct vsfusbd_setup_filter_t *setup_filter;
			uint8_t i;
			
			for (i = 0; i < config->num_of_ifaces; i++)
			{
				setup_filter = vsfusbd_get_request_filter_do(device, 
								config->iface[i].class_protocol->req_filter);
				if (setup_filter != NULL)
				{
					*iface = i;
					return setup_filter;
				}
			}
			*iface = -1;
			return NULL;
		}
		else if ((USB_REQ_GET_RECP(request->type) == USB_REQ_RECP_DEVICE) && 
				(USB_REQ_GET_TYPE(request->type) == USB_REQ_TYPE_CLASS))
		{
			*iface = (int8_t)device->device_class_iface;
		}
		else
		{
			*iface = -1;
			return NULL;
		}
		
		if ((*iface >= config->num_of_ifaces) || 
			(NULL == config->iface[*iface].class_protocol))
		{
			*iface = -1;
			return NULL;
		}
		
		return vsfusbd_get_class_request_filter(device,
										config->iface[*iface].class_protocol);
	}
}

// on_IN and on_OUT executer
static vsf_err_t vsfusbd_on_IN_do(struct vsfusbd_device_t *device, uint8_t ep)
{
	struct vsfusbd_transact_t *transact = &device->IN_transact[ep];
	struct vsf_transaction_buffer_t *tbuffer = &transact->tbuffer;
	uint32_t remain_size;
	uint16_t pkg_size, ep_size;
	
	if (--device->IN_transact[ep].pkt.in.num > 0)
	{
#if VSFUSBD_CFG_DBUFFER_EN
		uint32_t pkg_thres;
		
		if (device->drv->ep.is_IN_dbuffer(ep))
		{
			device->drv->ep.switch_IN_buffer(ep);
			pkg_thres = 1;
		}
		else
		{
			pkg_thres = 0;
		}
		
		if (device->IN_transact[ep].pkt.in.num > pkg_thres)
#else
		if (device->IN_transact[ep].pkt.in.num > 0)
#endif
		{
			remain_size = tbuffer->buffer.size - tbuffer->position;
			
			if (remain_size)
			{
				uint8_t *buffer_ptr = vsfusbd_data_io(transact);
				
				transact->need_poll = (NULL == buffer_ptr);
				if (transact->need_poll)
				{
					return VSFERR_NONE;
				}
				
				ep_size = device->drv->ep.get_IN_epsize(ep);
				pkg_size = min(remain_size, ep_size);
				device->drv->ep.write_IN_buffer(ep, buffer_ptr, pkg_size);
				device->drv->ep.set_IN_count(ep, pkg_size);
				tbuffer->position += pkg_size;
			}
			else
			{
				device->drv->ep.set_IN_count(ep, 0);
			}
		}
	}
	else
	{
		struct vsfusbd_transact_callback_t *callback = &transact->callback;
		
		if (callback->callback != NULL)
		{
			callback->callback(callback->param);
		}
	}
	
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_on_OUT_do(struct vsfusbd_device_t *device, uint8_t ep)
{
	struct vsfusbd_transact_t *transact = &device->OUT_transact[ep];
	struct vsf_transaction_buffer_t *tbuffer = &transact->tbuffer;
	uint16_t pkg_size;
	
#if VSFUSBD_CFG_DBUFFER_EN
	if (device->drv->ep.is_OUT_dbuffer(ep))
	{
		device->drv->ep.switch_OUT_buffer(ep);
	}
#endif
	pkg_size = device->drv->ep.get_OUT_count(ep);
	if ((tbuffer->position < tbuffer->buffer.size) &&
		((tbuffer->position + pkg_size) <= tbuffer->buffer.size))
	{
		// require the buffer by using NULL as the 3rd parameter
		uint8_t *data = NULL;
		
		if (pkg_size > 0)
		{
			data = vsfusbd_data_io(transact);
			if (NULL == data)
			{
				// TODO: error processor, maybe stall endpoint
				return VSFERR_FAIL;
			}
			
			device->drv->ep.read_OUT_buffer(ep, data, pkg_size);
// TODO: notify the upper layer for incoming data
//			vsfusbd_data_io(device, transact, data);
			tbuffer->position += pkg_size;
		}
		
		if (pkg_size < device->drv->ep.get_OUT_epsize(ep))
		{
			transact->pkt.out.isshort = true;
		}
		if (transact->pkt.out.isshort)
		{
			struct vsfusbd_transact_callback_t *callback = &transact->callback;
		
			if (callback->callback != NULL)
			{
				callback->callback(callback->param);
			}
			return VSFERR_NONE;
		}
		
		if (tbuffer->position < tbuffer->buffer.size)
		{
			// more data to receive
			transact->need_poll = (NULL == vsfusbd_data_io(transact));
			if (!transact->need_poll)
			{
				device->drv->ep.enable_OUT(ep);
			}
		}
		else
		{
			// zlp
			device->drv->ep.enable_OUT(ep);
		}
		return VSFERR_NONE;
	}
	return VSFERR_FAIL;
}

static void vsfusbd_setup_end_callback(void *param)
{
	struct vsfusbd_device_t *device = param;
	struct vsfusbd_ctrl_handler_t *ctrl_handler = &device->ctrl_handler;
	struct usb_ctrl_request_t *request = &ctrl_handler->request;
	struct vsf_transaction_buffer_t *tbuffer;
	
	if (ctrl_handler->filter->process != NULL)
	{
		tbuffer = (USB_REQ_GET_DIR(request->type) == USB_REQ_DIR_HTOD) ?
			&device->OUT_transact[0].tbuffer :
			&device->IN_transact[0].tbuffer;
		ctrl_handler->filter->process(device, &tbuffer->buffer);
	}
}

static void vsfusbd_setup_status_callback(void *param)
{
	struct vsfusbd_device_t *device = param;
	struct vsfusbd_ctrl_handler_t *ctrl_handler = &device->ctrl_handler;
	struct usb_ctrl_request_t *request = &ctrl_handler->request;
	
	if (USB_REQ_GET_DIR(request->type) == USB_REQ_DIR_HTOD)
	{
		device->IN_transact[0].tbuffer.buffer.buffer = NULL;
		device->IN_transact[0].tbuffer.buffer.size = 0;
		device->IN_transact[0].pkt.in.zlp = true;
		device->IN_transact[0].callback.param = device;
		device->IN_transact[0].callback.callback =
											vsfusbd_setup_end_callback;
		device->IN_transact[0].callback.data_io = NULL;
		vsfusbd_ep_send_nb(device, 0);
	}
	else
	{
		device->OUT_transact[0].tbuffer.buffer.buffer = NULL;
		device->OUT_transact[0].tbuffer.buffer.size = 0;
		device->OUT_transact[0].callback.param = device;
		device->OUT_transact[0].callback.callback =
											vsfusbd_setup_end_callback;
		device->OUT_transact[0].callback.data_io = NULL;
		device->OUT_transact[0].need_poll = false;
		vsfusbd_ep_receive_nb(device, 0);
	}
}

// state machines
static struct vsfsm_state_t *
vsfusbd_evt_handler(struct vsfsm_t *sm, vsfsm_evt_t evt)
{
	struct vsfusbd_device_t *device =
								container_of(sm, struct vsfusbd_device_t, sm);
	vsf_err_t err = VSFERR_NONE;
	
	switch (evt)
	{
	case VSFUSBD_INTEVT_RESET:
		{
			struct vsfusbd_config_t *config;
			uint8_t i;
		#if VSFUSBD_CFG_AUTOSETUP
			struct vsf_buffer_t desc = {NULL, 0};
			uint16_t ep_size;
		#endif
			
			memset(device->IN_transact, 0, sizeof(device->IN_transact));
			memset(device->OUT_transact, 0,sizeof(device->OUT_transact));
			
			device->configured = false;
			device->configuration = 0;
			device->feature = 0;
			device->ctrl_handler.state = USB_CTRL_STAT_WAIT_SETUP;
			
			for (i = 0; i < device->num_of_configuration; i++)
			{
				config = &device->config[i];
				memset(config->ep_OUT_iface_map, -1,
											sizeof(config->ep_OUT_iface_map));
				memset(config->ep_IN_iface_map, -1,
											sizeof(config->ep_OUT_iface_map));
			}
			
			// reset usb hw
			if (device->drv->reset() || device->drv->init(device->int_priority))
			{
				err = VSFERR_FAIL;
				goto reset_exit;
			}
			
		#if VSFUSBD_CFG_AUTOSETUP
			if (vsfusbd_device_get_descriptor(device, device->desc_filter, 
											USB_DESC_TYPE_DEVICE, 0, 0, &desc)
		#if __VSF_DEBUG__
				|| (NULL == desc.buffer) || (desc.size != USB_DESC_SIZE_DEVICE)
				|| (desc.buffer[0] != desc.size) 
				|| (desc.buffer[1] != USB_DESC_TYPE_DEVICE)
				|| (device->num_of_configuration != 
										desc.buffer[USB_DESC_DEVICE_OFF_CFGNUM])
		#endif
				)
			{
				err = VSFERR_FAIL;
				goto reset_exit;
			}
			ep_size = desc.buffer[USB_DESC_DEVICE_OFF_EP0SIZE];
			device->ctrl_handler.ep_size = ep_size;
			
			// config ep0
			if (device->drv->prepare_buffer() || 
				device->drv->ep.set_type(0, USB_EP_TYPE_CONTROL) || 
				device->drv->ep.set_IN_epsize(0, ep_size) || 
				device->drv->ep.set_OUT_epsize(0, ep_size))
			{
				err = VSFERR_FAIL;
				goto reset_exit;
			}
		#endif	// VSFUSBD_CFG_AUTOSETUP
			
			if (device->callback.on_RESET != NULL)
			{
				device->callback.on_RESET();
			}
			
			if (vsfusbd_set_IN_handler(device, 0, vsfusbd_on_IN_do) || 
				vsfusbd_set_OUT_handler(device, 0, vsfusbd_on_OUT_do) ||
				device->drv->set_address(0))
			{
				err = VSFERR_FAIL;
				goto reset_exit;
			}
		reset_exit:
			// what to do if fail to process setup?
			break;
		}
	case VSFUSBD_INTEVT_SETUP:
		{
			struct vsfusbd_ctrl_handler_t *ctrl_handler = &device->ctrl_handler;
			struct usb_ctrl_request_t *request = &ctrl_handler->request;
			struct vsf_buffer_t buffer;
			uint8_t buff[USB_SETUP_PKG_SIZE];
			uint8_t* (*data_io)(void *param) = NULL;
			
			if (device->drv->get_setup(buff))
			{
				// fail to get setup request data
				err = VSFERR_FAIL;
				goto setup_exit;
			}
			memcpy(request, buff, USB_SETUP_PKG_SIZE);
			ctrl_handler->state		= USB_CTRL_STAT_SETTING_UP;
			ctrl_handler->filter	= vsfusbd_get_request_filter(device, 
														&ctrl_handler->iface);
			buffer.size				= 0;
			buffer.buffer			= NULL;
			
			if ((NULL == ctrl_handler->filter) || 
				((ctrl_handler->filter->prepare != NULL) && 
					ctrl_handler->filter->prepare(device, &buffer, data_io)))
			{
				err = VSFERR_FAIL;
				goto setup_exit;
			}
			if (buffer.size > request->length)
			{
				buffer.size = request->length;
			}
			
			if (USB_REQ_GET_DIR(request->type) == USB_REQ_DIR_HTOD)
			{
				if (0 == request->length)
				{
					vsfusbd_setup_status_callback((void *)device);
				}
				else
				{
					device->OUT_transact[0].tbuffer.buffer = buffer;
					device->OUT_transact[0].callback.param = device;
					device->OUT_transact[0].callback.callback =
											vsfusbd_setup_status_callback;
					device->OUT_transact[0].callback.data_io = data_io;
					device->OUT_transact[0].need_poll = false;
					err = vsfusbd_ep_receive_nb(device, 0);
				}
			}
			else
			{
				device->IN_transact[0].tbuffer.buffer = buffer;
				device->IN_transact[0].pkt.in.zlp =
											buffer.size < request->length;
				device->IN_transact[0].callback.param = device;
				device->IN_transact[0].callback.callback = NULL;
				device->IN_transact[0].callback.data_io = data_io;
				device->IN_transact[0].need_poll = false;
				err = vsfusbd_ep_send_nb(device, 0);
				if (!err)
				{
					vsfusbd_setup_status_callback((void*)device);
				}
			}
		
		setup_exit:
			if (err)
			{
				device->drv->ep.set_IN_stall(0);
				device->drv->ep.set_OUT_stall(0);
			}
			break;
		}
#if VSFUSBD_CFG_LP_EN
	case VSFUSBD_INTEVT_WAKEUP:
		if (device->callback.on_WAKEUP != NULL)
		{
			device->callback.on_WAKEUP();
		}
		break;
	case VSFUSBD_INTEVT_SUSPEND:
		if (device->callback.on_SUSPEND != NULL)
		{
			device->callback.on_SUSPEND();
		}
		device->drv->suspend();
		break;
	case VSFUSBD_INTEVT_RESUME:
		if (device->callback.on_RESUME != NULL)
		{
			device->callback.on_RESUME();
		}
		device->drv->resume();
		break;
#endif
	case VSFUSBD_INTEVT_SOF:
		if (device->callback.on_SOF != NULL)
		{
			device->callback.on_SOF();
		}
		break;
	case VSFUSBD_INTEVT_ATTACH:
		if (device->callback.on_ATTACH != NULL)
		{
			device->callback.on_ATTACH();
		}
		break;
	case VSFUSBD_INTEVT_DETACH:
		if (device->callback.on_DETACH != NULL)
		{
			device->callback.on_DETACH();
		}
		break;
	default:
		{
			uint8_t ep = evt & 0xF;
			struct vsfusbd_transact_t *transact = NULL;
			
			switch (evt & VSFUSBD_INTEVT_INOUT_MASK)
			{
			case VSFUSBD_INTEVT_IN:
				device->IN_handler[ep](device, ep);
				break;
			case VSFUSBD_INTEVT_OUT:
				device->OUT_handler[ep](device, ep);
				break;
			case VSFUSBD_EVT_DATAIO_IN:
				{
					transact = &device->IN_transact[ep];
					if (transact->need_poll)
					{
						struct vsf_transaction_buffer_t *tbuffer =
									&transact->tbuffer;
						uint16_t remain_size =
									tbuffer->buffer.size - tbuffer->position;
						
						if (remain_size)
						{
							uint8_t *buffer = vsfusbd_data_io(transact);
							
							if (buffer != NULL)
							{
								uint16_t ep_size =
											device->drv->ep.get_IN_epsize(ep);
								uint16_t pkg_size = min(remain_size, ep_size);
								
								device->drv->ep.write_IN_buffer(ep, buffer,
																pkg_size);
								device->drv->ep.set_IN_count(ep, pkg_size);
								tbuffer->position += pkg_size;
								transact->need_poll = false;
							}
						}
						else
						{
							device->drv->ep.set_IN_count(ep, 0);
							transact->need_poll = false;
						}
					}
					break;
				}
			case VSFUSBD_EVT_DATAIO_OUT:
				{
					transact = &device->OUT_transact[ep];
					if (transact->need_poll)
					{
						struct vsf_transaction_buffer_t *tbuffer =
									&transact->tbuffer;
						uint16_t remain_size =
									tbuffer->buffer.size - tbuffer->position;
						
						if ((remain_size > 0) &&
							(vsfusbd_data_io(transact) != NULL))
						{
							device->drv->ep.enable_OUT(ep);
							transact->need_poll = false;
						}
					}
					break;
				}
			default:
				if (((evt & VSFUSBD_INTEVT_ERR_MASK) == VSFUSBD_INTEVT_ERR) &&
					(device->callback.on_ERROR != NULL))
				{
					enum interface_usbd_error_t type =
							(enum interface_usbd_error_t)(evt & 0xFF);
					device->callback.on_ERROR(type);
				}
			}
			break;
		}
	}
	// top sm, all events are processed here
	return NULL;
}

// interrupts, simply send(pending) interrupt event to sm
static vsf_err_t vsfusbd_on_SETUP(void *p)
{
	struct vsfusbd_device_t *device = p;
	struct vsfsm_t *sm = &device->sm;
	return vsfsm_post_evt_pending(sm, VSFUSBD_INTEVT_SETUP);
}

static vsf_err_t vsfusbd_on_IN(void *p, uint8_t ep)
{
	struct vsfusbd_device_t *device = (struct vsfusbd_device_t *)p;
	struct vsfsm_t *sm = &device->sm;
	if ((ep < VSFUSBD_CFG_MAX_OUT_EP) && (device->OUT_handler[ep] != NULL))
	{
		return vsfsm_post_evt_pending(sm, VSFUSBD_INTEVT_INEP(ep));
	}
	return VSFERR_NOT_SUPPORT;
}

static vsf_err_t vsfusbd_on_OUT(void *p, uint8_t ep)
{
	struct vsfusbd_device_t *device = (struct vsfusbd_device_t *)p;
	struct vsfsm_t *sm = &device->sm;
	if ((ep < VSFUSBD_CFG_MAX_OUT_EP) && (device->OUT_handler[ep] != NULL))
	{
		return vsfsm_post_evt_pending(sm, VSFUSBD_INTEVT_OUTEP(ep));
	}
	return VSFERR_NOT_SUPPORT;
}

vsf_err_t vsfusbd_on_UNDERFLOW(void *p, uint8_t ep)
{
	return VSFERR_NONE;
}

vsf_err_t vsfusbd_on_OVERFLOW(void *p, uint8_t ep)
{
	return VSFERR_NONE;
}

vsf_err_t vsfusbd_on_RESET(void *p)
{
	struct vsfusbd_device_t *device = p;
	struct vsfsm_t *sm = &device->sm;
	return vsfsm_post_evt_pending(sm, VSFUSBD_INTEVT_RESET);
}

#if VSFUSBD_CFG_LP_EN
vsf_err_t vsfusbd_on_WAKEUP(void *p)
{
	struct vsfusbd_device_t *device = p;
	struct vsfsm_t *sm = &device->sm;
	return vsfsm_post_evt_pending(sm, VSFUSBD_INTEVT_WAKEUP);
}

vsf_err_t vsfusbd_on_SUSPEND(void *p)
{
	struct vsfusbd_device_t *device = p;
	struct vsfsm_t *sm = &device->sm;
	return vsfsm_post_evt_pending(sm, VSFUSBD_INTEVT_SUSPEND);
}

vsf_err_t vsfusbd_on_RESUME(void *p)
{
	struct vsfusbd_device_t *device = p;
	struct vsfsm_t *sm = &device->sm;
	return vsfsm_post_evt_pending(sm, VSFUSBD_INTEVT_RESUME);
}
#endif

vsf_err_t vsfusbd_on_SOF(void *p)
{
	struct vsfusbd_device_t *device = p;
	struct vsfsm_t *sm = &device->sm;
	return vsfsm_post_evt_pending(sm, VSFUSBD_INTEVT_SOF);
}

vsf_err_t vsfusbd_on_ATTACH(void *p)
{
	struct vsfusbd_device_t *device = p;
	struct vsfsm_t *sm = &device->sm;
	return vsfsm_post_evt_pending(sm, VSFUSBD_INTEVT_ATTACH);
}

vsf_err_t vsfusbd_on_DETACH(void *p)
{
	struct vsfusbd_device_t *device = p;
	struct vsfsm_t *sm = &device->sm;
	return vsfsm_post_evt_pending(sm, VSFUSBD_INTEVT_DETACH);
}

vsf_err_t vsfusbd_on_ERROR(void *p, enum interface_usbd_error_t type)
{
	struct vsfusbd_device_t *device = p;
	struct vsfsm_t *sm = &device->sm;
	return vsfsm_post_evt_pending(sm, VSFUSBD_INTEVT_ERR);
}

vsf_err_t vsfusbd_device_init(struct vsfusbd_device_t *device)
{
#if VSFUSBD_CFG_AUTOSETUP
	struct vsf_buffer_t desc = {NULL, 0};
	uint8_t i;
#endif
	
	device->configured = false;
	device->configuration = 0;
	device->feature = 0;
	
#if VSFUSBD_CFG_AUTOSETUP
	for (i = 0; i < device->num_of_configuration; i++)
	{
		if (vsfusbd_device_get_descriptor(device, device->desc_filter, 
									USB_DESC_TYPE_CONFIGURATION, i, 0, &desc)
#if __VSF_DEBUG__
			|| (NULL == desc.buffer) 
			|| (desc.size <= USB_DESC_SIZE_CONFIGURATION)
			|| (desc.buffer[0] != USB_DESC_SIZE_CONFIGURATION)
			|| (desc.buffer[1] != USB_DESC_TYPE_CONFIGURATION)
			|| (config->num_of_ifaces != desc.buffer[USB_DESC_CONFIG_OFF_IFNUM])
#endif
			)
		{
			return VSFERR_FAIL;
		}
		device->config[i].configuration_value = 
									desc.buffer[USB_DESC_CONFIG_OFF_CFGVAL];
	}
#endif	// VSFUSBD_CFG_AUTOSETUP
	
	// initialize callback for low level driver before initializing the hardware
	if (device->drv->callback != NULL)
	{
		device->drv->callback->param = (void *)device;
		device->drv->callback->on_attach = NULL;
		device->drv->callback->on_detach = NULL;
		device->drv->callback->on_reset = vsfusbd_on_RESET;
		device->drv->callback->on_setup = vsfusbd_on_SETUP;
		device->drv->callback->on_error = vsfusbd_on_ERROR;
#if VSFUSBD_CFG_LP_EN
		device->drv->callback->on_wakeup = vsfusbd_on_WAKEUP;
		device->drv->callback->on_suspend = vsfusbd_on_SUSPEND;
//		device->drv->callback->on_resume = vsfusbd_on_RESUME;
#endif
		device->drv->callback->on_sof = vsfusbd_on_SOF;
		device->drv->callback->on_underflow = vsfusbd_on_UNDERFLOW;
		device->drv->callback->on_overflow = vsfusbd_on_OVERFLOW;
		device->drv->callback->on_in = vsfusbd_on_IN;
		device->drv->callback->on_out = vsfusbd_on_OUT;
	}
	
	// state machine init
	memset(&device->sm, 0, sizeof(device->sm));
	device->sm.evtq.evt_buffer = device->evtq;
	device->sm.evtq.evt_buffer_num = dimof(device->evtq);
	device->sm.init_state.evt_handler = vsfusbd_evt_handler;
	device->sm.user_data = (void*)device;
	vsfsm_init(&device->sm, true);
	
	if (device->drv->init(device->int_priority) ||
		((device->callback.init != NULL) && device->callback.init()))
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

