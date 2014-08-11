#include "app_cfg.h"
#include "interfaces.h"

#include "stack/usb/usb_common.h"
#include "stack/usb/device/vsfusbd.h"

#include "vsfusbd_CDC.h"

static vsf_err_t vsfusbd_CDCData_OUT_hanlder(struct vsfusbd_device_t *device,
												uint8_t ep)
{
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	int8_t iface = config->ep_OUT_iface_map[ep];
	struct vsfusbd_CDC_param_t *param = NULL;
	uint16_t pkg_size, ep_size;
	uint8_t buffer[64];
	struct vsf_buffer_t tx_buffer;
	
	if (iface < 0)
	{
		return VSFERR_FAIL;
	}
	param = (struct vsfusbd_CDC_param_t *)config->iface[iface].protocol_param;
	if (NULL == param)
	{
		return VSFERR_FAIL;
	}
	
	ep_size = device->drv->ep.get_OUT_epsize(ep);
	pkg_size = device->drv->ep.get_OUT_count(ep);
	if (pkg_size > ep_size)
	{
		return VSFERR_FAIL;
	}
	device->drv->ep.read_OUT_buffer(ep, buffer, pkg_size);
	if (param->out_enable)
	{
		device->drv->ep.enable_OUT(ep);
	}
	
	if (stream_get_free_size(param->stream_tx) < ep_size)
	{
		param->out_enable = false;
	}
	tx_buffer.buffer = buffer;
	tx_buffer.size = pkg_size;
	return (stream_tx(param->stream_tx, &tx_buffer) == tx_buffer.size) ?
				VSFERR_NONE : VSFERR_FAIL;
}

static vsf_err_t vsfusbd_CDCData_IN_hanlder(struct vsfusbd_device_t *device,
											uint8_t ep)
{
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	int8_t iface = config->ep_IN_iface_map[ep];
	struct vsfusbd_CDC_param_t *param = NULL;
	uint16_t pkg_size;
	uint8_t buffer[64];
	uint32_t rx_data_length;
	struct vsf_buffer_t rx_buffer;
	
	if (iface < 0)
	{
		return VSFERR_FAIL;
	}
	param = (struct vsfusbd_CDC_param_t *)config->iface[iface].protocol_param;
	if (NULL == param)
	{
		return VSFERR_FAIL;
	}
	
	pkg_size = device->drv->ep.get_IN_epsize(ep);
	rx_buffer.buffer = buffer;
	rx_buffer.size = pkg_size;
	rx_data_length = stream_rx(param->stream_rx, &rx_buffer);
	if (rx_data_length)
	{
		pkg_size = (rx_data_length > pkg_size) ? pkg_size : rx_data_length;
		device->drv->ep.write_IN_buffer(ep, buffer, pkg_size);
		device->drv->ep.set_IN_count(ep, pkg_size);
	}
	else
	{
		device->drv->ep.set_IN_count(ep, 0);
	}
	
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_CDCData_class_init(uint8_t iface, 
											struct vsfusbd_device_t *device)
{
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_CDC_param_t *param = 
		(struct vsfusbd_CDC_param_t *)config->iface[iface].protocol_param;
	
	if ((NULL == param) || 
		(NULL == param->stream_tx) || (NULL == param->stream_rx) || 
		vsfusbd_set_IN_handler(device, param->ep_in,
												vsfusbd_CDCData_IN_hanlder) ||
		device->drv->ep.set_IN_count(param->ep_in, 0) || 
		vsfusbd_set_OUT_handler(device, param->ep_out,
												vsfusbd_CDCData_OUT_hanlder))
	{
		return VSFERR_FAIL;
	}
	param->out_enable = false;
	
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_CDCData_class_poll(uint8_t iface, 
											struct vsfusbd_device_t *device)
{
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_CDC_param_t *param = 
		(struct vsfusbd_CDC_param_t *)config->iface[iface].protocol_param;
	
	if (NULL == param)
	{
		return VSFERR_FAIL;
	}
	
	if (!param->out_enable)
	{
		uint16_t ep_size = device->drv->ep.get_OUT_epsize(param->ep_out);
		
		if (stream_get_free_size(param->stream_tx) >= ep_size)
		{
			param->out_enable = true;
			device->drv->ep.enable_OUT(param->ep_out);
		}
	}
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_CDCControl_SendEncapsulatedCommand_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	struct usb_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_CDC_param_t *param = 
		(struct vsfusbd_CDC_param_t *)config->iface[iface].protocol_param;
	
	if (request->length > param->encapsulated_command_buffer.size)
	{
		return VSFERR_FAIL;
	}
	
	buffer->buffer = param->encapsulated_command_buffer.buffer;
	buffer->size = request->length;
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_CDCControl_SendEncapsulatedCommand_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct usb_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_CDC_param_t *param = 
		(struct vsfusbd_CDC_param_t *)config->iface[iface].protocol_param;
	
	if ((param->callback.send_encapsulated_command != NULL) &&
		param->callback.send_encapsulated_command(buffer))
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_CDCControl_GetEncapsulatedResponse_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	struct usb_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_CDC_param_t *param = 
		(struct vsfusbd_CDC_param_t *)config->iface[iface].protocol_param;
	
	if (request->length > param->encapsulated_response_buffer.size)
	{
		return VSFERR_FAIL;
	}
	
	buffer->buffer = param->encapsulated_response_buffer.buffer;
	buffer->size = request->length;
	return VSFERR_NONE;
}

static const struct vsfusbd_setup_filter_t vsfusbd_CDCControl_class_setup[] = 
{
/*	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_CDCREQ_SET_COMM_FEATURE,
		vsfusbd_CDCControl_SetCommFeature_prepare,
		NULL
	},
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_CDCREQ_GET_COMM_FEATURE,
		vsfusbd_CDCControl_GetCommFeature_prepare,
		NULL
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_CDCREQ_CLEAR_COMM_FEATURE,
		vsfusbd_CDCControl_ClearCommFeature_prepare,
		NULL
	},
*/	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_CDCREQ_SEND_ENCAPSULATED_COMMAND,
		vsfusbd_CDCControl_SendEncapsulatedCommand_prepare,
		vsfusbd_CDCControl_SendEncapsulatedCommand_process,
	},
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_CDCREQ_GET_ENCAPSULATED_RESPONSE,
		vsfusbd_CDCControl_GetEncapsulatedResponse_prepare,
		NULL,
	},
	VSFUSBD_SETUP_NULL
};

const struct vsfusbd_class_protocol_t vsfusbd_CDCControl_class = 
{
	NULL, NULL,
	(struct vsfusbd_setup_filter_t *)vsfusbd_CDCControl_class_setup, NULL,
	
	NULL, NULL, NULL
};

const struct vsfusbd_class_protocol_t vsfusbd_CDCData_class = 
{
	NULL, NULL, NULL, NULL,
	
	vsfusbd_CDCData_class_init, NULL, vsfusbd_CDCData_class_poll
};
