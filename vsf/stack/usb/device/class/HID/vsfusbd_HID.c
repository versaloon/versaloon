#include "app_cfg.h"
#include "interfaces.h"

#include "stack/usb/usb_common.h"
#include "stack/usb/device/vsfusbd.h"

#include "vsfusbd_HID.h"

static struct vsfusbd_HID_report_t* vsfusbd_HID_find_report(
		struct vsfusbd_HID_param_t *param, uint8_t type, uint8_t id)
{
	struct vsfusbd_HID_report_t *report = param->reports;
	uint8_t i;
	
	if (NULL == param)
	{
		return NULL;
	}
	
	for(i = 0; i < param->num_of_report; i++)
	{
		if ((param->reports[i].type == type) && (param->reports[i].id == id))
		{
			return report;
		}
	}
	
	return NULL;
}

static vsf_err_t vsfusbd_HID_class_update_report(
		struct vsfusbd_HID_report_t *report, struct vsf_buffer_t *temp_buffer)
{
	if ((NULL == report) || (NULL == temp_buffer))
	{
		return VSFERR_FAIL;
	}
	
	if (report->type == USB_HID_REPORT_TYPE_INPUT)
	{
		uint32_t size = report->buffer.size;
		
		if (NULL == report->on_set_get_report)
		{
			return VSFERR_FAIL;
		}
		
		memcpy(temp_buffer->buffer, report->buffer.buffer, size);
		report->lock = true;
		if (report->on_set_get_report(report))
		{
			report->lock = false;
			return VSFERR_FAIL;
		}
		report->lock = false;
	}
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_HID_OUT_hanlder(struct vsfusbd_device_t *device,
											uint8_t ep)
{
	struct interface_usbd_t *drv = device->drv;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	int8_t iface = config->ep_OUT_iface_map[ep];
	struct vsfusbd_HID_param_t *param;
	uint16_t pkg_size, ep_size;
	uint8_t buffer[64], *pbuffer = buffer;
	uint8_t report_id;
	struct vsfusbd_HID_report_t *report;
	
	if (iface < 0)
	{
		return VSFERR_FAIL;
	}
	param = (struct vsfusbd_HID_param_t *)config->iface[iface].protocol_param;
	if (NULL == param)
	{
		return VSFERR_FAIL;
	}
	
	ep_size = drv->ep.get_OUT_epsize(ep);
	pkg_size = drv->ep.get_OUT_count(ep);
	if (pkg_size > ep_size)
	{
		return VSFERR_FAIL;
	}
	drv->ep.read_OUT_buffer(ep, buffer, pkg_size);
	
	switch (param->output_state)
	{
	case HID_OUTPUT_STATE_WAIT:
		if (1 == param->num_of_OUTPUT_report)
		{
			report_id = 0;
		}
		else
		{
			report_id = buffer[0];
			pbuffer++;
			pkg_size--;
		}
		report = vsfusbd_HID_find_report(param, USB_HID_REPORT_OUTPUT, 
											report_id);
		if ((NULL == report) || (pkg_size > report->buffer.size))
		{
			return VSFERR_FAIL;
		}
		
		memcpy(report->buffer.buffer, pbuffer, pkg_size);
		if (pkg_size < report->buffer.size)
		{
			report->pos = pkg_size;
			param->output_state = HID_OUTPUT_STATE_RECEIVING;
			param->current_output_report_id = report_id;
		}
		else if (report->on_set_get_report != NULL)
		{
			report->on_set_get_report(report);
		}
		break;
	case HID_OUTPUT_STATE_RECEIVING:
		report_id = param->current_output_report_id;
		report = vsfusbd_HID_find_report(param, USB_HID_REPORT_OUTPUT, 
											report_id);
		if ((NULL == report) || 
			((pkg_size + report->pos) > report->buffer.size))
		{
			return VSFERR_FAIL;
		}
		
		memcpy(report->buffer.buffer + report->pos, pbuffer, pkg_size);
		report->pos += pkg_size;
		if (report->pos == report->buffer.size)
		{
			report->pos = 0;
			if (report->on_set_get_report != NULL)
			{
				report->on_set_get_report(report);
			}
			param->output_state = HID_OUTPUT_STATE_WAIT;
		}
		break;
	default:
		return VSFERR_NONE;
	}
	return drv->ep.enable_OUT(param->ep_out);
}

static vsf_err_t vsfusbd_HID_class_poll(uint8_t iface, 
										struct vsfusbd_device_t *device)
{
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_HID_param_t *param = 
		(struct vsfusbd_HID_param_t *)config->iface[iface].protocol_param;
	struct vsfusbd_HID_report_t *report;
	uint8_t ep = param->ep_in;
	struct vsfusbd_transact_t *transact = &device->IN_transact[ep];
	struct vsf_buffer_t *buffer = &transact->tbuffer.buffer;
	
	if (NULL == param)
	{
		return VSFERR_FAIL;
	}
	
	report = &param->reports[param->poll_report_idx];
	buffer->buffer = report->buffer.buffer;
	buffer->size = report->buffer.size;
	transact->callback.callback = NULL;
	transact->pkt.in.zlp = true;
	switch (report->type)
	{
	case USB_HID_REPORT_TYPE_INPUT:
		// TODO: process multi-report and idle, need 1 ms systick module
		if (!vsfusbd_ep_send_nb_isready(device, ep))
		{
			if (vsfusbd_HID_class_update_report(report, &param->temp_buffer))
			{
				return VSFERR_FAIL;
			}
			transact->tbuffer.buffer = param->temp_buffer;
			if (vsfusbd_ep_send_nb(device, ep))
			{
				return VSFERR_FAIL;
			}
			param->poll_report_idx++;
			if (param->poll_report_idx >= param->num_of_report)
			{
				param->poll_report_idx = 0;
			}
		}
		break;
	case USB_HID_REPORT_TYPE_OUTPUT:
		break;
	default:
		break;
	}
	
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_HID_class_init(uint8_t iface, 
										struct vsfusbd_device_t *device)
{
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_HID_param_t *param = 
		(struct vsfusbd_HID_param_t *)config->iface[iface].protocol_param;
	struct interface_usbd_t *drv = device->drv;
	uint8_t input_reports = 0, output_reports = 0, feature_reports = 0;
	uint8_t i;
	
	if ((param->ep_out != 0) && 
		(	vsfusbd_set_OUT_handler(device, param->ep_out,
												vsfusbd_HID_OUT_hanlder) ||
			drv->ep.enable_OUT(param->ep_out)))
	{
		return VSFERR_FAIL;
	}
	
	param->output_state = HID_OUTPUT_STATE_WAIT;
	param->poll_report_idx = 0;
	for(i = 0; i < param->num_of_report; i++)
	{
		param->reports[i].lock = false;
		param->reports[i].pos = 0;
		param->reports[i].idle_cnt = 0;
		switch (param->reports[i].type)
		{
		case USB_HID_REPORT_OUTPUT:
			output_reports++;
			break;
		case USB_HID_REPORT_INPUT:
			input_reports++;
			break;
		case USB_HID_REPORT_FEATURE:
			feature_reports++;
			break;
		}
	}
	param->num_of_INPUT_report = input_reports;
	param->num_of_OUTPUT_report = output_reports;
	param->num_of_FEATURE_report = feature_reports;
	return vsfusbd_HID_class_poll(iface, device);
}

static vsf_err_t vsfusbd_HID_GetReport_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	struct usb_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_HID_param_t *param = 
		(struct vsfusbd_HID_param_t *)config->iface[iface].protocol_param;
	uint8_t type = request->value >> 8, id = request->value;
	struct vsfusbd_HID_report_t *report = 
									vsfusbd_HID_find_report(param, type, id);
	
	if ((NULL == param) || (NULL == report) || (type != report->type))
	{
		return VSFERR_FAIL;
	}
	
	buffer->size = report->buffer.size;
	if (report->lock)
	{
		buffer->buffer = param->temp_buffer.buffer;
	}
	else
	{
		buffer->buffer = report->buffer.buffer;
	}
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_HID_GetIdle_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	struct usb_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t id = request->value;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_HID_param_t *param = 
		(struct vsfusbd_HID_param_t *)config->iface[iface].protocol_param;
	struct vsfusbd_HID_report_t *report = 
				vsfusbd_HID_find_report(param, USB_HID_REPORT_INPUT, id);
	
	if ((NULL == param) || (NULL == report) || (request->length != 1))
	{
		return VSFERR_FAIL;
	}
	
	buffer->size = 1;
	buffer->buffer = &report->idle;
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_HID_GetProtocol_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	struct usb_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_HID_param_t *param = 
		(struct vsfusbd_HID_param_t *)config->iface[iface].protocol_param;
	
	if ((NULL == param) || (request->value != 0) || (request->length != 1))
	{
		return VSFERR_FAIL;
	}
	
	buffer->size = 1;
	buffer->buffer = &param->protocol;
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_HID_SetReport_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	struct usb_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t type = request->value >> 8, id = request->value;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_HID_param_t *param = 
		(struct vsfusbd_HID_param_t *)config->iface[iface].protocol_param;
	struct vsfusbd_HID_report_t *report = 
									vsfusbd_HID_find_report(param, type, id);
	
	if ((NULL == param) || (NULL == report) || (type != report->type))
	{
		return VSFERR_FAIL;
	}
	
	buffer->size = report->buffer.size;
	buffer->buffer = report->buffer.buffer;
	return VSFERR_NONE;
}
static vsf_err_t vsfusbd_HID_SetReport_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct usb_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t type = request->value >> 8, id = request->value;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_HID_param_t *param = 
		(struct vsfusbd_HID_param_t *)config->iface[iface].protocol_param;
	struct vsfusbd_HID_report_t *report = 
									vsfusbd_HID_find_report(param, type, id);
	
	if ((NULL == param) || (NULL == report) || (type != report->type))
	{
		return VSFERR_FAIL;
	}
	
	if (report->on_set_get_report != NULL)
	{
		return report->on_set_get_report(report);
	}
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_HID_SetIdle_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	struct usb_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t id = request->value;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_HID_param_t *param = 
		(struct vsfusbd_HID_param_t *)config->iface[iface].protocol_param;
	struct vsfusbd_HID_report_t *report = 
				vsfusbd_HID_find_report(param, USB_HID_REPORT_INPUT, id);
	
	if ((NULL == param) || (NULL == report) || (request->length != 0))
	{
		return VSFERR_FAIL;
	}
	
	report->idle = request->value >> 8;
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_HID_SetProtocol_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	struct usb_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_HID_param_t *param = 
		(struct vsfusbd_HID_param_t *)config->iface[iface].protocol_param;
	
	if ((NULL == param) || (request->length != 1) || 
		((request->value != USB_HID_PROTOCOL_BOOT) && 
		 	(request->value != USB_HID_PROTOCOL_REPORT)))
	{
		return VSFERR_FAIL;
	}
	
	param->protocol = request->value;
	return VSFERR_NONE;
}

static const struct vsfusbd_setup_filter_t vsfusbd_HID_class_setup[] = 
{
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_HIDREQ_GET_REPORT,
		vsfusbd_HID_GetReport_prepare,
		NULL
	},
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_HIDREQ_GET_IDLE,
		vsfusbd_HID_GetIdle_prepare,
		NULL
	},
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_HIDREQ_GET_PROTOCOL,
		vsfusbd_HID_GetProtocol_prepare,
		NULL
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_HIDREQ_SET_REPORT,
		vsfusbd_HID_SetReport_prepare,
		vsfusbd_HID_SetReport_process
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_HIDREQ_SET_IDLE,
		vsfusbd_HID_SetIdle_prepare,
		NULL
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_HIDREQ_SET_PROTOCOL,
		vsfusbd_HID_SetProtocol_prepare,
		NULL
	},
	VSFUSBD_SETUP_NULL
};

vsf_err_t vsfusbd_HID_get_desc(struct vsfusbd_device_t *device, uint8_t type, 
			uint8_t index, uint16_t lanid, struct vsf_buffer_t *buffer)
{
	struct usb_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_HID_param_t *param = 
		(struct vsfusbd_HID_param_t *)config->iface[iface].protocol_param;
	
	if ((NULL == param) || (NULL == param->desc))
	{
		return VSFERR_FAIL;
	}
	
	return vsfusbd_device_get_descriptor(device, param->desc, type, index, 
											lanid, buffer);
}

const struct vsfusbd_class_protocol_t vsfusbd_HID_class = 
{
	vsfusbd_HID_get_desc, NULL,
	(struct vsfusbd_setup_filter_t *)vsfusbd_HID_class_setup, NULL,
	
	vsfusbd_HID_class_init, NULL, vsfusbd_HID_class_poll
};
