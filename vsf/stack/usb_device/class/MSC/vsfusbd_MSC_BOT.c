#include "compiler.h"
#include "interfaces.h"

#include "stack/usb_device/vsf_usbd_const.h"
#include "stack/usb_device/vsf_usbd.h"

#include "vsfusbd_MSC_BOT.h"

#define VSFUSBD_MSCBOT_STALL_IN					(1 << 0)
#define VSFUSBD_MSCBOT_STALL_OUT				(1 << 1)
#define VSFUSBD_MSCBOT_STALL_BOTH				\
							VSFUSBD_MSCBOT_STALL_IN | VSFUSBD_MSCBOT_STALL_OUT

static struct vsf_buffer_t *vsfusbd_MSCBOT_GetBuffer(
		struct vsfusbd_MSCBOT_param_t *param)
{
	return &param->page_buffer[++param->tick_tock & 1];
}

static uint16_t vsfusbd_MSCBOT_GetInPkgSize(struct interface_usbd_t *drv, 
											uint8_t ep, uint32_t size)
{
	uint16_t epsize = drv->ep.get_IN_epsize(ep);
	
	if (epsize > size)
	{
		return size;
	}
	else
	{
		return epsize;
	}
}

static vsf_err_t vsfusbd_MSCBOT_IN_hanlder(struct vsfusbd_device_t *device,
											uint8_t ep);
static vsf_err_t vsfusbd_MSCBOT_SendCSW(struct vsfusbd_device_t *device, 
							struct vsfusbd_MSCBOT_param_t *param)
{
	struct interface_usbd_t *drv = device->drv;
	uint8_t CSW_buffer[USBMSC_CSW_SIZE];
	
	param->poll = false;
	
	SET_LE_U32(&CSW_buffer[0], USBMSC_CSW_SIGNATURE);
	SET_LE_U32(&CSW_buffer[4], param->CBW.dCBWTag);
	SET_LE_U32(&CSW_buffer[8], 
		param->CBW.dCBWDataTransferLength - 
		(param->page_size * param->cur_usb_page + param->tbuffer.position));
	CSW_buffer[12] = param->dCSWStatus;
	param->cur_usb_page = param->tbuffer.position = 0;
	param->page_size = USBMSC_CSW_SIZE;
	param->page_num = 1;
	param->tbuffer.buffer.buffer = CSW_buffer;
	param->bot_status = VSFUSBD_MSCBOT_STATUS_CSW;
	
	drv->ep.write_IN_buffer(param->ep_in, CSW_buffer, USBMSC_CSW_SIZE);
	drv->ep.set_IN_count(param->ep_in, USBMSC_CSW_SIZE);
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_MSCBOT_ErrHandler(struct vsfusbd_device_t *device, 
			struct vsfusbd_MSCBOT_param_t *param,  uint8_t error)
{
	param->dCSWStatus = error;
	
	// OUT:	NACK(don't enable_OUT, if OUT is enabled, it will be disabled after data is sent)
	// IN:	if (dCBWDataTransferLength > 0)
	// 		  send_ZLP
	// 		send_CSW
	if (((param->CBW.bmCBWFlags & USBMSC_CBWFLAGS_DIR_MASK) == USBMSC_CBWFLAGS_DIR_IN) &&
		(param->CBW.dCBWDataTransferLength > 0))
	{
		param->bot_status = VSFUSBD_MSCBOT_STATUS_IN;
		param->page_size = 0;
		param->page_num = 1;
		param->tbuffer.position = 0;
		device->drv->ep.set_IN_count(param->ep_in, 0);
	}
	
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_MSCBOT_IN_hanlder(struct vsfusbd_device_t *device,
											uint8_t ep)
{
	struct interface_usbd_t *drv = device->drv;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	int8_t iface = config->ep_OUT_iface_map[ep];
	struct vsfusbd_MSCBOT_param_t *param = NULL;
	uint16_t pkg_size;
	uint32_t remain_size;
	uint8_t *pbuffer;
	
	if (iface < 0)
	{
		return VSFERR_FAIL;
	}
	param = (struct vsfusbd_MSCBOT_param_t *)config->iface[iface].protocol_param;
	if (NULL == param)
	{
		return VSFERR_FAIL;
	}
	
	switch (param->bot_status)
	{
	case VSFUSBD_MSCBOT_STATUS_ERROR:
	case VSFUSBD_MSCBOT_STATUS_CSW:
		param->bot_status = VSFUSBD_MSCBOT_STATUS_IDLE;
		drv->ep.enable_OUT(param->ep_out);
		break;
	case VSFUSBD_MSCBOT_STATUS_IN:
		remain_size = param->page_size - param->tbuffer.position;
		pbuffer = &param->tbuffer.buffer.buffer[param->tbuffer.position];
		if (remain_size)
		{
			pkg_size = vsfusbd_MSCBOT_GetInPkgSize(drv, ep, remain_size);
			drv->ep.write_IN_buffer(param->ep_in, pbuffer, pkg_size);
			drv->ep.set_IN_count(param->ep_in, pkg_size);
			param->tbuffer.position += pkg_size;
			return VSFERR_NONE;
		}
		
		param->tbuffer.position = 0;
		if (++param->cur_usb_page >= param->page_num)
		{
			return vsfusbd_MSCBOT_SendCSW(device, param);
		}
		
		if (param->cur_scsi_page > param->cur_usb_page)
		{
			param->idle = false;
			param->tbuffer.buffer = *vsfusbd_MSCBOT_GetBuffer(param);
			return vsfusbd_MSCBOT_IN_hanlder(device, ep);
		}
		else
		{
			param->idle = true;
		}
		break;
	default:
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_MSCBOT_OUT_hanlder(struct vsfusbd_device_t *device,
											uint8_t ep)
{
	struct interface_usbd_t *drv = device->drv;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	int8_t iface = config->ep_OUT_iface_map[ep];
	struct vsfusbd_MSCBOT_param_t *param = NULL;
	struct SCSI_LUN_info_t *lun_info = NULL;
	uint16_t pkg_size, ep_size;
	uint8_t buffer[64], *pbuffer;
	
	if (iface < 0)
	{
		return VSFERR_FAIL;
	}
	param = (struct vsfusbd_MSCBOT_param_t *)config->iface[iface].protocol_param;
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
	
	switch (param->bot_status)
	{
	case VSFUSBD_MSCBOT_STATUS_IDLE:
		memcpy(&param->CBW, buffer, sizeof(param->CBW));
		
		if ((param->CBW.dCBWSignature != USBMSC_CBW_SIGNATURE) || 
			(param->CBW.bCBWLUN > param->max_lun) || (pkg_size != 31))
		{
			// TODO: invalid CBW, how to process this error?
			return VSFERR_FAIL;
		}
		lun_info = &param->lun_info[param->CBW.bCBWLUN];
		if ((param->CBW.bCBWCBLength < 1) || (param->CBW.bCBWCBLength > 16))
		{
			// TODO: invalid CB length, how to process this error?
			return VSFERR_FAIL;
		}
		param->page_size = param->page_num = 0;
		param->cur_handlers = NULL;
		param->tbuffer.buffer = *vsfusbd_MSCBOT_GetBuffer(param);
		param->dCSWStatus = USBMSC_CSW_OK;
		if (SCSI_Handle(param->cur_handlers, lun_info, 
					param->CBW.CBWCB, &param->tbuffer.buffer, &param->page_size, 
					&param->page_num))
		{
			enum SCSI_errcode_t errcode = SCSI_GetErrorCode();
			if (SCSI_ERRCODE_INVALID_COMMAND == errcode)
			{
				param->cur_handlers = param->user_handlers;
				if (NULL == param->cur_handlers)
				{
					vsfusbd_MSCBOT_ErrHandler(device, param, USBMSC_CSW_FAIL);
					return VSFERR_FAIL;
				}
				else if (SCSI_Handle(param->cur_handlers, lun_info, 
						param->CBW.CBWCB, &param->tbuffer.buffer, 
						&param->page_size, &param->page_num))
				{
					errcode = SCSI_GetErrorCode();
					if (SCSI_ERRCODE_INVALID_COMMAND == errcode)
					{
						vsfusbd_MSCBOT_ErrHandler(device, param, USBMSC_CSW_FAIL);
						return VSFERR_FAIL;
					}
					return vsfusbd_MSCBOT_ErrHandler(device, param, USBMSC_CSW_FAIL);
				}
			}
			return vsfusbd_MSCBOT_ErrHandler(device, param, USBMSC_CSW_FAIL);
		}
		
		param->tbuffer.position = param->cur_usb_page = param->cur_scsi_page = 0;
		if (param->CBW.dCBWDataTransferLength)
		{
			if ((param->CBW.bmCBWFlags & USBMSC_CBWFLAGS_DIR_MASK) == 
						USBMSC_CBWFLAGS_DIR_IN)
			{
				param->bot_status = VSFUSBD_MSCBOT_STATUS_IN;
				if (param->tbuffer.buffer.size)
				{
					return vsfusbd_MSCBOT_IN_hanlder(device, param->ep_in);
				}
				else
				{
					param->poll = true;
					param->idle = true;
				}
			}
			else
			{
				param->bot_status = VSFUSBD_MSCBOT_STATUS_OUT;
				param->poll = true;
				param->idle = false;
				return drv->ep.enable_OUT(param->ep_out);
			}
		}
		else
		{
			return vsfusbd_MSCBOT_SendCSW(device, param);
		}
		break;
	case VSFUSBD_MSCBOT_STATUS_OUT:
		if (param->cur_usb_page >= param->page_num)
		{
			vsfusbd_MSCBOT_ErrHandler(device, param, USBMSC_CSW_PHASE_ERROR);
			lun_info->status.sense_key = SCSI_SENSEKEY_ILLEGAL_REQUEST;
			lun_info->status.asc = SCSI_ASC_INVALID_FIELED_IN_COMMAND;
			return VSFERR_FAIL;
		}
		
		lun_info = &param->lun_info[param->CBW.bCBWLUN];
		pbuffer = &param->tbuffer.buffer.buffer[param->tbuffer.position];
		if ((pkg_size + param->tbuffer.position) <= param->page_size)
		{
			memcpy(pbuffer, buffer, pkg_size);
			param->tbuffer.position += pkg_size;
			if (param->tbuffer.position < param->page_size)
			{
				drv->ep.enable_OUT(param->ep_out);
				return VSFERR_NONE;
			}
			
			param->tbuffer.position = 0;
			param->cur_usb_page++;
			if ((param->cur_usb_page - param->cur_scsi_page) < 2)
			{
				param->idle = false;
				param->tbuffer.buffer = *vsfusbd_MSCBOT_GetBuffer(param);
				drv->ep.enable_OUT(param->ep_out);
			}
			else
			{
				param->idle = true;
			}
		}
		else
		{
			vsfusbd_MSCBOT_ErrHandler(device, param, USBMSC_CSW_PHASE_ERROR);
			lun_info->status.sense_key = SCSI_SENSEKEY_ILLEGAL_REQUEST;
			lun_info->status.asc = SCSI_ASC_INVALID_FIELED_IN_COMMAND;
			return VSFERR_FAIL;
		}
		break;
	default:
		vsfusbd_MSCBOT_ErrHandler(device, param, USBMSC_CSW_PHASE_ERROR);
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_MSCBOT_class_init(uint8_t iface, 
											struct vsfusbd_device_t *device)
{
	struct interface_usbd_t *drv = device->drv;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_MSCBOT_param_t *param = 
		(struct vsfusbd_MSCBOT_param_t *)config->iface[iface].protocol_param;
	uint8_t i;
	
	if ((NULL == param) || 
		// minium ep size of MSCBOT is 32 bytes, 
		// so that CBW and CSW can be transfered in one package
		(drv->ep.get_IN_epsize(param->ep_in) < 32) || 
		(drv->ep.get_OUT_epsize(param->ep_in) < 32) || 
		vsfusbd_set_IN_handler(device, param->ep_in,
											vsfusbd_MSCBOT_IN_hanlder) || 
		vsfusbd_set_OUT_handler(device, param->ep_out,
											vsfusbd_MSCBOT_OUT_hanlder) || 
		drv->ep.enable_OUT(param->ep_out))
	{
		return VSFERR_FAIL;
	}
	
	param->idle = true;
	param->poll = false;
	param->bot_status = VSFUSBD_MSCBOT_STATUS_IDLE;
	for (i = 0; i <= param->max_lun; i++)
	{
		SCSI_Init(&param->lun_info[i]);
	}
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_MSCBOT_class_poll(uint8_t iface, 
											struct vsfusbd_device_t *device)
{
	struct interface_usbd_t *drv = device->drv;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_MSCBOT_param_t *param = 
		(struct vsfusbd_MSCBOT_param_t *)config->iface[iface].protocol_param;
	enum vsfusbd_MSCBOT_status_t bot_status = param->bot_status;
	uint8_t i;
	
	if (NULL == param)
	{
		return VSFERR_FAIL;
	}
	
	for (i = 0; i <= param->max_lun; i++)
	{
		SCSI_Poll(&param->lun_info[i]);
	}
	
	if (!param->poll)
	{
		return VSFERR_NONE;
	}
	if (((VSFUSBD_MSCBOT_STATUS_IN == bot_status) && 
			(param->cur_scsi_page < param->page_num) && 
			((param->cur_scsi_page - param->cur_usb_page) < 2)) || 
		((VSFUSBD_MSCBOT_STATUS_OUT == bot_status) && 
			(param->cur_scsi_page < param->page_num) && 
			(param->cur_usb_page > param->cur_scsi_page)))
	{
		struct SCSI_LUN_info_t *lun_info = &param->lun_info[param->CBW.bCBWLUN];
		uint8_t index = (param->tick_tock + 1) & 1;
		struct vsf_buffer_t *buffer = &param->page_buffer[index];
		vsf_err_t err;
		
		if (USBMSC_CSW_OK == param->dCSWStatus)
		{
			err = SCSI_IO(param->cur_handlers, lun_info, param->CBW.CBWCB,
							buffer, param->cur_scsi_page);
			if (err != VSFERR_NONE)
			{
				if (err > 0)
				{
					// not failure here
					return VSFERR_NONE;
				}
				return vsfusbd_MSCBOT_ErrHandler(device, param, USBMSC_CSW_FAIL);
			}
		}
		param->cur_scsi_page++;
		
		if (VSFUSBD_MSCBOT_STATUS_IN == bot_status)
		{
			if (param->idle)
			{
				param->idle = false;
				param->tbuffer.buffer = *vsfusbd_MSCBOT_GetBuffer(param);
				vsfusbd_MSCBOT_IN_hanlder((void *)device, param->ep_in);
			}
		}
		else if (VSFUSBD_MSCBOT_STATUS_OUT == bot_status)
		{
			if (param->cur_scsi_page >= param->page_num)
			{
				return vsfusbd_MSCBOT_SendCSW(device, param);
			}
			if (param->idle)
			{
				param->idle = false;
				param->tbuffer.buffer = *vsfusbd_MSCBOT_GetBuffer(param);
				return drv->ep.enable_OUT(param->ep_out);
			}
		}
	}
	
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_MSCBOT_GetMaxLun_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	uint8_t iface = request->index;
	struct vsfusbd_MSCBOT_param_t *param = 
		(struct vsfusbd_MSCBOT_param_t *)config->iface[iface].protocol_param;
	
	if ((NULL == param) || (request->length != 1) || (request->value != 0))
	{
		return VSFERR_FAIL;
	}
	
	buffer->buffer = &param->max_lun;
	buffer->size = 1;
	
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_MSCBOT_Reset_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer,
		uint8_t* (*data_io)(void *param))
{
	struct interface_usbd_t *drv = device->drv;
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	uint8_t iface = request->index;
	struct vsfusbd_MSCBOT_param_t *param = 
		(struct vsfusbd_MSCBOT_param_t *)config->iface[iface].protocol_param;
	
	if ((NULL == param) || (request->length != 0) || (request->value != 0) || 
		drv->ep.reset_IN_toggle(param->ep_in) || 
		drv->ep.reset_OUT_toggle(param->ep_out) || 
		drv->ep.enable_OUT(param->ep_out))
	{
		return VSFERR_FAIL;
	}
	
	return VSFERR_NONE;
}

static const struct vsfusbd_setup_filter_t vsfusbd_MSCBOT_class_setup[] = 
{
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_MSCBOTREQ_GET_MAX_LUN,
		vsfusbd_MSCBOT_GetMaxLun_prepare,
		NULL
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_MSCBOTREQ_RESET,
		vsfusbd_MSCBOT_Reset_prepare,
		NULL
	},
	VSFUSBD_SETUP_NULL
};

const struct vsfusbd_class_protocol_t vsfusbd_MSCBOT_class = 
{
	NULL, NULL,
	(struct vsfusbd_setup_filter_t *)vsfusbd_MSCBOT_class_setup, NULL,
	
	vsfusbd_MSCBOT_class_init, NULL, vsfusbd_MSCBOT_class_poll
};
