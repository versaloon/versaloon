#include "app_cfg.h"
#include "app_type.h"

#include "app_interfaces.h"

#include "vsf_usbd_cfg.h"
#include "stack/usb_device/vsf_usbd_const.h"
#include "stack/usb_device/vsf_usbd.h"

#include "vsfusbd_Versaloon.h"

#include "USB_TO_XXX.h"

#if STLINK_EN
#include "stlink.h"
#endif

uint8_t buffer_out[USB_DATA_BUFF_SIZE], asyn_rx_buf[ASYN_DATA_BUFF_SIZE];
volatile uint32_t count_out = 0;
volatile uint32_t usb_ovf = 0;
volatile uint32_t cmd_len = 0;

volatile uint32_t rep_len = 0;

static vsf_err_t Versaloon_OUT_hanlder(struct vsfusbd_device_t *device,
										uint8_t ep)
{
	uint32_t pkg_len;
#if VSFUSBD_CFG_DBUFFER_EN
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	int8_t iface = config->ep_OUT_iface_map[ep];
	struct vsfusbd_Versaloon_param_t *param = NULL;
	
	if (iface < 0)
	{
		return VSFERR_FAIL;
	}
	param = (struct vsfusbd_Versaloon_param_t *)config->iface[iface].protocol_param;
	if (NULL == param)
	{
		return VSFERR_FAIL;
	}
#endif

	if(cmd_len & 0x80000000)
	{
		usb_ovf = 1;
		count_out = 0;
	}
	
#if VSFUSBD_CFG_DBUFFER_EN
	if (param->dbuffer_en)
	{
		device->drv->ep.switch_OUT_buffer(ep);
	}
#endif
	pkg_len = device->drv->ep.get_OUT_count(ep);
	device->drv->ep.read_OUT_buffer(ep, buffer_out + count_out, pkg_len);
	device->drv->ep.enable_OUT(ep);
	
	if(pkg_len)
	{
		if(!count_out)
		{
			// first package
			if ((buffer_out[0] >= VERSALOON_COMMON_CMD_START) &&
				(buffer_out[0] <= VERSALOON_COMMON_CMD_END))
			{
				// Common Commands
				cmd_len = pkg_len;
			}
#if USB_TO_XXX_EN
			else if ((buffer_out[0] >= VERSALOON_USB_TO_XXX_CMD_START) &&
					(buffer_out[0] <= VERSALOON_USB_TO_XXX_CMD_END))
			{
				// USB_TO_XXX Support
				cmd_len = buffer_out[1] + ((uint16_t)buffer_out[2] << 8);
			}
#endif
#if STLINK_EN
			else if ((buffer_out[0] >= STLINK_CMD_START) &&
					(buffer_out[0] <= STLINK_CMD_END))
			{
				// STLINK
				cmd_len = stlink_getpkgsize(buffer_out, pkg_len);
			}
#endif
		}
		count_out += pkg_len;
		
		// all data received?
		pkg_len = cmd_len;
		if(count_out >= pkg_len)
		{
			cmd_len |= 0x80000000;
		}
	}
	
	return VSFERR_NONE;
}

static vsf_err_t versaloon_usb_init(uint8_t iface, struct vsfusbd_device_t *device)
{
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_Versaloon_param_t *param = 
		(struct vsfusbd_Versaloon_param_t *)config->iface[iface].protocol_param;
	
#if USB_TO_XXX_EN
	USB_TO_XXX_Init(asyn_rx_buf + 2048);
#endif
	
	if (vsfusbd_set_OUT_handler(device, param->ep_out, Versaloon_OUT_hanlder) ||
#if VSFUSBD_CFG_DBUFFER_EN
		(param->dbuffer_en && 
		(	device->drv->ep.set_IN_dbuffer(param->ep_in) || 
			device->drv->ep.set_OUT_dbuffer(param->ep_out))) ||
#endif
		device->drv->ep.enable_OUT(param->ep_out)
		)
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t versaloon_poll(uint8_t iface, struct vsfusbd_device_t *device)
{
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_Versaloon_param_t *param = 
		(struct vsfusbd_Versaloon_param_t *)config->iface[iface].protocol_param;
	
	if(cmd_len & 0x80000000)
	{
		// A valid USB package has been received
		LED_USB_ON();
		
		ProcessCommand(&buffer_out[0], cmd_len & 0xFFFF);
		if(rep_len > 0)
		{
			// indicate reply data is valid
			rep_len |= 0x80000000;
		}
		else
		{
			// no data to send, set cmd_len to 0
			cmd_len = 0;
		}
		count_out = 0;				// set USB receive pointer to 0
		
		if(rep_len & 0x80000000)	// there is valid data to be sent to PC
		{
			struct vsfusbd_transact_t *transact =
											&device->IN_transact[param->ep_in];
			struct vsf_buffer_t *buffer = &transact->tbuffer.buffer;
			
			buffer->buffer = buffer_out;
			buffer->size = rep_len & 0xFFFF;
			transact->pkt.in.zlp = (rep_len & VERSALOON_REP_ZLP) ? true : false;
			transact->callback.callback = NULL;
			vsfusbd_ep_send(device, param->ep_in);
			
			// reset command length and reply length for next command
			cmd_len = 0;
			rep_len = 0;
		}
		
		LED_USB_OFF();
	}
	
	return VSFERR_NONE;
}

const struct vsfusbd_class_protocol_t vsfusbd_Versaloon_class = 
{
	NULL, NULL, NULL, NULL,
	versaloon_usb_init,
	NULL, versaloon_poll
};
