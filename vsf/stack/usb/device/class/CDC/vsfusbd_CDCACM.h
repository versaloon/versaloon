#ifndef __VSFUSBD_CDCACM_H_INCLUDED__
#define __VSFUSBD_CDCACM_H_INCLUDED__

#include "vsfusbd_CDC.h"

struct vsfusbd_CDCACM_line_coding_t
{
	uint32_t bitrate;
	uint8_t stopbittype;
	uint8_t paritytype;
	uint8_t datatype;
};

#define USBCDCACM_CONTROLLINE_RTS			0x02
#define USBCDCACM_CONTROLLINE_DTR			0x01
#define USBCDCACM_CONTROLLINE_MASK			0x03

enum usb_CDCACM_req_t
{
	USB_CDCACMREQ_SET_LINE_CODING			= 0x20,
	USB_CDCACMREQ_GET_LINE_CODING			= 0x21,
	USB_CDCACMREQ_SET_CONTROL_LINE_STATE	= 0x22,
	USB_CDCACMREQ_SEND_BREAK				= 0x23,
};

extern const struct vsfusbd_class_protocol_t vsfusbd_CDCACMControl_class;
extern const struct vsfusbd_class_protocol_t vsfusbd_CDCACMData_class;

struct vsfusbd_CDCACM_param_t
{
	struct vsfusbd_CDC_param_t CDC_param;
	
	struct
	{
		vsf_err_t (*set_line_coding)(struct vsfusbd_CDCACM_line_coding_t *line_coding);
		vsf_err_t (*set_control_line)(uint8_t control_line);
		vsf_err_t (*get_control_line)(uint8_t *control_line);
		vsf_err_t (*send_break)(void);
	} callback;
	
	struct vsfusbd_CDCACM_line_coding_t line_coding;
	
	// no need to initialize below by user
	uint8_t control_line;
	uint8_t line_coding_buffer[7];
};

#endif	// __VSFUSBD_CDCACM_H_INCLUDED__
