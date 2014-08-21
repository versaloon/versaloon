#ifndef __VSFUSBD_HID_H_INCLUDED__
#define __VSFUSBD_HID_H_INCLUDED__

#include "framework/vsftimer/vsftimer.h"

enum usb_HID_description_type_t
{
	USB_HIDDESC_TYPE_HID					= 0x21,
	USB_HIDDESC_TYPE_REPORT					= 0x22,
	USB_HIDDESC_TYPE_PHYSICAL				= 0x23,
};

enum usb_HID_req_t
{
	USB_HIDREQ_GET_REPORT					= 0x01,
	USB_HIDREQ_GET_IDLE						= 0x02,
	USB_HIDREQ_GET_PROTOCOL					= 0x03,
	USB_HIDREQ_SET_REPORT					= 0x09,
	USB_HIDREQ_SET_IDLE						= 0x0A,
	USB_HIDREQ_SET_PROTOCOL					= 0x0B,
};

#define USB_HID_REPORT_TYPE_INPUT			1
#define USB_HID_REPORT_TYPE_OUTPUT			2
#define USB_HID_REPORT_TYPE_FEATURE			3

#define USB_HID_PROTOCOL_BOOT				0
#define USB_HID_PROTOCOL_REPORT				1

extern const struct vsfusbd_class_protocol_t vsfusbd_HID_class;

enum usb_HID_report_type_t
{
	USB_HID_REPORT_OUTPUT,
	USB_HID_REPORT_INPUT,
	USB_HID_REPORT_FEATURE,
};

struct vsfusbd_HID_report_t
{
	enum usb_HID_report_type_t type;
	uint8_t id;
	uint8_t idle;
	struct vsf_buffer_t buffer;
	vsf_err_t (*on_set_report)(struct vsfusbd_HID_report_t *report);
	bool changed;
	
	// private
	uint32_t pos;
	uint8_t idle_cnt;
};

#define VSFUSBD_DESC_HID_REPORT(ptr, size, func)			\
	{USB_HIDDESC_TYPE_REPORT, 0, 0, {(uint8_t*)(ptr), (size)}, (func)}

enum vsfusbd_HID_output_state_t
{
	HID_OUTPUT_STATE_WAIT,
	HID_OUTPUT_STATE_RECEIVING,
};

struct vsfusbd_HID_param_t
{
	uint8_t ep_out;
	uint8_t ep_in;
	
	struct vsfusbd_desc_filter_t *desc;
	
	uint8_t num_of_report;
	struct vsfusbd_HID_report_t *reports;
	
	// private
	uint8_t protocol;
	
	enum vsfusbd_HID_output_state_t output_state;
	uint8_t current_output_report_id;
	
	uint8_t num_of_INPUT_report;
	uint8_t num_of_OUTPUT_report;
	uint8_t num_of_FEATURE_report;
	
	struct vsftimer_timer_t timer4ms;
	struct vsfusbd_device_t *device;
	struct vsfusbd_iface_t *iface;
	bool busy;
};

vsf_err_t vsfusbd_HID_IN_report_changed(struct vsfusbd_HID_param_t *param,
										struct vsfusbd_HID_report_t *report);

#endif	// __VSFUSBD_HID_H_INCLUDED__
