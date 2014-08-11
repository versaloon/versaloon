#ifndef __USB_PROTOCOL_H_INCLUDED__
#define __USB_PROTOCOL_H_INCLUDED__

#include "vsfusbd_cfg.h"

#include "stack/usb/usb_common.h"
#include "stack/usb/device/vsfusbd.h"

#include "stack/usb/device/class/CDC/vsfusbd_CDCACM.h"
#include "stack/usb/device/class/HID/vsfusbd_HID.h"
#include "stack/usb/device/class/MSC/vsfusbd_MSC_BOT.h"

#include "vsfusbd_Versaloon.h"

extern struct vsfusbd_device_t usb_device;
extern volatile uint32_t rep_len;

vsf_err_t usb_protocol_init(void);
vsf_err_t usb_protocol_poll(void);

#endif	// __USB_PROTOCOL_H_INCLUDED__
