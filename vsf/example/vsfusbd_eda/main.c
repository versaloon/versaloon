#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "compiler.h"
#include "app_cfg.h"
#include "app_type.h"

#include "interfaces.h"
#include "framework/vsfsm/vsfsm.h"
#include "framework/vsftimer/vsftimer.h"

#include "stack/usb/device/vsfusbd.h"
#include "stack/usb/device/class/HID/vsfusbd_HID.h"

// USB descriptors
static const uint8_t HID_DeviceDescriptor[] =
{
	0x12,	// bLength = 18
	USB_DESC_TYPE_DEVICE,	// USB_DESC_TYPE_DEVICE
	0x00,
	0x02,	// bcdUSB
	0x00,	// device class
	0x00,	// device sub class
	0x00,	// device protocol
	0x40,	// max packet size
	0x83,
	0x04,	// vendor
	0x10,
	0x57,	// product
	0x00,
	0x02,	// bcdDevice
	1,	// manu facturer
	2,	// product
	3,	// serial number
	0x01	// number of configuration 
};

static const uint8_t HID_ConfigDescriptor[] =
{
	// Configuation Descriptor
	0x09,	// bLength: Configuation Descriptor size
	USB_DESC_TYPE_CONFIGURATION,
			// bDescriptorType: Configuration
	34,		// wTotalLength:no of returned bytes*
	0x00,
	0x01,	// bNumInterfaces: 1 interface
	0x01,	// bConfigurationValue: Configuration value
	0x00,	// iConfiguration: Index of string descriptor describing the configuration
	0x80,	// bmAttributes: bus powered
	0x64,	// MaxPower 200 mA

	// Interface Descriptor
	0x09,	// bLength: Interface Descriptor size
	USB_DESC_TYPE_INTERFACE,	// bDescriptorType:
	0x00,	// bInterfaceNumber: Number of Interface
	0x00,	// bAlternateSetting: Alternate setting
	0x01,	// bNumEndpoints
	0x03,	// bInterfaceClass
	0x01,	// bInterfaceSubClass : 1=BOOT, 0=no boot*
	0x01,	// nInterfaceProtocol : 0=none, 1=keyboard, 2=mouse
	0x00,	// iInterface:
	
	// HID Descriptor
	0x09,
	USB_HIDDESC_TYPE_HID,	// 0x21 == HID Discriptor
	0x00,
	0x01,	// HID BCD ID
	0x00,	//Country COde*
	0x01,	// Number of Descriptors
	USB_HIDDESC_TYPE_REPORT,	// Descriptor Type*
	63,
	0x00,	// Descriptor Length
	
	// Endpoint 1 Descriptor
	0x07,	// Endpoint descriptor length = 7
	USB_DESC_TYPE_ENDPOINT,	// Endpoint descriptor type
	0x81,	// Endpoint address (IN, address 1)
	0x03,	// interrupt endpoint type
	0x40,	// Maximum packet size (64 bytes)
	0x00,
	0x20	// Polling interval in milliseconds
};

static const uint8_t HID_ReportDescriptor[63] = {
    0x05, 0x01, // USAGE_PAGE (Generic Desktop)
    0x09, 0x06, // USAGE (Keyboard)
    0xa1, 0x01, // COLLECTION (Application)
    0x05, 0x07, // USAGE_PAGE (Keyboard)
    0x19, 0xe0, // USAGE_MINIMUM (Keyboard LeftControl)
    0x29, 0xe7, // USAGE_MAXIMUM (Keyboard Right GUI)
    0x15, 0x00, // LOGICAL_MINIMUM (0)
    0x25, 0x01, // LOGICAL_MAXIMUM (1)
    0x75, 0x01, // REPORT_SIZE (1)
    0x95, 0x08, // REPORT_COUNT (8)
    0x81, 0x02, // INPUT (Data,Var,Abs)
    0x95, 0x01, // REPORT_COUNT (1)
    0x75, 0x08, // REPORT_SIZE (8)
    0x81, 0x03, // INPUT (Cnst,Var,Abs)
    0x95, 0x05, // REPORT_COUNT (5)
    0x75, 0x01, // REPORT_SIZE (1)
    0x05, 0x08, // USAGE_PAGE (LEDs)
    0x19, 0x01, // USAGE_MINIMUM (Num Lock)
    0x29, 0x05, // USAGE_MAXIMUM (Kana)
    0x91, 0x02, // OUTPUT (Data,Var,Abs)
    0x95, 0x01, // REPORT_COUNT (1)
    0x75, 0x03, // REPORT_SIZE (3)
    0x91, 0x03, // OUTPUT (Cnst,Var,Abs) 
    0x95, 0x06, // REPORT_COUNT (6)
    0x75, 0x08, // REPORT_SIZE (8)
    0x15, 0x00, // LOGICAL_MINIMUM (0)
    0x25, 0xFF, // LOGICAL_MAXIMUM (255)
    0x05, 0x07, // USAGE_PAGE (Keyboard)
    0x19, 0x00, // USAGE_MINIMUM (Reserved (no event indicated))
    0x29, 0x65, // USAGE_MAXIMUM (Keyboard Application)
    0x81, 0x00, // INPUT (Data,Ary,Abs)
    0xc0 // END_COLLECTION
};

static const uint8_t HID_StringLangID[] =
{
	4,
	USB_DESC_TYPE_STRING,
	0x09,
	0x04
};

static const uint8_t HID_StringVendor[] =
{
	38,
	USB_DESC_TYPE_STRING,
	'N', 0, 'U', 0, 'C', 0, '4', 0, '0', 0, '0', 0, '.', 0, '.', 0,
	'.', 0, '.', 0, '.', 0, '.', 0, '.', 0, '.', 0, '.', 0, '.', 0,
	'.', 0, '.', 0
};

static const uint8_t HID_StringSerial[50] =
{
	50,
	USB_DESC_TYPE_STRING,
	'0', 0, '1', 0, '2', 0, '3', 0, '4', 0, '5', 0, '6', 0, '7', 0, 
	'8', 0, '9', 0, 'A', 0, 'B', 0, 'C', 0, 'D', 0, 'E', 0, 'F', 0, 
	'0', 0, '0', 0, '0', 0, '0', 0, '0', 0, '0', 0, '0', 0, '0', 0, 
};

static const uint8_t HID_StringProduct[] =
{
	16,
	USB_DESC_TYPE_STRING,
	'M', 0, 'O', 0, 'U', 0, 'S', 0, 'E', 0, '.', 0, '.', 0
};

static const struct vsfusbd_desc_filter_t HID_descriptors[] = 
{
	VSFUSBD_DESC_DEVICE(0, HID_DeviceDescriptor, sizeof(HID_DeviceDescriptor), NULL),
	VSFUSBD_DESC_CONFIG(0, 0, HID_ConfigDescriptor, sizeof(HID_ConfigDescriptor), NULL),
	VSFUSBD_DESC_STRING(0, 0, HID_StringLangID, sizeof(HID_StringLangID), NULL),
	VSFUSBD_DESC_STRING(0x0409, 1, HID_StringVendor, sizeof(HID_StringVendor), NULL),
	VSFUSBD_DESC_STRING(0x0409, 2, HID_StringProduct, sizeof(HID_StringProduct), NULL),
	VSFUSBD_DESC_STRING(0x0409, 3, HID_StringSerial, sizeof(HID_StringSerial), NULL),
	VSFUSBD_DESC_NULL
};

static const struct vsfusbd_desc_filter_t HID_Report_Descriptors[] =
{
    	VSFUSBD_DESC_HID_REPORT(HID_ReportDescriptor, sizeof(HID_ReportDescriptor), NULL),
		VSFUSBD_DESC_NULL
};

static vsf_err_t HID_on_set_get_report(struct vsfusbd_HID_report_t *report)
{
	return VSFERR_NONE;
}

// app state machine events
#define APP_EVT_USBPU_TO				VSFSM_EVT_USER_LOCAL_INSTANT + 0

static struct vsfsm_state_t *
app_evt_handler(struct vsfsm_t *sm, vsfsm_evt_t evt);

struct vsfapp_t
{
	// hw
	struct usb_pullup_port_t
	{
		uint8_t port;
		uint8_t pin;
	} usb_pullup;
	
	struct usbd_hid_t
	{
		struct vsfusbd_HID_param_t HID_param;
		struct vsfusbd_HID_report_t HID_reports[1];
		struct vsfusbd_iface_t ifaces[1];
		struct vsfusbd_config_t config[1];
		struct vsfusbd_device_t device;
		// private
		uint8_t HID_report0_buffer[8];
	} usbd_hid;
	
	struct vsfsm_t sm;
	struct vsftimer_timer_t usbpu_timer;
} static app =
{
	{
		2,						// uint8_t port;
		13,						// uint8_t pin;
	},							// struct usb_pullup_port_t usb_pullup;
	{
		{
			1, 1,
			(struct vsfusbd_desc_filter_t *)HID_Report_Descriptors,
			dimof(app.usbd_hid.HID_reports),
			(struct vsfusbd_HID_report_t *)&app.usbd_hid.HID_reports,
		},						// struct vsfusbd_HID_param_t HID_param;
		{
			{USB_HID_REPORT_INPUT, 1, NULL,
				{app.usbd_hid.HID_report0_buffer,
					sizeof(app.usbd_hid.HID_report0_buffer)},
				HID_on_set_get_report},
		},						// struct vsfusbd_HID_report_t HID_reports[1];
		{
			{(struct vsfusbd_class_protocol_t *)&vsfusbd_HID_class,
				(void *)&app.usbd_hid.HID_param},
		},						// struct vsfusbd_iface_t ifaces[1];
		{
			{NULL, NULL, dimof(app.usbd_hid.ifaces),
						(struct vsfusbd_iface_t *)app.usbd_hid.ifaces},
		},						// struct vsfusbd_config_t config[1];
		{
			dimof(app.usbd_hid.config),
			(struct vsfusbd_config_t *)app.usbd_hid.config,
			(struct vsfusbd_desc_filter_t *)HID_descriptors,
			0,
			(struct interface_usbd_t *)&core_interfaces.usbd,
		},						// struct vsfusbd_device_t device;
	},							// struct usbd_hid_t usbd_hid;
	{
		{NULL, 0},				// struct vsfsm_evtqueue_t evtq;
		{app_evt_handler},		// struct vsfsm_state_t init_state;
	},							// struct vsfsm_t sm;
	{
		200,					// uint32_t interval;
		&app.sm,				// struct vsfsm_t *sm;
		APP_EVT_USBPU_TO,		// vsfsm_evt_t evt;
	},							// struct vsftimer_timer_t usbpu_timer;
};

static struct vsfsm_state_t *
app_evt_handler(struct vsfsm_t *sm, vsfsm_evt_t evt)
{
	switch (evt)
	{
	case VSFSM_EVT_INIT:
		if (app.usb_pullup.port != IFS_DUMMY_PORT)
		{
			interfaces->gpio.init(app.usb_pullup.port);
			interfaces->gpio.clear(app.usb_pullup.port,
									1 << app.usb_pullup.pin);
			interfaces->gpio.config_pin(app.usb_pullup.port, app.usb_pullup.pin,
										GPIO_OUTPP);
		}
		app.usbd_hid.device.drv->disconnect();
		vsftimer_register(&app.usbpu_timer);
		break;
	case APP_EVT_USBPU_TO:
		if (app.usb_pullup.port != IFS_DUMMY_PORT)
		{
			interfaces->gpio.set(app.usb_pullup.port, 1 << app.usb_pullup.pin);
		}
		app.usbd_hid.device.drv->connect();
		vsftimer_unregister(&app.usbpu_timer);
		break;
	}
	return NULL;
}

// tickclk interrupt, simply call vsftimer_callback_int
static void app_tickclk_callback_int(void *param)
{
	vsftimer_callback_int();
}

int main(void)
{
	interfaces->core.init(NULL);
	interfaces->tickclk.init();
	interfaces->tickclk.start();
	vsftimer_init();
	interfaces->tickclk.set_callback(app_tickclk_callback_int, NULL);
	
	vsfusbd_device_init(&app.usbd_hid.device);
	vsfsm_init(&app.sm, true);
	while (1)
	{
		vsfsm_poll();
		
		vsf_enter_critical();
		if (!vsfsm_get_event_pending())
		{
			// sleep, will also enable interrupt
			interfaces->core.sleep(SLEEP_WFI);
		}
	}
}
