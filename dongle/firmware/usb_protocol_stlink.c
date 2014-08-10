#include "app_cfg.h"

#include "app_interfaces.h"
#include "GPIO/GPIO.h"

#include "dal/mal/mal.h"

#include "usb_protocol.h"

#include "dal/usart_stream/usart_stream.h"

static const uint8_t Versaloon_DeviceDescriptor[] =
{
	0x12,	// bLength
	USB_DESC_TYPE_DEVICE,
			// bDescriptorType
	0x00,
	0x02,	// bcdUSB = 2.00
	0xEF,	// bDeviceClass: IAD
	0x02,	// bDeviceSubClass
	0x01,	// bDeviceProtocol
	0x08,	// bMaxPacketSize0
	0x83,
	0x04,	// idVendor = 0x0483
	0x48,
	0x37,	// idProduct = 0x3748
	0x00,
	0x01,	// bcdDevice = 1.00
	1,		// Index of string descriptor describing manufacturer
	2,		// Index of string descriptor describing product
	3,		// Index of string descriptor describing the device's serial number
	0x01	// bNumConfigurations
};

static const uint8_t Versaloon_ConfigDescriptor[] =
{
	// Configuation Descriptor
	0x09,	// bLength: Configuation Descriptor size
	USB_DESC_TYPE_CONFIGURATION,
			// bDescriptorType: Configuration
	113,	// wTotalLength:no of returned bytes
	0x00,
	0x03,	// bNumInterfaces:
	0x01,	// bConfigurationValue: Configuration value
	0x00,	// iConfiguration: Index of string descriptor describing the configuration
	0x80,	// bmAttributes: bus powered
	0x32,	// MaxPower 200 mA
	
	// IAD
	0x08,	// bLength: IAD Descriptor size
	USB_DESC_TYPE_IAD,
			// bDescriptorType: IAD
	0,		// bFirstInterface
	1,		// bInterfaceCount
	0xFF,	// bFunctionClass
	0xFF,	// bFunctionSubClass
	0xFF,	// bFunctionProtocol
	0x04,	// iFunction
	
	// STLink
	0x09,	// bLength: Interface Descriptor size
	USB_DESC_TYPE_INTERFACE,
			// bDescriptorType: Interface
	0,		// bInterfaceNumber: Number of Interface
	0x00,	// bAlternateSetting: Alternate setting
	0x03,	// bNumEndpoints: 3 endpoints used
	0xFF,	// bInterfaceClass:
	0xFF,	// bInterfaceSubClass:
	0xFF,	// bInterfaceProtocol:
	0x04,	// iInterface:
	
	// IN1 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x81,	// bEndpointAddress: (IN1)
	0x02,	// bmAttributes: Bulk
	64,		// wMaxPacketSize:
	0x00,
	0x00,	// bInterval: ignore for Bulk transfer
	
	// OUT2 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x02,	// bEndpointAddress: (OUT2)
	0x02,	// bmAttributes: Bulk
	64,		// wMaxPacketSize:
	0x00,
	0x00,	// bInterval: ignore for Bulk transfer
	
	// IN3 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x83,	// bEndpointAddress: (IN3)
	0x02,	// bmAttributes: Bulk
	64,		// wMaxPacketSize:
	0x00,
	0x00,	// bInterval: ignore for Bulk transfer
	
	// IAD
	0x08,	// bLength: IAD Descriptor size
	USB_DESC_TYPE_IAD,
			// bDescriptorType: IAD
	1,		// bFirstInterface
	2,		// bInterfaceCount
	0x02,	// bFunctionClass
	0x02,	// bFunctionSubClass
	0x01,	// bFunctionProtocol
	0x05,	// iFunction
	
	// Interface Descriptor
	0x09,	// bLength: Interface Descriptor size
	USB_DESC_TYPE_INTERFACE,
			// bDescriptorType: Interface
	1,		// bInterfaceNumber: Number of Interface
	0x00,	// bAlternateSetting: Alternate setting
	0x01,	// bNumEndpoints: One endpoints used
	0x02,	// bInterfaceClass: Communication Interface Class
	0x02,	// bInterfaceSubClass: Abstract Control Model
	0x01,	// bInterfaceProtocol: Common AT commands
	0x05,	// iInterface:
	
	// Header Functional Descriptor
	0x05,	// bLength: Endpoint Descriptor size
	0x24,	// bDescriptorType: CS_INTERFACE
	0x00,	// bDescriptorSubtype: Header Func Desc
	0x10,	// bcdCDC: spec release number
	0x01,
	
	// Call Managment Functional Descriptor
	0x05,	// bFunctionLength
	0x24,	// bDescriptorType: CS_INTERFACE
	0x01,	// bDescriptorSubtype: Call Management Func Desc
	0x00,	// bmCapabilities: D0+D1
	0x01,	// bDataInterface: 1
	
	// ACM Functional Descriptor
	0x04,	// bFunctionLength
	0x24,	// bDescriptorType: CS_INTERFACE
	0x02,	// bDescriptorSubtype: Abstract Control Management desc
	0x02,	// bmCapabilities
	
	// Union Functional Descriptor
	0x05,	// bFunctionLength
	0x24,	// bDescriptorType: CS_INTERFACE
	0x06,	// bDescriptorSubtype: Union func desc
	1,		// bMasterInterface: Communication class interface
	2,		// bSlaveInterface0: Data Class Interface
	
	// IN4 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x84,	// bEndpointAddress: (IN4)
	0x03,	// bmAttributes: Interrupt
	8,		// wMaxPacketSize:
	0x00,
	0xFF,	// bInterval:
	
	// Data class interface descriptor
	0x09,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_INTERFACE,
			// bDescriptorType: Interface
	2,		// bInterfaceNumber: Number of Interface
	0x00,	// bAlternateSetting: Alternate setting
	0x02,	// bNumEndpoints: Two endpoints used
	0x0A,	// bInterfaceClass: CDC
	0x00,	// bInterfaceSubClass:
	0x00,	// bInterfaceProtocol:
	0x00,	// iInterface:
	
	// OUT5 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x05,	// bEndpointAddress: (OUT5)
	0x02,	// bmAttributes: Bulk
	32,		// wMaxPacketSize:
	0x00,
	0x00,	// bInterval: ignore for Bulk transfer
	
	// IN5 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x85,	// bEndpointAddress: (IN5)
	0x02,	// bmAttributes: Bulk
	32,		// wMaxPacketSize:
	0x00,
	0x00	// bInterval
};

static const uint8_t Versaloon_StringLangID[] =
{
	4,
	USB_DESC_TYPE_STRING,
	0x09,
	0x04
};

static const uint8_t Versaloon_StringVendor[] =
{
	38,
	USB_DESC_TYPE_STRING,
	'S', 0, 'T', 0, 'M', 0, 'i', 0, 'c', 0, 'r', 0, 'o', 0, 'e', 0,
	'l', 0, 'e', 0, 'c', 0, 't', 0, 'r', 0, 'o', 0, 'n', 0, 'i', 0,
	'c', 0, 's', 0
};

static const uint8_t Versaloon_StringProduct[] =
{
	26,
	USB_DESC_TYPE_STRING,
	'S', 0, 'T', 0, 'M', 0, '3', 0, '2', 0, ' ', 0, 'S', 0, 'T', 0,
	'L', 0, 'i', 0, 'n', 0, 'k', 0
};

static const uint8_t Versaloon_StringSerial[] =
{
	26,
	USB_DESC_TYPE_STRING,
	'0', 0, '1', 0, '2', 0, '3', 0, '4', 0, '5', 0, '6', 0, '7', 0, 
	'8', 0, '9', 0, 'A', 0, 'B', 0
};

static const uint8_t Versaloon_StringIF0[] =
{
	16,
	USB_DESC_TYPE_STRING,
	'S', 0, 'T', 0, ' ', 0, 'L', 0, 'i', 0, 'n', 0, 'k', 0
};

static const uint8_t CDConVersaloon_StringProduct[] =
{
	30,
	USB_DESC_TYPE_STRING,
	'C', 0, 'O', 0, 'M', 0, 'o', 0, 'n', 0, 'V', 0, 'e', 0, 'r', 0,
	's', 0, 'a', 0, 'l', 0, 'o', 0, 'o', 0, 'n', 0
};

static const struct vsfusbd_desc_filter_t descriptors[] = 
{
	VSFUSBD_DESC_DEVICE(0, Versaloon_DeviceDescriptor, sizeof(Versaloon_DeviceDescriptor), NULL),
	VSFUSBD_DESC_CONFIG(0, 0, Versaloon_ConfigDescriptor, sizeof(Versaloon_ConfigDescriptor), NULL),
	VSFUSBD_DESC_STRING(0, 0, Versaloon_StringLangID, sizeof(Versaloon_StringLangID), NULL),
	VSFUSBD_DESC_STRING(0x0409, 1, Versaloon_StringVendor, sizeof(Versaloon_StringVendor), NULL),
	VSFUSBD_DESC_STRING(0x0409, 2, Versaloon_StringProduct, sizeof(Versaloon_StringProduct), NULL),
	VSFUSBD_DESC_STRING(0x0409, 3, Versaloon_StringSerial, sizeof(Versaloon_StringSerial), NULL),
	VSFUSBD_DESC_STRING(0x0409, 4, Versaloon_StringIF0, sizeof(Versaloon_StringIF0), NULL),
	VSFUSBD_DESC_STRING(0x0409, 5, CDConVersaloon_StringProduct, sizeof(CDConVersaloon_StringProduct), NULL),
	VSFUSBD_DESC_NULL
};

// Versaloon
struct vsfusbd_Versaloon_param_t Versaloon_param = 
{
	2,				// uint8_t ep_out;
	1,				// uint8_t ep_in;
	
	true,			// bool dbuffer_en;
};

// CDCACM
extern struct usart_stream_info_t usart_stream_p0;

vsf_err_t VOM_set_line_coding(struct vsfusbd_CDCACM_line_coding_t *line_coding)
{
	usart_stream_p0.usart_info.datalength = line_coding->datatype;
	usart_stream_p0.usart_info.baudrate = line_coding->bitrate;
	usart_stream_p0.usart_info.mode = 0;
	switch(line_coding->stopbittype)
	{
	default:
	case 0:
		usart_stream_p0.usart_info.mode |= USART_STOPBITS_1;
		break;
	case 1:
		usart_stream_p0.usart_info.mode |= USART_STOPBITS_1P5;
		break;
	case 2:
		usart_stream_p0.usart_info.mode |= USART_STOPBITS_2;
		break;
	}
	switch(line_coding->paritytype)
	{
	default:
	case 0:
		usart_stream_p0.usart_info.mode |= USART_PARITY_NONE;
		usart_stream_p0.usart_info.datalength = 8;
		break;
	case 1:
		usart_stream_p0.usart_info.mode |= USART_PARITY_ODD;
		usart_stream_p0.usart_info.datalength = 9;
		break;
	case 2:
		usart_stream_p0.usart_info.mode |= USART_PARITY_EVEN;
		usart_stream_p0.usart_info.datalength = 9;
		break;
	}
	return usart_stream_config(&usart_stream_p0);
}

struct vsfusbd_CDCACM_param_t Versaloon_CDCACM_param = 
{
	5,			// ep_out
	5, 			// ep_in
	
	NULL, NULL,
	
	{
		VOM_set_line_coding
	},
	
	{
		115200,	// bitrate
		0,		// stopbittype
		0,		// paritytype
		8		// datatype
	},
};

static struct vsfusbd_iface_t ifaces[] = 
{
	{(struct vsfusbd_class_protocol_t *)&vsfusbd_Versaloon_class, (void *)&Versaloon_param},
	{(struct vsfusbd_class_protocol_t *)&vsfusbd_CDCACMMaster_class, (void *)&Versaloon_CDCACM_param},
	{(struct vsfusbd_class_protocol_t *)&vsfusbd_CDCACMData_class, (void *)&Versaloon_CDCACM_param},
};
static struct vsfusbd_config_t configurations[] = 
{
	{
		NULL, NULL, dimof(ifaces), (struct vsfusbd_iface_t *)ifaces,
	}
};
struct vsfusbd_device_t usb_device = 
{
	dimof(configurations), (struct vsfusbd_config_t *)configurations, 
	(struct vsfusbd_desc_filter_t *)descriptors, 0, 
	(struct interface_usbd_t *)&core_interfaces.usbd
};

vsf_err_t usb_protocol_init(void)
{
	NVIC_InitTypeDef NVIC_InitStructure;
	
	NVIC_PriorityGroupConfig(NVIC_PriorityGroup_2);
	
	NVIC_InitStructure.NVIC_IRQChannel = USART1_IRQn;
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 0;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 0;
	NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
	NVIC_Init(&NVIC_InitStructure);
	
	core_interfaces.gpio.init(0);
	core_interfaces.gpio.init(1);
	core_interfaces.gpio.init(2);
	
	LED_POWER_INIT();
	LED_STATE_INIT();
	LED_STATE_G_ON();
	LED_USB_INIT();
#if HW_HAS_LEDARRAY
	LED_ARRAY_INIT();
#endif
	
	app_interfaces.delay.init();
#if POWER_SAMPLE_EN
	core_interfaces.adc.init(TVCC_ADC_PORT);
	core_interfaces.adc.config(TVCC_ADC_PORT, CORE_APB2_FREQ_HZ / 8, ADC_ALIGNRIGHT);
	core_interfaces.adc.config_channel(TVCC_ADC_PORT, TVCC_ADC_CHANNEL, 0xFF);
	core_interfaces.adc.calibrate(TVCC_ADC_PORT, TVCC_ADC_CHANNEL);
#endif
	
	Versaloon_CDCACM_param.stream_tx = &usart_stream_p0.stream_tx;
	Versaloon_CDCACM_param.stream_rx = &usart_stream_p0.stream_rx;
	usart_stream_init(&usart_stream_p0);
	
	USB_Pull_Init();
	USB_Connect();
	return vsfusbd_device_init(&usb_device);
}

vsf_err_t usb_protocol_poll(void)
{
	usart_stream_poll(&usart_stream_p0);
#if POWER_OUT_EN
	app_interfaces.target_voltage.poll(0);
#endif
	return vsfusbd_device_poll(&usb_device);
}
