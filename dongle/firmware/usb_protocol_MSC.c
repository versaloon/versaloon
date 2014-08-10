#include "app_cfg.h"

#include "app_interfaces.h"
#include "usb_protocol.h"

#include "dal/mal/mal.h"
#include "dal/sd/sd_common.h"
#include "dal/sd/sd_spi_drv.h"

static const uint8_t MSCBOT_DeviceDescriptor[] =
{
	0x12,	// bLength
	USB_DESC_TYPE_DEVICE,	 // bDescriptorType
	0x00,
	0x02,	// bcdUSB = 2.00
	0x08,	// bDeviceClass
	0x06,	// bDeviceSubClass
	0x50,	// bDeviceProtocol
	0x08,	// bMaxPacketSize0
	0x83,
	0x04,	// idVendor = 0x0483
	0x44,
	0x37,	// idProduct = 0x3744
	0x00,
	0x01,	// bcdDevice = 1.00
	1,		// Index of string descriptor describing manufacturer
	2,		// Index of string descriptor describing product
	3,		// Index of string descriptor describing the device's serial number
	0x01	// bNumConfigurations
};

const uint8_t MSCBOT_ConfigDescriptor[] =
{
	// Configuation Descriptor
	0x09,// bLength: Configuation Descriptor size
	USB_DESC_TYPE_CONFIGURATION,	// bDescriptorType: Configuration
	32,		// wTotalLength:no of returned bytes
	0x00,
	0x01,	// bNumInterfaces: 1 interface
	0x01,	// bConfigurationValue: Configuration value
	0x00,	// iConfiguration: Index of string descriptor describing the configuration
	0x80,	// bmAttributes: bus powered
	0x64,	// MaxPower 200 mA

	// Interface Descriptor
	0x09,	// bLength: Interface Descriptor size
	0x04,	// bDescriptorType:
	0x00,	// bInterfaceNumber: Number of Interface
	0x00,	// bAlternateSetting: Alternate setting
	0x02,	// bNumEndpoints
	0x08,	// bInterfaceClass: MASS STORAGE Class
	0x06,	// bInterfaceSubClass : SCSI transparent
	0x50,	// nInterfaceProtocol
	0x00,	// iInterface:

	// Endpoint 4 Descriptor
	0x07,	// Endpoint descriptor length = 7
	0x05,	// Endpoint descriptor type
	0x84,	// Endpoint address (IN, address 4)
	0x02,	// Bulk endpoint type
	0x40,	// Maximum packet size (64 bytes)
	0x00,
	0x00,	// Polling interval in milliseconds

	// Endpoint 4 Descriptor
	0x07,	// Endpoint descriptor length = 7
	0x05,	// Endpoint descriptor type
	0x04,	// Endpoint address (OUT, address 4)
	0x02,	// Bulk endpoint type
	0x40,	// Maximum packet size (64 bytes)
	0x00,
	0x00,	// Polling interval in milliseconds
};

static const uint8_t MSCBOT_StringLangID[] =
{
	4,
	USB_DESC_TYPE_STRING,
	0x09,
	0x04
};

static const uint8_t MSCBOT_StringVendor[] =
{
	38,
	USB_DESC_TYPE_STRING,
	'S', 0, 'T', 0, 'M', 0, 'i', 0, 'c', 0, 'r', 0, 'o', 0, 'e', 0,
	'l', 0, 'e', 0, 'c', 0, 't', 0, 'r', 0, 'o', 0, 'n', 0, 'i', 0,
	'c', 0, 's', 0
};

static const uint8_t MSCBOT_StringProduct[] =
{
	20,
	USB_DESC_TYPE_STRING,
	'M', 0, 'S', 0, 'C', 0, 't', 0, 'e', 0, 's', 0, 't', 0, 'e', 0,
	'r', 0
};

static const uint8_t MSCBOT_StringSerial[50] =
{
	50,
	USB_DESC_TYPE_STRING,
	'0', 0, '1', 0, '2', 0, '3', 0, '4', 0, '5', 0, '6', 0, '7', 0, 
	'8', 0, '9', 0, 'A', 0, 'B', 0, 'C', 0, 'D', 0, 'E', 0, 'F', 0, 
	'0', 0, '0', 0, '0', 0, '0', 0, '0', 0, '0', 0, '0', 0, '0', 0, 
};

static const struct vsfusbd_desc_filter_t descriptors[] = 
{
	VSFUSBD_DESC_DEVICE(0, MSCBOT_DeviceDescriptor, sizeof(MSCBOT_DeviceDescriptor), NULL),
	VSFUSBD_DESC_CONFIG(0, 0, MSCBOT_ConfigDescriptor, sizeof(MSCBOT_ConfigDescriptor), NULL),
	VSFUSBD_DESC_STRING(0, 0, MSCBOT_StringLangID, sizeof(MSCBOT_StringLangID), NULL),
	VSFUSBD_DESC_STRING(0x0409, 1, MSCBOT_StringVendor, sizeof(MSCBOT_StringVendor), NULL),
	VSFUSBD_DESC_STRING(0x0409, 2, MSCBOT_StringProduct, sizeof(MSCBOT_StringProduct), NULL),
	VSFUSBD_DESC_STRING(0x0409, 3, MSCBOT_StringSerial, sizeof(MSCBOT_StringSerial), NULL),
	VSFUSBD_DESC_NULL
};

static struct sd_info_t sd_info;
static struct sd_param_t sd_param =
{
	9000		// uint16_t kHz;
};
static struct sd_spi_drv_info_t sd_spi_drv_info;
static struct sd_spi_drv_interface_t sd_spi_drv_ifs = 
{
	0,			// uint8_t cs_port;
	GPIO_SRST,	// uint32_t cs_pin;
	0,			// uint8_t spi_port;
};
static struct mal_info_t sd_mal_info = 
{
	{0, 0}, &sd_info, 0, 0, 0, &sd_spi_drv
};
static struct dal_info_t sd_dal_info = 
{
	&sd_spi_drv_ifs,
	&sd_param,
	&sd_spi_drv_info,
	&sd_mal_info,
};

struct SCSI_LUN_info_t MSCBOT_LunInfo[] = 
{
	{
		&sd_dal_info, 
		{
			true,
			{'S', 'i', 'm', 'o', 'n', ' ', ' ', ' '},
			{'S', 'i', 'm', 'o', 'n', ' ', ' ', ' ', 
			'S', 'i', 'm', 'o', 'n', ' ', ' ', ' '},
			{'1', '.', '0', '0'},
			SCSI_PDT_DIRECT_ACCESS_BLOCK
		}
	}
};
uint8_t MSCBOT_Buffer0[512], MSCBOT_Buffer1[512];

struct vsfusbd_MSCBOT_param_t MSCBOT_param = 
{
	4,							// uint8_t ep_out;
	4,							// uint8_t ep_in;
	
	dimof(MSCBOT_LunInfo) - 1,	// uint8_t max_lun;
	MSCBOT_LunInfo,				// struct SCSI_LUN_info_t *lun_info;
	NULL, 						// struct SCSI_handler_t *user_handlers;
	
	{
		{MSCBOT_Buffer0, sizeof(MSCBOT_Buffer0)},
		{MSCBOT_Buffer1, sizeof(MSCBOT_Buffer1)}
	},							// struct vsf_buffer_t page_buffer[2];
};

static struct vsfusbd_iface_t ifaces[] = 
{
	{(struct vsfusbd_class_protocol_t *)&vsfusbd_MSCBOT_class, (void *)&MSCBOT_param},
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
	LED_POWER_INIT();
	LED_STATE_INIT();
	LED_STATE_G_ON();
	LED_USB_INIT();
	
	USB_Pull_Init();
	USB_Connect();
	return vsfusbd_device_init(&usb_device);
}

vsf_err_t usb_protocol_poll(void)
{
	return vsfusbd_device_poll(&usb_device);
}
