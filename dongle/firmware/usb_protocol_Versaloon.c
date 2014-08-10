#include "app_cfg.h"

#include "app_interfaces.h"
#include "GPIO/GPIO.h"

#include "dal/mal/mal.h"

#include "usb_protocol.h"

#include "dal/usart_stream/usart_stream.h"
#if MSC_ON_VERSALOON_EN
#include "tool/fakefat32/fakefat32.h"
#endif

#if SCRIPTS_EN
#	define SCRIPTS_FIRST_IF				3
#	if MSC_ON_VERSALOON_EN
#		define MSC_FIRST_IF				5
#	endif
#elif MSC_ON_VERSALOON_EN
#	define MSC_FIRST_IF					3
#endif

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
	0x38,
	0xA0,	// idProduct = 0xA038
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
	106
#if SCRIPTS_EN
	+ 66
#endif
#if MSC_ON_VERSALOON_EN
	+ 31
#endif
	,		// wTotalLength:no of returned bytes
	0x00,
	0x03
#if SCRIPTS_EN
	+ 2
#endif
#if MSC_ON_VERSALOON_EN
	+ 1
#endif
	,	// bNumInterfaces:
		// 1 interfaces for Versaloon
		// 2 interfaces for COM
		// 2 interfaces for Shell
		// 1 interface for MSC
	0x01,	// bConfigurationValue: Configuration value
	0x00,	// iConfiguration: Index of string descriptor describing the configuration
	0x80,	// bmAttributes: bus powered
	0x64,	// MaxPower 200 mA
	
	// IAD
	0x08,	// bLength: IAD Descriptor size
	USB_DESC_TYPE_IAD,
			// bDescriptorType: IAD
	0,		// bFirstInterface
	1,		// bInterfaceCount
	0xFF,	// bFunctionClass
	0x00,	// bFunctionSubClass
	0x00,	// bFunctionProtocol
	0x00,	// iFunction
	
	// interface descriptor
	0x09,	// bLength: Interface Descriptor size
	USB_DESC_TYPE_INTERFACE,
			// bDescriptorType: Interface
	0,		// bInterfaceNumber: Number of Interface
	0x00,	// bAlternateSetting: Alternate setting
	0x02,	// bNumEndpoints: Two endpoints used
	0xFF,	// bInterfaceClass:
	0x00,	// bInterfaceSubClass:
	0x00,	// bInterfaceProtocol:
	0x00,	// iInterface:

	// Endpoint 3 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x03,	// bEndpointAddress: (OUT3)
	0x02,	// bmAttributes: Bulk
	64,		// wMaxPacketSize:
	0x00,
	0x00,	// bInterval: ignore for Bulk transfer

	// Endpoint 2 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x82,	// bEndpointAddress: (IN2)
	0x02,	// bmAttributes: Bulk
	64,		// wMaxPacketSize:
	0x00,
	0x00,	// bInterval
	
	// IAD
	0x08,	// bLength: IAD Descriptor size
	USB_DESC_TYPE_IAD,
			// bDescriptorType: IAD
	1,		// bFirstInterface
	2,		// bInterfaceCount
	0x02,	// bFunctionClass
	0x02,	// bFunctionSubClass
	0x01,	// bFunctionProtocol
	0x04,	// iFunction
	
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
	0x04,	// iInterface:
	
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
	
	// Endpoint 1 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x81,	// bEndpointAddress: (IN1)
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
	
	// Endpoint 4 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x04,	// bEndpointAddress: (OUT4)
	0x02,	// bmAttributes: Bulk
	32,		// wMaxPacketSize:
	0x00,
	0x00,	// bInterval: ignore for Bulk transfer
	
	// Endpoint 4 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x84,	// bEndpointAddress: (IN4)
	0x02,	// bmAttributes: Bulk
	32,		// wMaxPacketSize:
	0x00,
	0x00	// bInterval
	
#if SCRIPTS_EN
	,
	// IAD
	0x08,	// bLength: IAD Descriptor size
	USB_DESC_TYPE_IAD,
			// bDescriptorType: IAD
	SCRIPTS_FIRST_IF,
			// bFirstInterface
	2,		// bInterfaceCount
	0x02,	// bFunctionClass
	0x02,	// bFunctionSubClass
	0x01,	// bFunctionProtocol
	0x05,	// iFunction

	// Interface Descriptor
	0x09,	// bLength: Interface Descriptor size
	USB_DESC_TYPE_INTERFACE,
			// bDescriptorType: Interface
	SCRIPTS_FIRST_IF,
			// bInterfaceNumber: Number of Interface
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
	3,		// bMasterInterface: Communication class interface
	4,		// bSlaveInterface0: Data Class Interface
	
	// Endpoint 5 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x85,	// bEndpointAddress: (IN5)
	0x03,	// bmAttributes: Interrupt
	8,		// wMaxPacketSize:
	0x00,
	0xFF,	// bInterval:
	
	// Data class interface descriptor
	0x09,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_INTERFACE,
			// bDescriptorType: Interface
	SCRIPTS_FIRST_IF + 1,
			// bInterfaceNumber: Number of Interface
	0x00,	// bAlternateSetting: Alternate setting
	0x02,	// bNumEndpoints: Two endpoints used
	0x0A,	// bInterfaceClass: CDC
	0x00,	// bInterfaceSubClass:
	0x00,	// bInterfaceProtocol:
	0x00,	// iInterface:
	
	// Endpoint 6 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x06,	// bEndpointAddress: (OUT6)
	0x02,	// bmAttributes: Bulk
	8,		// wMaxPacketSize:
	0x00,
	0x00,	// bInterval: ignore for Bulk transfer
	
	// Endpoint 6 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x86,	// bEndpointAddress: (IN6)
	0x02,	// bmAttributes: Bulk
	8,		// wMaxPacketSize:
	0x00,
	0x00	// bInterval
#endif
#if MSC_ON_VERSALOON_EN
	,
	// IAD
	0x08,	// bLength: IAD Descriptor size
	USB_DESC_TYPE_IAD,
			// bDescriptorType: IAD
	MSC_FIRST_IF,
			// bFirstInterface
	1,		// bInterfaceCount
	0x08,	// bFunctionClass
	0x06,	// bFunctionSubClass
	0x50,	// bFunctionProtocol
	0x06,	// iFunction
	
	// Interface Descriptor
	0x09,	// bLength: Interface Descriptor size
	USB_DESC_TYPE_INTERFACE,
			// bDescriptorType: Interface
	MSC_FIRST_IF,
			// bInterfaceNumber: Number of Interface
	0x00,	// bAlternateSetting: Alternate setting
	0x02,	// bNumEndpoints
	0x08,	// bInterfaceClass: MASS STORAGE Class
	0x06,	// bInterfaceSubClass : SCSI transparent
	0x50,	// nInterfaceProtocol
	0x00,	// iInterface:
	
	// Endpoint 7 Descriptor
	0x07,	// Endpoint descriptor length = 7
	0x05,	// Endpoint descriptor type
	0x87,	// Endpoint address (IN7)
	0x02,	// Bulk endpoint type
	32,		// Maximum packet size
	0x00,
	0x00,	// Polling interval in milliseconds
	
	// Endpoint 7 Descriptor
	0x07,	// Endpoint descriptor length = 7
	0x05,	// Endpoint descriptor type
	0x07,	// Endpoint address (OUT7)
	0x02,	// Bulk endpoint type
	32,		// Maximum packet size
	0x00,
	0x00,	// Polling interval in milliseconds
#endif
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
	20,
	USB_DESC_TYPE_STRING,
	'V', 0, 'e', 0, 'r', 0, 's', 0, 'a', 0, 'l', 0, 'o', 0, 'o', 0,
	'n', 0
};

static const uint8_t CDConVersaloon_StringProduct[] =
{
	30,
	USB_DESC_TYPE_STRING,
	'C', 0, 'O', 0, 'M', 0, 'o', 0, 'n', 0, 'V', 0, 'e', 0, 'r', 0,
	's', 0, 'a', 0, 'l', 0, 'o', 0, 'o', 0, 'n', 0
};

#if SCRIPTS_EN
static const uint8_t ShellOnVersaloon_StringProduct[] =
{
	34,
	USB_DESC_TYPE_STRING,
	'S', 0, 'h', 0, 'e', 0, 'l', 0, 'l', 0, 'o', 0, 'n', 0, 'V', 0,
	'e', 0, 'r', 0, 's', 0, 'a', 0, 'l', 0, 'o', 0, 'o', 0, 'n', 0
};
#endif

#if MSC_ON_VERSALOON_EN
static const uint8_t MSConVersaloon_StringProduct[] =
{
	30,
	USB_DESC_TYPE_STRING,
	'M', 0, 'S', 0, 'C', 0, 'o', 0, 'n', 0, 'V', 0, 'e', 0, 'r', 0,
	's', 0, 'a', 0, 'l', 0, 'o', 0, 'o', 0, 'n', 0
};
#endif

static uint8_t Versaloon_StringSerial[50] =
{
	50,
	USB_DESC_TYPE_STRING,
	'0', 0, '1', 0, '2', 0, '3', 0, '4', 0, '5', 0, '6', 0, '7', 0, 
	'8', 0, '9', 0, 'A', 0, 'B', 0, 'C', 0, 'D', 0, 'E', 0, 'F', 0, 
	'0', 0, '0', 0, '0', 0, '0', 0, '0', 0, '0', 0, '0', 0, '0', 0, 
};

static const struct vsfusbd_desc_filter_t descriptors[] = 
{
	VSFUSBD_DESC_DEVICE(0, Versaloon_DeviceDescriptor, sizeof(Versaloon_DeviceDescriptor), NULL),
	VSFUSBD_DESC_CONFIG(0, 0, Versaloon_ConfigDescriptor, sizeof(Versaloon_ConfigDescriptor), NULL),
	VSFUSBD_DESC_STRING(0, 0, Versaloon_StringLangID, sizeof(Versaloon_StringLangID), NULL),
	VSFUSBD_DESC_STRING(0x0409, 1, Versaloon_StringVendor, sizeof(Versaloon_StringVendor), NULL),
	VSFUSBD_DESC_STRING(0x0409, 2, Versaloon_StringProduct, sizeof(Versaloon_StringProduct), NULL),
	VSFUSBD_DESC_STRING(0x0409, 3, Versaloon_StringSerial, sizeof(Versaloon_StringSerial), NULL),
	VSFUSBD_DESC_STRING(0x0409, 4, CDConVersaloon_StringProduct, sizeof(CDConVersaloon_StringProduct), NULL),
#if SCRIPTS_EN
	VSFUSBD_DESC_STRING(0x0409, 5, ShellOnVersaloon_StringProduct, sizeof(ShellOnVersaloon_StringProduct), NULL),
#endif
#if MSC_ON_VERSALOON_EN
	VSFUSBD_DESC_STRING(0x0409, 6, MSConVersaloon_StringProduct, sizeof(MSConVersaloon_StringProduct), NULL),
#endif
	VSFUSBD_DESC_NULL
};

// Versaloon
struct vsfusbd_Versaloon_param_t Versaloon_param = 
{
	3,				// uint8_t ep_out;
	2,				// uint8_t ep_in;
	
	true,			// bool dbuffer_en;
};

#if SCRIPTS_EN
// CDCACM for vss
extern struct usart_stream_info_t shell_stream;
struct vsfusbd_CDCACM_param_t Versaloon_Shell_param = 
{
	{
		6,			// ep_out
		6, 			// ep_in
	},
	{
		NULL, NULL, NULL, NULL,
	},
	{
		115200,	// bitrate
		0,		// stopbittype
		0,		// paritytype
		8		// datatype
	},
};
#endif

#if MSC_ON_VERSALOON_EN
// MSC
#define MSC_BLOCK_SIZE				512
static struct fakefat32_file_t empty_dir_under_root[] =
{
	{
		".", NULL,
		FAKEFAT32_FILEATTR_DIRECTORY,
	},
	{
		"..", NULL,
		FAKEFAT32_FILEATTR_DIRECTORY,
	},
	{
		NULL,
	}
};

extern uint8_t buffer_out[USB_DATA_BUFF_SIZE];
void ProcessCommand(uint8_t *dat, uint16_t len);
static vsf_err_t usbtoxxx_command_write(struct fakefat32_file_t*file, uint32_t addr,
										uint8_t *buff, uint32_t page_size)
{
	if (addr != 0)
	{
		return VSFERR_FAIL;
	}
	
	LED_USB_ON();
	memcpy(buffer_out, buff, page_size);
	ProcessCommand(&buffer_out[0], GET_LE_U16(&buffer_out[1]));
	return VSFERR_NONE;
}
static vsf_err_t usbtoxxx_command_read(struct fakefat32_file_t*file, uint32_t addr,
										uint8_t *buff, uint32_t page_size)
{
	if (addr != 0)
	{
		return VSFERR_FAIL;
	}
	
	memset(buff, 0, page_size);
	return VSFERR_NONE;
}
static vsf_err_t usbtoxxx_reply_read(struct fakefat32_file_t*file, uint32_t addr,
										uint8_t *buff, uint32_t page_size)
{
	if (addr != 0)
	{
		return VSFERR_FAIL;
	}
	
	memcpy(buff, buffer_out, page_size);
	return VSFERR_NONE;
}
static vsf_err_t versaloon_tvcc_voltage_read(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	uint16_t voltage;
	
#if POWER_OUT_EN
	app_interfaces.target_voltage.get(0, &voltage);
#else
	voltage = 0;
#endif
	
	buff[0] = voltage / 1000 + '0';
	buff[1] = '.';
	buff[2] = (voltage % 1000) / 100 + '0';
	buff[3] = (voltage % 100) / 10 + '0';
	buff[4] = 'V';
	buff[5] = '\n';
	return VSFERR_NONE;
}
static vsf_err_t versaloon_tvcc_control_read(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
#if POWER_OUT_EN
	extern uint8_t PWREXT_PowerState;
	
	buff[0] = PWREXT_PowerState + '0';
#else
	buff[0] = '0';
#endif
	buff[1] = '\n';
	return VSFERR_NONE;
}
static vsf_err_t versaloon_tvcc_control_write(struct fakefat32_file_t*file,
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	if (buff[0] == '0')
	{
#if POWER_OUT_EN
		interfaces->target_voltage.set(0, 0);
#endif
	}
	else if (buff[0] == '1')
	{
#if POWER_OUT_EN
		interfaces->target_voltage.set(0, 3300);
#endif
	}
	else
	{
		// not supported value
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}
static struct fakefat32_file_t usbtoxxx_dir[] =
{
	{
		".", NULL,
		FAKEFAT32_FILEATTR_DIRECTORY,
	},
	{
		"..", NULL,
		FAKEFAT32_FILEATTR_DIRECTORY,
	},
	{
		"command", NULL,
		FAKEFAT32_FILEATTR_ARCHIVE,
		0,
		{usbtoxxx_command_read, NULL, usbtoxxx_command_write, NULL},
	},
	{
		"reply", NULL,
		FAKEFAT32_FILEATTR_ARCHIVE | FAKEFAT32_FILEATTR_READONLY,
		0,
		{usbtoxxx_reply_read, NULL, NULL, NULL},
	},
	{
		NULL,
	}
};
static struct fakefat32_file_t versaloon_dir[] =
{
	{
		".", NULL,
		FAKEFAT32_FILEATTR_DIRECTORY,
	},
	{
		"..", NULL,
		FAKEFAT32_FILEATTR_DIRECTORY,
	},
	{
		"usbtoxxx", "",
		FAKEFAT32_FILEATTR_DIRECTORY,
		0,
		{fakefat32_dir_read, NULL, fakefat32_dir_write, NULL},
		usbtoxxx_dir
	},
	{
		"tvcc", "voltage",
		FAKEFAT32_FILEATTR_ARCHIVE | FAKEFAT32_FILEATTR_READONLY,
		6,
		{versaloon_tvcc_voltage_read, NULL, NULL, NULL},
	},
	{
		"tvcc", "control",
		FAKEFAT32_FILEATTR_ARCHIVE,
		2,
		{versaloon_tvcc_control_read, NULL, versaloon_tvcc_control_write, NULL},
	},
	{
		NULL,
	}
};
static struct fakefat32_file_t sys_dir[] =
{
	{
		".", NULL,
		FAKEFAT32_FILEATTR_DIRECTORY,
	},
	{
		"..", NULL,
		FAKEFAT32_FILEATTR_DIRECTORY,
	},
	{
		"versaloon", NULL,
		FAKEFAT32_FILEATTR_DIRECTORY,
		0,
		{fakefat32_dir_read, NULL, fakefat32_dir_write, NULL},
		versaloon_dir
	},
	{
		NULL,
	}
};
static struct fakefat32_file_t root_dir[] =
{
	{
		"VersaloonFS", NULL,
		FKAEFAT32_FILEATTR_VOLUMEID,
	},
	{
		"sys", NULL,
		FAKEFAT32_FILEATTR_DIRECTORY,
		0,
		{fakefat32_dir_read, NULL, fakefat32_dir_write, NULL},
		sys_dir
	},
	{
		"LOST", "DIR",
		FAKEFAT32_FILEATTR_DIRECTORY,
		0,
		{fakefat32_dir_read, NULL, fakefat32_dir_write, NULL},
		empty_dir_under_root
	},
	{
		NULL,
	}
};

static struct fakefat32_param_t fakefat32_param =
{
	MSC_BLOCK_SIZE,	// uint16_t sector_size;
	0x00760000,		// uint32_t sector_number;
	8,				// uint8_t sectors_per_cluster;
	
	0x0CA93E47,		// uint32_t volume_id;
	0x12345678,		// uint32_t disk_id;
	{				// struct fakefat32_file_t root;
		{
			"ROOT", NULL,
			0,
			0,
			{fakefat32_dir_read, NULL, fakefat32_dir_write, NULL},
			root_dir
		}
	}
};
static struct mal_info_t fakefat32_mal_info = 
{
	{0, 0}, NULL, 0, 0, 0, &fakefat32_drv
};
static struct dal_info_t fakefat32_dal_info = 
{
	NULL,
	&fakefat32_param,
	NULL,
	&fakefat32_mal_info,
};

struct SCSI_LUN_info_t MSCBOT_LunInfo = 
{
	&fakefat32_dal_info, 
	{
		true,
		{'S', 'i', 'm', 'o', 'n', ' ', ' ', ' '},
		{'V', 'e', 'r', 's', 'a', 'l', 'o', 'o', 
		'n', 'F', 'S', ' ', ' ', ' ', ' ', ' '},
		{'1', '.', '0', '0'},
		SCSI_PDT_DIRECT_ACCESS_BLOCK
	}
};
uint8_t MSCBOT_Buffer0[MSC_BLOCK_SIZE], MSCBOT_Buffer1[MSC_BLOCK_SIZE];

struct vsfusbd_MSCBOT_param_t MSCBOT_param = 
{
	7,							// uint8_t ep_out;
	7,							// uint8_t ep_in;
	
	0,							// uint8_t max_lun;
	&MSCBOT_LunInfo,			// struct SCSI_LUN_info_t *lun_info;
	NULL, 						// struct SCSI_handler_t *user_handlers;
	
	{
		{MSCBOT_Buffer0, sizeof(MSCBOT_Buffer0)},
		{MSCBOT_Buffer1, sizeof(MSCBOT_Buffer1)}
	},							// struct vsf_buffer_t page_buffer[2];
};
#endif

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
	{
		4,			// ep_out
		4, 			// ep_in
	},
	{
		VOM_set_line_coding,
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
	{(struct vsfusbd_class_protocol_t *)&vsfusbd_CDCACMControl_class, (void *)&Versaloon_CDCACM_param},
	{(struct vsfusbd_class_protocol_t *)&vsfusbd_CDCACMData_class, (void *)&Versaloon_CDCACM_param},
#if SCRIPTS_EN
	{(struct vsfusbd_class_protocol_t *)&vsfusbd_CDCACMControl_class, (void *)&Versaloon_Shell_param},
	{(struct vsfusbd_class_protocol_t *)&vsfusbd_CDCACMData_class, (void *)&Versaloon_Shell_param},
#endif
#if MSC_ON_VERSALOON_EN
	{(struct vsfusbd_class_protocol_t *)&vsfusbd_MSCBOT_class, (void *)&MSCBOT_param},
#endif
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
#if HW_HAS_LCM
	LCM_BACKLIGHT_INIT();
#endif
	
	app_interfaces.delay.init();
#if POWER_SAMPLE_EN
	core_interfaces.adc.init(TVCC_ADC_PORT);
	core_interfaces.adc.config(TVCC_ADC_PORT, CORE_APB2_FREQ_HZ / 8, ADC_ALIGNRIGHT);
	core_interfaces.adc.config_channel(TVCC_ADC_PORT, TVCC_ADC_CHANNEL, 0xFF);
	core_interfaces.adc.calibrate(TVCC_ADC_PORT, TVCC_ADC_CHANNEL);
#endif
	
	Versaloon_CDCACM_param.CDC_param.stream_tx = &usart_stream_p0.stream_tx;
	Versaloon_CDCACM_param.CDC_param.stream_rx = &usart_stream_p0.stream_rx;
	usart_stream_init(&usart_stream_p0);
#if SCRIPTS_EN
	Versaloon_Shell_param.CDC_param.stream_tx = &shell_stream.stream_tx;
	Versaloon_Shell_param.CDC_param.stream_rx = &shell_stream.stream_rx;
	usart_stream_init(&shell_stream);
#endif
#if MSC_ON_VERSALOON_EN
	fakefat32_param.sector_size = MSC_BLOCK_SIZE;
	fakefat32_param.sector_number = 128 * 1024 * 1024 / fakefat32_param.sector_size;
	fakefat32_param.sectors_per_cluster = 1;
	usbtoxxx_dir[2].size = usbtoxxx_dir[3].size = fakefat32_param.sector_size;
#endif
	
	// initialize Serial Number of USB Device
	{
		int i, pos;
		uint8_t uid[32];
		uint32_t uid_size = core_interfaces.uid.get(uid, sizeof(uid));
		const char hex[] = "0123456789ABCDEF";
		
		pos = 2;
		for (i = 0; i < min(uid_size, (sizeof(Versaloon_StringSerial) - 2) / 2);
				i++)
		{
			Versaloon_StringSerial[pos] = hex[(uid[i] >> 0) & 0x0F];
			pos += 2;
			Versaloon_StringSerial[pos] = hex[(uid[i] >> 4) & 0x0F];
			pos += 2;
		}
	}
	
	USB_Pull_Init();
	USB_Disconnect();
	interfaces->delay.delayms(100);
	USB_Connect();
	return vsfusbd_device_init(&usb_device);
}

vsf_err_t usb_protocol_poll(void)
{
	usart_stream_poll(&usart_stream_p0);
#if SCRIPTS_EN
	usart_stream_poll(&shell_stream);
#endif
#if POWER_OUT_EN
	app_interfaces.target_voltage.poll(0);
#endif
	return vsfusbd_device_poll(&usb_device);
}
