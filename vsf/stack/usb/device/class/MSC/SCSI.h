#ifndef __SCSI_H_INCLUDED__
#define __SCSI_H_INCLUDED__

#include "tool/buffer/buffer.h"

#define SCSI_CMD_FORMAT_UNIT						0x04
#define SCSI_CMD_INQUIRY							0x12
#define SCSI_CMD_MODE_SELECT6						0x15
#define SCSI_CMD_MODE_SELECT10						0x55
#define SCSI_CMD_MODE_SENSE6						0x1A
#define SCSI_CMD_MODE_SENSE10						0x5A
#define SCSI_CMD_ALLOW_MEDIUM_REMOVAL				0x1E
#define SCSI_CMD_READ6								0x08
#define SCSI_CMD_READ10								0x28
#define SCSI_CMD_READ12								0xA8
#define SCSI_CMD_READ16								0x88
#define SCSI_CMD_READ_CAPACITY10					0x25
#define SCSI_CMD_READ_CAPACITY16					0x9E
#define SCSI_CMD_REQUEST_SENSE						0x03
#define SCSI_CMD_START_STOP_UNIT					0x1B
#define SCSI_CMD_TEST_UNIT_READY					0x00
#define SCSI_CMD_WRITE6								0x0A
#define SCSI_CMD_WRITE10							0x2A
#define SCSI_CMD_WRITE12							0xAA
#define SCSI_CMD_WRITE16							0x8A
#define SCSI_CMD_VERIFY10							0x2F
#define SCSI_CMD_VERIFY12							0xAF
#define SCSI_CMD_VERIFY16							0x8F
#define SCSI_CMD_SEND_DIAGNOSTIC					0x1D
#define SCSI_CMD_READ_FORMAT_CAPACITIES				0x23
#define SCSI_CMD_GET_EVENT_STATUS_NOTIFICATION		0x4A
#define SCSI_CMD_READ_TOC							0x43

#define SCSI_SENSEKEY_NO_SENSE						0
#define SCSI_SENSEKEY_RECOVERED_ERROR				1
#define SCSI_SENSEKEY_NOT_READY						2
#define SCSI_SENSEKEY_MEDIUM_ERROR					3
#define SCSI_SENSEKEY_HARDWARE_ERROR				4
#define SCSI_SENSEKEY_ILLEGAL_REQUEST				5
#define SCSI_SENSEKEY_UNIT_ATTENTION				6
#define SCSI_SENSEKEY_DATA_PROTECT					7
#define SCSI_SENSEKEY_BLANK_CHECK					8
#define SCSI_SENSEKEY_VENDOR_SPECIFIC				9
#define SCSI_SENSEKEY_COPY_ABORTED					10
#define SCSI_SENSEKEY_ABORTED_COMMAND				11
#define SCSI_SENSEKEY_VOLUME_OVERFLOW				13
#define SCSI_SENSEKEY_MISCOMPARE					14

#define SCSI_ASC_INVALID_COMMAND					0x20
#define SCSI_ASC_INVALID_FIELED_IN_COMMAND			0x24
#define SCSI_ASC_PARAMETER_LIST_LENGTH_ERROR		0x1A
#define SCSI_ASC_INVALID_FIELD_IN_PARAMETER_LIST	0x26
#define SCSI_ASC_ADDRESS_OUT_OF_RANGE				0x21
#define SCSI_ASC_MEDIUM_NOT_PRESENT					0x3A
#define SCSI_ASC_MEDIUM_HAVE_CHANGED				0x28

enum SCSI_PDT_t
{
	SCSI_PDT_DIRECT_ACCESS_BLOCK					= 0x00,
	SCSI_PDT_CD_DVD									= 0x05,
};

struct SCSI_LUN_param_t
{
	const bool removable;
	const char vendor[8];
	const char product[16];
	const char revision[4];
	const enum SCSI_PDT_t type;
};

struct SCSI_LUN_status_t
{
	uint8_t sense_key;
	uint8_t asc;
	
	vsf_err_t status;
	
	enum SCSI_memstat_t
	{
		SCSI_MEMSTAT_NOINIT = 0,
		SCSI_MEMSTAT_WAITINIT,
		SCSI_MEMSTAT_POLL,
	} memstat;
	uint64_t page_num;
	enum SCSI_MAL_opt_t
	{
		SCSI_MAL_OPT_INIT = 0,
		SCSI_MAL_OPT_IO,
		SCSI_MAL_OPT_CHECKREADY,
	} mal_opt;
};

struct SCSI_LUN_info_t
{
	struct dal_info_t *dal_info;
	
	const struct SCSI_LUN_param_t param;
	struct SCSI_LUN_status_t status;
};

#define SCSI_HANDLER_NULL							{0, NULL, NULL}
struct SCSI_handler_t
{
	uint8_t operation_code;
	vsf_err_t (*handler)(struct SCSI_LUN_info_t *info, uint8_t CB[16], 
		struct vsf_buffer_t *buffer, uint32_t *page_size, uint32_t *page_num);
	vsf_err_t (*io)(struct SCSI_LUN_info_t *info, uint8_t CB[16], 
		struct vsf_buffer_t *buffer, uint32_t cur_page);
};

enum SCSI_errcode_t
{
	SCSI_ERRCODE_OK,
	SCSI_ERRCODE_FAIL,
	SCSI_ERRCODE_INVALID_PARAM,
	SCSI_ERRCODE_INVALID_COMMAND,
	SCSI_ERRCODE_NOT_READY,
};

vsf_err_t SCSI_Init(struct SCSI_LUN_info_t *info);
vsf_err_t SCSI_Poll(struct SCSI_LUN_info_t *info);
vsf_err_t SCSI_Handle(struct SCSI_handler_t *handlers, 
		struct SCSI_LUN_info_t *info, uint8_t CB[16], 
		struct vsf_buffer_t *buffer, uint32_t *page_size, uint32_t *page_num);
vsf_err_t SCSI_IO(struct SCSI_handler_t *handlers, 
		struct SCSI_LUN_info_t *info, uint8_t CB[16], 
		struct vsf_buffer_t *buffer, uint32_t cur_page);
enum SCSI_errcode_t SCSI_GetErrorCode(void);

#endif	// __SCSI_H_INCLUDED__

