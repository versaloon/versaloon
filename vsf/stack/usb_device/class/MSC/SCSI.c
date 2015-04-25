#include "compiler.h"
#include "app_type.h"

#include "dal/mal/mal.h"
#include "SCSI.h"

static enum SCSI_errcode_t SCSI_errcode = SCSI_ERRCODE_OK;

static vsf_err_t SCSI_handler_TEST_UNIT_READY(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	if (info->status.memstat != SCSI_MEMSTAT_POLL)
	{
		info->status.sense_key = 0;
		info->status.asc = 0;
		SCSI_errcode = SCSI_ERRCODE_NOT_READY;
		return VSFERR_FAIL;
	}
	
	buffer->size = 0;
	*page_size = 0;
	
	info->status.sense_key = 0;
	info->status.asc = 0;
	SCSI_errcode = SCSI_ERRCODE_OK;
	return VSFERR_NONE;
}

static vsf_err_t SCSI_hadler_REQUEST_SENSE(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	uint8_t *pbuffer = buffer->buffer;
	uint32_t size;
	
	memset(pbuffer, 0, 18);
	pbuffer[0] = 0x70;
	pbuffer[2] = info->status.sense_key;
	pbuffer[7] = 0x0A;
	pbuffer[12] = info->status.asc;
	size = CB[4] <= 18 ? CB[4] : 18;
	buffer->size = size;
	*page_size = size;
	*page_num = 1;
	SCSI_errcode = SCSI_ERRCODE_OK;
	
	return VSFERR_NONE;
}

static vsf_err_t SCSI_handler_FORMAT_UNIT(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	buffer->size = 0;
	*page_size = 0;
	
	info->status.sense_key = 0;
	info->status.asc = 0;
	SCSI_errcode = SCSI_ERRCODE_OK;
	return VSFERR_NONE;
}

static vsf_err_t SCSI_handler_INQUIRY(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	uint8_t *pbuffer = buffer->buffer;
	
	if (CB[1] & 1)
	{
		// When the EVPD bit is set to one, 
		// the PAGE CODE field specifies which page of 
		// vital product data information the device server shall return
		if (0 == CB[2])
		{
			// 0x00: Supported VPD Pages
			memset(pbuffer, 0, 5);
			buffer->size = 5;
			*page_size = 5;
			*page_num = 1;
			SCSI_errcode = SCSI_ERRCODE_OK;
		}
		else
		{
			info->status.sense_key = SCSI_SENSEKEY_ILLEGAL_REQUEST;
			info->status.asc = SCSI_ASC_INVALID_FIELED_IN_COMMAND;
			SCSI_errcode = SCSI_ERRCODE_INVALID_PARAM;
			return VSFERR_FAIL;
		}
	}
	else
	{
		if (CB[2] != 0)
		{
			// If the PAGE CODE field is not set to zero 
			// when the EVPD bit is set to zero, 
			// the command shall be terminated with CHECK CONDITION status, 
			// with the sense key set to ILLEGAL REQUEST, 
			// and the additional sense code set to INVALID FIELD IN CDB.
			info->status.sense_key = SCSI_SENSEKEY_ILLEGAL_REQUEST;
			info->status.asc = SCSI_ASC_INVALID_FIELED_IN_COMMAND;
			SCSI_errcode = SCSI_ERRCODE_INVALID_PARAM;
			return VSFERR_FAIL;
		}
		// If the EVPD bit is set to zero, 
		// the device server shall return the standard INQUIRY data.
		memset(pbuffer, 0, 36);
		pbuffer[0] = info->param.type;
		if (info->param.removable)
		{
			pbuffer[1] = 0x80;
		}
		pbuffer[3] = 2;
		pbuffer[4] = 31;
		pbuffer += 8;
		memcpy(pbuffer, info->param.vendor, sizeof(info->param.vendor));
		pbuffer += sizeof(info->param.vendor);
		memcpy(pbuffer, info->param.product, sizeof(info->param.product));
		pbuffer += sizeof(info->param.product);
		memcpy(pbuffer, info->param.revision, sizeof(info->param.revision));
		buffer->size = 36;
		*page_size = 36;
		*page_num = 1;
		SCSI_errcode = SCSI_ERRCODE_OK;
	}
	
	return VSFERR_NONE;
}

static vsf_err_t SCSI_handler_MODE_SENSE6(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	if (info->status.memstat != SCSI_MEMSTAT_POLL)
	{
		info->status.sense_key = SCSI_SENSEKEY_NOT_READY;
		info->status.asc = 0;
		SCSI_errcode = SCSI_ERRCODE_FAIL;
		return VSFERR_FAIL;
	}
	
	memset(buffer->buffer, 0, 4);
	buffer->buffer[0] = 3;
	buffer->size = 4;
	*page_size = 4;
	*page_num = 1;
	
	info->status.sense_key = 0;
	info->status.asc = 0;
	SCSI_errcode = SCSI_ERRCODE_OK;
	return VSFERR_NONE;
}

static vsf_err_t SCSI_handler_START_STOP_UNIT(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	buffer->size = 0;
	*page_size = 0;
	
	info->status.sense_key = 0;
	info->status.asc = 0;
	SCSI_errcode = SCSI_ERRCODE_OK;
	return VSFERR_NONE;
}

static vsf_err_t SCSI_handler_ALLOW_MEDIUM_REMOVAL(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	buffer->size = 0;
	*page_size = 0;
	
	info->status.sense_key = 0;
	info->status.asc = 0;
	SCSI_errcode = SCSI_ERRCODE_OK;
	return VSFERR_NONE;
}

static vsf_err_t SCSI_handler_READ_FORMAT_CAPACITIES(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	struct mal_info_t *mal_info = (struct mal_info_t *)info->dal_info->extra;
	uint32_t block_number = (uint32_t)mal_info->capacity.block_number;
	uint32_t block_size = (uint32_t)mal_info->capacity.block_size;
	
	if (info->status.memstat != SCSI_MEMSTAT_POLL)
	{
		info->status.sense_key = SCSI_SENSEKEY_NOT_READY;
		info->status.asc = 0;
		SCSI_errcode = SCSI_ERRCODE_FAIL;
		return VSFERR_FAIL;
	}
	
	buffer->buffer[3] = 8;
	SET_BE_U32(&buffer->buffer[4], block_number);
	SET_BE_U32(&buffer->buffer[8], block_size);
	buffer->buffer[8] = 2;
	buffer->size = 12;
	*page_size = 12;
	*page_num = 1;
	
	info->status.sense_key = 0;
	info->status.asc = 0;
	SCSI_errcode = SCSI_ERRCODE_OK;
	return VSFERR_NONE;
}

static vsf_err_t SCSI_handler_READ_CAPACITY10(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	struct mal_info_t *mal_info = (struct mal_info_t *)info->dal_info->extra;
	uint32_t block_number = (uint32_t)mal_info->capacity.block_number;
	uint32_t block_size = (uint32_t)mal_info->capacity.block_size;
	
	if (info->status.memstat != SCSI_MEMSTAT_POLL)
	{
		info->status.sense_key = SCSI_SENSEKEY_NOT_READY;
		info->status.asc = 0;
		SCSI_errcode = SCSI_ERRCODE_FAIL;
		return VSFERR_FAIL;
	}
	
	SET_BE_U32(&buffer->buffer[0], block_number - 1);
	SET_BE_U32(&buffer->buffer[4], block_size);
	buffer->size = 8;
	*page_size = 8;
	*page_num = 1;
	
	info->status.sense_key = 0;
	info->status.asc = 0;
	SCSI_errcode = SCSI_ERRCODE_OK;
	return VSFERR_NONE;
}

static vsf_err_t SCSI_io_WRITE10(struct SCSI_LUN_info_t *info, uint8_t CB[16], 
		struct vsf_buffer_t *buffer, uint32_t cur_page)
{
	struct mal_info_t *mal_info = (struct mal_info_t *)info->dal_info->extra;
	uint32_t lba = GET_BE_U32(&CB[2]);
	uint32_t block_size = (uint32_t)mal_info->capacity.block_size;
	vsf_err_t err;
	
	switch (info->status.mal_opt)
	{
	case SCSI_MAL_OPT_INIT:
		if (mal.writeblock_nb_start(info->dal_info, lba * block_size,
									info->status.page_num, buffer->buffer))
		{
			info->status.page_num = 0;
			info->status.memstat = SCSI_MEMSTAT_NOINIT;
			info->status.sense_key = SCSI_SENSEKEY_HARDWARE_ERROR;
			info->status.asc = 0;
			SCSI_errcode = SCSI_ERRCODE_FAIL;
			return VSFERR_FAIL;
		}
		info->status.mal_opt = SCSI_MAL_OPT_IO;
	case SCSI_MAL_OPT_IO:
		if (mal.writeblock_nb(info->dal_info, (lba + cur_page) * block_size,
								buffer->buffer))
		{
			info->status.page_num = 0;
			info->status.memstat = SCSI_MEMSTAT_NOINIT;
			info->status.sense_key = SCSI_SENSEKEY_HARDWARE_ERROR;
			info->status.asc = 0;
			SCSI_errcode = SCSI_ERRCODE_FAIL;
			return VSFERR_FAIL;
		}
		info->status.mal_opt = SCSI_MAL_OPT_CHECKREADY;
	case SCSI_MAL_OPT_CHECKREADY:
		err = mal.writeblock_nb_isready(info->dal_info,
								(lba + cur_page) * block_size, buffer->buffer);
		if (err)
		{
			if (err < 0)
			{
				info->status.page_num = 0;
				info->status.memstat = SCSI_MEMSTAT_NOINIT;
				info->status.sense_key = SCSI_SENSEKEY_HARDWARE_ERROR;
				info->status.asc = 0;
				SCSI_errcode = SCSI_ERRCODE_FAIL;
			}
			return err;
		}
		if (cur_page >= (info->status.page_num - 1))
		{
			info->status.page_num = 0;
			if (mal.writeblock_nb_end(info->dal_info))
			{
				info->status.page_num = 0;
				info->status.memstat = SCSI_MEMSTAT_NOINIT;
				info->status.sense_key = SCSI_SENSEKEY_HARDWARE_ERROR;
				info->status.asc = 0;
				SCSI_errcode = SCSI_ERRCODE_FAIL;
				return VSFERR_FAIL;
			}
		}
		else
		{
			info->status.mal_opt = SCSI_MAL_OPT_IO;
		}
		break;
	}
	
	info->status.sense_key = 0;
	info->status.asc = 0;
	SCSI_errcode = SCSI_ERRCODE_OK;
	return VSFERR_NONE;
}

static vsf_err_t SCSI_handler_WRITE10(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	struct mal_info_t *mal_info = (struct mal_info_t *)info->dal_info->extra;
	uint32_t block_size = (uint32_t)mal_info->capacity.block_size;
	uint16_t num_of_page = GET_BE_U16(&CB[7]);
	
	if (info->status.memstat != SCSI_MEMSTAT_POLL)
	{
		info->status.sense_key = SCSI_SENSEKEY_NOT_READY;
		info->status.asc = 0;
		SCSI_errcode = SCSI_ERRCODE_FAIL;
		return VSFERR_FAIL;
	}
	
	buffer->size = 0;
	*page_size = block_size;
	*page_num = num_of_page;
	
	info->status.page_num = num_of_page;
	info->status.mal_opt = SCSI_MAL_OPT_INIT;
	
	info->status.sense_key = 0;
	info->status.asc = 0;
	SCSI_errcode = SCSI_ERRCODE_OK;
	return VSFERR_NONE;
}

static vsf_err_t SCSI_io_READ10(struct SCSI_LUN_info_t *info, uint8_t CB[16], 
		struct vsf_buffer_t *buffer, uint32_t cur_page)
{
	struct mal_info_t *mal_info = (struct mal_info_t *)info->dal_info->extra;
	uint32_t lba = GET_BE_U32(&CB[2]);
	uint32_t block_size = (uint32_t)mal_info->capacity.block_size;
	vsf_err_t err;
	
	switch (info->status.mal_opt)
	{
	case SCSI_MAL_OPT_INIT:
		if (mal.readblock_nb_start(info->dal_info, lba * block_size,
									info->status.page_num, buffer->buffer))
		{
			info->status.page_num = 0;
			info->status.memstat = SCSI_MEMSTAT_NOINIT;
			info->status.sense_key = SCSI_SENSEKEY_HARDWARE_ERROR;
			info->status.asc = 0;
			SCSI_errcode = SCSI_ERRCODE_FAIL;
			return VSFERR_FAIL;
		}
		info->status.mal_opt = SCSI_MAL_OPT_CHECKREADY;
	case SCSI_MAL_OPT_CHECKREADY:
		err = mal.readblock_nb_isready(info->dal_info,
								(lba + cur_page) * block_size, buffer->buffer);
		if (err)
		{
			if (err < 0)
			{
				info->status.page_num = 0;
				info->status.memstat = SCSI_MEMSTAT_NOINIT;
				info->status.sense_key = SCSI_SENSEKEY_HARDWARE_ERROR;
				info->status.asc = 0;
				SCSI_errcode = SCSI_ERRCODE_FAIL;
			}
			return err;
		}
		info->status.mal_opt = SCSI_MAL_OPT_IO;
	case SCSI_MAL_OPT_IO:
		if (mal.readblock_nb(info->dal_info, (lba + cur_page) * block_size,
								buffer->buffer))
		{
			info->status.page_num = 0;
			info->status.memstat = SCSI_MEMSTAT_NOINIT;
			info->status.sense_key = SCSI_SENSEKEY_HARDWARE_ERROR;
			info->status.asc = 0;
			SCSI_errcode = SCSI_ERRCODE_FAIL;
			return VSFERR_FAIL;
		}
		if (cur_page >= (info->status.page_num - 1))
		{
			info->status.page_num = 0;
			if (mal.readblock_nb_end(info->dal_info))
			{
				info->status.page_num = 0;
				info->status.memstat = SCSI_MEMSTAT_NOINIT;
				info->status.sense_key = SCSI_SENSEKEY_HARDWARE_ERROR;
				info->status.asc = 0;
				SCSI_errcode = SCSI_ERRCODE_FAIL;
				return VSFERR_FAIL;
			}
		}
		else
		{
			info->status.mal_opt = SCSI_MAL_OPT_CHECKREADY;
		}
		break;
	}
	
	info->status.sense_key = 0;
	info->status.asc = 0;
	SCSI_errcode = SCSI_ERRCODE_OK;
	return VSFERR_NONE;
}
static vsf_err_t SCSI_handler_READ10(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	struct mal_info_t *mal_info = (struct mal_info_t *)info->dal_info->extra;
	uint32_t block_size = (uint32_t)mal_info->capacity.block_size;
	uint16_t num_of_page = GET_BE_U16(&CB[7]);
	
	if (info->status.memstat != SCSI_MEMSTAT_POLL)
	{
		info->status.sense_key = SCSI_SENSEKEY_NOT_READY;
		info->status.asc = 0;
		SCSI_errcode = SCSI_ERRCODE_FAIL;
		return VSFERR_FAIL;
	}
	
	buffer->size = 0;
	*page_size = block_size;
	*page_num = num_of_page;
	
	info->status.page_num = num_of_page;
	info->status.mal_opt = SCSI_MAL_OPT_INIT;
	
	info->status.sense_key = 0;
	info->status.asc = 0;
	SCSI_errcode = SCSI_ERRCODE_OK;
	return VSFERR_NONE;
}

static vsf_err_t SCSI_handler_VERIFY10(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	buffer->size = 0;
	*page_size = 0;
	
	info->status.sense_key = 0;
	info->status.asc = 0;
	SCSI_errcode = SCSI_ERRCODE_OK;
	return VSFERR_NONE;
}

static vsf_err_t SCSI_handler_READ_TOC(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	info->status.sense_key = SCSI_SENSEKEY_NOT_READY;
	info->status.asc = SCSI_ASC_MEDIUM_NOT_PRESENT;
	SCSI_errcode = SCSI_ERRCODE_OK;
	return VSFERR_NONE;
}

static vsf_err_t SCSI_handler_GET_EVENT_STATUS_NOTIFICATION(
		struct SCSI_LUN_info_t *info, uint8_t CB[16], 
		struct vsf_buffer_t *buffer, uint32_t *page_size, uint32_t *page_num)
{
	info->status.sense_key = SCSI_SENSEKEY_NOT_READY;
	info->status.asc = SCSI_ASC_MEDIUM_NOT_PRESENT;
	SCSI_errcode = SCSI_ERRCODE_OK;
	return VSFERR_NONE;
}

static vsf_err_t SCSI_handler_MODE_SENSE10(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	memset(buffer->buffer, 0, 8);
	buffer->buffer[1] = 6;
	buffer->size = 8;
	*page_size = 8;
	*page_num = 1;
	
	info->status.sense_key = 0;
	info->status.asc = 0;
	SCSI_errcode = SCSI_ERRCODE_OK;
	return VSFERR_NONE;
}

static const struct SCSI_handler_t SCSI_handlers[] = 
{
	{
		SCSI_CMD_TEST_UNIT_READY,
		SCSI_handler_TEST_UNIT_READY,
		NULL
	},
	{
		SCSI_CMD_REQUEST_SENSE,
		SCSI_hadler_REQUEST_SENSE,
		NULL
	},
	{
		SCSI_CMD_FORMAT_UNIT,
		SCSI_handler_FORMAT_UNIT,
		NULL
	},
	{
		SCSI_CMD_INQUIRY,
		SCSI_handler_INQUIRY,
		NULL
	},
	{
		SCSI_CMD_MODE_SENSE6,
		SCSI_handler_MODE_SENSE6,
		NULL
	},
	{
		SCSI_CMD_START_STOP_UNIT,
		SCSI_handler_START_STOP_UNIT,
		NULL
	},
	{
		SCSI_CMD_ALLOW_MEDIUM_REMOVAL,
		SCSI_handler_ALLOW_MEDIUM_REMOVAL,
		NULL
	},
	{
		SCSI_CMD_READ_FORMAT_CAPACITIES,
		SCSI_handler_READ_FORMAT_CAPACITIES,
		NULL
	},
	{
		SCSI_CMD_READ_CAPACITY10,
		SCSI_handler_READ_CAPACITY10,
		NULL
	},
	{
		SCSI_CMD_READ10,
		SCSI_handler_READ10,
		SCSI_io_READ10
	},
	{
		SCSI_CMD_WRITE10,
		SCSI_handler_WRITE10,
		SCSI_io_WRITE10
	},
	{
		SCSI_CMD_VERIFY10,
		SCSI_handler_VERIFY10,
		NULL
	},
	{
		SCSI_CMD_READ_TOC,
		SCSI_handler_READ_TOC,
		NULL
	},
	{
		SCSI_CMD_GET_EVENT_STATUS_NOTIFICATION,
		SCSI_handler_GET_EVENT_STATUS_NOTIFICATION,
		NULL
	},
	{
		SCSI_CMD_MODE_SENSE10,
		SCSI_handler_MODE_SENSE10,
		NULL
	},
	SCSI_HANDLER_NULL
};

static struct SCSI_handler_t* SCSI_get_handler(struct SCSI_handler_t *handlers, 
												uint8_t operation_code)
{
	while (handlers->handler != NULL)
	{
		if (handlers->operation_code == operation_code)
		{
			return handlers;
		}
		handlers++;
	}
	return NULL;
}

vsf_err_t SCSI_Init(struct SCSI_LUN_info_t *info)
{
	info->status.page_num = 0;
	return VSFERR_NONE;
}

vsf_err_t SCSI_Poll(struct SCSI_LUN_info_t *info)
{
	vsf_err_t err;
	
	switch (info->status.memstat)
	{
	case SCSI_MEMSTAT_NOINIT:
		err = mal.init_nb(info->dal_info);
		if (!err)
		{
			info->status.memstat = SCSI_MEMSTAT_WAITINIT;
		}
		break;
	case SCSI_MEMSTAT_WAITINIT:
		err = mal.init_nb_isready(info->dal_info);
		if (!err)
		{
			info->status.memstat = SCSI_MEMSTAT_POLL;
		}
		else if (err < 0)
		{
			info->status.memstat = SCSI_MEMSTAT_NOINIT;
		}
		break;
	case SCSI_MEMSTAT_POLL:
		// poll if no transaction
		if (!info->status.page_num)
		{
			
		}
		break;
	}
	return VSFERR_NONE;
}

vsf_err_t SCSI_Handle(struct SCSI_handler_t *handlers, 
		struct SCSI_LUN_info_t *info, uint8_t CB[16], 
		struct vsf_buffer_t *buffer, uint32_t *page_size, uint32_t *page_num)
{
	if (NULL == handlers)
	{
		handlers = (struct SCSI_handler_t *)SCSI_handlers;
	}
	handlers = SCSI_get_handler(handlers, CB[0]);
	if (NULL == handlers)
	{
		SCSI_errcode = SCSI_ERRCODE_INVALID_COMMAND;
		return VSFERR_FAIL;
	}
	
	SCSI_errcode = SCSI_ERRCODE_OK;
	return handlers->handler(info, CB, buffer, page_size, page_num);
}

vsf_err_t SCSI_IO(struct SCSI_handler_t *handlers, 
		struct SCSI_LUN_info_t *info, uint8_t CB[16], 
		struct vsf_buffer_t *buffer, uint32_t cur_page)
{
	if (NULL == handlers)
	{
		handlers = (struct SCSI_handler_t *)SCSI_handlers;
	}
	handlers = SCSI_get_handler(handlers, CB[0]);
	if ((NULL == handlers) || (NULL == handlers->io))
	{
		SCSI_errcode = SCSI_ERRCODE_INVALID_COMMAND;
		return VSFERR_FAIL;
	}
	
	SCSI_errcode = SCSI_ERRCODE_OK;
	return handlers->io(info, CB, buffer, cur_page);
}

enum SCSI_errcode_t SCSI_GetErrorCode(void)
{
	return SCSI_errcode;
}
