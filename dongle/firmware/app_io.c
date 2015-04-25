/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       main.c                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    main.c file                                               *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include <stdlib.h>
#include <stdarg.h>

#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "scripts.h"
#include "interfaces.h"
#include "tool/buffer/buffer.h"
#include "dal/usart_stream/usart_stream.h"

#include "usb_protocol.h"

#include "vsprog/target/target_data.h"

#if HW_HAS_LCM
#include "vsprog_ui.h"
#endif

VSS_HANDLER(appio_set_dummy);
VSS_HANDLER(appio_ls);
VSS_HANDLER(appio_cat);

static const struct vss_cmd_t appio_cmd[] =
{
	VSS_CMD(	"dummy",
				"set dummy mode of appio, format: appio.dummy DUMMY",
				appio_set_dummy,
				NULL),
	VSS_CMD(	"ls",
				"list files in appio, format: appio.ls",
				appio_ls,
				NULL),
	VSS_CMD(	"cat",
				"display files in appio, format: appio.cat FILENAME",
				appio_cat,
				NULL),
	VSS_CMD_END
};
struct vss_cmd_list_t appio_cmd_list =
							VSS_CMD_LIST("appio", appio_cmd);

#if APPIO_DUMMY
static bool appio_dummy = true;
#else
static bool appio_dummy = false;
#endif

static uint8_t shell_buff_tx[64], shell_buff_rx[64];
struct usart_stream_info_t shell_stream =
{
	IFS_DUMMY_PORT,								// usart_index
	{
		{{shell_buff_rx, sizeof(shell_buff_rx)}}// fifo
	},											// struct vsf_stream_t stream_rx;
	{
		{{shell_buff_tx, sizeof(shell_buff_tx)}}// fifo
	}											// struct vsf_stream_t stream_tx;
};

static char app_io_local_buff[APPIO_BUFFER_SIZE];

static void app_io_out_sync(void)
{
	int free_space;
	
	do
	{
		usb_protocol_poll();
		free_space = vsf_fifo_get_data_length(&shell_stream.stream_rx.fifo);
	} while (free_space);
}

// virtual files
struct appio_file_t
{
	char *filename;
	uint8_t *addr;
	uint64_t size;
	
	// private
	uint64_t pos;
	FILE fn;
} static appio_filelist[8];

void APP_IO_INIT(void)
{
	int i;
	uint8_t ch;
	
#if HW_HAS_LCM
	vsprog_ui_init();
#endif
	
	// free filenames first
	for (i = 0; i < dimof(appio_filelist); i++)
	{
		if (appio_filelist[i].filename != NULL)
		{
			free(appio_filelist[i].filename);
			appio_filelist[i].filename = NULL;
		}
	}
	
	memset(appio_filelist, 0, sizeof(appio_filelist));
	// virtual file 0 is the main script file
	appio_filelist[0].filename = strdup(EVSPROG_SCRIPT_FILE);
	appio_filelist[0].addr = (uint8_t*)EVSPROG_SCRIPT_ADDR;
	// virtual file 1..n is the target script file
	for (i = 0; i < min(target_slotnum, dimof(appio_filelist) - 1); i++)
	{
		appio_filelist[i + 1].filename = strdup("/target0/script.vts");
		if (NULL == appio_filelist[i + 1].filename)
		{
			break;
		}
		// fix filename for the real target number
		appio_filelist[i + 1].filename[7] += i;
		appio_filelist[i + 1].addr = (uint8_t *)target_slot[i].script_base;
	}
	
	// calculate size for all virtual files
	for (i = 0;
		(i < dimof(appio_filelist)) && (appio_filelist[i].filename != NULL);
		i++)
	{
		appio_filelist[i].size = 0;
		for (ch = appio_filelist[i].addr[appio_filelist[i].size];
				(ch != 0x00) && (ch != 0xFF);
				ch = appio_filelist[i].addr[++appio_filelist[i].size]);
	}
}

void APP_IO_FINI(void)
{
	
}

static struct appio_file_t* appio_file_byname(char *filename)
{
	int i;
	for (i = 0;
		(i < dimof(appio_filelist)) && (appio_filelist[i].filename != NULL);
		i++)
	{
		if (!strcmp(filename, appio_filelist[i].filename))
		{
			return &appio_filelist[i];
		}
	}
	return NULL;
}
static struct appio_file_t* appio_file_byfn(FILE *fn)
{
	int i;
	for (i = 0;
		(i < dimof(appio_filelist)) && (appio_filelist[i].filename != NULL);
		i++)
	{
		if (fn == &appio_filelist[i].fn)
		{
			return &appio_filelist[i];
		}
	}
	return NULL;
}

FILE *FOPEN(const char *filename, const char *mode)
{
	struct appio_file_t *file = appio_file_byname((char *)filename);
	if (file != NULL)
	{
		file->pos = 0;
		return &file->fn;
	}
	return NULL;
}

int FCLOSE(FILE *f)
{
	if ((f != stdin) && (f != stdout) && (f != stderr))
	{
		struct appio_file_t *file = appio_file_byfn(f);
		if (file != NULL)
		{
			file->pos = 0;
			return 0;
		}
	}
	return 0;
}

int FEOF(FILE *f)
{
	if ((stdin == f) || (stdout == f) || (stderr == f))
	{
		return 0;
	}
	else
	{
		struct appio_file_t *file = appio_file_byfn(f);
		if (file != NULL)
		{
			return file->pos >= file->size ? 1 : 0;
		}
		return 1;
	}
}

void REWIND(FILE *f)
{
	if ((f != stdin) && (f != stdout) && (f != stderr))
	{
	}
	else
	{
		struct appio_file_t *file = appio_file_byfn(f);
		if (file != NULL)
		{
			file->pos = 0;
		}
	}
}

int FFLUSH(FILE *f)
{
	if ((stdout == f) || (stderr == f))
	{
		if (!appio_dummy)
		{
			app_io_out_sync();
		}
		return 0;
	}
	else if (stdin == f)
	{
		if (!appio_dummy)
		{
			uint32_t i, size = vsf_fifo_get_data_length(&shell_stream.stream_tx.fifo);
			for (i = 0; i < size; i++)
			{
				vsf_fifo_pop8(&shell_stream.stream_tx.fifo);
			}
		}
		return 0;
	}
	else
	{
		struct appio_file_t *file = appio_file_byfn(f);
		if (file != NULL)
		{
			// TODO: flush appio_file
		}
	}
	return 0;
}

int FGETC(FILE *f)
{
	if ((stdout == f) || (stderr == f))
	{
		return 0;
	}
	else if (stdin == f)
	{
		if (!appio_dummy)
		{
			uint32_t size;
			do
			{
				usb_protocol_poll();
				size = vsf_fifo_get_data_length(&shell_stream.stream_tx.fifo);
			} while (!size);
			return vsf_fifo_pop8(&shell_stream.stream_tx.fifo);
		}
	}
	else
	{
		struct appio_file_t *file = appio_file_byfn(f);
		if (file != NULL)
		{
			return file->pos >= file->size ? EOF : file->addr[file->pos++];
		}
	}
	return 0;
}

int GETCHAR(void)
{
	return FGETC(stdin);
}

char* FGETS(char *buf, int count, FILE *f)
{
	char cur_char, *result = buf;
	int size = 0, cur_size, pos;
	
	if ((NULL == buf) || (NULL == f) || (stdout == f) || (stderr == f))
	{
		return NULL;
	}
	
	if (stdin == f)
	{
		if (!appio_dummy)
		{
#if HW_HAS_LCM
			char vsprog_ui_buf[2];
#endif
			pos = 0;
			cur_char = '\0';
			while ((size < count) && (cur_char != '\r'))
			{
				usb_protocol_poll();
				cur_size = vsf_fifo_get_data_length(&shell_stream.stream_tx.fifo);
				
				while (cur_size && (size < count) && (cur_char != '\r'))
				{
					cur_char = (char)vsf_fifo_pop8(&shell_stream.stream_tx.fifo);
					if ('\r' == cur_char)
					{
						vsf_fifo_push8(&shell_stream.stream_rx.fifo, '\n');
#if HW_HAS_LCM
						vsprog_ui_print("\n\0");
#endif
					}
					else if ('\b' == cur_char)
					{
						if (pos)
						{
							vsf_fifo_push8(&shell_stream.stream_rx.fifo, '\b');
							vsf_fifo_push8(&shell_stream.stream_rx.fifo, ' ');
							vsf_fifo_push8(&shell_stream.stream_rx.fifo, '\b');
#if HW_HAS_LCM
							vsprog_ui_print("\b \b\0");
#endif
							pos--;
						}
						cur_size--;
						continue;
					}
					else if (!((cur_char >= ' ') && (cur_char <= '~')))
					{
						cur_size--;
						continue;
					}
					vsf_fifo_push8(&shell_stream.stream_rx.fifo, (uint8_t)cur_char);
#if HW_HAS_LCM
					vsprog_ui_buf[0] = cur_char;
					vsprog_ui_buf[1] = '\0';
					vsprog_ui_print(vsprog_ui_buf);
#endif
					buf[pos++] = cur_char;
					size++;
					cur_size--;
				}
			}
			buf[pos] = '\0';
			app_io_out_sync();
		}
		else
		{
			return NULL;
		}
	}
	else
	{
		struct appio_file_t *file = appio_file_byfn(f);
		if (file != NULL)
		{
			if (count < 3)
			{
				return NULL;
			}
			count -= 3;
			
			while ((file->addr[file->pos] != '\0') &&
					(file->addr[file->pos] != 0xFF) &&
					(file->pos < file->size) &&
					((file->addr[file->pos] == '\n') ||
						(file->addr[file->pos] == '\r')))
			{
				file->pos++;
			}
			while (count-- && 
					(file->pos < file->size) &&
					(file->addr[file->pos] != '\0') &&
					(file->addr[file->pos] != '\n') &&
					(file->addr[file->pos] != '\r') &&
					(file->addr[file->pos] != 0xFF))
			{
				*buf++ = file->addr[file->pos++];
			}
			if (result == buf)
			{
				return NULL;
			}
			*buf++ = '\n';
			*buf++ = '\r';
			*buf++ = '\0';
		}
	}
	return result;
}

static void APPIO_OUTBUFF(uint8_t *buff, uint32_t size)
{
	uint32_t free_space, cur_size;
	
#if HW_HAS_LCM
	vsprog_ui_print((char *)buff);
#endif
	
	if (appio_dummy)
	{
		return;
	}
	
	while (size > 0)
	{
		do
		{
			usb_protocol_poll();
			free_space = vsf_fifo_get_avail_length(&shell_stream.stream_rx.fifo);
		} while (!free_space);
		
		if (free_space > size)
		{
			cur_size = size;
		}
		else
		{
			cur_size = free_space;
		}
		
		vsf_fifo_push(&shell_stream.stream_rx.fifo, cur_size, buff);
		
		size -= cur_size;
		buff += cur_size;
	}
	
	app_io_out_sync();
}

int FPRINTF(FILE *f, const char *format, ...)
{
	int number = 0;
	char *pbuff = app_io_local_buff;
	va_list ap;
	
	if ((NULL == f) || (stdin == f) || (appio_file_byfn(f) != NULL))
	{
		return 0;
	}
	
	va_start(ap, format);
	number = vsnprintf(app_io_local_buff, sizeof(app_io_local_buff), format, ap);
	va_end(ap);
	
	if ((stdout == f) || (stderr == f))
	{
		APPIO_OUTBUFF((uint8_t *)pbuff, (uint32_t)number);
	}
	else
	{
	}
	return number;
}

int PRINTF(const char *format, ...)
{
	int number = 0;
	char *pbuff = app_io_local_buff;
	va_list ap;
	
	va_start(ap, format);
	number = vsnprintf(app_io_local_buff, sizeof(app_io_local_buff), format, ap);
	va_end(ap);

	APPIO_OUTBUFF((uint8_t *)pbuff, (uint32_t)number);
	return number;
}

VSS_HANDLER(appio_set_dummy)
{
	VSS_CHECK_ARGC(2);
	
	appio_dummy = (strtoul(argv[1], NULL, 0) != 0);
	return VSFERR_NONE;
}

VSS_HANDLER(appio_ls)
{
	int i;
	
	VSS_CHECK_ARGC(1);
	
	for (i = 0;
		(i < dimof(appio_filelist)) && (appio_filelist[i].filename != NULL);
		i++)
	{
		LOG_INFO("%s", appio_filelist[i].filename);
	}
	return VSFERR_NONE;
}

VSS_HANDLER(appio_cat)
{
	FILE *f = NULL;
	char line_buffer[VSS_CFG_MAX_LINE_LENGTH];
	
	VSS_CHECK_ARGC(2);
	
	f = FOPEN(argv[1], "rt");
	if (NULL == f)
	{
		LOG_INFO("file %s not found", argv[1]);
		return VSFERR_NONE;
	}
	
	REWIND(f);
	while (1)
	{
		if (NULL == FGETS(line_buffer, sizeof(line_buffer) - 1, f))
		{
			break;
		}
		line_buffer[sizeof(line_buffer) - 1] = '\0';
		
		PRINTF("%s", line_buffer);
	}
	return VSFERR_NONE;
}
