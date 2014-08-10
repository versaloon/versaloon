/***************************************************************************
 *   Copyright (C) 2009 - 2010 by Simon Qian <SimonQian@SimonQian.com>     *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "port.h"
#include "app_cfg.h"
#if TARGET_COMISP_EN && TARGET_LPC1000_EN
#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"
#include "scripts.h"

#include "comisp.h"
#include "lpcarmisp.h"

#include "comisp_internal.h"
#include "lpc1000_internal.h"

#include "comport.h"

ENTER_PROGRAM_MODE_HANDLER(lpcarmisp);
LEAVE_PROGRAM_MODE_HANDLER(lpcarmisp);
ERASE_TARGET_HANDLER(lpcarmisp);
WRITE_TARGET_HANDLER(lpcarmisp);
READ_TARGET_HANDLER(lpcarmisp);
struct program_functions_t lpcarmisp_program_functions =
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(lpcarmisp),
	LEAVE_PROGRAM_MODE_FUNCNAME(lpcarmisp),
	ERASE_TARGET_FUNCNAME(lpcarmisp),
	WRITE_TARGET_FUNCNAME(lpcarmisp),
	READ_TARGET_FUNCNAME(lpcarmisp)
};

static char uuchar(uint8_t value)
{
	value = (value & 0x3F) + 32;
	return (value == 32) ? 96 : value;
}

static vsf_err_t uuencode(char *str, uint8_t *data, uint8_t len)
{
	uint8_t i;
	uint32_t value;
	
	*str++ = uuchar(len);
	len = (len + 2) / 3;
	for (i = 0; i < len; i++)
	{
		value = (data[0] << 16) + (data[1] << 8) + data[2];
		*str++ = uuchar((uint8_t)(value >> 18));
		*str++ = uuchar((uint8_t)(value >> 12));
		*str++ = uuchar((uint8_t)(value >> 6));
		*str++ = uuchar((uint8_t)(value >> 0));
		data += 3;
	}
	*str = '\0';
	return VSFERR_NONE;
}

static vsf_err_t uudecode(char *str, uint8_t *data, uint8_t *len)
{
	uint8_t i, loop;
	uint32_t value;
	uint8_t want_len = *len;
	
	if ((NULL == str) || (NULL == data) || (NULL == len) || (0 == strlen(str))
		|| (str[0] < 33) || (str[0] > 77))
	{
		return VSFERR_FAIL;
	}
	for (i = 0; i < strlen(str); i++)
	{
		if ((str[i] < 32) || (str[i] > 96))
		{
			return VSFERR_FAIL;
		}
		str[i] -= 32;
	}
	
	*len = *str++;
	loop = (*len + 2) / 3;
	for (i = 0; i < loop; i++)
	{
		value = ((str[0] & 0x3F) << 18) + ((str[1] & 0x3F) << 12)
				+ ((str[2] & 0x3F) << 6) + ((str[3] & 0x3F) << 0);
		if (want_len)
		{
			want_len--;
			*data++ = (value >> 16) & 0xFF;
		}
		if (want_len)
		{
			want_len--;
			*data++ = (value >> 8) & 0xFF;
		}
		if (want_len)
		{
			want_len--;
			*data++ = (value >> 0) & 0xFF;
		}
		str += 4;
	}
	return VSFERR_NONE;
}

static vsf_err_t uu_send(uint8_t *data, uint32_t size)
{
	// max format: data \r checksum \r
	char str[61 + 1 + 10 + 1], sum[10];
	uint8_t cur_size, sum_reply;
	int32_t comm_ret;
	uint8_t line_num;
	uint32_t sum_start_size;
	uint8_t *sum_start_pos;
	uint8_t i;
	uint32_t checksum;
	
	line_num = 0;
	sum_start_pos = NULL;
	sum_start_size = size;
	checksum = 0;
	while (size > 0)
	{
		if (NULL == sum_start_pos)
		{
			sum_start_pos = data;
			sum_start_size = size;
			checksum = 0;
		}
		
		cur_size = (uint8_t)((size > 45) ? 45 : size);
		size -= cur_size;
		
		for (i = 0; i < cur_size; i++)
		{
			checksum += data[i];
		}
		
		uuencode(str, data, cur_size);
		strcat(str, "\r");
		sum_reply = 0;
		if (!size || (++line_num >= 20))
		{
			sum_reply = 1;
			snprintf(sum, sizeof(sum), "%d", (int)checksum);
			strcat(str, sum);
			strcat(str, "\r");
		}
		comm_ret = comm_write((uint8_t *)str, strlen(str));
		if (comm_ret != (int32_t)strlen(str))
		{
			return VSFERR_FAIL;
		}
		data += cur_size;
		
		if (sum_reply)
		{
			line_num = 0;
			comm_ret = comm_read((uint8_t *)sum, 4);
			if (comm_ret != 4)
			{
				return VSFERR_FAIL;
			}
			sum[comm_ret] = '\0';
			if ((sum[comm_ret - 2] != '\r') || (sum[comm_ret - 1] != '\n'))
			{
				int32_t comm_ret1;
				
				comm_ret1 = comm_read((uint8_t *)&sum[comm_ret], 4);
				if ((comm_ret1 < 0) || (sum[comm_ret + comm_ret1 - 2] != '\r')
					|| (sum[comm_ret + comm_ret1 - 1] != '\n'))
				{
					return VSFERR_FAIL;
				}
				sum[comm_ret + comm_ret1] = '\0';
			}
			
			if (!strcmp(sum, "OK\r\n"))
			{
				
			}
			else if (!strcmp(sum, "RESEND\r\n"))
			{
				data = sum_start_pos;
				size = sum_start_size;
			}
			else
			{
				return VSFERR_FAIL;
			}
			sum_start_pos = NULL;
		}
	}
	return VSFERR_NONE;
}

static vsf_err_t uu_recv(uint8_t *data, uint32_t size)
{
	// max format: data \r\n checksum \r\n
	char str[61 + 2 + 10 + 2];
	char sum[10];
	uint8_t cur_size, len_tmp;
	int32_t comm_ret;
	uint32_t comm_data_size, comm_sum_size;
	uint8_t line_num;
	uint32_t sum_start_size;
	uint8_t *sum_start_pos;
	uint8_t i;
	uint32_t checksum;
	
	line_num = 0;
	sum_start_pos = NULL;
	sum_start_size = size;
	checksum = 0;
	while (size > 0)
	{
		if (NULL == sum_start_pos)
		{
			sum_start_pos = data;
			sum_start_size = size;
			checksum = 0;
		}
		
		cur_size = (uint8_t)((size > 45) ? 45 : size);
		size -= cur_size;
		
		comm_data_size = 1 + 2 + (cur_size + 2) / 3 * 4;
		comm_sum_size = 0;
		if (!size || (++line_num >= 20))
		{
			snprintf(sum, sizeof(sum), "%d", (int)checksum);
			comm_sum_size = 2 + strlen(sum);
		}
		comm_ret = comm_read((uint8_t *)str, comm_data_size + comm_sum_size);
		if (comm_ret < (int32_t)comm_data_size)
		{
			return VSFERR_FAIL;
		}
		if ((str[comm_ret - 2] != '\r') || (str[comm_ret - 1] != '\n'))
		{
			int32_t comm_ret1;
			
			comm_ret1 = comm_read((uint8_t *)&str[comm_ret], 10);
			if ((comm_ret1 < 0) || (str[comm_ret + comm_ret1 - 2] != '\r')
				|| (str[comm_ret + comm_ret1 - 1] != '\n'))
			{
				return VSFERR_FAIL;
			}
		}
		
		// remove \r\n
		str[comm_data_size - 2] = '\0';
		len_tmp = cur_size;
		if (uudecode(str, data, &len_tmp) || (len_tmp != cur_size))
		{
			return VSFERR_FAIL;
		}
		for (i = 0; i < cur_size; i++)
		{
			checksum += data[i];
		}
		data += cur_size;
		
		if (comm_sum_size)
		{
			uint32_t checksum_tmp;
			
			line_num = 0;
			checksum_tmp = strtoul(&str[comm_data_size], NULL, 0);
			
			if (checksum == checksum_tmp)
			{
				strcpy(str, "OK\r");
			}
			else
			{
				strcpy(str, "RESEND\r");
				data = sum_start_pos;
				size = sum_start_size;
			}
			comm_ret = comm_write((uint8_t *)str, strlen(str));
			if (comm_ret != (int32_t)strlen(str))
			{
				return VSFERR_FAIL;
			}
			sum_start_pos = NULL;
		}
	}
	return VSFERR_NONE;
}

#define LPCARMISP_CMD_ABORT				0x1B
#define LPCARMISP_CMD_SYNC				"?"
#define LPCARMISP_CMD_SYNCED			"Synchronized\r"
#define LPCARMISP_RPL_SYNCED			"Synchronized\r\n"
#define LPCARMISP_RPL_OK				"OK\r\n"
#define LPCARMISP_CMD_UNLOCK			"U"
#define LPCARMISP_CMD_BAUDRATE			"B"
#define LPCARMISP_CMD_ECHO				"A"
#define LPCARMISP_CMD_WRITE2RAM			"W"
#define LPCARMISP_CMD_READMEMORY		"R"
#define LPCARMISP_CMD_PREPARESECTORS	"P"
#define LPCARMISP_CMD_RAM2FLASH			"C"
#define LPCARMISP_CMD_ERASESECTORS		"E"
#define LPCARMISP_CMD_READCHIPID		"J"
#define LPCARMISP_CMD_READBOOTVER		"K"
#define LPCARMISP_CMD_READSERIAL		"N"
#define LPCARMISP_RPL_SUCCESS			"0\r\n"

static vsf_err_t lpcarmisp_transact(char *buff, uint8_t echo, char *reply,
									uint32_t *reply_size)
{
	int32_t comm_ret;
	
	if ((NULL == reply_size) || (NULL == buff) || (0 == strlen(buff)))
	{
		LOG_BUG(ERRMSG_INVALID_TARGET, "parameters");
		return VSFERR_FAIL;
	}
	
	comm_ret = comm_write((uint8_t *)buff, strlen(buff));
	if (comm_ret != (int32_t)strlen(buff))
	{
		*reply_size = 0;
		LOG_DEBUG(ERRMSG_FAILURE_HANDLE_DEVICE, "write", buff);
		return VSFERR_FAIL;
	}
	
	if ((reply != NULL) && (*reply_size > 0))
	{
		uint32_t reply_pos, reply_len;
		
		reply_pos = 0;
		reply_len = *reply_size;
		if (echo)
		{
			// '\n' is not sent when echoing command
			reply_pos += strlen(buff);
			reply_len += strlen(buff);
		}
		
		comm_ret = comm_read((uint8_t *)reply, reply_len);
		if (comm_ret < 0)
		{
			*reply_size = 0;
			LOG_DEBUG(ERRMSG_FAILURE_OPERATE_DEVICE, com_mode.comport);
			return ERRCODE_FAILURE_OPERATION;
		}
		if (comm_ret != (int32_t)reply_len)
		{
			*reply_size = (uint32_t)comm_ret;
			reply[comm_ret] = '\0';
			LOG_DEBUG(ERRMSG_FAILURE_HANDLE_DEVICE, "receive reply of", buff);
			return VSFERR_FAIL;
		}
		reply[reply_len] = '\0';
		
		if (echo && (strstr(reply, buff) != reply))
		{
			*reply_size = 0;
			LOG_DEBUG(ERRMSG_FAILURE_HANDLE_DEVICE, "receive echo of", buff);
			return VSFERR_FAIL;
		}
		
		strcpy(reply, &reply[reply_pos]);
	}
	return VSFERR_NONE;
}

static uint8_t echo;
static uint8_t cmd_argc, reply_argc;
static char *cmd_argv[4], *reply_argv[4];
static char reply_buff[64];
static char cmd_buff[64];
static vsf_err_t lpcarmisp_process(uint8_t cmd_argc, char **cmd_argv,
				uint32_t *reply_size, uint8_t *reply_argc, char **reply_argv)
{
	uint8_t i, new;
	uint32_t len;
	
	if ((0 == cmd_argc) || (NULL == cmd_argv) || (NULL == reply_size)
		|| (NULL == reply_argc) || (NULL == reply_argv))
	{
		return VSFERR_FAIL;
	}
	
	strcpy(cmd_buff, "");
	for (i = 0; i < cmd_argc; i++)
	{
		strcat(cmd_buff, cmd_argv[i]);
		if (i < (cmd_argc - 1))
		{
			strcat(cmd_buff, " ");
		}
	}
	strcat(cmd_buff, "\r");
	
	lpcarmisp_transact(cmd_buff, echo, reply_buff, reply_size);
	if (!*reply_size)
	{
		return VSFERR_FAIL;
	}
	
	// result
	if (strstr(reply_buff, LPCARMISP_RPL_SUCCESS) != reply_buff)
	{
		LOG_BUF(reply_buff, *reply_size, LOG_DEBUG, "%c", 255);
		LOG_ERROR(ERRMSG_FAILURE_OPERATION_ERRCODE, "get success sesult",
					(int)strtoul(reply_buff, NULL, 0));
		return VSFERR_FAIL;
	}
	
	// parse reply argc and argv
	*reply_argc = 0;
	len = strlen(reply_buff);
	new = 1;
	for (i = 3; i < len; i++)
	{
		if ((reply_buff[i] != '\n') && (reply_buff[i] != '\r'))
		{
			if (new)
			{
				new = 0;
				reply_argv[(*reply_argc)++] = &reply_buff[i];
			}
		}
		else
		{
			new = 1;
			reply_buff[i] = '\0';
		}
	}
	return VSFERR_NONE;
}

static vsf_err_t lpcarmisp_abort(void)
{
	char cmd[2] = {LPCARMISP_CMD_ABORT, 0};
	uint32_t reply_size = 0;
	vsf_err_t err;
	
	err = lpcarmisp_transact(cmd, 1, NULL, &reply_size);
	comm_flush();
	return err;
}

static vsf_err_t lpcarmisp_sync(uint32_t kernel_khz)
{
	char buffer[32];
	uint32_t reply_size;
	
	if ((com_mode.auxpin != 'N') && (com_mode.auxpin != 'n'))
	{
		// DTR <==> Reset
		// RTS <==> BOOT
		
		// Reset:0, BOOT:0
		comm_ctrl(0, 0);
		sleep_ms(10);
		// Reset:1, BOOT:0
		comm_ctrl(1, 0);
		sleep_ms(10);
	}
	
	comm_flush();
	
	reply_size = strlen(LPCARMISP_RPL_SYNCED);
	if (lpcarmisp_transact(LPCARMISP_CMD_SYNC, 0, buffer, &reply_size))
	{
//		if (!reply_size && strcmp(buffer, LPCARMISP_RPL_OK)
//			&& strcmp(buffer, LPCARMISP_RPL_SUCCESS))
		{
			lpcarmisp_abort();
		}
		return VSFERR_NONE;
	}
	if (strcmp(buffer, LPCARMISP_RPL_SYNCED))
	{
		LOG_DEBUG(ERRMSG_FAILURE_OPERATION_ERRSTRING,
					"receive sync reply", buffer);
		return VSFERR_FAIL;
	}
	
	reply_size = strlen(LPCARMISP_RPL_OK);
	if (lpcarmisp_transact(LPCARMISP_CMD_SYNCED, 1, buffer, &reply_size))
	{
		LOG_DEBUG(ERRMSG_FAILURE_OPERATION, "run synced command");
		return ERRCODE_FAILURE_OPERATION;
	}
	if (strcmp(buffer, LPCARMISP_RPL_OK))
	{
		LOG_DEBUG(ERRMSG_FAILURE_OPERATION_ERRSTRING,
					"receive synced reply", buffer);
		return VSFERR_FAIL;
	}
	
	sprintf(cmd_buff, "%d\r", (int)kernel_khz);
	reply_size = strlen(LPCARMISP_RPL_OK);
	if (lpcarmisp_transact(cmd_buff, 1, buffer, &reply_size))
	{
		LOG_DEBUG(ERRMSG_FAILURE_OPERATION, "run kernel_khz command");
		return ERRCODE_FAILURE_OPERATION;
	}
	if (strcmp(buffer, LPCARMISP_RPL_OK))
	{
		LOG_DEBUG(ERRMSG_FAILURE_OPERATION_ERRSTRING,
					"receive synced reply", buffer);
		return VSFERR_FAIL;
	}

	return VSFERR_NONE;
}

static vsf_err_t lpcarmisp_unlock(void)
{
	uint32_t reply_size;
	
	cmd_argv[0] = LPCARMISP_CMD_UNLOCK;
	cmd_argv[1] = "23130";
	cmd_argc = 2;
	reply_size = strlen(LPCARMISP_RPL_SUCCESS);
	if (lpcarmisp_process(cmd_argc, (char **)cmd_argv, &reply_size, &reply_argc,
							(char **)reply_argv))
	{
		LOG_DEBUG(ERRMSG_FAILURE_OPERATION, "run echo command");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return VSFERR_NONE;
}

static vsf_err_t lpcarmisp_echo(uint8_t on)
{
	uint32_t reply_size;
	
	cmd_argv[0] = LPCARMISP_CMD_ECHO;
	cmd_argv[1] = on ? "1" : "0";
	cmd_argc = 2;
	reply_size = strlen(LPCARMISP_RPL_SUCCESS);
	if (lpcarmisp_process(cmd_argc, (char **)cmd_argv, &reply_size, &reply_argc,
							(char **)reply_argv))
	{
		LOG_DEBUG(ERRMSG_FAILURE_OPERATION, "run echo command");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	if (on)
	{
		echo = 1;
	}
	else
	{
		echo = 0;
	}
	
	return VSFERR_NONE;
}

static vsf_err_t lpcarmisp_write_to_ram(uint32_t addr, uint8_t *buff,
										uint32_t size)
{
	uint32_t reply_size;
	char addr_str[11], size_str[11];
	
	if ((size & 0x02) || (addr & 0x03))
	{
		return VSFERR_FAIL;
	}
	
	cmd_argv[0] = LPCARMISP_CMD_WRITE2RAM;
	snprintf(addr_str, sizeof(addr_str), "%d", (int)addr);
	snprintf(size_str, sizeof(size_str), "%d", (int)size);
	cmd_argv[1] = addr_str;
	cmd_argv[2] = size_str;
	cmd_argc = 3;
	reply_size = strlen(LPCARMISP_RPL_SUCCESS);
	if (lpcarmisp_process(cmd_argc, (char **)cmd_argv, &reply_size, &reply_argc,
							(char **)reply_argv))
	{
		LOG_DEBUG(ERRMSG_FAILURE_OPERATION, "run prepare sectors command");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	if (uu_send(buff, size))
	{
		lpcarmisp_abort();
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t lpcarmisp_read_memory(uint32_t addr, uint8_t *buff,
										uint32_t size)
{
	uint32_t reply_size;
	char addr_str[11], size_str[11];
	
	if ((addr & 0x03) || (size & 0x02))
	{
		return VSFERR_FAIL;
	}
	
	cmd_argv[0] = LPCARMISP_CMD_READMEMORY;
	snprintf(addr_str, sizeof(addr_str), "%d", (int)addr);
	snprintf(size_str, sizeof(size_str), "%d", (int)size);
	cmd_argv[1] = addr_str;
	cmd_argv[2] = size_str;
	cmd_argc = 3;
	reply_size = strlen(LPCARMISP_RPL_SUCCESS);
	if (lpcarmisp_process(cmd_argc, (char **)cmd_argv, &reply_size, &reply_argc,
							(char **)reply_argv))
	{
		LOG_DEBUG(ERRMSG_FAILURE_OPERATION, "run prepare sectors command");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// read data
	if (uu_recv(buff, size))
	{
		lpcarmisp_abort();
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t lpcarmisp_prepare_sectors(uint8_t start_sector,
										uint8_t end_sector)
{
	uint32_t reply_size;
	char start_sector_str[4], end_sector_str[4];
	
	cmd_argv[0] = LPCARMISP_CMD_PREPARESECTORS;
	snprintf(start_sector_str, sizeof(start_sector_str), "%d", start_sector);
	snprintf(end_sector_str, sizeof(end_sector_str), "%d", end_sector);
	cmd_argv[1] = start_sector_str;
	cmd_argv[2] = end_sector_str;
	cmd_argc = 3;
	reply_size = strlen(LPCARMISP_RPL_SUCCESS);
	if (lpcarmisp_process(cmd_argc, (char **)cmd_argv, &reply_size, &reply_argc,
							(char **)reply_argv))
	{
		LOG_DEBUG(ERRMSG_FAILURE_OPERATION, "run prepare sectors command");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return VSFERR_NONE;
}

static vsf_err_t lpcarmisp_copy_ram_to_flash(uint32_t flash_addr,
					uint32_t ram_addr, uint32_t size)
{
	uint32_t reply_size;
	char flash_addr_str[11], ram_addr_str[11], size_str[11];
	
	if ((flash_addr & 0xFF)
		|| ((size != 256) && (size != 512) && (size != 1024) && (size != 4096)))
	{
		return VSFERR_FAIL;
	}
	
	cmd_argv[0] = LPCARMISP_CMD_RAM2FLASH;
	snprintf(flash_addr_str, sizeof(flash_addr_str), "%d", (int)flash_addr);
	snprintf(ram_addr_str, sizeof(ram_addr_str), "%d", (int)ram_addr);
	snprintf(size_str, sizeof(size_str), "%d", (int)size);
	cmd_argv[1] = flash_addr_str;
	cmd_argv[2] = ram_addr_str;
	cmd_argv[3] = size_str;
	cmd_argc = 4;
	reply_size = strlen(LPCARMISP_RPL_SUCCESS);
	if (lpcarmisp_process(cmd_argc, (char **)cmd_argv, &reply_size, &reply_argc,
							(char **)reply_argv))
	{
		LOG_DEBUG(ERRMSG_FAILURE_OPERATION, "run prepare sectors command");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return VSFERR_NONE;
}

static vsf_err_t lpcarmisp_erase_sectors(uint8_t start_sector,
										uint8_t end_sector)
{
	uint32_t reply_size;
	char start_sector_str[4], end_sector_str[4];
	
	cmd_argv[0] = LPCARMISP_CMD_ERASESECTORS;
	snprintf(start_sector_str, sizeof(start_sector_str), "%d", start_sector);
	snprintf(end_sector_str, sizeof(end_sector_str), "%d", end_sector);
	cmd_argv[1] = start_sector_str;
	cmd_argv[2] = end_sector_str;
	cmd_argc = 3;
	reply_size = strlen(LPCARMISP_RPL_SUCCESS);
	if (lpcarmisp_process(cmd_argc, (char **)cmd_argv, &reply_size, &reply_argc,
							(char **)reply_argv))
	{
		LOG_DEBUG(ERRMSG_FAILURE_OPERATION, "run erase sectors command");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return VSFERR_NONE;
}

static vsf_err_t lpcarmisp_read_part_id(uint32_t *id)
{
	uint32_t reply_size;
	
	cmd_argv[0] = LPCARMISP_CMD_READCHIPID;
	cmd_argc = 1;
	reply_size = strlen(LPCARMISP_RPL_SUCCESS) + 12;
	if (lpcarmisp_process(cmd_argc, (char **)cmd_argv, &reply_size, &reply_argc,
							(char **)reply_argv))
	{
		LOG_DEBUG(ERRMSG_FAILURE_OPERATION, "run echo command");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	if (id != NULL)
	{
		*id = strtoul(reply_argv[0], NULL, 0);
	}
	return VSFERR_NONE;
}

static vsf_err_t lpcarmisp_read_boot_version(uint8_t *major, uint8_t *minor)
{
	uint32_t reply_size;
	
	cmd_argv[0] = LPCARMISP_CMD_READBOOTVER;
	cmd_argc = 1;
	reply_size = strlen(LPCARMISP_RPL_SUCCESS) + 6;
	if (lpcarmisp_process(cmd_argc, (char **)cmd_argv, &reply_size, &reply_argc,
							(char **)reply_argv))
	{
		LOG_DEBUG(ERRMSG_FAILURE_OPERATION, "run echo command");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	if (reply_argc != 2)
	{
		return VSFERR_FAIL;
	}
	if (major != NULL)
	{
		*major = (uint8_t)strtoul(reply_argv[0], NULL, 0);
	}
	if (minor != NULL)
	{
		*minor = (uint8_t)strtoul(reply_argv[1], NULL, 0);
	}
	return VSFERR_NONE;
}

static vsf_err_t lpcarmisp_read_serial_number(uint32_t serial[4])
{
	uint32_t reply_size;
	
	cmd_argv[0] = LPCARMISP_CMD_READSERIAL;
	cmd_argc = 1;
	reply_size = strlen(LPCARMISP_RPL_SUCCESS) + 48;
	if (lpcarmisp_process(cmd_argc, (char **)cmd_argv, &reply_size, &reply_argc,
							(char **)reply_argv))
	{
		LOG_DEBUG(ERRMSG_FAILURE_OPERATION, "run echo command");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	if (reply_argc != 4)
	{
		return VSFERR_FAIL;
	}
	if (serial != NULL)
	{
		serial[0] = strtoul(reply_argv[0], NULL, 0);
		serial[1] = strtoul(reply_argv[1], NULL, 0);
		serial[2] = strtoul(reply_argv[2], NULL, 0);
		serial[3] = strtoul(reply_argv[3], NULL, 0);
	}
	return VSFERR_NONE;
}

#if 0
static vsf_err_t lpcarmisp_compare(uint32_t addr1, uint32_t addr2, uint32_t size)
{
	REFERENCE_PARAMETER(addr1);
	REFERENCE_PARAMETER(addr2);
	REFERENCE_PARAMETER(size);
	return VSFERR_FAIL;
}

static vsf_err_t lpcarmisp_set_baudrate(uint32_t baudrate)
{
	// Is this function necessary if auto-baudrate is supported?
	uint32_t reply_size;
	char baudrate_str[11];
	
	cmd_argv[0] = LPCARMISP_CMD_BAUDRATE;
	snprintf(baudrate_str, sizeof(baudrate_str), "%d", baudrate);
	cmd_argv[1] = baudrate_str;
	cmd_argv[2] = "1";
	cmd_argc = 3;
	reply_size = strlen(LPCARMISP_RPL_SUCCESS);
	if (lpcarmisp_process(cmd_argc, (char **)cmd_argv, &reply_size, &reply_argc,
							(char **)reply_argv))
	{
		LOG_DEBUG(ERRMSG_FAILURE_OPERATION, "run echo command");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return VSFERR_NONE;
}

static vsf_err_t lpcarmisp_blank_check_sectors(uint32_t start_sector,
											uint32_t end_sector)
{
	REFERENCE_PARAMETER(start_sector);
	REFERENCE_PARAMETER(end_sector);
	return VSFERR_NONE;
}

static vsf_err_t lpcarmisp_go(uint32_t addr, uint32_t mode)
{
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(mode);
	return VSFERR_NONE;
}
#endif


ENTER_PROGRAM_MODE_HANDLER(lpcarmisp)
{
	struct program_info_t *pi = context->pi;
	vsf_err_t err = VSFERR_NONE;
	uint8_t ver_major, ver_minor;
	uint8_t retry = 2;
	uint32_t serial[4];
	
	echo = 1;
	while (retry--)
	{
		// sync first
		err = lpcarmisp_sync(pi->kernel_khz);
		if (!err)
		{
			err = lpcarmisp_echo(0);
			if (!err)
			{
				err = lpcarmisp_read_boot_version(&ver_major, &ver_minor);
				if (!err)
				{
					LOG_INFO(INFOMSG_BOOTLOADER_VERSION, ver_major, ver_minor);
					err = lpcarmisp_read_serial_number(serial);
					if (!err)
					{
						LOG_INFO("Serial Number: %08X%08X%08X%08X",
							serial[3], serial[2], serial[1], serial[0]);
//						err = lpcarmisp_set_baudrate(230400);
						if (!err)
						{
							err = lpcarmisp_unlock();
							break;
						}
					}
				}
			}
		}
	}
	return err;
}

LEAVE_PROGRAM_MODE_HANDLER(lpcarmisp)
{
	REFERENCE_PARAMETER(success);
	REFERENCE_PARAMETER(context);
	return VSFERR_NONE;
}

ERASE_TARGET_HANDLER(lpcarmisp)
{
	struct chip_area_info_t *flash_info = NULL;
	uint8_t end_sector;
	vsf_err_t err = VSFERR_NONE;
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		flash_info = target_get_chip_area(context->param, APPLICATION_IDX);
		if (NULL == flash_info)
		{
			return VSFERR_FAIL;
		}
		
		end_sector = lpc1000_get_sector_idx_by_addr(context,
							flash_info->addr + flash_info->size - 1);
		err = lpcarmisp_prepare_sectors(0, end_sector);
		if (!err)
		{
			err = lpcarmisp_erase_sectors(0, end_sector);
		}
		break;
	default:
		err = VSFERR_FAIL;
	}
	return err;
}

WRITE_TARGET_HANDLER(lpcarmisp)
{
	uint8_t sector_idx;
	uint32_t ram_addr;
	vsf_err_t err = VSFERR_NONE;
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		ram_addr = LPC1000_SRAM_ADDR + 0x200;
		sector_idx = lpc1000_get_sector_idx_by_addr(context, addr);
		err = lpcarmisp_prepare_sectors(sector_idx, sector_idx);
		if (!err)
		{
			err = lpcarmisp_write_to_ram(ram_addr, buff, size);
			if (!err)
			{
				err = lpcarmisp_copy_ram_to_flash(addr, ram_addr, size);
			}
		}
		break;
	default:
		err = VSFERR_FAIL;
		break;
	}
	return err;
}

READ_TARGET_HANDLER(lpcarmisp)
{
	vsf_err_t err = VSFERR_NONE;
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case CHIPID_CHAR:
		err = lpcarmisp_read_part_id((uint32_t*)buff);
		break;
	case APPLICATION_CHAR:
		err = lpcarmisp_read_memory(addr, buff, size);
		break;
	default:
		err = VSFERR_FAIL;
		break;
	}
	return err;
}
#endif
