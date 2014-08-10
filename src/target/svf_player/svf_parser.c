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
#include <ctype.h>

#include "app_cfg.h"
#if TARGET_SVF_EN

#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"
#include "scripts.h"

#include "byte_tap.h"
#include "svf.h"
#include "svf_parser.h"
#include "svf_player.h"

const char *svf_trst_mode_name[4] =
{
	"ON",
	"OFF",
	"Z",
	"ABSENT"
};

const char *svf_command_name[14] =
{
	"ENDDR",
	"ENDIR",
	"FREQUENCY",
	"HDR",
	"HIR",
	"PIO",
	"PIOMAP",
	"RUNTEST",
	"SDR",
	"SIR",
	"STATE",
	"TDR",
	"TIR",
	"TRST"
};

struct svf_parser_check_tdo_para_t
{
	uint32_t line_num;
	uint8_t enabled;
	uint32_t buffer_offset;
	uint32_t bit_len;
};

uint32_t svf_file_index = 0;
uint32_t svf_line_number = 0;

static uint8_t svf_parser_tdo_buffer[SVF_PARSER_DATA_BUFFER_SIZE];
static uint8_t svf_parser_mask_buffer[SVF_PARSER_DATA_BUFFER_SIZE];
static uint32_t svf_parser_buffer_index = 0;

static uint8_t svf_parser_tdi_buffer[SVF_PARSER_DATA_BUFFER_SIZE];
static struct svf_parser_check_tdo_para_t
						svf_parser_check[SVF_PARSER_DATA_BUFFER_SIZE];
static uint32_t svf_parser_check_index = 0;

static struct svf_para_t svf_parser_para;

void svf_parser_free_xxd_para(struct svf_xxr_para_t *para)
{
	if (para->tdi != NULL)
	{
		free(para->tdi);
		para->tdi = NULL;
	}
	if (para->tdo != NULL)
	{
		free(para->tdo);
		para->tdo = NULL;
	}
	if (para->mask != NULL)
	{
		free(para->mask);
		para->mask = NULL;
	}
	if (para->smask != NULL)
	{
		free(para->smask);
		para->smask = NULL;
	}
}

void svf_parser_init(void)
{
	// init svf_para
	svf_line_number = 1;
	svf_file_index = 0;
	memset(&svf_parser_para, 0, sizeof(svf_parser_para));
	
	// reset all state to IDLE
	svf_parser_para.runtest_end_state = IDLE;
	svf_parser_para.runtest_run_state = IDLE;
	svf_parser_para.ir_end_state = IDLE;
	svf_parser_para.dr_end_state = IDLE;
	
	svf_parser_check_index = 0;
	svf_parser_buffer_index = 0;
}

void svf_parser_fini(void)
{
	svf_parser_free_xxd_para(&svf_parser_para.hdr_para);
	svf_parser_free_xxd_para(&svf_parser_para.hir_para);
	svf_parser_free_xxd_para(&svf_parser_para.tdr_para);
	svf_parser_free_xxd_para(&svf_parser_para.tir_para);
	svf_parser_free_xxd_para(&svf_parser_para.sdr_para);
	svf_parser_free_xxd_para(&svf_parser_para.sir_para);
	
	jtag_fini();
}

static vsf_err_t svf_parser_adjust_array_length(uint8_t **arr,
								uint32_t orig_bit_len, uint32_t new_bit_len)
{
	uint32_t new_byte_len = (new_bit_len + 7) >> 3;
	
	if ((NULL == *arr)
		|| (((orig_bit_len + 7) >> 3) < ((new_bit_len + 7) >> 3)))
	{
		if (*arr != NULL)
		{
			free(*arr);
			*arr = NULL;
		}
		*arr = (uint8_t*)malloc(new_byte_len);
		if (NULL == *arr)
		{
			LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
			return VSFERR_NOT_ENOUGH_RESOURCES;
		}
		memset(*arr, 0, new_byte_len);
	}
	
	return VSFERR_NONE;
}

static void svf_parser_append_1s(uint8_t *dest, uint32_t dest_bit_len,
								 uint32_t append_bit_len)
{
	uint32_t i;
	
	for (i = 0; i < append_bit_len; i++)
	{
		if (((dest_bit_len + i) & 7) == 0)
		{
			dest[(dest_bit_len + i) / 8] = 0;
		}
		dest[(dest_bit_len + i) / 8] |= 1 << ((dest_bit_len + i) % 8);
	}
}

static void svf_parser_append_bit(uint8_t *dest, uint32_t dest_bit_len,
								  uint8_t *src, uint32_t src_bit_len)
{
	uint32_t i;
	
	if ((src_bit_len > 0) && ((NULL == src) || (NULL == dest)))
	{
		LOG_BUG(ERRMSG_INVALID_BUFFER, "source");
		return;
	}
	
	for (i = 0; i < src_bit_len; i++)
	{
		if (((dest_bit_len + i) & 7) == 0)
		{
			dest[(dest_bit_len + i) / 8] = 0;
		}
		if (src[i / 8] & (1 << (i % 8)))
		{
			dest[(dest_bit_len + i) / 8] |= 1 << ((dest_bit_len + i) % 8);
		}
	}
}

static uint8_t svf_parser_find_string_in_array(char *str, char **strs,
											 uint32_t num_of_element)
{
	uint8_t i;
	
	for (i = 0; i < num_of_element; i++)
	{
		if (!strcmp(str, strs[i]))
		{
			return i;
		}
	}
	
	return 0xFF;
}

static vsf_err_t svf_parser_parse_cmd_string(char *str, uint32_t len,
											char **argus, uint32_t *num_of_argu)
{
	uint32_t pos = 0, num = 0;
	uint8_t space_found = 1, in_bracket = 0;
	
	while (pos < len)
	{
		switch (str[pos])
		{
		case '!':
		case '/':
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse svf command");
			return VSFERR_FAIL;
			break;
		case '(':
			in_bracket = 1;
			goto parse_char;
		case ')':
			in_bracket = 0;
			goto parse_char;
		default:
parse_char:
			if (!in_bracket && isspace(str[pos]))
			{
				space_found = 1;
				str[pos] = '\0';
			}
			else if (space_found)
			{
				argus[num++] = &str[pos];
				space_found = 0;
			}
			break;
		}
		pos++;
	}
	
	*num_of_argu = num;
	return VSFERR_NONE;
}

static vsf_err_t svf_parser_copy_hexstring_to_binary(char *str, uint8_t **bin,
													uint32_t bit_len)
{
	uint32_t i, str_len = strlen(str), str_hbyte_len = (bit_len + 3) >> 2;
	uint8_t ch = 0;

	for (i = 0; i < str_hbyte_len; i++)
	{
		ch = 0;
		while (str_len > 0)
		{
			ch = str[--str_len];

			if (!isspace(ch))
			{
				if ((ch >= '0') && (ch <= '9'))
				{
					ch = ch - '0';
					break;
				}
				else if ((ch >= 'A') && (ch <= 'F'))
				{
					ch = ch - 'A' + 10;
					break;
				}
				else
				{
					LOG_ERROR(ERRMSG_INVALID_TARGET, "hex string");
					return VSFERR_FAIL;
				}
			}

			ch = 0;
		}

		// write bin
		if (i % 2)
		{
			// MSB
			(*bin)[i / 2] |= ch << 4;
		}
		else
		{
			// LSB
			(*bin)[i / 2] = 0;
			(*bin)[i / 2] |= ch;
		}
	}

	// consume optional leading '0' or space characters
	while ((str_len > 0) &&
			((str[str_len - 1] == '0') || isspace(str[str_len - 1])))
	{
		str_len--;
	}

	// check valid
	if (str_len > 0 || (ch & ~((2 << ((bit_len - 1) % 4)) - 1)) != 0)
	{
		LOG_ERROR("value execeeds length");
		return VSFERR_FAIL;
	}

	return VSFERR_NONE;
}

#define SVFP_CMD_INC_CNT			1024
vsf_err_t svf_parser_get_command(FILE *file, char **cmd_buffer,
									uint32_t *cmd_len)
{
	char *tmp_buffer = NULL;
	uint32_t cmd_pos = 0;
	char ch;
	uint8_t comment = 0, slash = 0, cmd_ok = 0;
	
	// get 1 valid command
	while (!feof(file) && !cmd_ok)
	{
		ch = (char)fgetc(file);
		svf_file_index++;
		switch (ch)
		{
		case EOF:
			break;
		case '!':
			slash = 0;
			comment = 1;
			break;
		case '/':
			if (++slash == 2)
			{
				comment = 1;
			}
			break;
		case ';':
			slash = 0;
			if (!comment)
			{
				cmd_ok = 1;
			}
			break;
		case '\n':
			svf_line_number++;
		case '\r':
			slash = 0;
			comment = 0;
			// Don't save '\r' and '\n' if no data is parsed
			if (!cmd_pos)
			{
				break;
			}
		default:
			if (!comment)
			{
				if ((cmd_pos + 1) >= *cmd_len)
				{
					// 1 more byte for '\0'
					tmp_buffer = (char*)malloc(*cmd_len
											   + SVFP_CMD_INC_CNT + 1);
					if (NULL == tmp_buffer)
					{
						LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
						return VSFERR_NOT_ENOUGH_RESOURCES;
					}
					if (*cmd_len > 0)
					{
						memcpy(tmp_buffer, *cmd_buffer, *cmd_len);
					}
					if (*cmd_buffer != NULL)
					{
						free(*cmd_buffer);
					}
					*cmd_buffer = tmp_buffer;
					*cmd_len += SVFP_CMD_INC_CNT;
					tmp_buffer = NULL;
				}
				if ('(' == ch)
				{
					(*cmd_buffer)[cmd_pos++] = ' ';
				}
				(*cmd_buffer)[cmd_pos++] = (char)toupper(ch);
				if (')' == ch)
				{
					(*cmd_buffer)[cmd_pos++] = ' ';
				}
			}
			break;
		}
	}
	
	if (cmd_ok)
	{
		(*cmd_buffer)[cmd_pos] = '\0';
		return VSFERR_NONE;
	}
	else
	{
		return VSFERR_FAIL;
	}
}

vsf_err_t svf_parser_check_tdo(void)
{
	uint32_t i, j, byte_len, index;
	uint32_t bit_mask;

	for (i = 0; i < svf_parser_check_index; i++)
	{
		if (svf_parser_check[i].enabled)
		{
			byte_len = (svf_parser_check[i].bit_len + 7) >> 3;
			index = svf_parser_check[i].buffer_offset;
			for (j = 0; j < byte_len; j++)
			{
				if ((   svf_parser_tdi_buffer[index + j]
						& svf_parser_mask_buffer[index + j])
					!= (svf_parser_tdo_buffer[index + j]
						& svf_parser_mask_buffer[index + j]))
				{
					if (svf_parser_check[i].bit_len >= 32)
					{
						bit_mask = 0xFFFFFFFF;
					}
					else
					{
						bit_mask = (1 << svf_parser_check[i].bit_len) - 1;
					}
					LOG_ERROR(
							"tdo check error at line %d, read = 0x%X, "
							"want = 0x%X, mask = 0x%X",
							svf_parser_check[i].line_num,
							(*(uint32_t*)(svf_parser_tdi_buffer + index))
								& bit_mask,
							(*(uint32_t*)(svf_parser_tdo_buffer + index))
								& bit_mask,
							(*(uint32_t*)(svf_parser_mask_buffer + index))
								& bit_mask);
					
					return VSFERR_FAIL;
				}
			}
		}
	}
	
	svf_parser_check_index = 0;
	svf_parser_buffer_index = 0;
	
	return VSFERR_NONE;
}

vsf_err_t svf_parser_add_check_para(uint8_t enabled, uint32_t buffer_offset,
								 uint32_t bit_len)
{
	if (svf_parser_check_index >= SVF_PARSER_DATA_BUFFER_SIZE)
	{
		LOG_BUG("toooooo many operation undone");
		return VSFERR_FAIL;
	}
	
	svf_parser_check[svf_parser_check_index].line_num = svf_line_number;
	svf_parser_check[svf_parser_check_index].bit_len = bit_len;
	svf_parser_check[svf_parser_check_index].enabled = enabled;
	svf_parser_check[svf_parser_check_index].buffer_offset = buffer_offset;
	svf_parser_check_index++;
	
	return VSFERR_NONE;
}

vsf_err_t svf_parser_run_command(char *cmd_str)
{
	char *argus[256];
	uint32_t num_of_argu = 0;
	uint32_t i, i_tmp;
	uint8_t command;
	
	// for RUNTEST
	uint32_t run_count;
	float min_time, max_time;
	
	// for XXR
	struct svf_xxr_para_t *xxr_para_tmp;
	uint8_t **pbuffer_tmp;
	
	// for STATE
	enum tap_state_t *path = NULL;
	
	LOG_DEBUG("line %d: %s", svf_line_number, cmd_str);
	
	if (svf_parser_parse_cmd_string(cmd_str, (uint32_t)strlen(cmd_str),
									  argus, &num_of_argu))
	{
		return VSFERR_FAIL;
	}
	
	command = svf_parser_find_string_in_array(argus[0],
						(char **)svf_command_name, dimof(svf_command_name));
	
	switch (command)
	{
	case ENDDR:
	case ENDIR:
		if (num_of_argu != 2)
		{
			LOG_ERROR(ERRMSG_INVALID_PARAMETER, argus[0]);
			return VSFERR_INVALID_PARAMETER;
		}
		
		i_tmp = svf_parser_find_string_in_array(argus[1],
							(char **)tap_state_name, dimof(tap_state_name));
		if (!tap_state_is_stable(i_tmp))
		{
			LOG_ERROR("state defined is not stable state");
			return VSFERR_FAIL;
		}
		if (ENDDR == command)
		{
			svf_parser_para.dr_end_state = i_tmp;
			LOG_DEBUG("\tdr_end_state = %s",
					  tap_state_name[svf_parser_para.dr_end_state]);
		}
		else
		{
			svf_parser_para.ir_end_state = i_tmp;
			LOG_DEBUG("\tir_end_state = %s",
					  tap_state_name[svf_parser_para.ir_end_state]);
		}
		break;
	case FREQUENCY:
		if ((num_of_argu != 1) && (num_of_argu != 3))
		{
			LOG_ERROR(ERRMSG_INVALID_PARAMETER, argus[0]);
			return VSFERR_INVALID_PARAMETER;
		}
		
		if (1 == num_of_argu)
		{
			svf_parser_para.frequency = 0;
		}
		else
		{
			if (strcmp(argus[2], "HZ"))
			{
				LOG_ERROR("HZ not found in FREQUENCY command");
				return VSFERR_FAIL;
			}
			if (tap_commit())
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "do jtag");
				return ERRCODE_FAILURE_OPERATION;
			}
			if (svf_parser_check_tdo())
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "check tdo data");
				return ERRCODE_FAILURE_OPERATION;
			}
			
			svf_parser_para.frequency = (float)atof(argus[1]);
			jtag_config((uint16_t)(svf_parser_para.frequency / 1000));
			
			LOG_DEBUG("\tfrequency = %f", svf_parser_para.frequency);
		}
		break;
	case HDR:
		xxr_para_tmp = &svf_parser_para.hdr_para;
		goto XXR_common;
	case HIR:
		xxr_para_tmp = &svf_parser_para.hir_para;
		goto XXR_common;
	case TDR:
		xxr_para_tmp = &svf_parser_para.tdr_para;
		goto XXR_common;
	case TIR:
		xxr_para_tmp = &svf_parser_para.tir_para;
		goto XXR_common;
	case SDR:
		xxr_para_tmp = &svf_parser_para.sdr_para;
		goto XXR_common;
	case SIR:
		xxr_para_tmp = &svf_parser_para.sir_para;
		goto XXR_common;
XXR_common:
		if ((num_of_argu > 10) || (num_of_argu % 2))
		{
			LOG_ERROR(ERRMSG_INVALID_PARAMETER, argus[0]);
			return VSFERR_INVALID_PARAMETER;
		}
		
		i_tmp = xxr_para_tmp->len;
		xxr_para_tmp->len = atoi(argus[1]);
		if (xxr_para_tmp->len > SVF_PARSER_DATA_BUFFER_SIZE / 2 * 8)
		{
			LOG_ERROR("%d is toooo long for one scan.", xxr_para_tmp->len);
			return VSFERR_FAIL;
		}
		LOG_DEBUG("\tlength = %d", xxr_para_tmp->len);
		if (svf_parser_adjust_array_length(&xxr_para_tmp->tdi,
												i_tmp, xxr_para_tmp->len)
			|| svf_parser_adjust_array_length(&xxr_para_tmp->tdo,
												i_tmp, xxr_para_tmp->len)
			|| svf_parser_adjust_array_length(&xxr_para_tmp->mask,
												i_tmp, xxr_para_tmp->len)
			|| svf_parser_adjust_array_length(&xxr_para_tmp->smask,
												i_tmp, xxr_para_tmp->len))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "adjust length of array");
			return VSFERR_FAIL;
		}
		xxr_para_tmp->data_mask = 0;
		for (i = 2; i < num_of_argu; i += 2)
		{
			if ((argus[i + 1][0] != '(')
				|| (argus[i + 1][strlen(argus[i + 1]) - 1] != ')'))
			{
				LOG_ERROR("data section error");
				return VSFERR_FAIL;
			}
			
			argus[i + 1][strlen(argus[i + 1]) - 1] = '\0';
			// TDI, TDO, MASK, SMASK
			if (!strcmp(argus[i], "TDI"))
			{
				// TDI
				pbuffer_tmp = &xxr_para_tmp->tdi;
				xxr_para_tmp->data_mask |= XXR_TDI;
			}
			else if (!strcmp(argus[i], "TDO"))
			{
				// TDO
				pbuffer_tmp = &xxr_para_tmp->tdo;
				xxr_para_tmp->data_mask |= XXR_TDO;
			}
			else if (!strcmp(argus[i], "MASK"))
			{
				// MASK
				pbuffer_tmp = &xxr_para_tmp->mask;
				xxr_para_tmp->data_mask |= XXR_MASK;
			}
			else if (!strcmp(argus[i], "SMASK"))
			{
				// SMASK
				pbuffer_tmp = &xxr_para_tmp->smask;
				xxr_para_tmp->data_mask |= XXR_SMASK;
			}
			else
			{
				LOG_ERROR("unknow parameter: %s", argus[i]);
				return VSFERR_FAIL;
			}
			
			if (svf_parser_copy_hexstring_to_binary(&argus[i + 1][1],
									pbuffer_tmp, xxr_para_tmp->len))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse hex value");
				return ERRCODE_FAILURE_OPERATION;
			}
			
			LOG_DEBUG("\t%s = 0x%X", argus[i],
				(uint32_t)((**(uint32_t**)pbuffer_tmp)
					& (((uint64_t)1 << xxr_para_tmp->len) - 1)));
		}
		
		// If a command changes the length of the last scan of the same type
		// and the MASK parameter is absent, the mask pattern used is all cares
		if (!(xxr_para_tmp->data_mask & XXR_MASK)
			&& (i_tmp != xxr_para_tmp->len))
		{
			// MASK not defined and length changed
			svf_parser_append_1s(xxr_para_tmp->mask, 0, xxr_para_tmp->len);
		}
		// If TDO is absent, no comparison is needed, set the mask to 0
		if (!(xxr_para_tmp->data_mask & XXR_TDO))
		{
			memset(xxr_para_tmp->mask, 0, (xxr_para_tmp->len + 7) >> 3);
		}
		
		// do scan if necessary
		if (SDR == command)
		{
			// assemble ir data
			i = 0;
			svf_parser_append_bit(
							&svf_parser_tdi_buffer[svf_parser_buffer_index],
							i, svf_parser_para.hdr_para.tdi,
							svf_parser_para.hdr_para.len);
			i += svf_parser_para.hdr_para.len;
			svf_parser_append_bit(
							&svf_parser_tdi_buffer[svf_parser_buffer_index],
							i, svf_parser_para.sdr_para.tdi,
							svf_parser_para.sdr_para.len);
			i += svf_parser_para.sdr_para.len;
			svf_parser_append_bit(
							&svf_parser_tdi_buffer[svf_parser_buffer_index],
							i, svf_parser_para.tdr_para.tdi,
							svf_parser_para.tdr_para.len);
			i += svf_parser_para.tdr_para.len;
			
			// add check data
			if (svf_parser_para.sdr_para.data_mask & XXR_TDO)
			{
				// assemble dr mask data
				i = 0;
				svf_parser_append_bit(
							&svf_parser_mask_buffer[svf_parser_buffer_index],
							i, svf_parser_para.hdr_para.mask,
							svf_parser_para.hdr_para.len);
				i += svf_parser_para.hdr_para.len;
				svf_parser_append_bit(
							&svf_parser_mask_buffer[svf_parser_buffer_index],
							i, svf_parser_para.sdr_para.mask,
							svf_parser_para.sdr_para.len);
				i += svf_parser_para.sdr_para.len;
				svf_parser_append_bit(
							&svf_parser_mask_buffer[svf_parser_buffer_index],
							i, svf_parser_para.tdr_para.mask,
							svf_parser_para.tdr_para.len);
				i += svf_parser_para.tdr_para.len;
				// assemble dr check data
				i = 0;
				svf_parser_append_bit(
							&svf_parser_tdo_buffer[svf_parser_buffer_index],
							i, svf_parser_para.hdr_para.tdo,
							svf_parser_para.hdr_para.len);
				i += svf_parser_para.hdr_para.len;
				svf_parser_append_bit(
							&svf_parser_tdo_buffer[svf_parser_buffer_index],
							i, svf_parser_para.sdr_para.tdo,
							svf_parser_para.sdr_para.len);
				i += svf_parser_para.sdr_para.len;
				svf_parser_append_bit(
							&svf_parser_tdo_buffer[svf_parser_buffer_index],
							i, svf_parser_para.tdr_para.tdo,
							svf_parser_para.tdr_para.len);
				i += svf_parser_para.tdr_para.len;
				
				svf_parser_add_check_para(1, svf_parser_buffer_index, i);
			}
			else
			{
				svf_parser_add_check_para(0, svf_parser_buffer_index, i);
			}
			
			if (tap_end_state(svf_parser_para.dr_end_state))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "call tap_end_state");
				return VSFERR_FAIL;
			}
			if (tap_scan_dr(&svf_parser_tdi_buffer[svf_parser_buffer_index], i))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "call tap_scan_dr");
				return VSFERR_FAIL;
			}
			svf_parser_buffer_index += (i + 7) >> 3;
		}
		else if (SIR == command)
		{
			// assemble dr data
			i = 0;
			svf_parser_append_bit(
							&svf_parser_tdi_buffer[svf_parser_buffer_index],
							i, svf_parser_para.hir_para.tdi,
							svf_parser_para.hir_para.len);
			i += svf_parser_para.hir_para.len;
			svf_parser_append_bit(
							&svf_parser_tdi_buffer[svf_parser_buffer_index],
							i, svf_parser_para.sir_para.tdi,
							svf_parser_para.sir_para.len);
			i += svf_parser_para.sir_para.len;
			svf_parser_append_bit(
							&svf_parser_tdi_buffer[svf_parser_buffer_index],
							i, svf_parser_para.tir_para.tdi,
							svf_parser_para.tir_para.len);
			i += svf_parser_para.tir_para.len;
			
			// add check data
			if (svf_parser_para.sir_para.data_mask & XXR_TDO)
			{
				// assemble dr mask data
				i = 0;
				svf_parser_append_bit(
							&svf_parser_mask_buffer[svf_parser_buffer_index],
							i, svf_parser_para.hir_para.mask,
							svf_parser_para.hir_para.len);
				i += svf_parser_para.hir_para.len;
				svf_parser_append_bit(
							&svf_parser_mask_buffer[svf_parser_buffer_index],
							i, svf_parser_para.sir_para.mask,
							svf_parser_para.sir_para.len);
				i += svf_parser_para.sir_para.len;
				svf_parser_append_bit(
							&svf_parser_mask_buffer[svf_parser_buffer_index],
							i, svf_parser_para.tir_para.mask,
							svf_parser_para.tir_para.len);
				i += svf_parser_para.tir_para.len;
				
				// assemble dr check data
				i = 0;
				svf_parser_append_bit(
							&svf_parser_tdo_buffer[svf_parser_buffer_index],
							i, svf_parser_para.hir_para.tdo,
							svf_parser_para.hir_para.len);
				i += svf_parser_para.hir_para.len;
				svf_parser_append_bit(
							&svf_parser_tdo_buffer[svf_parser_buffer_index],
							i, svf_parser_para.sir_para.tdo,
							svf_parser_para.sir_para.len);
				i += svf_parser_para.sir_para.len;
				svf_parser_append_bit(
							&svf_parser_tdo_buffer[svf_parser_buffer_index],
							i, svf_parser_para.tir_para.tdo,
							svf_parser_para.tir_para.len);
				i += svf_parser_para.tir_para.len;
				
				svf_parser_add_check_para(1, svf_parser_buffer_index, i);
			}
			else
			{
				svf_parser_add_check_para(0, svf_parser_buffer_index, i);
			}
			
			if (tap_end_state(svf_parser_para.ir_end_state))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "call tap_end_state");
				return VSFERR_FAIL;
			}
			if (tap_scan_ir(&svf_parser_tdi_buffer[svf_parser_buffer_index], i))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "call tap_scan_ir");
				return VSFERR_FAIL;
			}
			svf_parser_buffer_index += (i + 7) >> 3;
		}
		break;
	case PIO:
	case PIOMAP:
		LOG_ERROR("PIO and PIOMAP are not supported");
		return VSFERR_FAIL;
		break;
	case RUNTEST:
		if ((num_of_argu < 3) && (num_of_argu > 11))
		{
			LOG_ERROR(ERRMSG_INVALID_PARAMETER, argus[0]);
			return VSFERR_INVALID_PARAMETER;
		}
		
		// init
		run_count = 0;
		min_time = 0;
		max_time = 0;
		i = 1;
		
		// run_state
		i_tmp = svf_parser_find_string_in_array(argus[i],
							(char **)tap_state_name, dimof(tap_state_name));
		if (tap_state_is_valid(i_tmp))
		{
			if (tap_state_is_stable(i_tmp))
			{
				svf_parser_para.runtest_run_state = i_tmp;
				
				// When a run_state is specified,
				// the new  run_state becomes the default end_state
				svf_parser_para.runtest_end_state = i_tmp;
				LOG_DEBUG("\trun_state = %s",
						  tap_state_name[svf_parser_para.runtest_run_state]);
				i++;
			}
			else
			{
				LOG_ERROR("%s is not valid state", tap_state_name[i_tmp]);
				return VSFERR_FAIL;
			}
		}
		
		// run_count run_clk
		if (((i + 2) <= num_of_argu) && strcmp(argus[i + 1], "SEC"))
		{
			if (!strcmp(argus[i + 1], "TCK"))
			{
				// clock source is TCK
				run_count = atoi(argus[i]);
				LOG_DEBUG("\trun_count@TCK = %d", run_count);
			}
			else
			{
				LOG_ERROR("%s not supported for clock", argus[i + 1]);
				return VSFERR_FAIL;
			}
			i += 2;
		}
		
		// min_time SEC
		if (((i + 2) <= num_of_argu) && !strcmp(argus[i + 1], "SEC"))
		{
			min_time = (float)atof(argus[i]);
			LOG_DEBUG("\tmin_time = %fs", min_time);
			i += 2;
		}
		
		// MAXIMUM max_time SEC
		if (((i + 3) <= num_of_argu) && !strcmp(argus[i], "MAXIMUM")
			&& !strcmp(argus[i + 2], "SEC"))
		{
			max_time = (float)atof(argus[i + 1]);
			LOG_DEBUG("\tmax_time = %fs", max_time);
			i += 3;
		}
		// ENDSTATE end_state
		if (((i + 2) <= num_of_argu) && !strcmp(argus[i], "ENDSTATE"))
		{
			i_tmp = svf_parser_find_string_in_array(argus[i + 1],
							(char **)tap_state_name, dimof(tap_state_name));
			if (tap_state_is_stable(i_tmp))
			{
				svf_parser_para.runtest_end_state = i_tmp;
				LOG_DEBUG("\tend_state = %s",
						  tap_state_name[svf_parser_para.runtest_end_state]);
			}
			else
			{
				LOG_ERROR("%s is not valid state", tap_state_name[i_tmp]);
				return VSFERR_FAIL;
			}
			i += 2;
		}
		
		// calculate run_count
		if ((0 == run_count) && (min_time > 0))
		{
			run_count = (uint32_t)(min_time * svf_parser_para.frequency);
		}
		// all parameter should be parsed
		if (i == num_of_argu)
		{
			if (run_count > 0)
			{
				if (tap_runtest(svf_parser_para.runtest_run_state,
								svf_parser_para.runtest_end_state, run_count))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "call tap_runtest");
					return VSFERR_FAIL;
				}
			}
		}
		else
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse parameter of RUNTEST");
			return ERRCODE_FAILURE_OPERATION;
		}
		break;
	case STATE:
		if (num_of_argu < 2)
		{
			LOG_ERROR(ERRMSG_INVALID_PARAMETER, argus[0]);
			return VSFERR_INVALID_PARAMETER;
		}
		
		// STATE pathstate1 ... stable_state
		path = malloc((num_of_argu - 1) * sizeof(enum tap_state_t));
		if (NULL == path)
		{
			LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
			return VSFERR_NOT_ENOUGH_RESOURCES;
		}
		for (i = 1; i < num_of_argu; i++)
		{
			path[i - 1] = svf_parser_find_string_in_array(argus[i],
						(char **)tap_state_name, dimof(tap_state_name));
			if (!tap_state_is_valid(path[i - 1]))
			{
				LOG_ERROR(ERRMSG_INVALID, tap_state_name[path[i - 1]],
							"tap state");
				free(path);
				path = NULL;
				return ERRCODE_INVALID;
			}
		}
		if (tap_state_is_stable(path[num_of_argu - 2]))
		{
			// last state MUST be stable state
			if (2 == num_of_argu)
			{
				// use state_move
				if (tap_end_state(path[0]) || tap_state_move())
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "call tap_state_move");
					free(path);
					path = NULL;
					return VSFERR_FAIL;
				}
				LOG_DEBUG("\tmove to %s by state_move",
						  tap_state_name[path[num_of_argu - 2]]);
			}
			else
			{
				// use path_move
				if (tap_path_move(num_of_argu - 1, path))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "call tap_path_move");
					free(path);
					path = NULL;
					return VSFERR_FAIL;
				}
				LOG_DEBUG("\tmove to %s by path_move",
						  tap_state_name[path[num_of_argu - 2]]);
			}
		}
		else
		{
			LOG_ERROR(ERRMSG_INVALID,
					tap_state_name[path[num_of_argu - 2]], "tap state");
			free(path);
			path = NULL;
			return ERRCODE_INVALID;
		}
		free(path);
		path = NULL;
		break;
	case TRST:
		if (num_of_argu != 2)
		{
			LOG_ERROR(ERRMSG_INVALID_PARAMETER, argus[0]);
			return VSFERR_INVALID_PARAMETER;
		}
		
		if (svf_parser_para.trst_mode != TRST_ABSENT)
		{
			if (tap_commit())
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "do jtag");
				return ERRCODE_FAILURE_OPERATION;
			}
			if (svf_parser_check_tdo())
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "check tdo data");
				return ERRCODE_FAILURE_OPERATION;
			}
			
			i_tmp = svf_parser_find_string_in_array(argus[1],
					(char **)svf_trst_mode_name, dimof(svf_trst_mode_name));
			switch (i_tmp)
			{
			case TRST_ON:
				if (jtag_trst_output(0))
				{
					return VSFERR_FAIL;
				}
				break;
			case TRST_OFF:
				if (jtag_trst_output(1))
				{
					return VSFERR_FAIL;
				}
				break;
			case TRST_Z:
			case TRST_ABSENT:
				if (jtag_trst_input())
				{
					return VSFERR_FAIL;
				}
				break;
			default:
				LOG_ERROR(ERRMSG_INVALID, argus[1], "TRST mode");
				return ERRCODE_INVALID;
			}
			svf_parser_para.trst_mode = i_tmp;
			LOG_DEBUG("\ttrst_mode = %s",
					  svf_trst_mode_name[svf_parser_para.trst_mode]);
		}
		else
		{
			LOG_ERROR("can not accpet TRST command if trst_mode is ABSENT");
			return VSFERR_FAIL;
		}
		break;
	default:
		LOG_ERROR(ERRMSG_INVALID, argus[0], "svf command");
		return ERRCODE_INVALID;
		break;
	}
	
	if (verbosity >= DEBUG_LEVEL)
	{
		uint32_t read_value;
		
		if ((command != STATE) && (command != RUNTEST))
		{
			if (tap_commit())
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "do jtag");
				return ERRCODE_FAILURE_OPERATION;
			}
			else if (svf_parser_check_tdo())
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "check tdo data");
				return ERRCODE_FAILURE_OPERATION;
			}
			
			switch (command)
			{
			case SIR:
			case SDR:
				memcpy(&read_value, svf_parser_tdi_buffer, sizeof(read_value));
				LOG_DEBUG("\tTDO read = 0x%X",
						read_value & ((1 << svf_parser_check[0].bit_len) - 1));
				break;
			default:
				break;
			}
		}
	}
	else
	{
		// half of the space is left for next command
		if ((svf_parser_buffer_index >= SVF_PARSER_DATA_BUFFER_SIZE / 2)
			&& (((command != STATE) && (command != RUNTEST))
				|| ((command == STATE) && (num_of_argu == 2))))
		{
			if (tap_commit())
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "do jtag");
				return ERRCODE_FAILURE_OPERATION;
			}
			else if (svf_parser_check_tdo())
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "check tdo data");
				return ERRCODE_FAILURE_OPERATION;
			}
		}
	}
	
	return VSFERR_NONE;
}

#endif
