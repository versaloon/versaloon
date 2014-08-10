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

#ifndef __SCRIPTS_H_INCLUDED__
#define __SCRIPTS_H_INCLUDED__

#include "tool/list/list.h"

struct vss_cmd_t
{
	const char *cmd_name;
	const char *help_str;
	vsf_err_t (*processor)(uint16_t argc, const char *argv[]);
	struct vss_cmd_t *subcmd;
};
struct vss_cmd_list_t
{
	char *list_name;
	struct vss_cmd_t *cmd;
	struct sllist list;
};

struct vss_param_t
{
	char *param_name;
	char *help_str;
	uint64_t value;
	char *value_str;
	struct vss_param_t *subparam;
	
	struct sllist list;
};
struct vss_param_list_t
{
	char *list_name;
	struct vss_param_t *param;
	struct sllist list;
};

struct vss_function_cmd_t
{
	char *func_cmd;
	struct sllist list;
};
struct vss_function_t
{
	char *func_name;
	uint16_t param_number;
	struct vss_function_cmd_t *cmds;
	
	struct sllist list;
};

struct vss_env_t
{
	// public
	struct vss_param_list_t *param;
	struct vss_cmd_list_t *cmd;
	struct vss_function_t *func;
	
	// private
	int8_t exit_mark;
	uint32_t loop_cnt;
	bool fatal_error;
	uint8_t *quiet_mode_ptr;
	struct vss_function_t *cur_register_function;
	struct vss_function_t *cur_call_function;
};

#define VSS_CMD_LIST(str_name, cmd_array)		\
			{(str_name), (struct vss_cmd_t *)(cmd_array), {NULL}}

#define VSS_HANDLER(name)						\
	vsf_err_t (name)(uint16_t argc, const char *argv[])

#define VSS_CMD(name, helpstr, handler, sub)	\
	{\
		(name),\
		(helpstr),\
		(handler),\
		(struct vss_cmd_t *)(sub)\
	}
#define VSS_CMD_END								VSS_CMD(NULL, NULL, NULL, NULL)
#define VSS_PARAM(name, helpstr, default, sub)	\
	{\
		(name),\
		(helpstr),\
		(default),\
		NULL,\
		(struct vss_param_t *)(sub)\
	}

#define VSS_CHECK_ARGC(n)						\
	if (argc != (n))\
	{\
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);\
		vss_print_help(argv[0]);\
		return VSFERR_FAIL;\
	}
#define VSS_CHECK_ARGC_2(n1, n2)				\
	if ((argc != (n1)) && (argc != (n2)))\
	{\
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);\
		vss_print_help(argv[0]);\
		return VSFERR_FAIL;\
	}
#define VSS_CHECK_ARGC_3(n1, n2, n3)			\
	if ((argc != (n1)) && (argc != (n2)) && (argc != (n3)))\
	{\
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);\
		vss_print_help(argv[0]);\
		return VSFERR_FAIL;\
	}
#define VSS_CHECK_ARGC_MIN(n)					\
	if (argc < (n))\
	{\
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);\
		vss_print_help(argv[0]);\
		return VSFERR_FAIL;\
	}
#define VSS_CHECK_ARGC_MAX(n)					\
	if (argc > (n))\
	{\
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);\
		vss_print_help(argv[0]);\
		return VSFERR_FAIL;\
	}
#define VSS_CHECK_ARGC_RANGE(min, max)			\
	if ((argc < (min)) || (argc > (max)))\
	{\
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);\
		vss_print_help(argv[0]);\
		return VSFERR_FAIL;\
	}

#define VSS_COMMENT_CHAR						'#'
#define VSS_HIDE_CHAR							'@'

vsf_err_t vss_init(void);
vsf_err_t vss_fini(void);
vsf_err_t vss_register_cmd_list(struct vss_cmd_list_t *cmdlist);
vsf_err_t vss_register_param_list(struct vss_param_list_t *paramlist);
vsf_err_t vss_add_param_array_to_list(struct vss_param_list_t *paramlist,
							const struct vss_param_t *param_array, uint32_t n);
void vss_set_fatal_error(void);
vsf_err_t vss_cmd_supported_by_notifier(const struct vss_cmd_t *notifier,
										char *notify_cmd);
vsf_err_t vss_call_notifier(const struct vss_cmd_t *notifier,
							char *notify_cmd, char *notify_param);
vsf_err_t vss_cmd_supported(char *name);
vsf_err_t vss_print_help(const char *name);
vsf_err_t vss_run_script(char *cmd);
vsf_err_t vss_run_cmd(uint16_t argc, char *argv[]);
vsf_err_t vss_get_binary_buffer(uint16_t argc, const char *argv[],
	uint8_t data_size, uint16_t data_num, void **pbuff, uint16_t *parsed_num);

#endif		// __SCRIPTS_H_INCLUDED__
