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
#include "app_type.h"
#include "app_io.h"
#include "app_log.h"
#include "app_err.h"
#include "port.h"

#include "interfaces.h"
#include "scripts.h"

#define VSS_PARAM_EXIT_ON_FAIL				"exit_on_fail"
#define VSS_PARAM_NO_COMMIT					"no_commit"

static const struct vss_param_t vss_param[] =
{
	VSS_PARAM(	VSS_PARAM_EXIT_ON_FAIL,
				"whether to exit when execute fail",
				0,
				NULL),
	VSS_PARAM(	VSS_PARAM_NO_COMMIT,
				"no commit on command except commit command",
				0,
				NULL),
};

VSS_HANDLER(vss_param_register);
VSS_HANDLER(vss_param_value);
VSS_HANDLER(vss_param_str);
VSS_HANDLER(vss_param_free);
VSS_HANDLER(vss_help);
VSS_HANDLER(vss_shell);
VSS_HANDLER(vss_run);
VSS_HANDLER(vss_loop);
VSS_HANDLER(vss_exit);
VSS_HANDLER(vss_close);
VSS_HANDLER(vss_run_command);
VSS_HANDLER(vss_log_info);
VSS_HANDLER(vss_getchar);
VSS_HANDLER(vss_sleep);
VSS_HANDLER(vss_quiet);
VSS_HANDLER(vss_function_register);
VSS_HANDLER(vss_function_end);
VSS_HANDLER(vss_function_call);
VSS_HANDLER(vss_function_free);
VSS_HANDLER(vss_out);

static struct vss_cmd_t vss_generic_cmd[] =
{
	VSS_CMD(	"param",
				"register parameter, format: param NAME [HELPER]",
				vss_param_register,
				NULL),
	VSS_CMD(	"param_val",
				"set value of parameters, format: param_val NAME VALUE",
				vss_param_value,
				NULL),
	VSS_CMD(	"param_str",
				"set string of parameters, format: param_str NAME STRING",
				vss_param_str,
				NULL),
	VSS_CMD(	"param_free",
				"free parameter(s), format: param_free [PARAM_NAME]",
				vss_param_free,
				NULL),
	VSS_CMD(	"vss-help",
				"print vss-help message, format: vss-help <OBJECT>",
				vss_help,
				NULL),
	VSS_CMD(	"shell",
				"enter shell mode, format: shell",
				vss_shell,
				NULL),
	VSS_CMD(	"run",
				"run script file, format: run FILE_NAME [quiet]",
				vss_run,
				NULL),
	VSS_CMD(	"loop",
				"loop next command, format: loop COUNT",
				vss_loop,
				NULL),
	VSS_CMD(	"exit",
				"exit current session, format: exit",
				vss_exit,
				NULL),
	VSS_CMD(	"close",
				"close program, format: close",
				vss_close,
				NULL),
	VSS_CMD(	"vss-cmd",
				"run vss command, format: vss-cmd/V COMMAND",
				vss_run_command,
				NULL),
	VSS_CMD(	"V",
				"run vss command, format: vss-cmd/V COMMAND",
				vss_run_command,
				NULL),
	VSS_CMD(	"out",
				"display information, format: out [INFO]",
				vss_out,
				NULL),
	VSS_CMD(	"log_info",
				"display information, format: log_info INFO",
				vss_log_info,
				NULL),
	VSS_CMD(	"getchar",
				"wait keyboard input, format: getchar",
				vss_getchar,
				NULL),
	VSS_CMD(	"sleep",
				"sleep defined ms, format: sleep MS",
				vss_sleep,
				NULL),
	VSS_CMD(	"quiet",
				"set quiet mode, format: quiet/q [0/1]",
				vss_quiet,
				NULL),
	VSS_CMD(	"q",
				"set quiet mode, format: quiet/q [0/1]",
				vss_quiet,
				NULL),
	VSS_CMD(	"function",
				"define a function, format: function FUNC_NAME",
				vss_function_register,
				NULL),
	VSS_CMD(	"function_end",
				"end a function",
				vss_function_end,
				NULL),
	VSS_CMD(	"function_call",
				"call a function, format: function_call FUNC_NAME",
				vss_function_call,
				NULL),
	VSS_CMD(	"function_free",
				"free function(s), format: function_free [FUNC_NAME]",
				vss_function_free,
				NULL),
	VSS_CMD_END
};
static struct vss_cmd_list_t vss_generic_cmd_list = 
										VSS_CMD_LIST("vss", vss_generic_cmd);

VSS_HANDLER(vss_math_add);
VSS_HANDLER(vss_math_sub);
VSS_HANDLER(vss_math_mul);
VSS_HANDLER(vss_math_div);
VSS_HANDLER(vss_math_mod);

static struct vss_cmd_t vss_math_cmd[] =
{
	VSS_CMD(	"add",
				"add function, format: add PARAM VAL0 VAL1",
				vss_math_add,
				NULL),
	VSS_CMD(	"sub",
				"sub function, format: sub PARAM VAL0 VAL1",
				vss_math_sub,
				NULL),
	VSS_CMD(	"mul",
				"mul function, format: mul PARAM VAL0 VAL1",
				vss_math_mul,
				NULL),
	VSS_CMD(	"div",
				"div function, format: div PARAM VAL0 VAL1",
				vss_math_div,
				NULL),
	VSS_CMD(	"mod",
				"mod function, format: mod PARAM VAL0 VAL1",
				vss_math_sub,
				NULL),
	VSS_CMD_END
};
static struct vss_cmd_list_t vss_math_cmd_list = 
										VSS_CMD_LIST("math", vss_math_cmd);

static struct vss_env_t vss_env;

static struct vss_param_t* vss_search_param_in_list(struct vss_param_t *param,
													const char *name)
{
	char *name_temp;
	struct vss_param_t *param_temp;
	
	while ((param != NULL) && (param->param_name != NULL))
	{
		if (param->subparam != NULL)
		{
			if ((strstr(name, param->param_name) == name) &&
				(name[strlen(param->param_name)] == '.'))
			{
				name_temp = (char *)&name[strlen(param->param_name) + 1];
				param_temp = vss_search_param_in_list(param->subparam,
														name_temp);
				if (param_temp != NULL)
				{
					return param_temp;
				}
			}
		}
		else
		{
			if (!strcmp(param->param_name, name))
			{
				return param;
			}
		}
		
		param = sllist_get_container(param->list.next, struct vss_param_t,
										list);
	}
	return NULL;
}

static struct vss_param_t* vss_search_param_in_lists(
				struct vss_param_list_t *param_list, const char *name)
{
	struct vss_param_t *param_temp = NULL;
	struct vss_param_list_t *temp = param_list;
	char *name_temp = NULL;
	
	if (NULL == name)
	{
		return NULL;
	}
	
	while (temp != NULL)
	{
		if ((strstr(name, temp->list_name) == name) &&
			(name[strlen(temp->list_name)] == '.'))
		{
			name_temp = (char *)&name[strlen(temp->list_name) + 1];
		}
		else
		{
			name_temp = (char *)name;
		}
		
		param_temp = vss_search_param_in_list(temp->param,
												(const char *)name_temp);
		if (param_temp != NULL)
		{
			return param_temp;
		}
		
		temp = sllist_get_container(temp->list.next, struct vss_param_list_t,
									list);
	}
	
	return NULL;
}

static struct vss_param_list_t* vss_search_paramlist(
				struct vss_param_list_t *param_list, const char *name)
{
	while (param_list != NULL)
	{
		if (!strcmp(param_list->list_name, name))
		{
			return param_list;
		}
		param_list = sllist_get_container(param_list->list.next,
											struct vss_param_list_t, list);
	}
	return NULL;
}

static vsf_err_t vss_add_param_to_list(struct vss_param_list_t *param_list,
										struct vss_param_t* param)
{
	if ((NULL == param_list) || (NULL == param))
	{
		return VSFERR_INVALID_PARAMETER;
	}
	
	if (vss_search_param_in_lists(param_list, param->param_name) != NULL)
	{
		// already exists
		return VSFERR_FAIL;
	}
	
	if (NULL == param_list->param)
	{
		sllist_init_node(param->list);
	}
	else
	{
		sllist_insert(param->list, param_list->param->list);
	}
	param_list->param = param;
	return VSFERR_NONE;
}

static struct vss_cmd_t* vss_search_cmd(struct vss_cmd_t *cmd, const char *name)
{
	char *name_temp;
	struct vss_cmd_t *cmd_temp;
	
	while ((cmd != NULL) && (cmd->cmd_name != NULL))
	{
		if ((cmd->processor != NULL) && (!strcmp(cmd->cmd_name, name)))
		{
			return cmd;
		}
		
		if ((cmd->subcmd != NULL) && (strstr(name, cmd->cmd_name) == name) &&
			(name[strlen(cmd->cmd_name)] == '.'))
		{
			name_temp = (char *)&name[strlen(cmd->cmd_name) + 1];
			cmd_temp = vss_search_cmd(cmd->subcmd, name_temp);
			if (cmd_temp != NULL)
			{
				return cmd_temp;
			}
		}
		
		cmd++;
	}
	return NULL;
}

static struct vss_cmd_t* vss_search_cmd_in_list(struct vss_cmd_list_t *cmd_list,
												const char *name)
{
	struct vss_cmd_t *cmd_temp = NULL;
	struct vss_cmd_list_t *temp = cmd_list;
	char *name_temp = NULL;
	
	if (NULL == name)
	{
		return NULL;
	}
	
	while (temp != NULL)
	{
		if ((strstr(name, temp->list_name) == name) &&
			(name[strlen(temp->list_name)] == '.'))
		{
			name_temp = (char *)&name[strlen(temp->list_name) + 1];
		}
		else
		{
			name_temp = (char *)name;
		}
		
		cmd_temp = vss_search_cmd(temp->cmd, (const char *)name_temp);
		if (cmd_temp != NULL)
		{
			return cmd_temp;
		}
		
		temp = sllist_get_container(temp->list.next, struct vss_cmd_list_t,
									list);
	}
	
	return NULL;
}

vsf_err_t vss_register_cmd_list(struct vss_cmd_list_t *cmdlist)
{
	if (NULL == vss_env.cmd)
	{
		sllist_init_node(cmdlist->list);
	}
	else
	{
		struct vss_cmd_list_t *templist = vss_env.cmd;
		
		while (templist)
		{
			if (!strcmp(templist->list_name, cmdlist->list_name))
			{
				// cmd_list with same name registered
				return VSFERR_NONE;
			}
			templist = sllist_get_container(templist->list.next,
					struct vss_cmd_list_t, list);
		}
		sllist_insert(cmdlist->list, vss_env.cmd->list);
	}
	vss_env.cmd = cmdlist;
	return VSFERR_NONE;
}

vsf_err_t vss_register_param_list(struct vss_param_list_t *paramlist)
{
	if (NULL == vss_env.param)
	{
		sllist_init_node(paramlist->list);
	}
	else
	{
		sllist_insert(paramlist->list, vss_env.param->list);
	}
	vss_env.param = paramlist;
	return VSFERR_NONE;
}

vsf_err_t vss_add_param_array_to_list(struct vss_param_list_t *paramlist,
							const struct vss_param_t *param_array, uint32_t n)
{
	struct vss_param_t *param;
	vsf_err_t err = VSFERR_NONE;
	uint32_t i;
	
	if ((NULL == paramlist) || ((n > 0) && (NULL == param_array)))
	{
		return VSFERR_FAIL;
	}
	
	for (i = 0; i < n; i++)
	{
		if (NULL == param_array[i].param_name)
		{
			return VSFERR_FAIL;
		}
		
		param = (struct vss_param_t *)malloc(sizeof(struct vss_param_t));
		if (NULL == param)
		{
			err = VSFERR_NOT_ENOUGH_RESOURCES;
			goto error;
		}
		memset(param, 0, sizeof(*param));
		sllist_init_node(param->list);
		
		param->param_name = strdup(param_array[i].param_name);
		if (param_array[i].help_str != NULL)
		{
			param->help_str = strdup(param_array[i].help_str);
		}
		param->value = param_array[i].value;
		if (param_array[i].value_str != NULL)
		{
			param->value_str = strdup(param_array[i].value_str);
		}
		
		err = vss_add_param_to_list(paramlist, param);
		if (err)
		{
			goto error;
		}
	}
	return err;
error:
	if (param != NULL)
	{
		free(param);
		param = NULL;
	}
	return err;
}

static vsf_err_t vss_free_param_list(struct vss_param_list_t *pl);
static vsf_err_t vss_free_param_node(struct vss_param_t *param)
{
	if (NULL == param)
	{
		return VSFERR_NONE;
	}
	
	if (param->param_name != NULL)
	{
		free(param->param_name);
		param->param_name = NULL;
	}
	if (param->help_str != NULL)
	{
		free(param->help_str);
		param->help_str = NULL;
	}
	if (param->value_str != NULL)
	{
		free(param->value_str);
		param->value_str = NULL;
	}
	if (param->subparam != NULL)
	{
		struct vss_param_list_t *pl =
			(struct vss_param_list_t *)malloc(sizeof(struct vss_param_list_t));
		
		if (NULL == pl)
		{
			return VSFERR_FAIL;
		}
		memset(pl, 0, sizeof(struct vss_param_list_t));
		sllist_init_node(pl->list);
		pl->param = param->subparam;
		vss_free_param_list(pl);
		param->subparam = NULL;
	}
	free(param);
	param = NULL;
	return VSFERR_NONE;
}

static vsf_err_t vss_free_param_list(struct vss_param_list_t *pl)
{
	struct vss_param_t *param, *param_temp;
	
	if (pl->list_name != NULL)
	{
		free(pl->list_name);
		pl->list_name = NULL;
	}
	
	param = pl->param;
	while (param != NULL)
	{
		param_temp = sllist_get_container(param->list.next, struct vss_param_t,
											list);
		vss_free_param_node(param);
		param = param_temp;
	}
	
	free(pl);
	pl = NULL;
	return VSFERR_NONE;
}

vsf_err_t vss_fini(void)
{
	vss_run_script("function_free");
	vss_run_script("param_free");
	
	vss_env.param = NULL;
	vss_env.cmd = NULL;
	return VSFERR_NONE;
}

vsf_err_t vss_init(void)
{
	struct vss_param_list_t *param_list = NULL;
	vsf_err_t err = VSFERR_NONE;
	
	memset(&vss_env, 0, sizeof(vss_env));
	vss_env.fatal_error = false;
	
	vss_env.param = NULL;
	vss_env.cmd = NULL;
	err = vss_register_cmd_list(&vss_generic_cmd_list);
	if (err)
	{
		goto error;
	}
	err = vss_register_cmd_list(&vss_math_cmd_list);
	if (err)
	{
		goto error;
	}
	
	// vss parameter
	param_list =
		(struct vss_param_list_t *)malloc(sizeof(struct vss_param_list_t));
	if (NULL == param_list)
	{
		err = VSFERR_NOT_ENOUGH_RESOURCES;
		goto error;
	}
	memset(param_list, 0, sizeof(*param_list));
	param_list->list_name = strdup("vss");
	sllist_init_node(param_list->list);
	err = vss_register_param_list(param_list);
	if (err)
	{
		goto error;
	}
	
	// env parameter
	param_list =
		(struct vss_param_list_t *)malloc(sizeof(struct vss_param_list_t));
	if (NULL == param_list)
	{
		err = VSFERR_NOT_ENOUGH_RESOURCES;
		goto error;
	}
	memset(param_list, 0, sizeof(*param_list));
	param_list->list_name = strdup("env");
	sllist_init_node(param_list->list);
	err = vss_register_param_list(param_list);
	if (err)
	{
		goto error;
	}
	
	// add param to vss_param_list
	err = vss_add_param_array_to_list(param_list, vss_param, dimof(vss_param));
	if (err)
	{
		goto error;
	}
	return err;
error:
	vss_fini();
	return err;
}

static struct vss_function_t *vss_search_function_in_list(
								struct vss_function_t *f, char *func_name)
{
	if (func_name != NULL)
	{
		while (f != NULL)
		{
			if (!strcmp(f->func_name, func_name))
			{
				return f;
			}
			f = sllist_get_container(f->list.next, struct vss_function_t, list);
		}
	}
	return NULL;
}

static void vss_format_cmd(char **cmd_str, char *param_str, char *replace_str)
{
	char *str_temp = NULL, *new_str = NULL;
	uint32_t pos;
	
	do {
		str_temp = strstr(*cmd_str, param_str);
		if (str_temp != NULL)
		{
			pos = str_temp - *cmd_str;
			new_str = (char *)malloc(strlen(*cmd_str) + strlen(replace_str) +
										1 - strlen(param_str));
			strncpy(new_str, &(*cmd_str)[0], pos);
			new_str[pos] = '\0';
			strcat(new_str, replace_str);
			strcat(new_str, &(*cmd_str)[pos + strlen(param_str)]);
			free(*cmd_str);
			*cmd_str = new_str;
		}
	} while (str_temp != NULL);
}

static vsf_err_t vss_run_function(struct vss_function_t *f, uint16_t argc,
									const char *argv[])
{
	struct vss_function_cmd_t *cmd;
	char param_str[9], *cmd_str;
	uint16_t i;
	
	if ((NULL == f) || (NULL == f->cmds))
	{
		return VSFERR_NONE;
	}
	
	cmd = f->cmds;
	while (cmd->func_cmd != NULL)
	{
		cmd_str = strdup(cmd->func_cmd);
		if (NULL == cmd_str)
		{
			return VSFERR_FAIL;
		}
		
		for (i = 0; i < argc; i++)
		{
			strcpy(param_str, "${");
			SNPRINTF(&param_str[2], 5, "%d", (int)i);
			strcat(param_str, "}");
			
			vss_format_cmd(&cmd_str, param_str, (char *)argv[i]);
			if (NULL == cmd_str)
			{
				return VSFERR_FAIL;
			}
		}
		
		if (vss_run_script(cmd_str))
		{
			free(cmd_str);
			cmd_str = NULL;
			return VSFERR_FAIL;
		}
		free(cmd_str);
		cmd_str = NULL;
		
		cmd = sllist_get_container(cmd->list.next, struct vss_function_cmd_t,
									list);
	}
	
	return VSFERR_NONE;
}

static vsf_err_t vss_free_function_node(struct vss_function_t *f)
{
	char *vss_argv[2];
	struct vss_function_cmd_t *cmd, *cmd_tmp;
	
	if (NULL == f)
	{
		return VSFERR_NONE;
	}
	
	cmd = f->cmds;
	while (cmd != NULL)
	{
		cmd_tmp = cmd;
		cmd = sllist_get_container(cmd->list.next, struct vss_function_cmd_t,
									list);
		if (cmd_tmp->func_cmd != NULL)
		{
			free(cmd_tmp->func_cmd);
			cmd_tmp->func_cmd = NULL;
		}
		free(cmd_tmp);
	}
	
	// free function parameters
	vss_argv[0] = "param_free";
	vss_argv[1] = f->func_name;
	if (vss_run_cmd(dimof(vss_argv), (char **)vss_argv))
	{
		return VSFERR_FAIL;
	}
	
	if (f->func_name != NULL)
	{
		free(f->func_name);
		f->func_name = NULL;
	}
	free(f);
	f = NULL;
	return VSFERR_NONE;
}

static vsf_err_t vss_append_function_cmd(struct vss_function_t *func, char * str)
{
	struct vss_function_cmd_t *cmd, *tmp;
	
	if (NULL == func)
	{
		return VSFERR_FAIL;
	}
	
	cmd = (struct vss_function_cmd_t *)malloc(sizeof(*cmd));
	if (NULL == cmd)
	{
		return VSFERR_FAIL;
	}
	memset(cmd, 0, sizeof(*cmd));
	sllist_init_node(cmd->list);
	
	if (str != NULL)
	{
		cmd->func_cmd = strdup(str);
		if (NULL == cmd->func_cmd)
		{
			free(cmd);
			cmd = NULL;
			return VSFERR_FAIL;
		}
	}
	
	tmp = func->cmds;
	if (NULL == tmp)
	{
		func->cmds = cmd;
	}
	else
	{
		while (tmp->list.next != NULL)
		{
			tmp = sllist_get_container(tmp->list.next, struct vss_function_cmd_t,
										list);
		}
		sllist_insert(tmp->list, cmd->list);
	}
	cmd = tmp = NULL;
	return VSFERR_NONE;
}

vsf_err_t vss_get_binary_buffer(uint16_t argc, const char *argv[],
	uint8_t data_size, uint16_t data_num, void **pbuff, uint16_t *parsed_num)
{
	uint16_t i, num_to_parse = min(argc, data_num);
	uint64_t value;
	
	if ((NULL == argv) || !num_to_parse || (NULL == pbuff)
		|| ((data_size != 1) && (data_size != 2) && (data_size != 4)
			&& (data_size != 8)))
	{
		return VSFERR_INVALID_PARAMETER;
	}
	
	if (NULL == *pbuff)
	{
		*pbuff = malloc(data_size * data_num);
		if (NULL == *pbuff)
		{
			return VSFERR_NOT_ENOUGH_RESOURCES;
		}
		memset(*pbuff, 0, data_size * data_num);
	}
	
	for (i = 0; i < num_to_parse; i++)
	{
		value = strtoull(argv[i], NULL, 0);
		switch (data_size)
		{
		case 1:
			(*(uint8_t **)pbuff)[i] = (uint8_t)value;
			break;
		case 2:
			(*(uint16_t **)pbuff)[i] = (uint16_t)value;
			break;
		case 4:
			(*(uint32_t **)pbuff)[i] = (uint32_t)value;
			break;
		case 8:
			(*(uint64_t **)pbuff)[i] = (uint64_t)value;
			break;
		}
	}
	if (parsed_num != NULL)
	{
		*parsed_num = num_to_parse;
	}
	return VSFERR_NONE;
}

void vss_set_fatal_error(void)
{
	vss_env.fatal_error = true;
}

static char vss_get_first_non_space_char(char *cmd, uint32_t *idx)
{
	char result = 0;
	uint32_t i;
	
	for (i = 0; i < strlen(cmd); i++)
	{
		if (!isspace((int)cmd[i]))
		{
			result = cmd[i];
			break;
		}
	}
	if (idx != NULL)
	{
		*idx = i;
	}
	return result;
}

vsf_err_t vss_print_help(const char *name)
{
	struct vss_cmd_t *cmd = vss_search_cmd_in_list(vss_env.cmd, name);
	
	if (NULL == cmd)
	{
		return VSFERR_FAIL;
	}
	
	LOG_INFO("%s", cmd->help_str);
	
	return VSFERR_NONE;
}

static vsf_err_t vss_parse_cmd_line(char *cmd, uint16_t *argc, char **argv)
{
	uint32_t i, cmd_len;
	uint16_t argu_num = 0;
	
	while (('"' == cmd[0]) || ('\'' == cmd[0]))
	{
		char ch = cmd[0];
		
		if (cmd[strlen(cmd) - 1] != ch)
		{
			return VSFERR_FAIL;
		}
		cmd[strlen(cmd) - 1] = '\0';
		strcpy(cmd, cmd + 1);
	}
	
	// parse arg
	memset(argv, 0, *argc);
	argu_num = 0;
	i = 0;
	cmd_len = strlen(cmd);
	while (i < cmd_len)
	{
		while (isspace((int)cmd[i]))
		{
			i++;
		}
		if ('\0' == cmd[i])
		{
			break;
		}
		
		if (('\'' == cmd[i]) || ('"' == cmd[i]))
		{
			// everything between ' or " is one parameter
			// nesting of ' and " is not supported
			uint32_t j;
			char div = cmd[i];
			
			j = i + 1;
			argv[argu_num++] = &cmd[j];
			while (cmd[j] != div)
			{
				if ('\0' == cmd[j])
				{
					// shouldn't end here too
					return VSFERR_FAIL;
				}
				j++;
			}
			i = j;
		}
		else
		{
			argv[argu_num++] = &cmd[i];
			while (!isspace((int)cmd[i]) && (cmd[i] != '\0'))
			{
				i++;
			}
		}
		
		cmd[i++] = '\0';
		if (argu_num >= *argc)
		{
			break;
		}
	}
	*argc = argu_num;
	
	return VSFERR_NONE;
}

vsf_err_t vss_cmd_supported_by_notifier(const struct vss_cmd_t *notifier,
										char *notify_cmd)
{
	struct vss_cmd_t *cmd;
	
	if ((NULL == notifier) || (NULL == notify_cmd))
	{
		return VSFERR_FAIL;
	}
	
	cmd = vss_search_cmd((struct vss_cmd_t *)notifier, notify_cmd);
	if (NULL == cmd)
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

vsf_err_t vss_call_notifier(const struct vss_cmd_t *notifier,
							char *notify_cmd, char *notify_param)
{
	struct vss_cmd_t *cmd;
	char *argv[2];
	uint16_t argc;
	
	if ((NULL == notifier) || (NULL == notify_cmd))
	{
		return VSFERR_FAIL;
	}
	
	argv[0] = notify_cmd;
	argv[1] = notify_param;
	if (notify_param != NULL)
	{
		argc = dimof(argv);
	}
	else
	{
		argc = 1;
	}
	
	cmd = vss_search_cmd((struct vss_cmd_t *)notifier, argv[0]);
	if (NULL == cmd)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, argv[0]);
		return VSFERR_FAIL;
	}
	
	if (NULL == cmd->processor)
	{
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);
		return VSFERR_FAIL;
	}
	else if (cmd->processor(argc, (const char **)argv))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "run", argv[0]);
		return VSFERR_FAIL;
	}
	
	return VSFERR_NONE;
}

vsf_err_t vss_cmd_supported(char *name)
{
	struct vss_cmd_t *cmd = vss_search_cmd_in_list(vss_env.cmd, name);
	
	if (NULL == cmd)
	{
		return VSFERR_FAIL;
	}
	else
	{
		return VSFERR_NONE;
	}
}

vsf_err_t vss_run_cmd(uint16_t argc, char *argv[])
{
	uint16_t i;
	uint16_t param_len;
	struct vss_param_list_t *pl = NULL;
	struct vss_param_t *param = NULL;
	struct vss_cmd_t *cmd = vss_search_cmd_in_list(vss_env.cmd, argv[0]);
	
	if (NULL == cmd)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, argv[0]);
		return VSFERR_FAIL;
	}
	if (NULL == cmd->processor)
	{
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);
		return VSFERR_FAIL;
	}
	
	// preprocess
	for (i = 1; i < argc; i++)
	{
		param_len = (uint16_t)strlen(argv[i]);

		if (('$' == argv[i][0]) && (param_len > 3))
		{
			char *str = strdup(&argv[i][2]);
			if (NULL == str)
			{
				LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
				return VSFERR_NOT_ENOUGH_RESOURCES;
			}
			str[param_len - 3] = '\0';
			
			// first find parameter in "env" list
			pl = vss_search_paramlist(vss_env.param, "env");
			if (pl != NULL)
			{
				param = vss_search_param_in_list(pl->param, str);
			}
			if ((NULL == param) && (vss_env.cur_call_function != NULL))
			{
				// find parameter in cur call function list
				pl = vss_search_paramlist(vss_env.param,
										vss_env.cur_call_function->func_name);
				if (pl != NULL)
				{
					param = vss_search_param_in_list(pl->param, str);
				}
			}
			free(str);
			
			if (param != NULL)
			{
				if (('{' == argv[i][1]) && ('}' == argv[i][param_len - 1]))
				{
					// use value_str
				}
				else if (('[' == argv[i][1]) && (']' == argv[i][param_len - 1]))
				{
					// use value
					if (param->value_str != NULL)
					{
						free(param->value_str);
					}
					param->value_str = (char *)malloc(32);
					snprintf(param->value_str, 32, "%d", (int)param->value);
				}
				argv[i] = param->value_str;
			}
		}
	}
	
	if (cmd->processor(argc, (const char **)argv))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "run command:", argv[0]);
		return VSFERR_FAIL;
	}
	
	return VSFERR_NONE;
}

vsf_err_t vss_run_script(char *cmd)
{
	struct vss_param_t *param = NULL;
	uint8_t no_commit = 0, exit_on_fail = 0;
	char *buff_in_memory = NULL;
	uint16_t argc;
	char *argv[VSS_CFG_MAX_ARGC];
	vsf_err_t err = VSFERR_NONE;
	uint32_t i, run_times;
	
	buff_in_memory = strdup(cmd);
	if (NULL == buff_in_memory)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return VSFERR_NOT_ENOUGH_RESOURCES;
	}
	
	argc = (uint16_t)dimof(argv);
	if (vss_parse_cmd_line(buff_in_memory, &argc, (char **)argv))
	{
		err = VSFERR_FAIL;
		goto end;
	}
	for (i = 0; i < argc; i++)
	{
		if (VSS_COMMENT_CHAR == argv[i][0])
		{
			argc = (uint16_t)i;
			break;
		}
	}
	// empty line or comment line
	if (0 == argc)
	{
		goto end;
	}
	
	// get param
	param = vss_search_param_in_lists(vss_env.param, VSS_PARAM_EXIT_ON_FAIL);
	if (param != NULL)
	{
		exit_on_fail = (uint8_t)param->value;
	}
	param = vss_search_param_in_lists(vss_env.param, VSS_PARAM_NO_COMMIT);
	if (param != NULL)
	{
		no_commit = (uint8_t)param->value;
	}
	
	// run command
	run_times = vss_env.loop_cnt;
	vss_env.loop_cnt = 0;
	if (!run_times)
	{
		run_times = 1;
	}
	for (i = 0; i < run_times; i++)
	{
		err = vss_run_cmd(argc, (char**)argv);
		if (err && (vss_env.fatal_error || exit_on_fail))
		{
			if (run_times > 1)
			{
				LOG_ERROR("fail to run the %dth times", (int)(i + 1));
			}
			vss_env.exit_mark = -1;
			goto end;
		}
	}
	
	// commit if required
	if ((interfaces != NULL)
		&& (interfaces->peripheral_commit != NULL))
	{
		if (0 == no_commit)
		{
			if (interfaces->peripheral_commit())
			{
				err = VSFERR_FAIL;
			}
		}
	}
end:
	if (buff_in_memory != NULL)
	{
		free(buff_in_memory);
		buff_in_memory = NULL;
	}
	return err;
}

static vsf_err_t vss_run_file(FILE *f, char *head, uint8_t quiet)
{
	struct vss_param_t *param = NULL;
	uint8_t exit_on_fail = 0;
	char cmd_line[VSS_CFG_MAX_LINE_LENGTH], *cmd_ptr;
	uint8_t cur_cmd_quiet, vss_quiet_mode;
	uint32_t i;
	
	vss_quiet_mode = 0;
	vss_env.quiet_mode_ptr = &vss_quiet_mode;
	
	REWIND(f);
	while (1)
	{
		if ((f == stdin) && !quiet && !vss_quiet_mode)
		{
			if (head != NULL)
			{
				PRINTF("%s", head);
			}
			PRINTF(">>>");
		}
		
		// get a line
		if (NULL == FGETS(cmd_line, sizeof(cmd_line), f))
		{
			if (!FEOF(f))
			{
				return VSFERR_FAIL;
			}
			else
			{
				return VSFERR_NONE;
			}
		}
		
		cur_cmd_quiet = 0;
		if (VSS_HIDE_CHAR == vss_get_first_non_space_char(cmd_line, &i))
		{
			i++;
			cur_cmd_quiet = 1;
		}
		cmd_ptr = &cmd_line[i];
		
		if ((f != stdin) && !quiet && !vss_quiet_mode && !cur_cmd_quiet)
		{
			if (head != NULL)
			{
				PRINTF("%s", head);
			}
			PRINTF(">>>");
		}
		
		if ((f != stdin) && !quiet && !vss_quiet_mode && !cur_cmd_quiet)
		{
			// run from non-shell mode, print the command line to run
			// in shell mode, the command line will have been printed in fgets
			PRINTF("%s", cmd_line);
		}
		
		// get param
		param = vss_search_param_in_lists(vss_env.param, VSS_PARAM_EXIT_ON_FAIL);
		if (param != NULL)
		{
			exit_on_fail = (uint8_t)param->value;
		}
		
		if ((vss_env.cur_register_function != NULL) && (cmd_ptr != strstr(cmd_ptr, "function_end")))
		{
			if ((cmd_ptr == strstr(cmd_ptr, "function")) && isspace((int)cmd_ptr[strlen("function")]))
			{
				LOG_ERROR("function nesting not supported");
				return VSFERR_FAIL;
			}
			if (vss_append_function_cmd(vss_env.cur_register_function, cmd_ptr))
			{
				return VSFERR_FAIL;
			}
		}
		else if (vss_run_script(cmd_ptr) &&
				(vss_env.fatal_error || exit_on_fail))
		{
			return VSFERR_FAIL;
		}
		if (!quiet && !vss_quiet_mode && !cur_cmd_quiet)
		{
			PRINTF(LOG_LINE_END);
		}
		if (vss_env.exit_mark != 0)
		{
			if (vss_env.exit_mark > 0)
			{
				vss_env.exit_mark = 0;
			}
			break;
		}
	}
	
	return VSFERR_NONE;
}

// commands
// param
VSS_HANDLER(vss_param_register)
{
	struct vss_param_list_t *pl = NULL;
	struct vss_param_t *param = NULL;
	char *param_list_name = NULL;
	
	VSS_CHECK_ARGC_2(2, 3);
	
	param = vss_search_param_in_lists(vss_env.param, argv[1]);
	if ((param != NULL) || (vss_env.cur_call_function != NULL))
	{
		// already registered
		// or calling function
		return VSFERR_NONE;
	}
	
	if (vss_env.cur_register_function != NULL)
	{
		param_list_name = vss_env.cur_register_function->func_name;
	}
	else
	{
		param_list_name = "env";
	}
	
	pl = vss_search_paramlist(vss_env.param, param_list_name);
	if (NULL == pl)
	{
		return VSFERR_FAIL;
	}
	
	param = (struct vss_param_t *)malloc(sizeof(struct vss_param_t));
	if (NULL == param)
	{
		return VSFERR_NOT_ENOUGH_RESOURCES;
	}
	memset(param, 0, sizeof(*param));
	sllist_init_node(param->list);
	
	param->param_name = strdup(argv[1]);
	if (3 == argc)
	{
		param->help_str = strdup(argv[2]);
	}
	return vss_add_param_to_list(pl, param);
}

VSS_HANDLER(vss_param_value)
{
	struct vss_param_t *param = NULL;
	
	VSS_CHECK_ARGC_2(2, 3);
	
	param = vss_search_param_in_lists(vss_env.param, argv[1]);
	if (NULL == param)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_AS, argv[1], "parameters");
		vss_print_help(argv[0]);
		return VSFERR_FAIL;
	}
	
	if (2 == argc)
	{
		LOG_INFO(INFOMSG_REG_08X, param->param_name, (uint32_t)param->value);
	}
	else if (3 == argc)
	{
		param->value = strtoul(argv[2], NULL, 0);
		vss_param_value(2, argv);
	}
	
	return VSFERR_NONE;
}

VSS_HANDLER(vss_param_str)
{
	struct vss_param_t *param = NULL;
	
	VSS_CHECK_ARGC_2(2, 3);
	
	param = vss_search_param_in_lists(vss_env.param, argv[1]);
	if (NULL == param)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_AS, argv[1], "parameters");
		vss_print_help(argv[0]);
		return VSFERR_FAIL;
	}
	
	if (2 == argc)
	{
		if (param->value_str != NULL)
		{
			LOG_INFO("%s = %s", param->param_name, param->value_str);
		}
		else
		{
			LOG_INFO("%s = NULL", param->param_name);
		}
	}
	else if (3 == argc)
	{
		if (param->value_str != NULL)
		{
			free(param->value_str);
		}
		param->value_str = strdup(argv[2]);
		vss_param_str(2, argv);
	}
	
	return VSFERR_NONE;
}

VSS_HANDLER(vss_param_free)
{
	struct vss_param_list_t *pl;
	
	VSS_CHECK_ARGC_MAX(2);
	
	if ((2 == argc) && (vss_env.param != NULL))
	{
		struct vss_param_t *param;
		
		// search param_list first
		pl = vss_search_paramlist(vss_env.param, argv[1]);
		if (pl != NULL)
		{
			// free param_list
			if (pl == vss_env.param)
			{
				vss_env.param = sllist_get_container(pl->list.next,
											struct vss_param_list_t, list);
			}
			else
			{
				struct vss_param_list_t *plpoll = vss_env.param, *pltemp;
				
				while (plpoll != NULL)
				{
					pltemp = sllist_get_container(plpoll->list.next,
											struct vss_param_list_t, list);
					if (pltemp == pl)
					{
						pltemp = sllist_get_container(pl->list.next,
											struct vss_param_list_t, list);
						if (pltemp != NULL)
						{
							sllist_insert(plpoll->list, pltemp->list);
						}
						else
						{
							sllist_init_node(plpoll->list);
						}
						break;
					}
					plpoll = pltemp;
				}
				if (NULL == plpoll)
				{
					// shouldn't run here
					return VSFERR_FAIL;
				}
			}
			
			return vss_free_param_list(pl);
		}
		
		// search param
		pl = vss_env.param;
		while (pl != NULL)
		{
			param = vss_search_param_in_list(pl->param, argv[1]);
			if (param != NULL)
			{
				if (param == pl->param)
				{
					pl->param = sllist_get_container(param->list.next,
													struct vss_param_t, list);
				}
				else
				{
					struct vss_param_t *param_poll = pl->param, *param_temp;
					
					while (param_poll != NULL)
					{
						param_temp = sllist_get_container(param_poll->list.next,
													struct vss_param_t, list);
						if (param_temp == param)
						{
							param_temp = sllist_get_container(param->list.next,
													struct vss_param_t, list);
							if (param_temp != NULL)
							{
								sllist_insert(param_poll->list,
												param_temp->list);
							}
							else
							{
								sllist_init_node(param_poll->list);
							}
							break;
						}
						param_poll = param_temp;
					}
					if (NULL == param_poll)
					{
						// shouldn't run here
						return VSFERR_FAIL;
					}
				}
				return vss_free_param_node(param);
			}
		}
	}
	else
	{
		// free all parameters
		struct vss_param_list_t *ptemp;
		
		pl = vss_env.param;
		while (pl != NULL)
		{
			ptemp = sllist_get_container(pl->list.next, struct vss_param_list_t,
											list);
			vss_free_param_list(pl);
			pl = ptemp;
		}
	}
	
	return VSFERR_NONE;
}

// help
static void vss_print_cmd_help(struct vss_cmd_t *cmd, char *prefix)
{
	char *cur_prefix;
	
	while (cmd->cmd_name != NULL)
	{
		if (cmd->subcmd != NULL)
		{
			cur_prefix =
				(char *)malloc(strlen(prefix) + strlen(cmd->cmd_name) + 2);
			if (NULL == cur_prefix)
			{
				return;
			}
			
			LOG_INFO("  %s.%s: %s", prefix, cmd->cmd_name, cmd->help_str);
			
			strcpy(cur_prefix, prefix);
			strcat(cur_prefix, ".");
			strcat(cur_prefix, cmd->cmd_name);
			vss_print_cmd_help(cmd->subcmd, cur_prefix);
			free(cur_prefix);
			cur_prefix = NULL;
		}
		else
		{
			LOG_INFO("  %s.%s: %s", prefix, cmd->cmd_name, cmd->help_str);
		}
		cmd++;
	}
}

VSS_HANDLER(vss_help)
{
	struct vss_cmd_list_t *temp = NULL;
	struct vss_cmd_t *cmd = NULL;
	
	VSS_CHECK_ARGC_2(1, 2);
	
	if (2 == argc)
	{
		cmd = vss_search_cmd_in_list(vss_env.cmd, argv[1]);
		
		if (NULL == cmd)
		{
			LOG_ERROR(ERRMSG_NOT_SUPPORT, argv[1]);
		}
		else
		{
			LOG_INFO("command %s:", argv[1]);
			LOG_INFO("  %s: %s", cmd->cmd_name, cmd->help_str);
		}
	}
	else if (1 == argc)
	{
		LOG_INFO("command list:");
		
		temp = vss_env.cmd;
		while (temp != NULL)
		{
			vss_print_cmd_help(temp->cmd, temp->list_name);
			temp = sllist_get_container(temp->list.next, struct vss_cmd_list_t,
										list);
		}
	}
	return VSFERR_NONE;
}

// shell
VSS_HANDLER(vss_shell)
{
	VSS_CHECK_ARGC(1);
	
	LOG_INFO("enter shell mode.");
	
	if (interfaces != NULL)
	{
		return vss_run_file(stdin, interfaces->name, 0);
	}
	else
	{
		return vss_run_file(stdin, "stdin", 0);
	}
}

// run
VSS_HANDLER(vss_run)
{
	FILE *f = NULL;
	uint8_t quiet = 0;
	vsf_err_t err = VSFERR_NONE;
	
	VSS_CHECK_ARGC_2(2, 3);
	
	if ((3 == argc) && (!strcmp(argv[2], "quiet")))
	{
		quiet = 1;
	}
	
	f = FOPEN(argv[1], "rt");
	if (NULL == f)
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "open script file", argv[1]);
		return VSFERR_FAIL;
	}
	else
	{
		err = vss_run_file(f, (char*)argv[1], quiet);
		FCLOSE(f);
	}
	return err;
}

// loop
VSS_HANDLER(vss_loop)
{
	VSS_CHECK_ARGC(2);
	
	vss_env.loop_cnt = strtoul(argv[1], NULL, 0);
	return VSFERR_NONE;
}

// exit
VSS_HANDLER(vss_exit)
{
	VSS_CHECK_ARGC(1);
	vss_env.exit_mark = 1;
	return VSFERR_NONE;
}

// close
VSS_HANDLER(vss_close)
{
	VSS_CHECK_ARGC(1);
	vss_env.exit_mark = -1;
	return VSFERR_NONE;
}

VSS_HANDLER(vss_run_command)
{
	VSS_CHECK_ARGC(2);
	return vss_run_script((char *)argv[1]);
}

VSS_HANDLER(vss_out)
{
	VSS_CHECK_ARGC_MAX(2);
	if (1 == argc)
	{
		PRINTF(LOG_LINE_END);
	}
	else
	{
		PRINTF("%s" LOG_LINE_END, argv[1]);
	}
	return VSFERR_NONE;
}

VSS_HANDLER(vss_log_info)
{
	VSS_CHECK_ARGC(2);
	LOG_INFO("%s", argv[1]);
	return VSFERR_NONE;
}

VSS_HANDLER(vss_getchar)
{
	VSS_CHECK_ARGC(1);
	GETCHAR();
	return VSFERR_NONE;
}

VSS_HANDLER(vss_sleep)
{
	uint32_t ms;
	VSS_CHECK_ARGC(2);
	ms = (uint32_t)strtoul(argv[1], NULL, 0);
	sleep_ms(ms);
	return VSFERR_NONE;
}

VSS_HANDLER(vss_quiet)
{
	VSS_CHECK_ARGC_RANGE(1, 2);
	if (NULL == vss_env.quiet_mode_ptr)
	{
		return VSFERR_NONE;
	}
	
	if (1 == argc)
	{
		*vss_env.quiet_mode_ptr = 1;
	}
	else if (2 == argc)
	{
		*vss_env.quiet_mode_ptr = (uint8_t)strtoul(argv[1], NULL, 0);
	}
	return VSFERR_NONE;
}

VSS_HANDLER(vss_function_register)
{
	struct vss_param_list_t *param_list;
	struct vss_function_t *func;
	
	VSS_CHECK_ARGC_2(2, 3);
	
	if (vss_env.cur_register_function != NULL)
	{
		LOG_ERROR("function nesting");
		return VSFERR_FAIL;
	}
	
	if (NULL != vss_search_function_in_list(vss_env.func, (char *)argv[1]))
	{
		LOG_ERROR("function %s already registered!!", argv[1]);
		return VSFERR_FAIL;
	}
	
	func = (struct vss_function_t *)malloc(sizeof(struct vss_function_t));
	if (NULL == func)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return VSFERR_NOT_ENOUGH_RESOURCES;
	}
	memset(func, 0, sizeof(*func));
	sllist_init_node(func->list);
	if (3 == argc)
	{
		func->param_number = (uint16_t)strtoul(argv[2], NULL, 0);
	}
	
	func->func_name = strdup(argv[1]);
	if (NULL == func->func_name)
	{
		free(func);
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return VSFERR_NOT_ENOUGH_RESOURCES;
	}
	
	if (NULL == vss_env.func)
	{
		sllist_init_node(func->list);
	}
	else
	{
		sllist_insert(func->list, vss_env.func->list);
	}
	vss_env.cur_register_function = vss_env.func = func;
	
	// register function parameters
	param_list =
		(struct vss_param_list_t *)malloc(sizeof(struct vss_param_list_t));
	if (NULL == param_list)
	{
		return VSFERR_NOT_ENOUGH_RESOURCES;
	}
	memset(param_list, 0, sizeof(*param_list));
	param_list->list_name = strdup(vss_env.cur_register_function->func_name);
	sllist_init_node(param_list->list);
	return vss_register_param_list(param_list);
}

VSS_HANDLER(vss_function_end)
{
	vsf_err_t err;
	VSS_CHECK_ARGC(1);
	
	if (NULL == vss_env.cur_register_function)
	{
		return VSFERR_FAIL;
	}
	
	err = vss_append_function_cmd(vss_env.cur_register_function, NULL);
	vss_env.cur_register_function = NULL;
	return err;
}

VSS_HANDLER(vss_function_call)
{
	struct vss_function_t *func, *cur_call_func;
	vsf_err_t err;
	
	VSS_CHECK_ARGC_MIN(2);
	
	func = vss_search_function_in_list(vss_env.func, (char *)argv[1]);
	if (NULL == func)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, argv[1]);
		return VSFERR_FAIL;
	}
	
	if (func->param_number != (argc - 2))
	{
		LOG_ERROR(ERRMSG_INVALID_TARGET, "parameter number");
		return VSFERR_FAIL;
	}
	
	cur_call_func = vss_env.cur_call_function;
	vss_env.cur_call_function = func;
	err = vss_run_function(func, argc - 1, &argv[1]);
	vss_env.cur_call_function = cur_call_func;
	return err;
}

VSS_HANDLER(vss_function_free)
{
	struct vss_function_t *f = vss_env.func;
	
	VSS_CHECK_ARGC_MAX(2);
	
	if ((2 == argc) && (vss_env.func != NULL))
	{
		if (!strcmp(f->func_name, argv[1]))
		{
			vss_env.func =
				sllist_get_container(f->list.next, struct vss_function_t, list);
			return vss_free_function_node(f);
		}
		else
		{
			struct vss_function_t *fnext, *ftemp;
			
			while (f != NULL)
			{
				fnext = sllist_get_container(f->list.next,
												struct vss_function_t, list);
				if ((fnext != NULL) && !strcmp(fnext->func_name, argv[1]))
				{
					ftemp = sllist_get_container(fnext->list.next,
												struct vss_function_t, list);
					if (ftemp != NULL)
					{
						sllist_insert(f->list, ftemp->list);
					}
					else
					{
						sllist_init_node(f->list);
					}
					return vss_free_function_node(fnext);
				}
				f = fnext;
			}
		}
	}
	else
	{
		// free all functions
		struct vss_function_t *ftemp;
		
		while (f != NULL)
		{
			ftemp = sllist_get_container(f->list.next, struct vss_function_t, list);
			vss_free_function_node(f);
			f = ftemp;
		}
	}
	
	return VSFERR_NONE;
}

// math
VSS_HANDLER(vss_math_add)
{
	struct vss_param_t *param = NULL;
	
	VSS_CHECK_ARGC(4);
	
	param = vss_search_param_in_lists(vss_env.param, argv[1]);
	if (NULL == param)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_AS, argv[1], "parameters");
		vss_print_help(argv[0]);
		return VSFERR_FAIL;
	}
	
	param->value = strtoul(argv[2], NULL, 0) + strtoul(argv[3], NULL, 0);
	return VSFERR_NONE;
}

VSS_HANDLER(vss_math_sub)
{
	struct vss_param_t *param = NULL;
	
	VSS_CHECK_ARGC(4);
	
	param = vss_search_param_in_lists(vss_env.param, argv[1]);
	if (NULL == param)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_AS, argv[1], "parameters");
		vss_print_help(argv[0]);
		return VSFERR_FAIL;
	}
	
	param->value = strtoul(argv[2], NULL, 0) - strtoul(argv[3], NULL, 0);
	return VSFERR_NONE;
}

VSS_HANDLER(vss_math_mul)
{
	struct vss_param_t *param = NULL;
	
	VSS_CHECK_ARGC(4);
	
	param = vss_search_param_in_lists(vss_env.param, argv[1]);
	if (NULL == param)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_AS, argv[1], "parameters");
		vss_print_help(argv[0]);
		return VSFERR_FAIL;
	}
	
	param->value = strtoul(argv[2], NULL, 0) * strtoul(argv[3], NULL, 0);
	return VSFERR_NONE;
}

VSS_HANDLER(vss_math_div)
{
	struct vss_param_t *param = NULL;
	
	VSS_CHECK_ARGC(4);
	
	param = vss_search_param_in_lists(vss_env.param, argv[1]);
	if (NULL == param)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_AS, argv[1], "parameters");
		vss_print_help(argv[0]);
		return VSFERR_FAIL;
	}
	
	param->value = strtoul(argv[2], NULL, 0) / strtoul(argv[3], NULL, 0);
	return VSFERR_NONE;
}

VSS_HANDLER(vss_math_mod)
{
	struct vss_param_t *param = NULL;
	
	VSS_CHECK_ARGC(4);
	
	param = vss_search_param_in_lists(vss_env.param, argv[1]);
	if (NULL == param)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_AS, argv[1], "parameters");
		vss_print_help(argv[0]);
		return VSFERR_FAIL;
	}
	
	param->value = strtoul(argv[2], NULL, 0) % strtoul(argv[3], NULL, 0);
	return VSFERR_NONE;
}

