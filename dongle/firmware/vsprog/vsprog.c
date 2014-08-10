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
#define VSPROG_VERSION		"VSProg " VERSION " " RELSTR PKGBLDREV
#define VSPROG_COPYRIGHT	\
				"CopyRight(c) 2008-2010 by SimonQian <SimonQian@SimonQian.com>"

#include "config.h"

#include <stdlib.h>

#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_log.h"
#include "app_err.h"

#include "usb_protocol.h"

#include "interfaces.h"
#include "vsprog.h"
#include "target.h"
#include "scripts.h"
#include "app_scripts.h"

#include "interfaces_script.h"
#include "pgbar.h"
#include "memlist.h"
#include "strparser.h"
#include "comisp.h"

#include "vsprog_ui.h"

VSS_HANDLER(vsprog_help);
VSS_HANDLER(vsprog_version);
VSS_HANDLER(vsprog_debug_level);
VSS_HANDLER(vsprog_support);
VSS_HANDLER(vsprog_operation);
VSS_HANDLER(vsprog_mass);
VSS_HANDLER(vsprog_free_all);
VSS_HANDLER(vsprog_init);
VSS_HANDLER(vsprog_select_slot);
VSS_HANDLER(vsprog_info);
VSS_HANDLER(vsprog_program);
VSS_HANDLER(vsprog_auto_program);

VSS_HANDLER(vsprog_wait_key_down);
VSS_HANDLER(vsprog_wait_key_up);
VSS_HANDLER(vsprog_wait_key_press);

static const struct vss_cmd_t vsprog_key_cmd[] =
{
	VSS_CMD(	"wait_down",
				"wait key pushed down, format: key.wait_down",
				vsprog_wait_key_down,
				NULL),
	VSS_CMD(	"wait_up",
				"wait key released, format: key.wait_up",
				vsprog_wait_key_up,
				NULL),
	VSS_CMD(	"wait_press",
				"wait key pressed, format: key.wait_press",
				vsprog_wait_key_press,
				NULL),
	VSS_CMD_END
};

static const struct vss_cmd_t vsprog_cmd[] =
{
	VSS_CMD(	"help",
				"show help, format: help/h",
				vsprog_help,
				NULL),
	VSS_CMD(	"h",
				"show help, format: help/h",
				vsprog_help,
				NULL),
	VSS_CMD(	"version",
				"show version, format: version/v",
				vsprog_version,
				NULL),
	VSS_CMD(	"v",
				"show version, format: version/v",
				vsprog_version,
				NULL),
	VSS_CMD(	"debug",
				"set debug level, format: debug/D LEVEL",
				vsprog_debug_level,
				NULL),
	VSS_CMD(	"d",
				"set debug level, format: debug/D LEVEL",
				vsprog_debug_level,
				NULL),
	VSS_CMD(	"support",
				"display support information, format: support/S [TARGET]",
				vsprog_support,
				NULL),
	VSS_CMD(	"S",
				"display support information, format: support/S [TARGET]",
				vsprog_support,
				NULL),
	VSS_CMD(	"operation",
				"define operations, format: operation/o [OPERATIONS]",
				vsprog_operation,
				NULL),
	VSS_CMD(	"o",
				"define operations, format: operation/o [OPERATIONS]",
				vsprog_operation,
				NULL),
	VSS_CMD(	"mass-product",
				"enable mass product mode, format: mass-product/M",
				vsprog_mass,
				NULL),
	VSS_CMD(	"M",
				"enable mass product mode, format: mass-product/M",
				vsprog_mass,
				NULL),
	VSS_CMD(	"free-all",
				"free everything, format: free-all",
				vsprog_free_all,
				NULL),
	VSS_CMD(	"init",
				"vsprog initialization, format: init",
				vsprog_init,
				NULL),
	VSS_CMD(	"key",
				"key functions",
				NULL,
				vsprog_key_cmd),
	VSS_CMD(	"slot",
				"select programming slot, format: slot/Y NUMBER",
				vsprog_select_slot,
				NULL),
	VSS_CMD(	"Y",
				"select programming slot, format: slot/Y NUMBER",
				vsprog_select_slot,
				NULL),
	VSS_CMD(	"info",
				"get target information, format: info",
				vsprog_info,
				NULL),
	VSS_CMD(	"program",
				"run program procedure, format: program",
				vsprog_program,
				NULL),
	VSS_CMD(	"auto_program",
				"run auto_program procedure, format: auto_program",
				vsprog_auto_program,
				NULL),
	VSS_CMD_END
};
struct vss_cmd_list_t vsprog_cmd_list = VSS_CMD_LIST("vsprog", vsprog_cmd);

int verbosity = LOG_DEFAULT_LEVEL;
int verbosity_pos = 0;
int verbosity_stack[16];

static struct operation_t operations[TARGET_SLOT_NUMBER];
static struct program_context_t context[TARGET_SLOT_NUMBER];
static struct chip_param_t target_chip_param[TARGET_SLOT_NUMBER];
static struct program_info_t program_info[TARGET_SLOT_NUMBER];
static struct chip_series_t target_chips[TARGET_SLOT_NUMBER];
struct program_context_t *cur_context = NULL;

static void free_all(void)
{
	struct program_area_t *prog_area = NULL;
	uint32_t i, j;
	
	// free program buffer
	for (i = 0; i < TARGET_SLOT_NUMBER; i++)
	{
		context[i].target = NULL;
		target_data_free(&context[i]);
		
		memset(&operations[i], 0, sizeof(operations[i]));
		
		if (target_chip_param[i].chip_areas != NULL)
		{
			target_chip_area_free(target_chip_param[i].chip_areas);
		}
		memset(&target_chip_param[i], 0, sizeof(target_chip_param[i]));
		
		if (program_info[i].chip_name != NULL)
		{
			free(program_info[i].chip_name);
			program_info[i].chip_name = NULL;
		}
		if (program_info[i].chip_type != NULL)
		{
			free(program_info[i].chip_type);
			program_info[i].chip_type = NULL;
		}
		if (program_info[i].ifs_indexes != NULL)
		{
			free(program_info[i].ifs_indexes);
			program_info[i].ifs_indexes = NULL;
		}
		for (j = 0; j < dimof(target_area_name); j++)
		{
			prog_area = target_get_program_area(&program_info[i], j);
			if (prog_area != NULL)
			{
				if (prog_area->cli_str != NULL)
				{
					free(prog_area->cli_str);
					prog_area->cli_str = NULL;
				}
			}
		}
		memset(&program_info[i], 0, sizeof(program_info[i]));
		
		target_release_chip_series(&target_chips[i]);
	}
#if TARGET_SLOT_NUMBER > 1
	cur_context = NULL;
#endif
}

static vsf_err_t parse_operation(uint32_t *operation, const char *opt,
									uint32_t optlen)
{
	uint32_t mask = 0, tmp;
	uint32_t i;
	
#if PARAM_CHECK
	if ((NULL == operation) || (NULL == opt))
	{
		LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
		return VSFERR_INVALID_PARAMETER;
	}
#endif
	
	for (i = 0; i < optlen; i++)
	{
		tmp = target_area_mask(opt[i]);
		if (tmp == 0)
		{
			LOG_ERROR(ERRMSG_INVALID_CHARACTER, opt[i], "target area");
			return VSFERR_FAIL;
		}
		mask |= tmp;
	}
	
	*operation = mask;
	return VSFERR_NONE;
}

static void print_title(void)
{
	PRINTF(_GETTEXT(VSPROG_VERSION LOG_LINE_END VSPROG_COPYRIGHT LOG_LINE_END));
	PRINTF(_GETTEXT(LOG_LINE_END));
	PRINTF(_GETTEXT("URL: http://www.SimonQian.com/en/Versaloon"LOG_LINE_END));
	PRINTF(_GETTEXT("mail: SimonQian@SimonQian.com" LOG_LINE_END LOG_LINE_END));
}

static void print_system_info(void)
{
	PRINTF("System Information:" LOG_LINE_END);
	PRINTF(LOG_LINE_END);
}

VSS_HANDLER(vsprog_help)
{
	VSS_CHECK_ARGC(1);
	
	PRINTF(_GETTEXT("Usage: %s [OPTION]..."LOG_LINE_END), "vsprog");
	PRINTF(_GETTEXT("  -h,  --help                               display this help"LOG_LINE_END));
	PRINTF(_GETTEXT("  -v,  --version                            display vsprog version"LOG_LINE_END));
	PRINTF(_GETTEXT("  -S,  --support <TARGET>                   display support information"LOG_LINE_END));
	PRINTF(_GETTEXT("  -V,  --vss-cmd \"<CMD PARA>\"               run programmer defined command"LOG_LINE_END));
	PRINTF(_GETTEXT("  -P,  --parameter <AREA>                   display parameter for target area"LOG_LINE_END));
	PRINTF(_GETTEXT("  -D,  --memory-detail <AREA>               display memory info for target area"LOG_LINE_END));
	PRINTF(_GETTEXT("  -J,  --jtag-dc <UB UA BB BA>              set JTAG Daisy Chain"LOG_LINE_END));
	PRINTF(_GETTEXT("  -d,  --debug <LEVEL>                      set debug level <0-2>"LOG_LINE_END));
	PRINTF(_GETTEXT("  -s,  --target-series <SERIES>             set target series"LOG_LINE_END));
	PRINTF(_GETTEXT("  -c,  --target-module <MODULE>             set target module"LOG_LINE_END));
	PRINTF(_GETTEXT("  -p,  --programmer <PROGRAMMER>            set programmer"LOG_LINE_END));
	PRINTF(_GETTEXT("  -l,  --virtualprog <VIRTUAL_PROGRAMMER>   set virtual programmer"LOG_LINE_END));
	PRINTF(_GETTEXT("  -i,  --indexes <INDEX_STR>                configure indexes of virtual programmer"LOG_LINE_END));
	PRINTF(_GETTEXT("  -o,  --operation <OPERATIONS>             set programming operation"LOG_LINE_END));
	PRINTF(_GETTEXT("  -e,  --erase-on-demand                    erase target according to demand"LOG_LINE_END));
	PRINTF(_GETTEXT("  -I,  --input-file \"<FILE>[@SEG,ADDR]\"     set input file"LOG_LINE_END));
	PRINTF(_GETTEXT("  -O,  --output-file \"<FILE>[@SEG,ADDR]\"    set output file"LOG_LINE_END));
	PRINTF(_GETTEXT("  -F,  --frequency <FREQUENCY_KHZ>          set programming frequency"LOG_LINE_END));
	PRINTF(_GETTEXT("  -K,  --kernel-khz <KERNEL_KHZ>            set kernel frequency"LOG_LINE_END));
	PRINTF(_GETTEXT("  -Q,  --quartz-khz <QUARTZ_KHZ>            set quartz frequency"LOG_LINE_END));
	PRINTF(_GETTEXT("  -A,  --auto-adjust                        enable auto-adjust feature"LOG_LINE_END));
	PRINTF(_GETTEXT("  -m,  --mode <MODE>                        set programming mode"LOG_LINE_END));
	PRINTF(_GETTEXT("  -t,  --target <TARGET VALUE>              set target value, eg(fuse): -tu0x02"LOG_LINE_END));
	PRINTF(_GETTEXT("  -L,  --list-programmer                    list programmers available"LOG_LINE_END));
	PRINTF(_GETTEXT("  -M,  --mass-product                       set mass_product mode"LOG_LINE_END));
	PRINTF(_GETTEXT("  -G,  --gui-mode                           set gui_mode"LOG_LINE_END));
	PRINTF(_GETTEXT("  -a,  --address                            set address of target chip" LOG_LINE_END));
	PRINTF(_GETTEXT("  -E,  --embedded-vsprog-config             generate config data for embedded vsprog"LOG_LINE_END));
	PRINTF(_GETTEXT("  -Z,  --embedded-vsprog-data               generate target data for embedded vsprog"LOG_LINE_END));
	PRINTF(_GETTEXT("  -Y,  --slot                               select target data slot"LOG_LINE_END));
	PRINTF(_GETTEXT(LOG_LINE_END));

	target_print_help();
	
	return VSFERR_NONE;
}

VSS_HANDLER(vsprog_version)
{
	VSS_CHECK_ARGC(1);
	
	PRINTF(_GETTEXT(VSPROG_VERSION LOG_LINE_END VSPROG_COPYRIGHT LOG_LINE_END LOG_LINE_END));
	PRINTF(_GETTEXT("This is free software; see the source for copying conditions."LOG_LINE_END));
	PRINTF(_GETTEXT("There is NO warranty; not even for MERCHANTABILITY or FITNESS"LOG_LINE_END));
	PRINTF(_GETTEXT("FOR A PARTICULAR PURPOSE." LOG_LINE_END));
	
	return VSFERR_NONE;
}

VSS_HANDLER(vsprog_debug_level)
{
	int value;
	
	VSS_CHECK_ARGC(2);
	
	value = (int)strtoul(argv[1], NULL, 0);
	if ((value < 0) || (value > DEBUG_LEVEL))
	{
		vss_print_help(argv[0]);
		return VSFERR_FAIL;
	}
	
	verbosity = value;
	return VSFERR_NONE;
}

VSS_HANDLER(vsprog_support)
{
	VSS_CHECK_ARGC(2);
	
	if (!strcmp(argv[1], "all"))
	{
		// print system information
		print_system_info();
		// print all Supported programmers
		// print all Supported devices
		target_print_list();
	}
	else if (!strcmp(argv[1], "system"))
	{
		print_system_info();
	}
	else if (!strcmp(argv[1], "target"))
	{
		// print all Supported devices
		target_print_list();
	}
	else
	{
		uint32_t i;
		
		for (i = 0; targets_info[i].name != NULL; i++)
		{
			if (!strcmp(targets_info[i].name, argv[1]))
			{
				target_print_target(i);
				return VSFERR_NONE;
			}
		}
		
		LOG_ERROR(ERRMSG_NOT_SUPPORT, argv[1]);
		LOG_ERROR(ERRMSG_TRY_SUPPORT);
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

VSS_HANDLER(vsprog_operation)
{
	uint32_t argu_num;
	uint32_t *popt_tmp = NULL;
	
	VSS_CHECK_ARGC_2(1, 2);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	if (1 == argc)
	{
		memset(cur_context->op, 0, sizeof(*cur_context->op));
		return VSFERR_NONE;
	}
	
	argu_num = strlen(argv[1]) - 1;
	if (NULL == cur_context->target)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "target");
		return VSFERR_FAIL;
	}
	
	switch (argv[1][0])
	{
	case 'e':
		// Erase
		popt_tmp = &cur_context->op->erase_operations;
		goto Parse_Operation;
	case 'r':
		// Read
		popt_tmp = &cur_context->op->read_operations;
		goto Parse_Operation;
	case 'v':
		// Verify
		popt_tmp = &cur_context->op->verify_operations;
		goto Parse_Operation;
	case 'w':
		// Write
		popt_tmp = &cur_context->op->write_operations;
		goto Parse_Operation;
	case 'i':
		// Information
Parse_Operation:
		if (popt_tmp != NULL)
		{
			if (*popt_tmp != 0)
			{
				LOG_ERROR(ERRMSG_MUTIPLE_DEFINED, "operation");
				return VSFERR_FAIL;
			}
			if (0 == argu_num)
			{
				*popt_tmp = ALL;
			}
			else
			{
				if (parse_operation(popt_tmp, &argv[1][1], argu_num))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse operation");
					return VSFERR_FAIL;
				}
			}
		}
		break;
	default:
		LOG_ERROR(ERRMSG_INVALID_OPERATION, argv[1][0]);
		return VSFERR_FAIL;
		break;
	}
	return VSFERR_NONE;
}

VSS_HANDLER(vsprog_mass)
{
	VSS_CHECK_ARGC(1);
	LOG_ERROR(ERRMSG_NOT_SUPPORT, "mass product mode");
	return VSFERR_FAIL;
}

VSS_HANDLER(vsprog_free_all)
{
	VSS_CHECK_ARGC(1);
	free_all();
	return VSFERR_NONE;
}

VSS_HANDLER(vsprog_init)
{
	extern struct vss_cmd_list_t target_data_cmd_list;
	
	VSS_CHECK_ARGC(1);
	
	print_title();
	vsprog_ui_set_title(VSPROG_VERSION);
	
	vss_register_cmd_list(&target_cmd_list);
	vss_register_cmd_list(&target_data_cmd_list);
	vss_register_cmd_list(&pgbar_cmd_list);
	vss_register_cmd_list(&interface_cmd_list);
	vss_register_cmd_list(&app_cmd_list);
#if TARGET_COMISP_EN
	vss_register_cmd_list(&comisp_cmd_list);
#endif
	
	return vss_run_script("free-all");
}

VSS_HANDLER(vsprog_select_slot)
{
	uint8_t number;
	
	VSS_CHECK_ARGC(2);
	number = (uint8_t)strtoul(argv[1], NULL, 0);
	if (number >= TARGET_SLOT_NUMBER)
	{
		LOG_ERROR(ERRMSG_INVALID_INDEX, number, "programming slot");
	}
	
	context[number].op = &operations[number];
	context[number].param = &target_chip_param[number];
	context[number].pi = &program_info[number];
	context[number].series = &target_chips[number];
	cur_context = &context[number];
	return VSFERR_NONE;
}

VSS_HANDLER(vsprog_info)
{
	struct operation_t operation_temp, *operation_orig;
	vsf_err_t err;
	
	VSS_CHECK_ARGC(1);
	
	operation_orig = cur_context->op;
	memset(&operation_temp, 0, sizeof(operation_temp));
	operation_temp.read_operations = CHIPID;
	cur_context->op = &operation_temp;
	err = vss_run_script("program 0");
	cur_context->op = operation_orig;
	
	return err;
}

VSS_HANDLER(vsprog_program)
{
	bool led = true;
	
	VSS_CHECK_ARGC_2(1, 2);
	if (2 == argc)
	{
		led = (bool)strtoul(argv[1], NULL, 0);
	}
	
	if (led)
	{
		LED_STATE_R_OFF();
		LED_STATE_G_OFF();
	}
	if (vss_run_script("enter_program_mode") ||
		vss_run_script("operate"))
	{
		if (led)
		{
			LED_STATE_R_ON();
		}
		vss_run_script("leave_program_mode 0");
		return VSFERR_FAIL;
	}
	if (vss_run_script("leave_program_mode 1"))
	{
		if (led)
		{
			LED_STATE_R_ON();
		}
		return VSFERR_FAIL;
	}
	if (led)
	{
		LED_STATE_G_ON();
	}
	return VSFERR_NONE;
}

VSS_HANDLER(vsprog_auto_program)
{
	VSS_CHECK_ARGC(1);
	
	while (1)
	{
		// wait for target insert
		LOG_INFO("Waiting target connection ...\n");
		LOG_PUSH();
		LOG_MUTE();
		while (vss_run_script("info"))
		{
			usb_protocol_poll();
		}
		LOG_POP();
		
		// program
		vss_run_script("program 1");
		
		// wait for target remove
		LOG_INFO("Waiting target removal ...\n");
		LOG_PUSH();
		LOG_MUTE();
		while (!vss_run_script("info"))
		{
			usb_protocol_poll();
		}
		LOG_POP();
	}
	return VSFERR_NONE;
}

VSS_HANDLER(vsprog_wait_key_down)
{
	uint32_t key_count = 0;
	
	VSS_CHECK_ARGC(1);
	
	while (1)
	{
		usb_protocol_poll();
		if (KEY_IsDown())
		{
			if (++key_count > 0x1000)
			{
				break;
			}
		}
		else
		{
			key_count = 0;
		}
	}
	return VSFERR_NONE;
}

VSS_HANDLER(vsprog_wait_key_up)
{
	uint32_t key_count = 0;
	
	VSS_CHECK_ARGC(1);
	
	while (1)
	{
		usb_protocol_poll();
		if (!KEY_IsDown())
		{
			if (++key_count > 0x1000)
			{
				break;
			}
		}
		else
		{
			key_count = 0;
		}
	}
	return VSFERR_NONE;
}

VSS_HANDLER(vsprog_wait_key_press)
{
	VSS_CHECK_ARGC(1);
	
	if (vss_run_script("vsprog.key.wait_down") ||
		vss_run_script("vsprog.key.wait_up"))
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}
