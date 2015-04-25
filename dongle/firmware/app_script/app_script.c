
#include <stdlib.h>
#include <ctype.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_log.h"
#include "app_err.h"

#include "interfaces.h"
#include "scripts.h"
#include "app_script.h"

#include "strparser.h"
#include "dal/mal/mal.h"

#if HW_HAS_BEEPER
VSS_HANDLER(app_beeper_init);
VSS_HANDLER(app_beeper_on);
VSS_HANDLER(app_beeper_off);

static const struct vss_cmd_t beeper_cmd[] =
{
	VSS_CMD(	"init",
				"initialize beeper, format: beeper.init",
				app_beeper_init,
				NULL),
	VSS_CMD(	"on",
				"set beeper on, format: beeper on",
				app_beeper_on,
				NULL),
	VSS_CMD(	"off",
				"set beeper off, format: beeper.off",
				app_beeper_off,
				NULL),
	VSS_CMD_END
};
#endif

#if HW_HAS_7COLOR_LED
VSS_HANDLER(app_led7c_init);
VSS_HANDLER(app_led7c_set);

static const struct vss_cmd_t led7c_cmd[] =
{
	VSS_CMD(	"init",
				"initialize 7-color led, format: led7c.init",
				app_led7c_init,
				NULL),
	VSS_CMD(	"set",
				"set 7-color led, format: led7c.set COLOR",
				app_led7c_set,
				NULL),
	VSS_CMD_END
};
#endif

#if HW_HAS_LEDARRAY
VSS_HANDLER(app_ledarr_init);
VSS_HANDLER(app_ledarr_set);

static const struct vss_cmd_t ledarr_cmd[] =
{
	VSS_CMD(	"init",
				"initialize led array, format: ledarr.init",
				app_ledarr_init,
				NULL),
	VSS_CMD(	"set",
				"set led array value, format: ledarr.set VALUE",
				app_ledarr_set,
				NULL),
	VSS_CMD_END
};
#endif

#if HW_HAS_LCM
VSS_HANDLER(app_lcm_init);
VSS_HANDLER(app_lcm_backlight);

static const struct vss_cmd_t lcm_cmd[] =
{
	VSS_CMD(	"init",
				"initialize lcm, format: lcm.init",
				app_lcm_init,
				NULL),
	VSS_CMD(	"backlight",
				"set backlight of lcm, format: lcm.backligh [ON|OFF]",
				app_lcm_backlight,
				NULL),
	VSS_CMD_END
};
#endif

VSS_HANDLER(dal_vss_init);
VSS_HANDLER(dal_vss_fini);

static const struct vss_cmd_t dal_cmd[] =
{
	VSS_CMD(	"init",
				"initialize driver abstraction layer, format: dal.init KHZ",
				dal_vss_init,
				NULL),
	VSS_CMD(	"fini",
				"finialize driver abstraction layer, format: dal.fini",
				dal_vss_fini,
				NULL),
	VSS_CMD_END
};

VSS_HANDLER(app_show_tickcount);
static const struct vss_cmd_t app_cmd[] =
{
	VSS_CMD(	"time",
				"show ms passed after power-on",
				app_show_tickcount,
				NULL),
	VSS_CMD(	"dal",
				"dal processors",
				dal_vss_init,
				dal_cmd),
#if HW_HAS_BEEPER
	VSS_CMD(	"beeper",
				"beeper handler",
				app_beeper_init,
				beeper_cmd),
#endif
#if HW_HAS_LEDARRAY
	VSS_CMD(	"ledarr",
				"led array handler",
				app_ledarr_init,
				ledarr_cmd),
#endif
#if HW_HAS_7COLOR_LED
	VSS_CMD(	"led7c",
				"7-color led handler",
				app_led7c_init,
				led7c_cmd),
#endif
#if HW_HAS_LCM
	VSS_CMD(	"lcm",
				"lcm handler",
				app_lcm_init,
				lcm_cmd),
#endif
	VSS_CMD_END
};

struct vss_cmd_list_t app_cmd_list = VSS_CMD_LIST("app", app_cmd);

VSS_HANDLER(app_show_tickcount)
{
	VSS_CHECK_ARGC(1);
	PRINTF("%dMS" LOG_LINE_END, interfaces->tickclk.get_count());
	return VSFERR_NONE;
}

vsf_err_t dal_commit(void)
{
	return interfaces->peripheral_commit();
}

vsf_err_t dal_config_interface(char *dal_name, char *ifs, struct dal_info_t *info)
{
	uint32_t i;
	uint32_t size;
	struct dal_driver_t *d = NULL;
	uint8_t *buff = NULL;
	vsf_err_t err = VSFERR_NONE;
	
	i = 0;
	while (dal_drivers[i] != NULL)
	{
		if (!strcmp(dal_drivers[i]->name, dal_name))
		{
			d = dal_drivers[i];
			break;
		}
		i++;
	}
	if (NULL == d)
	{
		return VSFERR_FAIL;
	}
	
	size = strparser_getsize(d->ifs_format);
	if (size > 1024)
	{
		LOG_WARNING("ifs_format too large: %d bytes.", size);
	}
	
	buff = (uint8_t *)malloc(size);
	if (NULL == buff)
	{
		return VSFERR_FAIL;
	}
	
	if (strparser_parse(ifs, d->ifs_format, buff, size) ||
		d->parse_interface(info, buff))
	{
		err = VSFERR_FAIL;
	}
	free(buff);
	buff = NULL;
	return err;
}

VSS_HANDLER(dal_vss_init)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	if (interface_assert(&ifs) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return VSFERR_FAIL;
	}
	
	return VSFERR_NONE;
}

VSS_HANDLER(dal_vss_fini)
{
	VSS_CHECK_ARGC(1);
	return VSFERR_NONE;
}

#if HW_HAS_BEEPER
VSS_HANDLER(app_beeper_init)
{
	VSS_CHECK_ARGC(1);
	BEEPER_INIT();
	return VSFERR_NONE;
}

VSS_HANDLER(app_beeper_on)
{
	VSS_CHECK_ARGC(1);
	BEEPER_ON();
	return VSFERR_NONE;
}

VSS_HANDLER(app_beeper_off)
{
	VSS_CHECK_ARGC(1);
	BEEPER_OFF();
	return VSFERR_NONE;
}
#endif

#if HW_HAS_7COLOR_LED
VSS_HANDLER(app_led7c_init)
{
	VSS_CHECK_ARGC(1);
	LED_STATE_INIT();
	return VSFERR_NONE;
}

VSS_HANDLER(app_led7c_set)
{
	VSS_CHECK_ARGC(2);
	LED_STATE_R_OFF();
	LED_STATE_G_OFF();
	LED_STATE_B_OFF();
	if (!strcmp(argv[1], "red"))
	{
		LED_STATE_R_ON();
	}
	else if (!strcmp(argv[1], "green"))
	{
		LED_STATE_G_ON();
	}
	else if (!strcmp(argv[1], "blue"))
	{
		LED_STATE_B_ON();
	}
	else if (!strcmp(argv[1], "yellow"))
	{
		LED_STATE_R_ON();
		LED_STATE_G_ON();
	}
	else if (!strcmp(argv[1], "purple"))
	{
		LED_STATE_R_ON();
		LED_STATE_B_ON();
	}
	else if (!strcmp(argv[1], "unk"))
	{
		// any one know what's this color?
		LED_STATE_B_ON();
		LED_STATE_G_ON();
	}
	else if (!strcmp(argv[1], "white"))
	{
		LED_STATE_R_ON();
		LED_STATE_G_ON();
		LED_STATE_B_ON();
	}
	else if (!strcmp(argv[1], "none"))
	{
		// simply turn all off
	}
	else
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}
#endif

#if HW_HAS_LEDARRAY
VSS_HANDLER(app_ledarr_init)
{
	VSS_CHECK_ARGC(1);
	LED_ARRAY_INIT();
	return VSFERR_NONE;
}

VSS_HANDLER(app_ledarr_set)
{
	uint32_t value;
	
	VSS_CHECK_ARGC(2);
	value = (uint32_t)strtoul(argv[1], NULL, 0);
	LED_ARRAY_SET(value);
	return VSFERR_NONE;
}
#endif

#if HW_HAS_LCM
VSS_HANDLER(app_lcm_init)
{
	VSS_CHECK_ARGC(1);
	
	LCM_BACKLIGHT_INIT();
	return VSFERR_NONE;
}

VSS_HANDLER(app_lcm_backlight)
{
	VSS_CHECK_ARGC(2);
	
	if (!strcmp(argv[1], "ON"))
	{
		LCM_BACKLIGHT_ON();
	}
	else if (!strcmp(argv[1], "OFF"))
	{
		LCM_BACKLIGHT_OFF();
	}
	else
	{
		LOG_ERROR(ERRMSG_INVALID, argv[1], "backlight state");
		vss_print_help(argv[0]);
		return VSFERR_INVALID_PARAMETER;
	}
	return VSFERR_NONE;
}
#endif
