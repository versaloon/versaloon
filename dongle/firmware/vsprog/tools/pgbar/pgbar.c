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

#include "compiler.h"

#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_log.h"
#include "app_err.h"

#include "port.h"
#include "interfaces.h"
#include "pgbar.h"
#include "scripts.h"

#if HW_HAS_LCM
#include "vsprog_ui.h"
#endif

VSS_HANDLER(pgbar_gui);
static struct vss_cmd_t pgbar_cmd[] =
{
	VSS_CMD(	"gui-mode",
				"enable GUI mode, format: gui-mode/G",
				pgbar_gui,
				NULL),
	VSS_CMD(	"G",
				"enable GUI mode, format: gui-mode/G",
				pgbar_gui,
				NULL),
	VSS_CMD_END
};
struct vss_cmd_list_t pgbar_cmd_list = VSS_CMD_LIST("pgbar", pgbar_cmd);

static char *end_str = NULL, disp_char = 0;
static int32_t min_num = 0, max_num = 0, position = 0;
static uint32_t max_num_of_chars = 0;
static uint32_t start_time, end_time;
static uint8_t gui_mode_flag = 0;

static uint32_t pgbar_get_char_num(int32_t pos)
{
	return (uint32_t)((uint64_t)(pos - min_num) * max_num_of_chars / 
						(max_num - min_num));
}

VSS_HANDLER(pgbar_gui)
{
	VSS_CHECK_ARGC(1);
	gui_mode_flag = 1;
	return VSFERR_NONE;
}

void pgbar_update(int32_t step)
{
	int32_t noc;
	uint32_t pos_pre;
#if HW_HAS_LEDARRAY
	uint32_t led_mask;
#endif
	
	pos_pre = position;
	noc = pgbar_get_char_num(position);
	
	// adjust new position
	position += step;
	if (position > max_num)
	{
		position = max_num;
	}
	else if (position < min_num)
	{
		position = min_num;
	}
	
	// output new characters
	noc = pgbar_get_char_num(position) - noc;
	if (noc != 0)
	{
		uint8_t erase = 0;
		uint8_t percentage;
		// update
		// erase previous characters
		if (100 == pgbar_get_char_num(pos_pre) * 100 / max_num_of_chars)
		{
			erase = 4;	// "%100"
		}
		else
		{
			erase = 3;	// "%xx"
		}
		while (erase-- > 0)
		{
			if (!gui_mode_flag)
			{
				PRINTF("\b \b");
			}
		}
		
		while (noc != 0)
		{
			if (noc > 0)
			{
				PRINTF("%c", disp_char);
				noc--;
			}
			else
			{
				PRINTF("\b \b");
				noc++;
			}
		}
		
		// output percentage
		percentage = pgbar_get_char_num(position) * 100 / max_num_of_chars;
		PRINTF("%%%02d", percentage);
#if HW_HAS_LCM
		vsprog_ui_set_progress(percentage);
#endif
#if HW_HAS_LEDARRAY
		led_mask = pgbar_get_char_num(position) * LED_ARRAY_LEN / max_num_of_chars;
		led_mask = (1 << led_mask) - 1;
		LED_ARRAY_SET(led_mask);
#endif
		
		if (gui_mode_flag)
		{
			PRINTF(LOG_LINE_END);
		}
		
		// flush output
		fflush(stdout);
	}
}

vsf_err_t pgbar_init(char *s, char *e, uint32_t min, uint32_t max,
						uint32_t max_chars, char c)
{
	// save settings
	if (e != NULL)
	{
		end_str = (char*)malloc(strlen(e) + 1);
		if (NULL == end_str)
		{
			LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
			return VSFERR_NOT_ENOUGH_RESOURCES;
		}
		strcpy(end_str, (const char *)e);
	}
	max_num = max;
	position = min_num = min;
	max_num_of_chars = max_chars;
	disp_char = c;
	
	// print initial string
	if (NULL != s)
	{
		PRINTF("%s", s);
#if HW_HAS_LCM
		vsprog_ui_set_task(s);
#endif
	}
	// print initial percentage
	PRINTF("%%00");
#if HW_HAS_LCM
	vsprog_ui_set_progress(0);
#endif
	if (gui_mode_flag)
	{
		PRINTF(LOG_LINE_END);
	}
	
	// get start time
	start_time = (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
	
	// flush output
	fflush(stdout);
	return VSFERR_NONE;
}

uint32_t pgbar_fini(void)
{
#if HW_HAS_LEDARRAY
	LED_ARRAY_SET(0);
#endif
	// print final string
	if (end_str != NULL)
	{
		PRINTF("%s ", end_str);
		// free allocated memory
		free(end_str);
		end_str = NULL;
	}
	
	// get current time and calculate time used
	end_time = (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
	PRINTF("%02.02fs used" LOG_LINE_END, (float)(end_time - start_time) / 1000);
	
	// flush output
	fflush(stdout);
	
	return end_time - start_time;
}

