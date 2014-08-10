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

#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "interfaces.h"
#include "target.h"

vsf_err_t target_release_chip_fl(struct chip_fl_t *fl)
{
	LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, "chip_fl_t", "embedded vsprog");
	return VSFERR_FAIL;
}

vsf_err_t target_build_chip_fl(struct target_info_t *target,
				const char *chip_module, char *type, struct chip_fl_t *fl)
{
	LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, "chip_fl_t", "embedded vsprog");
	return VSFERR_FAIL;
}

vsf_err_t target_release_chip_series(struct chip_series_t *s)
{
	return VSFERR_NONE;
}

vsf_err_t target_build_chip_series(struct target_info_t *target,
		const struct program_mode_t *program_mode, struct chip_series_t *s)
{
	uint32_t pos = TARGET_CFG_ADDR, cur_pos = TARGET_CFG_ADDR;
	char *series_name = NULL;
	
	do {
		series_name = *(char **)pos;
		cur_pos = pos;
		pos += *(uint32_t *)(pos + sizeof(char *));
		if (!strcmp(series_name, target->name))
		{
			break;
		}
	} while (series_name != NULL);
	if (NULL == series_name)
	{
		return VSFERR_FAIL;
	}
	
	s->series_name = *(char **)cur_pos;
	cur_pos += sizeof(char *);
	s->size = *(uint32_t *)cur_pos;
	cur_pos += sizeof(uint32_t);
	s->num_of_chips = *(uint32_t *)cur_pos;
	cur_pos += sizeof(uint32_t);
	s->chips_param = *(struct chip_param_t **)cur_pos;
	
	return VSFERR_NONE;
}

