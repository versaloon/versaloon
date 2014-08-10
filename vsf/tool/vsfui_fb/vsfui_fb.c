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

#include "app_type.h"
#include "tool/buffer/buffer.h"

#include "dal/mal/mal.h"
#include "vsfui_fb.h"

// frame_buffer driver for vsfui

vsf_err_t vsfui_fb_init(struct vsfui_fb_t *vsfui_fb)
{
	if (vsf_multibuf_init(vsfui_fb->mbuffer) ||
		(NULL == vsfui_fb->screen.dal) ||
		mal.init(vsfui_fb->screen.dal))
	{
		return VSFERR_FAIL;
	}
	vsfui_fb->displaying = false;
	return VSFERR_NONE;
}

vsf_err_t vsfui_fb_fini(struct vsfui_fb_t *vsfui_fb)
{
	return mal.fini(vsfui_fb->screen.dal);
}

void* vsfui_fb_get_buffer(struct vsfui_fb_t *vsfui_fb)
{
	return vsf_multibuf_get_empty(vsfui_fb->mbuffer);
}

vsf_err_t vsfui_fb_validate_buffer(struct vsfui_fb_t *vsfui_fb)
{
	return vsf_multibuf_push(vsfui_fb->mbuffer);
}

vsf_err_t vsfui_fb_poll(struct vsfui_fb_t *vsfui_fb)
{
	struct dal_info_t *dal_info = vsfui_fb->screen.dal;
	struct mal_info_t *mal_info = (struct mal_info_t *)dal_info->extra;
	uint64_t block_size = mal_info->capacity.block_size;
	uint64_t block_num = mal_info->capacity.block_number;
	uint64_t cur_addr;
	uint8_t *buffer = vsf_multibuf_get_payload(vsfui_fb->mbuffer);
	
	if (NULL == buffer)
	{
		return VSFERR_NONE;
	}
	
	if (vsfui_fb->displaying)
	{
		vsf_err_t err;
		
		cur_addr = vsfui_fb->cur_block * block_size;
		
		err = mal.writeblock_nb_isready(dal_info, cur_addr, buffer);
		if (err < 0)
		{
			return err;
		}
		if (!err)
		{
			vsfui_fb->cur_block++;
			cur_addr = vsfui_fb->cur_block * block_size;
			if (vsfui_fb->cur_block >= block_num)
			{
				mal.writeblock_nb_end(dal_info);
				vsf_multibuf_pop(vsfui_fb->mbuffer);
				vsfui_fb->displaying = false;
			}
			else
			{
				return mal.writeblock_nb(dal_info, cur_addr, &buffer[cur_addr]);
			}
		}
	}
	else
	{
		vsfui_fb->cur_block = 0;
		cur_addr = vsfui_fb->cur_block * block_size;
		if (mal.writeblock_nb_start(dal_info, 0, block_num, buffer) ||
			mal.writeblock_nb(dal_info, cur_addr, &buffer[cur_addr]))
		{
			return VSFERR_NONE;
		}
		vsfui_fb->displaying  = true;
	}
	return VSFERR_NONE;
}
