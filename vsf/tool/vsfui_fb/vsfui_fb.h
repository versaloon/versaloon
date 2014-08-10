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

#ifndef __VSFUI_FB_H_INCLUDED__
#define __VSFUI_FB_H_INCLUDED__

struct vsfui_fb_t
{
	struct vsfui_fb_screen_t
	{
		uint16_t width;
		uint16_t height;
		uint8_t pixel_size;
		
		struct dal_info_t *dal;
	} screen;
	
	// buffer
	struct vsf_multibuf_t *mbuffer;
	
	// private
	bool displaying;
	uint32_t cur_block;
};

vsf_err_t vsfui_fb_init(struct vsfui_fb_t *vsfui_fb);
vsf_err_t vsfui_fb_fini(struct vsfui_fb_t *vsfui_fb);
void* vsfui_fb_get_buffer(struct vsfui_fb_t *vsfui_fb);
vsf_err_t vsfui_fb_validate_buffer(struct vsfui_fb_t *vsfui_fb);
vsf_err_t vsfui_fb_poll(struct vsfui_fb_t *vsfui_fb);

#endif	// __VSFUI_FB_H_INCLUDED__