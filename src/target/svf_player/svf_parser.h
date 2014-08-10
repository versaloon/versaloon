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
#ifndef __SVF_PARSER_H_INCLUDED__
#define __SVF_PARSER_H_INCLUDED__

#define SVF_PARSER_DATA_BUFFER_SIZE		(10 * 1024)

void svf_parser_free_xxr_para(struct svf_xxr_para_t *para);
void svf_parser_init(void);
void svf_parser_fini(void);

vsf_err_t svf_parser_get_command(FILE *file, char **cmd_buffer,
									uint32_t *cmd_len);
vsf_err_t svf_parser_run_command(char *cmd_str);
vsf_err_t svf_parser_check_tdo(void);

extern uint32_t svf_file_index;
extern uint32_t svf_line_number;

#endif /* __SVF_PARSER_H_INCLUDED__ */

