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
#ifndef __APP_IO_H_INCLUDED__
#define __APP_IO_H_INCLUDED__

#include <stdio.h>
#include "app_type.h"

extern struct vss_cmd_list_t appio_cmd_list;

void APP_IO_INIT(void);
void APP_IO_FINI(void);

FILE *FOPEN(const char *filename, const char *mode);
int FCLOSE(FILE *f);
int FEOF(FILE *f);
void REWIND(FILE *f);
int FFLUSH(FILE *f);

int FGETC(FILE *f);
int GETCHAR(void);
char* FGETS(char *buf, int count, FILE *f);

int FPRINTF(FILE *f, const char *format, ...);
int PRINTF(const char *format, ...);
#define SNPRINTF						snprintf

#endif	// __APP_IO_H_INCLUDED__

