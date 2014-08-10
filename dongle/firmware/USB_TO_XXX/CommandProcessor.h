/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       CommandProcesor.h                                         *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    header of CommandProcesor for Versaloon/USB_TO_XXX        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-08-30:     created(by SimonQian)                             *
 **************************************************************************/

// Common Commands
#define VERSALOON_COMMON_CMD_START		0x00
#define VERSALOON_COMMON_CMD_END		0x0F

#define VERSALOON_GET_INFO				0x00
#define VERSALOON_GET_TVCC				0x01
#define VERSALOON_GET_HARDWARE			0x02
#define VERSALOON_FW_UPDATE				0x0F

// USB_TO_XXX Commands
#define VERSALOON_USB_TO_XXX_CMD_START	0x20
#define VERSALOON_USB_TO_XXX_CMD_END	0x7F

// REPLY
#define VERSALOON_REP_ZLP				0x00010000

void ProcessCommand(uint8_t *dat, uint16_t len);
