/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       CommandProcesor.c                                         *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    CommandProcesor for Versaloon/USB_TO_XXX                  *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-08-30:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"

#include "app_interfaces.h"
#include "CommandProcessor.h"

#if USB_TO_XXX_EN
#	include "USB_TO_XXX.h"
#endif

#if STLINK_EN
#	include "stlink.h"
#endif

const uint8_t Versaloon_Ver[] = "Versaloon(" _HARDWARE_VER_STR ")by Simon(compiled on " __DATE__ ")";

static void Versaloon_ProcessCommonCmd(uint8_t *dat, uint16_t len)
{
	uint16_t voltage;
	
	switch(dat[0])
	{
	case VERSALOON_GET_INFO:
		SET_LE_U16(&dat[0], USB_DATA_BUFF_SIZE);
		memcpy(dat + 2, Versaloon_Ver, sizeof(Versaloon_Ver));
		rep_len = sizeof(Versaloon_Ver) + 2;
		break;
	case VERSALOON_GET_TVCC:
#if POWER_OUT_EN
		app_interfaces.target_voltage.get(0, &voltage);
#else
		voltage = 0;
#endif
		SET_LE_U16(&dat[0], voltage);
		rep_len = 2;
		break;
	case VERSALOON_GET_HARDWARE:
		dat[0] = _HARDWARE_VER;
		rep_len = 1;
		break;
	case VERSALOON_FW_UPDATE:
		// TODO: Write notifier to bootlaoder and then reset
		core_interfaces.core.reset(NULL);
		break;
	}
}

void ProcessCommand(uint8_t* dat, uint16_t len)
{
	uint8_t cmd = 0;

	// first byte of the USB package is the command byte
	cmd = dat[0];
	// check command and call corresponding module
	if ((cmd >= VERSALOON_COMMON_CMD_START) &&
		(cmd <= VERSALOON_COMMON_CMD_END))
	{
		// Common Commands
		Versaloon_ProcessCommonCmd(dat, len);
		if (rep_len)
		{
			rep_len |= VERSALOON_REP_ZLP;
		}
	}
#if USB_TO_XXX_EN
	else if ((cmd >= VERSALOON_USB_TO_XXX_CMD_START) &&
			(cmd <= VERSALOON_USB_TO_XXX_CMD_END))
	{
		buffer_reply = dat;
#if USB_TO_POLL_EN
		USB_TO_POLL_Index = -1;
#endif
		USB_TO_XXX_ProcessCmd(dat, len);
		if (rep_len)
		{
			rep_len |= VERSALOON_REP_ZLP;
		}
	}
#endif		// #if USB_TO_XXX_EN
#if STLINK_EN
	else if ((cmd >= STLINK_CMD_START) &&
			(cmd <= STLINK_CMD_END))
	{
		rep_len = stlink_process(dat, len);
	}
#endif		// #if STLINK_EN
}
