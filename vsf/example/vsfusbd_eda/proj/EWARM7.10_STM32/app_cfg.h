/**************************************************************************
 *  Copyright (C) 2008 - 2012 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    VSF                                                       *
 *  File:       app_cfg.h                                                 *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    configuration file                                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

// hardware config file
#include "hw_cfg_STM32.h"

// compiler config
#include "compiler.h"

#define APP_CFG_MSC_WRITEONLY				0
#define APP_CFG_BOOTSIZE					(32 * 1024)
#define APP_CFG_FWSIZE						(224 * 1024)

// APP config
#define EVSPROG_EN							0
#if EVSPROG_EN
#	define EVSPROG_DATA_ADDR				(APP_CFG_BOOTSIZE + APP_CFG_FWSIZE)
#	define EVSPROG_TARGET_CFG_ADDR			EVSPROG_DATA_ADDR
#	define EVSPROG_TARGET_CFG_SIZE			(240 * 1024)
#	define EVSPROG_MAINSCRIPT_ADDR			(EVSPROG_TARGET_CFG_ADDR + EVSPROG_TARGET_CFG_SIZE)
#	define EVSPROG_MAINSCRIPT_SIZE			(16 * 1024)
#	define EVSPROG_TARGETSCRIPT_SIZE		(16 * 1024)
#endif
