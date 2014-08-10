/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       main.c                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    main.c file                                               *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

/* Includes ------------------------------------------------------------------*/
#include "app_cfg.h"
#include "interfaces.h"

#include "usb_protocol.h"

#if SCRIPTS_EN
#	include "app_io.h"
#	include "app_log.h"
#	include "scripts.h"
#	include "vsprog.h"
#	include "target_data.h"
#endif

/* Private typedef -----------------------------------------------------------*/
/* Private define ------------------------------------------------------------*/
/* Private macro -------------------------------------------------------------*/
/* Private variables ---------------------------------------------------------*/
/* Private function prototypes -----------------------------------------------*/
/* Private functions ---------------------------------------------------------*/
int main(void)
{
#if SCRIPTS_EN
	FILE *script_file = NULL;
#endif
	
	core_interfaces.core.init(NULL);
	interfaces->tickclk.init();
	interfaces->tickclk.start();
	usb_protocol_init();
	
#if SCRIPTS_EN
run:
	KEY_Init();
#endif
	while (1
#if SCRIPTS_EN
		&& !KEY_IsDown()
#endif
		)
	{
		usb_protocol_poll();
	}
	
#if SCRIPTS_EN
	target_init_slots();
	APP_IO_INIT();
	vss_init();
	vss_register_cmd_list(&appio_cmd_list);
	vss_register_cmd_list(&vsprog_cmd_list);
	
	script_file = FOPEN(EVSPROG_SCRIPT_FILE, "rt");
	if (script_file != NULL)
	{
		FCLOSE(script_file);
		script_file = NULL;
		if (!usb_device.configured)
		{
			vss_run_script("dummy 1");
		}
		vss_run_script("run " EVSPROG_SCRIPT_FILE);
	}
	else
	{
		vss_run_script("shell");
	}
	APP_IO_FINI();
	vss_fini();
	goto run;
#endif	// if SCRIPTS_EN
}

/******************* (C) COPYRIGHT 2007 STMicroelectronics *****END OF FILE****/
