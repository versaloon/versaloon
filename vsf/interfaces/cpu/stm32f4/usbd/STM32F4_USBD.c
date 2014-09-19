/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       BDM.c                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    BDM interface implementation file                         *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2011-05-09:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_type.h"
#include "compiler.h"
#include "interfaces.h"

#if IFS_USBD_EN

#include "STM32F4_USBD.h"

#define STM32F4_USBD_EP_NUM					8

const uint8_t stm32f4_usbd_ep_num = STM32F4_USBD_EP_NUM;
struct interface_usbd_callback_t stm32f4_usbd_callback;

vsf_err_t stm32f4_usbd_init(uint32_t int_priority)
{
	return VSFERR_NONE;
}

vsf_err_t stm32f4_usbd_fini(void)
{
	return VSFERR_NONE;
}

vsf_err_t stm32f4_usbd_reset(void)
{
	return VSFERR_NONE;
}

vsf_err_t stm32f4_usbd_poll(void)
{
	return VSFERR_NONE;
}

vsf_err_t stm32f4_usbd_connect(void)
{
	return VSFERR_NONE;
}

vsf_err_t stm32f4_usbd_disconnect(void)
{
	return VSFERR_NONE;
}

vsf_err_t stm32f4_usbd_set_address(uint8_t address)
{
	return VSFERR_NONE;
}

uint8_t stm32f4_usbd_get_address(void)
{
	return 0;
}

vsf_err_t stm32f4_usbd_suspend(void)
{
	return VSFERR_NONE;
}

vsf_err_t stm32f4_usbd_resume(void)
{
	return VSFERR_NONE;
}

vsf_err_t stm32f4_usbd_lowpower(uint8_t level)
{
	return VSFERR_NONE;
}

uint32_t stm32f4_usbd_get_frame_number(void)
{
	return 0;
}

vsf_err_t stm32f4_usbd_get_setup(uint8_t *buffer)
{
	return VSFERR_NONE;
}

vsf_err_t stm32f4_usbd_prepare_buffer(void)
{
	return VSFERR_NONE;
}

vsf_err_t stm32f4_usbd_ep_reset(uint8_t idx)
{
	return VSFERR_NONE;
}

vsf_err_t stm32f4_usbd_ep_set_type(uint8_t idx, enum interface_usbd_eptype_t type)
{
	return VSFERR_NONE;
}

vsf_err_t stm32f4_usbd_ep_set_IN_dbuffer(uint8_t idx)
{
	return VSFERR_NONE;
}

bool stm32f4_usbd_ep_is_IN_dbuffer(uint8_t idx)
{
	return false;
}

vsf_err_t stm32f4_usbd_ep_switch_IN_buffer(uint8_t idx)
{
	return VSFERR_NONE;
}

vsf_err_t stm32f4_usbd_ep_set_IN_epsize(uint8_t idx, uint16_t epsize)
{
	return VSFERR_NONE;
}

uint16_t stm32f4_usbd_ep_get_IN_epsize(uint8_t idx)
{
	return 0;
}

vsf_err_t stm32f4_usbd_ep_set_IN_stall(uint8_t idx)
{
	return VSFERR_NONE;
}

vsf_err_t stm32f4_usbd_ep_clear_IN_stall(uint8_t idx)
{
	return VSFERR_NONE;
}

bool stm32f4_usbd_ep_is_IN_stall(uint8_t idx)
{
	return false;
}

vsf_err_t stm32f4_usbd_ep_reset_IN_toggle(uint8_t idx)
{
	return VSFERR_NONE;
}

vsf_err_t stm32f4_usbd_ep_toggle_IN_toggle(uint8_t idx)
{
	return VSFERR_NONE;
}


vsf_err_t stm32f4_usbd_ep_set_IN_count(uint8_t idx, uint16_t size)
{
	return VSFERR_NONE;
}

vsf_err_t stm32f4_usbd_ep_write_IN_buffer(uint8_t idx, uint8_t *buffer,
										uint16_t size)
{
	return VSFERR_NONE;
}

vsf_err_t stm32f4_usbd_ep_set_OUT_dbuffer(uint8_t idx)
{
	return VSFERR_NONE;
}

bool stm32f4_usbd_ep_is_OUT_dbuffer(uint8_t idx)
{
	return false;
}

vsf_err_t stm32f4_usbd_ep_switch_OUT_buffer(uint8_t idx)
{
	return VSFERR_NONE;
}

vsf_err_t stm32f4_usbd_ep_set_OUT_epsize(uint8_t idx, uint16_t epsize)
{
	return VSFERR_NONE;
}

uint16_t stm32f4_usbd_ep_get_OUT_epsize(uint8_t idx)
{
	return 0;
}

vsf_err_t stm32f4_usbd_ep_set_OUT_stall(uint8_t idx)
{
	return VSFERR_NONE;
}

vsf_err_t stm32f4_usbd_ep_clear_OUT_stall(uint8_t idx)
{
	return VSFERR_NONE;
}

bool stm32f4_usbd_ep_is_OUT_stall(uint8_t idx)
{
	return false;
}

vsf_err_t stm32f4_usbd_ep_reset_OUT_toggle(uint8_t idx)
{
	return VSFERR_NONE;
}

vsf_err_t stm32f4_usbd_ep_toggle_OUT_toggle(uint8_t idx)
{
	return VSFERR_NONE;
}

uint16_t stm32f4_usbd_ep_get_OUT_count(uint8_t idx)
{
	return 0;
}

vsf_err_t stm32f4_usbd_ep_read_OUT_buffer(uint8_t idx, uint8_t *buffer,
										uint16_t size)
{
	return VSFERR_NONE;
}

vsf_err_t stm32f4_usbd_ep_enable_OUT(uint8_t idx)
{
	return VSFERR_NONE;
}

#endif
