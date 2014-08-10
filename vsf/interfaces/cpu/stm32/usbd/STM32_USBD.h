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

extern const uint8_t stm32_usbd_ep_num;
extern struct interface_usbd_callback_t stm32_usbd_callback;

vsf_err_t stm32_usbd_init(void);
vsf_err_t stm32_usbd_fini(void);
vsf_err_t stm32_usbd_reset(void);
vsf_err_t stm32_usbd_poll(void);
vsf_err_t stm32_usbd_connect(void);
vsf_err_t stm32_usbd_disconnect(void);
vsf_err_t stm32_usbd_set_address(uint8_t address);
uint8_t stm32_usbd_get_address(void);
vsf_err_t stm32_usbd_suspend(void);
vsf_err_t stm32_usbd_resume(void);
vsf_err_t stm32_usbd_lowpower(uint8_t level);
uint32_t stm32_usbd_get_frame_number(void);
vsf_err_t stm32_usbd_get_setup(uint8_t *buffer);

vsf_err_t stm32_usbd_ep_reset(uint8_t idx);
vsf_err_t stm32_usbd_ep_set_type(uint8_t idx,
									enum interface_usbd_eptype_t type);

vsf_err_t stm32_usbd_ep_set_IN_dbuffer(uint8_t idx);
bool stm32_usbd_ep_is_IN_dbuffer(uint8_t idx);
vsf_err_t stm32_usbd_ep_switch_IN_buffer(uint8_t idx);
vsf_err_t stm32_usbd_ep_set_IN_epsize(uint8_t idx, uint16_t epsize);
uint16_t stm32_usbd_ep_get_IN_epsize(uint8_t idx);
vsf_err_t stm32_usbd_ep_set_IN_stall(uint8_t idx);
vsf_err_t stm32_usbd_ep_clear_IN_stall(uint8_t idx);
bool stm32_usbd_ep_is_IN_stall(uint8_t idx);
vsf_err_t stm32_usbd_ep_reset_IN_toggle(uint8_t idx);
vsf_err_t stm32_usbd_ep_toggle_IN_toggle(uint8_t idx);
enum usb_ep_state_t stm32_usbd_ep_get_IN_state(uint8_t idx);
vsf_err_t stm32_usbd_ep_set_IN_count(uint8_t idx, uint16_t size);
vsf_err_t stm32_usbd_ep_write_IN_buffer(uint8_t idx, uint8_t *buffer,
										uint16_t size);

vsf_err_t stm32_usbd_ep_set_OUT_dbuffer(uint8_t idx);
bool stm32_usbd_ep_is_OUT_dbuffer(uint8_t idx);
vsf_err_t stm32_usbd_ep_switch_OUT_buffer(uint8_t idx);
vsf_err_t stm32_usbd_ep_set_OUT_epsize(uint8_t idx, uint16_t epsize);
uint16_t stm32_usbd_ep_get_OUT_epsize(uint8_t idx);
vsf_err_t stm32_usbd_ep_set_OUT_stall(uint8_t idx);
vsf_err_t stm32_usbd_ep_clear_OUT_stall(uint8_t idx);
bool stm32_usbd_ep_is_OUT_stall(uint8_t idx);
vsf_err_t stm32_usbd_ep_reset_OUT_toggle(uint8_t idx);
vsf_err_t stm32_usbd_ep_toggle_OUT_toggle(uint8_t idx);
vsf_err_t stm32_usbd_ep_set_OUT_state(uint8_t idx, enum usb_ep_state_t state);
enum usb_ep_state_t stm32_usbd_ep_get_OUT_state(uint8_t idx);
uint16_t stm32_usbd_ep_get_OUT_count(uint8_t idx);
vsf_err_t stm32_usbd_ep_read_OUT_buffer(uint8_t idx, uint8_t *buffer,
										uint16_t size);
vsf_err_t stm32_usbd_ep_enable_OUT(uint8_t idx);

