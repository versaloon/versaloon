/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USART.h                                                   *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    USART interface header file                               *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

vsf_err_t usart_init(uint8_t index);
vsf_err_t usart_fini(uint8_t index);
vsf_err_t usart_config(uint8_t index, uint32_t baudrate, uint8_t datalength, 
						uint8_t mode);
vsf_err_t usart_send(uint8_t index, uint8_t *buf, uint16_t len);
vsf_err_t usart_receive(uint8_t index, uint8_t *buf, uint16_t len);
vsf_err_t usart_status(uint8_t index, struct usart_status_t *status);
vsf_err_t usart_poll(uint8_t index);
