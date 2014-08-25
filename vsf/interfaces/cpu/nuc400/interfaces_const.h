/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       interfaces_const.h                                        *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    consts of interface module                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2011-04-08:     created(by SimonQian)                             *
 **************************************************************************/
#ifndef __NUC400_INTERFACE_CONST_H_INCLUDED__
#define __NUC400_INTERFACE_CONST_H_INCLUDED__

#define nuc400_SLEEP_WFI			0
#define nuc400_SLEEP_PWRDOWN		1

#define nuc400_USART_MODE0			
#define nuc400_USART_MODE1			
#define nuc400_USART_MODE2			
#define nuc400_USART_MODE3			
#define nuc400_USART_CLKEN			
#define nuc400_USART_STOPBITS_0P5	
#define nuc400_USART_STOPBITS_1		
#define nuc400_USART_STOPBITS_1P5	
#define nuc400_USART_STOPBITS_2		
#define nuc400_USART_PARITY_NONE	
#define nuc400_USART_PARITY_ODD		
#define nuc400_USART_PARITY_EVEN	

#define nuc400_SPI_MASTER			
#define nuc400_SPI_SLAVE			
#define nuc400_SPI_MODE0			
#define nuc400_SPI_MODE1			
#define nuc400_SPI_MODE2			
#define nuc400_SPI_MODE3			
#define nuc400_SPI_MSB_FIRST		
#define nuc400_SPI_LSB_FIRST		

#define nuc400_ADC_ALIGNLEFT		
#define nuc400_ADC_ALIGNRIGHT		

#define nuc400_GPIO_INFLOAT			0x00
#define nuc400_GPIO_INPU			0x00
#define nuc400_GPIO_INPD			0x00
#define nuc400_GPIO_OUTPP			0x01
#define nuc400_GPIO_OUTOD			0x02

#define nuc400_EINT_ONFALL			
#define nuc400_EINT_ONRISE			
#define nuc400_EINT_INT				
#define nuc400_EINT_EVT				

#define nuc400_SDIO_RESP_NONE		
#define nuc400_SDIO_RESP_SHORT		
#define nuc400_SDIO_RESP_LONG		

#endif	// __NUC400_INTERFACE_CONST_H_INCLUDED__
