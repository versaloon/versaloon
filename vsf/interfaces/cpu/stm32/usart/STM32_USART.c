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

#include "app_type.h"
#include "compiler.h"
#include "interfaces.h"

// TODO: remove MACROs below to stm32_reg.h
#define STM32_AFIO_MAPR_USART1			((uint32_t)1 << 2)
#define STM32_AFIO_MAPR_USART2			((uint32_t)1 << 3)
#define STM32_AFIO_MAPR_USART3_SFT		((uint32_t)1 << 4)
#define STM32_AFIO_MAPR_USART3_MSK		((uint32_t)0X03 << \
											STM32_AFIO_MAPR_USART3_SFT)
#define STM32_AFIO_MAPR_SWJCFG			((uint32_t)7 << 24)

#define STM32_RCC_APB1ENR_USART2EN		((uint32_t)1 << 17)
#define STM32_RCC_APB1ENR_USART3EN		((uint32_t)1 << 18)
#define STM32_RCC_APB1ENR_USART4EN		((uint32_t)1 << 19)
#define STM32_RCC_APB1ENR_USART5EN		((uint32_t)1 << 20)
#define STM32_RCC_APB2ENR_USART1EN		((uint32_t)1 << 14)
#define STM32_RCC_APB2ENR_IOPAEN		((uint32_t)1 << 2)
#define STM32_RCC_APB2ENR_IOPBEN		((uint32_t)1 << 3)
#define STM32_RCC_APB2ENR_IOPCEN		((uint32_t)1 << 4)
#define STM32_RCC_APB2ENR_IOPDEN		((uint32_t)1 << 5)

#define STM32_USART_SR_TXE				((uint32_t)1 << 7)
#define STM32_USART_SR_TC				((uint32_t)1 << 6)
#define STM32_USART_SR_RXNE				((uint32_t)1 << 5)

#define STM32_USART_CR1_UE				((uint32_t)1 << 13)
#define STM32_USART_CR1_M				((uint32_t)1 << 12)
#define STM32_USART_CR1_TXEIE			((uint32_t)1 << 7)
#define STM32_USART_CR1_RXNEIE			((uint32_t)1 << 5)
#define STM32_USART_CR1_TE				((uint32_t)1 << 3)
#define STM32_USART_CR1_RE				((uint32_t)1 << 2)

#define STM32_USART_CR2_CLKEN			((uint32_t)1 << 11)
#define STM32_USART_CR2_STOP_SFT		12
#define STM32_USART_CR2_STOP_MSK		((uint32_t)0x03 << \
											STM32_USART_CR2_STOP_SFT)

#define STM32_USART_CR3_CTSE			((uint32_t)1 << 9)
#define STM32_USART_CR3_RTSE			((uint32_t)1 << 8)
#define STM32_USART_CR3_HDSEL			((uint32_t)1 << 3)

#if IFS_USART_EN

#include "STM32_USART.h"

static void (*stm32_usart_ontx[USART_NUM])(void *);
static void (*stm32_usart_onrx[USART_NUM])(void *, uint16_t data);
static void *stm32_usart_callback_param[USART_NUM];
static const uint8_t stm32_usart_irqn[USART_NUM] = 
{
#if USART_NUM >= 1
	USART1_IRQn, 
#endif
#if USART_NUM >= 2
	USART2_IRQn, 
#endif
#if USART_NUM >= 3
	USART3_IRQn, 
#endif
#if USART_NUM >= 4
	USART4_IRQn, 
#endif
#if USART_NUM >= 5
	USART5_IRQn
#endif
};
static const USART_TypeDef *stm32_usarts[USART_NUM] = 
{
#if USART_NUM >= 1
	USART1, 
#endif
#if USART_NUM >= 2
	USART2, 
#endif
#if USART_NUM >= 3
	USART3, 
#endif
#if USART_NUM >= 4
	USART4, 
#endif
#if USART_NUM >= 5
	USART5
#endif
};

vsf_err_t stm32_usart_init(uint8_t index)
{
	uint8_t usart_idx = index & 0x0F;
	uint8_t remap_idx = (index >> 4) & 0x0F;
	
#if __VSF_DEBUG__
	if (usart_idx >= USART_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	switch (usart_idx)
	{
	#if USART00_ENABLE || USART10_ENABLE
	case 0:
		RCC->APB2ENR |= STM32_RCC_APB2ENR_USART1EN;
		switch (remap_idx)
		{
		#if USART00_ENABLE
		case 0:
			AFIO->MAPR = (AFIO->MAPR & ~STM32_AFIO_MAPR_USART1)
							| STM32_AFIO_MAPR_SWJCFG;
			RCC->APB2ENR |= STM32_RCC_APB2ENR_IOPAEN;
			#if USART00_CTS_ENABLE
			GPIOA->CRH = (GPIOA->CRH & ~(0x0F << ((11 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((11 - 8) * 4));
			#endif
			#if USART00_RTS_ENABLE
			GPIOA->CRH = (GPIOA->CRH & ~(0x0F << ((12 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_AFPP << ((12 - 8) * 4));
			#endif
			#if USART00_TX_ENABLE
			GPIOA->CRH = (GPIOA->CRH & ~(0x0F << ((9 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_AFPP << ((9 - 8) * 4));
			#endif
			#if USART00_RX_ENABLE
			GPIOA->CRH = (GPIOA->CRH & ~(0x0F << ((10 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((10 - 8) * 4));
			#endif
			#if USART00_CK_ENABLE
			GPIOA->CRH = (GPIOA->CRH & ~(0x0F << ((8 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_AFPP << ((8 - 8) * 4));
			#endif
			break;
		#endif
		#if USART10_ENABLE
		case 1:
			AFIO->MAPR |= STM32_AFIO_MAPR_USART1 | STM32_AFIO_MAPR_SWJCFG;
			RCC->APB2ENR |= STM32_RCC_APB2ENR_IOPBEN;
			#if USART10_TX_ENABLE
			GPIOB->CRL = (GPIOB->CRL & ~(0x0F << (6 * 4))) | 
							((uint32_t)stm32_GPIO_AFPP << (6 * 4));
			#endif
			#if USART10_RX_ENABLE
			GPIOB->CRL = (GPIOB->CRL & ~(0x0F << (7 * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << (7 * 4));
			#endif
			break;
		#endif
		default:
			return VSFERR_NOT_SUPPORT;
		}
		break;
	#endif
	#if USART01_ENABLE || USART11_ENABLE
	case 1:
		RCC->APB1ENR |= STM32_RCC_APB1ENR_USART2EN;
		switch (remap_idx)
		{
		#if USART01_ENABLE
		case 0:
			AFIO->MAPR = (AFIO->MAPR & ~STM32_AFIO_MAPR_USART2)
							| STM32_AFIO_MAPR_SWJCFG;
			RCC->APB2ENR |= STM32_RCC_APB2ENR_IOPAEN;
			#if USART00_CTS_ENABLE
			GPIOA->CRL = (GPIOA->CRL & ~(0x0F << (0 * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << (0 * 4));
			#endif
			#if USART00_RTS_ENABLE
			GPIOA->CRL = (GPIOA->CRL & ~(0x0F << (1 * 4))) | 
							((uint32_t)stm32_GPIO_AFPP << (1 * 4));
			#endif
			#if USART00_TX_ENABLE
			GPIOA->CRL = (GPIOA->CRL & ~(0x0F << (2 * 4))) | 
							((uint32_t)stm32_GPIO_AFPP << (2 * 4));
			#endif
			#if USART00_RX_ENABLE
			GPIOA->CRL = (GPIOA->CRL & ~(0x0F << (3 * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << (3 * 4));
			#endif
			#if USART00_CK_ENABLE
			GPIOA->CRL = (GPIOA->CRL & ~(0x0F << (4 * 4))) | 
							((uint32_t)stm32_GPIO_AFPP << (4 * 4));
			#endif
			break;
		#endif
		#if USART11_ENABLE
		case 1:
			AFIO->MAPR |= STM32_AFIO_MAPR_USART2 | STM32_AFIO_MAPR_SWJCFG;
			RCC->APB2ENR |= STM32_RCC_APB2ENR_IOPDEN;
			#if USART11_CTS_ENABLE
			GPIOD->CRL = (GPIOD->CRL & ~(0x0F << (3 * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << (3 * 4));
			#endif
			#if USART11_RTS_ENABLE
			GPIOD->CRL = (GPIOD->CRL & ~(0x0F << (4 * 4))) | 
							((uint32_t)stm32_GPIO_AFPP << (4 * 4));
			#endif
			#if USART11_TX_ENABLE
			GPIOD->CRL = (GPIOD->CRL & ~(0x0F << (5 * 4))) | 
							((uint32_t)stm32_GPIO_AFPP << (5 * 4));
			#endif
			#if USART11_RX_ENABLE
			GPIOD->CRL = (GPIOD->CRL & ~(0x0F << (6 * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << (6 * 4));
			#endif
			#if USART11_CK_ENABLE
			GPIOD->CRL = (GPIOD->CRL & ~(0x0F << (7 * 4))) | 
							((uint32_t)stm32_GPIO_AFPP << (7 * 4));
			#endif
			break;
		#endif
		default:
			return VSFERR_NOT_SUPPORT;
		}
		break;
	#endif
	#if USART02_ENABLE || USART12_ENABLE || USART32_ENABLE
	case 2:
		RCC->APB1ENR |= STM32_RCC_APB1ENR_USART3EN;
		switch (remap_idx)
		{
		#if USART02_ENABLE
		case 0:
			AFIO->MAPR = (AFIO->MAPR & ~STM32_AFIO_MAPR_USART3_MSK)
							| STM32_AFIO_MAPR_SWJCFG;
			RCC->APB2ENR |= STM32_RCC_APB2ENR_IOPBEN;
			#if USART02_CTS_ENABLE
			GPIOB->CRH = (GPIOB->CRH & ~(0x0F << ((13 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((13 - 8) * 4));
			#endif
			#if USART02_RTS_ENABLE
			GPIOB->CRH = (GPIOB->CRH & ~(0x0F << ((14 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_AFPP << ((14 - 8) * 4));
			#endif
			#if USART02_TX_ENABLE
			GPIOB->CRH = (GPIOB->CRH & ~(0x0F << ((10 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_AFPP << ((10 - 8) * 4));
			#endif
			#if USART02_RX_ENABLE
			GPIOB->CRH = (GPIOB->CRH & ~(0x0F << ((11 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((11 - 8) * 4));
			#endif
			#if USART02_CK_ENABLE
			GPIOB->CRH = (GPIOB->CRH & ~(0x0F << ((12 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_AFPP << ((12 - 8) * 4));
			#endif
			break;
		#endif
		#if USART12_ENABLE
		case 1:
			AFIO->MAPR = (AFIO->MAPR & ~STM32_AFIO_MAPR_USART3_MSK) | 
							(remap_idx << STM32_AFIO_MAPR_USART3_SFT) | 
							STM32_AFIO_MAPR_SWJCFG;
			RCC->APB2ENR |= STM32_RCC_APB2ENR_IOPCEN;
			#if USART12_CTS_ENABLE
			GPIOC->CRH = (GPIOC->CRH & ~(0x0F << ((13 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((13 - 8) * 4));
			#endif
			#if USART12_RTS_ENABLE
			GPIOC->CRH = (GPIOC->CRH & ~(0x0F << ((14 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_AFPP << ((14 - 8) * 4));
			#endif
			#if USART12_TX_ENABLE
			GPIOC->CRH = (GPIOC->CRH & ~(0x0F << ((10 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_AFPP << ((10 - 8) * 4));
			#endif
			#if USART12_RX_ENABLE
			GPIOC->CRH = (GPIOC->CRH & ~(0x0F << ((11 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((11 - 8) * 4));
			#endif
			#if USART12_CK_ENABLE
			GPIOC->CRH = (GPIOC->CRH & ~(0x0F << ((12 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_AFPP << ((12 - 8) * 4));
			#endif
			break;
		#endif
		#if USART32_ENABLE
		case 2:
			AFIO->MAPR = (AFIO->MAPR & ~STM32_AFIO_MAPR_USART3_MSK) | 
							(remap_idx << STM32_AFIO_MAPR_USART3_SFT) | 
							STM32_AFIO_MAPR_SWJCFG;
			RCC->APB2ENR |= STM32_RCC_APB2ENR_IOPDEN;
			#if USART32_CTS_ENABLE
			GPIOD->CRH = (GPIOD->CRH & ~(0x0F << ((11 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((13 - 8) * 4));
			#endif
			#if USART32_RTS_ENABLE
			GPIOD->CRH = (GPIOD->CRH & ~(0x0F << ((12 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_AFPP << ((14 - 8) * 4));
			#endif
			#if USART32_TX_ENABLE
			GPIOD->CRH = (GPIOD->CRH & ~(0x0F << ((8 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_AFPP << ((10 - 8) * 4));
			#endif
			#if USART32_RX_ENABLE
			GPIOD->CRH = (GPIOD->CRH & ~(0x0F << ((9 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((11 - 8) * 4));
			#endif
			#if USART32_CK_ENABLE
			GPIOD->CRH = (GPIOD->CRH & ~(0x0F << ((10 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_AFPP << ((12 - 8) * 4));
			#endif
			break;
		#endif
		default:
			return VSFERR_NOT_SUPPORT;
		}
		break;
	#endif
	#if USART03_ENABLE
	case 3:
		RCC->APB1ENR |= STM32_RCC_APB1ENR_USART4EN;
		break;
	#endif
	#if USART04_ENABLE
	case 4:
		RCC->APB1ENR |= STM32_RCC_APB1ENR_USART5EN;
		break;
	#endif
	}
	return VSFERR_NONE;
}

vsf_err_t stm32_usart_fini(uint8_t index)
{
	USART_TypeDef *usart;
	uint8_t usart_idx = index & 0x0F;
	uint8_t remap_idx = (index >> 4) & 0x0F;
	
#if __VSF_DEBUG__
	if (usart_idx >= USART_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	usart = (USART_TypeDef *)stm32_usarts[usart_idx];
	
	usart->CR1 = 0;
	switch (usart_idx)
	{
	#if USART00_ENABLE || USART10_ENABLE
	case 0:
		RCC->APB2ENR &= ~STM32_RCC_APB2ENR_USART1EN;
		switch (remap_idx)
		{
		#if USART00_ENABLE
		case 0:
			AFIO->MAPR = (AFIO->MAPR & ~STM32_AFIO_MAPR_USART1)
							| STM32_AFIO_MAPR_SWJCFG;
			#if USART00_CTS_ENABLE
			GPIOA->CRH = (GPIOA->CRH & ~(0x0F << ((11 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((13 - 8) * 4));
			#endif
			#if USART00_RTS_ENABLE
			GPIOA->CRH = (GPIOA->CRH & ~(0x0F << ((12 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((14 - 8) * 4));
			#endif
			#if USART00_TX_ENABLE
			GPIOA->CRH = (GPIOA->CRH & ~(0x0F << ((9 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((10 - 8) * 4));
			#endif
			#if USART00_RX_ENABLE
			GPIOA->CRH = (GPIOA->CRH & ~(0x0F << ((10 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((11 - 8) * 4));
			#endif
			#if USART00_CK_ENABLE
			GPIOA->CRH = (GPIOA->CRH & ~(0x0F << ((8 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((12 - 8) * 4));
			#endif
			break;
		#endif
		#if USART10_ENABLE
		case 1:
			AFIO->MAPR = (AFIO->MAPR & ~STM32_AFIO_MAPR_USART1)
							| STM32_AFIO_MAPR_SWJCFG;
			#if USART10_TX_ENABLE
			GPIOB->CRL = (GPIOB->CRL & ~(0x0F << (6 * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << (6 * 4));
			#endif
			#if USART10_RX_ENABLE
			GPIOB->CRL = (GPIOB->CRL & ~(0x0F << (7 * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << (7 * 4));
			#endif
			break;
		#endif
		default:
			return VSFERR_NOT_SUPPORT;
		}
		break;
	#endif
	#if USART01_ENABLE || USART11_ENABLE
	case 1:
		RCC->APB1ENR &= ~STM32_RCC_APB1ENR_USART2EN;
		switch (remap_idx)
		{
		#if USART01_ENABLE
		case 0:
			AFIO->MAPR = (AFIO->MAPR & ~STM32_AFIO_MAPR_USART2)
							| STM32_AFIO_MAPR_SWJCFG;
			#if USART00_CTS_ENABLE
			GPIOA->CRL = (GPIOA->CRL & ~(0x0F << (0 * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << (0 * 4));
			#endif
			#if USART00_RTS_ENABLE
			GPIOA->CRL = (GPIOA->CRL & ~(0x0F << (1 * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << (1 * 4));
			#endif
			#if USART00_TX_ENABLE
			GPIOA->CRL = (GPIOA->CRL & ~(0x0F << (2 * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << (2 * 4));
			#endif
			#if USART00_RX_ENABLE
			GPIOA->CRL = (GPIOA->CRL & ~(0x0F << (3 * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << (3 * 4));
			#endif
			#if USART00_CK_ENABLE
			GPIOA->CRL = (GPIOA->CRL & ~(0x0F << (4 * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << (4 * 4));
			#endif
			break;
		#endif
		#if USART11_ENABLE
		case 1:
			AFIO->MAPR = (AFIO->MAPR & ~STM32_AFIO_MAPR_USART2)
							| STM32_AFIO_MAPR_SWJCFG;
			#if USART11_CTS_ENABLE
			GPIOD->CRL = (GPIOD->CRL & ~(0x0F << (3 * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << (3 * 4));
			#endif
			#if USART11_RTS_ENABLE
			GPIOD->CRL = (GPIOD->CRL & ~(0x0F << (4 * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << (4 * 4));
			#endif
			#if USART11_TX_ENABLE
			GPIOD->CRL = (GPIOD->CRL & ~(0x0F << (5 * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << (5 * 4));
			#endif
			#if USART11_RX_ENABLE
			GPIOD->CRL = (GPIOD->CRL & ~(0x0F << (6 * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << (6 * 4));
			#endif
			#if USART11_CK_ENABLE
			GPIOD->CRL = (GPIOD->CRL & ~(0x0F << (7 * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << (7 * 4));
			#endif
			break;
		#endif
		default:
			return VSFERR_NOT_SUPPORT;
		}
		break;
	#endif
	#if USART02_ENABLE || USART12_ENABLE || USART32_ENABLE
	case 2:
		RCC->APB1ENR &= ~STM32_RCC_APB1ENR_USART3EN;
		switch (remap_idx)
		{
		#if USART02_ENABLE
		case 0:
			AFIO->MAPR = (AFIO->MAPR & ~STM32_AFIO_MAPR_USART3_MSK)
							| STM32_AFIO_MAPR_SWJCFG;
			#if USART02_CTS_ENABLE
			GPIOB->CRH = (GPIOB->CRH & ~(0x0F << ((13 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((13 - 8) * 4));
			#endif
			#if USART02_RTS_ENABLE
			GPIOB->CRH = (GPIOB->CRH & ~(0x0F << ((14 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((14 - 8) * 4));
			#endif
			#if USART02_TX_ENABLE
			GPIOB->CRH = (GPIOB->CRH & ~(0x0F << ((10 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((10 - 8) * 4));
			#endif
			#if USART02_RX_ENABLE
			GPIOB->CRH = (GPIOB->CRH & ~(0x0F << ((11 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((11 - 8) * 4));
			#endif
			#if USART02_CK_ENABLE
			GPIOB->CRH = (GPIOB->CRH & ~(0x0F << ((12 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((12 - 8) * 4));
			#endif
			break;
		#endif
		#if USART12_ENABLE
		case 1:
			AFIO->MAPR = (AFIO->MAPR & ~STM32_AFIO_MAPR_USART3_MSK) | 
							(remap_idx << STM32_AFIO_MAPR_USART3_SFT) | 
							STM32_AFIO_MAPR_SWJCFG;
			#if USART12_CTS_ENABLE
			GPIOC->CRH = (GPIOC->CRH & ~(0x0F << ((13 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((13 - 8) * 4));
			#endif
			#if USART12_RTS_ENABLE
			GPIOC->CRH = (GPIOC->CRH & ~(0x0F << ((14 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((14 - 8) * 4));
			#endif
			#if USART12_TX_ENABLE
			GPIOC->CRH = (GPIOC->CRH & ~(0x0F << ((10 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((10 - 8) * 4));
			#endif
			#if USART12_RX_ENABLE
			GPIOC->CRH = (GPIOC->CRH & ~(0x0F << ((11 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((11 - 8) * 4));
			#endif
			#if USART12_CK_ENABLE
			GPIOC->CRH = (GPIOC->CRH & ~(0x0F << ((12 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((12 - 8) * 4));
			#endif
			break;
		#endif
		#if USART32_ENABLE
		case 2:
			AFIO->MAPR = (AFIO->MAPR & ~STM32_AFIO_MAPR_USART3_MSK) | 
							(remap_idx << STM32_AFIO_MAPR_USART3_SFT) | 
							STM32_AFIO_MAPR_SWJCFG;
			#if USART32_CTS_ENABLE
			GPIOD->CRH = (GPIOD->CRH & ~(0x0F << ((11 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((13 - 8) * 4));
			#endif
			#if USART32_RTS_ENABLE
			GPIOD->CRH = (GPIOD->CRH & ~(0x0F << ((12 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((14 - 8) * 4));
			#endif
			#if USART32_TX_ENABLE
			GPIOD->CRH = (GPIOD->CRH & ~(0x0F << ((8 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((10 - 8) * 4));
			#endif
			#if USART32_RX_ENABLE
			GPIOD->CRH = (GPIOD->CRH & ~(0x0F << ((9 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((11 - 8) * 4));
			#endif
			#if USART32_CK_ENABLE
			GPIOD->CRH = (GPIOD->CRH & ~(0x0F << ((10 - 8) * 4))) | 
							((uint32_t)stm32_GPIO_INFLOAT << ((12 - 8) * 4));
			#endif
			break;
		#endif
		default:
			return VSFERR_NOT_SUPPORT;
		}
		break;
	#endif
	#if USART03_ENABLE
	case 3:
		RCC->APB1ENR &= ~STM32_RCC_APB1ENR_USART4EN;
		break;
	#endif
	#if USART04_ENABLE
	case 4:
		RCC->APB1ENR &= ~STM32_RCC_APB1ENR_USART5EN;
		break;
	#endif
	}
	return VSFERR_NONE;
}

vsf_err_t stm32_usart_config(uint8_t index, uint32_t baudrate, 
	uint8_t datalength, uint8_t mode)
{
	USART_TypeDef *usart;
	uint8_t usart_idx = index & 0x0F;
	uint32_t cr1 = 0, cr2 = 0, cr3 = 0;
	struct stm32_info_t *info;
	uint32_t module_hz;
	uint32_t mantissa, fraction, divider;
	
#if __VSF_DEBUG__
	if (usart_idx >= USART_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	usart = (USART_TypeDef *)stm32_usarts[usart_idx];
	
	if (stm32_interface_get_info(&info))
	{
		return VSFERR_FAIL;
	}
	
	cr1 = usart->CR1 & (STM32_USART_CR1_TXEIE | STM32_USART_CR1_RXNEIE);
	switch (usart_idx)
	{
	#if USART00_ENABLE || USART10_ENABLE
	case 0:
		module_hz = info->apb2_freq_hz;
		#if (USART00_ENABLE && USART00_TX_ENABLE) || \
			(USART10_ENABLE && USART10_TX_ENABLE)
		cr1 |= STM32_USART_CR1_TE;
		#endif
		#if (USART00_ENABLE && USART00_RX_ENABLE) || \
			(USART10_ENABLE && USART10_RX_ENABLE)
		cr1 |= STM32_USART_CR1_RE;
		#endif
		#if (USART00_ENABLE && USART00_CK_ENABLE) || \
			(USART10_ENABLE && USART10_CK_ENABLE)
		cr2 |= STM32_USART_CR2_CLKEN;
		#endif
		#if (USART00_ENABLE && USART00_CTS_ENABLE) || \
			(USART10_ENABLE && USART10_CTS_ENABLE)
		cr3 |= STM32_USART_CR3_CTSE;
		#endif
		#if (USART00_ENABLE && USART00_RTS_ENABLE) || \
			(USART10_ENABLE && USART10_RTS_ENABLE)
		cr3 |= STM32_USART_CR3_RTSE;
		#endif
		break;
	#endif
	#if USART01_ENABLE || USART11_ENABLE
	case 1:
		module_hz = info->apb1_freq_hz;
		#if (USART01_ENABLE && USART01_TX_ENABLE) || \
			(USART11_ENABLE && USART11_TX_ENABLE)
		cr1 |= STM32_USART_CR1_TE;
		#endif
		#if (USART01_ENABLE && USART01_RX_ENABLE) || \
			(USART11_ENABLE && USART11_RX_ENABLE)
		cr1 |= STM32_USART_CR1_RE;
		#endif
		#if (USART01_ENABLE && USART01_CK_ENABLE) || \
			(USART11_ENABLE && USART11_CK_ENABLE)
		cr2 |= STM32_USART_CR2_CLKEN;
		#endif
		#if (USART01_ENABLE && USART01_CTS_ENABLE) || \
			(USART11_ENABLE && USART11_CTS_ENABLE)
		cr3 |= STM32_USART_CR3_CTSE;
		#endif
		#if (USART01_ENABLE && USART01_RTS_ENABLE) || \
			(USART11_ENABLE && USART11_RTS_ENABLE)
		cr3 |= STM32_USART_CR3_RTSE;
		#endif
		break;
	#endif
	#if USART02_ENABLE || USART12_ENABLE || USART32_ENABLE
	case 2:
		module_hz = info->apb1_freq_hz;
		#if (USART02_ENABLE && USART02_TX_ENABLE) || \
			(USART12_ENABLE && USART12_TX_ENABLE) || \
			(USART32_ENABLE && USART32_TX_ENABLE)
		cr1 |= STM32_USART_CR1_TE;
		#endif
		#if (USART02_ENABLE && USART02_RX_ENABLE) || \
			(USART12_ENABLE && USART12_RX_ENABLE) || \
			(USART32_ENABLE && USART32_RX_ENABLE)
		cr1 |= STM32_USART_CR1_RE;
		#endif
		#if (USART02_ENABLE && USART02_CK_ENABLE) || \
			(USART12_ENABLE && USART12_CK_ENABLE) || \
			(USART32_ENABLE && USART32_CK_ENABLE)
		cr2 |= STM32_USART_CR2_CLKEN;
		#endif
		#if (USART02_ENABLE && USART02_CTS_ENABLE) || \
			(USART12_ENABLE && USART12_CTS_ENABLE) || \
			(USART32_ENABLE && USART32_CTS_ENABLE)
		cr3 |= STM32_USART_CR3_CTSE;
		#endif
		#if (USART02_ENABLE && USART02_RTS_ENABLE) || \
			(USART12_ENABLE && USART12_RTS_ENABLE) || \
			(USART32_ENABLE && USART32_RTS_ENABLE)
		cr3 |= STM32_USART_CR3_RTSE;
		#endif
		break;
	#endif
	#if USART03_ENABLE
	case 3:
		module_hz = info->apb1_freq_hz;
		#if USART03_ENABLE && USART03_TX_ENABLE
		cr1 |= STM32_USART_CR1_TE;
		#endif
		#if USART03_ENABLE && USART03_RX_ENABLE
		cr1 |= STM32_USART_CR1_RE;
		#endif
		break;
	#endif
	#if USART04_ENABLE
	case 4:
		module_hz = info->apb1_freq_hz;
		#if USART04_ENABLE && USART04_TX_ENABLE
		cr1 |= STM32_USART_CR1_TE;
		#endif
		#if USART04_ENABLE && USART04_RX_ENABLE
		cr1 |= STM32_USART_CR1_RE;
		#endif
		break;
	#endif
	default:
		return VSFERR_NOT_SUPPORT;
	}
	
	switch (datalength)
	{
	case 8:
		break;
	case 9:
		cr1 |= STM32_USART_CR1_M;
		break;
	default:
		return VSFERR_INVALID_PARAMETER;
	}
	// mode:
	// bit0 - bit1: parity
	// bit5 - bit6: stopbits
	// bit2 - bit3: mode
	cr1 |= ((uint32_t)mode << 9) & 0x0600;
	cr2 |= ((uint32_t)mode << 7) & 0x3600;
	
	// baudrate
	divider = module_hz / baudrate;
	mantissa = divider / 16;
	fraction = divider - mantissa * 16;
	usart->BRR = (mantissa << 4) | fraction;
	
	usart->CR1 = cr1;
	usart->CR2 = cr2;
	usart->CR2 = cr3;
	usart->CR1 |= STM32_USART_CR1_UE;
	return VSFERR_NONE;
}

vsf_err_t stm32_usart_config_callback(uint8_t index, uint32_t int_priority,
				void *p, void (*ontx)(void *), void (*onrx)(void *, uint16_t))
{
	USART_TypeDef *usart;
	uint8_t usart_idx = index & 0x0F;
	uint32_t cr1 = 0;
	uint8_t irqn;
	
#if __VSF_DEBUG__
	if (usart_idx >= USART_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	usart = (USART_TypeDef *)stm32_usarts[usart_idx];
	irqn = stm32_usart_irqn[index];
	
	stm32_usart_ontx[usart_idx] = ontx;
	stm32_usart_onrx[usart_idx] = onrx;
	stm32_usart_callback_param[usart_idx] = p;
	if (ontx != NULL)
	{
		cr1 |= STM32_USART_CR1_TXEIE;
	}
	if (onrx != NULL)
	{
		cr1 |= STM32_USART_CR1_RXNEIE;
	}
	usart->CR1 &= ~(STM32_USART_CR1_TXEIE | STM32_USART_CR1_RXNEIE);
	usart->CR1 |= cr1;
	
	if ((ontx != NULL) || (onrx != NULL))
	{
		NVIC->IP[irqn] = int_priority;
		NVIC->ISER[irqn >> 0x05] = 1UL << (irqn & 0x1F);
	}
	else
	{
		NVIC->ICER[irqn >> 0x05] = 1UL << (irqn & 0x1F);
	}
	return VSFERR_NONE;
}

vsf_err_t stm32_usart_tx(uint8_t index, uint16_t data)
{
	USART_TypeDef *usart;
	uint8_t usart_idx = index & 0x0F;
	
#if __VSF_DEBUG__
	if (usart_idx >= USART_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	usart = (USART_TypeDef *)stm32_usarts[usart_idx];
	
	usart->DR = data;
	return VSFERR_NONE;
}

vsf_err_t stm32_usart_tx_isready(uint8_t index)
{
	USART_TypeDef *usart;
	uint8_t usart_idx = index & 0x0F;
	
#if __VSF_DEBUG__
	if (usart_idx >= USART_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	usart = (USART_TypeDef *)stm32_usarts[usart_idx];
	
	return ((usart->SR & STM32_USART_SR_TC) != 0) ?
				VSFERR_NONE : VSFERR_NOT_READY;
}

uint16_t stm32_usart_rx(uint8_t index)
{
	USART_TypeDef *usart;
	uint8_t usart_idx = index & 0x0F;
	
#if __VSF_DEBUG__
	if (usart_idx >= USART_NUM)
	{
		return 0;
	}
#endif
	usart = (USART_TypeDef *)stm32_usarts[usart_idx];
	
	return usart->DR;
}

vsf_err_t stm32_usart_rx_isready(uint8_t index)
{
	USART_TypeDef *usart;
	uint8_t usart_idx = index & 0x0F;
	
#if __VSF_DEBUG__
	if (usart_idx >= USART_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	usart = (USART_TypeDef *)stm32_usarts[usart_idx];
	
	return ((usart->SR & STM32_USART_SR_RXNE) != 0) ?
				VSFERR_NONE : VSFERR_NOT_READY;
}

#if USART0_INT_EN && (USART00_ENABLE || USART10_ENABLE)
ROOTFUNC void USART1_IRQHandler(void)
{
	if ((stm32_usart_onrx[0] != NULL) && (USART1->SR & STM32_USART_SR_RXNE))
	{
		stm32_usart_onrx[0](stm32_usart_callback_param[0], USART1->DR);
	}
	if ((stm32_usart_ontx[0] != NULL) && (USART1->SR & STM32_USART_SR_TXE))
	{
		stm32_usart_ontx[0](stm32_usart_callback_param[0]);
	}
}
#endif

#if USART1_INT_EN && (USART01_ENABLE || USART11_ENABLE)
ROOTFUNC void USART2_IRQHandler(void)
{
	if ((stm32_usart_onrx[1] != NULL) && (USART2->SR & STM32_USART_SR_RXNE))
	{
		stm32_usart_onrx[1](stm32_usart_callback_param[1], USART2->DR);
	}
	if ((stm32_usart_ontx[1] != NULL) && (USART2->SR & STM32_USART_SR_TXE))
	{
		stm32_usart_ontx[1](stm32_usart_callback_param[1]);
	}
}
#endif

#if USART2_INT_EN && (USART02_ENABLE || USART12_ENABLE || USART32_ENABLE)
ROOTFUNC void USART3_IRQHandler(void)
{
	if ((stm32_usart_onrx[2] != NULL) && (USART3->SR & STM32_USART_SR_RXNE))
	{
		stm32_usart_onrx[2](stm32_usart_callback_param[2], USART3->DR);
	}
	if ((stm32_usart_ontx[2] != NULL) && (USART3->SR & STM32_USART_SR_TXE))
	{
		stm32_usart_ontx[2](stm32_usart_callback_param[2]);
	}
}
#endif

#endif
