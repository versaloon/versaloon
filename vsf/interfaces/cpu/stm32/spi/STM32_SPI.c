/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       GPIO.h                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    GPIO interface header file                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_type.h"
#include "interfaces.h"

// TODO: remove MACROs below to stm32_reg.h
#define STM32_AFIO_MAPR_SPI1			((uint32_t)1 << 0)
#define STM32_AFIO_MAPR_SWJCFG			((uint32_t)7 << 24)

#define STM32_RCC_APB1ENR_SPI2EN		((uint32_t)1 << 14)
#define STM32_RCC_APB1ENR_SPI3EN		((uint32_t)1 << 15)
#define STM32_RCC_APB2ENR_SPI1EN		((uint32_t)1 << 12)
#define STM32_RCC_APB2ENR_IOPAEN		((uint32_t)1 << 2)
#define STM32_RCC_APB2ENR_IOPBEN		((uint32_t)1 << 3)
#define STM32_RCC_APB2ENR_IOPCEN		((uint32_t)1 << 4)
#define STM32_RCC_APB2ENR_IOPDEN		((uint32_t)1 << 5)

#define STM32_RCC_AHBENR_DMA1			((uint32_t)1 << 0)
#define STM32_RCC_AHBENR_DMA2			((uint32_t)1 << 1)

#define STM32_SPI_CR1_SSM				((uint32_t)1 << 9)
#define STM32_SPI_CR1_SSI				((uint32_t)1 << 8)
#define STM32_SPI_CR1_SPE				((uint32_t)1 << 6)

#define STM32_SPI_SR_RXNE				((uint32_t)1 << 0)
#define STM32_SPI_SR_TXE				((uint32_t)1 << 1)

#define STM32_SPI_I2SCFGR				((uint32_t)1 << 11)

#define STM32_DMA_CCR_EN				((uint32_t)1 << 0)

#if IFS_SPI_EN

#include "STM32_SPI.h"

static const SPI_TypeDef *stm32_spis[SPI_NUM] = 
{
#if SPI_NUM >= 1
	SPI1, 
#endif
#if SPI_NUM >= 2
	SPI2, 
#endif
#if SPI_NUM >= 3
	SPI3
#endif
};

static uint8_t stm32_spi_get_sck_div(uint32_t module_khz, uint32_t khz)
{
	// Set Speed
	if(khz >= module_khz / 2)
	{
		khz = 0;
	}
	else if(khz >= module_khz / 4)
	{
		khz = 1;
	}
	else if(khz >= module_khz / 8)
	{
		khz = 2;
	}
	else if(khz >= module_khz / 16)
	{
		khz = 3;
	}
	else if(khz >= module_khz / 32)
	{
		khz = 4;
	}
	else if(khz > module_khz / 64)
	{
		khz = 5;
	}
	else if(khz > module_khz / 128)
	{
		khz = 6;
	}
	else
	{
		khz = 7;
	}

	return (uint8_t)(khz << 3);
}

vsf_err_t stm32_spi_init(uint8_t index)
{
	uint8_t spi_idx = index & 0x0F;
	uint8_t remap_idx = (index >> 4) & 0x0F;
	
#if __VSF_DEBUG__
	if (spi_idx >= SPI_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	switch (spi_idx)
	{
	#if SPI00_ENABLE || SPI10_ENABLE
	case 0:
		RCC->APB2ENR |= STM32_RCC_APB2ENR_SPI1EN;
		RCC->APB2RSTR |= STM32_RCC_APB2ENR_SPI1EN;
		RCC->APB2RSTR &= ~STM32_RCC_APB2ENR_SPI1EN;
		switch (remap_idx)
		{
		#if SPI00_ENABLE
		case 0:
			AFIO->MAPR = (AFIO->MAPR & ~STM32_AFIO_MAPR_SPI1)
							| STM32_AFIO_MAPR_SWJCFG;
			RCC->APB2ENR |= STM32_RCC_APB2ENR_IOPAEN;
			break;
		#endif
		#if SPI10_ENABLE
		case 1:
			AFIO->MAPR |= STM32_AFIO_MAPR_SPI1 | STM32_AFIO_MAPR_SWJCFG;
			RCC->APB2ENR |= STM32_RCC_APB2ENR_IOPBEN
			#if SPI10_NSS_ENABLE
							| STM32_RCC_APB2ENR_IOPAEN
			#endif
							;
			break;
		#endif
		default:
			return VSFERR_NOT_SUPPORT;
		}
		break;
	#endif
	#if SPI01_ENABLE
	case 1:
		RCC->APB1ENR |= STM32_RCC_APB1ENR_SPI2EN;
		RCC->APB1RSTR |= STM32_RCC_APB1ENR_SPI2EN;
		RCC->APB1RSTR &= ~STM32_RCC_APB1ENR_SPI2EN;
		switch (remap_idx)
		{
		case 0:
			RCC->APB2ENR |= STM32_RCC_APB2ENR_IOPBEN;
			break;
		default:
			return VSFERR_NOT_SUPPORT;
		}
		break;
	#endif
	#if SPI02_ENABLE
	case 2:
		RCC->APB1ENR |= STM32_RCC_APB1ENR_SPI3EN;
		RCC->APB1RSTR |= STM32_RCC_APB1ENR_SPI3EN;
		RCC->APB1RSTR &= ~STM32_RCC_APB1ENR_SPI3EN;
		switch (remap_idx)
		{
		case 0:
			RCC->APB2ENR |= STM32_RCC_APB2ENR_IOPBEN
			#if SPI02_NSS_ENABLE
							| STM32_RCC_APB2ENR_IOPAEN
			#endif
							;
			break;
		default:
			return VSFERR_NOT_SUPPORT;
		}
		break;
	#endif
	}
	return VSFERR_NONE;
}

vsf_err_t stm32_spi_fini(uint8_t index)
{
	uint8_t spi_idx = index & 0x0F;
	uint8_t remap_idx = (index >> 4) & 0x0F;
	
#if __VSF_DEBUG__
	if (spi_idx >= SPI_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	switch (spi_idx)
	{
	#if SPI00_ENABLE || SPI10_ENABLE
	case 0:
		RCC->APB2ENR &= ~STM32_RCC_APB2ENR_SPI1EN;
		switch (remap_idx)
		{
		#if SPI00_ENABLE
		case 0:
			GPIOA->CRL = (GPIOA->CRL & ~(0x0F << (5 * 4))) | 
							(uint32_t)stm32_GPIO_INFLOAT << (5 * 4);
			#if SPI00_MISO_ENABLE
			GPIOA->CRL = (GPIOA->CRL & ~(0x0F << (6 * 4))) | 
							(uint32_t)stm32_GPIO_INFLOAT << (6 * 4);
			#endif
			#if SPI00_MOSI_ENABLE
			GPIOA->CRL = (GPIOA->CRL & ~(0x0F << (7 * 4))) | 
							(uint32_t)stm32_GPIO_INFLOAT << (7 * 4);
			#endif
			#if SPI00_NSS_ENABLE
			GPIOA->CRL = (GPIOA->CRL & ~(0x0F << (4 * 4))) | 
							(uint32_t)stm32_GPIO_INFLOAT << (4 * 4);
			#endif
			break;
		#endif
		#if SPI10_ENABLE
		case 1:
			GPIOB->CRL = (GPIOB->CRL & ~(0x0F << (3 * 4))) | 
							(uint32_t)stm32_GPIO_INFLOAT << (3 * 4);
			#if SPI10_MISO_ENABLE
			GPIOB->CRL = (GPIOB->CRL & ~(0x0F << (4 * 4))) | 
							(uint32_t)stm32_GPIO_INFLOAT << (4 * 4);
			#endif
			#if SPI10_MOSI_ENABLE
			GPIOB->CRL = (GPIOB->CRL & ~(0x0F << (5 * 4))) | 
							(uint32_t)stm32_GPIO_INFLOAT << (5 * 4);
			#endif
			#if SPI10_NSS_ENABLE
			GPIOA->CRH = (GPIOA->CRH & ~(0x0F << ((15 - 8) * 4))) | 
							(uint32_t)stm32_GPIO_INFLOAT << ((15 - 8) * 4);
			#endif
			break;
		#endif
		default:
			return VSFERR_NOT_SUPPORT;
		}
		break;
	#endif
	#if SPI01_ENABLE
	case 1:
		RCC->APB1ENR &= ~STM32_RCC_APB1ENR_SPI2EN;
		switch (remap_idx)
		{
		case 0:
			GPIOB->CRH = (GPIOB->CRH & ~(0x0F << ((13 - 8) * 4))) | 
							(uint32_t)stm32_GPIO_INFLOAT << ((13 - 8) * 4);
			#if SPI01_MISO_ENABLE
			GPIOB->CRH = (GPIOB->CRH & ~(0x0F << ((14 - 8) * 4))) | 
							(uint32_t)stm32_GPIO_INFLOAT << ((14 - 8) * 4);
			#endif
			#if SPI01_MOSI_ENABLE
			GPIOB->CRH = (GPIOB->CRH & ~(0x0F << ((15 - 8) * 4))) | 
							(uint32_t)stm32_GPIO_INFLOAT << ((15 - 8) * 4);
			#endif
			#if SPI01_NSS_ENABLE
			GPIOB->CRH = (GPIOB->CRH & ~(0x0F << ((12 - 8) * 4))) | 
							(uint32_t)stm32_GPIO_INFLOAT << ((12 - 8) * 4);
			#endif
			break;
		default:
			return VSFERR_NOT_SUPPORT;
		}
		break;
	#endif
	#if SPI02_ENABLE
	case 2:
		RCC->APB1ENR &= ~STM32_RCC_APB1ENR_SPI3EN;
		switch (remap_idx)
		{
		case 0:
			GPIOB->CRL = (GPIOB->CRL & ~(0x0F << (3 * 4))) | 
							(uint32_t)stm32_GPIO_INFLOAT << (3 * 4);
			#if SPI02_MISO_ENABLE
			GPIOB->CRL = (GPIOB->CRL & ~(0x0F << (4 * 4))) | 
							(uint32_t)stm32_GPIO_INFLOAT << (4 * 4);
			#endif
			#if SPI01_MOSI_ENABLE
			GPIOB->CRL = (GPIOB->CRL & ~(0x0F << (5 * 4))) | 
							(uint32_t)stm32_GPIO_INFLOAT << (5 * 4);
			#endif
			#if SPI02_NSS_ENABLE
			GPIOA->CRH = (GPIOA->CRH & ~(0x0F << ((15 - 8) * 4))) | 
							(uint32_t)stm32_GPIO_INFLOAT << ((15 - 8) * 4);
			#endif
			break;
		default:
			return VSFERR_NOT_SUPPORT;
		}
		break;
	#endif
	}
	SPI_I2S_DeInit((SPI_TypeDef *)stm32_spis[spi_idx]);
	return VSFERR_NONE;
}

vsf_err_t stm32_spi_enable(uint8_t index)
{
	uint8_t spi_idx = index & 0x0F;
	SPI_TypeDef *spi;
	
#if __VSF_DEBUG__
	if (spi_idx >= SPI_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	spi = (SPI_TypeDef *)stm32_spis[spi_idx];
	spi->CR1 |= STM32_SPI_CR1_SPE;
	return VSFERR_NONE;
}

vsf_err_t stm32_spi_disable(uint8_t index)
{
	uint8_t spi_idx = index & 0x0F;
	SPI_TypeDef *spi;
	
#if __VSF_DEBUG__
	if (spi_idx >= SPI_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	spi = (SPI_TypeDef *)stm32_spis[spi_idx];
	spi->CR1 &= ~STM32_SPI_CR1_SPE;
	return VSFERR_NONE;
}

vsf_err_t stm32_spi_get_ability(uint8_t index, struct spi_ability_t *ability)
{
	uint8_t spi_idx = index & 0x0F;
	struct stm32_info_t *info;
	
#if __VSF_DEBUG__
	if (spi_idx >= SPI_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	if (stm32_interface_get_info(&info))
	{
		return VSFERR_FAIL;
	}
	
	switch (spi_idx)
	{
	case 0:
		ability->max_freq_hz = ability->min_freq_hz = info->apb2_freq_hz;
		break;
	case 1:
	case 2:
		ability->max_freq_hz = ability->min_freq_hz = info->apb1_freq_hz;
		break;
	}
	ability->max_freq_hz /= 2;
	ability->min_freq_hz /= 256;
	return VSFERR_NONE;
}

vsf_err_t stm32_spi_config(uint8_t index, uint32_t kHz, uint8_t mode)
{
	uint8_t spi_idx = index & 0x0F;
	uint8_t remap_idx = (index >> 4) & 0x0F;
	SPI_TypeDef *spi;
	uint32_t module_khz;
	struct stm32_info_t *info;
	uint8_t master = mode & stm32_SPI_MASTER;
	
#if __VSF_DEBUG__
	if (spi_idx >= SPI_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	spi = (SPI_TypeDef *)stm32_spis[spi_idx];
	
	if (stm32_interface_get_info(&info))
	{
		return VSFERR_FAIL;
	}
	
	switch (spi_idx)
	{
	#if SPI00_ENABLE || SPI10_ENABLE
	case 0:
		module_khz = info->apb2_freq_hz / 1000;
		switch (remap_idx)
		{
		#if SPI00_ENABLE
		case 0:
			if (master)
			{
				GPIOA->CRL = (GPIOA->CRL & ~(0x0F << (5 * 4))) | 
								(uint32_t)stm32_GPIO_AFPP << (5 * 4);
				#if SPI00_MISO_ENABLE
				GPIOA->CRL = (GPIOA->CRL & ~(0x0F << (6 * 4))) | 
								(uint32_t)stm32_GPIO_INFLOAT << (6 * 4);
				#endif
				#if SPI00_MOSI_ENABLE
				GPIOA->CRL = (GPIOA->CRL & ~(0x0F << (7 * 4))) | 
								(uint32_t)stm32_GPIO_AFPP << (7 * 4);
				#endif
				#if SPI00_NSS_ENABLE
				GPIOA->CRL = (GPIOA->CRL & ~(0x0F << (4 * 4))) | 
								(uint32_t)stm32_GPIO_OUTOD << (4 * 4);
				GPIOA->BSRR = 1UL << 4;
				#endif
			}
			else
			{
				GPIOA->CRL = (GPIOA->CRL & ~(0x0F << (5 * 4))) | 
								(uint32_t)stm32_GPIO_INFLOAT << (5 * 4);
				#if SPI00_MISO_ENABLE
				GPIOA->CRL = (GPIOA->CRL & ~(0x0F << (6 * 4))) | 
								(uint32_t)stm32_GPIO_AFPP << (6 * 4);
				#endif
				#if SPI00_MOSI_ENABLE
				GPIOA->CRL = (GPIOA->CRL & ~(0x0F << (7 * 4))) | 
								(uint32_t)stm32_GPIO_INFLOAT << (7 * 4);
				#endif
				#if SPI00_NSS_ENABLE
				GPIOA->CRL = (GPIOA->CRL & ~(0x0F << (4 * 4))) | 
								(uint32_t)stm32_GPIO_INP << (4 * 4);
				GPIOA->BSRR = (uint32_t)1 << 4;
				#endif
			}
			break;
		#endif
		#if SPI10_ENABLE
		case 1:
			if (master)
			{
				GPIOB->CRL = (GPIOB->CRL & ~(0x0F << (3 * 4))) | 
								(uint32_t)stm32_GPIO_AFPP << (3 * 4);
				#if SPI10_MISO_ENABLE
				GPIOB->CRL = (GPIOB->CRL & ~(0x0F << (4 * 4))) | 
								(uint32_t)stm32_GPIO_INFLOAT << (4 * 4);
				#endif
				#if SPI10_MOSI_ENABLE
				GPIOB->CRL = (GPIOB->CRL & ~(0x0F << (5 * 4))) | 
								(uint32_t)stm32_GPIO_AFPP << (5 * 4);
				#endif
				#if SPI10_NSS_ENABLE
				GPIOA->CRH = (GPIOA->CRH & ~(0x0F << ((15 - 8) * 4))) | 
								(uint32_t)stm32_GPIO_OUTOD << ((15 - 8) * 4);
				GPIOA->BSRR = 1UL << 15;
				#endif
			}
			else
			{
				GPIOB->CRL = (GPIOB->CRL & ~(0x0F << (3 * 4))) | 
								(uint32_t)stm32_GPIO_INFLOAT << (3 * 4);
				#if SPI10_MISO_ENABLE
				GPIOB->CRL = (GPIOB->CRL & ~(0x0F << (4 * 4))) | 
								(uint32_t)stm32_GPIO_AFPP << (4 * 4);
				#endif
				#if SPI10_MOSI_ENABLE
				GPIOB->CRL = (GPIOB->CRL & ~(0x0F << (5 * 4))) | 
								(uint32_t)stm32_GPIO_INFLOAT << (5 * 4);
				#endif
				#if SPI10_NSS_ENABLE
				GPIOA->CRH = (GPIOA->CRH & ~(0x0F << ((15 - 8) * 4))) | 
								(uint32_t)stm32_GPIO_INP << ((15 - 8) * 4);
				GPIOA->BSRR = (uint32_t)1 << 15;
				#endif
			}
			break;
		#endif
		default:
			return VSFERR_NOT_SUPPORT;
		}
		break;
	#endif
	#if SPI01_ENABLE
	case 1:
		module_khz = info->apb1_freq_hz / 1000;
		switch (remap_idx)
		{
		case 0:
			if (master)
			{
				GPIOB->CRH = (GPIOB->CRH & ~(0x0F << ((13 - 8) * 4))) | 
								(uint32_t)stm32_GPIO_AFPP << ((13 - 8) * 4);
				#if SPI01_MISO_ENABLE
				GPIOB->CRH = (GPIOB->CRH & ~(0x0F << ((14 - 8) * 4))) | 
								(uint32_t)stm32_GPIO_INFLOAT << ((14 - 8) * 4);
				#endif
				#if SPI01_MOSI_ENABLE
				GPIOB->CRH = (GPIOB->CRH & ~(0x0F << ((15 - 8) * 4))) | 
								(uint32_t)stm32_GPIO_AFPP << ((15 - 8) * 4);
				#endif
				#if SPI01_NSS_ENABLE
				GPIOB->CRH = (GPIOB->CRH & ~(0x0F << ((12 - 8) * 4))) | 
								(uint32_t)stm32_GPIO_OUTOD << ((12 - 8) * 4);
				GPIOB->BSRR = 1UL << 12;
				#endif
			}
			else
			{
				GPIOB->CRH = (GPIOB->CRH & ~(0x0F << ((13 - 8) * 4))) | 
								(uint32_t)stm32_GPIO_INFLOAT << ((13 - 8) * 4);
				#if SPI01_MISO_ENABLE
				GPIOB->CRH = (GPIOB->CRH & ~(0x0F << ((14 - 8) * 4))) | 
								(uint32_t)stm32_GPIO_AFPP << ((14 - 8) * 4);
				#endif
				#if SPI01_MOSI_ENABLE
				GPIOB->CRH = (GPIOB->CRH & ~(0x0F << ((15 - 8) * 4))) | 
								(uint32_t)stm32_GPIO_INFLOAT << ((15 - 8) * 4);
				#endif
				#if SPI01_NSS_ENABLE
				GPIOB->CRH = (GPIOB->CRH & ~(0x0F << ((12 - 8) * 4))) | 
								(uint32_t)stm32_GPIO_INP << ((12 - 8) * 4);
				GPIOB->BSRR = (uint32_t)1 << 12;
				#endif
			}
			break;
		default:
			return VSFERR_NOT_SUPPORT;
		}
		break;
	#endif
	#if SPI02_ENABLE
	case 2:
		module_khz = info->apb1_freq_hz / 1000;
		switch (remap_idx)
		{
		case 0:
			if (master)
			{
				GPIOB->CRL = (GPIOB->CRL & ~(0x0F << (3 * 4))) | 
								(uint32_t)stm32_GPIO_AFPP << (3 * 4);
				#if SPI02_MISO_ENABLE
				GPIOB->CRL = (GPIOB->CRL & ~(0x0F << (4 * 4))) | 
								(uint32_t)stm32_GPIO_INFLOAT << (4 * 4);
				#endif
				#if SPI02_MOSI_ENABLE
				GPIOB->CRL = (GPIOB->CRL & ~(0x0F << (5 * 4))) | 
								(uint32_t)stm32_GPIO_AFPP << (5 * 4);
				#endif
				#if SPI02_NSS_ENABLE
				GPIOA->CRH = (GPIOA->CRH & ~(0x0F << ((15 - 8) * 4))) | 
								(uint32_t)stm32_GPIO_OUTOD << ((15 - 8) * 4);
				GPIOA->BSRR = 1UL << 15;
				#endif
			}
			else
			{
				GPIOB->CRL = (GPIOB->CRL & ~(0x0F << (3 * 4))) | 
								(uint32_t)stm32_GPIO_INFLOAT << (3 * 4);
				#if SPI02_MISO_ENABLE
				GPIOB->CRL = (GPIOB->CRL & ~(0x0F << (4 * 4))) | 
								(uint32_t)stm32_GPIO_AFPP << (4 * 4);
				#endif
				#if SPI02_MOSI_ENABLE
				GPIOB->CRL = (GPIOB->CRL & ~(0x0F << (5 * 4))) | 
								(uint32_t)stm32_GPIO_INFLOAT << (5 * 4);
				#endif
				#if SPI02_NSS_ENABLE
				GPIOA->CRH = (GPIOA->CRH & ~(0x0F << ((15 - 8) * 4))) | 
								(uint32_t)stm32_GPIO_INP << ((15 - 8) * 4);
				GPIOA->BSRR = (uint32_t)1 << 15;
				#endif
			}
			break;
		default:
			return VSFERR_NOT_SUPPORT;
		}
		break;
	#endif
	default:
		return VSFERR_NOT_SUPPORT;
	}
	
	spi->CR1 &= ~STM32_SPI_CR1_SPE;
	spi->CR1 = mode | STM32_SPI_CR1_SSM | 
				stm32_spi_get_sck_div(module_khz, kHz);
	if (master)
	{
		spi->CR1 |= STM32_SPI_CR1_SSI;
	}
	spi->I2SCFGR &= ~STM32_SPI_I2SCFGR;
	spi->CR1 |= STM32_SPI_CR1_SPE;
	return VSFERR_NONE;
}

vsf_err_t stm32_spi_select(uint8_t index, uint8_t cs)
{
	uint8_t spi_idx = index & 0x0F;
	uint8_t remap_idx = (index >> 4) & 0x0F;
	
#if __VSF_DEBUG__
	if ((spi_idx >= SPI_NUM) || (cs > 0))
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	switch (spi_idx)
	{
	#if (SPI00_ENABLE && SPI00_NSS_ENABLE) || (SPI10_ENABLE && SPI10_NSS_ENABLE)
	case 0:
		switch (remap_idx)
		{
		#if SPI00_ENABLE && SPI00_NSS_ENABLE
		case 0:
			GPIOA->BRR = 1UL << 4;
			break;
		#endif
		#if SPI10_ENABLE && SPI10_NSS_ENABLE
		case 1:
			GPIOA->BRR = 1UL << 15;
			break;
		#endif
		default:
			return VSFERR_NOT_SUPPORT;
		}
		break;
	#endif
	#if SPI01_ENABLE && SPI01_NSS_ENABLE
	case 1:
		switch (remap_idx)
		{
		case 0:
			GPIOB->BRR = 1UL << 12;
			break;
		default:
			return VSFERR_NOT_SUPPORT;
		}
		break;
	#endif
	#if SPI02_ENABLE && SPI02_NSS_ENABLE
	case 2:
		switch (remap_idx)
		{
		case 0:
			GPIOA->BRR = 1UL << 15;
			break;
		default:
			return VSFERR_NOT_SUPPORT;
		}
		break;
	#endif
	default:
		return VSFERR_NOT_SUPPORT;
	}
	return VSFERR_NONE;
}

vsf_err_t stm32_spi_deselect(uint8_t index, uint8_t cs)
{
	uint8_t spi_idx = index & 0x0F;
	uint8_t remap_idx = (index >> 4) & 0x0F;
	
#if __VSF_DEBUG__
	if ((spi_idx >= SPI_NUM) || (cs > 0))
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	switch (spi_idx)
	{
	#if (SPI00_ENABLE && SPI00_NSS_ENABLE) || (SPI10_ENABLE && SPI10_NSS_ENABLE)
	case 0:
		switch (remap_idx)
		{
		#if SPI00_ENABLE && SPI00_NSS_ENABLE
		case 0:
			GPIOA->BSRR = 1UL << 4;
			break;
		#endif
		#if SPI10_ENABLE && SPI10_NSS_ENABLE
		case 1:
			GPIOA->BSRR = 1UL << 15;
			break;
		#endif
		default:
			return VSFERR_NOT_SUPPORT;
		}
		break;
	#endif
	#if SPI01_ENABLE && SPI01_NSS_ENABLE
	case 1:
		switch (remap_idx)
		{
		case 0:
			GPIOB->BSRR = 1UL << 12;
			break;
		default:
			return VSFERR_NOT_SUPPORT;
		}
		break;
	#endif
	#if SPI02_ENABLE && SPI02_NSS_ENABLE
	case 2:
		switch (remap_idx)
		{
		case 0:
			GPIOA->BSRR = 1UL << 15;
			break;
		default:
			return VSFERR_NOT_SUPPORT;
		}
		break;
	#endif
	default:
		return VSFERR_NOT_SUPPORT;
	}
	return VSFERR_NONE;
}

vsf_err_t stm32_spi_io_tx(uint8_t index, uint8_t out)
{
	uint8_t spi_idx = index & 0x0F;
	SPI_TypeDef *spi;
	
#if __VSF_DEBUG__
	if (spi_idx >= SPI_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	spi = (SPI_TypeDef *)stm32_spis[spi_idx];
	
	spi->DR = out;
	return VSFERR_NONE;
}

vsf_err_t stm32_spi_io_tx_isready(uint8_t index)
{
	uint8_t spi_idx = index & 0x0F;
	SPI_TypeDef *spi;
	
#if __VSF_DEBUG__
	if (spi_idx >= SPI_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	spi = (SPI_TypeDef *)stm32_spis[spi_idx];
	
	return ((spi->SR & STM32_SPI_SR_TXE) != 0) ? VSFERR_NONE : VSFERR_NOT_READY;
}

vsf_err_t stm32_spi_io_rx_isready(uint8_t index)
{
	uint8_t spi_idx = index & 0x0F;
	SPI_TypeDef *spi;
	
#if __VSF_DEBUG__
	if (spi_idx >= SPI_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	spi = (SPI_TypeDef *)stm32_spis[spi_idx];
	
	return ((spi->SR & STM32_SPI_SR_RXNE) != 0) ?
				VSFERR_NONE : VSFERR_NOT_READY;
}

uint8_t stm32_spi_io_rx(uint8_t index)
{
	uint8_t spi_idx = index & 0x0F;
	SPI_TypeDef *spi;
	
#if __VSF_DEBUG__
	if (spi_idx >= SPI_NUM)
	{
		return 0;
	}
#endif
	spi = (SPI_TypeDef *)stm32_spis[spi_idx];
	
	return spi->DR;
}

vsf_err_t stm32_spi_io(uint8_t index, uint8_t *out, uint8_t *in, uint32_t len)
{
	uint8_t spi_idx = index & 0x0F;
	SPI_TypeDef *spi;
	uint32_t i;
	uint8_t dummy = 0, out8;
	
	REFERENCE_PARAMETER(dummy);
#if __VSF_DEBUG__
	if (spi_idx >= SPI_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	spi = (SPI_TypeDef *)stm32_spis[spi_idx];
	
	if (NULL == out)
	{
		out8 = 0xFF;
		if (NULL == in)
		{
			for(i = 0; i < len; i++)
			{
				spi->DR = out8;
				while(!(spi->SR & SPI_I2S_FLAG_RXNE));
				dummy = spi->DR;
			}
		}
		else
		{
			for(i = 0; i < len; i++)
			{
				spi->DR = out8;
				while(!(spi->SR & SPI_I2S_FLAG_RXNE));
				in[i] = spi->DR;
			}
		}
	}
	else
	{
		if (NULL == in)
		{
			for(i = 0; i < len; i++)
			{
				spi->DR = out[i];
				while(!(spi->SR & SPI_I2S_FLAG_RXNE));
				dummy = spi->DR;
			}
		}
		else
		{
			for(i = 0; i < len; i++)
			{
				spi->DR = out[i];
				while(!(spi->SR & SPI_I2S_FLAG_RXNE));
				in[i] = spi->DR;
			}
		}
	}
	
	return VSFERR_NONE;
}

vsf_err_t stm32_spi_io_dma_start(uint8_t index, uint8_t *out, uint8_t *in, 
								uint32_t len)
{
	uint8_t spi_idx = index & 0x0F;
	SPI_TypeDef *spi;
	
#if __VSF_DEBUG__
	if ((spi_idx >= SPI_NUM) || (len > 0xFFFF))
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	spi = (SPI_TypeDef *)stm32_spis[spi_idx];
	
	switch (spi_idx)
	{
	#if SPI00_ENABLE || SPI10_ENABLE
	case 0:
		RCC->AHBENR |= STM32_RCC_AHBENR_DMA1;
		if (in != NULL)
		{
			DMA1_Channel2->CCR = 0x00000080;
			DMA1_Channel2->CNDTR = len;
			DMA1_Channel2->CPAR = (uint32_t)spi->DR;
			DMA1_Channel2->CMAR = (uint32_t)in;
			DMA1_Channel2->CCR |= STM32_DMA_CCR_EN;
		}
		if (out != NULL)
		{
			DMA1_Channel3->CCR = 0x00000090;
			DMA1_Channel3->CNDTR = len;
			DMA1_Channel3->CPAR = (uint32_t)spi->DR;
			DMA1_Channel3->CMAR = (uint32_t)out;
			DMA1_Channel3->CCR |= STM32_DMA_CCR_EN;
		}
		break;
	#endif
	#if SPI01_ENABLE
	case 1:
		RCC->AHBENR |= STM32_RCC_AHBENR_DMA1;
		break;
	#endif
	#if SPI01_ENABLE
	case 2:
		RCC->AHBENR |= STM32_RCC_AHBENR_DMA2;
		break;
	#endif
	}
	
	return VSFERR_NONE;
}

vsf_err_t stm32_spi_io_dma_isready(uint8_t index)
{
	REFERENCE_PARAMETER(index);
	return VSFERR_NONE;
}

vsf_err_t stm32_spi_io_dma_end(uint8_t index)
{
	REFERENCE_PARAMETER(index);
	return VSFERR_NONE;
}

#endif
