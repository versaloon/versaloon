#include "vsf_err.h"

#include "app_cfg.h"
#include "app_type.h"

#include "interfaces_cfg.h"
#include "interfaces_const.h"
#include "interfaces.h"
#include "core.h"

#define STM32_RCC_CR_HSEON				(1 << 16)
#define STM32_RCC_CR_HSERDY				(1 << 17)
#define STM32_RCC_CR_HSEBYP				(1 << 18)
#define STM32_RCC_CR_PLLON				(1 << 24)
#define STM32_RCC_CR_PLLRDY				(1 << 25)

#define STM32_RCC_CFGR_SW_MSK			0x00000003
#define STM32_RCC_CFGR_HPRE_SFT			4
#define STM32_RCC_CFGR_HPRE_MSK			(0x0F << STM32_RCC_CFGR_HPRE_SFT)
#define STM32_RCC_CFGR_PPRE1_SFT		8
#define STM32_RCC_CFGR_PPRE1_MSK		(0x07 << STM32_RCC_CFGR_PPRE1_SFT)
#define STM32_RCC_CFGR_PPRE2_SFT		11
#define STM32_RCC_CFGR_PPRE2_MSK		(0x07 << STM32_RCC_CFGR_PPRE2_SFT)
#define STM32_RCC_CFGR_PLLSRC			(1 << 16)
#define STM32_RCC_CFGR_PLLXTPRE			(1 << 17)
#define STM32_RCC_CFGR_PLLMUL_SFT		18
#define STM32_RCC_CFGR_PLLMUL_MSK		(0x0F << STM32_RCC_CFGR_PLLMUL_SFT)

#define RCC_APB2Periph_TIM1              ((uint32_t)0x00000800)
#define RCC_APB1Periph_TIM2              ((uint32_t)0x00000001)
#define RCC_APB1Periph_TIM5              ((uint32_t)0x00000008)

#define STM32_FLASH_ACR_PRFTBE			(1 << 4)

#define STM32_RCC_APB2ENR_AFIO			(1 << 0)

#define STM32_AFIO_MAPR_SWJCFG_SFT		24

#define STM32_HSI_FREQ_HZ				(8 * 1000 * 1000)

#define STM32_UID_ADDR					0x1FFFF7E8
#define STM32_UID_SIZE					12

static struct stm32_info_t stm32_info = 
{
	0, CORE_VECTOR_TABLE, CORE_CLKSRC, CORE_PLLSRC, CORE_RTCSRC, CORE_HSE_TYPE,
	OSC0_FREQ_HZ, CORE_PLL_FREQ_HZ, CORE_AHB_FREQ_HZ, CORE_APB1_FREQ_HZ,
	CORE_APB2_FREQ_HZ, CORE_FLASH_LATENCY, CORE_DEBUG
};

vsf_err_t stm32_interface_get_info(struct stm32_info_t **info)
{
	*info = &stm32_info;
	return VSFERR_NONE;
}

vsf_err_t stm32_interface_fini(void *p)
{
	return VSFERR_NONE;
}

vsf_err_t stm32_interface_reset(void *p)
{
	NVIC_SystemReset();
	return VSFERR_NONE;
}

vsf_err_t stm32_interface_set_stack(uint32_t sp)
{
	__set_MSP(sp);
	return VSFERR_NONE;
}

// sleep will enable interrupt
// for cortex processor, if an interrupt occur between enable the interrupt
// 		and __WFI, wfi will not make the core sleep
void stm32_interface_sleep(uint32_t mode)
{
	vsf_leave_critical();
	__WFI();
}

static uint32_t __log2__(uint32_t n)
{
	uint32_t i, value = 1;
	
	for (i = 0; i < 31; i++)
	{
		if (value == n)
		{
			return i;
		}
		value <<= 1;
	}
	return 0;
}

vsf_err_t stm32_interface_init(void *p)
{
	uint32_t tmp32;
	
	if (p != NULL)
	{
		stm32_info = *(struct stm32_info_t *)p;
	}
	
	switch (stm32_info.clksrc)
	{
	case STM32_CLKSRC_HSI:
		stm32_info.sys_freq_hz = STM32_HSI_FREQ_HZ;
		break;
	case STM32_CLKSRC_HSE:
		stm32_info.sys_freq_hz = OSC0_FREQ_HZ;
		break;
	case STM32_CLKSRC_PLL:
		stm32_info.sys_freq_hz = CORE_PLL_FREQ_HZ;
		break;
	}
	
	// RCC Reset
	RCC->CR |= (uint32_t)0x00000001;
#ifndef STM32F10X_CL
	RCC->CFGR &= (uint32_t)0xF8FF0000;
#else
	RCC->CFGR &= (uint32_t)0xF0FF0000;
#endif
	RCC->CR &= (uint32_t)0xFEF6FFFF;
	RCC->CR &= (uint32_t)0xFFFBFFFF;
	RCC->CFGR &= (uint32_t)0xFF80FFFF;
#ifdef STM32F10X_CL
	RCC->CR &= (uint32_t)0xEBFFFFFF;
	RCC->CIR = 0x00FF0000;
	RCC->CFGR2 = 0x00000000;
#elif defined (STM32F10X_LD_VL) || defined (STM32F10X_MD_VL) 
	RCC->CIR = 0x009F0000;
	RCC->CFGR2 = 0x00000000;      
#else
	RCC->CIR = 0x009F0000;
#endif
	
	if ((STM32_CLKSRC_HSE == stm32_info.clksrc) || 
		(STM32_PLLSRC_HSE == stm32_info.pllsrc) || 
		(STM32_PLLSRC_HSEd2 == stm32_info.pllsrc) || 
		(STM32_RTCSRC_HSEd128 == stm32_info.rtcsrc))
	{
		RCC->CR |= STM32_RCC_CR_HSEON;
		
		if (STM32_HSE_TYPE_CLOCK == stm32_info.hse_type)
		{
			RCC->CR |= STM32_RCC_CR_HSEBYP;
		}
		else
		{
			RCC->CR &= ~STM32_RCC_CR_HSEBYP;
		}
		
		while (!(RCC->CR & STM32_RCC_CR_HSERDY));
	}
	else
	{
		RCC->CR &= ~STM32_RCC_CR_HSEON;
	}
	
	FLASH->ACR = STM32_FLASH_ACR_PRFTBE | CORE_FLASH_LATENCY;
	RCC->CFGR &= ~(STM32_RCC_CFGR_HPRE_MSK | STM32_RCC_CFGR_PPRE1_MSK | 
					STM32_RCC_CFGR_PPRE2_MSK);
	
	tmp32 = __log2__(stm32_info.sys_freq_hz / stm32_info.ahb_freq_hz);
	if (tmp32)
	{
		RCC->CFGR |= (0x08 | (tmp32 - 1)) << STM32_RCC_CFGR_HPRE_SFT;
	}
	tmp32 = __log2__(stm32_info.sys_freq_hz / stm32_info.apb1_freq_hz);
	if (tmp32)
	{
		RCC->CFGR |= (0x04 | (tmp32 - 1)) << STM32_RCC_CFGR_PPRE1_SFT;
	}
	tmp32 = __log2__(stm32_info.sys_freq_hz / stm32_info.apb2_freq_hz);
	if (tmp32)
	{
		RCC->CFGR |= (0x04 | (tmp32 - 1)) << STM32_RCC_CFGR_PPRE2_SFT;
	}
	
	if (stm32_info.pll_freq_hz)
	{
		RCC->CFGR &= ~(STM32_RCC_CFGR_PLLMUL_MSK | STM32_RCC_CFGR_PLLSRC | 
						STM32_RCC_CFGR_PLLXTPRE);
		switch (stm32_info.pllsrc)
		{
		case STM32_PLLSRC_HSE:
			tmp32 = stm32_info.osc_freq_hz;
			RCC->CFGR |= STM32_RCC_CFGR_PLLSRC;
			break;
		case STM32_PLLSRC_HSEd2:
			tmp32 = stm32_info.osc_freq_hz / 2;
			RCC->CFGR |= STM32_RCC_CFGR_PLLSRC | STM32_RCC_CFGR_PLLXTPRE;
			break;
		case STM32_PLLSRC_HSId2:
			tmp32 = STM32_HSI_FREQ_HZ / 2;
			break;
		}
		tmp32 = stm32_info.pll_freq_hz / tmp32;
#if __VSF_DEBUG__
		if ((tmp32 < 2) || (tmp32 > 16))
		{
			return VSFERR_INVALID_PARAMETER;
		}
#endif
		RCC->CFGR |= ((tmp32 - 2) << STM32_RCC_CFGR_PLLMUL_SFT);
		
		RCC->CR |= STM32_RCC_CR_PLLON;
		while (!(RCC->CR & STM32_RCC_CR_PLLRDY));
	}
	
	RCC->CFGR &= ~STM32_RCC_CFGR_SW_MSK;
	RCC->CFGR |= CORE_CLKSRC;
	while (((RCC->CFGR >> 2) & STM32_RCC_CFGR_SW_MSK) != CORE_CLKSRC);
	
	RCC->APB2ENR |= STM32_RCC_APB2ENR_AFIO;
	AFIO->MAPR |= stm32_info.debug_setting << STM32_AFIO_MAPR_SWJCFG_SFT;
	
	SCB->VTOR = stm32_info.vector_table;
	SCB->AIRCR = 0x05FA0000 | stm32_info.priority_group;
	return VSFERR_NONE;
}

uint32_t stm32_uid_get(uint8_t *buffer, uint32_t size)
{
	if (NULL == buffer)
	{
		return 0;
	}
	
	if (size > STM32_UID_SIZE)
	{
		size = STM32_UID_SIZE;
	}
	
	memcpy(buffer, (uint8_t *)STM32_UID_ADDR, size);
	return size;
}

#define CM3_SYSTICK_ENABLE				(1 << 0)
#define CM3_SYSTICK_CLKSOURCE			(1 << 2)
#define CM3_SYSTICK_COUNTFLAG			(1 << 16)

vsf_err_t stm32_delay_init(void)
{
	SysTick->CTRL = CM3_SYSTICK_CLKSOURCE;
	SysTick->VAL = 0;
	return VSFERR_NONE;
}

static vsf_err_t stm32_delay_delayus_do(uint32_t tick)
{
	uint32_t dly_tmp;
	
	stm32_delay_init();
	while (tick)
	{
		dly_tmp = (tick > ((1 << 24) - 1)) ? ((1 << 24) - 1) : tick;
		SysTick->LOAD = dly_tmp;
		SysTick->CTRL |= CM3_SYSTICK_ENABLE;
		while (!(SysTick->CTRL & CM3_SYSTICK_COUNTFLAG));
		stm32_delay_init();
		tick -= dly_tmp;
	}
	return VSFERR_NONE;
}

vsf_err_t stm32_delay_delayus(uint16_t us)
{
	stm32_delay_delayus_do(us * (stm32_info.sys_freq_hz / (1000 * 1000)));
	return VSFERR_NONE;
}

vsf_err_t stm32_delay_delayms(uint16_t ms)
{
	stm32_delay_delayus_do(ms * (stm32_info.sys_freq_hz / 1000));
	return VSFERR_NONE;
}

// tickclk
#define TIM_INT_Update						((uint16_t)0x0001)
#define TIM_SlaveMode_External1				((uint16_t)0x0007)
#define TIM_TRGOSource_Update				((uint16_t)0x0020)
#define TIM_TS_ITR0							((uint16_t)0x0000)
#define TIM_CounterMode_Up					((uint16_t)0x0000)
#define TIM_PSCReloadMode_Immediate			((uint16_t)0x0001)

static void (*stm32_tickclk_callback)(void *param) = NULL;
static void *stm32_tickclk_param = NULL;
vsf_err_t stm32_tickclk_start(void)
{
	TIM5->CR1 |= TIM_CR1_CEN;
	TIM2->CR1 |= TIM_CR1_CEN;
	TIM1->CR1 |= TIM_CR1_CEN;
	return VSFERR_NONE;
}

vsf_err_t stm32_tickclk_stop(void)
{
	TIM1->CR1 &= ~TIM_CR1_CEN;
	TIM2->CR1 &= ~TIM_CR1_CEN;
	TIM5->CR1 &= ~TIM_CR1_CEN;
	return VSFERR_NONE;
}

static uint32_t stm32_tickclk_get_count_local(void)
{
	uint32_t count;
	count = TIM2->CNT;
	count |= (uint32_t)TIM5->CNT << 16;
	return count;
}

uint32_t stm32_tickclk_get_count(void)
{
	uint32_t count1, count2;
	
	do {
		count1 = stm32_tickclk_get_count_local();
		count2 = stm32_tickclk_get_count_local();
	} while (count1 != count2);
	return count1;
}

#if defined(STM32F10X_XL)
ROOTFUNC void TIM1_UP_TIM10_IRQHandler(void)
#elif defined(STM32F10X_LD) || defined(STM32F10X_MD) || defined(STM32F10X_HD) || defined(STM32F10X_CL)
ROOTFUNC void TIM1_UP_IRQHandler(void)
#elif defined(STM32F10X_MD_VL) || defined(STM32F10X_LD_VL) || defined(STM32F10X_MD_VL)
ROOTFUNC void TIM1_UP_TIM16_IRQHandler(void)
#endif
{
	if (stm32_tickclk_callback != NULL)
	{
		stm32_tickclk_callback(stm32_tickclk_param);
	}
	TIM1->SR = ~TIM_INT_Update;
}

vsf_err_t stm32_tickclk_set_callback(void (*callback)(void*), void *param)
{
	uint8_t irqn;
	
#if defined(STM32F10X_XL)
	irqn = TIM1_UP_TIM10_IRQn;
#elif defined(STM32F10X_MD_VL) || defined(STM32F10X_LD_VL) || defined(STM32F10X_MD_VL)
	irqn = TIM1_UP_TIM16_IRQn;
#elif defined(STM32F10X_LD) || defined(STM32F10X_MD) || defined(STM32F10X_HD) || defined(STM32F10X_CL)
	irqn = TIM1_UP_IRQn;
#endif
	
	if (callback != NULL)
	{
		// enable interrupt
		TIM1->DIER |= TIM_INT_Update;
		stm32_tickclk_callback = callback;
		stm32_tickclk_param = param;
		NVIC->IP[irqn] = 0xFF;
		NVIC->ISER[irqn >> 0x05] = 1UL << (irqn & 0x1F);
	}
	else
	{
		// disable interrupt
		TIM1->DIER &= ~TIM_INT_Update;
		stm32_tickclk_callback = callback;
		stm32_tickclk_param = param;
		NVIC->ICER[irqn >> 0x05] = 1UL << (irqn & 0x1F);
	}
	return VSFERR_NONE;
}

vsf_err_t stm32_tickclk_init(void)
{
	uint16_t tmp_reg;
	
	RCC->APB2ENR |= RCC_APB2Periph_TIM1;
	RCC->APB1ENR |= RCC_APB1Periph_TIM2 | RCC_APB1Periph_TIM5;
	RCC->APB2RSTR |= RCC_APB2Periph_TIM1;
	RCC->APB1RSTR |= RCC_APB1Periph_TIM2 | RCC_APB1Periph_TIM5;
	RCC->APB2RSTR &= ~RCC_APB2Periph_TIM1;
	RCC->APB1RSTR &= ~(RCC_APB1Periph_TIM2 | RCC_APB1Periph_TIM5);
	
	// TIM1 generate 1ms event
	tmp_reg = TIM1->CR1;
	tmp_reg &= ~(TIM_CR1_DIR | TIM_CR1_CMS);
    tmp_reg |= TIM_CounterMode_Up;
	tmp_reg &= ~TIM_CR1_CKD;
	TIM1->CR1 = tmp_reg;
	TIM1->ARR = stm32_info.apb2_freq_hz / 2 / 1000;
	TIM1->PSC = 1;
	TIM1->RCR = 0;
	TIM1->EGR = TIM_PSCReloadMode_Immediate;
	TIM1->CR2 &= ~TIM_CR2_MMS;
	TIM1->CR2 |= TIM_TRGOSource_Update;
	TIM1->SMCR &= ~TIM_SMCR_MSM;
	TIM1->SMCR |= TIM_SMCR_MSM;
	
	// TIM2 accept 1ms clock from TIM1
	tmp_reg = TIM2->CR1;
	tmp_reg &= ~(TIM_CR1_DIR | TIM_CR1_CMS);
    tmp_reg |= TIM_CounterMode_Up;
	tmp_reg &= ~TIM_CR1_CKD;
	TIM2->CR1 = tmp_reg;
	TIM2->ARR = 0xFFFF;
	TIM2->PSC = 0;
	TIM2->RCR = 0;
	TIM2->EGR = TIM_PSCReloadMode_Immediate;
	TIM2->SMCR = (TIM2->SMCR & ~TIM_SMCR_TS) | TIM_TS_ITR0;
	TIM2->SMCR &= ~TIM_SMCR_SMS;
	TIM2->SMCR |= TIM_SlaveMode_External1;
	TIM2->CR2 &= ~TIM_CR2_MMS;
	TIM2->CR2 |= TIM_TRGOSource_Update;
	TIM2->SMCR &= ~TIM_SMCR_MSM;
	TIM2->SMCR |= TIM_SMCR_MSM;
	
	// TIM5 accept 65536ms clock from TIM2
	tmp_reg = TIM5->CR1;
	tmp_reg &= ~(TIM_CR1_DIR | TIM_CR1_CMS);
    tmp_reg |= TIM_CounterMode_Up;
	tmp_reg &= ~TIM_CR1_CKD;
	TIM5->CR1 = tmp_reg;
	TIM5->ARR = 0xFFFF;
	TIM5->PSC = 0;
	TIM5->RCR = 0;
	TIM5->EGR = TIM_PSCReloadMode_Immediate;
	TIM5->SMCR = (TIM5->SMCR & ~TIM_SMCR_TS) | TIM_TS_ITR0;
	TIM5->SMCR &= ~TIM_SMCR_SMS;
	TIM5->SMCR |= TIM_SlaveMode_External1;
	
	return VSFERR_NONE;
}

vsf_err_t stm32_tickclk_fini(void)
{
	RCC->APB2ENR &= ~RCC_APB2Periph_TIM1;
	RCC->APB1ENR &= ~(RCC_APB1Periph_TIM2 | RCC_APB1Periph_TIM5);
	return VSFERR_NONE;
}
