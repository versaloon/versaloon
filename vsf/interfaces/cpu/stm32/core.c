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

#define STM32_FLASH_ACR_PRFTBE			(1 << 4)

#define STM32_RCC_APB2ENR_AFIO			(1 << 0)

#define STM32_AFIO_MAPR_SWJCFG_SFT		24

#define STM32_HSI_FREQ_HZ				(8 * 1000 * 1000)

#define STM32_UID_ADDR					0x1FFFF7E8
#define STM32_UID_SIZE					12

static struct stm32_info_t stm32_info = 
{
	CORE_CLKSRC, CORE_PLLSRC, CORE_RTCSRC, CORE_HSE_TYPE, OSC0_FREQ_HZ, 
	CORE_PLL_FREQ_HZ, CORE_AHB_FREQ_HZ, CORE_APB1_FREQ_HZ, CORE_APB2_FREQ_HZ, 
	CORE_FLASH_LATENCY, CORE_VECTOR_TABLE, CORE_DEBUG
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
	
	RCC_DeInit();
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

static void (*stm32_tickclk_callback)(void *param) = NULL;
static void *stm32_tickclk_param = NULL;
vsf_err_t stm32_tickclk_start(void)
{
	TIM_Cmd(TIM1, ENABLE);
	return VSFERR_NONE;
}

vsf_err_t stm32_tickclk_stop(void)
{
	TIM_Cmd(TIM1, DISABLE);
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
	TIM_ClearITPendingBit(TIM1, TIM_FLAG_Update);
}

vsf_err_t stm32_tickclk_set_callback(void (*callback)(void*), void *param)
{
	NVIC_InitTypeDef NVIC_InitStructure;
	
#if defined(STM32F10X_XL)
	NVIC_InitStructure.NVIC_IRQChannel = TIM1_UP_TIM10_IRQn;
#elif defined(STM32F10X_MD_VL) || defined(STM32F10X_LD_VL) || defined(STM32F10X_MD_VL)
	NVIC_InitStructure.NVIC_IRQChannel = TIM1_UP_TIM16_IRQn;
#elif defined(STM32F10X_LD) || defined(STM32F10X_MD) || defined(STM32F10X_HD) || defined(STM32F10X_CL)
	NVIC_InitStructure.NVIC_IRQChannel = TIM1_UP_IRQn;
#endif
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 0;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 0;
	
	if (callback != NULL)
	{
		// enable interrupt
		TIM_ITConfig(TIM1, TIM_IT_Update, ENABLE);
		NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
		stm32_tickclk_callback = callback;
		stm32_tickclk_param = param;
		NVIC_Init(&NVIC_InitStructure);
	}
	else
	{
		// disable interrupt
		TIM_ITConfig(TIM1, TIM_IT_Update, DISABLE);
		NVIC_InitStructure.NVIC_IRQChannelCmd = DISABLE;
		NVIC_Init(&NVIC_InitStructure);
		stm32_tickclk_callback = callback;
		stm32_tickclk_param = param;
	}
	return VSFERR_NONE;
}

vsf_err_t stm32_tickclk_init(void)
{
	TIM_TimeBaseInitTypeDef TIM_TimeBaseStructure;
	
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_TIM1, ENABLE);
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM2, ENABLE);
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM5, ENABLE);
	TIM_DeInit(TIM1);
	TIM_DeInit(TIM2);
	TIM_DeInit(TIM5);
	
	// TIM1 generate 1ms event
	TIM_TimeBaseStructInit(&TIM_TimeBaseStructure);
	TIM_TimeBaseStructure.TIM_Prescaler = 1;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseStructure.TIM_Period = stm32_info.apb2_freq_hz / 2 / 1000;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;
	TIM_TimeBaseInit(TIM1, &TIM_TimeBaseStructure);
	TIM_SelectOutputTrigger(TIM1, TIM_TRGOSource_Update);
	TIM_SelectMasterSlaveMode(TIM1, TIM_MasterSlaveMode_Enable);
	
	// TIM2 accept 1ms clock from TIM1
	TIM_TimeBaseStructInit(&TIM_TimeBaseStructure);
	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseStructure.TIM_Period = 0xFFFF;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;
	TIM_TimeBaseInit(TIM2, &TIM_TimeBaseStructure);
	TIM_SelectSlaveMode(TIM2, TIM_SlaveMode_External1);
	TIM_ITRxExternalClockConfig(TIM2, TIM_TS_ITR0);
	TIM_SelectOutputTrigger(TIM2, TIM_TRGOSource_Update);
	TIM_SelectMasterSlaveMode(TIM2, TIM_MasterSlaveMode_Enable);
	TIM_Cmd(TIM2, ENABLE);
	
	// TIM5 accept 65536ms clock from TIM2
	TIM_TimeBaseStructInit(&TIM_TimeBaseStructure);
	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseStructure.TIM_Period = 0xFFFF;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;
	TIM_TimeBaseInit(TIM5, &TIM_TimeBaseStructure);
	TIM_SelectSlaveMode(TIM5, TIM_SlaveMode_External1);
	TIM_ITRxExternalClockConfig(TIM5, TIM_TS_ITR0);
	TIM_Cmd(TIM5, ENABLE);
	
	return VSFERR_NONE;
}

vsf_err_t stm32_tickclk_fini(void)
{
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_TIM1, DISABLE);
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM2, DISABLE);
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM5, DISABLE);
	return VSFERR_NONE;
}
