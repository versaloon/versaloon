#include "vsf_err.h"

#include "app_cfg.h"
#include "app_type.h"

#include "interfaces_cfg.h"
#include "interfaces_const.h"
#include "interfaces.h"
#include "core.h"

static struct stm32f4_info_t stm32f4_info = 
{
	0, CORE_VECTOR_TABLE, CORE_CLKSRC, CORE_PLLSRC, CORE_RTCSRC, CORE_HSE_TYPE,
	OSC0_FREQ_HZ, CORE_PLL_FREQ_HZ, CORE_AHB_FREQ_HZ, CORE_APB1_FREQ_HZ,
	CORE_APB2_FREQ_HZ, CORE_FLASH_LATENCY, CORE_DEBUG
};

vsf_err_t stm32f4_interface_get_info(struct stm32f4_info_t **info)
{
	*info = &stm32f4_info;
	return VSFERR_NONE;
}

vsf_err_t stm32f4_interface_fini(void *p)
{
	return VSFERR_NONE;
}

vsf_err_t stm32f4_interface_reset(void *p)
{
	NVIC_SystemReset();
	return VSFERR_NONE;
}

vsf_err_t stm32f4_interface_set_stack(uint32_t sp)
{
	__set_MSP(sp);
	return VSFERR_NONE;
}

// sleep will enable interrupt
// for cortex processor, if an interrupt occur between enable the interrupt
// 		and __WFI, wfi will not make the core sleep
void stm32f4_interface_sleep(uint32_t mode)
{
	vsf_leave_critical();
	__WFI();
}

vsf_err_t stm32f4_interface_init(void *p)
{
	SCB->VTOR = stm32f4_info.vector_table;
	SCB->AIRCR = 0x05FA0000 | stm32f4_info.priority_group;
	return VSFERR_NONE;
}

uint32_t stm32f4_uid_get(uint8_t *buffer, uint32_t size)
{
	return 0;
}

#define CM3_SYSTICK_ENABLE				(1 << 0)
#define CM3_SYSTICK_CLKSOURCE			(1 << 2)
#define CM3_SYSTICK_COUNTFLAG			(1 << 16)

vsf_err_t stm32f4_delay_init(void)
{
	SysTick->CTRL = CM3_SYSTICK_CLKSOURCE;
	SysTick->VAL = 0;
	return VSFERR_NONE;
}

static vsf_err_t stm32f4_delay_delayus_do(uint32_t tick)
{
	uint32_t dly_tmp;
	
	stm32f4_delay_init();
	while (tick)
	{
		dly_tmp = (tick > ((1 << 24) - 1)) ? ((1 << 24) - 1) : tick;
		SysTick->LOAD = dly_tmp;
		SysTick->CTRL |= CM3_SYSTICK_ENABLE;
		while (!(SysTick->CTRL & CM3_SYSTICK_COUNTFLAG));
		stm32f4_delay_init();
		tick -= dly_tmp;
	}
	return VSFERR_NONE;
}

vsf_err_t stm32f4_delay_delayus(uint16_t us)
{
	stm32f4_delay_delayus_do(us * (stm32f4_info.sys_freq_hz / (1000 * 1000)));
	return VSFERR_NONE;
}

vsf_err_t stm32f4_delay_delayms(uint16_t ms)
{
	stm32f4_delay_delayus_do(ms * (stm32f4_info.sys_freq_hz / 1000));
	return VSFERR_NONE;
}

// tickclk
#define TICKCLK_TIM							TIM5

static void (*stm32f4_tickclk_callback)(void *param) = NULL;
static void *stm32f4_tickclk_param = NULL;
static uint32_t stm32f4_tickcnt = 0;
vsf_err_t stm32f4_tickclk_start(void)
{
	TICKCLK_TIM->CR1 |= TIM_CR1_CEN;
	return VSFERR_NONE;
}

vsf_err_t stm32f4_tickclk_stop(void)
{
	TICKCLK_TIM->CR1 &= ~TIM_CR1_CEN;
	return VSFERR_NONE;
}

static uint32_t stm32f4_tickclk_get_count_local(void)
{
	return stm32f4_tickcnt;
}

uint32_t stm32f4_tickclk_get_count(void)
{
	uint32_t count1, count2;
	
	do {
		count1 = stm32f4_tickclk_get_count_local();
		count2 = stm32f4_tickclk_get_count_local();
	} while (count1 != count2);
	return count1;
}

ROOTFUNC void TIM5_IRQHandler(void)
{
	stm32f4_tickcnt++;
	if (stm32f4_tickclk_callback != NULL)
	{
		stm32f4_tickclk_callback(stm32f4_tickclk_param);
	}
	TICKCLK_TIM->SR = ~TIM_SR_UIF;
}

vsf_err_t stm32f4_tickclk_set_callback(void (*callback)(void*), void *param)
{
	TICKCLK_TIM->DIER &= ~TIM_DIER_UIE;
	stm32f4_tickclk_callback = callback;
	stm32f4_tickclk_param = param;
	TICKCLK_TIM->DIER |= TIM_DIER_UIE;
	return VSFERR_NONE;
}

vsf_err_t stm32f4_tickclk_init(void)
{
	stm32f4_tickcnt = 0;
	RCC->APB1ENR |= RCC_APB1ENR_TIM5EN;
	RCC->APB1RSTR |= RCC_APB1RSTR_TIM5RST;
	RCC->APB1RSTR &= ~RCC_APB1RSTR_TIM5RST;
	
	// TIM5 generate 1ms event
	TICKCLK_TIM->CR1 &= 0x03FF;
	TICKCLK_TIM->ARR = stm32f4_info.apb1_freq_hz / 2 / 1000;
	TICKCLK_TIM->PSC = 1;
	TICKCLK_TIM->RCR = 0;
	TICKCLK_TIM->EGR |= TIM_EGR_UG;
	TICKCLK_TIM->DIER |= TIM_DIER_UIE;
	NVIC->IP[TIM5_IRQn] = 0xFF;
	NVIC->ISER[TIM5_IRQn >> 0x05] = 1UL << (TIM5_IRQn & 0x1F);
	
	return VSFERR_NONE;
}

vsf_err_t stm32f4_tickclk_fini(void)
{
	RCC->APB1ENR &= ~RCC_APB1ENR_TIM5EN;
	return VSFERR_NONE;
}
