#include "vsf_err.h"

#include "app_cfg.h"
#include "app_type.h"

#include "interfaces_cfg.h"
#include "interfaces_const.h"
#include "core.h"

#include "NUC472_442.h"

#define CORE_SYSTICK_TIMER					TIMER0

#define NUC400_CLK_PLLCTL_NR(x)				(((x) - 2) << 9)
#define NUC400_CLK_PLLCTL_NF(x)				(((x) - 2) << 0)

#define NUC400_CLK_PLLCTL_NO_1				(0x0UL << CLK_PLLCTL_OUTDV_Pos)
#define NUC400_CLK_PLLCTL_NO_2				(0x1UL << CLK_PLLCTL_OUTDV_Pos)
#define NUC400_CLK_PLLCTL_NO_4				(0x3UL << CLK_PLLCTL_OUTDV_Pos)

#define NUC400_CLK_CLKSEL0_HCLKSEL_HXT		(0x00UL << CLK_CLKSEL0_HCLKSEL_Pos)
#define NUC400_CLK_CLKSEL0_HCLKSEL_LXT		(0x01UL << CLK_CLKSEL0_HCLKSEL_Pos)
#define NUC400_CLK_CLKSEL0_HCLKSEL_PLL		(0x02UL << CLK_CLKSEL0_HCLKSEL_Pos)
#define NUC400_CLK_CLKSEL0_HCLKSEL_LIRC		(0x03UL << CLK_CLKSEL0_HCLKSEL_Pos)
#define NUC400_CLK_CLKSEL0_HCLKSEL_USBPLL	(0x04UL << CLK_CLKSEL0_HCLKSEL_Pos)
#define NUC400_CLK_CLKSEL0_HCLKSEL_HIRC		(0x07UL << CLK_CLKSEL0_HCLKSEL_Pos)

#define NUC400_CLK_CLKSEL1_TIM0SEL_HXT		(0x00UL << CLK_CLKSEL1_TMR0SEL_Pos)
#define NUC400_CLK_CLKSEL1_TIM0SEL_LXT		(0x01UL << CLK_CLKSEL1_TMR0SEL_Pos)
#define NUC400_CLK_CLKSEL1_TIM0SEL_PCLK		(0x02UL << CLK_CLKSEL1_TMR0SEL_Pos)
#define NUC400_CLK_CLKSEL1_TIM0SEL_EXTTRG	(0x03UL << CLK_CLKSEL1_TMR0SEL_Pos)
#define NUC400_CLK_CLKSEL1_TIM0SEL_LIRC		(0x05UL << CLK_CLKSEL1_TMR0SEL_Pos)
#define NUC400_CLK_CLKSEL1_TIM0SEL_HIRC		(0x07UL << CLK_CLKSEL1_TMR0SEL_Pos)

#define NUC400_TCSR_MODE_ONESHOT			(0x00UL << TIMER_CTL_OPMODE_Pos)
#define NUC400_TCSR_MODE_PERIODIC			(0x01UL << TIMER_CTL_OPMODE_Pos)
#define NUC400_TCSR_MODE_TOGGLE				(0x02UL << TIMER_CTL_OPMODE_Pos)
#define NUC400_TCSR_MODE_COUNTINUOUS		(0x03UL << TIMER_CTL_OPMODE_Pos)

static struct nuc400_info_t nuc400_info =
{
	0, CORE_VECTOR_TABLE, CORE_CLKEN, CORE_HCLKSRC, CORE_PCLKSRC, CORE_PLLSRC,
	OSC0_FREQ_HZ, OSC32_FREQ_HZ, CORE_PLL_FREQ_HZ, CPU_FREQ_HZ, HCLK_FREQ_HZ,
	PCLK_FREQ_HZ,
};

vsf_err_t nuc400_interface_get_info(struct nuc400_info_t **info)
{
	*info = &nuc400_info;
	return VSFERR_NONE;
}

vsf_err_t nuc400_interface_fini(void *p)
{
	return VSFERR_NONE;
}

vsf_err_t nuc400_interface_reset(void *p)
{
	return VSFERR_NONE;
}

vsf_err_t nuc400_interface_set_stack(uint32_t sp)
{
	__set_MSP(sp);
	return VSFERR_NONE;
}

// sleep will enable interrupt
// for cortex processor, if an interrupt occur between enable the interrupt
// 		and __WFI, wfi will not make the core sleep
void nuc400_interface_sleep(uint32_t mode)
{
	vsf_leave_critical();
	__WFI();
}

void nuc400_unlock_reg(void)
{
	while(SYS->REGLCTL != SYS_REGLCTL_REGLCTL_Msk) {
        SYS->REGLCTL = 0x59;
        SYS->REGLCTL = 0x16;
        SYS->REGLCTL = 0x88;
    }
}

void nuc400_lock_reg(void)
{
	SYS->REGLCTL = 0;
}

vsf_err_t nuc400_interface_init(void *p)
{
	uint32_t temp32;
	uint32_t freq_in;
	
	if (p != NULL)
	{
		nuc400_info = *(struct nuc400_info_t *)p;
	}
	nuc400_info.hirc_freq_hz = 22 * 1000 * 1000;
	nuc400_info.lirc_freq_hz = 10 * 1000;
	
	nuc400_unlock_reg();
	
	// enable clocks
	CLK->PWRCTL |= nuc400_info.clk_enable;
	temp32 = nuc400_info.clk_enable;
	temp32 = ((temp32 & NUC400_CLK_HXT) ? CLK_STATUS_HXTSTB_Msk : 0) |
				((temp32 & NUC400_CLK_LXT) ? CLK_STATUS_LXTSTB_Msk : 0) |
				((temp32 & NUC400_CLK_HIRC) ? CLK_STATUS_HIRCSTB_Msk : 0) |
				((temp32 & NUC400_CLK_LIRC) ? CLK_STATUS_LIRCSTB_Msk : 0);
	while ((CLK->STATUS & temp32) != temp32);
	
	// enable PLLs
	if (nuc400_info.pllsrc != NUC400_PLLSRC_NONE)
	{
		uint8_t no;
		uint32_t no_mask;

		switch (nuc400_info.pllsrc)
		{
		case NUC400_PLLSRC_HXT:
			temp32 = 0;
			freq_in = nuc400_info.osc_freq_hz;
			break;
		case NUC400_PLLSRC_HIRC:
			temp32 = CLK_PLLCTL_PLLSRC_Pos;
			freq_in = nuc400_info.hirc_freq_hz;
			break;
		default:
			return VSFERR_INVALID_PARAMETER;
		}
		// Fin/NR: 2MHz
		if ((nuc400_info.pll_freq_hz * 1 > (120 * 1000 * 1000)) &&
				(nuc400_info.pll_freq_hz * 1 < (200 * 1000 * 1000)))
		{
			no = 1;
			no_mask = NUC400_CLK_PLLCTL_NO_1;
		}
		else if ((nuc400_info.pll_freq_hz * 2 > (120 * 1000 * 1000)) &&
				(nuc400_info.pll_freq_hz * 2 < (200 * 1000 * 1000)))
		{
			no = 2;
			no_mask = NUC400_CLK_PLLCTL_NO_2;
		}
		else if ((nuc400_info.pll_freq_hz * 4 > (120 * 1000 * 1000)) &&
				(nuc400_info.pll_freq_hz * 4 < (200 * 1000 * 1000)))
		{
			no = 4;
			no_mask = NUC400_CLK_PLLCTL_NO_4;
		}
		else
		{
			return VSFERR_INVALID_PARAMETER;
		}
		
		CLK->PLLCTL = temp32 | no_mask |
			NUC400_CLK_PLLCTL_NR(freq_in / 2000000) |
			NUC400_CLK_PLLCTL_NF(nuc400_info.pll_freq_hz * no / 2000000);
		while ((CLK->STATUS & CLK_STATUS_PLLSTB_Msk) != CLK_STATUS_PLLSTB_Msk);
	}
	
	// set hclk
	switch (nuc400_info.hclksrc)
	{
	case NUC400_HCLKSRC_HIRC:
		freq_in = nuc400_info.hirc_freq_hz;
		temp32 = NUC400_CLK_CLKSEL0_HCLKSEL_HIRC;
		break;
	case NUC400_HCLKSRC_PLL2FOUT:
		freq_in = 480 * 1000 * 1000;
		temp32 = NUC400_CLK_CLKSEL0_HCLKSEL_USBPLL;
		break;
	case NUC400_HCLKSRC_LIRC:
		freq_in = nuc400_info.lirc_freq_hz;
		temp32 = NUC400_CLK_CLKSEL0_HCLKSEL_LIRC;
		break;
	case NUC400_HCLKSRC_PLLFOUT:
		freq_in = nuc400_info.pll_freq_hz;
		temp32 = NUC400_CLK_CLKSEL0_HCLKSEL_PLL;
		break;
	case NUC400_HCLKSRC_LXT:
		freq_in = nuc400_info.osc32k_freq_hz;
		temp32 = NUC400_CLK_CLKSEL0_HCLKSEL_LXT;
		break;
	case NUC400_HCLKSRC_HXT:
		freq_in = nuc400_info.osc_freq_hz;
		temp32 = NUC400_CLK_CLKSEL0_HCLKSEL_HXT;
		break;
	default:
		return VSFERR_INVALID_PARAMETER;
	}
	CLK->CLKSEL0 = (CLK->CLKSEL0 & ~CLK_CLKSEL0_HCLKSEL_Msk) | temp32;
	CLK->CLKDIV0 = (CLK->CLKDIV0 & ~CLK_CLKDIV0_HCLKDIV_Msk) |
			((freq_in / nuc400_info.hclk_freq_hz) - 1);
	
	nuc400_lock_reg();
	
	SCB->VTOR = nuc400_info.vector_table;
	SCB->AIRCR = 0x05FA0000 | nuc400_info.priority_group;
	return VSFERR_NONE;
}

uint32_t nuc400_uid_get(uint8_t *buffer, uint32_t size)
{
	return 0;
}

#define CM3_SYSTICK_ENABLE				(1 << 0)
#define CM3_SYSTICK_CLKSOURCE			(1 << 2)
#define CM3_SYSTICK_COUNTFLAG			(1 << 16)

vsf_err_t nuc400_delay_init(void)
{
	SysTick->CTRL = CM3_SYSTICK_CLKSOURCE;
	SysTick->VAL = 0;
	return VSFERR_NONE;
}

static vsf_err_t nuc400_delay_delayus_do(uint32_t tick)
{
	uint32_t dly_tmp;
	
	nuc400_delay_init();
	while (tick)
	{
		dly_tmp = (tick > ((1 << 24) - 1)) ? ((1 << 24) - 1) : tick;
		SysTick->LOAD = dly_tmp;
		SysTick->CTRL |= CM3_SYSTICK_ENABLE;
		while (!(SysTick->CTRL & CM3_SYSTICK_COUNTFLAG));
		nuc400_delay_init();
		tick -= dly_tmp;
	}
	return VSFERR_NONE;
}

vsf_err_t nuc400_delay_delayus(uint16_t us)
{
	nuc400_delay_delayus_do(us * (nuc400_info.cpu_freq_hz / (1000 * 1000)));
	return VSFERR_NONE;
}

vsf_err_t nuc400_delay_delayms(uint16_t ms)
{
	nuc400_delay_delayus_do(ms * (nuc400_info.cpu_freq_hz / 1000));
	return VSFERR_NONE;
}

static void (*nuc400_tickclk_callback)(void *param) = NULL;
static void *nuc400_tickclk_param = NULL;
static volatile uint32_t nuc400_tickcnt = 0;

void TMR0_IRQHandler(void)
{
	nuc400_tickcnt++;
	if (nuc400_tickclk_callback != NULL)
	{
		nuc400_tickclk_callback(nuc400_tickclk_param);
	}
	TIMER0->INTSTS = TIMER_INTSTS_TIF_Msk;
}

vsf_err_t nuc400_tickclk_start(void)
{
	CORE_SYSTICK_TIMER->CTL |= TIMER_CTL_CNTEN_Msk;
	return VSFERR_NONE;
}

vsf_err_t nuc400_tickclk_stop(void)
{
	CORE_SYSTICK_TIMER->CTL &= ~TIMER_CTL_CNTEN_Msk;
	return VSFERR_NONE;
}

static uint32_t nuc400_tickclk_get_count_local(void)
{
	return nuc400_tickcnt;
}

uint32_t nuc400_tickclk_get_count(void)
{
	uint32_t count1, count2;
	
	do {
		count1 = nuc400_tickclk_get_count_local();
		count2 = nuc400_tickclk_get_count_local();
	} while (count1 != count2);
	return count1;
}

vsf_err_t nuc400_tickclk_set_callback(void (*callback)(void*), void *param)
{
	CORE_SYSTICK_TIMER->CTL &= ~TIMER_CTL_INTEN_Msk;
	nuc400_tickclk_callback = callback;
	nuc400_tickclk_param = param;
	CORE_SYSTICK_TIMER->CTL |= TIMER_CTL_INTEN_Msk;
	return VSFERR_NONE;
}

vsf_err_t nuc400_tickclk_init(void)
{
	nuc400_tickcnt = 0;
	CLK->APBCLK0 |= CLK_APBCLK0_TMR0CKEN_Msk;
	CLK->CLKSEL1 &= ~CLK_CLKSEL1_TMR0SEL_Msk;
	CLK->CLKSEL1 |= NUC400_CLK_CLKSEL1_TIM0SEL_LIRC;
	CORE_SYSTICK_TIMER->CTL = TIMER_CTL_RSTCNT_Msk;
	CORE_SYSTICK_TIMER->CTL = TIMER_CTL_CNTDATEN_Msk | TIMER_CTL_INTEN_Msk |
								TIMER_CTL_WKEN_Msk | NUC400_TCSR_MODE_PERIODIC;
	CORE_SYSTICK_TIMER->CMP = nuc400_info.lirc_freq_hz / 1000;
	NVIC_EnableIRQ(TMR0_IRQn);
	return VSFERR_NONE;
}

vsf_err_t nuc400_tickclk_fini(void)
{
	CORE_SYSTICK_TIMER->CTL = 0;
	CLK->APBCLK0 &= ~CLK_APBCLK0_TMR0CKEN_Msk;
	return VSFERR_NONE;
}
