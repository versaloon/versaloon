#include "app_type.h"
#include "interfaces.h"

// TODO: remove MACROs below to stm32_reg.h
#define STM32_RCC_APB2ENR_IOPDEN		((uint32_t)1 << 5)
#define STM32_RCC_APB2ENR_IOPEEN		((uint32_t)1 << 6)
#define STM32_RCC_APB2ENR_IOPFEN		((uint32_t)1 << 7)
#define STM32_RCC_APB2ENR_IOPGEN		((uint32_t)1 << 8)

#define STM32_RCC_AHBENR_FSMC			((uint32_t)1 << 8)

#define STM32_FSMC_BCR_MBKEN			((uint32_t)1 << 0)
#define STM32_FSMC_BCR_MUXEN			((uint32_t)1 << 1)
#define STM32_FSMC_BCR_MTYP_SRAM		((uint32_t)0 << 2)
#define STM32_FSMC_BCR_MTYP_PSRAM		((uint32_t)1 << 2)
#define STM32_FSMC_BCR_MTYP_NOR			((uint32_t)2 << 2)
#define STM32_FSMC_BCR_MWID_8			((uint32_t)0 << 4)
#define STM32_FSMC_BCR_MWID_16			((uint32_t)1 << 4)
#define STM32_FSMC_BCR_FACCEN			((uint32_t)1 << 6)
#define STM32_FSMC_BCR_BURSTEN			((uint32_t)1 << 8)
#define STM32_FSMC_BCR_WAITPOL			((uint32_t)1 << 9)
#define STM32_FSMC_BCR_WRAPMOD			((uint32_t)1 << 10)
#define STM32_FSMC_BCR_WAITCFG			((uint32_t)1 << 11)
#define STM32_FSMC_BCR_WREN				((uint32_t)1 << 12)
#define STM32_FSMC_BCR_WAITEN			((uint32_t)1 << 13)
#define STM32_FSMC_BCR_EXTMOD			((uint32_t)1 << 14)
#define STM32_FSMC_BCR_ASYNCWAIT		((uint32_t)1 << 15)
#define STM32_FSMC_BCR_CBURSTRW			((uint32_t)1 << 19)

#define STM32_FSMC_BTR_ACCMOD_A			((uint32_t)0 << 28)
#define STM32_FSMC_BTR_ACCMOD_B			((uint32_t)1 << 28)
#define STM32_FSMC_BTR_ACCMOD_C			((uint32_t)2 << 28)
#define STM32_FSMC_BTR_ACCMOD_D			((uint32_t)3 << 28)

#define STM32_FSMC_PCR_ECCEN			((uint32_t)1 << 6)
#define STM32_FSMC_PCR_PWID_8			((uint32_t)0 << 4)
#define STM32_FSMC_PCR_PWID_16			((uint32_t)1 << 4)
#define STM32_FSMC_PCR_PTYP_NAND		((uint32_t)1 << 3)
#define STM32_FSMC_PCR_PTYP_PCCARD		((uint32_t)0 << 3)
#define STM32_FSMC_PCR_PBKEN			((uint32_t)1 << 2)
#define STM32_FSMC_PCR_PWAITEN			((uint32_t)1 << 1)
#define STM32_FSMC_PCR_ECCPS_256		((uint32_t)0 << 17)
#define STM32_FSMC_PCR_ECCPS_512		((uint32_t)1 << 17)
#define STM32_FSMC_PCR_ECCPS_1024		((uint32_t)2 << 17)
#define STM32_FSMC_PCR_ECCPS_2048		((uint32_t)3 << 17)
#define STM32_FSMC_PCR_ECCPS_4096		((uint32_t)4 << 17)
#define STM32_FSMC_PCR_ECCPS_8192		((uint32_t)5 << 17)

#if IFS_EBI_EN

#include "STM32_EBI.h"

// stm32_ebi connection
// A00 -- A05 : PF0  -- PF5
// A06 -- A09 : PF12 -- PF15
// A10 -- A15 : PG0  -- PG5
// A16 -- A18 : PD11 -- PD13
// A19 -- A22 : PE3  -- PE6
// A23        : PE2
// A24 -- A25 : PG13 -- PG14
// D00 -- D01 : PD14 -- PD15
// D02 -- D03 : PD0  -- PD1
// D04 -- D12 : PE7  -- PE15
// D13 -- D15 : PD8  -- PD10
// NOE        : PD4
// NWE        : PD5
// NWAIT      : PD6
// nor/psram
// CLK        : PD3
// NADV       : PB7
// NBL0       : PE0
// NBL1       : PE1
// NE1        : PD7
// NE2        : PG9
// NE3        : PG10
// NE4        : PG12
// nand
// NCE2       : PD7
// NCE3       : PG9
// INT2       : PG6
// INT3       : PG7
// ALE        : PD11
// CLE        : PD12
// pccard
// NIORD      : PF6
// NREG       : PF7
// NIOWR      : PF8
// CD         : PF9
// INTR       : PF10
// NIOS16     : PF11
// NCE4_1     : PG10
// NCE4_2     : PG11

vsf_err_t stm32_ebi_init(uint8_t index)
{
	// common signals
	// NOE, NWE, DATA, ADDRESS
	switch (index & 0x0F)
	{
	case 0:
		RCC->APB2ENR |= STM32_RCC_APB2ENR_IOPDEN | STM32_RCC_APB2ENR_IOPEEN | 
						STM32_RCC_APB2ENR_IOPFEN | STM32_RCC_APB2ENR_IOPGEN;
		RCC->AHBENR |= STM32_RCC_AHBENR_FSMC;
		
		// noe nwe
		GPIOD->CRL = (GPIOD->CRL & ~(0xFF << (4 * 4))) | 
							(uint32_t)0xBB << (4 * 4);
		
		// data gpio
		GPIOD->CRL = (GPIOD->CRL & ~(0xFF << (0 * 4))) | 
							(uint32_t)0xBB << (0 * 4);
		GPIOE->CRL = (GPIOE->CRL & ~(0x0F << (7 * 4))) | 
							(uint32_t)0x0B << (7 * 4);
		#if FSMC00_DATA_LEN == 8
		GPIOD->CRH = (GPIOD->CRH & ~(0xFF << ((14 - 8) * 4))) | 
							(uint32_t)0xBB << ((14 - 8) * 4);
		GPIOE->CRH = (GPIOE->CRH & ~(0xFFF << ((8 - 8) * 4))) | 
							(uint32_t)0xBBB << ((8 - 8) * 4);
		#elif FSMC00_DATA_LEN == 16
		GPIOD->CRH = (GPIOD->CRH & 0x00FFF000) | 0xBB000BBB;
		GPIOE->CRH = 0xBBBBBBBB;
		#else
		#error "FSMC00_DATA_LEN MUST be 8 or 16"
		#endif
		
		// address gpio
		GPIOF->CRL = (GPIOF->CRL & ~(0xFFFFFF << (0 * 4))) | 
							(uint32_t)0xBBBBBB << (0 * 4);
		GPIOF->CRH = (GPIOF->CRH & ~(0xFFFF << ((12 - 8) * 4))) | 
							(uint32_t)0xBBBB << ((12 - 8) * 4);
		GPIOG->CRL = (GPIOG->CRL & ~(0xFFFFFF << (0 * 4))) | 
							(uint32_t)0xBBBBBB << (0 * 4);
		#if FSMC00_ADDR_LEN < 16
		#error "FSMC00_ADDR_LEN MUST be >= 16"
		#elif FSMC00_ADDR_LEN < 17
		#elif FSMC00_ADDR_LEN < 18
		GPIOD->CRH = (GPIOD->CRH & ~(0x0F << ((11 - 8) * 4))) | 
							(uint32_t)0x0B << ((11 - 8) * 4);
		#elif FSMC00_ADDR_LEN < 19
		GPIOD->CRH = (GPIOD->CRH & ~(0xFF << ((11 - 8) * 4))) | 
							(uint32_t)0xBB << ((11 - 8) * 4);
		#elif FSMC00_ADDR_LEN < 20
		GPIOD->CRH = (GPIOD->CRH & ~(0xFFF << ((11 - 8) * 4))) | 
							(uint32_t)0xBBB << ((11 - 8) * 4);
		#elif FSMC00_ADDR_LEN < 21
		GPIOD->CRH = (GPIOD->CRH & ~(0xFFF << ((11 - 8) * 4))) | 
							(uint32_t)0xBBB << ((11 - 8) * 4);
		GPIOE->CRL = (GPIOE->CRL & ~(0x0F << (3 * 4))) | 
							(uint32_t)0x0B << (3 * 4);
		#elif FSMC00_ADDR_LEN < 22
		GPIOD->CRH = (GPIOD->CRH & ~(0xFFF << ((11 - 8) * 4))) | 
							(uint32_t)0xBBB << ((11 - 8) * 4);
		GPIOE->CRL = (GPIOE->CRL & ~(0xFF << (3 * 4))) | 
							(uint32_t)0xBB << (3 * 4);
		#elif FSMC00_ADDR_LEN < 23
		GPIOD->CRH = (GPIOD->CRH & ~(0xFFF << ((11 - 8) * 4))) | 
							(uint32_t)0xBBB << ((11 - 8) * 4);
		GPIOE->CRL = (GPIOE->CRL & ~(0xFFF << (3 * 4))) | 
							(uint32_t)0xBBB << (3 * 4);
		#elif FSMC00_ADDR_LEN < 24
		GPIOD->CRH = (GPIOD->CRH & ~(0xFFF << ((11 - 8) * 4))) | 
							(uint32_t)0xBBB << ((11 - 8) * 4);
		GPIOE->CRL = (GPIOE->CRL & ~(0xFFFF << (3 * 4))) | 
							(uint32_t)0xBBBB << (3 * 4);
		#elif FSMC00_ADDR_LEN < 25
		GPIOD->CRH = (GPIOD->CRH & ~(0xFFF << ((11 - 8) * 4))) | 
							(uint32_t)0xBBB << ((11 - 8) * 4);
		GPIOE->CRL = (GPIOE->CRL & ~(0xFFFFF << (2 * 4))) | 
							(uint32_t)0xBBBBB << (2 * 4);
		#elif FSMC00_ADDR_LEN < 26
		GPIOD->CRH = (GPIOD->CRH & ~(0xFFF << ((11 - 8) * 4))) | 
							(uint32_t)0xBBB << ((11 - 8) * 4);
		GPIOE->CRL = (GPIOE->CRL & ~(0xFFFFF << (2 * 4))) | 
							(uint32_t)0xBBBBB << (2 * 4);
		GPIOG->CRH = (GPIOG->CRH & ~(0x0F << ((13 - 8) * 4))) | 
							(uint32_t)0x0B << ((13 - 8) * 4);
		#elif FSMC00_ADDR_LEN < 27
		GPIOD->CRH = (GPIOD->CRH & ~(0xFFF << ((11 - 8) * 4))) | 
							(uint32_t)0xBBB << ((11 - 8) * 4);
		GPIOE->CRL = (GPIOE->CRL & ~(0xFFFFF << (2 * 4))) | 
							(uint32_t)0xBBBBB << (2 * 4);
		GPIOG->CRH = (GPIOG->CRH & ~(0xFF << ((13 - 8) * 4))) | 
							(uint32_t)0xBB << ((13 - 8) * 4);
		#else
		#error "FSMC00_ADDR_LEN MUST be < 27"
		#endif
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t stm32_ebi_fini(uint8_t index)
{
	switch (index & 0x0F)
	{
	case 0:
		// noe nwe
		GPIOD->CRL = (GPIOD->CRL & ~(0xFF << (4 * 4))) | 
							(uint32_t)0x44 << (4 * 4);
		
		// data gpio
		GPIOD->CRL = (GPIOD->CRL & ~(0xFF << (0 * 4))) | 
							(uint32_t)0x44 << (0 * 4);
		GPIOE->CRL = (GPIOE->CRL & ~(0x0F << (7 * 4))) | 
							(uint32_t)0x04 << (7 * 4);
		#if FSMC00_DATA_LEN == 8
		GPIOD->CRH = (GPIOD->CRH & ~(0xFF << ((14 - 8) * 4))) | 
							(uint32_t)0x44 << ((14 - 8) * 4);
		GPIOE->CRH = (GPIOE->CRH & ~(0xFFF << ((8 - 8) * 4))) | 
							(uint32_t)0x444 << ((8 - 8) * 4);
		#elif FSMC00_DATA_LEN == 16
		GPIOD->CRH = (GPIOD->CRH & 0x00FFF000) | 0x44000444;
		GPIOE->CRH = 0x44444444;
		#else
		#error "FSMC00_DATA_LEN MUST be 8 or 16"
		#endif
		
		// address gpio
		GPIOF->CRL = (GPIOF->CRL & ~(0xFFFFFF << (0 * 4))) | 
							(uint32_t)0x444444 << (0 * 4);
		GPIOF->CRH = (GPIOF->CRH & ~(0xFFFF << ((12 - 8) * 4))) | 
							(uint32_t)0x4444 << ((12 - 8) * 4);
		GPIOG->CRL = (GPIOG->CRL & ~(0xFFFFFF << (0 * 4))) | 
							(uint32_t)0x444444 << (0 * 4);
		#if FSMC00_ADDR_LEN < 16
		#error "FSMC00_ADDR_LEN MUST be >= 16"
		#elif FSMC00_ADDR_LEN < 17
		#elif FSMC00_ADDR_LEN < 18
		GPIOD->CRH = (GPIOD->CRH & ~(0x0F << ((11 - 8) * 4))) | 
							(uint32_t)0x04 << ((11 - 8) * 4);
		#elif FSMC00_ADDR_LEN < 19
		GPIOD->CRH = (GPIOD->CRH & ~(0xFF << ((11 - 8) * 4))) | 
							(uint32_t)0x44 << ((11 - 8) * 4);
		#elif FSMC00_ADDR_LEN < 20
		GPIOD->CRH = (GPIOD->CRH & ~(0xFFF << ((11 - 8) * 4))) | 
							(uint32_t)0x444 << ((11 - 8) * 4);
		#elif FSMC00_ADDR_LEN < 21
		GPIOD->CRH = (GPIOD->CRH & ~(0xFFF << ((11 - 8) * 4))) | 
							(uint32_t)0x444 << ((11 - 8) * 4);
		GPIOE->CRL = (GPIOE->CRL & ~(0x0F << (3 * 4))) | 
							(uint32_t)0x04 << (3 * 4);
		#elif FSMC00_ADDR_LEN < 22
		GPIOD->CRH = (GPIOD->CRH & ~(0xFFF << ((11 - 8) * 4))) | 
							(uint32_t)0x444 << ((11 - 8) * 4);
		GPIOE->CRL = (GPIOE->CRL & ~(0xFF << (3 * 4))) | 
							(uint32_t)0x44 << (3 * 4);
		#elif FSMC00_ADDR_LEN < 23
		GPIOD->CRH = (GPIOD->CRH & ~(0xFFF << ((11 - 8) * 4))) | 
							(uint32_t)0x444 << ((11 - 8) * 4);
		GPIOE->CRL = (GPIOE->CRL & ~(0xFFF << (3 * 4))) | 
							(uint32_t)0x444 << (3 * 4);
		#elif FSMC00_ADDR_LEN < 24
		GPIOD->CRH = (GPIOD->CRH & ~(0xFFF << ((11 - 8) * 4))) | 
							(uint32_t)0x444 << ((11 - 8) * 4);
		GPIOE->CRL = (GPIOE->CRL & ~(0xFFFF << (3 * 4))) | 
							(uint32_t)0x4444 << (3 * 4);
		#elif FSMC00_ADDR_LEN < 25
		GPIOD->CRH = (GPIOD->CRH & ~(0xFFF << ((11 - 8) * 4))) | 
							(uint32_t)0x444 << ((11 - 8) * 4);
		GPIOE->CRL = (GPIOE->CRL & ~(0xFFFFF << (2 * 4))) | 
							(uint32_t)0x44444 << (2 * 4);
		#elif FSMC00_ADDR_LEN < 26
		GPIOD->CRH = (GPIOD->CRH & ~(0xFFF << ((11 - 8) * 4))) | 
							(uint32_t)0x444 << ((11 - 8) * 4);
		GPIOE->CRL = (GPIOE->CRL & ~(0xFFFFF << (2 * 4))) | 
							(uint32_t)0x44444 << (2 * 4);
		GPIOG->CRH = (GPIOG->CRH & ~(0x0F << ((13 - 8) * 4))) | 
							(uint32_t)0x04 << ((13 - 8) * 4);
		#elif FSMC00_ADDR_LEN < 27
		GPIOD->CRH = (GPIOD->CRH & ~(0xFFF << ((11 - 8) * 4))) | 
							(uint32_t)0x444 << ((11 - 8) * 4);
		GPIOE->CRL = (GPIOE->CRL & ~(0xFFFFF << (2 * 4))) | 
							(uint32_t)0x44444 << (2 * 4);
		GPIOG->CRH = (GPIOG->CRH & ~(0xFF << ((13 - 8) * 4))) | 
							(uint32_t)0x44 << ((13 - 8) * 4);
		#else
		#error "FSMC00_ADDR_LEN MUST be < 27"
		#endif
		
		RCC->AHBENR &= ~STM32_RCC_AHBENR_FSMC;
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

static vsf_err_t stm32_ebi_config_sram_psram_nor(uint8_t index,
										uint32_t control, uint32_t timing,
										struct ebi_sram_psram_nor_info_t *info)
{
	uint8_t target_index = (index >> 6) & 0x0F;
	uint32_t bcr = control | STM32_FSMC_BCR_WREN | STM32_FSMC_BCR_MBKEN;
	uint32_t btr = timing, bwtr = timing;
	uint8_t clkdiv;
	struct stm32_info_t *cpu_info;
	
#if __VSF_DEBUG__
	if ((NULL == info) || 
		(info->param.address_setup_cycle_r < 1) || 
		(info->param.address_setup_cycle_r > 16) || 
		(info->param.address_hold_cycle_r < 2) || 
		(info->param.address_hold_cycle_r > 16) || 
		(info->param.data_setup_cycle_r < 2) || 
		(info->param.data_setup_cycle_r > 256) || 
		(info->param.address_setup_cycle_w < 1) || 
		(info->param.address_setup_cycle_w > 16) || 
		(info->param.address_hold_cycle_w < 2) || 
		(info->param.address_hold_cycle_w > 16) || 
		(info->param.data_setup_cycle_w < 2) || 
		(info->param.data_setup_cycle_w > 256))
	{
		return VSFERR_INVALID_RANGE;
	}
#endif
	if (stm32_interface_get_info(&cpu_info))
	{
		return VSFERR_FAIL;
	}
	
	switch (index & 0x0F)
	{
	case 0:
		switch (target_index)
		{
		case 0:
			GPIOD->CRL = (GPIOD->CRL & ~(0x0F << (7 * 4))) | 
							(uint32_t)stm32_GPIO_AFPP << (7 * 4);
			break;
		case 1:
			GPIOG->CRH = (GPIOG->CRH & ~(0x0F << ((9 - 8) * 4))) | 
							(uint32_t)stm32_GPIO_AFPP << ((9 - 8) * 4);
			break;
		case 2:
			GPIOG->CRH = (GPIOG->CRH & ~(0x0F << ((10 - 8) * 4))) | 
							(uint32_t)stm32_GPIO_AFPP << ((10 - 8) * 4);
			break;
		case 3:
			GPIOG->CRH = (GPIOG->CRH & ~(0x0F << ((12 - 8) * 4))) | 
							(uint32_t)stm32_GPIO_AFPP << ((12 - 8) * 4);
			break;
		default:
			return VSFERR_NOT_SUPPORT;
		}
		
		if (info->param.timing.clock_hz_r)
		{
			GPIOB->CRL = (GPIOB->CRL & ~(0x0F << (7 * 4))) | 
							(uint32_t)stm32_GPIO_AFPP << (7 * 4);
		}
		
		if (info->param.addr_multiplex)
		{
			GPIOD->CRL = (GPIOD->CRL & ~(0x0F << (3 * 4))) | 
							(uint32_t)stm32_GPIO_AFPP << (3 * 4);
			bcr |= STM32_FSMC_BCR_MUXEN;
		}
		
		if (info->common_info.wait_signal != EBI_WAIT_NONE)
		{
			GPIOD->CRL = (GPIOD->CRL & ~(0x0F << (6 * 4))) | 
							(uint32_t)stm32_GPIO_INP << (6 * 4);
			GPIOD->BSRR = (uint32_t)1 << 6;
			
			switch (info->common_info.wait_signal)
			{
			case EBI_WAIT_POLHIGH_VI:
				bcr |= STM32_FSMC_BCR_WAITEN | STM32_FSMC_BCR_WAITPOL | 
						STM32_FSMC_BCR_WAITCFG;
				break;
			case EBI_WAIT_POLHIGH_VN:
				bcr |= STM32_FSMC_BCR_WAITEN | STM32_FSMC_BCR_WAITPOL;
				break;
			case EBI_WAIT_POLLOW_VI:
				bcr |= STM32_FSMC_BCR_WAITEN | STM32_FSMC_BCR_WAITCFG;
				break;
			case EBI_WAIT_POLLOW_VN:
				bcr |= STM32_FSMC_BCR_WAITEN;
				break;
			default:
				return VSFERR_INVALID_PARAMETER;
			}
		}
		
		if (8 == info->common_info.data_width)
		{
			bcr |= STM32_FSMC_BCR_MWID_8;
		}
		else if (16 == info->common_info.data_width)
		{
			bcr |= STM32_FSMC_BCR_MWID_16;
		}
		else
		{
			return VSFERR_INVALID_PARAMETER;
		}
		
		if (info->param.timing.clock_hz_r)
		{
			clkdiv = cpu_info->ahb_freq_hz / info->param.timing.clock_hz_r;
			if ((clkdiv < 2) || (clkdiv > 16))
			{
				return VSFERR_INVALID_RANGE;
			}
			clkdiv--;
		}
		else
		{
			clkdiv = 0;
		}
		btr |= (((info->param.timing.address_setup_cycle_r - 1) & 0x0F) << 0) | 
				(((info->param.timing.address_hold_cycle_r - 1) & 0x0F) << 4) | 
				(((info->param.timing.data_setup_cycle_r - 1) & 0xFF) << 8) | 
				((clkdiv & 0x0F) << 20);
		
		if ((info->param.timing.clock_hz_w != info->param.timing.clock_hz_r) || 
			(info->param.timing.address_setup_cycle_w != 
								info->param.timing.address_setup_cycle_r) || 
			(info->param.timing.address_hold_cycle_w != 
	 							info->param.timing.address_hold_cycle_r) || 
			(info->param.timing.data_setup_cycle_w != 
	 							info->param.timing.data_setup_cycle_r))
		{
			bcr |= STM32_FSMC_BCR_EXTMOD;
			if (info->param.timing.clock_hz_w)
			{
				clkdiv = cpu_info->ahb_freq_hz / info->param.timing.clock_hz_w;
			}
			else
			{
				clkdiv = 0;
			}
			bwtr |= (((info->param.timing.address_setup_cycle_w - 1) & 0x0F) << 0) | 
					(((info->param.timing.address_hold_cycle_w - 1) & 0x0F) << 4) | 
					(((info->param.timing.data_setup_cycle_w - 1) & 0xFF) << 8) | 
					((clkdiv & 0x0F) << 20);
		}
		else
		{
			bwtr = 0x0FFFFFFF;
		}
		
		FSMC_Bank1->BTCR[(target_index << 1) + 1] = btr;
		FSMC_Bank1E->BWTR[(target_index << 1) + 0] = bwtr;
		FSMC_Bank1->BTCR[(target_index << 1) + 0] = bcr;
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t stm32_ebi_config_nor(uint8_t index,
								struct ebi_sram_psram_nor_info_t *info)
{
	return stm32_ebi_config_sram_psram_nor(index,
				STM32_FSMC_BCR_MTYP_NOR | STM32_FSMC_BCR_FACCEN,
				STM32_FSMC_BTR_ACCMOD_B, info);
}

vsf_err_t stm32_ebi_config_sram(uint8_t index,
								struct ebi_sram_psram_nor_info_t *info)
{
	GPIOE->CRL = (GPIOE->CRL & ~(0xFF << (0 * 4))) | 
							(uint32_t)0xBB << (0 * 4);
	return stm32_ebi_config_sram_psram_nor(index,
				STM32_FSMC_BCR_MTYP_SRAM,
				STM32_FSMC_BTR_ACCMOD_B, info);
}

vsf_err_t stm32_ebi_config_psram(uint8_t index,
									struct ebi_sram_psram_nor_info_t *info)
{
	GPIOE->CRL = (GPIOE->CRL & ~(0xFF << (0 * 4))) | 
							(uint32_t)0xBB << (0 * 4);
	return stm32_ebi_config_sram_psram_nor(index,
				STM32_FSMC_BCR_MTYP_PSRAM,
				STM32_FSMC_BTR_ACCMOD_B, info);
}

vsf_err_t stm32_ebi_config_nand(uint8_t index, struct ebi_nand_info_t *info)
{
	uint8_t bank_index = (index >> 4) & 0x03;
	uint32_t pcr = STM32_FSMC_PCR_PTYP_NAND | STM32_FSMC_PCR_PBKEN;
	uint32_t pmem = 0, patt = 0;
	
#if __VSF_DEBUG__
	if ((NULL == info) || 
		(info->param.timing.ale_to_re_cycle < 1) || 
		(info->param.timing.ale_to_re_cycle > 16) || 
		(info->param.timing.cle_to_re_cycle < 1) || 
		(info->param.timing.cle_to_re_cycle > 16))
	{
		return VSFERR_INVALID_PARAMETER;
	}
#endif
	
	switch (index & 0x0F)
	{
	case 0:
		// ALE & CLE
		GPIOD->CRH = (GPIOD->CRH & ~(0xFF << ((11 - 8) * 4))) | 
						(uint32_t)0xBB << ((11 - 8) * 4);
		
		switch (bank_index)
		{
		case 1:
			GPIOD->CRL = (GPIOD->CRL & ~(0x0F << (7 * 4))) | 
							(uint32_t)stm32_GPIO_AFPP << (7 * 4);
			GPIOG->CRL = (GPIOG->CRL & ~(0x0F << (6 * 4))) | 
							(uint32_t)stm32_GPIO_INP << (6 * 4);
			GPIOG->BSRR = (uint32_t)1 << 6;
			break;
		case 2:
			GPIOG->CRH = (GPIOG->CRH & ~(0x0F << ((9 - 8) * 4))) | 
							(uint32_t)stm32_GPIO_AFPP << ((9 - 8) * 4);
			GPIOG->CRL = (GPIOG->CRL & ~(0x0F << (7 * 4))) | 
							(uint32_t)stm32_GPIO_INP << (7 * 4);
			GPIOG->BSRR = (uint32_t)1 << 7;
			break;
		default:
			return VSFERR_NOT_SUPPORT;
		}
		
		pcr |= (((info->param.timing.ale_to_re_cycle - 1) & 0x0F) << 13) | 
				(((info->param.timing.cle_to_re_cycle - 1) & 0x0F) << 9);
		if (info->param.ecc.ecc_enable)
		{
			pcr |= STM32_FSMC_PCR_ECCEN;
			switch (info->param.ecc.ecc_page_size)
			{
			case 256:
				pcr |= STM32_FSMC_PCR_ECCPS_256;
				break;
			case 512:
				pcr |= STM32_FSMC_PCR_ECCPS_512;
				break;
			case 1024:
				pcr |= STM32_FSMC_PCR_ECCPS_1024;
				break;
			case 2048:
				pcr |= STM32_FSMC_PCR_ECCPS_2048;
				break;
			case 4096:
				pcr |= STM32_FSMC_PCR_ECCPS_4096;
				break;
			case 8192:
				pcr |= STM32_FSMC_PCR_ECCPS_8192;
				break;
			default:
				return VSFERR_INVALID_PARAMETER;
			}
		}
		
		if (info->common_info.wait_signal != EBI_WAIT_NONE)
		{
			GPIOD->CRL = (GPIOD->CRL & ~(0x0F << (6 * 4))) | 
							(uint32_t)0x0B << (6 * 4);
			pcr |= STM32_FSMC_PCR_PWAITEN;
		}
		
		if (8 == info->common_info.data_width)
		{
			pcr |= STM32_FSMC_PCR_PWID_8;
		}
		else if (16 == info->common_info.data_width)
		{
			pcr |= STM32_FSMC_PCR_PWID_16;
		}
		else
		{
			return VSFERR_INVALID_PARAMETER;
		}
		
		pmem = ((info->param.timing.setup_cycle - 1) << 0) | 
				((info->param.timing.wait_cycle - 1) << 8) | 
				(info->param.timing.hold_cycle << 16) | 
				(info->param.timing.hiz_cycle << 24);
		patt = ((info->param.timing.setup_cycle_attr - 1) << 0) | 
				((info->param.timing.wait_cycle_attr - 1) << 8) | 
				(info->param.timing.hold_cycle_attr << 16) | 
				(info->param.timing.hiz_cycle_attr << 24);
		
		switch (bank_index)
		{
		case 1:
			FSMC_Bank2->PMEM2 = pmem;
			FSMC_Bank2->PATT2 = patt;
			FSMC_Bank2->PCR2 = pcr;
			break;
		case 2:
			FSMC_Bank3->PMEM3 = pmem;
			FSMC_Bank3->PATT3 = patt;
			FSMC_Bank3->PCR3 = pcr;
			break;
		default:
			return VSFERR_NOT_SUPPORT;
		}
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t stm32_ebi_config_sdram(uint8_t index, struct ebi_sdram_info_t *info)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(info);
	return VSFERR_FAIL;
}

vsf_err_t stm32_ebi_config_ddram(uint8_t index, struct ebi_ddram_info_t *info)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(info);
	return VSFERR_FAIL;
}

vsf_err_t stm32_ebi_config_pccard(uint8_t index, struct ebi_pccard_info_t *info)
{
#if __VSF_DEBUG__
	if (NULL == info)
	{
		return VSFERR_INVALID_PARAMETER;
	}
#endif
	
	switch (index & 0x0F)
	{
	case 0:
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t stm32_ebi_config(uint8_t index, uint8_t target_index, void *param)
{
	uint8_t target_type = target_index & 0xF0;
	
	target_index &= 0x0F;
	switch (index)
	{
	case 0:
		switch (target_type)
		{
		case EBI_TGTTYP_NOR:
			return stm32_ebi_config_nor(
				(target_index & 0x03) << 6, (struct ebi_sram_psram_nor_info_t *)param);
		case EBI_TGTTYP_SRAM:
			return stm32_ebi_config_sram(
				(target_index & 0x03) << 6, (struct ebi_sram_psram_nor_info_t *)param);
		case EBI_TGTTYP_PSRAM:
			return stm32_ebi_config_psram(
				(target_index & 0x03) << 6, (struct ebi_sram_psram_nor_info_t *)param);
		case EBI_TGTTYP_NAND:
			return stm32_ebi_config_nand(
				((target_index + 1) & 0x03) << 4, (struct ebi_nand_info_t *)param);
		default:
			return VSFERR_NOT_SUPPORT;
		}
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

void* stm32_ebi_get_base_addr(uint8_t index, uint8_t target_index)
{
	uint8_t target_type = target_index & 0xF0;
	
	target_index &= 0x0F;
	switch (index)
	{
	case 0:
		switch (target_type)
		{
		case EBI_TGTTYP_NOR:
			if (target_index > 3)
			{
				return NULL;
			}
			return (void *)(0x60000000 + target_index * 0x4000000);
		case EBI_TGTTYP_NAND:
			if (target_index > 1)
			{
				return NULL;
			}
			return (void *)(0x60000000 + (1 + target_index) * 0x10000000);
		default:
			return NULL;
		}
	default:
		return NULL;
	}
}

vsf_err_t stm32_ebi_isready(uint8_t index, uint8_t target_index)
{
	uint8_t target_type = target_index & 0xF0;
	
	target_index &= 0x0F;
	switch (index)
	{
	case 0:
		switch (target_type)
		{
		case EBI_TGTTYP_NOR:
			if (target_index > 3)
			{
				return VSFERR_NOT_SUPPORT;
			}
			return VSFERR_NONE;
		case EBI_TGTTYP_NAND:
			switch (target_index)
			{
			case 0:
				return GPIOG->IDR & (1UL << 6) ? VSFERR_NONE : VSFERR_NOT_READY;
			case 1:
				return GPIOG->IDR & (1UL << 7) ? VSFERR_NONE : VSFERR_NOT_READY;
			default:
				return VSFERR_NOT_SUPPORT;
			}
		default:
			return VSFERR_NOT_SUPPORT;
		}
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

uint8_t stm32_ebi_read8(uint8_t index, uint32_t address)
{
	uint8_t target_index = (index >> 6) & 0x03;
	uint8_t bank_index = (index >> 4) & 0x03;
	
	address += 0x60000000 + (0x10000000 * bank_index);
	switch (bank_index)
	{
	case 0:
		address += target_index * 0x4000000;
	case 1:
	case 2:
	case 3:
		break;
	}
	return *(volatile uint8_t *)address;
}

void stm32_ebi_write8(uint8_t index, uint32_t address, uint8_t data)
{
	uint8_t target_index = (index >> 6) & 0x03;
	uint8_t bank_index = (index >> 4) & 0x03;
	
	address += 0x60000000 + (0x10000000 * bank_index);
	switch (bank_index)
	{
	case 0:
		address += target_index * 0x4000000;
	case 1:
	case 2:
	case 3:
		break;
	}
	*(volatile uint8_t *)address = data;
}

uint16_t stm32_ebi_read16(uint8_t index, uint32_t address)
{
	uint8_t target_index = (index >> 6) & 0x03;
	uint8_t bank_index = (index >> 4) & 0x03;
	
	address += 0x60000000 + (0x10000000 * bank_index);
	switch (bank_index)
	{
	case 0:
		address += target_index * 0x4000000;
	case 1:
	case 2:
	case 3:
		break;
	}
	return *(volatile uint16_t *)address;
}

void stm32_ebi_write16(uint8_t index, uint32_t address, uint16_t data)
{
	uint8_t target_index = (index >> 6) & 0x03;
	uint8_t bank_index = (index >> 4) & 0x03;
	
	address += 0x60000000 + (0x10000000 * bank_index);
	switch (bank_index)
	{
	case 0:
		address += target_index * 0x4000000;
	case 1:
	case 2:
	case 3:
		break;
	}
	*(volatile uint16_t *)address = data;
}

uint32_t stm32_ebi_read32(uint8_t index, uint32_t address)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(address);
	return 0;
}

void stm32_ebi_write32(uint8_t index, uint32_t address, uint32_t data)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(data);
}

vsf_err_t stm32_ebi_readp8(uint8_t index, uint32_t address, uint32_t count, 
						uint8_t *buff);
vsf_err_t stm32_ebi_readp8_isready(uint8_t index);
vsf_err_t stm32_ebi_writep8(uint8_t index, uint32_t address, uint32_t count, 
							uint8_t *buff);
vsf_err_t stm32_ebi_writep8_isready(uint8_t index);
vsf_err_t stm32_ebi_readp16(uint8_t index, uint32_t address, uint32_t count, 
							uint16_t *buff);
vsf_err_t stm32_ebi_readp16_isready(uint8_t index);
vsf_err_t stm32_ebi_writep16(uint8_t index, uint32_t address, uint32_t count, 
							uint16_t *buff);
vsf_err_t stm32_ebi_writep16_isready(uint8_t index);

vsf_err_t stm32_ebi_readp32(uint8_t index, uint32_t address, uint32_t count, 
							uint32_t *buff)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	REFERENCE_PARAMETER(buff);
	return VSFERR_FAIL;
}

vsf_err_t stm32_ebi_readp32_isready(uint8_t index)
{
	REFERENCE_PARAMETER(index);
	return VSFERR_FAIL;
}

vsf_err_t stm32_ebi_writep32(uint8_t index, uint32_t address, uint32_t count, 
							uint32_t *buff)
{
	REFERENCE_PARAMETER(index);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	REFERENCE_PARAMETER(buff);
	return VSFERR_FAIL;
}

vsf_err_t stm32_ebi_writep32_isready(uint8_t index)
{
	REFERENCE_PARAMETER(index);
	return VSFERR_FAIL;
}

vsf_err_t stm32_ebi_read(uint8_t index, uint8_t target_index, uint32_t address, 
						uint8_t data_size, uint8_t *buff, uint32_t count)
{
	uint32_t i;
	uint8_t target_type = target_index & 0xF0;
	
	target_index &= 0x0F;
	switch (index)
	{
	case 0:
		address += 0x60000000;
		switch (target_type)
		{
		case EBI_TGTTYP_NOR:
			address += (target_index & 0x03) * 0x4000000;
			break;
		case EBI_TGTTYP_NAND:
			address += (1 + (target_index & 0x01)) * 0x10000000;
			break;
		default:
			return VSFERR_NOT_SUPPORT;
		}
		
		switch (data_size)
		{
		case 1:
			for (i = 0; i < count; i++)
			{
				buff[i] = *(volatile uint8_t *)address;
				address += 1;
			}
			return VSFERR_NONE;
		case 2:
			for (i = 0; i < count; i++)
			{
				((uint16_t *)buff)[i] = *(volatile uint16_t *)address;
				address += 2;
			}
			return VSFERR_NONE;
		case 4:
			for (i = 0; i < count; i++)
			{
				((uint32_t *)buff)[i] = *(volatile uint32_t *)address;
				address += 4;
			}
			return VSFERR_NONE;
		default:
			return VSFERR_INVALID_PARAMETER;
		}
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t stm32_ebi_write(uint8_t index, uint8_t target_index, uint32_t address, 
						uint8_t data_size, uint8_t *buff, uint32_t count)
{
	uint32_t i;
	uint8_t target_type = target_index & 0xF0;
	
	target_index &= 0x0F;
	switch (index)
	{
	case 0:
		address += 0x60000000;
		switch (target_type)
		{
		case EBI_TGTTYP_NOR:
			address += (target_index & 0x03) * 0x4000000;
			break;
		case EBI_TGTTYP_NAND:
			address += (1 + (target_index & 0x01)) * 0x10000000;
			break;
		default:
			return VSFERR_NOT_SUPPORT;
		}
		
		switch (data_size)
		{
		case 1:
			for (i = 0; i < count; i++)
			{
				*(volatile uint8_t *)address = buff[i];
				address += 1;
			}
			return VSFERR_NONE;
		case 2:
			for (i = 0; i < count; i++)
			{
				*(volatile uint16_t *)address = ((uint16_t *)buff)[i];
				address += 2;
			}
			return VSFERR_NONE;
		case 4:
			for (i = 0; i < count; i++)
			{
				*(volatile uint32_t *)address = ((uint32_t *)buff)[i];
				address += 4;
			}
			return VSFERR_NONE;
		default:
			return VSFERR_INVALID_PARAMETER;
		}
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

#endif
