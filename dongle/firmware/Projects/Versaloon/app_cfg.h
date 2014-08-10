/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       app_cfg.h                                                 *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    configuration file                                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 *      2011-01-09:     Add stmxx-discovery (by Bingo)                    *
 **************************************************************************/

/************************ What do U have ************************/
// For IAR, define _HARDWARE_VER and FLASH_LOAD_OFFSET in pre-defined symbols
// For GCC, edit HW_BOARD in makefile

//
// Note The below definitions must correspond to the definitions made in 
// board_defs.mk (same place where the makefile resides)
//
#define NanoRelease1					0x01
#define MiniRelease1					0x15
#define ProRelease1						0x21
#define ProRelease3						0x23
#define STBee_Mini						0x31
#define STM8S_Discovery					0x32
#define STM32VL_Discovery				0x33
#define STM32L_Discovery				0x34
#define STM32F4_Discovery				0x35
#define STM8L_Discovery					0x36
#define STM8SVL_Discovery				0x37
#define ST_Link							0x38
#define STM32F0_Discovery				0x39

#if _HARDWARE_VER == NanoRelease1
#include "hw_cfg_NanoRelease1.h"
#elif _HARDWARE_VER == MiniRelease1
#include "hw_cfg_MiniRelease1.h"
#elif _HARDWARE_VER == ProRelease1
#include "hw_cfg_ProRelease1.h"
#elif _HARDWARE_VER == ProRelease3
#include "hw_cfg_ProRelease3.h"
#elif _HARDWARE_VER == STBee_Mini
#include "hw_cfg_STBee_Mini.h"
#elif (_HARDWARE_VER == ST_Link) ||\
	(_HARDWARE_VER == STM8S_Discovery) ||\
	(_HARDWARE_VER == STM8L_Discovery) ||\
	(_HARDWARE_VER == STM8SVL_Discovery) ||\
	(_HARDWARE_VER == STM32L_Discovery) ||\
	(_HARDWARE_VER == STM32VL_Discovery) ||\
	(_HARDWARE_VER == STM32F4_Discovery)
#include "hw_cfg_st_link.h"
#else
#error "Unknown or missing HW_BOARD definition"
#endif

// compiler config
#include "compiler.h"

/************************ What do U want ************************/
#define __VSF_DEBUG__					0

#define SYS_CFG_LARGE_MEMORY			0

#if (_HARDWARE_VER == ProRelease1) || (_HARDWARE_VER == ProRelease3)
#define SCRIPTS_EN						1
#else
#define SCRIPTS_EN						0
#endif
#define MSC_ON_VERSALOON_EN				0

#define VSS_CFG_MAX_ARGC				8
#define VSS_CFG_MAX_LINE_LENGTH			128
#define INTERFACES_INFO_T				app_interfaces_info_t
#define APPIO_BUFFER_SIZE				256
#if SCRIPTS_EN
#	define APPIO_DUMMY					0
#else
#	define APPIO_DUMMY					1
#endif

// internal flash layout:
// 0	..	32K		: bootloader
// 32K	..	256K	: firmware
// 256K	..	496K	: configuration data
// 496K ..	512K	: main script
// 512K	..	1008K	: target data
// 1008K..	1024K	: target script
#define TARGET_CFG_ADDR					0x08040000

#define EVSPROG_SCRIPT_FILE				"/evsprog.vts"
#define EVSPROG_SCRIPT_ADDR				0x0807C000
#define EVSPROG_SCRIPT_SIZE				(16 * 1024)
// EVSPROG_FW_SIZE including bootloader, firmware, configure, main script
#define EVSPROG_FW_SIZE					(EVSPROG_TARGET_MAINFLASH_ADDR - 0x08000000)
#define EVSPROG_TARGET_MAINFLASH_ADDR	(EVSPROG_SCRIPT_ADDR + EVSPROG_SCRIPT_SIZE)

#define PARAM_CHECK						1

// Target Support Configuration
#define TARGET_SLOT_NUMBER				1

#if SCRIPTS_EN
#	define TARGET_COMISP_EN				1
#	define TARGET_ARM_ADI_EN			1
#	define TARGET_AT89S5X_EN			1
#	define TARGET_AT91SAM3_EN			1
#	define TARGET_AVR32_EN				0
#	define TARGET_AVR8_EN				1
#	define TARGET_AVRXMEGA_EN			0
#	define TARGET_C8051F_EN				1
#	define TARGET_HCS08_EN				1
#	define TARGET_HCS12_EN				1
#	define TARGET_LM3S_EN				1
#	define TARGET_LPC1000_EN			1
#	define TARGET_LPC900_EN				1
#	define TARGET_MSP430_EN				0
#	define TARGET_PSOC1_EN				1
#	define TARGET_STM32F1_EN			1
#	define TARGET_STM8_EN				1
#	define TARGET_SVF_EN				0
#	define TARGET_EE93CX6_EN			1
#	define TARGET_EE24CXX_EN			1
#	define TARGET_DF25XX_EN				1
#	define TARGET_STM32F2_EN			1
#	define TARGET_STM32F4_EN			1
#	define TARGET_STM32L1_EN			1
#	define TARGET_NUC100_EN				1
#	define TARGET_KINETIS_EN			1
#	define TARGET_SD_EN					1
#	define TARGET_CFI_EN				0
#	define TARGET_NAND_EN				0
#else
#	define TARGET_COMISP_EN				0
#	define TARGET_ARM_ADI_EN			0
#	define TARGET_AT89S5X_EN			0
#	define TARGET_AT91SAM3_EN			0
#	define TARGET_AVR32_EN				0
#	define TARGET_AVR8_EN				0
#	define TARGET_AVRXMEGA_EN			0
#	define TARGET_C8051F_EN				0
#	define TARGET_HCS08_EN				0
#	define TARGET_HCS12_EN				0
#	define TARGET_LM3S_EN				0
#	define TARGET_LPC1000_EN			0
#	define TARGET_LPC900_EN				0
#	define TARGET_MSP430_EN				0
#	define TARGET_PSOC1_EN				0
#	define TARGET_STM32F1_EN			0
#	define TARGET_STM8_EN				0
#	define TARGET_SVF_EN				0
#	define TARGET_EE93CX6_EN			0
#	define TARGET_EE24CXX_EN			0
#	define TARGET_DF25XX_EN				0
#	define TARGET_STM32F2_EN			0
#	define TARGET_STM32F4_EN			0
#	define TARGET_STM32L1_EN			0
#	define TARGET_NUC100_EN				0
#	define TARGET_KINETIS_EN			0
#	define TARGET_SD_EN					0
#	define TARGET_CFI_EN				0
#	define TARGET_NAND_EN				0
#endif

#define STLINK_EN						0

#define USB_TO_XXX_EN					1

#define POWER_OUT_EN					(1 && HW_HAS_POWERCONTROL)
#define POWER_SAMPLE_EN					(1 && HW_HAS_ADC)

#if USB_TO_XXX_EN
// page 0
#	define USB_TO_USART_EN				(0 && HW_HAS_USART)
#	define USB_TO_SPI_EN				(1 && HW_HAS_SPI)
#	define USB_TO_IIC_EN				(1 && HW_HAS_IIC)
#	define USB_TO_GPIO_EN				(1 && HW_HAS_GPIO)
#	define USB_TO_CAN_EN				(0 && HW_HAS_CAN)
#	define USB_TO_PWM_EN				(1 && HW_HAS_PWM)
#	define USB_TO_ADC_EN				(1 && HW_HAS_ADC)
#	define USB_TO_DAC_EN				(0 && HW_HAS_DAC)
#	define USB_TO_MICROWIRE_EN			(1 && HW_HAS_MICROWIRE)
#	define USB_TO_DUSI_EN				(1 && HW_HAS_DUSI)
#	define USB_TO_EBI_EN				(1 && HW_HAS_EBI)
#	define USB_TO_CLKO_EN				(1 && HW_HAS_CLKO)
// page 1
#	define USB_TO_JTAG_LL_EN			(1 && HW_HAS_JTAG)
#	define USB_TO_JTAG_HL_EN			(1 && HW_HAS_JTAG)
#	define USB_TO_JTAG_RAW_EN			(1 && HW_HAS_JTAG)
#	define USB_TO_ISSP_EN				(1 && HW_HAS_ISSP)
#	define USB_TO_C2_EN					(1 && HW_HAS_C2)
#	define USB_TO_MSP430_JTAG_EN		(1 && HW_HAS_MSP430_JTAG)
#	define USB_TO_MSP430_SBW_EN			(0 && HW_HAS_MSP430_SBW)
#	define USB_TO_LPCICP_EN				(1 && HW_HAS_LPCICP)
#	define USB_TO_SWD_EN				(1 && HW_HAS_SWD)
#	define USB_TO_SWIM_EN				(1 && HW_HAS_SWIM)
#	define USB_TO_BDM_EN				(1 && HW_HAS_BDM)
// page 2
#	define USB_TO_POWER_EN				(1 && POWER_OUT_EN)
#	define USB_TO_POLL_EN				1
#endif

/***************************** Buffer ****************************/
#define USB_DATA_BUFF_SIZE				(8 * 1024)
#define ASYN_DATA_BUFF_SIZE				(4 * 1024)

/**************************** Checks ****************************/
#define _HARDWARE_VER_STR				make_ver(_HARDWARE_VER)
#define make_ver(v)						make_str(v)
#define make_str(s)						# s

#define INTERFACE_IIC_EN				HW_HAS_IIC
#define INTERFACE_SPI_EN				HW_HAS_SPI
#define INTERFACE_SDIO_EN				HW_HAS_SDIO
#define INTERFACE_USART_EN				HW_HAS_USART
#define INTERFACE_GPIO_EN				HW_HAS_GPIO
#define INTERFACE_PWM_EN				HW_HAS_PWM
#define INTERFACE_EBI_EN				HW_HAS_EBI
#define INTERFACE_ADC_EN				HW_HAS_ADC
#define INTERFACE_CLKO_EN				HW_HAS_CLKO

#define INTERFACE_C2_EN					(USB_TO_XXX_EN && USB_TO_C2_EN)
#define INTERFACE_ISSP_EN				(USB_TO_XXX_EN && USB_TO_ISSP_EN)
#define INTERFACE_LPC_ICP_EN			(USB_TO_XXX_EN && USB_TO_LPCICP_EN)
#define INTERFACE_JTAG_EN				(USB_TO_XXX_EN && \
				(USB_TO_JTAG_HL_EN || USB_TO_JTAG_LL_EN || USB_TO_JTAG_RAW_EN))
#define INTERFACE_MSP430_JTAG_EN		(USB_TO_XXX_EN && USB_TO_MSP430_JTAG_EN)
#define INTERFACE_MSP430_SBW_EN			(USB_TO_XXX_EN && USB_TO_MSP430_SBW_EN)
#define INTERFACE_SWIM_EN				(USB_TO_XXX_EN && USB_TO_SWIM_EN)
#define INTERFACE_SWD_EN				(USB_TO_XXX_EN && USB_TO_SWD_EN)
#define INTERFACE_BDM_EN				(USB_TO_XXX_EN && USB_TO_BDM_EN)
#define INTERFACE_DUSI_EN				(USB_TO_XXX_EN && USB_TO_DUSI_EN)
#define INTERFACE_MICROWIRE_EN			(USB_TO_XXX_EN && USB_TO_MICROWIRE_EN)
