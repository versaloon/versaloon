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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <ctype.h>
#include <inttypes.h>

#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"
#include "scripts.h"

#include "memlist.h"
#include "pgbar.h"
#include "strparser.h"

#include "at89s5x/at89s5x.h"
#include "psoc1/psoc1.h"
#include "lpc900/lpc900.h"
#include "msp430/msp430.h"
#include "c8051f/c8051f.h"
#include "avr8/avr8.h"
#include "comisp/comisp.h"
#include "svf_player/svf_player.h"
#include "cortex-m/cm.h"
#include "stm32f1/stm32f1.h"
#include "lpc1000/lpc1000.h"
#include "stm8/stm8.h"
#include "at91sam3/at91sam3.h"
#include "avr32/avr32.h"
#include "avrxmega/avrxmega.h"
#include "lm3s/lm3s.h"
#include "hcs08/hcs08.h"
#include "hcs12/hcs12.h"
#include "ee93cx6/ee93cx6.h"
#include "ee24cxx/ee24cxx.h"
#include "df25xx/df25xx.h"
#include "stm32f2/stm32f2.h"
#include "sd/sd.h"
#include "cfi/cfi.h"
#include "nand/nand.h"
#include "stm32l1/stm32l1.h"
#include "nuc100/nuc100.h"
#include "nuc400/nuc400.h"
#include "kinetis/kinetis.h"
#include "sst32hfxx/sst32hfxx.h"

VSS_HANDLER(target_memory_detail);
VSS_HANDLER(target_parameter_detail);
VSS_HANDLER(target_series);
VSS_HANDLER(target_chip);
VSS_HANDLER(target_value);
VSS_HANDLER(target_interface_frequency);
VSS_HANDLER(target_kernel_khz);
VSS_HANDLER(target_quartz_khz);
VSS_HANDLER(target_erase_on_demand);
VSS_HANDLER(target_wait_state);
VSS_HANDLER(target_auto_adjust);
VSS_HANDLER(target_address);
VSS_HANDLER(target_jtag_dc);
VSS_HANDLER(target_interface_mode);
VSS_HANDLER(target_prepare);
VSS_HANDLER(target_operate);
VSS_HANDLER(target_execute_addr);
VSS_HANDLER(target_raw_mode);
VSS_HANDLER(target_enter_program_mode);
VSS_HANDLER(target_leave_program_mode);
VSS_HANDLER(target_erase);
VSS_HANDLER(target_write);
VSS_HANDLER(target_read);
VSS_HANDLER(target_verify);
VSS_HANDLER(target_interface_indexes);
VSS_HANDLER(target_set_specified_param);

static const struct vss_cmd_t target_cmd[] =
{
	VSS_CMD(	"memory-detail",
				"show memory detail, format: memory-detail/D TARGET",
				target_memory_detail,
				NULL),
	VSS_CMD(	"D",
				"show memory detail, format: memory-detail/D TARGET",
				target_memory_detail,
				NULL),
	VSS_CMD(	"parameter",
				"show parameter detail, format: parameter/P TARGET",
				target_parameter_detail,
				NULL),
	VSS_CMD(	"P",
				"show parameter detail, format: parameter/P TARGET",
				target_parameter_detail,
				NULL),
	VSS_CMD(	"target-series",
				"set target series, format: target-series/s SERIES",
				target_series,
				NULL),
	VSS_CMD(	"s",
				"set target series, format: target-series/s SERIES",
				target_series,
				NULL),
	VSS_CMD(	"target-chip",
				"set target chip, format: target-chip/c CHIP",
				target_chip,
				NULL),
	VSS_CMD(	"c",
				"set target chip, format: target-chip/c CHIP",
				target_chip,
				NULL),
	VSS_CMD(	"target",
				"set target value, format: target/t TARGET VALUE",
				target_value,
				NULL),
	VSS_CMD(	"t",
				"set target value, format: target/t TARGET VALUE",
				target_value,
				NULL),
	VSS_CMD(	"frequency",
				"set frequency of programming interface, "
				"format: frequency/f FREQUENCY",
				target_interface_frequency,
				NULL),
	VSS_CMD(	"F",
				"set frequency of programming interface, "
				"format: frequency/f FREQUENCY",
				target_interface_frequency,
				NULL),
	VSS_CMD(	"kernel-khz",
				"set target kernel frequency in khz, format: kernel-khz/K KHZ",
				target_kernel_khz,
				NULL),
	VSS_CMD(	"K",
				"set target kernel frequency in khz, format: kernel-khz/K KHZ",
				target_kernel_khz,
				NULL),
	VSS_CMD(	"quartz-khz",
				"set target quartz frequency in khz, format: quartz-khz/Q KHZ",
				target_quartz_khz,
				NULL),
	VSS_CMD(	"Q",
				"set target quartz frequency in khz, format: quartz-khz/Q KHZ",
				target_quartz_khz,
				NULL),
	VSS_CMD(	"wait-state",
				"set target wait state, format: wait-state/W WAIT",
				target_wait_state,
				NULL),
	VSS_CMD(	"W",
				"set target wait state, format: wait-state/W WAIT",
				target_wait_state,
				NULL),
	VSS_CMD(	"e",
				"erase on demand feature, format: e/erase-on-demand",
				target_erase_on_demand,
				NULL),
	VSS_CMD(	"erase-on-demand",
				"erase on demand feature, format: e/erase-on-demand",
				target_erase_on_demand,
				NULL),
	VSS_CMD(	"auto-adjust",
				"set target auto adjust, format: auto-adjust/A",
				target_auto_adjust,
				NULL),
	VSS_CMD(	"A",
				"set target auto adjust, format: auto-adjust/A",
				target_auto_adjust,
				NULL),
	VSS_CMD(	"jtag-dc",
				"set JTAG daisy chain, format: jtag-dc/J UB_UA_BB_BA",
				target_jtag_dc,
				NULL),
	VSS_CMD(	"address",
				"set address of target chip, format: address/a ADDRESS",
				target_address,
				NULL),
	VSS_CMD(	"a",
				"set address of target chip, format: address/a ADDRESS",
				target_address,
				NULL),
	VSS_CMD(	"J",
				"set JTAG daisy chain, format: jtag-dc/J UB_UA_BB_BA",
				target_jtag_dc,
				NULL),
	VSS_CMD(	"mode",
				"set interface mode, format: mode/m MODE",
				target_interface_mode,
				NULL),
	VSS_CMD(	"m",
				"set interface mode, format: mode/m MODE",
				target_interface_mode,
				NULL),
	VSS_CMD(	"prepare",
				"prepare target programming, format: prepare",
				target_prepare,
				NULL),
	VSS_CMD(	"operate",
				"operate target programming, format: operate",
				target_operate,
				NULL),
	VSS_CMD(	"execute",
				"execute defined address, format: execute/x ADDR",
				target_execute_addr,
				NULL),
	VSS_CMD(	"x",
				"execute defined address, format: execute/x ADDR",
				target_execute_addr,
				NULL),
	VSS_CMD(	"raw",
				"enable raw mode, format: raw/r",
				target_raw_mode,
				NULL),
	VSS_CMD(	"r",
				"enable raw mode, format: raw/r",
				target_raw_mode,
				NULL),
	VSS_CMD(	"enter_program_mode",
				"enter program mode, format: enter_program_mode",
				target_enter_program_mode,
				NULL),
	VSS_CMD(	"leave_program_mode",
				"leave program mode, format: leave_program_mode SUCCESS",
				target_leave_program_mode,
				NULL),
	VSS_CMD(	"erase",
				"erase target area, format: erase [AREA_CHAR]",
				target_erase,
				NULL),
	VSS_CMD(	"read",
				"read target area, format: read AREA_CHAR ADDR SIZE",
				target_read,
				NULL),
	VSS_CMD(	"write",
				"write target area, format: write [AREA_CHAR]",
				target_write,
				NULL),
	VSS_CMD(	"verify",
				"verify target area, format: verify [AREA_CHAR]",
				target_verify,
				NULL),
	VSS_CMD(	"indexes",
				"define interfaces indexes used, format: indexes/i INDEX_STR",
				target_interface_indexes,
				NULL),
	VSS_CMD(	"i",
				"define interfaces indexes used, format: indexes/i INDEX_STR",
				target_interface_indexes,
				NULL),
	VSS_CMD(	"target_param",
				"set target specified parameter, "
				"format: target_param/T PARAMETER",
				target_set_specified_param,
				NULL),
	VSS_CMD(	"T",
				"set target specified parameter, "
				"format: target_param/T PARAMETER",
				target_set_specified_param,
				NULL),
	VSS_CMD_END
};
struct vss_cmd_list_t target_cmd_list = VSS_CMD_LIST("target", target_cmd);

const struct target_area_name_t target_area_name[NUM_OF_TARGET_AREA] =
{
	{CHIPID_CHAR,				CHIPID,				"chipid"},
	{CHIPID_CHKSUM_CHAR,		CHIPID_CHKSUM,		"chipid_checksum"},
	{BOOTLOADER_CHAR,			BOOTLOADER,			"bootloader"},
	{BOOTLOADER_CHKSUM_CHAR,	BOOTLOADER_CHKSUM,	"bootloader_checksum"},
	{APPLICATION_CHAR,			APPLICATION,		"flash"},
	{APPLICATION_CHKSUM_CHAR,	APPLICATION_CHKSUM,	"flash_checksum"},
	{EEPROM_CHAR,				EEPROM,				"eeprom"},
	{EEPROM_CHKSUM_CHAR,		EEPROM_CHKSUM,		"eeprom_checksum"},
	{OTPROM_CHAR,				OTPROM,				"otprom"},
	{OTPROM_CHKSUM_CHAR,		OTPROM_CHKSUM,		"otprom_checksum"},
	{FUSE_CHAR,					FUSE,				"fuse"},
	{FUSE_CHKSUM_CHAR,			FUSE_CHKSUM,		"fuse_checksum"},
	{LOCK_CHAR,					LOCK,				"lock"},
	{LOCK_CHKSUM_CHAR,			LOCK_CHKSUM,		"lock_checksum"},
	{USRSIG_CHAR,				USRSIG,				"usrsig"},
	{USRSIG_CHKSUM_CHAR,		USRSIG_CHKSUM,		"usrsig_checksum"},
	{CALIBRATION_CHAR,			CALIBRATION,		"calibration"},
	{CALIBRATION_CHKSUM_CHAR,	CALIBRATION_CHKSUM,	"calibration_checksum"},
	{SRAM_CHAR,					SRAM,				"sram"},
	{SPECIAL_STRING_CHAR,		SPECIAL_STRING,		"special_str"},
	{UNIQUEID_CHAR,				UNIQUEID,			"uniqueid"}
};

const struct target_info_t targets_info[] =
{
	// stm32f1
#if TARGET_STM32F1_EN
	{
		STM32F1_STRING,						// name
		AUTO_DETECT CAN_EXECUTE,			// feature
		stm32f1_program_area_map,			// program_area_map
		stm32f1_program_mode,				// program_mode
		&stm32f1_program_functions,			// program_functions
		stm32f1_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
	// stm32f2
#if TARGET_STM32F2_EN
	{
		STM32F2_STRING,						// name
		AUTO_DETECT CAN_EXECUTE,			// feature
		stm32f2_program_area_map,			// program_area_map
		stm32f2_program_mode,				// program_mode
		&stm32f2_program_functions,			// program_functions
		stm32f2_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
	// stm32f4
#if TARGET_STM32F4_EN
	{
		STM32F4_STRING,						// name
		AUTO_DETECT CAN_EXECUTE,			// feature
		stm32f2_program_area_map,			// program_area_map
		stm32f2_program_mode,				// program_mode
		&stm32f2_program_functions,			// program_functions
		stm32f4_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
	// stm32l1
#if TARGET_STM32L1_EN
	{
		STM32L1_STRING,						// name
		AUTO_DETECT CAN_EXECUTE,			// feature
		stm32l1_program_area_map,			// program_area_map
		stm32l1_program_mode,				// program_mode
		&stm32l1_program_functions,			// program_functions
		stm32l1_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_LPC1000_EN
	// lpc1000
	{
		LPC1000_STRING,						// name
		AUTO_DETECT CAN_EXECUTE,			// feature
		lpc1000_program_area_map,			// program_area_map
		lpc1000_program_mode,				// program_mode
		&lpc1000_program_functions,			// program_functions
		lpc1000_notifier,					// notifier
		lpc1000_adjust_setting,				// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_LM3S_EN
	// lm3s
	{
		LM3S_STRING,						// name
		AUTO_DETECT CAN_EXECUTE,			// feature
		lm3s_program_area_map,				// program_area_map
		lm3s_program_mode,					// program_mode
		&lm3s_program_functions,			// program_functions
		lm3s_notifier,						// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_AT91SAM3_EN
	// at91sam3
	{
		AT91SAM3_STRING,					// name
		AUTO_DETECT CAN_EXECUTE,			// feature
		at91sam3_program_area_map,			// program_area_map
		at91sam3_program_mode,				// program_mode
		&at91sam3_program_functions,		// program_functions
		at91sam3_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if	TARGET_NUC100_EN
	// nuc100
	{
		NUC100_STRING,						// name
		AUTO_DETECT CAN_EXECUTE,			// feature
		nuc100_program_area_map,			// program_area_map
		nuc100_program_mode,				// program_mode
		&nuc100_program_functions,			// program_functions
		nuc100_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if	TARGET_NUC400_EN
	// nuc400
	{
		NUC400_STRING,						// name
		AUTO_DETECT CAN_EXECUTE,			// feature
		nuc400_program_area_map,			// program_area_map
		nuc400_program_mode,				// program_mode
		&nuc400_program_functions,			// program_functions
		nuc400_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if	TARGET_KINETIS_EN
	// kinetis
	{
		KINETIS_STRING,						// name
		AUTO_DETECT CAN_EXECUTE,			// feature
		kinetis_program_area_map,			// program_area_map
		kinetis_program_mode,				// program_mode
		&kinetis_program_functions,			// program_functions
		kinetis_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_AVR32_EN
	// avr32
	{
		AVR32_STRING,						// name
		AUTO_DETECT CAN_EXECUTE,			// feature
		avr32_program_area_map,				// program_area_map
		avr32_program_mode,					// program_mode
		&avr32_program_functions,			// program_functions
		avr32_notifier,						// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_AVRXMEGA_EN
	// avrxmega
	{
		AVRXMEGA_STRING,					// name
		AUTO_DETECT,						// feature
		avrxmega_program_area_map,			// program_area_map
		avrxmega_program_mode,				// program_mode
		&avrxmega_program_functions,		// program_functions
		avrxmega_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_STM8_EN
	// stm8
	{
		STM8_STRING,						// name
		"",									// feature
		stm8_program_area_map,				// program_area_map
		stm8_program_mode,					// program_mode
		&stm8_program_functions,			// program_functions
		stm8_notifier,						// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_AT89S5X_EN
	// at89s5x
	{
		S5X_STRING,							// name
		AUTO_DETECT,						// feature
		s5x_program_area_map,				// program_area_map
		s5x_program_mode,					// program_mode
		&s5x_program_functions,				// program_functions
		s5x_notifier,						// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_PSOC1_EN
	// psoc1
	{
		PSOC1_STRING,						// name
		AUTO_DETECT,						// feature
		psoc1_program_area_map,				// program_area_map
		psoc1_program_mode,					// program_mode
		&psoc1_program_functions,			// program_functions
		psoc1_notifier,						// notifier
		psoc1_adjust_setting,				// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_MSP430_EN
	// msp430
	{
		MSP430_STRING,						// name
		AUTO_DETECT,						// feature
		msp430_program_area_map,			// program_area_map
		msp430_program_mode,				// program_mode
		&msp430_program_functions,			// program_functions
		msp430_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_C8051F_EN
	// c8051f
	{
		C8051F_STRING,						// name
		AUTO_DETECT,						// feature
		c8051f_program_area_map,			// program_area_map
		c8051f_program_mode,				// program_mode
		&c8051f_program_functions,			// program_functions
		c8051f_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_AVR8_EN
	// avr8
	{
		AVR8_STRING,						// name
		AUTO_DETECT,						// feature
		avr8_program_area_map,				// program_area_map
		avr8_program_mode,					// program_mode
		&avr8_program_functions,			// program_functions
		avr8_notifier,						// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_SVF_EN
	// svf_player
	{
		SVFP_STRING,						// name
		NO_TARGET,							// feature
		svfp_program_area_map,				// program_area_map
		svfp_program_mode,					// program_mode
		&svfp_program_functions,			// program_functions
		svfp_notifier,						// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_LPC900_EN
	// lpc900
	{
		LPC900_STRING,						// name
		AUTO_DETECT,						// feature
		lpc900_program_area_map,			// program_area_map
		lpc900_program_mode,				// program_mode
		&lpc900_program_functions,			// program_functions
		lpc900_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_HCS08_EN
	// HCS08
	{
		HCS08_STRING,						// name
		AUTO_DETECT,						// feature
		hcs08_program_area_map,				// program_area_map
		hcs08_program_mode,					// program_mode
		&hcs08_program_functions,			// program_functions
		hcs08_notifier,						// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_HCS12_EN
	// HCS12
	{
		HCS12_STRING,						// name
		"",									// feature
		hcs12_program_area_map,				// program_area_map
		hcs12_program_mode,					// program_mode
		&hcs12_program_functions,			// program_functions
		hcs12_notifier,						// notifier
		NULL,								// adjust_setting
		hcs12_adjust_mapping,				// adjust_mapping
	},
#endif
#if TARGET_EE93CX6_EN
	// EE93CX6
	{
		EE93CX6_STRING,						// name
		"",									// feature
		ee93cx6_program_area_map,			// program_area_map
		ee93cx6_program_mode,				// program_mode
		&ee93cx6_program_functions,			// program_functions
		ee93cx6_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_EE24CXX_EN
	// EE24CXX
	{
		EE24CXX_STRING,						// name
		"",									// feature
		ee24cxx_program_area_map,			// program_area_map
		ee24cxx_program_mode,				// program_mode
		&ee24cxx_program_functions,			// program_functions
		ee24cxx_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_DF25XX_EN
	// DF25XX
	{
		DF25XX_STRING,						// name
		AUTO_DETECT,						// feature
		df25xx_program_area_map,			// program_area_map
		df25xx_program_mode,				// program_mode
		&df25xx_program_functions,			// program_functions
		df25xx_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_SD_EN
	// SD
	{
		SD_STRING,							// name
		AUTO_DETECT,						// feature
		sd_program_area_map,				// program_area_map
		sd_program_mode,					// program_mode
		&sd_program_functions,				// program_functions
		sd_notifier,						// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_CFI_EN
	// CFI
	{
		CFI_STRING,							// name
		AUTO_DETECT,						// feature
		cfi_program_area_map,				// program_area_map
		cfi_program_mode,					// program_mode
		&cfi_program_functions,				// program_functions
		cfi_notifier,						// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_NAND_EN
	// NAND
	{
		NAND_STRING,						// name
		AUTO_DETECT,						// feature
		nand_program_area_map,				// program_area_map
		nand_program_mode,					// program_mode
		&nand_program_functions,			// program_functions
		nand_notifier,						// notifier
		nand_adjust_setting,				// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_SST32HFXX_EN
	// SST32HFXX
	{
		SST32HFXX_STRING,					// name
		AUTO_DETECT,						// feature
		sst32hfxx_program_area_map,			// program_area_map
		sst32hfxx_program_mode,				// program_mode
		&sst32hfxx_program_functions,		// program_functions
		sst32hfxx_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
	{
		NULL,								// name
		0,									// areas
		NULL,								// program_area_map
		NULL,								// program_mode
		NULL,								// program_functions
		NULL,								// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	}
};

vsf_err_t target_build_chip_series(struct target_info_t *target,
		const struct program_mode_t *program_mode, struct chip_series_t *s);
vsf_err_t target_build_chip_fl(struct target_info_t *target,
				const char *chip_module, char *type, struct chip_fl_t *fl);

void target_chip_area_free(struct chip_area_info_t *area_info)
{
	struct chip_area_info_t *temp = NULL;
	
	while (area_info != NULL)
	{
		temp = area_info;
		area_info = sllist_get_container(area_info->list.next,
											struct chip_area_info_t, list);
		free(temp);
	}
}

struct chip_area_info_t* target_chip_area_dup(
										struct chip_area_info_t *area_info)
{
	struct chip_area_info_t *result = NULL, *temp = NULL, *last = NULL;
	
	while (area_info != NULL)
	{
		temp = (struct chip_area_info_t *)malloc(sizeof(*temp));
		if (NULL == temp)
		{
			return NULL;
		}
		memset(temp, 0, sizeof(*temp));
		*temp = *area_info;
		sllist_init_node(temp->list);
		
		if (NULL == result)
		{
			result = temp;
			last = result;
		}
		else
		{
			sllist_insert(last->list, temp->list);
			last = temp;
		}
		
		area_info = sllist_get_container(area_info->list.next,
											struct chip_area_info_t, list);
	}
	return result;
}

static vsf_err_t target_parse_cli_string(struct program_context_t *context)
{
	struct program_area_t *prog_area = NULL;
	struct chip_area_info_t *area_info = NULL;
	uint8_t i;
	char *format;
	char format_tmp[32];
	
	for (i = 0; i < dimof(target_area_name); i++)
	{
		prog_area = target_get_program_area(context->pi, i);
		area_info = target_get_chip_area(context->param, i);
		if ((prog_area != NULL) && (area_info != NULL) &&
			(prog_area->cli_str != NULL))
		{
			if (area_info->cli_format != NULL)
			{
				format = area_info->cli_format;
			}
			else if (area_info->size <= 8)
			{
				// cli_format not defined
				// simply use %nx as format, which is simple integer input
				SNPRINTF(format_tmp, sizeof(format_tmp), "%%%dx",
							(int)area_info->size);
				format = format_tmp;
			}
			else
			{
				LOG_ERROR(ERRMSG_NOT_DEFINED, "cli_format");
				return VSFERR_FAIL;
			}
			
			if (strparser_parse(prog_area->cli_str, format, prog_area->buff,
								prog_area->size))
			{
				LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "parse",
							target_area_name[i].full_name);
				return VSFERR_FAIL;
			}
		}
	}
	
	return VSFERR_NONE;
}

struct program_area_t* target_get_program_area(struct program_info_t *pi,
												uint32_t area_idx)
{
	if (area_idx < dimof(target_area_name))
	{
		return &pi->program_areas[area_idx];
	}
	return NULL;
}

struct chip_area_info_t* target_get_chip_area(struct chip_param_t *param,
												uint32_t area_idx)
{
	struct chip_area_info_t *area_info = NULL;
	
	area_info = param->chip_areas;
	while (area_info != NULL)
	{
		if (area_info->index == area_idx)
		{
			break;
		}
		area_info = sllist_get_container(area_info->list.next,
											struct chip_area_info_t, list);
	}
	return area_info;
}

void target_get_target_area(struct program_info_t *pi, char area,
							uint8_t **buff, uint32_t *size)
{
	struct program_area_t *prog_area = NULL;
	int8_t i;
	
	i = target_area_idx(area);
	if (i >= 0)
	{
		prog_area = target_get_program_area(pi, i);
		if (NULL == prog_area)
		{
			*buff = NULL;
			*size = 0;
		}
		else
		{
			*buff = prog_area->buff;
			*size = prog_area->size;
		}
	}
	else
	{
		*buff = NULL;
		*size = 0;
	}
}

int8_t target_area_idx_by_fullname(char *fullname)
{
	int8_t i;
	
	for (i = 0; i < (int8_t)dimof(target_area_name); i++)
	{
		if (!strcmp(target_area_name[i].full_name, fullname))
		{
			return i;
		}
	}
	return -1;
}

char target_area_char_by_fullname(char *fullname)
{
	uint32_t i;
	
	for (i = 0; i < dimof(target_area_name); i++)
	{
		if (!strcmp(target_area_name[i].full_name, fullname))
		{
			return target_area_name[i].name;
		}
	}
	return '\0';
}

int8_t target_area_idx_by_mask(uint32_t mask)
{
	int8_t i;
	
	for (i = 0; i < (int8_t)dimof(target_area_name); i++)
	{
		if (target_area_name[i].mask == mask)
		{
			return i;
		}
	}
	return -1;
}

char* target_area_fullname_by_mask(uint32_t mask)
{
	uint32_t i;
	
	for (i = 0; i < dimof(target_area_name); i++)
	{
		if (target_area_name[i].mask == mask)
		{
			return (char *)target_area_name[i].full_name;
		}
	}
	return NULL;
}

int8_t target_area_idx(char area_name)
{
	int8_t i;
	
	for (i = 0; i < (int8_t)dimof(target_area_name); i++)
	{
		if (target_area_name[i].name == area_name)
		{
			return i;
		}
	}
	return -1;
}

char* target_area_fullname(char area_name)
{
	int8_t i;
	
	i = target_area_idx(area_name);
	if (i < 0)
	{
		return NULL;
	}
	else
	{
		return (char *)target_area_name[i].full_name;
	}
}

uint32_t target_area_mask(char area_name)
{
	int8_t i;
	
	i = target_area_idx(area_name);
	if (i < 0)
	{
		return 0;
	}
	else
	{
		return target_area_name[i].mask;
	}
}

int8_t target_area_idx_by_name(char *name)
{
	if (strlen(name) == 1)
	{
		return target_area_idx(name[0]);
	}
	else
	{
		return target_area_idx_by_fullname(name);
	}
}

static vsf_err_t target_check_single_defined(struct program_info_t *pi,
												uint32_t opt)
{
	uint8_t i;
	
	opt = (pi->areas_defined ^ opt) & opt;
	
	for (i = 0; i < 32; i++)
	{
		if (opt & (1 << i))
		{
			LOG_ERROR(ERRMSG_NOT_DEFINED, target_area_fullname_by_mask(1 << i));
			return VSFERR_FAIL;
		}
	}
	return VSFERR_NONE;
}

int8_t target_mode_get_idx(const struct program_mode_t *mode, char mode_name)
{
	int8_t i;
	
	if (NULL == mode)
	{
		return -1;
	}
	
	i = 0;
	while (mode[i].name != 0)
	{
		if (mode[i].name == mode_name)
		{
			return i;
		}
		i++;
	}
	return -1;
}

static vsf_err_t target_check_defined(struct program_context_t *context)
{
	if (target_check_single_defined(context->pi, context->op->verify_operations)
		|| target_check_single_defined(context->pi, context->op->write_operations))
	{
		return VSFERR_FAIL;
	}
	else
	{
		return VSFERR_NONE;
	}
}

static vsf_err_t MEMLIST_VerifyBuff(struct memlist *ml, uint8_t *buf1,
				uint8_t *buf2, uint32_t addr, uint32_t len, uint32_t *pos)
{
	uint32_t i, len_tmp, offset, verified_len;
	
	if ((NULL == buf1) || (NULL == buf2))
	{
		return VSFERR_FAIL;
	}
	
	verified_len = 0;
	while ((ml != NULL) && (verified_len < len))
	{
		if ((ml->addr >= addr) && (ml->addr < (addr + len)))
		{
			offset = ml->addr - addr;
			if ((ml->addr + ml->len) <= (addr + len))
			{
				len_tmp = ml->len;
			}
			else
			{
				len_tmp = len - offset;
			}
		}
		else if ((addr >= ml->addr) && (addr < (ml->addr + ml->len)))
		{
			offset = 0;
			if ((ml->addr + ml->len) <= (addr + len))
			{
				len_tmp = ml->len - (addr - ml->addr);
			}
			else
			{
				len_tmp = len;
			}
		}
		else
		{
			ml = MEMLIST_GetNext(ml);
			continue;
		}
		
		for (i = 0; i < len_tmp; i++)
		{
			if (buf1[offset + i] != buf2[offset + i])
			{
				*pos = offset + i;
				return VSFERR_FAIL;
			}
		}
		verified_len += len_tmp;
		ml = MEMLIST_GetNext(ml);
	}
	return VSFERR_NONE;
}

static vsf_err_t target_program_check(struct program_context_t *context)
{
	const struct program_functions_t *pf = context->target->program_functions;
	struct program_info_t *pi = context->pi;
	struct chip_param_t *param = context->param;
	uint64_t i;
	
	if ((NULL == pf)
		|| ((NULL == pf->execute)
			&& ((NULL == pf->read_target)
				|| (NULL == pf->write_target)
				|| (NULL == pf->erase_target))))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, "current mode", pi->chip_name);
		return VSFERR_FAIL;
	}
	
	// check mode
	if ((context->series->num_of_chips > 0)
		&& (context->series->chips_param[0].program_mode != 0)
		&& !(param->program_mode & (1 << pi->mode)))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, "current mode", pi->chip_name);
		return VSFERR_FAIL;
	}
	
	// assert programmer
	i = context->target->program_mode[context->pi->mode].interface_needed;
	if (i)
	{
		if (interface_assert(&context->prog) || (NULL == context->prog))
		{
			LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "programmer module");
			return VSFERR_FAIL;
		}
		if ((context->prog->support_mask & i) != i)
		{
			LOG_ERROR("interface not supported: %s.", get_interface_name(i));
			return VSFERR_FAIL;
		}
	}
	return VSFERR_NONE;
}

static vsf_err_t target_enter_progmode(struct program_context_t *context)
{
	const struct program_functions_t *pf = context->target->program_functions;
	
	if (target_program_check(context))
	{
		return VSFERR_FAIL;
	}
	
	// switch target first
	if (pf->switch_target != NULL)
	{
		vsf_err_t err;
		
		err = pf->switch_target(context);
		if (err)
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "swith target");
			return err;
		}
	}
	
	if ((pf->enter_program_mode != NULL)
		&& pf->enter_program_mode(context))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "enter program mode");
		return ERRCODE_FAILURE_ENTER_PROG_MODE;
	}
	return VSFERR_NONE;
}

static vsf_err_t target_leave_progmode(struct program_context_t *context,
										uint8_t success)
{
	const struct program_functions_t *pf = context->target->program_functions;
	
	if (target_program_check(context))
	{
		return VSFERR_FAIL;
	}
	
	// switch target first
	if (pf->switch_target != NULL)
	{
		vsf_err_t err;
		
		err = pf->switch_target(context);
		if (err)
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "swith target");
			return err;
		}
	}
	
	// leave with success
	if ((pf->leave_program_mode != NULL)
		&& pf->leave_program_mode(context, success))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "leave program mode");
		return ERRCODE_FAILURE_ENTER_PROG_MODE;
	}
	return VSFERR_NONE;
}

static vsf_err_t target_program(struct program_context_t *context)
{
	const struct program_functions_t *pf = context->target->program_functions;
	const struct program_area_map_t *p_map = context->target->program_area_map;
	struct program_info_t *pi = context->pi;
	struct operation_t *op = context->op;
	struct chip_param_t *param = context->param;
	struct INTERFACES_INFO_T *prog = context->prog;
	
	struct chip_area_info_t *area_info = NULL;
	struct program_area_t *prog_area = NULL;
	vsf_err_t err = VSFERR_NONE;
	uint32_t i, j;
	int8_t area_idx;
	char area_char;
	uint32_t area_mask;
	enum area_attr_t area_attr;
	uint32_t target_size, page_size, start_addr;
	char *format = NULL;
	char format_tmp[32];
	char *fullname, str_tmp[256];
	struct memlist **ml, *ml_tmp, *ml_exact = NULL;
	uint32_t time_in_ms = 1000;
	uint8_t special_string[256];
	
	if (target_program_check(context))
	{
		return VSFERR_FAIL;
	}
	
	if (pf->execute != NULL)
	{
		err = pf->execute(context);
		goto target_program_exit;
	}
	
	// switch target first
	if (pf->switch_target != NULL)
	{
		err = pf->switch_target(context);
		if (err)
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "swith target");
			goto target_program_exit;
		}
	}
	
	// read chip id
	pi->chip_id = 0;
	if (pf->read_target(context, CHIPID_CHAR, 0, (uint8_t *)&pi->chip_id, 0))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read chip id");
		err = ERRCODE_FAILURE_OPERATION;
		goto target_program_exit;
	}
	LOG_INFO(INFOMSG_TARGET_CHIP_ID, pi->chip_id);
	if (!(op->read_operations & CHIPID))
	{
		if (pi->chip_id != param->chip_id)
		{
			LOG_WARNING(ERRMSG_INVALID_CHIP_ID, pi->chip_id, param->chip_id);
		}
	}
	else
	{
		goto target_program_exit;
	}
	
	// chip erase
	if (op->erase_operations && param->chip_erase)
	{
		LOG_INFO(INFOMSG_ERASING, "chip");
		pgbar_init("erasing chip |", "|", 0, 1, PROGRESS_STEP,
						PROGRESS_CHAR);
		
		if (pf->erase_target(context, ALL_CHAR, 0, 0))
		{
			pgbar_fini();
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "erase chip");
			err = ERRCODE_FAILURE_OPERATION;
			goto target_program_exit;
		}
		
		pgbar_update(1);
		pgbar_fini();
		LOG_INFO(INFOMSG_ERASED, "chip");
	}
	
	// erase, program, verify/read cycle
	i = 0;
	while (p_map[i].name != 0)
	{
		area_char = p_map[i].name;
		area_attr = p_map[i].attr;
		area_idx = target_area_idx(area_char);
		area_mask = target_area_mask(area_char);
		fullname = target_area_fullname(area_char);
		if (area_idx < 0)
		{
			// invalid area
			i++;
			continue;
		}
		
		area_info = target_get_chip_area(param, (uint32_t)area_idx);
		prog_area = target_get_program_area(pi, (uint32_t)area_idx);
		if ((NULL == area_info) || (NULL == prog_area))
		{
			i++;
			continue;
		}
		
		page_size = area_info->page_size;
		start_addr = area_info->addr;
		if ((p_map[i].fpage_size > page_size)
			&& ((p_map[i].fpage_size % page_size) == 0))
		{
			page_size = p_map[i].fpage_size;
		}
		
		if (area_info->size > prog_area->size)
		{
			area_info->size = prog_area->size;
		}
		if (p_map[i].data_pos)
		{
			ml = &(prog_area->memlist);
			ml_exact = prog_area->exact_memlist;
			target_size = MEMLIST_CalcAllSize(*ml);
		}
		else
		{
			ml = NULL;
			target_size = area_info->size;
			format = area_info->cli_format;
			if (NULL == format)
			{
				if (target_size > 8)
				{
					LOG_ERROR(ERRMSG_NOT_DEFINED, "cli_format");
					err = VSFERR_FAIL;
					goto target_program_exit;
				}
				// default type is hex value with 16-bit length
				SNPRINTF(format_tmp, sizeof(format_tmp), "%%%dx",
							(int)target_size);
				format = format_tmp;
			}
			if ((0 == target_size) && (SPECIAL_STRING_CHAR == area_char))
			{
				target_size = 1;
			}
		}
		if ((area_char != SPECIAL_STRING_CHAR) && !area_info->size)
		{
			i++;
			continue;
		}
		
		// not chip_erase, required to be erased, erasable
		// erase while write feature and write operation defined
		if (!param->chip_erase && (op->erase_operations & area_mask)
			&& (area_attr & AREA_ATTR_E)
			&& (!(area_attr & AREA_ATTR_EWW)
				|| !(op->write_operations & area_mask)))
		{
			uint32_t page_num =
				area_info->page_num > 0 ? area_info->page_num : 1;
			// target erase
			LOG_INFO(INFOMSG_ERASING, fullname);
			strcpy(str_tmp, "erasing ");
			strcat(str_tmp, fullname);
			strcat(str_tmp, " |");
			pgbar_init(str_tmp, "|", 0, page_num, PROGRESS_STEP, PROGRESS_CHAR);
			
			if (area_attr & AREA_ATTR_EP)
			{
				// erase every page
				for (j = 0; j < area_info->page_num; j++)
				{
					if (pf->erase_target(context, area_char,
							start_addr + j * page_size, page_size))
					{
						pgbar_fini();
						LOG_ERROR(ERRMSG_FAILURE_ERASE, fullname);
						err = ERRCODE_FAILURE_OPERATION;
						goto target_program_exit;
					}
					pgbar_update(1);
				}
			}
			else
			{
				// erase all in one run
				if (pf->erase_target(context, area_char, start_addr, 0))
				{
					pgbar_fini();
					LOG_ERROR(ERRMSG_FAILURE_ERASE, fullname);
					err = ERRCODE_FAILURE_OPERATION;
					goto target_program_exit;
				}
				pgbar_update(page_num);
			}
			if ((prog != NULL) && prog->peripheral_commit())
			{
				LOG_ERROR(ERRMSG_FAILURE_ERASE, fullname);
				err = ERRCODE_FAILURE_OPERATION;
				goto target_program_exit;
			}
			
			pgbar_fini();
			LOG_INFO(INFOMSG_ERASED, fullname);
			
			// Reset After Erase
			if ((area_attr & AREA_ATTR_RAE)
				&& ((op->checksum_operations != 0)
					|| (op->read_operations != 0)
					|| (op->verify_operations != 0)
					|| (op->write_operations != 0)))
			{
				if ((pf->leave_program_mode != NULL)
					&& pf->leave_program_mode(context, 0))
				{
					// no need to goto leave_program_mode here
					// operation failed IS leave_program_mode
					return ERRCODE_FAILURE_OPERATION;
				}
				sleep_ms(100);
				if (target_enter_progmode(context))
				{
					err = ERRCODE_FAILURE_OPERATION;
					goto target_program_exit;
				}
			}
		}
		
		// required to program, writable
		if ((op->write_operations & area_mask) && (area_attr & AREA_ATTR_W))
		{
			LOG_INFO(INFOMSG_PROGRAMMING, fullname);
			strcpy(str_tmp, "writing ");
			strcat(str_tmp, fullname);
			strcat(str_tmp, " |");
			pgbar_init(str_tmp, "|", 0, target_size, PROGRESS_STEP, '=');
			
			if (ml != NULL)
			{
				ml_tmp = *ml;
				while(ml_tmp != NULL)
				{
					if (area_attr & AREA_ATTR_WNP)
					{
						uint32_t tmp_addr = ml_tmp->addr;
						uint8_t *tmp_buf = ml_tmp->buff;
						
						if (pf->write_target(context, area_char,
								tmp_addr, tmp_buf, ml_tmp->len))
						{
							pgbar_fini();
							LOG_ERROR(ERRMSG_FAILURE_PROGRAM, fullname);
							err = ERRCODE_FAILURE_OPERATION;
							goto target_program_exit;
						}
					}
					else
					{
						for (j = 0; j < ml_tmp->len; j += page_size)
						{
							uint32_t tmp_addr = ml_tmp->addr + j;
							uint8_t *tmp_buf = &ml_tmp->buff[j];
							
							if (pf->write_target(context,
									area_char, tmp_addr, tmp_buf, page_size))
							{
								pgbar_fini();
								LOG_ERROR(ERRMSG_FAILURE_PROGRAM, fullname);
								err = ERRCODE_FAILURE_OPERATION;
								goto target_program_exit;
							}
							pgbar_update(page_size);
						}
					}
					ml_tmp = MEMLIST_GetNext(ml_tmp);
				}
			}
			else
			{
				uint8_t *buff_tmp;
				if (SPECIAL_STRING_CHAR == area_char)
				{
					if (NULL == prog_area->cli_str)
					{
						pgbar_fini();
						LOG_BUG(ERRMSG_INVALID_BUFFER,
								TO_STR(prog_area->cli_str));
						err = VSFERR_FAIL;
						goto target_program_exit;
					}
					buff_tmp = (uint8_t *)prog_area->cli_str;
				}
				else if (prog_area->buff != NULL)
				{
					buff_tmp = prog_area->buff;
				}
				else
				{
					LOG_ERROR(ERRMSG_INVALID_BUFFER, "prog_area->buff");
					err = VSFERR_FAIL;
					goto target_program_exit;
				}
				if (pf->write_target(context, area_char,
											start_addr, buff_tmp, target_size))
				{
					pgbar_fini();
					LOG_ERROR(ERRMSG_FAILURE_PROGRAM, fullname);
					err = ERRCODE_FAILURE_OPERATION;
					goto target_program_exit;
				}
				pgbar_update(target_size);
			}
			if ((prog != NULL) && prog->peripheral_commit())
			{
				LOG_ERROR(ERRMSG_FAILURE_PROGRAM, fullname);
				err = ERRCODE_FAILURE_OPERATION;
				goto target_program_exit;
			}
			
			time_in_ms = pgbar_fini();
			LOG_INFO(INFOMSG_PROGRAMMED_SIZE, fullname, target_size,
						(target_size / 1024.0) / (time_in_ms / 1000.0));
			
			// Reset After Write
			if ((area_attr & AREA_ATTR_RAW)
				&& ((op->checksum_operations != 0)
					|| (op->read_operations != 0)
					|| (op->verify_operations != 0)))
			{
				if ((pf->leave_program_mode != NULL)
					&& pf->leave_program_mode(context, 0))
				{
					// no need to goto leave_program_mode here
					// operation failed IS leave_program_mode
					return ERRCODE_FAILURE_OPERATION;
				}
				sleep_ms(100);
				if (target_enter_progmode(context))
				{
					err = ERRCODE_FAILURE_OPERATION;
					goto target_program_exit;
				}
			}
		}
		
		if ((op->verify_operations & area_mask)
			&& (area_attr & AREA_ATTR_V))
		{
			// specific verify defined by target
			LOG_INFO(INFOMSG_VERIFYING, fullname);
			strcpy(str_tmp, "verifying ");
			strcat(str_tmp, fullname);
			strcat(str_tmp, " |");
			pgbar_init(str_tmp, "|", 0, target_size, PROGRESS_STEP, '=');
			
			if (((page_size == 0) || (area_attr & AREA_ATTR_RNP)) &&
				(prog_area->buff != NULL))
			{
				// verify whole target area
				if (pf->read_target(context, area_char, start_addr,
										prog_area->buff, target_size))
				{
					pgbar_fini();
					LOG_ERROR(ERRMSG_FAILURE_VERIFY, fullname);
					err = ERRCODE_FAILURE_OPERATION;
					goto target_program_exit;
				}
				
				pgbar_update(target_size);
			}
			else if (ml != NULL)
			{
				// verify target area page by page
				ml_tmp = *ml;
				while(ml_tmp != NULL)
				{
					for (j = 0; j < ml_tmp->len; j += page_size)
					{
						uint32_t tmp_addr = ml_tmp->addr + j;
						uint8_t *tmp_buf = &ml_tmp->buff[j];
						
						if (pf->read_target(context, area_char,
								tmp_addr, tmp_buf, page_size))
						{
							pgbar_fini();
							LOG_ERROR(ERRMSG_FAILURE_READ, fullname);
							err = ERRCODE_FAILURE_OPERATION;
							goto target_program_exit;
						}
						pgbar_update(page_size);
					}
					ml_tmp = MEMLIST_GetNext(ml_tmp);
				}
			}
			else
			{
				return VSFERR_FAIL;
			}
			if ((prog != NULL) && prog->peripheral_commit())
			{
				LOG_ERROR(ERRMSG_FAILURE_READ, fullname);
				err = ERRCODE_FAILURE_OPERATION;
				goto target_program_exit;
			}
			
			pgbar_fini();
			
			if (op->verify_operations & area_mask)
			{
				LOG_INFO(INFOMSG_VERIFIED_SIZE, fullname, target_size,
							(target_size / 1024.0) / (time_in_ms / 1000.0));
			}
			else
			{
				LOG_INFO(INFOMSG_READ_SIZE, fullname, target_size,
							(target_size / 1024.0) / (time_in_ms / 1000.0));
			}
		}
		else if (((op->read_operations & area_mask)
					|| (op->verify_operations & area_mask))
				&& (area_attr & AREA_ATTR_R))
		{
			if ((p_map[i].data_pos) && (op->read_operations & area_mask) &&
				(NULL == *ml))
			{
				if (MEMLIST_Add(ml, area_info->addr, area_info->size,
								page_size, prog_area->buff))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "add memory list");
					err = ERRCODE_FAILURE_OPERATION;
					goto target_program_exit;
				}
				target_size = MEMLIST_CalcAllSize(*ml);
			}
			
			if (op->verify_operations & area_mask)
			{
				LOG_INFO(INFOMSG_VERIFYING, fullname);
			}
			else
			{
				LOG_INFO(INFOMSG_READING, fullname);
			}
			strcpy(str_tmp, "reading ");
			strcat(str_tmp, fullname);
			strcat(str_tmp, " |");
			pgbar_init(str_tmp, "|", 0, target_size, PROGRESS_STEP, '=');
			
			if (ml != NULL)
			{
				ml_tmp = *ml;
				while(ml_tmp != NULL)
				{
					uint8_t *read_buf = NULL, *buff_tmp = ml_tmp->buff;
					uint32_t buf_size;
					uint32_t err_pos;
					
#if SYS_CFG_LARGE_MEMORY
					if (!(area_attr & AREA_ATTR_RNP)
						&& (ml_tmp->len % page_size))
					{
						buf_size = ((ml_tmp->len / page_size) + 1) * page_size;
					}
					else
					{
						buf_size = ml_tmp->len;
					}
#else
					buf_size = page_size;
#endif
					read_buf = (uint8_t*)malloc(buf_size);
					if (NULL == read_buf)
					{
						pgbar_fini();
						LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
						err = VSFERR_NOT_ENOUGH_RESOURCES;
						goto target_program_exit;
					}
					
#if SYS_CFG_LARGE_MEMORY
					if (area_attr & AREA_ATTR_RNP)
					{
						if (pf->read_target(context, area_char,
								ml_tmp->addr, read_buf, ml_tmp->len))
						{
							free(read_buf);
							read_buf = NULL;
							pgbar_fini();
							LOG_ERROR(ERRMSG_FAILURE_READ, fullname);
							err = ERRCODE_FAILURE_OPERATION;
							goto target_program_exit;
						}
					}
					else
#endif
					{
						for (j = 0; j < ml_tmp->len; j += page_size)
						{
							if (pf->read_target(context,
									area_char, ml_tmp->addr + j,
#if SYS_CFG_LARGE_MEMORY
									read_buf + j,
#else
									read_buf,
#endif
									page_size))
							{
								free(read_buf);
								read_buf = NULL;
								pgbar_fini();
								LOG_ERROR(ERRMSG_FAILURE_READ, fullname);
								err = ERRCODE_FAILURE_OPERATION;
								goto target_program_exit;
							}
#if !SYS_CFG_LARGE_MEMORY
							if ((prog != NULL) && prog->peripheral_commit())
							{
								pgbar_fini();
								LOG_ERROR(ERRMSG_FAILURE_READ, fullname);
								err = ERRCODE_FAILURE_OPERATION;
								goto target_program_exit;
							}
							
							if (op->verify_operations & area_mask)
							{
								// verify according to ml_exact
								if (NULL == ml_exact)
								{
									pgbar_fini();
									LOG_BUG(ERRMSG_INVALID_BUFFER, "ml_exact");
									err = VSFERR_FAIL;
									goto target_program_exit;
								}
								err_pos = 0;
								if (MEMLIST_VerifyBuff(ml_exact, read_buf,
											&buff_tmp[j], ml_tmp->addr + j,
											page_size, &err_pos))
								{
									pgbar_fini();
									LOG_ERROR(ERRMSG_FAILURE_VERIFY_AT_02X,
											fullname,
											ml_tmp->addr + j + err_pos,
											read_buf[err_pos],
											buff_tmp[j + err_pos]);
									free(read_buf);
									read_buf = NULL;
									err = ERRCODE_FAILURE_VERIFY;
									goto target_program_exit;
								}
							}
							else
							{
								memcpy(&buff_tmp[j], read_buf, page_size);
							}
#endif
							pgbar_update(page_size);
						}
					}
#if SYS_CFG_LARGE_MEMORY
					if ((prog != NULL) && prog->peripheral_commit())
					{
						LOG_ERROR(ERRMSG_FAILURE_READ, fullname);
						err = ERRCODE_FAILURE_OPERATION;
						goto target_program_exit;
					}
					
					if (op->verify_operations & area_mask)
					{
						// verify according to ml_exact
						if (NULL == ml_exact)
						{
							pgbar_fini();
							LOG_BUG(ERRMSG_INVALID_BUFFER, "ml_exact");
							err = VSFERR_FAIL;
							goto target_program_exit;
						}
						err_pos = 0;
						if (MEMLIST_VerifyBuff(ml_exact, read_buf,
									buff_tmp, ml_tmp->addr,
									ml_tmp->len, &err_pos))
						{
							pgbar_fini();
							LOG_ERROR(ERRMSG_FAILURE_VERIFY_AT_02X,
										fullname, ml_tmp->addr + err_pos,
										read_buf[err_pos],
										buff_tmp[err_pos]);
							free(read_buf);
							read_buf = NULL;
							err = ERRCODE_FAILURE_VERIFY;
							goto target_program_exit;
						}
					}
					else
					{
						memcpy(buff_tmp, read_buf, ml_tmp->len);
					}
#endif
					free(read_buf);
					read_buf = NULL;
					
					ml_tmp = MEMLIST_GetNext(ml_tmp);
				}
				time_in_ms = pgbar_fini();
			}
			else
			{
				uint8_t *buff_tmp;
				uint8_t alloced = 0;
				
				if (SPECIAL_STRING_CHAR == area_char)
				{
					buff_tmp = special_string;
				}
				else if (prog_area->buff != NULL)
				{
					buff_tmp = prog_area->buff;
				}
				else
				{
					buff_tmp = (uint8_t*)malloc(target_size);
					if (NULL == buff_tmp)
					{
						LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
						err = VSFERR_NOT_ENOUGH_RESOURCES;
						goto target_program_exit;
					}
					memset(buff_tmp, 0, target_size);
					alloced = 1;
				}
				if (pf->read_target(context, area_char,
							start_addr, buff_tmp, target_size))
				{
					pgbar_fini();
					LOG_ERROR(ERRMSG_FAILURE_READ, fullname);
					err = ERRCODE_FAILURE_OPERATION;
					goto target_program_exit;
				}
				if ((prog != NULL) && prog->peripheral_commit())
				{
					LOG_ERROR(ERRMSG_FAILURE_READ, fullname);
					err = ERRCODE_FAILURE_OPERATION;
					goto target_program_exit;
				}
				pgbar_update(target_size);
				time_in_ms = pgbar_fini();
				
				if (op->verify_operations & area_mask)
				{
					if (SPECIAL_STRING_CHAR == area_char)
					{
						if (!strcmp((const char*)prog_area->cli_str,
									(const char*)special_string))
						{
							LOG_INFO(INFOMSG_VERIFIED, fullname);
						}
						else
						{
							LOG_ERROR(ERRMSG_FAILURE_VERIFY_STR,
								fullname, special_string, prog_area->cli_str);
							err = ERRCODE_FAILURE_VERIFY;
							goto target_program_exit;
						}
					}
					else
					{
						if (!memcmp(buff_tmp, prog_area->buff, target_size))
						{
							LOG_INFO(INFOMSG_VERIFIED, fullname);
						}
						else
						{
							char *read_str, *want_str;
							if (NULL == format)
							{
								LOG_BUG(ERRMSG_INVALID_BUFFER, "format");
								err = VSFERR_FAIL;
								goto target_program_exit;
							}
							read_str = strparser_solve(format, buff_tmp, 0);
							if (NULL == read_str)
							{
								LOG_ERROR(ERRMSG_FAILURE_OPERATION,
											"solve value");
								err = ERRCODE_FAILURE_OPERATION;
								goto target_program_exit;
							}
							want_str = strparser_solve(format, prog_area->buff,
														0);
							if (NULL == want_str)
							{
								free(read_str);
								read_str = NULL;
								LOG_ERROR(ERRMSG_FAILURE_OPERATION,
											"solve value");
								err = ERRCODE_FAILURE_OPERATION;
								goto target_program_exit;
							}
							LOG_ERROR(ERRMSG_FAILURE_VERIFY_STR,
								fullname, read_str, want_str);
							free(read_str);
							read_str = NULL;
							free(want_str);
							want_str = NULL;
							err = ERRCODE_FAILURE_VERIFY;
							goto target_program_exit;
						}
					}
				}
				else
				{
					if (SPECIAL_STRING_CHAR == area_char)
					{
						LOG_INFO(INFOMSG_TARGET_READ, fullname, special_string);
					}
					else
					{
						char *read_str;
						if (NULL == format)
						{
							LOG_BUG(ERRMSG_INVALID_BUFFER, "format");
							err = VSFERR_FAIL;
							goto target_program_exit;
						}
						read_str = strparser_solve(format, buff_tmp, 0);
						if (NULL == read_str)
						{
							LOG_ERROR(ERRMSG_FAILURE_OPERATION, "solve value");
							err = ERRCODE_FAILURE_OPERATION;
							goto target_program_exit;
						}
						LOG_INFO(INFOMSG_TARGET_READ, fullname, read_str);
						free(read_str);
						read_str = NULL;
					}
				}
				if (alloced && (buff_tmp != NULL))
				{
					free(buff_tmp);
					buff_tmp = NULL;
				}
			}
			
			if (op->verify_operations & area_mask)
			{
				LOG_INFO(INFOMSG_VERIFIED_SIZE, fullname, target_size,
							(target_size / 1024.0) / (time_in_ms / 1000.0));
			}
			else
			{
				LOG_INFO(INFOMSG_READ_SIZE, fullname, target_size,
							(target_size / 1024.0) / (time_in_ms / 1000.0));
			}
		}
		i++;
	}
	
target_program_exit:
	return err;
}

static vsf_err_t target_init(struct program_context_t *context)
{
	uint16_t i;
	int8_t area_idx;
	char mode_buff[4];
	struct chip_area_info_t *area_info = NULL;
	struct program_area_t *prog_area = NULL;
	struct program_info_t *pi = context->pi;
	struct chip_param_t *param = context->param;
	struct target_info_t *target = context->target;
	struct chip_series_t *series = context->series;
	
	LOG_PUSH();
	LOG_MUTE();
	sprintf(mode_buff, "%d", pi->mode);
	vss_call_notifier(target->notifier, "mode", mode_buff);
	LOG_POP();
	pi->mode_char = target->program_mode[pi->mode].name;
	
	if (NULL == pi->chip_name)
	{
		if (strchr(target->feature, NO_TARGET[0]) != NULL)
		{
			return VSFERR_NONE;
		}
		else if (NULL == strchr(target->feature, AUTO_DETECT[0]))
		{
			LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, "Auto-detect",
						target->name);
			return VSFERR_NOT_SUPPORT;
		}
		
		// auto detect
		if (param->chip_areas != NULL)
		{
			target_chip_area_free(param->chip_areas);
			param->chip_areas = NULL;
		}
		memcpy(param, &series->chips_param[0], sizeof(*param));
		param->chip_areas =
			target_chip_area_dup(series->chips_param[0].chip_areas);
		
		if (series->num_of_chips > 1)
		{
			struct operation_t opt_tmp, *opt_orig;
			
			LOG_INFO(INFOMSG_TRY_AUTODETECT);
			
			memset(&opt_tmp, 0, sizeof(opt_tmp));
			opt_tmp.read_operations = CHIPID;
			opt_orig = context->op;
			context->op = &opt_tmp;
			if (target_enter_progmode(context))
			{
				context->op = opt_orig;
				LOG_ERROR(ERRMSG_AUTODETECT_FAIL, pi->chip_type);
				return ERRCODE_AUTODETECT_FAIL;
			}
			if (target_program(context))
			{
				context->op = opt_orig;
				target_leave_progmode(context, 0);
				LOG_ERROR(ERRMSG_AUTODETECT_FAIL, pi->chip_type);
				return ERRCODE_AUTODETECT_FAIL;
			}
			if (target_leave_progmode(context, 1))
			{
				context->op = opt_orig;
				LOG_ERROR(ERRMSG_AUTODETECT_FAIL, pi->chip_type);
				return ERRCODE_AUTODETECT_FAIL;
			}
			context->op = opt_orig;
			
			// insert a dly between 2 operations
			sleep_ms(100);
			
			LOG_INFO(INFOMSG_AUTODETECT_SIGNATURE, pi->chip_id);
			for (i = 0; i < series->num_of_chips; i++)
			{
				if (pi->chip_id == series->chips_param[i].chip_id)
				{
					pi->chip_name = strdup(series->chips_param[i].chip_name);
					if (NULL == pi->chip_name)
					{
						LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
						return VSFERR_NOT_ENOUGH_RESOURCES;
					}
					LOG_INFO(INFOMSG_CHIP_FOUND, pi->chip_name);
					
					goto Post_Init;
				}
			}
		}
		else
		{
			i = 0;
			goto Post_Init;
		}
		
		LOG_ERROR(ERRMSG_AUTODETECT_FAIL, pi->chip_type);
		return ERRCODE_AUTODETECT_FAIL;
	}
	else
	{
		for (i = 0; i < series->num_of_chips; i++)
		{
			if (!strcmp(series->chips_param[i].chip_name, pi->chip_name))
			{
				goto Post_Init;
			}
		}
		
		return VSFERR_FAIL;
	}
Post_Init:
	if ((target->adjust_setting != NULL)
		&& target->adjust_setting(pi, &series->chips_param[i], pi->mode))
	{
		return VSFERR_FAIL;
	}
	
	if (param->chip_areas != NULL)
	{
		target_chip_area_free(param->chip_areas);
		param->chip_areas = NULL;
	}
	memcpy(param, &series->chips_param[i], sizeof(*param));
	param->chip_areas =
			target_chip_area_dup(series->chips_param[i].chip_areas);
	
	i = 0;
	while (target->program_area_map[i].name != 0)
	{
		area_idx = target_area_idx(target->program_area_map[i].name);
		if (area_idx < 0)
		{
			return VSFERR_FAIL;
		}
		area_info = target_get_chip_area(param, (uint32_t)area_idx);
		prog_area = target_get_program_area(pi, (uint32_t)area_idx);
		if ((NULL == area_info) || (NULL == prog_area))
		{
			i++;
			continue;
		}
		
		if (!prog_area->size)
		{
			// if size is not detected, copy from record in config
			prog_area->size = area_info->size;
		}
		else
		{
			// if size is detected, overwrite target parameters
			area_info->size = prog_area->size;
		}
		i++;
	}
	
	return VSFERR_NONE;
}

static uint32_t target_prepare_operation(struct program_context_t *context,
											uint32_t *operation)
{
	uint32_t i;
	uint32_t ret;
	struct program_area_map_t *a;
	
	a = (struct program_area_map_t *)context->target->program_area_map;
	if (*operation & ALL)
	{
		i = 0;
		while (a[i].name != 0)
		{
			*operation |= target_area_mask(a[i].name);
			i++;
		}
	}
	
	ret = 0;
	i = 0;
	while (a[i].name != 0)
	{
		if (*operation & target_area_mask(a[i].name))
		{
			ret += a[i].data_pos;
		}
		i++;
	}
	return ret;
}

vsf_err_t target_prepare_operations(struct program_context_t *context,
									uint32_t *readfile, uint32_t *writefile)
{
	uint32_t readfile_local, writefile_local;
	struct operation_t *op;
	
	if ((NULL == context) || (NULL == context->target) ||
		(NULL == context->target->program_area_map) || (NULL == context->op))
	{
		return VSFERR_FAIL;
	}
	op = context->op;
	
	readfile_local = writefile_local = 0;
	target_prepare_operation(context, &op->erase_operations);
	readfile_local += target_prepare_operation(context, &op->write_operations);
	readfile_local += target_prepare_operation(context, &op->verify_operations);
	writefile_local += target_prepare_operation(context, &op->read_operations);
	
	if (readfile != NULL)
	{
		*readfile = readfile_local;
	}
	if (writefile != NULL)
	{
		*writefile = writefile_local;
	}
	
	return VSFERR_NONE;
}

static void target_print_single_memory(struct program_context_t *context,
										char type)
{
	uint32_t mapidx;
	int8_t paramidx;
	char *full_type = target_area_fullname(type);
	struct program_area_map_t *p_map;
	struct chip_area_info_t *area_info = NULL;
	struct program_info_t *pi = context->pi;
	
	p_map = (struct program_area_map_t *)context->target->program_area_map;
	mapidx = 0;
	while ((p_map[mapidx].name != 0) && (p_map[mapidx].name != type))
	{
		mapidx++;
	}
	if (0 == p_map[mapidx].name)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, full_type, pi->chip_name);
		return;
	}
	
	paramidx = target_area_idx(type);
	if (paramidx < 0)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, full_type, pi->chip_name);
		return;
	}
	area_info = target_get_chip_area(context->param, (uint32_t)paramidx);
	if (NULL == area_info)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, full_type, pi->chip_name);
		return;
	}
	
	PRINTF("%s of %s:" LOG_LINE_END, full_type, pi->chip_name);
	if (p_map[mapidx].data_pos)
	{
		PRINTF("%c_seg = 0x%08X, ", type, area_info->seg);
		PRINTF("%c_addr = 0x%08X, ", type, area_info->addr);
	}
	else if (area_info->cli_format != NULL)
	{
		PRINTF("%c_format = %s, ", type, area_info->cli_format);
	}
	PRINTF("%c_default = 0x%"PRIX64", ", type, area_info->default_value);
	PRINTF("%c_bytelen = %d" LOG_LINE_END, type, (int)area_info->size);
}

void target_print_memory(struct program_context_t *context, char type)
{
	uint8_t i;
	struct target_info_t *target = context->target;
	
	if (NULL == target)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
		return;
	}
	
	if (type > 0)
	{
		target_print_single_memory(context, type);
	}
	else
	{
		i = 0;
		while (target->program_area_map[i].name != 0)
		{
			target_print_single_memory(context, target->program_area_map[i].name);
			i++;
		}
	}
}

void target_print_setting(struct program_context_t *context, char type)
{
	struct chip_fl_t fl;
	uint32_t i, j;
	char *full_type = target_area_fullname(type);
	struct program_area_map_t *p_map;
	struct target_info_t *target = context->target;
	struct program_info_t *pi = context->pi;
	
	if (NULL == full_type)
	{
		LOG_BUG(ERRMSG_INVALID_TARGET, "target");
		return;
	}
	
	p_map = (struct program_area_map_t *)target->program_area_map;
	i = 0;
	while ((p_map[i].name != 0) && (p_map[i].name != type))
	{
		i++;
	}
	if (0 == p_map[i].name)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, full_type, pi->chip_name);
		return;
	}
	
	memset(&fl, 0, sizeof(struct chip_fl_t));
	if (target_build_chip_fl(target, pi->chip_name, full_type, &fl))
	{
		target_release_chip_fl(&fl);
		LOG_ERROR(ERRMSG_INVALID_TGTCFG_SETTING, pi->chip_name);
		return;
	}
	
	// print fl
	PRINTF("%s of %s:" LOG_LINE_END, full_type, pi->chip_name);
	PRINTF("init = %s, ", fl.init_value);
	PRINTF("num_of_warnings = %d, ", fl.num_of_fl_warnings);
	PRINTF("num_of_settings = %d" LOG_LINE_END, fl.num_of_fl_settings);
	for (i = 0; i < fl.num_of_fl_warnings; i++)
	{
		PRINTF("warning: mask = %s, ", fl.warnings[i].mask);
		PRINTF("value = %s, ", fl.warnings[i].value);
		PRINTF("msg = %s, ", fl.warnings[i].msg);
		PRINTF("ban = %d" LOG_LINE_END, fl.warnings[i].ban);
	}
	for (i = 0; i < fl.num_of_fl_settings; i++)
	{
		PRINTF("setting: name = %s, ", fl.settings[i].name);
		PRINTF("mask = %s, ", fl.settings[i].mask);
		PRINTF("num_of_choices = %d", fl.settings[i].num_of_choices);
		if (fl.settings[i].ban != NULL)
		{
			PRINTF(", ban = %s", fl.settings[i].ban);
		}
		if (fl.settings[i].info != NULL)
		{
			PRINTF(", info = %s", fl.settings[i].info);
		}
		if (fl.settings[i].format != NULL)
		{
			PRINTF(", format = %s", fl.settings[i].format);
		}
		if (fl.settings[i].use_checkbox)
		{
			PRINTF(", checked = %s", fl.settings[i].checked);
			PRINTF(", unchecked = %s", fl.settings[i].unchecked);
		}
		else if (fl.settings[i].use_edit)
		{
			PRINTF(", radix = %d", fl.settings[i].radix);
			PRINTF(", shift = %d", fl.settings[i].shift);
			PRINTF(", bytelen = %d", fl.settings[i].bytelen);
		}
		PRINTF(LOG_LINE_END);
		for (j = 0; j < fl.settings[i].num_of_choices; j++)
		{
			PRINTF("choice: value = %s, ", fl.settings[i].choices[j].value);
			PRINTF("text = %s" LOG_LINE_END, fl.settings[i].choices[j].text);
		}
	}
	
	target_release_chip_fl(&fl);
}

void target_print_target(uint32_t index)
{
	uint32_t i, j;
	struct chip_series_t series;
	struct chip_param_t *p_param;
	struct program_area_map_t *p_map;
	char area[3];
	
	memset(&series, 0, sizeof(series));
	if (target_build_chip_series((struct target_info_t *)&targets_info[index],
			targets_info[index].program_mode, &series))
	{
		target_release_chip_series(&series);
		LOG_ERROR(ERRMSG_INVALID_TGTCFG_SETTING, targets_info[index].name);
		return;
	}
	
	if (0 == series.num_of_chips)
	{
		return;
	}
	
	if (strlen(targets_info[index].feature) > 0)
	{
		PRINTF("Support list of %s(%s):", targets_info[index].name,
				targets_info[index].feature);
	}
	else
	{
		PRINTF("Support list of %s:", targets_info[index].name);
	}
	// fake
	p_map = (struct program_area_map_t *)targets_info[index].program_area_map;
	i = 0;
	while (p_map[i].name != 0)
	{
		if (p_map[i].fseg_addr)
		{
			PRINTF(" %c_fseg = 0x%X,", p_map[i].name, p_map[i].fseg_addr);
		}
		if (p_map[i].fstart_addr)
		{
			PRINTF(" %c_faddr = 0x%X,", p_map[i].name, p_map[i].fstart_addr);
		}
		i++;
	}
	// extra info from target
	if (!vss_cmd_supported_by_notifier(targets_info[index].notifier, "extra"))
	{
		vss_call_notifier(targets_info[index].notifier, "extra", NULL);
	}
	PRINTF(LOG_LINE_END);
	
	// Targets based on ComPort outputs there special COM settings
	if (strchr(targets_info[index].feature, 'C') != NULL)
	{
		vss_call_notifier(targets_info[index].notifier, "support", NULL);
	}
	else
	{
		for (i = 0; i < series.num_of_chips; i++)
		{
			p_param = &series.chips_param[i];
			
			// name
			PRINTF("%s:", p_param->chip_name);
			// id
			PRINTF(" id = 0x%X,", p_param->chip_id);
			// mode
			if (p_param->program_mode_str != NULL)
			{
				PRINTF(" mode = %s,", p_param->program_mode_str);
			}
			// area
			PRINTF(" area = ");
			area[2] = 0;
			j = 0;
			while (p_map[j].name != 0)
			{
				area[0] = p_map[j].name;
				area[1] = p_map[j].data_pos + '0';
				PRINTF("%s", area);
				j++;
			}
			PRINTF(LOG_LINE_END);
		}
		PRINTF(LOG_LINE_END);
	}
	
	target_release_chip_series(&series);
}

void target_print_list(void)
{
	uint32_t i;
	
	PRINTF(_GETTEXT("Supported targets:" LOG_LINE_END));
	for (i = 0; targets_info[i].name != NULL; i++)
	{
		target_print_target(i);
	}
}

void target_print_help(void)
{
	uint32_t i;
	
	for (i = 0; targets_info[i].name != NULL; i++)
	{
		vss_call_notifier(targets_info[i].notifier, "help", NULL);
	}
}

static vsf_err_t target_probe_chip(struct chip_series_t *series, char *chip_name)
{
	uint32_t i;
	
	if (NULL == chip_name)
	{
		return VSFERR_FAIL;
	}
	
	for (i = 0; i < series->num_of_chips; i++)
	{
		if (NULL == series->chips_param[i].chip_name)
		{
			continue;
		}
		
		if (!strcmp(series->chips_param[i].chip_name, chip_name))
		{
			return VSFERR_NONE;
		}
	}
	
	return VSFERR_FAIL;
}

static vsf_err_t target_info_init(struct program_context_t *context)
{
	uint32_t i;
	vsf_err_t (*probe_chip)(struct chip_series_t *series, char *chip_name);
	struct program_info_t *pi = context->pi;
	struct chip_series_t *series = context->series;
	
#if PARAM_CHECK
	if ((NULL == pi) || ((NULL == pi->chip_name) && (NULL == pi->chip_type)))
	{
		LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
		return VSFERR_INVALID_PARAMETER;
	}
#endif
	
	target_release_chip_series(series);
	
	if (NULL == pi->chip_type)
	{
		// find which series of target contain current chip_name
		for (i = 0; targets_info[i].name != NULL; i++)
		{
			if (!target_build_chip_series(
					(struct target_info_t *)&targets_info[i],
					targets_info[i].program_mode, series))
			{
				// configuration file exists, use default probe function
				probe_chip = target_probe_chip;
			}
			else
			{
				// use probe function defined by target chip
				continue;
			}
			
			if (!probe_chip(series, pi->chip_name))
			{
				context->target = (struct target_info_t *)&targets_info[i];
				pi->chip_type = strdup(targets_info[i].name);
				if (NULL == pi->chip_type)
				{
					LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
					return VSFERR_NOT_ENOUGH_RESOURCES;
				}
				LOG_DEBUG("%s initialized for %s.", context->target->name,
							pi->chip_name);
				
				return VSFERR_NONE;
			}
			target_release_chip_series(series);
		}
		
		LOG_ERROR(ERRMSG_NOT_SUPPORT, pi->chip_name);
	}
	else
	{
		// find current series of chip_type
		for (i = 0; targets_info[i].name != NULL; i++)
		{
			if (!strcmp(targets_info[i].name, pi->chip_type))
			{
				if (!target_build_chip_series(
						(struct target_info_t *)&targets_info[i],
						targets_info[i].program_mode, series))
				{
					// configuration file exists, use default probe function
					probe_chip = target_probe_chip;
				}
				else if (strchr(targets_info[i].feature, NO_TARGET[0]) != NULL)
				{
					context->target = (struct target_info_t *)&targets_info[i];
					return VSFERR_NONE;
				}
				else
				{
					LOG_BUG(ERRMSG_NOT_SUPPORT_BY, "probe_chip",
							targets_info[i].name);
					return VSFERR_FAIL;
				}
				
				if ((pi->chip_name != NULL)
					&& probe_chip(series, pi->chip_name))
				{
					LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, pi->chip_name,
								targets_info[i].name);
					target_release_chip_series(series);
					context->target = NULL;
					return VSFERR_NOT_SUPPORT;
				}
				else
				{
					context->target = (struct target_info_t *)&targets_info[i];
					LOG_DEBUG("%s initialized.", cur_context->target->name);
					return VSFERR_NONE;
				}
			}
		}
		
		LOG_ERROR(ERRMSG_NOT_SUPPORT, pi->chip_type);
	}
	
	context->target = NULL;
	return VSFERR_NOT_SUPPORT;
}

VSS_HANDLER(target_memory_detail)
{
	char target_char;
	struct target_info_t *target;
	struct program_info_t *pi;
	
	VSS_CHECK_ARGC(2);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	target = cur_context->target;
	pi = cur_context->pi;
	if (((NULL == pi->chip_name) || (NULL == target))
		&& (NULL == pi->chip_type))
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
		return VSFERR_FAIL;
	}
	if (((NULL == pi->chip_name) || (NULL == target))
		&& (NULL != pi->chip_type))
	{
		pi->chip_name = strdup(pi->chip_type);
		if (NULL == pi->chip_name)
		{
			LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
			return VSFERR_NOT_ENOUGH_RESOURCES;
		}
		
		target_info_init(cur_context);
		if (NULL == target)
		{
			LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
			return VSFERR_FAIL;
		}
	}
	
	if (!strcmp(argv[1], "all"))
	{
		target_char = 0;
	}
	else
	{
		target_char = target_area_char_by_fullname((char *)argv[1]);
		if (0 == target_char)
		{
			LOG_ERROR(ERRMSG_NOT_SUPPORT, argv[1]);
			return VSFERR_FAIL;
		}
	}
	
	if (target_init(cur_context))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "initialize target");
		return VSFERR_FAIL;
	}
	
	target_print_memory(cur_context, target_char);
	return VSFERR_NONE;
}

VSS_HANDLER(target_parameter_detail)
{
	char target_char;
	
	VSS_CHECK_ARGC(2);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	if ((NULL == cur_context->pi->chip_name)
		|| (NULL == cur_context->pi->chip_type)
		|| (NULL == cur_context->target))
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
		return VSFERR_FAIL;
	}
	
	if (strlen(argv[1]) > 1)
	{
		target_char = target_area_char_by_fullname((char *)argv[1]);
		if (0 == target_char)
		{
			LOG_ERROR(ERRMSG_NOT_SUPPORT, argv[1]);
			return VSFERR_FAIL;
		}
	}
	else
	{
		target_char = argv[1][0];
	}
	
	if (target_init(cur_context))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "initialize target");
		return VSFERR_FAIL;
	}
	
	target_print_setting(cur_context, target_char);
	return VSFERR_NONE;
}

VSS_HANDLER(target_series)
{
	struct program_info_t *pi;
	
	VSS_CHECK_ARGC(2);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	pi = cur_context->pi;
	
	if (pi->chip_type != NULL)
	{
		free(pi->chip_type);
		pi->chip_type = NULL;
	}
	pi->chip_type = strdup(argv[1]);
	if (NULL == pi->chip_type)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return VSFERR_NOT_ENOUGH_RESOURCES;
	}
	
	if (target_info_init(cur_context))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "initialize target: ", argv[1]);
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

VSS_HANDLER(target_chip)
{
	struct program_info_t *pi;
	
	VSS_CHECK_ARGC(2);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	pi = cur_context->pi;
	
	if (pi->chip_name != NULL)
	{
		free(pi->chip_name);
		pi->chip_name = NULL;
	}
	pi->chip_name = strdup(argv[1]);
	if (NULL == pi->chip_name)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return VSFERR_NOT_ENOUGH_RESOURCES;
	}
	
	if (target_info_init(cur_context))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "initialize target: ", argv[1]);
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

VSS_HANDLER(target_value)
{
	struct program_area_t *prog_area = NULL;
	char *dest = NULL;
	int8_t target_idx;
	
	VSS_CHECK_ARGC(2);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	
	if (strlen(argv[1]) < 2)
	{
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);
		vss_print_help(argv[0]);
		return VSFERR_FAIL;
	}
	target_idx = (int)target_area_idx(argv[1][0]);
	if (target_idx < 0)
	{
		LOG_ERROR(ERRMSG_INVALID_CHARACTER, argv[1][0], "target");
		return VSFERR_FAIL;
	}
	prog_area = target_get_program_area(cur_context->pi, (uint32_t)target_idx);
	if (NULL == prog_area)
	{
		LOG_ERROR(ERRMSG_INVALID_CHARACTER, argv[1][0], "target");
		return VSFERR_FAIL;
	}
	dest = strdup(&argv[1][1]);
	if (NULL == dest)
	{
		return VSFERR_FAIL;
	}
	if (NULL != prog_area->cli_str)
	{
		free(prog_area->cli_str);
		prog_area->cli_str = NULL;
	}
	prog_area->cli_str = dest;
	cur_context->pi->areas_defined |= target_area_name[target_idx].mask;
	return VSFERR_NONE;
}

VSS_HANDLER(target_interface_frequency)
{
	VSS_CHECK_ARGC(2);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	
	cur_context->pi->frequency = (uint16_t)strtoul(argv[1], NULL, 0);
	return VSFERR_NONE;
}

VSS_HANDLER(target_kernel_khz)
{
	VSS_CHECK_ARGC(2);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	
	cur_context->pi->kernel_khz = (uint32_t)strtoul(argv[1], NULL, 0);
	return VSFERR_NONE;
}

VSS_HANDLER(target_quartz_khz)
{
	VSS_CHECK_ARGC(2);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	
	cur_context->pi->quartz_khz = (uint32_t)strtoul(argv[1], NULL, 0);
	return VSFERR_NONE;
}

VSS_HANDLER(target_erase_on_demand)
{
	uint32_t tmp;
	
	VSS_CHECK_ARGC_2(1, 2);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	
	if (1 == argc)
	{
		cur_context->pi->erase_on_demand = true;
	}
	else
	{
		tmp = strtoul(argv[1], NULL, 0);
		if (tmp)
		{
			cur_context->pi->erase_on_demand = true;
		}
		else
		{
			cur_context->pi->erase_on_demand = false;
		}
	}
	return VSFERR_NONE;
}

VSS_HANDLER(target_wait_state)
{
	VSS_CHECK_ARGC(2);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	
	cur_context->pi->wait_state = (uint8_t)strtoul(argv[1], NULL, 0);
	return VSFERR_NONE;
}

VSS_HANDLER(target_auto_adjust)
{
	VSS_CHECK_ARGC(1);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	
	cur_context->pi->auto_adjust = 1;
	return VSFERR_NONE;
}

VSS_HANDLER(target_address)
{
	VSS_CHECK_ARGC(2);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	
	cur_context->pi->chip_address = (uint32_t)strtoul(argv[1], NULL, 0);
	return VSFERR_NONE;
}

VSS_HANDLER(target_jtag_dc)
{
	// UB(1) UA(1) BB(2) BA(2)
	uint8_t buff[6];
	char format[] = "%1d%1d%2d%2d";
	
	VSS_CHECK_ARGC(2);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	
	if (strparser_parse((char *)argv[1], format, buff, sizeof(buff)))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_AS, argv[1], format);
		return VSFERR_FAIL;
	}
	cur_context->pi->jtag_pos.ub = buff[0];
	cur_context->pi->jtag_pos.ua = buff[1];
	cur_context->pi->jtag_pos.bb = buff[2] + (buff[3] << 8);
	cur_context->pi->jtag_pos.ba = buff[4] + (buff[5] << 8);
	return VSFERR_NONE;
}

VSS_HANDLER(target_interface_mode)
{
	int8_t mode;
	
	VSS_CHECK_ARGC(2);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	if (NULL == cur_context->target)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "target");
		return VSFERR_FAIL;
	}
	if (strlen(argv[1]) != 1)
	{
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);
		vss_print_help(argv[0]);
		return VSFERR_FAIL;
	}
	mode = target_mode_get_idx(cur_context->target->program_mode, argv[1][0]);
	if (mode < 0)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, argv[1], cur_context->target->name);
		return VSFERR_FAIL;
	}
	
	cur_context->pi->mode = (uint8_t)mode;
	return VSFERR_NONE;
}

VSS_HANDLER(target_execute_addr)
{
	VSS_CHECK_ARGC_2(1, 2);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	
	if (1 == argc)
	{
		cur_context->pi->execute_addr = 0;
		cur_context->pi->execute_flag = 0;
	}
	else
	{
		cur_context->pi->execute_addr = (uint32_t)strtoul(argv[1], NULL, 0);
		cur_context->pi->execute_flag = 1;
	}
	
	return VSFERR_NONE;
}

VSS_HANDLER(target_raw_mode)
{
	VSS_CHECK_ARGC_2(1, 2);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	
	if (1 == argc)
	{
		cur_context->pi->raw = true;
	}
	else
	{
		cur_context->pi->raw = ((uint32_t)strtoul(argv[1], NULL, 0)) > 0;
	}
	
	return VSFERR_NONE;
}

VSS_HANDLER(target_prepare)
{
	VSS_CHECK_ARGC(1);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	if (NULL == cur_context->target)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
		return VSFERR_FAIL;
	}
	
	// init target
	if (target_init(cur_context))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "initialize target");
		return VSFERR_FAIL;
	}
	
	if (target_data_read(cur_context))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "initialize target data");
		return VSFERR_FAIL;
	}
	if (target_parse_cli_string(cur_context))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse cli_string");
		return VSFERR_FAIL;
	}
	
	if (target_check_defined(cur_context))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "check target defined content");
		return VSFERR_FAIL;
	}
	
	return VSFERR_NONE;
}

VSS_HANDLER(target_enter_program_mode)
{
	VSS_CHECK_ARGC(1);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	if (NULL == cur_context->target)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
		return VSFERR_FAIL;
	}
	
	if (target_enter_progmode(cur_context))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATE_DEVICE, cur_context->target->name);
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

VSS_HANDLER(target_leave_program_mode)
{
	uint8_t success;
	
	VSS_CHECK_ARGC(2);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	if (NULL == cur_context->target)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
		return VSFERR_FAIL;
	}
	success = (uint8_t)strtoul(argv[1], NULL, 0);
	
	if (target_leave_progmode(cur_context, success))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATE_DEVICE, cur_context->target->name);
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

VSS_HANDLER(target_erase)
{
	struct operation_t operations_tmp, *operations_orig;
	int8_t index;
	vsf_err_t err;
	
	VSS_CHECK_ARGC_2(1, 2);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	if (NULL == cur_context->target)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
		return VSFERR_FAIL;
	}
	
	memset(&operations_tmp, 0, sizeof(operations_tmp));
	if ((1 == argc) || (argv[1][0] == ALL_CHAR))
	{
		operations_tmp.erase_operations = 0xFFFFFFFF;
	}
	else
	{
		index = target_area_idx_by_name((char *)argv[1]);
		if (index < 0)
		{
			LOG_ERROR(ERRMSG_INVALID_TARGET, "target area");
			return VSFERR_FAIL;
		}
		operations_tmp.erase_operations = target_area_name[index].mask;
	}
	
	operations_orig = cur_context->op;
	cur_context->op = &operations_tmp;
	err = target_program(cur_context);
	cur_context->op = operations_orig;
	if (err)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATE_DEVICE, cur_context->target->name);
	}
	return err;
}

VSS_HANDLER(target_write)
{
	struct operation_t operations_tmp, *operations_orig;
	vsf_err_t err;
	int8_t index;
	
	VSS_CHECK_ARGC_2(1, 2);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	if (NULL == cur_context->target)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
		return VSFERR_FAIL;
	}
	
	memset(&operations_tmp, 0, sizeof(operations_tmp));
	if ((1 == argc) || (argv[1][0] == ALL_CHAR))
	{
		operations_tmp.write_operations = 0xFFFFFFFF;
	}
	else
	{
		index = target_area_idx_by_name((char *)argv[1]);
		if (index < 0)
		{
			LOG_ERROR(ERRMSG_INVALID_TARGET, "target area");
			return VSFERR_FAIL;
		}
		operations_tmp.write_operations = target_area_name[index].mask;
	}
	
	operations_orig = cur_context->op;
	cur_context->op = &operations_tmp;
	err = target_program(cur_context);
	cur_context->op = operations_orig;
	if (err)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATE_DEVICE, cur_context->target->name);
	}
	return err;
}

VSS_HANDLER(target_verify)
{
	struct operation_t operations_tmp, *operations_orig;
	vsf_err_t err;
	int8_t index;
	
	VSS_CHECK_ARGC_2(1, 2);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	if (NULL == cur_context->target)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
		return VSFERR_FAIL;
	}
	
	memset(&operations_tmp, 0, sizeof(operations_tmp));
	if ((1 == argc) || (argv[1][0] == ALL_CHAR))
	{
		operations_tmp.verify_operations = 0xFFFFFFFF;
	}
	else
	{
		index = target_area_idx_by_name((char *)argv[1]);
		if (index < 0)
		{
			LOG_ERROR(ERRMSG_INVALID_TARGET, "target area");
			return VSFERR_FAIL;
		}
		operations_tmp.verify_operations = target_area_name[index].mask;
	}
	
	operations_orig = cur_context->op;
	cur_context->op = &operations_tmp;
	err = target_program(cur_context);
	cur_context->op = operations_orig;
	if (err)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATE_DEVICE, cur_context->target->name);
	}
	return err;
}

VSS_HANDLER(target_read)
{
	struct operation_t operations_tmp, *operations_orig;
	struct chip_area_info_t *area_info = NULL;
	struct program_area_t *prog_area = NULL;
	struct memlist *ml_tmp = NULL, *pml_save = NULL;
	vsf_err_t err;
	uint32_t byteaddr, bytesize;
	int8_t index;
	
	VSS_CHECK_ARGC(4);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	if (NULL == cur_context->target)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
		return VSFERR_FAIL;
	}
	
	index = target_area_idx_by_name((char *)argv[1]);
	if (index < 0)
	{
		LOG_ERROR(ERRMSG_INVALID_TARGET, "target area");
		return VSFERR_FAIL;
	}
	area_info = target_get_chip_area(cur_context->param, (uint32_t)index);
	prog_area = target_get_program_area(cur_context->pi, (uint32_t)index);
	if ((NULL == area_info) || (NULL == prog_area) || (NULL == prog_area->buff))
	{
		LOG_ERROR(ERRMSG_INVALID_TARGET, "target area");
		return VSFERR_FAIL;
	}
	
	memset(&operations_tmp, 0, sizeof(operations_tmp));
	operations_tmp.read_operations = target_area_name[index].mask;
	
	byteaddr = strtoul(argv[2], NULL, 0);
	bytesize = strtoul(argv[3], NULL, 0);
	
	if ((byteaddr < area_info->addr) ||
		((byteaddr + bytesize) >= (area_info->addr + area_info->size)))
	{
		LOG_ERROR(ERRMSG_INVALID_TARGET, "target addr/size");
		return VSFERR_FAIL;
	}
	
	if (MEMLIST_Add(&ml_tmp, byteaddr, bytesize, area_info->page_size,
					&prog_area->buff[byteaddr - area_info->addr]))
	{
		return VSFERR_FAIL;
	}
	pml_save = prog_area->memlist;
	prog_area->memlist = ml_tmp;
	
	operations_orig = cur_context->op;
	cur_context->op = &operations_tmp;
	err = target_program(cur_context);
	cur_context->op = operations_orig;
	
	prog_area->memlist = pml_save;
	MEMLIST_Free(&ml_tmp);
	if (err)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATE_DEVICE, cur_context->target->name);
	}
	
	return err;
}

VSS_HANDLER(target_operate)
{
	struct operation_t *op;
	
	VSS_CHECK_ARGC(1);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	if (NULL == cur_context->target)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
		return VSFERR_FAIL;
	}
	op = cur_context->op;
	
	// in system programmer
	if ((!(op->checksum_operations || op->erase_operations
			|| op->read_operations || op->verify_operations
			|| op->write_operations))
		&& (NULL == strchr(cur_context->target->feature, NO_TARGET[0])))
	{
		// no operation defined
		// and not no_target operation
		return VSFERR_NONE;
	}
	
	if (target_program(cur_context))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATE_DEVICE, cur_context->target->name);
		return VSFERR_FAIL;
	}
	
	return target_data_save(cur_context);
}

VSS_HANDLER(target_interface_indexes)
{
	struct program_info_t *pi;
	
	VSS_CHECK_ARGC_2(1, 2);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	pi = cur_context->pi;
	
	if (pi->ifs_indexes != NULL)
	{
		free(pi->ifs_indexes);
		pi->ifs_indexes = NULL;
	}
	
	if (2 == argc)
	{
		pi->ifs_indexes = strdup(argv[1]);
		if (NULL == pi->ifs_indexes)
		{
			LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
			return VSFERR_NOT_ENOUGH_RESOURCES;
		}
	}
	return VSFERR_NONE;
}

VSS_HANDLER(target_set_specified_param)
{
	struct program_info_t *pi;
	
	VSS_CHECK_ARGC_2(1, 2);
	
	if (NULL == cur_context)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "programming slot");
		return VSFERR_FAIL;
	}
	pi = cur_context->pi;
	
	if (pi->param != NULL)
	{
		free(pi->param);
		pi->param = NULL;
	}
	
	if (2 == argc)
	{
		pi->param = strdup(argv[1]);
		if (NULL == pi->param)
		{
			LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
			return VSFERR_NOT_ENOUGH_RESOURCES;
		}
	}
	return VSFERR_NONE;
}

