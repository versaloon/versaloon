/***************************************************************************
 *   Copyright (C) 2009 - 2010 by Simon Qian <SimonQian@SimonQian.com>     *
 *                                                                         *
 *   This ifsram is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This ifsram is distributed in the hope that it will be useful,        *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this ifsram; if not, write to the                          *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <ctype.h>

#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_log.h"
#include "app_err.h"

#include "interfaces.h"
#include "interfaces_script.h"
#include "scripts.h"

#if POWER_OUT_EN
VSS_HANDLER(interface_get_target_voltage);
VSS_HANDLER(interface_set_target_voltage);

static const struct vss_cmd_t tvcc_cmd[] =
{
	VSS_CMD(	"get",
				"get target voltage, format: tvcc.get",
				interface_get_target_voltage,
				NULL),
	VSS_CMD(	"set",
				"output power to target, format: tvcc.set VOLTAGE_IN_MV",
				interface_set_target_voltage,
				NULL),
	VSS_CMD_END
};
#endif

#if INTERFACE_CLKO_EN
VSS_HANDLER(interface_clko_init);
VSS_HANDLER(interface_clko_fini);
VSS_HANDLER(interface_clko_config);

static const struct vss_cmd_t clko_cmd[] =
{
	VSS_CMD(	"init",
				"initialize clko, format: clko.init",
				interface_clko_init,
				NULL),
	VSS_CMD(	"fini",
				"finalize clko, format: clko.fini",
				interface_clko_fini,
				NULL),
	VSS_CMD(	"config",
				"configure clko, format: clko.config FREQUENCY_KHZ",
				interface_clko_config,
				NULL),
	VSS_CMD_END
};
#endif

#if INTERFACE_ADC_EN
VSS_HANDLER(interface_adc_init);
VSS_HANDLER(interface_adc_fini);
VSS_HANDLER(interface_adc_config);
VSS_HANDLER(interface_adc_get);

static const struct vss_cmd_t adc_cmd[] =
{
	VSS_CMD(	"init",
				"initialize adc, format: adc.init",
				interface_adc_init,
				NULL),
	VSS_CMD(	"fini",
				"finalize adc, format: adc.fini",
				interface_adc_fini,
				NULL),
	VSS_CMD(	"config",
				"configure adc, format: adc.config CHANNEL RATE[0x00 .. 0xFF]",
				interface_adc_config,
				NULL),
	VSS_CMD(	"sample",
				"sample adc result, format: adc.sample CHANNEL",
				interface_adc_get,
				NULL),
	VSS_CMD_END
};
#endif

#if INTERFACE_JTAG_EN
VSS_HANDLER(interface_jtag_init);
VSS_HANDLER(interface_jtag_fini);
VSS_HANDLER(interface_jtag_config);
VSS_HANDLER(interface_jtag_reset);
VSS_HANDLER(interface_jtag_runtest);
VSS_HANDLER(interface_jtag_ir);
VSS_HANDLER(interface_jtag_dr);

static const struct vss_cmd_t jtag_cmd[] =
{
	VSS_CMD(	"init",
				"initialize jtag, format: jtag.init [KHZ UB UA BB BA]",
				interface_jtag_init,
				NULL),
	VSS_CMD(	"fini",
				"finalize jtag, format: jtag.fini",
				interface_jtag_fini,
				NULL),
	VSS_CMD(	"config",
				"configure jtag, format: jtag.config KHZ [UB UA BB BA]",
				interface_jtag_config,
				NULL),
	VSS_CMD(	"reset",
				"reset jtag, format: jtag.reset",
				interface_jtag_reset,
				NULL),
	VSS_CMD(	"runtest",
				"jtag runtest, format: jtag.runtest CYCLES",
				interface_jtag_runtest,
				NULL),
	VSS_CMD(	"ir",
				"jtag ir, format: jtag.ir BITLEN IR",
				interface_jtag_ir,
				NULL),
	VSS_CMD(	"dr",
				"jtag dr, format: jtag.dr BITLEN DATASIZE DR_DATA...",
				interface_jtag_dr,
				NULL),
	VSS_CMD_END
};
#endif

#if INTERFACE_IIC_EN
VSS_HANDLER(interface_iic_init);
VSS_HANDLER(interface_iic_fini);
VSS_HANDLER(interface_iic_config);
VSS_HANDLER(interface_iic_read);
VSS_HANDLER(interface_iic_write);
VSS_HANDLER(interface_iic_read_buff8);
VSS_HANDLER(interface_iic_write_buff8);

static const struct vss_cmd_t iic_cmd[] =
{
	VSS_CMD(	"init",
				"initialize iic, format: iic.init [KHZ MAX_DLY_US]",
				interface_iic_init,
				NULL),
	VSS_CMD(	"fini",
				"finalize iic, format: iic.fini",
				interface_iic_fini,
				NULL),
	VSS_CMD(	"config",
				"config iic, format: iic.config KHZ MAX_DLY_US",
				interface_iic_config,
				NULL),
	VSS_CMD(	"read",
				"read data from iic, "
				"format: iic.read SLAVE_ADDR STOP NACKLAST DATA_SIZE",
				interface_iic_read,
				NULL),
	VSS_CMD(	"write",
				"write data to iic, format: "
				"iic.write SLAVE_ADDR STOP DATA_SIZE DATA0...",
				interface_iic_write,
				NULL),
	VSS_CMD(	"read_buff8",
				"read data from iic, "
				"format: iic.read_buff8 SLAVE_ADDR NACKLAST DATA_SIZE ADDR",
				interface_iic_read_buff8,
				NULL),
	VSS_CMD(	"write_buff8",
				"write data to iic, format: "
				"iic.write_buff8 SLAVE_ADDR DATA_SIZE ADDR DATA0...",
				interface_iic_write_buff8,
				NULL),
	VSS_CMD_END
};
#endif

#if INTERFACE_GPIO_EN
VSS_HANDLER(interface_gpio_init);
VSS_HANDLER(interface_gpio_fini);
VSS_HANDLER(interface_gpio_config);
VSS_HANDLER(interface_gpio_out);
VSS_HANDLER(interface_gpio_in);

static const struct vss_cmd_t gpio_cmd[] =
{
	VSS_CMD(	"init",
				"initialize gpio, format: gpio.init [MASK IO PULL_EN PULL]",
				interface_gpio_init,
				NULL),
	VSS_CMD(	"fini",
				"finalize gpio, format: gpio.fini",
				interface_gpio_fini,
				NULL),
	VSS_CMD(	"config",
				"config gpio, format: gpio.config MASK IO PULL_EN PULL",
				interface_gpio_config,
				NULL),
	VSS_CMD(	"out",
				"gpio output, format: gpio.out MASK VALUE",
				interface_gpio_out,
				NULL),
	VSS_CMD(	"in",
				"gpio input, format: gpio.in MASK",
				interface_gpio_in,
				NULL),
	VSS_CMD_END
};
#endif

#if INTERFACE_SPI_EN
VSS_HANDLER(interface_spi_init);
VSS_HANDLER(interface_spi_fini);
VSS_HANDLER(interface_spi_config);
VSS_HANDLER(interface_spi_io);

static const struct vss_cmd_t spi_cmd[] =
{
	VSS_CMD(	"init",
				"initialize spi, format: spi.init [KHZ MODE FIRSTBIT]",
				interface_spi_init,
				NULL),
	VSS_CMD(	"fini",
				"finalize spi, format: spi.fini",
				interface_spi_fini,
				NULL),
	VSS_CMD(	"config",
				"config spi, format: spi.config KHZ MODE FIRSTBIT",
				interface_spi_config,
				NULL),
	VSS_CMD(	"io",
				"spi input and output, format: spi.io DATASIZE DATA...",
				interface_spi_io,
				NULL),
	VSS_CMD_END
};
#endif

#if INTERFACE_PWM_EN
VSS_HANDLER(interface_pwm_init);
VSS_HANDLER(interface_pwm_fini);
VSS_HANDLER(interface_pwm_config_mode);
VSS_HANDLER(interface_pwm_config_freq);
VSS_HANDLER(interface_pwm_out);

static const struct vss_cmd_t pwm_cmd[] =
{
	VSS_CMD(	"init",
				"initialize pwm module, format: pwm.init [PUSHPULL POLARITY]",
				interface_pwm_init,
				NULL),
	VSS_CMD(	"fini",
				"finialize pwm module, format: pwm.fini",
				interface_pwm_fini,
				NULL),
	VSS_CMD(	"config_mode",
				"config pwm module, format: pwm.config_mode PUSHPULL POLARITY",
				interface_pwm_config_mode,
				NULL),
	VSS_CMD(	"config_freq",
				"config pwm module, format: pwm.config_freq KHZ",
				interface_pwm_config_freq,
				NULL),
	VSS_CMD(	"out",
				"output pwm sequence, format: pwm.out CNT RATE0 RATE1 ...",
				interface_pwm_out,
				NULL),
	VSS_CMD_END
};
#endif

#if INTERFACE_EBI_EN
VSS_HANDLER(interface_ebi_init);
VSS_HANDLER(interface_ebi_fini);
VSS_HANDLER(interface_ebi_config);
VSS_HANDLER(interface_ebi_read);
VSS_HANDLER(interface_ebi_write);
VSS_HANDLER(interface_ebi_read8);
VSS_HANDLER(interface_ebi_write8);
VSS_HANDLER(interface_ebi_read16);
VSS_HANDLER(interface_ebi_write16);
VSS_HANDLER(interface_ebi_read32);
VSS_HANDLER(interface_ebi_write32);

static const struct vss_cmd_t ebi_cmd[] =
{
	VSS_CMD(	"init",
				"initialize ebi module, format: ebi.init",
				interface_ebi_init,
				NULL),
	VSS_CMD(	"fini",
				"finialize ebi module, format: ebi.fini",
				interface_ebi_fini,
				NULL),
	VSS_CMD(	"config",
				"config ebi port, format: ebi.config PORT WIDTH [ADD_SET DATA_SET]",
				interface_ebi_config,
				NULL),
	VSS_CMD(	"read",
				"read data from ebi port, format: ebi.read PORT ADDR SIZE COUNT",
				interface_ebi_read,
				NULL),
	VSS_CMD(	"write",
				"write data to ebi port, format: ebi.write PORT ADDR SIZE COUNT [DATA...]",
				interface_ebi_write,
				NULL),
	VSS_CMD(	"read8",
				"read data8 from ebi port, format: ebi.read PORT ADDR8 COUNT",
				interface_ebi_read8,
				NULL),
	VSS_CMD(	"write8",
				"write data8 to ebi port, format: ebi.write PORT ADDR8 COUNT [DATA8...]",
				interface_ebi_write8,
				NULL),
	VSS_CMD(	"read16",
				"read data16 from ebi port, format: ebi.read PORT ADDR16 COUNT",
				interface_ebi_read16,
				NULL),
	VSS_CMD(	"write16",
				"write data16 to ebi port, format: ebi.write PORT ADDR16 COUNT [DATA16...]",
				interface_ebi_write16,
				NULL),
	VSS_CMD(	"read32",
				"read data32 from ebi port, format: ebi.read PORT ADDR32 COUNT",
				interface_ebi_read32,
				NULL),
	VSS_CMD(	"write32",
				"write data32 to ebi port, format: ebi.write PORT ADDR32 SIZE COUNT [DATA32...]",
				interface_ebi_write32,
				NULL),
	VSS_CMD_END
};
#endif

VSS_HANDLER(interface_delay_us);
VSS_HANDLER(interface_delay_ms);
VSS_HANDLER(interface_commit);

static const struct vss_cmd_t delay_cmd[] =
{
	VSS_CMD(	"delayus",
				"delay us, format: delay.delayus US",
				interface_delay_us,
				NULL),
	VSS_CMD(	"delayms",
				"delay ms, format: delay.delayus MS",
				interface_delay_ms,
				NULL),
	VSS_CMD_END
};

static const struct vss_cmd_t interface_cmd[] =
{
#if POWER_OUT_EN
	VSS_CMD(	"tvcc",
				"adjust target voltge",
				NULL,
				tvcc_cmd),
#endif
#if INTERFACE_ADC_EN
	VSS_CMD(	"adc",
				"analog to digital",
				interface_adc_init,
				adc_cmd),
#endif
#if INTERFACE_JTAG_EN
	VSS_CMD(	"jtag",
				"jtag interface handler",
				interface_jtag_init,
				jtag_cmd),
#endif
#if INTERFACE_GPIO_EN
	VSS_CMD(	"gpio",
				"gpio interface handler",
				interface_gpio_init,
				gpio_cmd),
#endif
#if INTERFACE_SPI_EN
	VSS_CMD(	"spi",
				"spi interface handler",
				interface_spi_init,
				spi_cmd),
#endif
#if INTERFACE_IIC_EN
	VSS_CMD(	"iic",
				"iic interface handler",
				interface_iic_init,
				iic_cmd),
#endif
#if INTERFACE_PWM_EN
	VSS_CMD(	"pwm",
				"pwm interface handler",
				interface_pwm_init,
				pwm_cmd),
#endif
#if INTERFACE_EBI_EN
	VSS_CMD(	"ebi",
				"ebi interface handler",
				interface_ebi_init,
				ebi_cmd),
#endif
#if INTERFACE_CLKO_EN
	VSS_CMD(	"clko",
				"clko interface handler",
				interface_clko_init,
				clko_cmd),
#endif
	VSS_CMD(	"delay",
				"delay interface handler",
				NULL,
				delay_cmd),
	VSS_CMD(	"commit",
				"commit all commands",
				interface_commit,
				NULL),
	VSS_CMD_END
};
struct vss_cmd_list_t interface_cmd_list =
							VSS_CMD_LIST("interface", interface_cmd);

// interfaces scripts
#define INTERFACE_ASSERT(ifs_mask, ifs_name)							\
	do{\
		if (interface_assert(&ifs) || (NULL == ifs))\
		{\
			LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");\
			return VSFERR_FAIL;\
		}\
		if (!(ifs->support_mask & ifs_mask))\
		{\
			LOG_ERROR(ERRMSG_NOT_SUPPORT, ifs_name " interface");\
			return VSFERR_FAIL;\
		}\
	} while (0)

#if POWER_OUT_EN
VSS_HANDLER(interface_get_target_voltage)
{
	uint16_t voltage = 0;
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	INTERFACE_ASSERT(IFS_POWER, "power");
	
	if (ifs->target_voltage.get(0, &voltage))
	{
		return VSFERR_FAIL;
	}
	else
	{
		LOG_INFO(INFOMSG_TARGET_VOLTAGE, voltage / 1000.0);
		return VSFERR_NONE;
	}
}

VSS_HANDLER(interface_set_target_voltage)
{
	uint16_t voltage = 0;
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(2);
	INTERFACE_ASSERT(IFS_POWER, "power");
	
	voltage = (uint16_t)strtoul(argv[1], NULL, 0);
	if (ifs->target_voltage.set(0, voltage) || ifs->peripheral_commit())
	{
		return VSFERR_FAIL;
	}

	vss_run_script("tvcc.get");
	return VSFERR_NONE;
}
#endif

#if INTERFACE_CLKO_EN
VSS_HANDLER(interface_clko_init)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC_2(1, 2);
	INTERFACE_ASSERT(IFS_CLOCK, "clko");
	
	if (ifs->clko.init(0))
	{
		return VSFERR_FAIL;
	}
	
	if (2 == argc)
	{
		return interface_clko_config(argc, argv);
	}
	
	return VSFERR_NONE;
}

VSS_HANDLER(interface_clko_fini)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	INTERFACE_ASSERT(IFS_CLOCK, "clko");
	
	if (ifs->clko.fini(0) || ifs->peripheral_commit())
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

VSS_HANDLER(interface_clko_config)
{
	uint32_t kHz;
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(2);
	INTERFACE_ASSERT(IFS_CLOCK, "clko");
	
	kHz = (uint32_t)strtoul(argv[1], NULL, 0);
	
	if (ifs->clko.config(0, kHz) ||
		ifs->clko.enable(0) ||
		ifs->peripheral_commit())
	{
		return VSFERR_FAIL;
	}
	
	return VSFERR_NONE;
}
#endif

#if INTERFACE_ADC_EN
VSS_HANDLER(interface_adc_init)
{
	uint32_t clock_hz;
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(2);
	INTERFACE_ASSERT(IFS_ADC, "adc");
	
	clock_hz = (uint32_t)strtoul(argv[1], NULL, 0);
	if (ifs->adc.init(0) || 
		ifs->adc.config(0, clock_hz, ADC_ALIGNRIGHT) || 
		ifs->peripheral_commit())
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

VSS_HANDLER(interface_adc_fini)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	INTERFACE_ASSERT(IFS_ADC, "adc");
	
	if (ifs->adc.fini(0) || ifs->peripheral_commit())
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

VSS_HANDLER(interface_adc_config)
{
	uint8_t channel;
	uint8_t rate;
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(3);
	INTERFACE_ASSERT(IFS_ADC, "adc");
	
	channel = (uint8_t)strtoul(argv[1], NULL, 0);
	rate = (uint8_t)strtoul(argv[2], NULL, 0);
	if (ifs->adc.config_channel(0, channel, rate) || 
		ifs->adc.calibrate(0, channel) || 
		ifs->peripheral_commit())
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

VSS_HANDLER(interface_adc_get)
{
	uint8_t channel;
	uint32_t voltage, max;
	struct INTERFACES_INFO_T *ifs = NULL;
	vsf_err_t err;
	
	VSS_CHECK_ARGC(2);
	INTERFACE_ASSERT(IFS_ADC, "adc");
	
	max = ifs->adc.get_max_value(0);
	if (!max)
	{
		return VSFERR_NONE;
	}
	channel = (uint8_t)strtoul(argv[1], NULL, 0);
	if (ifs->adc.start(0, channel))
	{
		return VSFERR_FAIL;
	}
	do {
		err = ifs->adc.isready(0, channel);
	} while (err > 0);
	if (err < 0)
	{
		return VSFERR_FAIL;
	}
	
	LOG_INFO(INFOMSG_VOLTAGE, "ADC result", voltage * 3.3 / max);
	return VSFERR_NONE;
}
#endif

#if INTERFACE_GPIO_EN
VSS_HANDLER(interface_gpio_init)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC_2(1, 5);
	INTERFACE_ASSERT(IFS_GPIO, "gpio");
	
	if (ifs->gpio.init(0))
	{
		return VSFERR_FAIL;
	}
	
	if (5 == argc)
	{
		return interface_gpio_config(argc, argv);
	}
	return VSFERR_NONE;
}

VSS_HANDLER(interface_gpio_fini)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	INTERFACE_ASSERT(IFS_GPIO, "gpio");
	
	return ifs->gpio.fini(0);
}

VSS_HANDLER(interface_gpio_config)
{
	uint32_t mask, io, pull, pull_en;
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(5);
	INTERFACE_ASSERT(IFS_GPIO, "gpio");
	
	mask = (uint32_t)strtoul(argv[1], NULL, 0);
	io = (uint32_t)strtoul(argv[2], NULL, 0);
	pull_en = (uint32_t)strtoul(argv[3], NULL, 0);
	pull = (uint32_t)strtoul(argv[4], NULL, 0);
	
	return ifs->gpio.config(0, mask, io, pull_en, pull);
}

VSS_HANDLER(interface_gpio_out)
{
	uint32_t mask, value;
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(3);
	INTERFACE_ASSERT(IFS_GPIO, "gpio");
	
	mask = (uint32_t)strtoul(argv[1], NULL, 0);
	value = (uint32_t)strtoul(argv[2], NULL, 0);
	
	return ifs->gpio.out(0, mask, value);
}

VSS_HANDLER(interface_gpio_in)
{
	uint32_t mask, value;
	vsf_err_t err = VSFERR_NONE;
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(2);
	INTERFACE_ASSERT(IFS_GPIO, "gpio");
	
	mask = (uint32_t)strtoul(argv[1], NULL, 0);
	
	err = ifs->gpio.in(0, mask, &value);
	if (!err)
	{
		err = ifs->peripheral_commit();
		if (!err)
		{
			LOG_INFO(INFOMSG_REG_08X, "GPIO", value);
		}
	}
	return err;
}
#endif

#if INTERFACE_JTAG_EN
VSS_HANDLER(interface_jtag_init)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC_3(1, 2, 6);
	INTERFACE_ASSERT(IFS_JTAG_HL, "jtag");
	
	if (ifs->jtag_hl.init(0))
	{
		return VSFERR_FAIL;
	}
	
	if (argc > 1)
	{
		return interface_jtag_config(argc, argv);
	}
	return VSFERR_NONE;
}

VSS_HANDLER(interface_jtag_fini)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	INTERFACE_ASSERT(IFS_JTAG_HL, "jtag");
	
	return ifs->jtag_hl.fini(0);
}

VSS_HANDLER(interface_jtag_config)
{
	uint32_t khz = 0;
	struct jtag_pos_t pos;
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC_2(2, 6);
	INTERFACE_ASSERT(IFS_JTAG_HL, "jtag");
	
	khz = (uint32_t)strtoul(argv[1], NULL, 0);
	pos.ub = pos.ua = 0;
	pos.bb = pos.ba = 0;
	if (argc > 2)
	{
		pos.ub = (uint8_t)strtoul(argv[2], NULL, 0);
		pos.ua = (uint8_t)strtoul(argv[3], NULL, 0);
		pos.bb = (uint16_t)strtoul(argv[4], NULL, 0);
		pos.ba = (uint16_t)strtoul(argv[5], NULL, 0);
	}
	
	return ifs->jtag_hl.config(0, khz, &pos);
}

VSS_HANDLER(interface_jtag_reset)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	uint8_t tms = 0x7F;
	
	VSS_CHECK_ARGC(1);
	INTERFACE_ASSERT(IFS_JTAG_HL, "jtag");
	
	return ifs->jtag_hl.tms(0, &tms, 8);
}

VSS_HANDLER(interface_jtag_runtest)
{
	uint32_t cycles = 0;
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(2);
	INTERFACE_ASSERT(IFS_JTAG_HL, "jtag");
	
	cycles = (uint32_t)strtoul(argv[1], NULL, 0);
	
	return ifs->jtag_hl.runtest(0, cycles);
}

VSS_HANDLER(interface_jtag_ir)
{
	uint32_t ir;
	uint16_t bitlen;
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(3);
	INTERFACE_ASSERT(IFS_JTAG_HL, "jtag");
	
	bitlen = (uint16_t)strtoul(argv[1], NULL, 0);
	ir = (uint32_t)strtoul(argv[2], NULL, 0);
	ir = SYS_TO_LE_U32(ir);
	
	LOG_INFO(INFOMSG_REG_08X, "IR_out", ir);
	if (ifs->jtag_hl.ir(0, (uint8_t *)&ir, bitlen, 1, 1) ||
		ifs->peripheral_commit())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "jtag ir");
		return VSFERR_FAIL;
	}
	LOG_INFO(INFOMSG_REG_08X, "IR_in", ir);
	
	return VSFERR_NONE;
}

VSS_HANDLER(interface_jtag_dr)
{
	uint16_t bitlen, bytelen;
	uint8_t *dr = NULL, data_size;
	uint16_t data_num;
	struct INTERFACES_INFO_T *ifs = NULL;
	vsf_err_t err;
	
	VSS_CHECK_ARGC_MIN(4);
	INTERFACE_ASSERT(IFS_JTAG_HL, "jtag");
	
	bitlen = (uint16_t)strtoul(argv[1], NULL, 0);
	bytelen = (bitlen + 7) >> 3;
	data_size = (uint8_t)strtoul(argv[2], NULL, 0);
	data_num = (bytelen + data_size - 1) / data_size;
	
	VSS_CHECK_ARGC(3 + data_num);
	if ((data_size != 1) && (data_size != 2) && (data_size != 4))
	{
		LOG_ERROR(ERRMSG_INVALID_TARGET, "data_size");
		vss_print_help(argv[0]);
		return VSFERR_FAIL;
	}
	
	err = vss_get_binary_buffer(argc - 3, &argv[3], data_size, data_num,
								(void**)&dr, NULL);
	if (err)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse data");
		vss_print_help(argv[0]);
	}
	else
	{
		err = ifs->jtag_hl.dr(0, dr, bitlen, 1, 1);
		if (!err)
		{
			err = ifs->peripheral_commit();
			if (!err)
			{
				LOG_BUF_STD(data_size, dr, data_num, LOG_INFO);
			}
		}
	}
	
	if (dr != NULL)
	{
		free(dr);
		dr = NULL;
	}
	return err;
}
#endif

#if INTERFACE_SPI_EN
VSS_HANDLER(interface_spi_init)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC_2(1, 4);
	INTERFACE_ASSERT(IFS_SPI, "spi");
	
	if (ifs->spi.init(0))
	{
		return VSFERR_FAIL;
	}
	
	if (4 == argc)
	{
		return interface_spi_config(argc, argv);
	}
	return VSFERR_NONE;
}

VSS_HANDLER(interface_spi_fini)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	INTERFACE_ASSERT(IFS_SPI, "spi");
	
	return ifs->spi.fini(0);
}

VSS_HANDLER(interface_spi_config)
{
	uint32_t khz = 0;
	uint8_t mode, firstbit;
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(4);
	INTERFACE_ASSERT(IFS_SPI, "spi");
	
	khz = (uint32_t)strtoul(argv[1], NULL, 0);
	mode = (uint8_t)strtoul(argv[2], NULL, 0);
	if (mode > SPI_MODE3)
	{
		LOG_ERROR(ERRMSG_INVALID_TARGET, "spi mode");
		return VSFERR_FAIL;
	}
	firstbit = (uint8_t)strtoul(argv[3], NULL, 0);
	firstbit = firstbit ? SPI_MSB_FIRST : SPI_LSB_FIRST;
	
	return ifs->spi.config(0, khz, mode | firstbit);
}

VSS_HANDLER(interface_spi_io)
{
	uint16_t data_num = 0;
	uint16_t parsed_num;
	uint8_t *buff = NULL;
	vsf_err_t err = VSFERR_NONE;
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC_MIN(2);
	INTERFACE_ASSERT(IFS_SPI, "spi");
	
	data_num = (uint16_t)strtoul(argv[1], NULL, 0);
	if (0 == data_num)
	{
		LOG_ERROR(ERRMSG_INVALID_TARGET, "data_size");
		vss_print_help(argv[0]);
		return VSFERR_FAIL;
	}

	VSS_CHECK_ARGC_MAX(2 + data_num);
	err = vss_get_binary_buffer(argc - 2, &argv[2], 1, data_num, (void**)&buff,
								&parsed_num);
	if (err)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse data");
		vss_print_help(argv[0]);
	}
	else
	{
		if (parsed_num && (parsed_num < data_num))
		{
			memset(&buff[parsed_num], buff[parsed_num - 1], data_num - parsed_num);
		}
		err = ifs->spi.io(0, buff, buff, data_num);
		if (!err)
		{
			err = ifs->peripheral_commit();
			if (!err)
			{
				LOG_BUF_STD(1, buff, data_num, LOG_INFO);
			}
		}
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return err;
}
#endif

#if INTERFACE_IIC_EN
VSS_HANDLER(interface_iic_init)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC_2(1, 3);
	INTERFACE_ASSERT(IFS_I2C, "iic");
	
	if (ifs->i2c.init(0))
	{
		return VSFERR_FAIL;
	}
	
	if (3 == argc)
	{
		return interface_iic_config(argc, argv);
	}
	return VSFERR_NONE;
}

VSS_HANDLER(interface_iic_fini)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	INTERFACE_ASSERT(IFS_I2C, "iic");
	
	return ifs->i2c.fini(0);
}

VSS_HANDLER(interface_iic_config)
{
	uint16_t khz = 0;
	uint16_t max_dly = 0;
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(3);
	INTERFACE_ASSERT(IFS_I2C, "iic");
	
	khz = (uint16_t)strtoul(argv[1], NULL, 0);
	max_dly = (uint16_t)strtoul(argv[2], NULL, 0);
	
	return ifs->i2c.config(0, khz, 0, max_dly);
}

VSS_HANDLER(interface_iic_read)
{
	uint8_t data_size = 0;
	uint8_t addr = 0;
	uint8_t stop = 0;
	bool nacklast = false;
	uint8_t *buff = NULL;
	vsf_err_t err = VSFERR_NONE;
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(5);
	INTERFACE_ASSERT(IFS_I2C, "iic");
	
	addr = (uint8_t)strtoul(argv[1], NULL, 0);
	stop = (uint8_t)strtoul(argv[2], NULL, 0);
	nacklast = strtoul(argv[3], NULL, 0) > 0;
	data_size = (uint8_t)strtoul(argv[4], NULL, 0);
	buff = (uint8_t*)malloc(data_size);
	if (NULL == buff)
	{
		return VSFERR_FAIL;
	}
	
	err = ifs->i2c.read(0, addr, buff, data_size, stop, nacklast);
	if (!err)
	{
		err = ifs->peripheral_commit();
		if (!err)
		{
			LOG_BUF_STD(1, buff, data_size, LOG_INFO);
		}
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return err;
}

VSS_HANDLER(interface_iic_write)
{
	uint8_t data_size = 0;
	uint8_t addr = 0;
	uint8_t stop = 0;
	uint8_t *buff = NULL;
	vsf_err_t err = VSFERR_NONE;
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC_MIN(4);
	INTERFACE_ASSERT(IFS_I2C, "iic");
	
	addr = (uint8_t)strtoul(argv[1], NULL, 0);
	stop = (uint8_t)strtoul(argv[2], NULL, 0);
	data_size = (uint8_t)strtoul(argv[3], NULL, 0);
	
	VSS_CHECK_ARGC(4 + data_size);
	if (0 == data_size)
	{
		LOG_ERROR(ERRMSG_INVALID_TARGET, "data_size");
		vss_print_help(argv[0]);
		return VSFERR_FAIL;
	}
	
	err = vss_get_binary_buffer(argc - 4, &argv[4], 1, data_size, (void**)&buff,
								NULL);
	if (err)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse data");
		vss_print_help(argv[0]);
	}
	else
	{
		err = ifs->i2c.write(0, addr, buff, data_size, stop);
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return err;
}

VSS_HANDLER(interface_iic_read_buff8)
{
	uint8_t data_size = 0;
	uint8_t slave_addr = 0, addr;
	bool nacklast = false;
	uint8_t *buff = NULL;
	vsf_err_t err = VSFERR_NONE;
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(5);
	INTERFACE_ASSERT(IFS_I2C, "iic");
	
	slave_addr = (uint8_t)strtoul(argv[1], NULL, 0);
	nacklast = strtoul(argv[2], NULL, 0) > 0;
	data_size = (uint8_t)strtoul(argv[3], NULL, 0);
	addr = (uint8_t)strtoul(argv[4], NULL, 0);
	buff = (uint8_t*)malloc(data_size);
	if (NULL == buff)
	{
		return VSFERR_FAIL;
	}
	
	err = ifs->i2c.write(0, slave_addr, &addr, 1, 0);
	if (!err)
	{
		err = ifs->i2c.read(0, slave_addr, buff, data_size, 1,
										nacklast);
		if (!err)
		{
			err = ifs->peripheral_commit();
			if (!err)
			{
				LOG_BUF_STD(1, buff, data_size, LOG_INFO);
			}
		}
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return err;
}

VSS_HANDLER(interface_iic_write_buff8)
{
	uint8_t data_size = 0;
	uint8_t slave_addr = 0;
	uint8_t *buff = NULL;
	vsf_err_t err = VSFERR_NONE;
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC_MIN(4);
	INTERFACE_ASSERT(IFS_I2C, "iic");
	
	slave_addr = (uint8_t)strtoul(argv[1], NULL, 0);
	data_size = (uint8_t)strtoul(argv[2], NULL, 0);
	
	VSS_CHECK_ARGC(4 + data_size);
	if (0 == data_size)
	{
		LOG_ERROR(ERRMSG_INVALID_TARGET, "data_size");
		vss_print_help(argv[0]);
		return VSFERR_FAIL;
	}
	
	err = vss_get_binary_buffer(argc - 3, &argv[3], 1, data_size + 1,
								(void**)&buff, NULL);
	if (err)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse data");
		vss_print_help(argv[0]);
	}
	else
	{
		err = ifs->i2c.write(0, slave_addr, buff, data_size + 1, 1);
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return err;
}
#endif

#if INTERFACE_PWM_EN
VSS_HANDLER(interface_pwm_init)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC_2(1, 3);
	INTERFACE_ASSERT(IFS_PWM, "pwm");
	
	if (ifs->pwm.init(0))
	{
		return VSFERR_FAIL;
	}
	
	if (3 == argc)
	{
		return interface_pwm_config_mode(argc, argv);
	}
	return VSFERR_NONE;
}

VSS_HANDLER(interface_pwm_fini)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	INTERFACE_ASSERT(IFS_PWM, "pwm");
	
	return ifs->pwm.fini(0);
}

VSS_HANDLER(interface_pwm_config_mode)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	uint8_t pushpull, polarity, mode;
	
	VSS_CHECK_ARGC(3);
	INTERFACE_ASSERT(IFS_PWM, "pwm");
	
	pushpull = (uint8_t)strtoul(argv[1], NULL, 0);
	polarity = (uint8_t)strtoul(argv[2], NULL, 0);
	
	mode = 0;
	if (pushpull)
	{
		mode |= PWM_OUTPP;
	}
	if (polarity)
	{
		mode |= PWM_OUTPOLARITY;
	}
	return ifs->pwm.config_mode(0, mode);
}

VSS_HANDLER(interface_pwm_config_freq)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	uint16_t kHz;
	
	VSS_CHECK_ARGC(2);
	INTERFACE_ASSERT(IFS_PWM, "pwm");
	
	kHz = (uint16_t)strtoul(argv[1], NULL, 0);
	
	return ifs->pwm.config_freq(0, kHz);
}

VSS_HANDLER(interface_pwm_out)
{
	uint16_t count = 0;
	uint16_t *buff = NULL;
	vsf_err_t err = VSFERR_NONE;
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC_MIN(3);
	INTERFACE_ASSERT(IFS_PWM, "pwm");
	
	count = (uint16_t)strtoul(argv[1], NULL, 0);
	
	VSS_CHECK_ARGC(2 + count);
	if (0 == count)
	{
		LOG_ERROR(ERRMSG_INVALID_TARGET, "count");
		vss_print_help(argv[0]);
		return VSFERR_FAIL;
	}
	
	err = vss_get_binary_buffer(argc - 2, &argv[2], 2, count, (void**)&buff,
								NULL);
	if (err)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse data");
		vss_print_help(argv[0]);
	}
	else
	{
		err = ifs->pwm.out(0, count, buff);
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return err;
}
#endif

#if INTERFACE_EBI_EN
VSS_HANDLER(interface_ebi_init)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	INTERFACE_ASSERT(IFS_EBI, "ebi");
	
	return  ifs->ebi.init(0);
}

VSS_HANDLER(interface_ebi_fini)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	INTERFACE_ASSERT(IFS_EBI, "ebi");
	
	return ifs->ebi.fini(0);
}

VSS_HANDLER(interface_ebi_config)
{
	struct ebi_sram_psram_nor_info_t nor_info;
	struct INTERFACES_INFO_T *ifs = NULL;
	uint8_t index;
	uint8_t width;
	uint8_t add_set = 15, data_set  = 255;
	
	VSS_CHECK_ARGC_2(3, 5);
	INTERFACE_ASSERT(IFS_EBI, "ebi");
	
	index = (uint8_t)strtoul(argv[1], NULL, 0);
	width = (uint8_t)strtoul(argv[2], NULL, 0);
	if ((width != 8) && (width != 16) && (width != 32))
	{
		LOG_ERROR(ERRMSG_INVALID_PARAMETER, "width");
		return VSFERR_INVALID_PARAMETER;
	}
	if (argc == 5)
	{
		add_set = (uint8_t)strtoul(argv[3], NULL, 0);
		data_set = (uint8_t)strtoul(argv[4], NULL, 0);
	}
	
	nor_info.common_info.data_width = width;
	nor_info.common_info.wait_signal = EBI_WAIT_NONE;
	nor_info.param.addr_multiplex = false;
	nor_info.param.timing.clock_hz_r = 
		nor_info.param.timing.clock_hz_w = 0;
	nor_info.param.timing.address_setup_cycle_r = 
		nor_info.param.timing.address_setup_cycle_w = add_set;
	nor_info.param.timing.address_hold_cycle_r = 
		nor_info.param.timing.address_hold_cycle_w = 0;
	nor_info.param.timing.data_setup_cycle_r = 
		nor_info.param.timing.data_setup_cycle_w = data_set;
	return ifs->ebi.config(0, index | EBI_TGTTYP_NOR, &nor_info);
}

VSS_HANDLER(interface_ebi_read)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	vsf_err_t err = VSFERR_NONE;
	uint8_t index;
	uint32_t address;
	uint8_t size;
	uint8_t *buff = NULL;
	uint32_t count;
	
	VSS_CHECK_ARGC(5);
	INTERFACE_ASSERT(IFS_EBI, "ebi");
	
	index = (uint8_t)strtoul(argv[1], NULL, 0);
	address = (uint32_t)strtoul(argv[2], NULL, 0);
	size = (uint8_t)strtoul(argv[3], NULL, 0);
	count = (uint32_t)strtoul(argv[4], NULL, 0);
	
	switch (size)
	{
	case 1:
		// 8-bit mode
		break;
	case 2:
		// 16-bit mode
		address <<= 1;
		break;
	case 4:
		// 32-bit mode
		address <<= 2;
		break;
	default:
		LOG_ERROR(ERRMSG_INVALID_PARAMETER, "size");
		return VSFERR_INVALID_PARAMETER;
	}
	
	buff = (uint8_t *)malloc(size * count);
	if (NULL == buff)
	{
		return VSFERR_FAIL;
	}
	
	if (!err)
	{
		err = ifs->ebi.read(0, index | EBI_TGTTYP_NOR, address, size, buff, count);
	}
	if (!err)
	{
		err = ifs->peripheral_commit();
	}
	if (!err)
	{
		LOG_BUF_STD(size, buff, count, LOG_INFO);
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return err;
}

VSS_HANDLER(interface_ebi_write)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	vsf_err_t err = VSFERR_NONE;
	uint8_t index;
	uint32_t address;
	uint8_t size;
	uint8_t *buff = NULL;
	uint16_t count, parsed_num;;
	
	VSS_CHECK_ARGC_MIN(5);
	INTERFACE_ASSERT(IFS_EBI, "ebi");
	
	index = (uint8_t)strtoul(argv[1], NULL, 0);
	address = (uint32_t)strtoul(argv[2], NULL, 0);
	size = (uint8_t)strtoul(argv[3], NULL, 0);
	count = (uint16_t)strtoul(argv[4], NULL, 0);
	
	switch (size)
	{
	case 1:
		// 8-bit mode
		break;
	case 2:
		// 16-bit mode
		address <<= 1;
		break;
	case 4:
		// 32-bit mode
		address <<= 2;
		break;
	default:
		LOG_ERROR(ERRMSG_INVALID_PARAMETER, "size");
		return VSFERR_INVALID_PARAMETER;
	}
	VSS_CHECK_ARGC_MAX(count + 5);
	
	err = vss_get_binary_buffer(argc - 5, &argv[5], size, count, (void**)&buff,
								&parsed_num);
	if (err)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse data");
		vss_print_help(argv[0]);
	}
	else
	{
		uint16_t i;
		
		for (i = parsed_num; i < count; i++)
		{
			switch (size)
			{
			case 1:
				((uint8_t *)buff)[i] = ((uint8_t *)buff)[parsed_num - 1];
				break;
			case 2:
				((uint16_t *)buff)[i] = ((uint16_t *)buff)[parsed_num - 1];
				break;
			case 4:
				((uint32_t *)buff)[i] = ((uint32_t *)buff)[parsed_num - 1];
				break;
			}
		}
		err = ifs->ebi.write(0, index | EBI_TGTTYP_NOR, address, size,
								(uint8_t *)buff, count);
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return err;
}

VSS_HANDLER(interface_ebi_read8)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	vsf_err_t err = VSFERR_NONE;
	uint8_t index;
	uint32_t address;
	uint8_t size = 1;
	uint8_t *buff = NULL;
	uint32_t count;
	
	VSS_CHECK_ARGC(4);
	INTERFACE_ASSERT(IFS_EBI, "ebi");
	
	index = (uint8_t)strtoul(argv[1], NULL, 0);
	address = ((uint32_t)strtoul(argv[2], NULL, 0)) << 0;
	count = (uint32_t)strtoul(argv[3], NULL, 0);
	
	buff = (uint8_t *)malloc(count * size);
	if (NULL == buff)
	{
		return VSFERR_FAIL;
	}
	
	if (!err)
	{
		err = ifs->ebi.read(0, index | EBI_TGTTYP_NOR, address, size,
							(uint8_t *)buff, count);
	}
	if (!err)
	{
		err = ifs->peripheral_commit();
	}
	if (!err)
	{
		LOG_BUF_STD(size, buff, count, LOG_INFO);
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return err;
}

VSS_HANDLER(interface_ebi_write8)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	vsf_err_t err = VSFERR_NONE;
	uint8_t index;
	uint32_t address;
	uint8_t size = 1;
	uint8_t *buff = NULL;
	uint16_t count, parsed_num;
	
	VSS_CHECK_ARGC_MIN(4);
	INTERFACE_ASSERT(IFS_EBI, "ebi");
	
	index = (uint8_t)strtoul(argv[1], NULL, 0);
	address = ((uint32_t)strtoul(argv[2], NULL, 0)) << 0;
	count = (uint16_t)strtoul(argv[3], NULL, 0);
	
	VSS_CHECK_ARGC_MAX(count + 4);
	
	err = vss_get_binary_buffer(argc - 4, &argv[4], size, count, (void**)&buff,
								&parsed_num);
	if (err)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse data");
		vss_print_help(argv[0]);
	}
	else
	{
		uint16_t i;
		
		for (i = parsed_num; i < count; i++)
		{
			switch (size)
			{
			case 1:
				((uint8_t *)buff)[i] = ((uint8_t *)buff)[parsed_num - 1];
				break;
			case 2:
				((uint16_t *)buff)[i] = ((uint16_t *)buff)[parsed_num - 1];
				break;
			case 4:
				((uint32_t *)buff)[i] = ((uint32_t *)buff)[parsed_num - 1];
				break;
			}
		}
		err = ifs->ebi.write(0, index | EBI_TGTTYP_NOR, address, size,
								(uint8_t *)buff, count);
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return err;
}

VSS_HANDLER(interface_ebi_read16)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	vsf_err_t err = VSFERR_NONE;
	uint8_t index;
	uint32_t address;
	uint8_t size = 2;
	uint16_t *buff = NULL;
	uint32_t count;
	
	VSS_CHECK_ARGC(4);
	INTERFACE_ASSERT(IFS_EBI, "ebi");
	
	index = (uint8_t)strtoul(argv[1], NULL, 0);
	address = ((uint32_t)strtoul(argv[2], NULL, 0)) << 1;
	count = (uint32_t)strtoul(argv[3], NULL, 0);
	
	buff = (uint16_t *)malloc(count * size);
	if (NULL == buff)
	{
		return VSFERR_FAIL;
	}
	
	if (!err)
	{
		err = ifs->ebi.read(0, index | EBI_TGTTYP_NOR, address, size,
							(uint8_t *)buff, count);
	}
	if (!err)
	{
		err = ifs->peripheral_commit();
	}
	if (!err)
	{
		LOG_BUF_STD(size, buff, count, LOG_INFO);
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return err;
}

VSS_HANDLER(interface_ebi_write16)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	vsf_err_t err = VSFERR_NONE;
	uint8_t index;
	uint32_t address;
	uint8_t size = 2;
	uint16_t *buff = NULL;
	uint16_t count, parsed_num;
	
	VSS_CHECK_ARGC_MIN(4);
	INTERFACE_ASSERT(IFS_EBI, "ebi");
	
	index = (uint8_t)strtoul(argv[1], NULL, 0);
	address = ((uint32_t)strtoul(argv[2], NULL, 0)) << 1;
	count = (uint16_t)strtoul(argv[3], NULL, 0);
	
	VSS_CHECK_ARGC(count + 4);
	
	err = vss_get_binary_buffer(argc - 4, &argv[4], size, count, (void**)&buff,
								&parsed_num);
	if (err)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse data");
		vss_print_help(argv[0]);
	}
	else
	{
		uint16_t i;
		
		for (i = parsed_num; i < count; i++)
		{
			switch (size)
			{
			case 1:
				((uint8_t *)buff)[i] = ((uint8_t *)buff)[parsed_num - 1];
				break;
			case 2:
				((uint16_t *)buff)[i] = ((uint16_t *)buff)[parsed_num - 1];
				break;
			case 4:
				((uint32_t *)buff)[i] = ((uint32_t *)buff)[parsed_num - 1];
				break;
			}
		}
		err = ifs->ebi.write(0, index | EBI_TGTTYP_NOR, address, size,
								(uint8_t *)buff, count);
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return err;
}

VSS_HANDLER(interface_ebi_read32)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	vsf_err_t err = VSFERR_NONE;
	uint8_t index;
	uint32_t address;
	uint8_t size = 4;
	uint32_t *buff = NULL;
	uint32_t count;
	
	VSS_CHECK_ARGC(4);
	INTERFACE_ASSERT(IFS_EBI, "ebi");
	
	index = (uint8_t)strtoul(argv[1], NULL, 0);
	address = ((uint32_t)strtoul(argv[2], NULL, 0)) << 2;
	count = (uint32_t)strtoul(argv[3], NULL, 0);
	
	buff = (uint32_t *)malloc(count * size);
	if (NULL == buff)
	{
		return VSFERR_FAIL;
	}
	
	if (!err)
	{
		err = ifs->ebi.read(0, index | EBI_TGTTYP_NOR, address, size,
							(uint8_t *)buff, count);
	}
	if (!err)
	{
		err = ifs->peripheral_commit();
	}
	if (!err)
	{
		LOG_BUF_STD(size, buff, count, LOG_INFO);
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return err;
}

VSS_HANDLER(interface_ebi_write32)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	vsf_err_t err = VSFERR_NONE;
	uint8_t index;
	uint32_t address;
	uint8_t size = 4;
	uint32_t *buff = NULL;
	uint16_t count, parsed_num;
	
	VSS_CHECK_ARGC_MIN(4);
	INTERFACE_ASSERT(IFS_EBI, "ebi");
	
	index = (uint8_t)strtoul(argv[1], NULL, 0);
	address = ((uint32_t)strtoul(argv[2], NULL, 0)) << 2;
	count = (uint16_t)strtoul(argv[3], NULL, 0);
	
	VSS_CHECK_ARGC(count + 4);
	
	err = vss_get_binary_buffer(argc - 4, &argv[4], size, count, (void**)&buff,
								&parsed_num);
	if (err)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse data");
		vss_print_help(argv[0]);
	}
	else
	{
		uint16_t i;
		
		for (i = parsed_num; i < count; i++)
		{
			switch (size)
			{
			case 1:
				((uint8_t *)buff)[i] = ((uint8_t *)buff)[parsed_num - 1];
				break;
			case 2:
				((uint16_t *)buff)[i] = ((uint16_t *)buff)[parsed_num - 1];
				break;
			case 4:
				((uint32_t *)buff)[i] = ((uint32_t *)buff)[parsed_num - 1];
				break;
			}
		}
		err = ifs->ebi.write(0, index | EBI_TGTTYP_NOR, address, size,
							(uint8_t *)buff, count);
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return err;
}
#endif

// delay
VSS_HANDLER(interface_delay_us)
{
	uint16_t delay;
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(2);
	if (interface_assert(&ifs) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return VSFERR_FAIL;
	}
	
	delay = (uint16_t)strtoul(argv[1], NULL, 0);
	return ifs->delay.delayus(delay);
}

VSS_HANDLER(interface_delay_ms)
{
	uint16_t delay;
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(2);
	if (interface_assert(&ifs) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return VSFERR_FAIL;
	}
	
	delay = (uint16_t)strtoul(argv[1], NULL, 0);
	return ifs->delay.delayms(delay);
}

// commit
VSS_HANDLER(interface_commit)
{
	struct INTERFACES_INFO_T *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	if (interface_assert(&ifs) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return VSFERR_FAIL;
	}
	
	return ifs->peripheral_commit();
}

