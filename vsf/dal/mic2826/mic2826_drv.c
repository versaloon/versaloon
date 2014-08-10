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

#include "app_cfg.h"
#include "app_type.h"

#include "../dal.h"

#if DAL_MIC2826_EN

#include "mic2826_drv_cfg.h"
#include "mic2826_drv.h"

#define MIC2826_IIC_ADDR					0xB4
#define MIC2826_IIC_RETRY					10

#define MIC2826_REG_ENABLE					0x00
#define MIC2826_REG_STATUS					0x01
#define MIC2826_REG_DCDC					0x02
#define MIC2826_REG_LDO1					0x03
#define MIC2826_REG_LDO2					0x04
#define MIC2826_REG_LDO3					0x05

#define MIC2826_REG_ENABLE_POAF				0x20
#define MIC2826_REG_ENABLE_SEQCNT			0x10
#define MIC2826_REG_ENABLE_LDO3				0x08
#define MIC2826_REG_ENABLE_LDO2				0x04
#define MIC2826_REG_ENABLE_LDO1				0x02
#define MIC2826_REG_ENABLE_DCDC				0x01

static uint8_t mic2826_drv_ldo_regmap[] = 
{
	0x00, 0x0B, 0x14, 0x1D, 0x25, 0x2E, 0x37, 0x3E, 
	0x45, 0x4C, 0x52, 0x57, 0x5C, 0x61, 0x65, 0x69, 
	0x6D, 0x72, 0x79, 0x7F, 0x85, 0x8B, 0x91, 0x96, 
	0x9A, 0x9F, 0xA4, 0xA8, 0xAC, 0xB0, 0xB4, 0xB7, 
	0xBA, 0xBD, 0xC1, 0xC4, 0xC7, 0xC9, 0xCC, 0xCE, 
	0xD1, 0xD3, 0xD6, 0xD8, 0xDA, 0xDC, 0xDE, 0xE1, 
	0xE3, 0xE6, 0xE8
};

static uint8_t mic2826_dcdc_voltage2reg(uint16_t mV)
{
	// 800 - 1175: 25 step
	// 1200 - 1800: 50 step
	if ((mV >= 1200) && (mV <= 1800))
	{
		return (uint8_t)((1200 - 800) / 25 + (mV - 1200) / 50);
	}
	else if ((mV >= 800) && (mV < 1200))
	{
		return (uint8_t)((mV - 800) / 25);
	}
	else
	{
		return 0;
	}
}

static uint8_t mic2826_ldo_voltage2reg(uint16_t mV)
{
	// 800 - 3300: 25 step
	if ((mV >= 800) && (mV <= 3300))
	{
		return mic2826_drv_ldo_regmap[(mV - 800) / 50];
	}
	else
	{
		return 0;
	}
}

static vsf_err_t mic2826_write_reg(struct mic2826_drv_interface_t *ifs, 
									uint8_t addr, uint8_t reg)
{
	uint8_t cmd[2];
	uint8_t retry;
	vsf_err_t err = VSFERR_FAIL;
	
	cmd[0] = addr;
	cmd[1] = reg;
	retry = MIC2826_IIC_RETRY;
	while (retry--)
	{
		if (interfaces->i2c.write(ifs->iic_port, MIC2826_IIC_ADDR, cmd, 2, 1) ||
			interfaces->peripheral_commit())
		{
			continue;
		}
		err = VSFERR_NONE;
		break;
	}
	return err;
}

static vsf_err_t mic2826_drv_init(struct dal_info_t *info)
{
	struct mic2826_drv_interface_t *ifs = 
								(struct mic2826_drv_interface_t *)info->ifs;
	struct mic2826_drv_param_t *param = 
								(struct mic2826_drv_param_t *)info->param;
	
	interfaces->i2c.init(ifs->iic_port);
	interfaces->i2c.config(ifs->iic_port, param->kHz, 0, 0xFFFF);
	return interfaces->peripheral_commit();
}

static vsf_err_t mic2826_drv_fini(struct dal_info_t *info)
{
	struct mic2826_drv_interface_t *ifs = 
								(struct mic2826_drv_interface_t *)info->ifs;
	
	interfaces->i2c.fini(ifs->iic_port);
	return interfaces->peripheral_commit();
}

static vsf_err_t mic2826_drv_config(struct dal_info_t *info, uint16_t DCDC_mV, 
						uint16_t LDO1_mV, uint16_t LDO2_mV, uint16_t LDO3_mV)
{
	struct mic2826_drv_interface_t *ifs = 
								(struct mic2826_drv_interface_t *)info->ifs;
	uint8_t en_reg;
	
	if ((((DCDC_mV != 0) && (DCDC_mV < 800)) || (DCDC_mV > 1200)) || 
		(((LDO1_mV != 0) && (LDO1_mV < 800)) || (LDO1_mV > 3300)) || 
		(((LDO2_mV != 0) && (LDO2_mV < 800)) || (LDO2_mV > 3300)) || 
		(((LDO3_mV != 0) && (LDO3_mV < 800)) || (LDO3_mV > 3300)))
	{
		return VSFERR_INVALID_RANGE;
	}
	
	en_reg = (DCDC_mV ? MIC2826_REG_ENABLE_DCDC : 0) 
				| (LDO1_mV ? MIC2826_REG_ENABLE_LDO1 : 0) 
				| (LDO2_mV ? MIC2826_REG_ENABLE_LDO2 : 0) 
#if MIC2826_HAS_LDO3
				| (LDO3_mV ? MIC2826_REG_ENABLE_LDO3 : 0)
#endif
				;
	
	if (mic2826_write_reg(ifs, MIC2826_REG_DCDC,
							mic2826_dcdc_voltage2reg(DCDC_mV))
		|| mic2826_write_reg(ifs, MIC2826_REG_LDO1, 
							mic2826_ldo_voltage2reg(LDO1_mV))
		|| mic2826_write_reg(ifs, MIC2826_REG_LDO2, 
							mic2826_ldo_voltage2reg(LDO2_mV))
#if MIC2826_HAS_LDO3
		|| mic2826_write_reg(ifs, MIC2826_REG_LDO3, 
							mic2826_ldo_voltage2reg(LDO3_mV))
#endif
		|| mic2826_write_reg(ifs, MIC2826_REG_ENABLE, en_reg))
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

#if DAL_INTERFACE_PARSER_EN
static vsf_err_t mic2826_drv_parse_interface(struct dal_info_t *info, 
												uint8_t *buff)
{
	struct mic2826_drv_interface_t *ifs = 
								(struct mic2826_drv_interface_t *)info->ifs;
	
	ifs->iic_port = buff[0];
	return VSFERR_NONE;
}
#endif

const struct mic2826_drv_t mic2826_drv = 
{
	{
		"mic2826",
#if DAL_INTERFACE_PARSER_EN
		"iic:%1d",
		mic2826_drv_parse_interface,
#endif
	},
	
	mic2826_drv_init,
	mic2826_drv_fini,
	mic2826_drv_config
};

#endif
