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

#include "compiler.h"

#include "interfaces.h"
#include "usbtoxxx.h"
#include "usbtoxxx_internal.h"

vsf_err_t usbtoebi_init(uint8_t index)
{
	return usbtoxxx_init_command(USB_TO_EBI, index);
}

vsf_err_t usbtoebi_fini(uint8_t index)
{
	return usbtoxxx_fini_command(USB_TO_EBI, index);
}

vsf_err_t usbtoebi_config(uint8_t index, uint8_t target_index,
						void *param)
{
	uint8_t target_type = target_index & 0xF0;
	struct ebi_sram_psram_nor_info_t *nor_info = (struct ebi_sram_psram_nor_info_t *)param;
	struct ebi_nand_info_t *nand_info = (struct ebi_nand_info_t *)param;
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	usbtoxxx_info->cmd_buff[0] = target_index;
	switch (target_type)
	{
	case EBI_TGTTYP_NOR:
		usbtoxxx_info->cmd_buff[1] = nor_info->common_info.data_width;
		usbtoxxx_info->cmd_buff[2] = nor_info->common_info.wait_signal;
		usbtoxxx_info->cmd_buff[3] = nor_info->param.addr_multiplex ? 1 : 0;
		SET_LE_U16(&usbtoxxx_info->cmd_buff[4], 
								nor_info->param.timing.address_setup_cycle_r);
		SET_LE_U16(&usbtoxxx_info->cmd_buff[6], 
								nor_info->param.timing.address_hold_cycle_r);
		SET_LE_U16(&usbtoxxx_info->cmd_buff[8], 
								nor_info->param.timing.data_setup_cycle_r);
		SET_LE_U32(&usbtoxxx_info->cmd_buff[10], 
								nor_info->param.timing.clock_hz_r);
		SET_LE_U16(&usbtoxxx_info->cmd_buff[14], 
								nor_info->param.timing.address_setup_cycle_w);
		SET_LE_U16(&usbtoxxx_info->cmd_buff[16], 
								nor_info->param.timing.address_hold_cycle_w);
		SET_LE_U16(&usbtoxxx_info->cmd_buff[18], 
								nor_info->param.timing.data_setup_cycle_w);
		SET_LE_U32(&usbtoxxx_info->cmd_buff[20], nor_info->param.timing.clock_hz_w);
		
		return usbtoxxx_conf_command(USB_TO_EBI, index,
										usbtoxxx_info->cmd_buff, 24);
	case EBI_TGTTYP_NAND:
		usbtoxxx_info->cmd_buff[1] = nand_info->common_info.data_width;
		usbtoxxx_info->cmd_buff[2] = nand_info->common_info.wait_signal;
		SET_LE_U32(&usbtoxxx_info->cmd_buff[3], nand_info->param.clock_hz);
		usbtoxxx_info->cmd_buff[7] = nand_info->param.ecc.ecc_enable ? 1 : 0;
		SET_LE_U16(&usbtoxxx_info->cmd_buff[8], nand_info->param.ecc.ecc_page_size);
		usbtoxxx_info->cmd_buff[10] = nand_info->param.timing.ale_to_re_cycle;
		usbtoxxx_info->cmd_buff[11] = nand_info->param.timing.cle_to_re_cycle;
		SET_LE_U16(&usbtoxxx_info->cmd_buff[12], nand_info->param.timing.setup_cycle);
		SET_LE_U16(&usbtoxxx_info->cmd_buff[14], nand_info->param.timing.wait_cycle);
		usbtoxxx_info->cmd_buff[16] = nand_info->param.timing.hold_cycle;
		usbtoxxx_info->cmd_buff[17] = nand_info->param.timing.hiz_cycle;
		SET_LE_U16(&usbtoxxx_info->cmd_buff[18], 
									nand_info->param.timing.setup_cycle_attr);
		SET_LE_U16(&usbtoxxx_info->cmd_buff[20], 
									nand_info->param.timing.wait_cycle_attr);
		usbtoxxx_info->cmd_buff[22] = nand_info->param.timing.hold_cycle_attr;
		usbtoxxx_info->cmd_buff[23] = nand_info->param.timing.hiz_cycle_attr;
		
		return usbtoxxx_conf_command(USB_TO_EBI, index,
										usbtoxxx_info->cmd_buff, 24);
	default:
		return VSFERR_FAIL;
	}
}

vsf_err_t usbtoebi_isready(uint8_t index, uint8_t target_index)
{
	int8_t ret;
	
	if (usbtoxxx_status_command(USB_TO_EBI, index, &target_index, 1, 1,
									(uint8_t *)&ret, 0, 1, 0) ||
		interfaces->peripheral_commit())
	{
		return VSFERR_FAIL;
	}
	
	return (vsf_err_t)ret;
}

vsf_err_t usbtoebi_read(uint8_t index, uint8_t target_index,
			uint32_t address,uint8_t data_size, uint8_t *buff, uint32_t count)
{
	uint16_t cur_count;
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	while (count)
	{
		cur_count = (uint16_t)min(count,
			(uint32_t)((usbtoxxx_info->buff_len - 16) / data_size));
		
		usbtoxxx_info->cmd_buff[0] = target_index;
		usbtoxxx_info->cmd_buff[1] = data_size;
		SET_LE_U32(&usbtoxxx_info->cmd_buff[2], address);
		SET_LE_U32(&usbtoxxx_info->cmd_buff[6], cur_count);
		
		if (usbtoxxx_in_command(USB_TO_EBI, index, usbtoxxx_info->cmd_buff,
				(uint16_t)(10 + cur_count * data_size), 
				(uint16_t)(cur_count * data_size), buff, 0, 
				(uint16_t)(cur_count * data_size), 0))
		{
			return VSFERR_FAIL;
		}
		
		count -= cur_count;
		address += cur_count * data_size;
	}
	return VSFERR_NONE;
}

vsf_err_t usbtoebi_write(uint8_t index, uint8_t target_index,
			uint32_t address,uint8_t data_size, uint8_t *buff, uint32_t count)
{
	uint16_t cur_count;
	uint32_t i;
	
#if PARAM_CHECK
	if (index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, index);
		return VSFERR_FAIL;
	}
#endif
	
	while (count)
	{
		cur_count = (uint16_t)min(count,
			(uint32_t)((usbtoxxx_info->buff_len - 16) / data_size));
		
		usbtoxxx_info->cmd_buff[0] = target_index;
		usbtoxxx_info->cmd_buff[1] = data_size;
		SET_LE_U32(&usbtoxxx_info->cmd_buff[2], address);
		SET_LE_U32(&usbtoxxx_info->cmd_buff[6], cur_count);
		switch (data_size)
		{
		case 1:
			memcpy(&usbtoxxx_info->cmd_buff[10], buff, data_size * cur_count);
			break;
		case 2:
			for (i = 0; i < cur_count; i++)
			{
				SET_LE_U16(&usbtoxxx_info->cmd_buff[10 + i * 2], 
							GET_SYS_U16(&buff[i * 2]));
			}
			break;
		case 4:
			for (i = 0; i < cur_count; i++)
			{
				SET_LE_U32(&usbtoxxx_info->cmd_buff[10 + i * 4], 
							GET_SYS_U32(&buff[i * 4]));
			}
			break;
		default:
			return VSFERR_FAIL;
		}
		if (usbtoxxx_out_command(USB_TO_EBI, index, usbtoxxx_info->cmd_buff,
									(uint16_t)(10 + cur_count * data_size), 0))
		{
			return VSFERR_FAIL;
		}
		
		count -= cur_count;
		address += cur_count * data_size;
	}
	return VSFERR_NONE;
}
