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

#include "compiler.h"

#include "app_type.h"

#include "interfaces.h"

#include "dal/mal/mal.h"
#include "dal/mal/mal_driver.h"

#include "mal_embflash.h"

static vsf_err_t embflash_drv_init_nb(struct dal_info_t *info)
{
	struct embflash_param_t *param = (struct embflash_param_t *)info->param;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	uint32_t pagesize, pagenum;
	
	if (interfaces->flash.init(param->index) ||
		interfaces->flash.getcapacity(param->index, &pagesize, &pagenum) ||
		(0 == pagesize))
	{
		return VSFERR_NONE;
	}
	
	mal_info->capacity.block_size = pagesize;
	mal_info->capacity.block_number = pagenum;
	return VSFERR_NONE;
}

static vsf_err_t embflash_drv_fini(struct dal_info_t *info)
{
	struct embflash_param_t *param = (struct embflash_param_t *)info->param;
	return interfaces->flash.fini(param->index);
}

static vsf_err_t embflash_drv_eraseblock_nb_start(struct dal_info_t *info,
										uint64_t address, uint64_t count)
{
	struct embflash_param_t *param = (struct embflash_param_t *)info->param;
	
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	return interfaces->flash.unlock(param->index);
}

static vsf_err_t embflash_drv_eraseblock_nb(struct dal_info_t *info,
											uint64_t address)
{
	struct embflash_param_t *param = (struct embflash_param_t *)info->param;
	return interfaces->flash.erasepage(param->index, address);
}

static vsf_err_t embflash_drv_eraseblock_nb_isready(struct dal_info_t *info,
													uint64_t address)
{
	struct embflash_param_t *param = (struct embflash_param_t *)info->param;
	return interfaces->flash.erasepage_isready(param->index, address);
}

static vsf_err_t embflash_drv_eraseblock_nb_end(struct dal_info_t *info)
{
	struct embflash_param_t *param = (struct embflash_param_t *)info->param;
	return interfaces->flash.unlock(param->index);
}

static vsf_err_t embflash_drv_readblock_nb_start(struct dal_info_t *info, 
								uint64_t address, uint64_t count, uint8_t *buff)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	REFERENCE_PARAMETER(buff);
	return VSFERR_NONE;
}

static vsf_err_t embflash_drv_readblock_nb(struct dal_info_t *info, 
											uint64_t address, uint8_t *buff)
{
	struct embflash_param_t *param = (struct embflash_param_t *)info->param;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	
	return interfaces->flash.read(param->index, address, buff,
									mal_info->capacity.block_size);
}

static vsf_err_t embflash_drv_readblock_nb_isready(struct dal_info_t *info, 
												uint64_t address, uint8_t *buff)
{
	struct embflash_param_t *param = (struct embflash_param_t *)info->param;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	
	return interfaces->flash.read_isready(param->index,
								address, buff, mal_info->capacity.block_size);
}

static vsf_err_t embflash_drv_readblock_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return VSFERR_NONE;
}

static vsf_err_t embflash_drv_writeblock_nb_start(struct dal_info_t *info, 
								uint64_t address, uint64_t count, uint8_t *buff)
{
	struct embflash_param_t *param = (struct embflash_param_t *)info->param;
	
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	REFERENCE_PARAMETER(buff);
	return interfaces->flash.unlock(param->index);
}

static vsf_err_t embflash_drv_writeblock_nb(struct dal_info_t *info, 
											uint64_t address, uint8_t *buff)
{
	struct embflash_param_t *param = (struct embflash_param_t *)info->param;
	
	REFERENCE_PARAMETER(buff);
	param->erased = false;
	return interfaces->flash.erasepage(param->index, address);
}

static vsf_err_t embflash_drv_writeblock_nb_isready(struct dal_info_t *info, 
												uint64_t address, uint8_t *buff)
{
	struct embflash_param_t *param = (struct embflash_param_t *)info->param;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	vsf_err_t err;
	
	if (param->erased)
	{
		return interfaces->flash.write_isready(param->index,
								address, buff, mal_info->capacity.block_size);
	}
	else
	{
		err = interfaces->flash.erasepage_isready(param->index, address);
		if (err)
		{
			return err;
		}
		param->erased = true;
		return interfaces->flash.write(param->index, address, buff,
										mal_info->capacity.block_size);
	}
}

static vsf_err_t embflash_drv_writeblock_nb_end(struct dal_info_t *info)
{
	struct embflash_param_t *param = (struct embflash_param_t *)info->param;
	return interfaces->flash.unlock(param->index);
}

#if DAL_INTERFACE_PARSER_EN
static vsf_err_t embflash_drv_parse_interface(struct dal_info_t *info, 
												uint8_t *buff)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(buff);
	return VSFERR_NONE;
}
#endif

struct mal_driver_t embflash_drv = 
{
	{
		"embflash",
#if DAL_INTERFACE_PARSER_EN
		"",
		embflash_drv_parse_interface,
#endif
	},
	
	MAL_SUPPORT_ERASEBLOCK | MAL_SUPPORT_WRITEBLOCK | MAL_SUPPORT_READBLOCK,
	
	embflash_drv_init_nb,
	NULL,
	embflash_drv_fini,
	NULL,
	NULL,
	
	NULL, NULL, NULL, NULL,
	
	NULL, NULL, NULL, NULL,
	
	embflash_drv_eraseblock_nb_start,
	embflash_drv_eraseblock_nb,
	embflash_drv_eraseblock_nb_isready,
	NULL,
	embflash_drv_eraseblock_nb_end,
	
	embflash_drv_readblock_nb_start,
	embflash_drv_readblock_nb,
	embflash_drv_readblock_nb_isready,
	NULL,
	embflash_drv_readblock_nb_end,
	
	embflash_drv_writeblock_nb_start,
	embflash_drv_writeblock_nb,
	embflash_drv_writeblock_nb_isready,
	NULL,
	embflash_drv_writeblock_nb_end
};
