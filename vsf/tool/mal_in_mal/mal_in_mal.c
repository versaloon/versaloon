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

#include "mal_in_mal.h"

static vsf_err_t malinmal_drv_init_nb(struct dal_info_t *info)
{
	struct malinmal_param_t *param = (struct malinmal_param_t *)info->param;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	struct mal_info_t *malp_info = (struct mal_info_t *)param->maldal->extra;
	
	mal_info->capacity.block_size = malp_info->capacity.block_size;
	mal_info->capacity.block_number = param->size / mal_info->capacity.block_size;
	mal_info->erase_page_size = malp_info->erase_page_size;
	mal_info->read_page_size = malp_info->read_page_size;
	mal_info->write_page_size = malp_info->write_page_size;
	return VSFERR_NONE;
}

static vsf_err_t malinmal_drv_fini(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return VSFERR_NONE;
}

static vsf_err_t malinmal_drv_getinfo(struct dal_info_t *info)
{
	struct malinmal_param_t *param = (struct malinmal_param_t *)info->param;
	struct mal_info_t *malp_info = (struct mal_info_t *)param->maldal->extra;
	struct mal_driver_t* mal_driver = (struct mal_driver_t *)malp_info->driver;
	
	if ((NULL == mal_driver) || (NULL == mal_driver->getinfo))
	{
		return VSFERR_NOT_SUPPORT;
	}
	
	return mal_driver->getinfo(param->maldal);
}

static vsf_err_t malinmal_drv_eraseblock_nb_start(struct dal_info_t *info,
										uint64_t address, uint64_t count)
{
	struct malinmal_param_t *param = (struct malinmal_param_t *)info->param;
	struct mal_info_t *malp_info = (struct mal_info_t *)param->maldal->extra;
	struct mal_driver_t* mal_driver = (struct mal_driver_t *)malp_info->driver;
	
	if ((NULL == mal_driver) || (NULL == mal_driver->eraseblock_nb_start))
	{
		return VSFERR_NOT_SUPPORT;
	}
	
	return mal_driver->eraseblock_nb_start(param->maldal, param->addr + address,
											count);
}

static vsf_err_t malinmal_drv_eraseblock_nb(struct dal_info_t *info,
											uint64_t address)
{
	struct malinmal_param_t *param = (struct malinmal_param_t *)info->param;
	struct mal_info_t *malp_info = (struct mal_info_t *)param->maldal->extra;
	struct mal_driver_t* mal_driver = (struct mal_driver_t *)malp_info->driver;
	
	if ((NULL == mal_driver) || (NULL == mal_driver->eraseblock_nb))
	{
		return VSFERR_NOT_SUPPORT;
	}
	
	return mal_driver->eraseblock_nb(param->maldal, param->addr + address);
}

static vsf_err_t malinmal_drv_eraseblock_nb_isready(struct dal_info_t *info,
													uint64_t address)
{
	struct malinmal_param_t *param = (struct malinmal_param_t *)info->param;
	struct mal_info_t *malp_info = (struct mal_info_t *)param->maldal->extra;
	struct mal_driver_t* mal_driver = (struct mal_driver_t *)malp_info->driver;
	
	if ((NULL == mal_driver) || (NULL == mal_driver->eraseblock_nb_isready))
	{
		return VSFERR_NOT_SUPPORT;
	}
	
	return mal_driver->eraseblock_nb_isready(param->maldal, param->addr + address);
}

static vsf_err_t malinmal_drv_eraseblock_nb_end(struct dal_info_t *info)
{
	struct malinmal_param_t *param = (struct malinmal_param_t *)info->param;
	struct mal_info_t *malp_info = (struct mal_info_t *)param->maldal->extra;
	struct mal_driver_t* mal_driver = (struct mal_driver_t *)malp_info->driver;
	
	if ((NULL == mal_driver) || (NULL == mal_driver->eraseblock_nb_end))
	{
		return VSFERR_NOT_SUPPORT;
	}
	
	return mal_driver->eraseblock_nb_end(param->maldal);
}

static vsf_err_t malinmal_drv_readblock_nb_start(struct dal_info_t *info, 
								uint64_t address, uint64_t count, uint8_t *buff)
{
	struct malinmal_param_t *param = (struct malinmal_param_t *)info->param;
	struct mal_info_t *malp_info = (struct mal_info_t *)param->maldal->extra;
	struct mal_driver_t* mal_driver = (struct mal_driver_t *)malp_info->driver;
	
	if ((NULL == mal_driver) || (NULL == mal_driver->readblock_nb_start))
	{
		return VSFERR_NOT_SUPPORT;
	}
	
	return mal_driver->readblock_nb_start(param->maldal, param->addr + address,
											count, buff);
}

static vsf_err_t malinmal_drv_readblock_nb(struct dal_info_t *info, 
											uint64_t address, uint8_t *buff)
{
	struct malinmal_param_t *param = (struct malinmal_param_t *)info->param;
	struct mal_info_t *malp_info = (struct mal_info_t *)param->maldal->extra;
	struct mal_driver_t* mal_driver = (struct mal_driver_t *)malp_info->driver;
	
	if ((NULL == mal_driver) || (NULL == mal_driver->readblock_nb))
	{
		return VSFERR_NOT_SUPPORT;
	}
	
	return mal_driver->readblock_nb(param->maldal, param->addr + address, buff);
}

static vsf_err_t malinmal_drv_readblock_nb_isready(struct dal_info_t *info, 
												uint64_t address, uint8_t *buff)
{
	struct malinmal_param_t *param = (struct malinmal_param_t *)info->param;
	struct mal_info_t *malp_info = (struct mal_info_t *)param->maldal->extra;
	struct mal_driver_t* mal_driver = (struct mal_driver_t *)malp_info->driver;
	
	if ((NULL == mal_driver) || (NULL == mal_driver->readblock_nb_isready))
	{
		return VSFERR_NOT_SUPPORT;
	}
	
	return mal_driver->readblock_nb_isready(param->maldal,
											param->addr + address, buff);
}

static vsf_err_t malinmal_drv_readblock_nb_end(struct dal_info_t *info)
{
	struct malinmal_param_t *param = (struct malinmal_param_t *)info->param;
	struct mal_info_t *malp_info = (struct mal_info_t *)param->maldal->extra;
	struct mal_driver_t* mal_driver = (struct mal_driver_t *)malp_info->driver;
	
	if ((NULL == mal_driver) || (NULL == mal_driver->readblock_nb_end))
	{
		return VSFERR_NOT_SUPPORT;
	}
	
	return mal_driver->readblock_nb_end(param->maldal);
}

static vsf_err_t malinmal_drv_writeblock_nb_start(struct dal_info_t *info, 
								uint64_t address, uint64_t count, uint8_t *buff)
{
	struct malinmal_param_t *param = (struct malinmal_param_t *)info->param;
	struct mal_info_t *malp_info = (struct mal_info_t *)param->maldal->extra;
	struct mal_driver_t* mal_driver = (struct mal_driver_t *)malp_info->driver;
	
	if ((NULL == mal_driver) || (NULL == mal_driver->writeblock_nb_start))
	{
		return VSFERR_NOT_SUPPORT;
	}
	
	return mal_driver->writeblock_nb_start(param->maldal, param->addr + address,
											count, buff);
}

static vsf_err_t malinmal_drv_writeblock_nb(struct dal_info_t *info, 
											uint64_t address, uint8_t *buff)
{
	struct malinmal_param_t *param = (struct malinmal_param_t *)info->param;
	struct mal_info_t *malp_info = (struct mal_info_t *)param->maldal->extra;
	struct mal_driver_t* mal_driver = (struct mal_driver_t *)malp_info->driver;
	
	if ((NULL == mal_driver) || (NULL == mal_driver->writeblock_nb))
	{
		return VSFERR_NOT_SUPPORT;
	}
	
	return mal_driver->writeblock_nb(param->maldal, param->addr + address,
										buff);
}

static vsf_err_t malinmal_drv_writeblock_nb_isready(struct dal_info_t *info, 
												uint64_t address, uint8_t *buff)
{
	struct malinmal_param_t *param = (struct malinmal_param_t *)info->param;
	struct mal_info_t *malp_info = (struct mal_info_t *)param->maldal->extra;
	struct mal_driver_t* mal_driver = (struct mal_driver_t *)malp_info->driver;
	
	if ((NULL == mal_driver) || (NULL == mal_driver->writeblock_nb_isready))
	{
		return VSFERR_NOT_SUPPORT;
	}
	
	return mal_driver->writeblock_nb_isready(param->maldal,
												param->addr + address, buff);
}

static vsf_err_t malinmal_drv_writeblock_nb_end(struct dal_info_t *info)
{
	struct malinmal_param_t *param = (struct malinmal_param_t *)info->param;
	struct mal_info_t *malp_info = (struct mal_info_t *)param->maldal->extra;
	struct mal_driver_t* mal_driver = (struct mal_driver_t *)malp_info->driver;
	
	if ((NULL == mal_driver) || (NULL == mal_driver->writeblock_nb_end))
	{
		return VSFERR_NOT_SUPPORT;
	}
	
	return mal_driver->writeblock_nb_end(param->maldal);
}

#if DAL_INTERFACE_PARSER_EN
static vsf_err_t malinmal_drv_parse_interface(struct dal_info_t *info, 
												uint8_t *buff)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(buff);
	return VSFERR_NONE;
}
#endif

struct mal_driver_t malinmal_drv = 
{
	{
		"malinmal",
#if DAL_INTERFACE_PARSER_EN
		"",
		malinmal_drv_parse_interface,
#endif
	},
	
	MAL_SUPPORT_ERASEBLOCK | MAL_SUPPORT_WRITEBLOCK | MAL_SUPPORT_READBLOCK,
	
	malinmal_drv_init_nb,
	NULL,
	malinmal_drv_fini,
	malinmal_drv_getinfo,
	NULL,
	
	NULL, NULL, NULL, NULL,
	
	NULL, NULL, NULL, NULL,
	
	malinmal_drv_eraseblock_nb_start,
	malinmal_drv_eraseblock_nb,
	malinmal_drv_eraseblock_nb_isready,
	NULL,
	malinmal_drv_eraseblock_nb_end,
	
	malinmal_drv_readblock_nb_start,
	malinmal_drv_readblock_nb,
	malinmal_drv_readblock_nb_isready,
	NULL,
	malinmal_drv_readblock_nb_end,
	
	malinmal_drv_writeblock_nb_start,
	malinmal_drv_writeblock_nb,
	malinmal_drv_writeblock_nb_isready,
	NULL,
	malinmal_drv_writeblock_nb_end
};
