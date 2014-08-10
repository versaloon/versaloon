#ifndef __VSFMEM_H_INCLUDED__
#define __VSFMEM_H_INCLUDED__

#include "dal/mal/mal.h"
#include "dal/cfi/cfi_drv.h"
#include "dal/nand/nand_drv.h"
#include "dal/df25xx/df25xx_drv.h"
#include "dal/sd/sd_common.h"
#include "dal/sd/sd_spi_drv.h"
#include "dal/sd/sd_sdio_drv.h"
#include "dal/ee24cxx/ee24cxx_drv.h"

struct vsfmem_info_t
{
	// public
	// CFI
	struct cfi_t
	{
		// public
		struct cfi_drv_param_t cfi_drv_param;
		struct cfi_drv_interface_t cfi_drv_ifs;
		struct mal_info_t cfi_mal_info;
		struct dal_info_t cfi_handle;
		
		// private
		struct cfi_drv_info_t cfi_drv_info;
	} cfi;
	
	// Nand
	struct nand_t
	{
		// public
		struct nand_drv_param_t nand_drv_param;
		struct nand_drv_interface_t nand_drv_ifs;
		struct mal_info_t nand_mal_info;
		struct dal_info_t nand_handle;
		
		// private
		struct nand_drv_info_t nand_drv_info;
	} nand;
	
	// SD
	struct sd_t
	{
		// public
		struct sd_param_t sd_param;
		struct sd_spi_drv_interface_t sd_spi_drv_ifs;
		struct mal_info_t sd_mal_info;
		struct dal_info_t sd_handle;
		
		// private
		struct sd_info_t sd_info;
		struct sd_spi_drv_info_t sd_spi_drv_info;
	} sd;
	
	// Dataflash
	struct df25xx_t
	{
		// public
		struct df25xx_drv_param_t df25xx_drv_param;
		struct df25xx_drv_interface_t df25xx_drv_ifs;
		struct mal_info_t df25xx_mal_info;
		struct dal_info_t df25xx_handle;
		
		// private
		struct df25xx_drv_info_t df25xx_drv_info;
	} df25xx;
	
	// EEPROM
	struct ee24cxx_t
	{
		// public
		struct ee24cxx_drv_param_t ee24cxx_drv_param;
		struct ee24cxx_drv_interface_t ee24cxx_drv_ifs;
		struct mal_info_t ee24cxx_mal_info;
		struct dal_info_t ee24cxx_handle;
	} ee24cxx;
	
	// private
};

vsf_err_t vsfmem_init(struct vsfmem_info_t *vsfmem);
vsf_err_t vsfmem_config(struct vsfmem_info_t *vsfmem, uint8_t cfi_port,
				uint8_t nand_port, uint8_t sd_spi_port, uint8_t df_spi_port,
				uint8_t cle, uint8_t ale, uint8_t eeprom_addr);

#endif		// __VSFMEM_H_INCLUDED__

