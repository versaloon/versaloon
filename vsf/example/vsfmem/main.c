#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

#include "app_cfg.h"
#include "app_io.h"
#include "app_log.h"

#include "interfaces.h"
#include "vsfmem.h"

static struct vsfmem_info_t vsfmem;

int main(void)
{
	interfaces->core.init(NULL);
	vsfmem_init(&vsfmem);
	vsfmem_config(&vsfmem, 3, 0, 0, 1, 23, 22, 0);
	
	if (!mal.init(&vsfmem.cfi.cfi_handle) &&
		!mal.getinfo(&vsfmem.cfi.cfi_handle))
	{
		LOG_INFO("CFI detected: 0x%08X %dKB * %d blocks",
			((uint32_t)vsfmem.cfi.cfi_drv_info.manufacturer_id << 24) |
			((uint32_t)(vsfmem.cfi.cfi_drv_info.device_id[0] & 0xFF) << 16) |
			((uint32_t)(vsfmem.cfi.cfi_drv_info.device_id[1] & 0xFF) <<  8) |
			((uint32_t)(vsfmem.cfi.cfi_drv_info.device_id[2] & 0xFF) <<  0),
			(int)(vsfmem.cfi.cfi_mal_info.capacity.block_size / 1024),
			(int)vsfmem.cfi.cfi_mal_info.capacity.block_number);
	}
	else
	{
		LOG_ERROR("Fail to connect to CFI");
	}
	if (!mal.init(&vsfmem.nand.nand_handle) &&
		!mal.getinfo(&vsfmem.nand.nand_handle))
	{
		LOG_INFO("NAND detected: 0x%08X",
			((uint32_t)vsfmem.nand.nand_drv_info.manufacturer_id << 24) |
			((uint32_t)(vsfmem.nand.nand_drv_info.device_id[0] & 0xFF) << 16) |
			((uint32_t)(vsfmem.nand.nand_drv_info.device_id[1] & 0xFF) <<  8) |
			((uint32_t)(vsfmem.nand.nand_drv_info.device_id[2] & 0xFF) <<  0));
	}
	else
	{
		LOG_ERROR("Fail to connect to NAND");
	}
	if (!mal.init(&vsfmem.sd.sd_handle) && !mal.getinfo(&vsfmem.sd.sd_handle))
	{
		LOG_INFO("SD detected: %d MBytes",
			(uint32_t)(vsfmem.sd.sd_mal_info.capacity.block_size *
			vsfmem.sd.sd_mal_info.capacity.block_number / (1024 * 1024)));
	}
	else
	{
		LOG_ERROR("Fail to connect to SD");
	}
	if (!mal.init(&vsfmem.df25xx.df25xx_handle) &&
		!mal.getinfo(&vsfmem.df25xx.df25xx_handle))
	{
		LOG_INFO("Dataflash detected: 0x%02X:0x%04X",
					vsfmem.df25xx.df25xx_drv_info.manufacturer_id,
					vsfmem.df25xx.df25xx_drv_info.device_id);
	}
	else
	{
		LOG_ERROR("Fail to connect to Dataflash");
	}
	
	return 0;
}
