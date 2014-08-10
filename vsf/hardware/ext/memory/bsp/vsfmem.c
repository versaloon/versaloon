#include "interfaces.h"

#include "vsfmem.h"

static struct vsfmem_info_t vsfmem_template =
{
	// public
	// CFI
	{
		{
			{
				{
					16,				// uint8_t data_width;
					EBI_WAIT_NONE	// enum wait_signal_t wait_signal;
				},					// struct ebi_info_t common_info;
				{
					// A0-15 == D0-15 with ALE
					false,			// bool addr_multiplex;
					
					{
						2,			// uint16_t address_setup_cycle_r;
						0,			// uint16_t address_hold_cycle_r;
						7,			// uint16_t data_setup_cycle_r;
						0,			// uint32_t clock_hz_r;
						2,			// uint16_t address_setup_cycle_w;
						0,			// uint16_t address_hold_cycle_w;
						7,			// uint16_t data_setup_cycle_w;
						0			// uint32_t clock_hz_w;
					}				// struct ebi_sram_param_nor_timing_t timing;
				}					// struct ebi_sram_psram_nor_param_t param;
			},						// struct ebi_nor_info_t nor_info;
		},							// struct cfi_drv_param_t cfi_drv_param;
		{
			0,						// uint8_t ebi_port;
			0,						// uint8_t nor_index;
		},							// struct cfi_drv_interface_t cfi_drv_ifs;
		{
			{
				0,					// uint64_t block_size;
				0					// uint64_t block_number;
			},						// struct mal_capacity_t;
			NULL,					// void *extra;
			0,						// uint32_t erase_page_size;
			0,						// uint32_t write_page_size;
			0,						// uint32_t read_page_size;
			&cfi_drv				// const struct mal_driver_t *driver;
		},							// struct mal_info_t cfi_mal_info;
		{
			NULL, NULL, NULL, NULL
		},							// struct dal_info_t cfi_handle;
	},								// struct cfi_t cfi;
	
	// Nand
	{
		{
			{
				{
					8,				// uint8_t data_width;
					EBI_WAIT_NONE	// enum wait_signal_t wait_signal;
				},					// struct ebi_info_t common_info;
				{
					0,				// uint32_t clock_hz;
					{
						true,		// bool ecc_enable;
						512			// uint16_t ecc_page_size;
					},				// struct ebi_nand_ecc_t ecc;
					{
						1,			// uint8_t ale_to_re_cycle;
						1,			// uint8_t cle_to_re_cycle;
						10,			// uint16_t setup_cycle;
						10,			// uint16_t wait_cycle;
						10,			// uint8_t hold_cycle;
						10,			// uint8_t hiz_cycle;
						10,			// uint16_t setup_cycle_attr;
						10,			// uint16_t wait_cycle_attr;
						10,			// uint8_t hold_cycle_attr;
						10,			// uint8_t hiz_cycle_attr;
					},				// struct ebi_nand_timing_t timing;
					{
						0,			// uint32_t cmd;
						0,			// uint32_t addr;
						0			// uint32_t data;
					},				// struct ebi_nand_addr_t addr;
				},					// struct ebi_nand_param_t param;
			},						//struct ebi_nand_info_t nand_info;
			false,					// bool block_read_en;
			2,						// uint8_t col_addr_size;
			10,						// uint8_t col_addr_msb;
			11,						// uint8_t row_addr_lsb;
			2,						// uint8_t row_addr_size;
		},							// struct nand_drv_param_t nand_drv_param;
		{
			0,						// uint8_t ebi_port;
			0,						// uint8_t nand_index;
		},							// struct nand_drv_interface_t nand_drv_ifs;
		{
			{
				0,					// uint64_t block_size;
				0					// uint64_t block_number;
			},						// struct mal_capacity_t;
			NULL,					// void *extra;
			0,						// uint32_t erase_page_size;
			0,						// uint32_t write_page_size;
			0,						// uint32_t read_page_size;
			&nand_drv				// const struct mal_driver_t *driver;
		},							// struct mal_info_t nand_mal_info;
		{
			NULL, NULL, NULL, NULL
		},							// struct dal_info_t nand_handle;
	},								// struct nand_t nand;
	
	// SD
	{
		{
			20 * 1000				// uint16_t kHz;
		},							// struct sd_param_t sd_param;
		{
			0xFF,					// uint8_t cs_port;
			0,						// uint32_t cs_pin;
			0,						// uint8_t spi_port;
		},							// struct sd_spi_drv_interface_t sd_spi_drv_ifs;
		{
			{
				0,					// uint64_t block_size;
				0					// uint64_t block_number;
			},						// struct mal_capacity_t;
			NULL,					// void *extra;
			0,						// uint32_t erase_page_size;
			0,						// uint32_t write_page_size;
			0,						// uint32_t read_page_size;
			&sd_spi_drv				// const struct mal_driver_t *driver;
		},							// struct mal_info_t sd_mal_info;
		{
			NULL, NULL, NULL, NULL
		},							// struct dal_info_t sd_handle;
	},								// struct sd_t sd;
	
	// Dataflash
	{
		{
			8 * 1000,				// uint16_t spi_khz;
		},							// struct df25xx_drv_param_t df25xx_drv_param;
		{
			0xFF,					// uint8_t cs_port;
			0,						// uint32_t cs_pin;
			0,						// uint8_t spi_port;
		},							// struct df25xx_drv_interface_t df25xx_drv_ifs;
		{
			{
				4 * 1024,			// uint64_t block_size;
				4 * 1024 / 4		// uint64_t block_number;
			},						// struct mal_capacity_t;
			NULL,					// void *extra;
			0,						// uint32_t erase_page_size;
			0,						// uint32_t write_page_size;
			0,						// uint32_t read_page_size;
			&df25xx_drv				// const struct mal_driver_t *driver;
		},							// struct mal_info_t df25xx_mal_info;
		{
			NULL, NULL, NULL, NULL
		},							// struct dal_info_t df25xx_handle;
	},								// struct df25xx_t df25xx;
	
	// EEPROM
	{
		{
			100,					// uint16_t iic_khz;
			0,						// uint8_t iic_addr;
		},							// struct ee24cxx_drv_param_t ee24cxx_drv_param;
		{
			0,						// uint8_t iic_port;
		},							// struct ee24cxx_drv_interface_t ee24cxx_drv_ifs;
		{
			{
				128,				// uint64_t block_size;
				64 * 1024 / 128		// uint64_t block_number;
			},						// struct mal_capacity_t;
			NULL,					// void *extra;
			0,						// uint32_t erase_page_size;
			0,						// uint32_t write_page_size;
			0,						// uint32_t read_page_size;
			&ee24cxx_drv			// const struct mal_driver_t *driver;
		},							// struct mal_info_t ee24cxx_mal_info;
		{
			NULL, NULL, NULL, NULL
		},							// struct dal_info_t ee24cxx_handle;
	},								// struct ee24cxx_t
};

vsf_err_t vsfmem_init(struct vsfmem_info_t *vsfmem)
{
	if (vsfmem != NULL)
	{
		*vsfmem = vsfmem_template;
		
		// CFI
		vsfmem->cfi.cfi_handle.ifs = &vsfmem->cfi.cfi_drv_ifs;
		vsfmem->cfi.cfi_handle.param = &vsfmem->cfi.cfi_drv_param;
		vsfmem->cfi.cfi_handle.info = &vsfmem->cfi.cfi_drv_info;
		vsfmem->cfi.cfi_handle.extra = &vsfmem->cfi.cfi_mal_info;
		
		// Nand
		vsfmem->nand.nand_handle.ifs = &vsfmem->nand.nand_drv_ifs;
		vsfmem->nand.nand_handle.param = &vsfmem->nand.nand_drv_param;
		vsfmem->nand.nand_handle.info = &vsfmem->nand.nand_drv_info;
		vsfmem->nand.nand_handle.extra = &vsfmem->nand.nand_mal_info;
		
		// SD
		vsfmem->sd.sd_handle.ifs = &vsfmem->sd.sd_spi_drv_ifs;
		vsfmem->sd.sd_handle.param = &vsfmem->sd.sd_param;
		vsfmem->sd.sd_handle.info = &vsfmem->sd.sd_spi_drv_info;
		vsfmem->sd.sd_mal_info.extra = &vsfmem->sd.sd_info;
		vsfmem->sd.sd_handle.extra = &vsfmem->sd.sd_mal_info;
		
		// Dataflash
		vsfmem->df25xx.df25xx_handle.ifs = &vsfmem->df25xx.df25xx_drv_ifs;
		vsfmem->df25xx.df25xx_handle.param = &vsfmem->df25xx.df25xx_drv_param;
		vsfmem->df25xx.df25xx_handle.info = &vsfmem->df25xx.df25xx_drv_info;
		vsfmem->df25xx.df25xx_handle.extra = &vsfmem->df25xx.df25xx_mal_info;
		
		// EEPROM
		vsfmem->ee24cxx.ee24cxx_handle.ifs = &vsfmem->ee24cxx.ee24cxx_drv_ifs;
		vsfmem->ee24cxx.ee24cxx_handle.param = &vsfmem->ee24cxx.ee24cxx_drv_param;
		vsfmem->ee24cxx.ee24cxx_handle.extra = &vsfmem->ee24cxx.ee24cxx_mal_info;
	}
	return VSFERR_NONE;
}

vsf_err_t vsfmem_config(struct vsfmem_info_t *vsfmem, uint8_t cfi_port,
					uint8_t nand_port, uint8_t sd_spi_port, uint8_t df_spi_port,
					uint8_t cle, uint8_t ale, uint8_t eeprom_addr)
{
	if (vsfmem != NULL)
	{
		vsfmem->cfi.cfi_drv_ifs.nor_index = cfi_port;
		vsfmem->nand.nand_drv_ifs.nand_index = nand_port;
		vsfmem->sd.sd_spi_drv_ifs.spi_port = sd_spi_port;
		vsfmem->df25xx.df25xx_drv_ifs.spi_port = df_spi_port;
		
		vsfmem->nand.nand_drv_param.nand_info.param.addr.cmd = 1UL << cle;
		vsfmem->nand.nand_drv_param.nand_info.param.addr.addr = 1UL << ale;
		vsfmem->nand.nand_drv_param.nand_info.param.addr.data = 0;
		
		vsfmem->ee24cxx.ee24cxx_drv_param.iic_addr =
										((eeprom_addr & 0x03) << 1) | 0xA0;
	}
	
	return VSFERR_NONE;
}

