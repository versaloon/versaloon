vsf_err_t stm32_ebi_init(uint8_t index);
vsf_err_t stm32_ebi_fini(uint8_t index);
vsf_err_t stm32_ebi_config_sram(uint8_t index,
								struct ebi_sram_psram_nor_info_t *info);
vsf_err_t stm32_ebi_config_psram(uint8_t index,
								struct ebi_sram_psram_nor_info_t *info);
vsf_err_t stm32_ebi_config_nor(uint8_t index,
								struct ebi_sram_psram_nor_info_t *info);
vsf_err_t stm32_ebi_config_nand(uint8_t index, struct ebi_nand_info_t *info);
vsf_err_t stm32_ebi_config_sdram(uint8_t index, struct ebi_sdram_info_t *info);
vsf_err_t stm32_ebi_config_ddram(uint8_t index, struct ebi_ddram_info_t *info);
vsf_err_t stm32_ebi_config_pccard(uint8_t index, struct ebi_pccard_info_t *info);

void* stm32_ebi_get_base_addr(uint8_t index, uint8_t target_index);
vsf_err_t stm32_ebi_isready(uint8_t index, uint8_t target_index);

uint8_t stm32_ebi_read8(uint8_t index, uint32_t address);
void stm32_ebi_write8(uint8_t index, uint32_t address, uint8_t data);
uint16_t stm32_ebi_read16(uint8_t index, uint32_t address);
void stm32_ebi_write16(uint8_t index, uint32_t address, uint16_t data);
uint32_t stm32_ebi_read32(uint8_t index, uint32_t address);
void stm32_ebi_write32(uint8_t index, uint32_t address, uint32_t data);

vsf_err_t stm32_ebi_readp8(uint8_t index, uint32_t address, uint32_t count, 
							uint8_t *buff);
vsf_err_t stm32_ebi_readp8_isready(uint8_t index);
vsf_err_t stm32_ebi_writep8(uint8_t index, uint32_t address, uint32_t count, 
							uint8_t *buff);
vsf_err_t stm32_ebi_writep8_isready(uint8_t index);
vsf_err_t stm32_ebi_readp16(uint8_t index, uint32_t address, uint32_t count, 
							uint16_t *buff);
vsf_err_t stm32_ebi_readp16_isready(uint8_t index);
vsf_err_t stm32_ebi_writep16(uint8_t index, uint32_t address, uint32_t count, 
								uint16_t *buff);
vsf_err_t stm32_ebi_writep16_isready(uint8_t index);
vsf_err_t stm32_ebi_readp32(uint8_t index, uint32_t address, uint32_t count, 
							uint32_t *buff);
vsf_err_t stm32_ebi_readp32_isready(uint8_t index);
vsf_err_t stm32_ebi_writep32(uint8_t index, uint32_t address, uint32_t count, 
								uint32_t *buff);
vsf_err_t stm32_ebi_writep32_isready(uint8_t index);
