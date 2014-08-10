vsf_err_t stm32_flash_init(uint8_t index);
vsf_err_t stm32_flash_fini(uint8_t index);

vsf_err_t stm32_flash_lock(uint8_t index);
vsf_err_t stm32_flash_unlock(uint8_t index);

vsf_err_t stm32_flash_getcapacity(uint8_t index, uint32_t *pagesize, 
									uint32_t *pagenum);

vsf_err_t stm32_flash_read(uint8_t index, uint32_t offset, uint8_t *buff, 
							uint32_t size);
vsf_err_t stm32_flash_read_isready(uint8_t index, uint32_t offset,
									uint8_t *buff, uint32_t size);
vsf_err_t stm32_flash_write(uint8_t index, uint32_t offset, uint8_t *buff, 
							uint32_t size);
vsf_err_t stm32_flash_write_isready(uint8_t index, uint32_t offset,
									uint8_t *buff, uint32_t size);

vsf_err_t stm32_flash_readpage(uint8_t index, uint32_t offset, uint8_t *buff);
vsf_err_t stm32_flash_readpage_isready(uint8_t index, uint32_t offset, 
										uint8_t *buff);
vsf_err_t stm32_flash_writepage(uint8_t index, uint32_t offset, uint8_t *buff);
vsf_err_t stm32_flash_writepage_isready(uint8_t index, uint32_t offset, 
										uint8_t *buff);
vsf_err_t stm32_flash_erasepage(uint8_t index, uint32_t offset);
vsf_err_t stm32_flash_erasepage_isready(uint8_t index, uint32_t offset);
vsf_err_t stm32_flash_protect(uint8_t index);
bool stm32_flash_isprotected(uint8_t index);
