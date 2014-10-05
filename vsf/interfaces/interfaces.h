/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       interfaces.h                                              *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    interfaces header file                                    *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#ifndef __INTERFACES_H_INCLUDED__
#define __INTERFACES_H_INCLUDED__

#include "app_type.h"
#include "interfaces_cfg.h"
#include "interfaces_const.h"

#define IFS_DUMMY_PORT					0xFF

#define CORE_SLEEP_WFI(m)			__CONNECT(m, _SLEEP_WFI)
#define CORE_SLEEP_PWRDOWN(m)		__CONNECT(m, _SLEEP_PWRDOWN)
#define SLEEP_WFI					CORE_SLEEP_WFI(__TARGET_CHIP__)
#define SLEEP_PWRDOWN				CORE_SLEEP_PWRDOWN(__TARGET_CHIP__)

struct interface_core_t
{
	vsf_err_t (*init)(void *p);
	vsf_err_t (*fini)(void *p);
	vsf_err_t (*reset)(void *p);
	uint32_t (*get_stack)(void);
	vsf_err_t (*set_stack)(uint32_t sp);
	void (*sleep)(uint32_t mode);
};

#define CORE_INIT(m)					__CONNECT(m, _interface_init)
#define CORE_FINI(m)					__CONNECT(m, _interface_fini)
#define CORE_RESET(m)					__CONNECT(m, _interface_reset)
#define CORE_GET_STACK(m)				__CONNECT(m, _interface_get_stack)
#define CORE_SET_STACK(m)				__CONNECT(m, _interface_set_stack)
#define CORE_SLEEP(m)					__CONNECT(m, _interface_sleep)

vsf_err_t CORE_INIT(__TARGET_CHIP__)(void *p);
vsf_err_t CORE_FINI(__TARGET_CHIP__)(void *p);
vsf_err_t CORE_RESET(__TARGET_CHIP__)(void *p);
uint32_t CORE_GET_STACK(__TARGET_CHIP__)(void);
vsf_err_t CORE_SET_STACK(__TARGET_CHIP__)(uint32_t sp);
void CORE_SLEEP(__TARGET_CHIP__)(uint32_t mode);

#if IFS_UNIQUEID_EN

struct interface_uid_t
{
	uint32_t (*get)(uint8_t *buffer, uint32_t size);
};

#define CORE_UID_GET(m)					__CONNECT(m, _uid_get)

uint32_t CORE_UID_GET(__TARGET_CHIP__)(uint8_t *buffer, uint32_t size);

#endif

#if IFS_FLASH_EN

struct interface_flash_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	
	vsf_err_t (*lock)(uint8_t index);
	vsf_err_t (*unlock)(uint8_t index);
	
	vsf_err_t (*getcapacity)(uint8_t index, uint32_t *pagesize, 
								uint32_t *pagenum);
	
	vsf_err_t (*read)(uint8_t index, uint32_t offset, uint8_t *buff, 
					uint32_t size);
	vsf_err_t (*read_isready)(uint8_t index, uint32_t offset, uint8_t *buff, 
					uint32_t size);
	vsf_err_t (*write)(uint8_t index, uint32_t offset, uint8_t *buff, 
					uint32_t size);
	vsf_err_t (*write_isready)(uint8_t index, uint32_t offset, uint8_t *buff, 
					uint32_t size);
	
	vsf_err_t (*readpage)(uint8_t index, uint32_t offset, uint8_t *buff);
	vsf_err_t (*readpage_isready)(uint8_t index, uint32_t offset, 
									uint8_t *buff);
	vsf_err_t (*erasepage)(uint8_t index, uint32_t offset);
	vsf_err_t (*erasepage_isready)(uint8_t index, uint32_t offset);
	vsf_err_t (*writepage)(uint8_t index, uint32_t offset, uint8_t *buff);
	vsf_err_t (*writepage_isready)(uint8_t index, uint32_t offset, 
									uint8_t *buff);
	
	bool (*isprotected)(uint8_t index);
	vsf_err_t (*protect)(uint8_t index);
};

#define CORE_FLASH_INIT(m)				__CONNECT(m, _flash_init)
#define CORE_FLASH_FINI(m)				__CONNECT(m, _flash_fini)
#define CORE_FLASH_LOCK(m)				__CONNECT(m, _flash_lock)
#define CORE_FLASH_UNLOCK(m)			__CONNECT(m, _flash_unlock)
#define CORE_FLASH_GETCAPACITY(m)		__CONNECT(m, _flash_getcapacity)
#define CORE_FLASH_READ(m)				__CONNECT(m, _flash_read)
#define CORE_FLASH_READ_ISREADY(m)		__CONNECT(m, _flash_read_isready)
#define CORE_FLASH_WRITE(m)				__CONNECT(m, _flash_write)
#define CORE_FLASH_WRITE_ISREADY(m)		__CONNECT(m, _flash_write_isready)
#define CORE_FLASH_READPAGE(m)			__CONNECT(m, _flash_readpage)
#define CORE_FLASH_READPAGE_ISREADY(m)	__CONNECT(m, _flash_readpage_isready)
#define CORE_FLASH_ERASEPAGE(m)			__CONNECT(m, _flash_erasepage)
#define CORE_FLASH_ERASEPAGE_ISREADY(m)	__CONNECT(m, _flash_erasepage_isready)
#define CORE_FLASH_WRITEPAGE(m)			__CONNECT(m, _flash_writepage)
#define CORE_FLASH_WRITEPAGE_ISREADY(m)	__CONNECT(m, _flash_writepage_isready)
#define CORE_FLASH_ISPROTECTED(m)		__CONNECT(m, _flash_isprotected)
#define CORE_FLASH_PROTECT(m)			__CONNECT(m, _flash_protect)

vsf_err_t CORE_FLASH_INIT(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_FLASH_FINI(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_FLASH_LOCK(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_FLASH_UNLOCK(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_FLASH_GETCAPACITY(__TARGET_CHIP__)(uint8_t index, 
									uint32_t *pagesize, uint32_t *pagenum);
vsf_err_t CORE_FLASH_READ(__TARGET_CHIP__)(uint8_t index, uint32_t offset, 
										uint8_t *buff, uint32_t size);
vsf_err_t CORE_FLASH_READ_ISREADY(__TARGET_CHIP__)(uint8_t index, 
								uint32_t offset, uint8_t *buff, uint32_t size);
vsf_err_t CORE_FLASH_WRITE(__TARGET_CHIP__)(uint8_t index, uint32_t offset, 
										uint8_t *buff, uint32_t size);
vsf_err_t CORE_FLASH_WRITE_ISREADY(__TARGET_CHIP__)(uint8_t index, 
								uint32_t offset, uint8_t *buff, uint32_t size);
vsf_err_t CORE_FLASH_READPAGE(__TARGET_CHIP__)(uint8_t index, uint32_t offset, 
								uint8_t *buff);
vsf_err_t CORE_FLASH_READPAGE_ISREADY(__TARGET_CHIP__)(uint8_t index, 
												uint32_t offset, uint8_t *buff);
vsf_err_t CORE_FLASH_ERASEPAGE(__TARGET_CHIP__)(uint8_t index, uint32_t offset);
vsf_err_t CORE_FLASH_ERASEPAGE_ISREADY(__TARGET_CHIP__)(uint8_t index, 
														uint32_t offset);
vsf_err_t CORE_FLASH_WRITEPAGE(__TARGET_CHIP__)(uint8_t index, uint32_t offset, 
								uint8_t *buff);
vsf_err_t CORE_FLASH_WRITEPAGE_ISREADY(__TARGET_CHIP__)(uint8_t index, 
												uint32_t offset, uint8_t *buff);
vsf_err_t CORE_FLASH_PROTECT(__TARGET_CHIP__)(uint8_t index);
bool CORE_FLASH_ISPROTECTED(__TARGET_CHIP__)(uint8_t index);

#endif

#if IFS_CLKO_EN

struct interface_clko_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint32_t kHz);
	vsf_err_t (*enable)(uint8_t index);
	vsf_err_t (*disable)(uint8_t index);
};

#define CORE_CLKO_INIT(m)				__CONNECT(m, _clko_init)
#define CORE_CLKO_FINI(m)				__CONNECT(m, _clko_fini)
#define CORE_CLKO_CONFIG(m)				__CONNECT(m, _clko_config)
#define CORE_CLKO_ENABLE(m)				__CONNECT(m, _clko_enable)
#define CORE_CLKO_DISABLE(m)			__CONNECT(m, _clko_disable)

vsf_err_t CORE_CLKO_INIT(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_CLKO_FINI(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_CLKO_CONFIG(__TARGET_CHIP__)(uint8_t index, uint32_t kHz);
vsf_err_t CORE_CLKO_ENABLE(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_CLKO_DISABLE(__TARGET_CHIP__)(uint8_t index);

#endif

#if IFS_USART_EN

#define CORE_USART_MODE0(m)			__CONNECT(m, _USART_MODE0)
#define CORE_USART_MODE1(m)			__CONNECT(m, _USART_MODE1)
#define CORE_USART_MODE2(m)			__CONNECT(m, _USART_MODE2)
#define CORE_USART_MODE3(m)			__CONNECT(m, _USART_MODE3)
#define CORE_USART_CLKEN(m)			__CONNECT(m, _USART_CLKEN)
#define CORE_USART_STOPBITS_0P5(m)	__CONNECT(m, _USART_STOPBITS_0P5)
#define CORE_USART_STOPBITS_1(m)	__CONNECT(m, _USART_STOPBITS_1)
#define CORE_USART_STOPBITS_1P5(m)	__CONNECT(m, _USART_STOPBITS_1P5)
#define CORE_USART_STOPBITS_2(m)	__CONNECT(m, _USART_STOPBITS_2)
#define CORE_USART_PARITY_NONE(m)	__CONNECT(m, _USART_PARITY_NONE)
#define CORE_USART_PARITY_ODD(m)	__CONNECT(m, _USART_PARITY_ODD)
#define CORE_USART_PARITY_EVEN(m)	__CONNECT(m, _USART_PARITY_EVEN)
#define USART_MODE0					CORE_USART_MODE0(__TARGET_CHIP__)
#define USART_MODE1					CORE_USART_MODE1(__TARGET_CHIP__)
#define USART_MODE2					CORE_USART_MODE2(__TARGET_CHIP__)
#define USART_MODE3					CORE_USART_MODE3(__TARGET_CHIP__)
#define USART_CLKEN					CORE_USART_CLKEN(__TARGET_CHIP__)
#define USART_STOPBITS_1			CORE_USART_STOPBITS_1(__TARGET_CHIP__)
#define USART_STOPBITS_1P5			CORE_USART_STOPBITS_1P5(__TARGET_CHIP__)
#define USART_STOPBITS_2			CORE_USART_STOPBITS_2(__TARGET_CHIP__)
#define USART_PARITY_NONE			CORE_USART_PARITY_NONE(__TARGET_CHIP__)
#define USART_PARITY_ODD			CORE_USART_PARITY_ODD(__TARGET_CHIP__)
#define USART_PARITY_EVEN			CORE_USART_PARITY_EVEN(__TARGET_CHIP__)
struct interface_usart_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint32_t baudrate, uint8_t datalength, 
						uint8_t mode);
	vsf_err_t (*config_callback)(uint8_t index, uint32_t int_priority,
			void *p, void (*ontx)(void *), void (*onrx)(void *, uint16_t));
	vsf_err_t (*tx)(uint8_t index, uint16_t data);
	vsf_err_t (*tx_isready)(uint8_t index);
	uint16_t (*rx)(uint8_t index);
	vsf_err_t (*rx_isready)(uint8_t index);
	
//	vsf_err_t (*dma_tx_start)(uint8_t index, void *data, uint32_t len);
//	vsf_err_t (*dma_tx_isready)(uint8_t index);
//	vsf_err_t (*dma_tx_end)(uint8_t index);
//	vsf_err_t (*dma_rx_start)(uint8_t index, void *data, uint32_t len);
//	vsf_err_t (*dma_rx_isready)(uint8_t index);
//	vsf_err_t (*dma_rx_end)(uint8_t index);
};

#define CORE_USART_INIT(m)				__CONNECT(m, _usart_init)
#define CORE_USART_FINI(m)				__CONNECT(m, _usart_fini)
#define CORE_USART_CONFIG(m)			__CONNECT(m, _usart_config)
#define CORE_USART_CONFIG_CALLBACK(m)	__CONNECT(m, _usart_config_callback)
#define CORE_USART_RX(m)				__CONNECT(m, _usart_rx)
#define CORE_USART_RX_ISREADY(m)		__CONNECT(m, _usart_rx_isready)
#define CORE_USART_TX(m)				__CONNECT(m, _usart_tx)
#define CORE_USART_TX_ISREADY(m)		__CONNECT(m, _usart_tx_isready)

vsf_err_t CORE_USART_INIT(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_USART_FINI(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_USART_CONFIG(__TARGET_CHIP__)(uint8_t index, uint32_t baudrate, 
	uint8_t datalength, uint8_t mode);
vsf_err_t CORE_USART_CONFIG_CALLBACK(__TARGET_CHIP__)(uint8_t index,
	uint32_t int_priority, void *p, void (*ontx)(void *),
	void (*onrx)(void *, uint16_t));
vsf_err_t CORE_USART_TX(__TARGET_CHIP__)(uint8_t index, uint16_t data);
vsf_err_t CORE_USART_TX_ISREADY(__TARGET_CHIP__)(uint8_t index);
uint16_t CORE_USART_RX(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_USART_RX_ISREADY(__TARGET_CHIP__)(uint8_t index);

#endif

#if IFS_SPI_EN

#define CORE_SPI_MASTER(m)			__CONNECT(m, _SPI_MASTER)
#define CORE_SPI_SLAVE(m)			__CONNECT(m, _SPI_SLAVE)
#define CORE_SPI_MODE0(m)			__CONNECT(m, _SPI_MODE0)
#define CORE_SPI_MODE1(m)			__CONNECT(m, _SPI_MODE1)
#define CORE_SPI_MODE2(m)			__CONNECT(m, _SPI_MODE2)
#define CORE_SPI_MODE3(m)			__CONNECT(m, _SPI_MODE3)
#define CORE_SPI_MSB_FIRST(m)		__CONNECT(m, _SPI_MSB_FIRST)
#define CORE_SPI_LSB_FIRST(m)		__CONNECT(m, _SPI_LSB_FIRST)
#define SPI_MASTER					CORE_SPI_MASTER(__TARGET_CHIP__)
#define SPI_SLAVE					CORE_SPI_SLAVE(__TARGET_CHIP__)
#define SPI_MODE0					CORE_SPI_MODE0(__TARGET_CHIP__)
#define SPI_MODE1					CORE_SPI_MODE1(__TARGET_CHIP__)
#define SPI_MODE2					CORE_SPI_MODE2(__TARGET_CHIP__)
#define SPI_MODE3					CORE_SPI_MODE3(__TARGET_CHIP__)
#define SPI_MSB_FIRST				CORE_SPI_MSB_FIRST(__TARGET_CHIP__)
#define SPI_LSB_FIRST				CORE_SPI_LSB_FIRST(__TARGET_CHIP__)
struct spi_ability_t
{
	uint32_t max_freq_hz;
	uint32_t min_freq_hz;
};
struct interface_spi_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*get_ability)(uint8_t index, struct spi_ability_t *ability);
	vsf_err_t (*enable)(uint8_t index);
	vsf_err_t (*disable)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint32_t kHz, uint8_t mode);
	
	vsf_err_t (*select)(uint8_t index, uint8_t cs);
	vsf_err_t (*deselect)(uint8_t index, uint8_t cs);
	
	vsf_err_t (*io_tx)(uint8_t index, uint8_t out);
	vsf_err_t (*io_tx_isready)(uint8_t index);
	uint8_t (*io_rx)(uint8_t index);
	vsf_err_t (*io_rx_isready)(uint8_t index);
	
	vsf_err_t (*io)(uint8_t index, uint8_t *out, uint8_t *in, uint32_t len);
	
	vsf_err_t (*io_dma_start)(uint8_t index, uint8_t *out, uint8_t *in, 
								uint32_t len);
	vsf_err_t (*io_dma_isready)(uint8_t index);
	vsf_err_t (*io_dma_end)(uint8_t index);
};

#define CORE_SPI_INIT(m)				__CONNECT(m, _spi_init)
#define CORE_SPI_FINI(m)				__CONNECT(m, _spi_fini)
#define CORE_SPI_GET_ABILITY(m)			__CONNECT(m, _spi_get_ability)
#define CORE_SPI_ENABLE(m)				__CONNECT(m, _spi_enable)
#define CORE_SPI_DISABLE(m)				__CONNECT(m, _spi_disable)
#define CORE_SPI_CONFIG(m)				__CONNECT(m, _spi_config)
#define CORE_SPI_SELECT(m)				__CONNECT(m, _spi_select)
#define CORE_SPI_DESELECT(m)			__CONNECT(m, _spi_deselect)
#define CORE_SPI_IO_TX(m)				__CONNECT(m, _spi_io_tx)
#define CORE_SPI_IO_TX_ISREADY(m)		__CONNECT(m, _spi_io_tx_isready)
#define CORE_SPI_IO_RX(m)				__CONNECT(m, _spi_io_rx)
#define CORE_SPI_IO_RX_ISREADY(m)		__CONNECT(m, _spi_io_rx_isready)
#define CORE_SPI_IO(m)					__CONNECT(m, _spi_io)
#define CORE_SPI_IO_DMA_START(m)		__CONNECT(m, _spi_io_dma_start)
#define CORE_SPI_IO_DMA_ISREADY(m)		__CONNECT(m, _spi_io_dma_isready)
#define CORE_SPI_IO_DMA_END(m)			__CONNECT(m, _spi_io_dma_end)

vsf_err_t CORE_SPI_INIT(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_SPI_FINI(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_SPI_GET_ABILITY(__TARGET_CHIP__)(uint8_t index, 
												struct spi_ability_t *ability);
vsf_err_t CORE_SPI_ENABLE(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_SPI_DISABLE(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_SPI_CONFIG(__TARGET_CHIP__)(uint8_t index, uint32_t kHz, 
										uint8_t mode);
vsf_err_t CORE_SPI_SELECT(__TARGET_CHIP__)(uint8_t index, uint8_t cs);
vsf_err_t CORE_SPI_DESELECT(__TARGET_CHIP__)(uint8_t index, uint8_t cs);
vsf_err_t CORE_SPI_IO_TX(__TARGET_CHIP__)(uint8_t index, uint8_t out);
vsf_err_t CORE_SPI_IO_TX_ISREADY(__TARGET_CHIP__)(uint8_t index);
uint8_t CORE_SPI_IO_RX(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_SPI_IO_RX_ISREADY(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_SPI_IO(__TARGET_CHIP__)(uint8_t index, uint8_t *out, uint8_t *in, 
									uint32_t len);
vsf_err_t CORE_SPI_IO_DMA_START(__TARGET_CHIP__)(uint8_t index, uint8_t *out, 
												uint8_t *in, uint32_t len);
vsf_err_t CORE_SPI_IO_DMA_ISREADY(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_SPI_IO_DMA_END(__TARGET_CHIP__)(uint8_t index);

#endif

#if IFS_ADC_EN

#define CORE_ADC_ALIGNLEFT(m)		__CONNECT(m, _ADC_ALIGNLEFT)
#define CORE_ADC_ALIGNRIGHT(m)		__CONNECT(m, _ADC_ALIGNRIGHT)
#define ADC_ALIGNLEFT				CORE_ADC_ALIGNLEFT(__TARGET_CHIP__)
#define ADC_ALIGNRIGHT				CORE_ADC_ALIGNRIGHT(__TARGET_CHIP__)
struct interface_adc_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint32_t clock_hz, uint8_t mode);
	vsf_err_t (*config_channel)(uint8_t index, uint8_t channel, uint8_t cycles);
	uint32_t (*get_max_value)(uint8_t index);
	vsf_err_t (*calibrate)(uint8_t index, uint8_t channel);
	vsf_err_t (*start)(uint8_t index, uint8_t channel);
	vsf_err_t (*isready)(uint8_t index, uint8_t channel);
	uint32_t (*get)(uint8_t index, uint8_t channel);
};

#define CORE_ADC_INIT(m)				__CONNECT(m, _adc_init)
#define CORE_ADC_FINI(m)				__CONNECT(m, _adc_fini)
#define CORE_ADC_CONFIG(m)				__CONNECT(m, _adc_config)
#define CORE_ADC_CONFIG_CHANNEL(m)		__CONNECT(m, _adc_config_channel)
#define CORE_ADC_CALIBRATE(m)			__CONNECT(m, _adc_calibrate)
#define CORE_ADC_GET_MAX_VALUE(m)		__CONNECT(m, _adc_get_max_value)
#define CORE_ADC_START(m)				__CONNECT(m, _adc_start)
#define CORE_ADC_ISREADY(m)				__CONNECT(m, _adc_isready)
#define CORE_ADC_GET(m)					__CONNECT(m, _adc_get)

vsf_err_t CORE_ADC_INIT(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_ADC_FINI(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_ADC_CONFIG(__TARGET_CHIP__)(uint8_t index, uint32_t clock_hz, 
										uint8_t mode);
vsf_err_t CORE_ADC_CONFIG_CHANNEL(__TARGET_CHIP__)(uint8_t index, 
											uint8_t channel, uint8_t cycles);
vsf_err_t CORE_ADC_CALIBRATE(__TARGET_CHIP__)(uint8_t index, uint8_t channel);
uint32_t CORE_ADC_GET_MAX_VALUE(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_ADC_START(__TARGET_CHIP__)(uint8_t index, uint8_t channel);
vsf_err_t CORE_ADC_ISREADY(__TARGET_CHIP__)(uint8_t index, uint8_t channel);
uint32_t CORE_ADC_GET(__TARGET_CHIP__)(uint8_t index, uint8_t channel);

#endif

#if IFS_GPIO_EN

#define CORE_GPIO_INFLOAT(m)		__CONNECT(m, _GPIO_INFLOAT)
#define CORE_GPIO_INPU(m)			__CONNECT(m, _GPIO_INPU)
#define CORE_GPIO_INPD(m)			__CONNECT(m, _GPIO_INPD)
#define CORE_GPIO_OUTPP(m)			__CONNECT(m, _GPIO_OUTPP)
#define CORE_GPIO_OUTOD(m)			__CONNECT(m, _GPIO_OUTOD)
#define GPIO_INFLOAT				CORE_GPIO_INFLOAT(__TARGET_CHIP__)
#define GPIO_INPU					CORE_GPIO_INPU(__TARGET_CHIP__)
#define GPIO_INPD					CORE_GPIO_INPD(__TARGET_CHIP__)
#define GPIO_OUTPP					CORE_GPIO_OUTPP(__TARGET_CHIP__)
#define GPIO_OUTOD					CORE_GPIO_OUTOD(__TARGET_CHIP__)
struct interface_gpio_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config_pin)(uint8_t index, uint8_t pin_idx, uint8_t mode);
	vsf_err_t (*config)(uint8_t index, uint32_t pin_mask, uint32_t io, 
						uint32_t pull_en_mask, uint32_t input_pull_mask);
	vsf_err_t (*set)(uint8_t index, uint32_t pin_mask);
	vsf_err_t (*clear)(uint8_t index, uint32_t pin_mask);
	vsf_err_t (*out)(uint8_t index, uint32_t pin_mask, uint32_t value);
	vsf_err_t (*in)(uint8_t index, uint32_t pin_mask, uint32_t *value);
	uint32_t (*get)(uint8_t index, uint32_t pin_mask);
};

struct interface_gpio_pin_t
{
	uint8_t port;
	uint32_t pin;
};

#define CORE_GPIO_INIT(m)				__CONNECT(m, _gpio_init)
#define CORE_GPIO_FINI(m)				__CONNECT(m, _gpio_fini)
#define CORE_GPIO_CONFIG_PIN(m)			__CONNECT(m, _gpio_config_pin)
#define CORE_GPIO_CONFIG(m)				__CONNECT(m, _gpio_config)
#define CORE_GPIO_IN(m)					__CONNECT(m, _gpio_in)
#define CORE_GPIO_OUT(m)				__CONNECT(m, _gpio_out)
#define CORE_GPIO_SET(m)				__CONNECT(m, _gpio_set)
#define CORE_GPIO_CLEAR(m)				__CONNECT(m, _gpio_clear)
#define CORE_GPIO_GET(m)				__CONNECT(m, _gpio_get)

vsf_err_t CORE_GPIO_INIT(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_GPIO_FINI(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_GPIO_CONFIG_PIN(__TARGET_CHIP__)(uint8_t index, uint8_t pin_idx, 
												uint8_t mode);
vsf_err_t CORE_GPIO_CONFIG(__TARGET_CHIP__)(uint8_t index, uint32_t pin_mask, 
		uint32_t io, uint32_t pull_en_mask, uint32_t input_pull_mask);
vsf_err_t CORE_GPIO_IN(__TARGET_CHIP__)(uint8_t index, uint32_t pin_mask, 
		uint32_t *value);
vsf_err_t CORE_GPIO_OUT(__TARGET_CHIP__)(uint8_t index, uint32_t pin_mask, 
		uint32_t value);
vsf_err_t CORE_GPIO_SET(__TARGET_CHIP__)(uint8_t index, uint32_t pin_mask);
vsf_err_t CORE_GPIO_CLEAR(__TARGET_CHIP__)(uint8_t index, uint32_t pin_mask);
uint32_t CORE_GPIO_GET(__TARGET_CHIP__)(uint8_t index, uint32_t pin_mask);

#endif

struct interface_delay_t
{
	vsf_err_t (*init)(void);
	vsf_err_t (*delayms)(uint16_t ms);
	vsf_err_t (*delayus)(uint16_t us);
};

#define CORE_DELAY_INIT(m)				__CONNECT(m, _delay_init)
#define CORE_DELAY_DELAYMS(m)			__CONNECT(m, _delay_delayms)
#define CORE_DELAY_DELAYUS(m)			__CONNECT(m, _delay_delayus)

vsf_err_t CORE_DELAY_INIT(__TARGET_CHIP__)(void);
vsf_err_t CORE_DELAY_DELAYMS(__TARGET_CHIP__)(uint16_t ms);
vsf_err_t CORE_DELAY_DELAYUS(__TARGET_CHIP__)(uint16_t us);

struct interface_tickclk_t
{
	vsf_err_t (*init)(void);
	vsf_err_t (*fini)(void);
	vsf_err_t (*start)(void);
	vsf_err_t (*stop)(void);
	uint32_t (*get_count)(void);
	vsf_err_t (*set_callback)(void (*callback)(void *param), void *param);
};

#define CORE_TICKCLK_INIT(m)			__CONNECT(m, _tickclk_init)
#define CORE_TICKCLK_FINI(m)			__CONNECT(m, _tickclk_fini)
#define CORE_TICKCLK_START(m)			__CONNECT(m, _tickclk_start)
#define CORE_TICKCLK_STOP(m)			__CONNECT(m, _tickclk_stop)
#define CORE_TICKCLK_GET_COUNT(m)		__CONNECT(m, _tickclk_get_count)
#define CORE_TICKCLK_SET_CALLBACK(m)	__CONNECT(m, _tickclk_set_callback)

vsf_err_t CORE_TICKCLK_INIT(__TARGET_CHIP__)(void);
vsf_err_t CORE_TICKCLK_FINI(__TARGET_CHIP__)(void);
vsf_err_t CORE_TICKCLK_START(__TARGET_CHIP__)(void);
vsf_err_t CORE_TICKCLK_STOP(__TARGET_CHIP__)(void);
uint32_t CORE_TICKCLK_GET_COUNT(__TARGET_CHIP__)(void);
vsf_err_t CORE_TICKCLK_SET_CALLBACK(__TARGET_CHIP__)(
					void (*callback)(void *param), void *param);

#if IFS_IIC_EN

struct interface_i2c_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint16_t kHz, uint16_t byte_interval, 
					 uint16_t max_dly);
	vsf_err_t (*read)(uint8_t index, uint16_t chip_addr, uint8_t *data, 
				   uint16_t data_len, uint8_t stop, bool nacklast);
	vsf_err_t (*write)(uint8_t index, uint16_t chip_addr, uint8_t *data, 
					uint16_t data_len, uint8_t stop);
};

#endif

#if IFS_PWM_EN

#define PWM_OUTPP				0x01
#define PWM_OUTPOLARITY			0x02

struct interface_pwm_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config_mode)(uint8_t index, uint8_t mode);
	vsf_err_t (*config_freq)(uint8_t index, uint16_t kHz);
	vsf_err_t (*out)(uint8_t index, uint16_t count, uint16_t *rate);
	vsf_err_t (*in)(uint8_t index, uint16_t count, uint16_t *rate);
};

#endif

#if IFS_MICROWIRE_EN

struct interface_microwire_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint16_t kHz, uint8_t sel_polarity);
	vsf_err_t (*transport)(uint8_t index, 
						uint32_t opcode, uint8_t opcode_bitlen, 
						uint32_t addr, uint8_t addr_bitlen, 
						uint32_t data, uint8_t data_bitlen, 
						uint8_t *reply, uint8_t reply_bitlen);
	vsf_err_t (*poll)(uint8_t index, uint16_t interval_us, uint16_t retry_cnt);
};

#endif

#if IFS_TIMER_EN

struct interface_timer_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint32_t kHz, uint32_t mode, 
						void (*overflow)(void));
	vsf_err_t (*start)(uint8_t index);
	vsf_err_t (*stop)(uint8_t index);
	vsf_err_t (*get_count)(uint8_t index, uint32_t *count);
	vsf_err_t (*set_count)(uint8_t index, uint32_t count);
	
	vsf_err_t (*config_channel)(uint8_t index, uint8_t channel, uint32_t mode, 
								void (*callback)(void));
	vsf_err_t (*get_channel)(uint8_t index, uint8_t channel, uint32_t *count);
	vsf_err_t (*set_channel)(uint8_t index, uint8_t channel, uint32_t count);
};

#define CORE_TIMER_INIT(m)				__CONNECT(m, _timer_init)
#define CORE_TIMER_FINI(m)				__CONNECT(m, _timer_fini)
#define CORE_TIMER_CONFIG(m)			__CONNECT(m, _timer_config)
#define CORE_TIMER_START(m)				__CONNECT(m, _timer_start)
#define CORE_TIMER_STOP(m)				__CONNECT(m, _timer_stop)
#define CORE_TIMER_GET_COUNT(m)			__CONNECT(m, _timer_get_count)
#define CORE_TIMER_SET_COUNT(m)			__CONNECT(m, _timer_set_count)
#define CORE_TIMER_CONFIG_CHANNEL(m)	__CONNECT(m, _timer_config_channel)
#define CORE_TIMER_GET_CHANNEL(m)		__CONNECT(m, _timer_get_channel)
#define CORE_TIMER_SET_CHANNEL(m)		__CONNECT(m, _timer_set_channel)

vsf_err_t CORE_TIMER_INIT(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_TIMER_FINI(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_TIMER_CONFIG(__TARGET_CHIP__)(uint8_t index, uint32_t kHz, 
										uint32_t mode, void (*overflow)(void));
vsf_err_t CORE_TIMER_START(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_TIMER_STOP(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_TIMER_GET_COUNT(__TARGET_CHIP__)(uint8_t index, uint32_t *count);
vsf_err_t CORE_TIMER_SET_COUNT(__TARGET_CHIP__)(uint8_t index, uint32_t count);
vsf_err_t CORE_TIMER_CONFIG_CHANNEL(__TARGET_CHIP__)(uint8_t index, 
						uint8_t channel, uint32_t mode, void (*callback)(void));
vsf_err_t CORE_TIMER_GET_CHANNEL(__TARGET_CHIP__)(uint8_t index, 
											uint8_t channel, uint32_t *count);
vsf_err_t CORE_TIMER_SET_CHANNEL(__TARGET_CHIP__)(uint8_t index, 
											uint8_t channel, uint32_t count);

#endif

#if IFS_EINT_EN

#define CORE_EINT_ONFALL(m)			__CONNECT(m, _EINT_ONFALL)
#define CORE_EINT_ONRISE(m)			__CONNECT(m, _EINT_ONRISE)
#define CORE_EINT_INT(m)			__CONNECT(m, _EINT_INT)
#define CORE_EINT_EVT(m)			__CONNECT(m, _EINT_EVT)
#define EINT_ONFALL					CORE_EINT_ONFALL(__TARGET_CHIP__)
#define EINT_ONRISE					CORE_EINT_ONRISE(__TARGET_CHIP__)
#define EINT_INT					CORE_EINT_INT(__TARGET_CHIP__)
#define EINT_EVT					CORE_EINT_EVT(__TARGET_CHIP__)
struct interface_eint_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint8_t type, void (*callback)(void));
	vsf_err_t (*enable)(uint8_t index);
	vsf_err_t (*disable)(uint8_t index);
	vsf_err_t (*trigger)(uint8_t index);
};

#define CORE_EINT_INIT(m)				__CONNECT(m, _eint_init)
#define CORE_EINT_FINI(m)				__CONNECT(m, _eint_fini)
#define CORE_EINT_CONFIG(m)				__CONNECT(m, _eint_config)
#define CORE_EINT_ENABLE(m)				__CONNECT(m, _eint_enable)
#define CORE_EINT_DISABLE(m)			__CONNECT(m, _eint_disable)
#define CORE_EINT_TRIGGER(m)			__CONNECT(m, _eint_trigger)

vsf_err_t CORE_EINT_INIT(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_EINT_FINI(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_EINT_CONFIG(__TARGET_CHIP__)(uint8_t index, uint8_t type, 
											void (*callback)(void));
vsf_err_t CORE_EINT_ENABLE(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_EINT_DISABLE(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_EINT_TRIGGER(__TARGET_CHIP__)(uint8_t index);

#endif

#if IFS_NAND_EN
struct nand_info_t
{
	uint32_t clock_hz;
	struct nand_ecc_t
	{
		bool ecc_enable;
		uint16_t ecc_page_size;
	} ecc;
	struct nand_timing_t
	{
		uint8_t ale_to_re_cycle;
		uint8_t cle_to_re_cycle;
		uint16_t setup_cycle;
		uint16_t wait_cycle;
		uint8_t hold_cycle;
		uint8_t hiz_cycle;
		uint16_t setup_cycle_attr;
		uint16_t wait_cycle_attr;
		uint8_t hold_cycle_attr;
		uint8_t hiz_cycle_attr;
	} timing;
};

struct interface_nand_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, struct nand_info_t *param);
	vsf_err_t (*write_cmd)(uint8_t index, uint8_t *cmd, uint8_t bytelen);
	vsf_err_t (*write_addr)(uint8_t index, uint8_t *addr, uint8_t bytelen);
	vsf_err_t (*write_data)(uint8_t index, uint8_t *data, uint16_t bytelen);
	vsf_err_t (*read_data)(uint8_t index, uint8_t *data, uint16_t bytelen);
};

#define CORE_NAND_INIT(m)				__CONNECT(m, _nand_init)
#define CORE_NAND_FINI(m)				__CONNECT(m, _nand_fini)
#define CORE_NAND_CONFIG(m)				__CONNECT(m, _nand_config)
#define CORE_NAND_WRITE_CMD(m)			__CONNECT(m, _nand_write_cmd)
#define CORE_NAND_WRITE_ADDR(m)			__CONNECT(m, _nand_write_addr)
#define CORE_NAND_WRITE_DATA(m)			__CONNECT(m, _nand_write_data)
#define CORE_NAND_READ_DATA(m)			__CONNECT(m, _nand_read_data)

vsf_err_t CORE_NAND_INIT(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_NAND_FINI(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_NAND_CONFIG(__TARGET_CHIP__)(uint8_t index,
												struct nand_info_t *param);
vsf_err_t CORE_NAND_WRITE_CMD(__TARGET_CHIP__)(uint8_t index, uint8_t *cmd,
												uint8_t bytelen);
vsf_err_t CORE_NAND_WRITE_ADDR(__TARGET_CHIP__)(uint8_t index, uint8_t *addr,
												uint8_t bytelen);
vsf_err_t CORE_NAND_WRITE_DATA(__TARGET_CHIP__)(uint8_t index, uint8_t *data,
												uint16_t bytelen);
vsf_err_t CORE_NAND_READ_DATA(__TARGET_CHIP__)(uint8_t index, uint8_t *data,
												uint16_t bytelen);

#endif

#if IFS_EBI_EN

#define EBI_TGTTYP_NOR				0x00
#define EBI_TGTTYP_SRAM				0x10
#define EBI_TGTTYP_PSRAM			0x20
#define EBI_TGTTYP_NAND				0x30
struct ebi_info_t
{
	uint8_t data_width;
	enum wait_signal_t
	{
		EBI_WAIT_NONE = 0,
		EBI_WAIT_POLHIGH_VI = 1,
		EBI_WAIT_POLHIGH_VN = 2,
		EBI_WAIT_POLLOW_VI = 3,
		EBI_WAIT_POLLOW_VN = 4
	} wait_signal;
	
	// mux_addr_mask is used when a multiplexer(eg. 74LS138) is used.
	// If no multiplexer is used, set mux_addr_mask to 0.
	// mux_addr_mask defines the address mask to select current chip.
	uint32_t mux_addr_mask;
};
struct ebi_sram_psram_nor_param_t
{
	// A0-15 == D0-15 with ALE
	bool addr_multiplex;
	
	struct ebi_sram_param_nor_timing_t
	{
		uint16_t address_setup_cycle_r;
		uint16_t address_hold_cycle_r;
		uint16_t data_setup_cycle_r;
		uint32_t clock_hz_r;
		uint16_t address_setup_cycle_w;
		uint16_t address_hold_cycle_w;
		uint16_t data_setup_cycle_w;
		uint32_t clock_hz_w;
	} timing;
};
struct ebi_sram_psram_nor_info_t
{
	struct ebi_info_t common_info;
	struct ebi_sram_psram_nor_param_t param;
};
struct ebi_nand_info_t
{
	struct ebi_info_t common_info;
	struct ebi_nand_param_t
	{
		uint32_t clock_hz;
		struct ebi_nand_ecc_t
		{
			bool ecc_enable;
			uint16_t ecc_page_size;
		} ecc;
		struct ebi_nand_timing_t
		{
			uint8_t ale_to_re_cycle;
			uint8_t cle_to_re_cycle;
			uint16_t setup_cycle;
			uint16_t wait_cycle;
			uint8_t hold_cycle;
			uint8_t hiz_cycle;
			uint16_t setup_cycle_attr;
			uint16_t wait_cycle_attr;
			uint8_t hold_cycle_attr;
			uint8_t hiz_cycle_attr;
		} timing;
		struct ebi_nand_addr_t
		{
			uint32_t cmd;
			uint32_t addr;
			uint32_t data;
		} addr;
	} param;
};
struct ebi_sdram_info_t
{
	struct ebi_info_t common_info;
};
struct ebi_ddram_info_t
{
	struct ebi_info_t common_info;
};
struct ebi_pccard_info_t
{
	struct ebi_info_t common_info;
};
struct interface_ebi_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	
	vsf_err_t (*config)(uint8_t index, uint8_t target_index, void *param);
	vsf_err_t (*config_sram)(uint8_t index, struct ebi_sram_psram_nor_info_t *info);
	vsf_err_t (*config_psram)(uint8_t index, struct ebi_sram_psram_nor_info_t *info);
	vsf_err_t (*config_nor)(uint8_t index, struct ebi_sram_psram_nor_info_t *info);
	vsf_err_t (*config_nand)(uint8_t index, struct ebi_nand_info_t *info);
	vsf_err_t (*config_sdram)(uint8_t index, struct ebi_sdram_info_t *info);
	vsf_err_t (*config_ddram)(uint8_t index, struct ebi_ddram_info_t *info);
	vsf_err_t (*config_pccard)(uint8_t index, struct ebi_pccard_info_t *info);
	
	void* (*get_base_addr)(uint8_t index, uint8_t target_index);
	vsf_err_t (*isready)(uint8_t index, uint8_t target_index);
	
	vsf_err_t (*read)(uint8_t index, uint8_t target_index, uint32_t address, 
					uint8_t data_size, uint8_t *buff, uint32_t count);
	vsf_err_t (*write)(uint8_t index, uint8_t target_index, uint32_t address, 
					uint8_t data_size, uint8_t *buff, uint32_t count);
	
	uint8_t (*read8)(uint8_t index, uint32_t address);
	void (*write8)(uint8_t index, uint32_t address, uint8_t data);
	uint16_t (*read16)(uint8_t index, uint32_t address);
	void (*write16)(uint8_t index, uint32_t address, uint16_t data);
	uint32_t (*read32)(uint8_t index, uint32_t address);
	void (*write32)(uint8_t index, uint32_t address, uint32_t data);
	
	vsf_err_t (*readp8)(uint8_t index, uint32_t address, uint32_t count, 
						uint8_t *buff);
	vsf_err_t (*readp8_isready)(uint8_t index);
	vsf_err_t (*writep8)(uint8_t index, uint32_t address, uint32_t count, 
						uint8_t *buff);
	vsf_err_t (*writep8_isready)(uint8_t index);
	vsf_err_t (*readp16)(uint8_t index, uint32_t address, uint32_t count, 
						uint16_t *buff);
	vsf_err_t (*readp16_isready)(uint8_t index);
	vsf_err_t (*writep16)(uint8_t index, uint32_t address, uint32_t count, 
						uint16_t *buff);
	vsf_err_t (*writep16_isready)(uint8_t index);
	vsf_err_t (*readp32)(uint8_t index, uint32_t address, uint32_t count, 
						uint32_t *buff);
	vsf_err_t (*readp32_isready)(uint8_t index);
	vsf_err_t (*writep32)(uint8_t index, uint32_t address, uint32_t count, 
						uint32_t *buff);
	vsf_err_t (*writep32_isready)(uint8_t index);
};

#define CORE_EBI_INIT(m)				__CONNECT(m, _ebi_init)
#define CORE_EBI_FINI(m)				__CONNECT(m, _ebi_fini)
#define CORE_EBI_CONFIG(m)				__CONNECT(m, _ebi_config)
#define CORE_EBI_CONFIG_SRAM(m)			__CONNECT(m, _ebi_config_sram)
#define CORE_EBI_CONFIG_PSRAM(m)		__CONNECT(m, _ebi_config_psram)
#define CORE_EBI_CONFIG_NOR(m)			__CONNECT(m, _ebi_config_nor)
#define CORE_EBI_CONFIG_NAND(m)			__CONNECT(m, _ebi_config_nand)
#define CORE_EBI_CONFIG_SDRAM(m)		__CONNECT(m, _ebi_config_sdram)
#define CORE_EBI_CONFIG_DDRAM(m)		__CONNECT(m, _ebi_config_ddram)
#define CORE_EBI_CONFIG_PCCARD(m)		__CONNECT(m, _ebi_config_pccard)
#define CORE_EBI_GET_BASE_ADDR(m)		__CONNECT(m, _ebi_get_base_addr)
#define CORE_EBI_ISREADY(m)				__CONNECT(m, _ebi_isready)
#define CORE_EBI_READ(m)				__CONNECT(m, _ebi_read)
#define CORE_EBI_WRITE(m)				__CONNECT(m, _ebi_write)
#define CORE_EBI_READ8(m)				__CONNECT(m, _ebi_read8)
#define CORE_EBI_WRITE8(m)				__CONNECT(m, _ebi_write8)
#define CORE_EBI_READ16(m)				__CONNECT(m, _ebi_read16)
#define CORE_EBI_WRITE16(m)				__CONNECT(m, _ebi_write16)
#define CORE_EBI_READ32(m)				__CONNECT(m, _ebi_read32)
#define CORE_EBI_WRITE32(m)				__CONNECT(m, _ebi_write32)

vsf_err_t CORE_EBI_INIT(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_EBI_FINI(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_EBI_CONFIG(__TARGET_CHIP__)(uint8_t index, uint8_t target_index, 
										void *param);
vsf_err_t CORE_EBI_CONFIG_SRAM(__TARGET_CHIP__)(uint8_t index, 
										struct ebi_sram_psram_nor_info_t *info);
vsf_err_t CORE_EBI_CONFIG_PSRAM(__TARGET_CHIP__)(uint8_t index, 
										struct ebi_sram_psram_nor_info_t *info);
vsf_err_t CORE_EBI_CONFIG_NOR(__TARGET_CHIP__)(uint8_t index, 
										struct ebi_sram_psram_nor_info_t *info);
vsf_err_t CORE_EBI_CONFIG_NAND(__TARGET_CHIP__)(uint8_t index, 
												struct ebi_nand_info_t *info);
vsf_err_t CORE_EBI_CONFIG_SDRAM(__TARGET_CHIP__)(uint8_t index, 
												struct ebi_sdram_info_t *info);
vsf_err_t CORE_EBI_CONFIG_DDRAM(__TARGET_CHIP__)(uint8_t index, 
												struct ebi_ddram_info_t *info);
vsf_err_t CORE_EBI_CONFIG_PCCARD(__TARGET_CHIP__)(uint8_t index, 
												struct ebi_pccard_info_t *info);
void* CORE_EBI_GET_BASE_ADDR(__TARGET_CHIP__)(uint8_t index,
												uint8_t target_index);
vsf_err_t CORE_EBI_ISREADY(__TARGET_CHIP__)(uint8_t index,
												uint8_t target_index);
vsf_err_t CORE_EBI_READ(__TARGET_CHIP__)(uint8_t index, uint8_t target_index, 
		uint32_t address, uint8_t data_size, uint8_t *buff, uint32_t count);
vsf_err_t CORE_EBI_WRITE(__TARGET_CHIP__)(uint8_t index, uint8_t target_index, 
		uint32_t address, uint8_t data_size, uint8_t *buff, uint32_t count);
uint8_t CORE_EBI_READ8(__TARGET_CHIP__)(uint8_t index, uint32_t address);
void CORE_EBI_WRITE8(__TARGET_CHIP__)(uint8_t index, uint32_t address, 
										uint8_t data);
uint16_t CORE_EBI_READ16(__TARGET_CHIP__)(uint8_t index, uint32_t address);
void CORE_EBI_WRITE16(__TARGET_CHIP__)(uint8_t index, uint32_t address, 
										uint16_t data);
uint32_t CORE_EBI_READ32(__TARGET_CHIP__)(uint8_t index, uint32_t address);
void CORE_EBI_WRITE32(__TARGET_CHIP__)(uint8_t index, uint32_t address, 
										uint32_t data);

#endif

#if IFS_SDIO_EN

#define CORE_SDIO_RESP_NONE(m)			__CONNECT(m, _SDIO_RESP_NONE)
#define CORE_SDIO_RESP_SHORT(m)			__CONNECT(m, _SDIO_RESP_SHORT)
#define CORE_SDIO_RESP_LONG(m)			__CONNECT(m, _SDIO_RESP_LONG)
#define SDIO_RESP_NONE					CORE_SDIO_RESP_NONE(__TARGET_CHIP__)
#define SDIO_RESP_SHORT					CORE_SDIO_RESP_SHORT(__TARGET_CHIP__)
#define SDIO_RESP_LONG					CORE_SDIO_RESP_LONG(__TARGET_CHIP__)

struct interface_sdio_t
{
	vsf_err_t (*init)(uint8_t index);
	vsf_err_t (*fini)(uint8_t index);
	vsf_err_t (*config)(uint8_t index, uint16_t kHz, uint8_t buswidth);
	vsf_err_t (*start)(uint8_t index);
	vsf_err_t (*stop)(uint8_t index);
	vsf_err_t (*send_cmd)(uint8_t index, uint8_t cmd, uint32_t arg,
							uint8_t resp);
	vsf_err_t (*send_cmd_isready)(uint8_t index, uint8_t resp);
	vsf_err_t (*get_resp)(uint8_t index, uint8_t *cresp, uint32_t *resp,
							uint8_t resp_num);
	vsf_err_t (*data_tx)(uint8_t index, uint32_t to_ms, uint32_t size,
							uint32_t block_size);
	vsf_err_t (*data_tx_isready)(uint8_t index, uint32_t size, uint8_t *buffer);
	vsf_err_t (*data_rx)(uint8_t index, uint32_t to_ms, uint32_t size,
							uint32_t block_size);
	vsf_err_t (*data_rx_isready)(uint8_t index, uint32_t size, uint8_t *buffer);
};

#define CORE_SDIO_INIT(m)				__CONNECT(m, _sdio_init)
#define CORE_SDIO_FINI(m)				__CONNECT(m, _sdio_fini)
#define CORE_SDIO_CONFIG(m)				__CONNECT(m, _sdio_config)
#define CORE_SDIO_START(m)				__CONNECT(m, _sdio_start)
#define CORE_SDIO_STOP(m)				__CONNECT(m, _sdio_stop)
#define CORE_SDIO_SEND_CMD(m)			__CONNECT(m, _sdio_send_cmd)
#define CORE_SDIO_SEND_CMD_ISREADY(m)	__CONNECT(m, _sdio_send_cmd_isready)
#define CORE_SDIO_GET_RESP(m)			__CONNECT(m, _sdio_get_resp)
#define CORE_SDIO_DATA_TX(m)			__CONNECT(m, _sdio_data_tx)
#define CORE_SDIO_DATA_TX_ISREADY(m)	__CONNECT(m, _sdio_data_tx_isready)
#define CORE_SDIO_DATA_RX(m)			__CONNECT(m, _sdio_data_rx)
#define CORE_SDIO_DATA_RX_ISREADY(m)	__CONNECT(m, _sdio_data_rx_isready)

vsf_err_t CORE_SDIO_INIT(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_SDIO_FINI(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_SDIO_CONFIG(__TARGET_CHIP__)(uint8_t index, uint16_t kHz,
											uint8_t buswide);
vsf_err_t CORE_SDIO_START(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_SDIO_STOP(__TARGET_CHIP__)(uint8_t index);
vsf_err_t CORE_SDIO_SEND_CMD(__TARGET_CHIP__)(uint8_t index, uint8_t cmd,
												uint32_t arg, uint8_t resp);
vsf_err_t CORE_SDIO_SEND_CMD_ISREADY(__TARGET_CHIP__)(uint8_t index,
														uint8_t resp);
vsf_err_t CORE_SDIO_GET_RESP(__TARGET_CHIP__)(uint8_t index, uint8_t *cresp,
											uint32_t *resp, uint8_t resp_num);
vsf_err_t CORE_SDIO_DATA_TX(__TARGET_CHIP__)(uint8_t index, uint32_t to_ms,
											uint32_t size, uint32_t block_size);
vsf_err_t CORE_SDIO_DATA_TX_ISREADY(__TARGET_CHIP__)(uint8_t index,
												uint32_t size, uint8_t *buffer);
vsf_err_t CORE_SDIO_DATA_RX(__TARGET_CHIP__)(uint8_t index, uint32_t to_ms,
											uint32_t size, uint32_t block_size);
vsf_err_t CORE_SDIO_DATA_RX_ISREADY(__TARGET_CHIP__)(uint8_t index,
												uint32_t size, uint8_t *buffer);

#endif

#if IFS_USBD_EN

enum interface_usbd_eptype_t
{
	USB_EP_TYPE_CONTROL,
	USB_EP_TYPE_INTERRUPT,
	USB_EP_TYPE_BULK,
	USB_EP_TYPE_ISO
};

enum interface_usbd_error_t
{
	USBERR_ERROR,
	USBERR_INVALID_CRC,
	USBERR_SOF_TO,
};

struct interface_usbd_callback_t
{
	void *param;
	vsf_err_t (*on_attach)(void*);
	vsf_err_t (*on_detach)(void*);
	vsf_err_t (*on_reset)(void*);
	vsf_err_t (*on_setup)(void*);
	vsf_err_t (*on_error)(void*, enum interface_usbd_error_t error);
	vsf_err_t (*on_wakeup)(void*);
	vsf_err_t (*on_suspend)(void*);
	vsf_err_t (*on_sof)(void*);
	vsf_err_t (*on_underflow)(void*, uint8_t);
	vsf_err_t (*on_overflow)(void*, uint8_t);
	vsf_err_t (*on_in)(void*, uint8_t);
	vsf_err_t (*on_out)(void*, uint8_t);
};

struct interface_usbd_t
{
	vsf_err_t (*init)(uint32_t int_priority);
	vsf_err_t (*fini)(void);
	vsf_err_t (*reset)(void);
	vsf_err_t (*poll)(void);
	
	vsf_err_t (*connect)(void);
	vsf_err_t (*disconnect)(void);
	
	vsf_err_t (*set_address)(uint8_t addr);
	uint8_t (*get_address)(void);
	
	vsf_err_t (*suspend)(void);
	vsf_err_t (*resume)(void);
	vsf_err_t (*lowpower)(uint8_t level);
	
	uint32_t (*get_frame_number)(void);
	
	vsf_err_t (*get_setup)(uint8_t *buffer);
	vsf_err_t (*prepare_buffer)(void);
	struct usbd_endpoint_t
	{
		const uint8_t *num_of_ep;
		
		vsf_err_t (*reset)(uint8_t idx);
		vsf_err_t (*set_type)(uint8_t idx, enum interface_usbd_eptype_t type);
		
		vsf_err_t (*set_IN_dbuffer)(uint8_t idx);
		bool (*is_IN_dbuffer)(uint8_t idx);
		vsf_err_t (*switch_IN_buffer)(uint8_t idx);
		vsf_err_t (*set_IN_epsize)(uint8_t idx, uint16_t size);
		uint16_t (*get_IN_epsize)(uint8_t idx);
		vsf_err_t (*set_IN_stall)(uint8_t idx);
		vsf_err_t (*clear_IN_stall)(uint8_t idx);
		bool (*is_IN_stall)(uint8_t idx);
		vsf_err_t (*reset_IN_toggle)(uint8_t idx);
		vsf_err_t (*toggle_IN_toggle)(uint8_t idx);
		vsf_err_t (*set_IN_count)(uint8_t idx, uint16_t size);
		vsf_err_t (*write_IN_buffer)(uint8_t idx, uint8_t *buffer, 
										uint16_t size);
		
		vsf_err_t (*set_OUT_dbuffer)(uint8_t idx);
		bool (*is_OUT_dbuffer)(uint8_t idx);
		vsf_err_t (*switch_OUT_buffer)(uint8_t idx);
		vsf_err_t (*set_OUT_epsize)(uint8_t idx, uint16_t size);
		uint16_t (*get_OUT_epsize)(uint8_t idx);
		vsf_err_t (*set_OUT_stall)(uint8_t idx);
		vsf_err_t (*clear_OUT_stall)(uint8_t idx);
		bool (*is_OUT_stall)(uint8_t idx);
		vsf_err_t (*reset_OUT_toggle)(uint8_t idx);
		vsf_err_t (*toggle_OUT_toggle)(uint8_t idx);
		uint16_t (*get_OUT_count)(uint8_t idx);
		vsf_err_t (*read_OUT_buffer)(uint8_t idx, uint8_t *buffer, 
										uint16_t size);
		vsf_err_t (*enable_OUT)(uint8_t idx);
	} ep;
	struct interface_usbd_callback_t *callback;
};

#define CORE_USBD_INIT(m)				__CONNECT(m, _usbd_init)
#define CORE_USBD_FINI(m)				__CONNECT(m, _usbd_fini)
#define CORE_USBD_RESET(m)				__CONNECT(m, _usbd_reset)
#define CORE_USBD_POLL(m)				__CONNECT(m, _usbd_poll)
#define CORE_USBD_CONNECT(m)			__CONNECT(m, _usbd_connect)
#define CORE_USBD_DISCONNECT(m)			__CONNECT(m, _usbd_disconnect)
#define CORE_USBD_SET_ADDRESS(m)		__CONNECT(m, _usbd_set_address)
#define CORE_USBD_GET_ADDRESS(m)		__CONNECT(m, _usbd_get_address)
#define CORE_USBD_SUSPEND(m)			__CONNECT(m, _usbd_suspend)
#define CORE_USBD_RESUME(m)				__CONNECT(m, _usbd_resume)
#define CORE_USBD_LOWPOWER(m)			__CONNECT(m, _usbd_lowpower)
#define CORE_USBD_GET_FRAME_NUM(m)		__CONNECT(m, _usbd_get_frame_number)
#define CORE_USBD_GET_SETUP(m)			__CONNECT(m, _usbd_get_setup)
#define CORE_USBD_PREPARE_BUFFER(m)		__CONNECT(m, _usbd_prepare_buffer)
#define CORE_USBD_EP_NUM(m)				__CONNECT(m, _usbd_ep_num)
#define CORE_USBD_EP_RESET(m)			__CONNECT(m, _usbd_ep_reset)
#define CORE_USBD_EP_SET_TYPE(m)		__CONNECT(m, _usbd_ep_set_type)
#define CORE_USBD_EP_SET_IN_DBUFFER(m)	__CONNECT(m, _usbd_ep_set_IN_dbuffer)
#define CORE_USBD_EP_IS_IN_DBUFFER(m)	__CONNECT(m, _usbd_ep_is_IN_dbuffer)
#define CORE_USBD_EP_SWITCH_IN_BUFFER(m)\
										__CONNECT(m, _usbd_ep_switch_IN_buffer)
#define CORE_USBD_EP_SET_IN_EPSIZE(m)	__CONNECT(m, _usbd_ep_set_IN_epsize)
#define CORE_USBD_EP_GET_IN_EPSIZE(m)	__CONNECT(m, _usbd_ep_get_IN_epsize)
#define CORE_USBD_EP_SET_IN_STALL(m)	__CONNECT(m, _usbd_ep_set_IN_stall)
#define CORE_USBD_EP_CLEAR_IN_STALL(m)	__CONNECT(m, _usbd_ep_clear_IN_stall)
#define CORE_USBD_EP_IS_IN_STALL(m)		__CONNECT(m, _usbd_ep_is_IN_stall)
#define CORE_USBD_EP_RESET_IN_TOGGLE(m)	__CONNECT(m, _usbd_ep_reset_IN_toggle)
#define CORE_USBD_EP_TOGGLE_IN_TOGGLE(m)\
										__CONNECT(m, _usbd_ep_toggle_IN_toggle)
#define CORE_USBD_EP_SET_IN_COUNT(m)	__CONNECT(m, _usbd_ep_set_IN_count)
#define CORE_USBD_EP_WRITE_IN_BUFFER(m)	__CONNECT(m, _usbd_ep_write_IN_buffer)
#define CORE_USBD_EP_SET_OUT_DBUFFER(m)	__CONNECT(m, _usbd_ep_set_OUT_dbuffer)
#define CORE_USBD_EP_IS_OUT_DBUFFER(m)	__CONNECT(m, _usbd_ep_is_OUT_dbuffer)
#define CORE_USBD_EP_SWITCH_OUT_BUFFER(m)\
										__CONNECT(m, _usbd_ep_switch_OUT_buffer)
#define CORE_USBD_EP_SET_OUT_EPSIZE(m)	__CONNECT(m, _usbd_ep_set_OUT_epsize)
#define CORE_USBD_EP_GET_OUT_EPSIZE(m)	__CONNECT(m, _usbd_ep_get_OUT_epsize)
#define CORE_USBD_EP_SET_OUT_STALL(m)	__CONNECT(m, _usbd_ep_set_OUT_stall)
#define CORE_USBD_EP_CLEAR_OUT_STALL(m)	__CONNECT(m, _usbd_ep_clear_OUT_stall)
#define CORE_USBD_EP_IS_OUT_STALL(m)	__CONNECT(m, _usbd_ep_is_OUT_stall)
#define CORE_USBD_EP_RESET_OUT_TOGGLE(m)\
										__CONNECT(m, _usbd_ep_reset_OUT_toggle)
#define CORE_USBD_EP_TOGGLE_OUT_TOGGLE(m)\
										__CONNECT(m, _usbd_ep_toggle_OUT_toggle)
#define CORE_USBD_EP_GET_OUT_COUNT(m)	__CONNECT(m, _usbd_ep_get_OUT_count)
#define CORE_USBD_EP_READ_OUT_BUFFER(m)	__CONNECT(m, _usbd_ep_read_OUT_buffer)
#define CORE_USBD_EP_ENABLE_OUT(m)		__CONNECT(m, _usbd_ep_enable_OUT)
#define CORE_USBD_CALLBACK(m)			__CONNECT(m, _usbd_callback)

vsf_err_t CORE_USBD_INIT(__TARGET_CHIP__)(uint32_t int_priority);
vsf_err_t CORE_USBD_FINI(__TARGET_CHIP__)(void);
vsf_err_t CORE_USBD_RESET(__TARGET_CHIP__)(void);
vsf_err_t CORE_USBD_POLL(__TARGET_CHIP__)(void);
vsf_err_t CORE_USBD_CONNECT(__TARGET_CHIP__)(void);
vsf_err_t CORE_USBD_DISCONNECT(__TARGET_CHIP__)(void);
vsf_err_t CORE_USBD_SET_ADDRESS(__TARGET_CHIP__)(uint8_t addr);
uint8_t CORE_USBD_GET_ADDRESS(__TARGET_CHIP__)(void);
vsf_err_t CORE_USBD_SUSPEND(__TARGET_CHIP__)(void);
vsf_err_t CORE_USBD_RESUME(__TARGET_CHIP__)(void);
vsf_err_t CORE_USBD_LOWPOWER(__TARGET_CHIP__)(uint8_t level);
uint32_t CORE_USBD_GET_FRAME_NUM(__TARGET_CHIP__)(void);
vsf_err_t CORE_USBD_GET_SETUP(__TARGET_CHIP__)(uint8_t *buffer);
vsf_err_t CORE_USBD_PREPARE_BUFFER(__TARGET_CHIP__)(void);
extern const uint8_t CORE_USBD_EP_NUM(__TARGET_CHIP__);
vsf_err_t CORE_USBD_EP_RESET(__TARGET_CHIP__)(uint8_t idx);
vsf_err_t CORE_USBD_EP_SET_TYPE(__TARGET_CHIP__)(uint8_t idx, 
		enum interface_usbd_eptype_t type);
vsf_err_t CORE_USBD_EP_SET_IN_DBUFFER(__TARGET_CHIP__)(uint8_t idx);
bool CORE_USBD_EP_IS_IN_DBUFFER(__TARGET_CHIP__)(uint8_t idx);
vsf_err_t CORE_USBD_EP_SWITCH_IN_BUFFER(__TARGET_CHIP__)(uint8_t idx);
vsf_err_t CORE_USBD_EP_SET_IN_EPSIZE(__TARGET_CHIP__)(uint8_t idx, 
														uint16_t size);
uint16_t CORE_USBD_EP_GET_IN_EPSIZE(__TARGET_CHIP__)(uint8_t idx);
vsf_err_t CORE_USBD_EP_SET_IN_STALL(__TARGET_CHIP__)(uint8_t idx);
vsf_err_t CORE_USBD_EP_CLEAR_IN_STALL(__TARGET_CHIP__)(uint8_t idx);
bool CORE_USBD_EP_IS_IN_STALL(__TARGET_CHIP__)(uint8_t idx);
vsf_err_t CORE_USBD_EP_RESET_IN_TOGGLE(__TARGET_CHIP__)(uint8_t idx);
vsf_err_t CORE_USBD_EP_TOGGLE_IN_TOGGLE(__TARGET_CHIP__)(uint8_t idx);
vsf_err_t CORE_USBD_EP_SET_IN_COUNT(__TARGET_CHIP__)(uint8_t idx, 
														uint16_t size);
vsf_err_t CORE_USBD_EP_WRITE_IN_BUFFER(__TARGET_CHIP__)(uint8_t idx, 
		uint8_t *buffer, uint16_t size);
vsf_err_t CORE_USBD_EP_SET_OUT_DBUFFER(__TARGET_CHIP__)(uint8_t idx);
bool CORE_USBD_EP_IS_OUT_DBUFFER(__TARGET_CHIP__)(uint8_t idx);
vsf_err_t CORE_USBD_EP_SWITCH_OUT_BUFFER(__TARGET_CHIP__)(uint8_t idx);
vsf_err_t CORE_USBD_EP_SET_OUT_EPSIZE(__TARGET_CHIP__)(uint8_t idx, 
														uint16_t size);
uint16_t CORE_USBD_EP_GET_OUT_EPSIZE(__TARGET_CHIP__)(uint8_t idx);
vsf_err_t CORE_USBD_EP_SET_OUT_STALL(__TARGET_CHIP__)(uint8_t idx);
vsf_err_t CORE_USBD_EP_CLEAR_OUT_STALL(__TARGET_CHIP__)(uint8_t idx);
bool CORE_USBD_EP_IS_OUT_STALL(__TARGET_CHIP__)(uint8_t idx);
vsf_err_t CORE_USBD_EP_RESET_OUT_TOGGLE(__TARGET_CHIP__)(uint8_t idx);
vsf_err_t CORE_USBD_EP_TOGGLE_OUT_TOGGLE(__TARGET_CHIP__)(uint8_t idx);
uint16_t CORE_USBD_EP_GET_OUT_COUNT(__TARGET_CHIP__)(uint8_t idx);
vsf_err_t CORE_USBD_EP_READ_OUT_BUFFER(__TARGET_CHIP__)(uint8_t idx, 
		uint8_t *buffer, uint16_t size);
vsf_err_t CORE_USBD_EP_ENABLE_OUT(__TARGET_CHIP__)(uint8_t idx);
extern struct interface_usbd_callback_t CORE_USBD_CALLBACK(__TARGET_CHIP__);

#endif

struct interfaces_comm_t
{
	vsf_err_t (*init)(void);
	vsf_err_t (*fini)(void);
	void (*set_timeout)(uint32_t ms);
	vsf_err_t (*transact)(uint8_t *buffer_out, uint16_t out_len,
							uint8_t *buffer_in, uint16_t *in_len);
	
	vsf_err_t (*display_all)(void);
};

struct interfaces_info_t
{
	struct interfaces_comm_t *comm;
	
	struct interface_core_t core;
#if IFS_UNIQUEID_EN
	struct interface_uid_t uid;
#endif
#if IFS_FLASH_EN
	struct interface_flash_t flash;
#endif
#if IFS_CLKO_EN
	struct interface_clko_t clko;
#endif
#if IFS_GPIO_EN
	struct interface_gpio_t gpio;
#endif
#if IFS_TIMER_EN
	struct interface_timer_t timer;
#endif
#if IFS_EINT_EN
	struct interface_eint_t eint;
#endif
#if IFS_USART_EN
	struct interface_usart_t usart;
#endif
#if IFS_SPI_EN
	struct interface_spi_t spi;
#endif
#if IFS_ADC_EN
	struct interface_adc_t adc;
#endif
#if IFS_IIC_EN
	struct interface_i2c_t i2c;
#endif
#if IFS_USBD_EN
	struct interface_usbd_t usbd;
#endif
#if IFS_PWM_EN
	struct interface_pwm_t pwm;
#endif
#if IFS_MICROWIRE_EN
	struct interface_microwire_t microwire;
#endif
#if IFS_EBI_EN
	struct interface_ebi_t ebi;
#endif
#if IFS_SDIO_EN
	struct interface_sdio_t sdio;
#endif
	struct interface_tickclk_t tickclk;
	struct interface_delay_t delay;
	vsf_err_t (*peripheral_commit)(void);
};

extern const struct interfaces_info_t core_interfaces;

#include "app_interfaces.h"

#endif	// __INTERFACES_H_INCLUDED__
