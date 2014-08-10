/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       hw_cfg_MiniRelease1.h                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    hardware configuration file for Mini Version Release1     *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 *      2008-11-22:     rewrite GPIO_Dir(by SimonQian)                    *
 *      2011-12-07:     rewrite from stm32vl-discovery(by SimonQian)      *
 **************************************************************************/

//
// To make the ST_LINK firmware
//

#ifndef HSE_VALUE
#define HSE_VALUE						((uint32_t)8000000)
#endif

#define OSC_HZ							HSE_VALUE

#define _SYS_FREQUENCY					72		// in MHz
#define _SYS_FLASH_VECTOR_TABLE_SHIFT	FLASH_LOAD_OFFSET // From board_defs.mk

/****************************** Abilities ******************************/
#define HW_HAS_USART					1
#define HW_HAS_SPI						0
#define HW_HAS_SDIO						0
#define HW_HAS_EBI						0
#define HW_HAS_IIC						0
#define HW_HAS_GPIO						1
#define HW_HAS_CAN						0
#define HW_HAS_PWM						0
#define HW_HAS_ADC						0
#define HW_HAS_DAC						0
#define HW_HAS_MICROWIRE				0
#define HW_HAS_JTAG						0
#define HW_HAS_ISSP						0
#define HW_HAS_C2						0
#define HW_HAS_MSP430_JTAG				0
#define HW_HAS_MSP430_SBW				0
#define HW_HAS_LPCICP					0
#define HW_HAS_SWD						1
#define HW_HAS_SWIM						0
#define HW_HAS_BDM						0
#define HW_HAS_POWERCONTROL				1
#define HW_HAS_CLKO						0

#define HW_HAS_BEEPER					0
#define HW_HAS_LEDARRAY					0
#define HW_HAS_7COLOR_LED				0

/****************************** Power ******************************/
#define PWREXT_INIT()
#define PWREXT_ENABLE()
#define PWREXT_DISABLE()

/****************************** ST_LINK Pins ******************************/
#define ST_LINK_SWD_PORT				GPIOB

#define	ST_LINK_T_JTMS					GPIO_PIN_14	//GPIOB - Used as SWCLK
#define	ST_LINK_T_JTCK					GPIO_PIN_13	//GPIOB - Used as SWDIO

#define	ST_LINK_SWIM_1					GPIO_PIN_8	//GPIOB
#define	ST_LINK_SWIM_2					GPIO_PIN_11	//GPIOB

#define	ST_LINK_SWIM_RST				GPIO_PIN_6	//GPIOB
#define	ST_LINK_SWIM_RST_IN				GPIO_PIN_5	//GPIOB

#define	ST_LINK_STM_JRST				PB4
#define	ST_LINK_STM_JTDO				PB3
#define	ST_LINK_STM_JTDI				PA15
#define	ST_LINK_STM_JTCK				PA14
#define	ST_LINK_STM_JTMS				PA13
#define ST_LINK_USB_DP					PA12
#define ST_LINK_USB_DM					PA11

#define	ST_LINK_U1_RX					PA10
#define	ST_LINK_U1_TX					PA9

#if (_HARDWARE_VER == STM32F4_Discovery) ||\
	(_HARDWARE_VER == STM32L_Discovery) ||\
	(_HARDWARE_VER == STM32F0_Discovery)
#define	ST_LINK_ST_LINK_LED				9			//GPIOA
#elif (_HARDWARE_VER == STM32VL_Discovery) ||\
	(_HARDWARE_VER == STM8L_Discovery) ||\
	(_HARDWARE_VER == STM8S_Discovery) ||\
	(_HARDWARE_VER == STM8SVL_Discovery) ||\
	(_HARDWARE_VER == ST_Link)
#define	ST_LINK_ST_LINK_LED				8			//GPIOA
#else
#error "hardware not supported"
#endif

#define ST_LINK_T_JRST					GPIO_PIN_1	//GPIOB
#define ST_LINK_T_NRST					GPIO_PIN_0	//GPIOB
#define	ST_LINK_T_JTDI					GPIO_PIN_7	//GPIOA
#define ST_LINK_T_JTDO					GPIO_PIN_6	//GPIOA

#define	ST_LINK_U2_CK					GPIO_PIN_4	//GPIOA
#define ST_LINK_U2_RX					GPIO_PIN_3	//GPIOA
#define ST_LINK_U2_TX					GPIO_PIN_2	//GPIOA	

/****************************** USART ******************************/
#define USART_PORT						0
#define USART_RTS_PORT					0
#define USART_RTS_PIN					GPIO_TDI
#define USART_DTR_PORT					0
#define USART_DTR_PIN					GPIO_TMS

/****************************** SW ******************************/
#define SW_PORT							GPIOB
#define SW_PIN							ST_LINK_T_JRST
#define SW_RST_PORT						GPIOB
#define SW_RST_PIN						ST_LINK_T_NRST

#define SYNCSW_IN_PORT					ST_LINK_SWD_PORT
#define SYNCSW_IN_PIN					ST_LINK_T_JTMS
#define SYNCSW_OUT_PORT					ST_LINK_SWD_PORT
#define SYNCSW_OUT_PIN					ST_LINK_T_JTMS

#define SW_PULL_INIT()					
#define SW_DIR_INIT()					
#define SW_SETINPUT()					GPIO_SetMode(SW_PORT, SW_PIN, GPIO_MODE_IN_FLOATING)
#define SW_SETINPUT_PU()				GPIO_SetMode(SW_PORT, SW_PIN, GPIO_MODE_IPU)
#define SW_SETINPUT_PD()				GPIO_SetMode(SW_PORT, SW_PIN, GPIO_MODE_IPD)
#define SW_SETOUTPUT()					GPIO_SetMode(SW_PORT, SW_PIN, GPIO_MODE_OUT_PP)
#define SW_SETOUTPUT_OD()				GPIO_SetMode(SW_PORT, SW_PIN, GPIO_MODE_OUT_OD)
#define SW_SET()						GPIO_SetPins(SW_PORT, SW_PIN)
#define SW_CLR()						GPIO_ClrPins(SW_PORT, SW_PIN)
#define SW_GET()						GPIO_GetInPins(SW_PORT, SW_PIN)

#define SW_RST_PULL_INIT()				
#define SW_RST_DIR_INIT()				
#define SW_RST_SETINPUT()				GPIO_SetMode(SW_RST_PORT, SW_RST_PIN, GPIO_MODE_IN_FLOATING)
#define SW_RST_SETINPUT_PU()			GPIO_SetMode(SW_RST_PORT, SW_RST_PIN, GPIO_MODE_IPU)
#define SW_RST_SETINPUT_PD()			GPIO_SetMode(SW_RST_PORT, SW_RST_PIN, GPIO_MODE_IPD)
#define SW_RST_SETOUTPUT()				GPIO_SetMode(SW_RST_PORT, SW_RST_PIN, GPIO_MODE_OUT_PP)
#define SW_RST_SET()					GPIO_SetPins(SW_RST_PORT, SW_RST_PIN)
#define SW_RST_CLR()					GPIO_ClrPins(SW_RST_PORT, SW_RST_PIN)
#define SW_RST_GET()					GPIO_GetInPins(SW_RST_PORT, SW_RST_PIN)

#define SYNCSW_DIR_INIT()				
#define SYNCSW_SETINPUT()				GPIO_SetMode(SYNCSW_OUT_PORT, SYNCSW_OUT_PIN, GPIO_MODE_IN_FLOATING)
#define SYNCSW_SETOUTPUT()				GPIO_SetMode(SYNCSW_OUT_PORT, SYNCSW_OUT_PIN, GPIO_MODE_OUT_PP)
#define SYNCSW_SET()					GPIO_SetPins(SYNCSW_OUT_PORT, SYNCSW_OUT_PIN)
#define SYNCSW_CLR()					GPIO_ClrPins(SYNCSW_OUT_PORT, SYNCSW_OUT_PIN)
#define SYNCSW_GET()					GPIO_GetInPins(SYNCSW_IN_PORT, SYNCSW_IN_PIN)

// SYNCSW in PWM mode
#define SYNCSWPWM_GPIO_PORT				GPIOB
#define SYNCSWPWM_GPIO_PIN				GPIO_PIN_13

#define SYNCSWPWM_GPIO_SETINPUT()		GPIO_SetMode(SYNCSWPWM_GPIO_PORT, SYNCSWPWM_GPIO_PIN, GPIO_MODE_IN_FLOATING)
#define SYNCSWPWM_GPIO_SETINPUT_PU()	GPIO_SetMode(SYNCSWPWM_GPIO_PORT, SYNCSWPWM_GPIO_PIN, GPIO_MODE_IPU)
#define SYNCSWPWM_GPIO_SETINPUT_PD()	GPIO_SetMode(SYNCSWPWM_GPIO_PORT, SYNCSWPWM_GPIO_PIN, GPIO_MODE_IPD)
#define SYNCSWPWM_GPIO_SETOUTPUT()		GPIO_SetMode(SYNCSWPWM_GPIO_PORT, SYNCSWPWM_GPIO_PIN, GPIO_MODE_OUT_PP)
#define SYNCSWPWM_GPIO_SET()			GPIO_SetPins(SYNCSWPWM_GPIO_PORT, SYNCSWPWM_GPIO_PIN)
#define SYNCSWPWM_GPIO_CLR()			GPIO_ClrPins(SYNCSWPWM_GPIO_PORT, SYNCSWPWM_GPIO_PIN)
#define SYNCSWPWM_GPIO_GET()			GPIO_GetInPins(SYNCSWPWM_GPIO_PORT, SYNCSWPWM_GPIO_PIN)

/***************************** SWD ******************************/
#define SWD_SWDIO_SETOUTPUT()			JTAG_TAP_TMS_SETOUTPUT()
#define SWD_SWDIO_SETINPUT()			JTAG_TAP_TMS_SETINPUT()
#define SWD_SWDIO_SET()					JTAG_TAP_TMS_SET()
#define SWD_SWDIO_CLR()					JTAG_TAP_TMS_CLR()
#define SWD_SWDIO_GET()					JTAG_TAP_TMS_GET()

#define SWD_SWCLK_SETOUTPUT()			JTAG_TAP_TCK_SETOUTPUT()
#define SWD_SWCLK_SETINPUT()			JTAG_TAP_TCK_SETINPUT()
#define SWD_SWCLK_SET()					JTAG_TAP_TCK_SET()
#define SWD_SWCLK_CLR()					JTAG_TAP_TCK_CLR()

/***************************** JTAG ******************************/
#define JTAG_TAP_PORT					GPIOB
#define JTAG_TAP_TCK_PIN				ST_LINK_T_JTCK
#define JTAG_TAP_TDO_PIN				ST_LINK_SWIM_RST
#define JTAG_TAP_TDI_PIN				ST_LINK_SWIM_RST_IN
#define JTAG_TAP_RTCK_PORT				GPIOA
#define JTAG_TAP_RTCK_PIN				GPIO_PIN_6

#define JTAG_HAS_USER_PIN				1

#define JTAG_TAP_USR_PORT				GPIOA
#define JTAG_TAP_USR1_PIN				GPIO_PIN_9
#define JTAG_TAP_USR2_PIN				GPIO_PIN_10

#define JTAG_TAP_USR1_SETOUTPUT()		GPIO_SetMode(JTAG_TAP_USR_PORT, JTAG_TAP_USR1_PIN, GPIO_MODE_OUT_PP)
#define JTAG_TAP_USR1_SETINPUT()		GPIO_SetMode(JTAG_TAP_USR_PORT, JTAG_TAP_USR1_PIN, GPIO_MODE_IN_FLOATING)
#define JTAG_TAP_USR1_SETINPUT_PU()		GPIO_SetMode(JTAG_TAP_USR_PORT, JTAG_TAP_USR1_PIN, GPIO_MODE_IPU)
#define JTAG_TAP_USR1_SETINPUT_PD()		GPIO_SetMode(JTAG_TAP_USR_PORT, JTAG_TAP_USR1_PIN, GPIO_MODE_IPD)
#define JTAG_TAP_USR1_SET()				GPIO_SetPins(JTAG_TAP_USR_PORT, JTAG_TAP_USR1_PIN)
#define JTAG_TAP_USR1_CLR()				GPIO_ClrPins(JTAG_TAP_USR_PORT, JTAG_TAP_USR1_PIN)
#define JTAG_TAP_USR1_GET()				GPIO_GetInPins(JTAG_TAP_USR_PORT, JTAG_TAP_USR1_PIN)

#define JTAG_TAP_USR2_SETOUTPUT()		GPIO_SetMode(JTAG_TAP_USR_PORT, JTAG_TAP_USR2_PIN, GPIO_MODE_OUT_PP)
#define JTAG_TAP_USR2_SETINPUT()		GPIO_SetMode(JTAG_TAP_USR_PORT, JTAG_TAP_USR2_PIN, GPIO_MODE_IN_FLOATING)
#define JTAG_TAP_USR2_SETINPUT_PU()		GPIO_SetMode(JTAG_TAP_USR_PORT, JTAG_TAP_USR2_PIN, GPIO_MODE_IPU)
#define JTAG_TAP_USR2_SETINPUT_PD()		GPIO_SetMode(JTAG_TAP_USR_PORT, JTAG_TAP_USR2_PIN, GPIO_MODE_IPD)
#define JTAG_TAP_USR2_SET()				GPIO_SetPins(JTAG_TAP_USR_PORT, JTAG_TAP_USR2_PIN)
#define JTAG_TAP_USR2_CLR()				GPIO_ClrPins(JTAG_TAP_USR_PORT, JTAG_TAP_USR2_PIN)
#define JTAG_TAP_USR2_GET()				GPIO_GetInPins(JTAG_TAP_USR_PORT, JTAG_TAP_USR2_PIN)

#define JTAG_TAP_TMS_SETOUTPUT()		SYNCSW_SETOUTPUT()
#define JTAG_TAP_TMS_SETINPUT()			SYNCSW_SETINPUT()
#define JTAG_TAP_TMS_SET()				SYNCSW_SET()
#define JTAG_TAP_TMS_CLR()				SYNCSW_CLR()
#define JTAG_TAP_TMS_GET()				SYNCSW_GET()

#define JTAG_TAP_TCK_SETOUTPUT()		GPIO_SetMode(JTAG_TAP_PORT, JTAG_TAP_TCK_PIN, GPIO_MODE_OUT_PP)
#define JTAG_TAP_TCK_SETINPUT()			GPIO_SetMode(JTAG_TAP_PORT, JTAG_TAP_TCK_PIN, GPIO_MODE_IN_FLOATING)
#define JTAG_TAP_TCK_SET()				GPIO_SetPins(JTAG_TAP_PORT, JTAG_TAP_TCK_PIN)
#define JTAG_TAP_TCK_CLR()				GPIO_ClrPins(JTAG_TAP_PORT, JTAG_TAP_TCK_PIN)
#define JTAG_TAP_TCK_GET()				GPIO_GetOutPins(JTAG_TAP_PORT, JTAG_TAP_TCK_PIN)

#define JTAG_TAP_TDI_SETOUTPUT()		GPIO_SetMode(JTAG_TAP_PORT, JTAG_TAP_TDI_PIN, GPIO_MODE_OUT_PP)
#define JTAG_TAP_TDI_SETINPUT()			GPIO_SetMode(JTAG_TAP_PORT, JTAG_TAP_TDI_PIN, GPIO_MODE_IN_FLOATING)
#define JTAG_TAP_TDI_SET()				GPIO_SetPins(JTAG_TAP_PORT, JTAG_TAP_TDI_PIN)
#define JTAG_TAP_TDI_CLR()				GPIO_ClrPins(JTAG_TAP_PORT, JTAG_TAP_TDI_PIN)
#define JTAG_TAP_TDI_GET()				GPIO_GetOutPins(JTAG_TAP_PORT, JTAG_TAP_TDI_PIN)

#define JTAG_TAP_TDO_SETINPUT()			GPIO_SetMode(JTAG_TAP_PORT, JTAG_TAP_TDO_PIN, GPIO_MODE_IN_FLOATING)
#define JTAG_TAP_TDO_SET()				GPIO_SetPins(JTAG_TAP_PORT, JTAG_TAP_TDO_PIN)
#define JTAG_TAP_TDO_CLR()				GPIO_ClrPins(JTAG_TAP_PORT, JTAG_TAP_TDO_PIN)
#define JTAG_TAP_TDO_GET()				GPIO_GetInPins(JTAG_TAP_PORT, JTAG_TAP_TDO_PIN)

#define JTAG_TAP_TRST_SETOUTPUT()		SW_RST_SETOUTPUT()
#define JTAG_TAP_TRST_SETINPUT()		SW_RST_SETINPUT_PU()
#define JTAG_TAP_TRST_SET()				SW_RST_SET()
#define JTAG_TAP_TRST_CLR()				SW_RST_CLR()
#define JTAG_TAP_TRST_GET()				SW_RST_GET()

#define JTAG_TAP_RTCK_SETINPUT()		GPIO_SetMode(JTAG_TAP_RTCK_PORT, JTAG_TAP_RTCK_PIN, GPIO_MODE_IN_FLOATING)
#define JTAG_TAP_RTCK_GET()				GPIO_GetInPins(JTAG_TAP_RTCK_PORT, JTAG_TAP_RTCK_PIN)

#define JTAG_TAP_SRST_SETOUTPUT()		SW_SETOUTPUT()
#define JTAG_TAP_SRST_SETINPUT()		SW_SETINPUT_PU()
#define JTAG_TAP_SRST_SET()				SW_SET()
#define JTAG_TAP_SRST_CLR()				SW_CLR()
#define JTAG_TAP_SRST_GET()				SW_GET()


#define JTAG_TAP_HS_SPI_M				SPI2
#define JTAG_TAP_HS_SPI_M_PORT			0x01
#define JTAG_TAP_HS_SPI_S				SPI1
#define JTAG_TAP_HS_SPI_S_PORT			0x10

// DMA
#define JTAG_TAP_HS_SPI_M_TX_DMA		DMA1_Channel5
#define JTAG_TAP_HS_SPI_M_RX_DMA		DMA1_Channel4
#define JTAG_TAP_HS_SPI_S_TX_DMA		DMA1_Channel3

#define JTAG_TAP_HS_SPI_EnableDMA()		do{\
											SPI_I2S_DMACmd(JTAG_TAP_HS_SPI_M, SPI_I2S_DMAReq_Rx, ENABLE);\
											SPI_I2S_DMACmd(JTAG_TAP_HS_SPI_S, SPI_I2S_DMAReq_Tx, ENABLE);\
										}while(0)

#define JTAG_TAP_HS_SPIS_Disable()		SPI_Cmd(JTAG_TAP_HS_SPI_S, DISABLE)
#define JTAG_TAP_HS_SPIS_Enable()		SPI_Cmd(JTAG_TAP_HS_SPI_S, ENABLE)

#define JTAG_TAP_HS_DMA_FINI()			do{\
											DMA_DeInit(JTAG_TAP_HS_SPI_M_RX_DMA);\
											DMA_DeInit(JTAG_TAP_HS_SPI_M_TX_DMA);\
											DMA_DeInit(JTAG_TAP_HS_SPI_S_TX_DMA);\
											RCC_AHBPeriphClockCmd(RCC_AHBPeriph_DMA1, DISABLE);\
										} while (0)
#define JTAG_TAP_HS_DMA_INIT()			do{\
											DMA_InitTypeDef  DMA_InitStructure;\
											\
											RCC_AHBPeriphClockCmd(RCC_AHBPeriph_DMA1, ENABLE);\
											\
											DMA_InitStructure.DMA_PeripheralBaseAddr = (uint32_t)&JTAG_TAP_HS_SPI_M->DR;\
											DMA_InitStructure.DMA_MemoryBaseAddr = (uint32_t)0;\
											DMA_InitStructure.DMA_DIR = DMA_DIR_PeripheralSRC;\
											DMA_InitStructure.DMA_BufferSize = 0;\
											DMA_InitStructure.DMA_PeripheralInc = DMA_PeripheralInc_Disable;\
											DMA_InitStructure.DMA_MemoryInc = DMA_MemoryInc_Enable;\
											DMA_InitStructure.DMA_PeripheralDataSize = DMA_PeripheralDataSize_Byte;\
											DMA_InitStructure.DMA_MemoryDataSize = DMA_MemoryDataSize_Byte;\
											DMA_InitStructure.DMA_Mode = DMA_Mode_Normal;\
											DMA_InitStructure.DMA_Priority = DMA_Priority_VeryHigh;\
											DMA_InitStructure.DMA_M2M = DMA_M2M_Disable;\
											DMA_Init(JTAG_TAP_HS_SPI_M_RX_DMA, &DMA_InitStructure);\
											\
											DMA_InitStructure.DMA_PeripheralBaseAddr = (uint32_t)&JTAG_TAP_HS_SPI_S->DR;\
											DMA_InitStructure.DMA_DIR = DMA_DIR_PeripheralDST;\
											DMA_Init(JTAG_TAP_HS_SPI_S_TX_DMA, &DMA_InitStructure);\
											\
											JTAG_TAP_HS_SPI_EnableDMA();\
										} while (0)

#define JTAG_TAP_HS_SPI_M_RX_DMA_LEN(l)	(JTAG_TAP_HS_SPI_M_RX_DMA->CNDTR = (uint32_t)(l))
#define JTAG_TAP_HS_SPI_M_RX_DMA_ADDR(a)(JTAG_TAP_HS_SPI_M_RX_DMA->CMAR = (uint32_t)(a))
#define JTAG_TAP_HS_SPI_M_RX_DMA_EN()	(JTAG_TAP_HS_SPI_M_RX_DMA->CCR |= (uint32_t)1)
#define JTAG_TAP_HS_SPI_M_RX_DMA_DIS()	(JTAG_TAP_HS_SPI_M_RX_DMA->CCR &= ~(uint32_t)1)
#define JTAG_TAP_HS_SPI_M_RX_DMA_WAIT()	do{while(!(DMA1->ISR & DMA1_FLAG_TC4)); DMA1->IFCR = DMA1_FLAG_TC4;}while(0)

#define JTAG_TAP_HS_SPI_S_TX_DMA_LEN(l)	(JTAG_TAP_HS_SPI_S_TX_DMA->CNDTR = (uint32_t)(l))
#define JTAG_TAP_HS_SPI_S_TX_DMA_ADDR(a)(JTAG_TAP_HS_SPI_S_TX_DMA->CMAR = (uint32_t)(a))
#define JTAG_TAP_HS_SPI_S_TX_DMA_EN()	(JTAG_TAP_HS_SPI_S_TX_DMA->CCR |= (uint32_t)1)
#define JTAG_TAP_HS_SPI_S_TX_DMA_DIS()	(JTAG_TAP_HS_SPI_S_TX_DMA->CCR &= ~(uint32_t)1)
#define JTAG_TAP_HS_SPI_S_TX_DMA_WAIT()	do{while(!(DMA1->ISR & DMA1_FLAG_TC3)); DMA1->IFCR = DMA1_FLAG_TC3;}while(0)

#define JTAG_TAP_HS_MPORT				GPIOB

#define JTAG_TAP_HS_SPORT				GPIOB
#define JTAG_TAP_TCK1_PIN				GPIO_PIN_3
#define JTAG_TAP_TMS_PIN				GPIO_PIN_4

#define JTAG_TAP_HS_PortIOInit()		do{\
											GPIO_SetMode(JTAG_TAP_HS_MPORT, JTAG_TAP_TCK_PIN, GPIO_MODE_AF_PP);\
											GPIO_SetMode(JTAG_TAP_HS_MPORT, JTAG_TAP_TDO_PIN, GPIO_MODE_AF_PP);\
											GPIO_SetMode(JTAG_TAP_HS_MPORT, JTAG_TAP_TDI_PIN, GPIO_MODE_AF_PP);\
											GPIO_SetMode(JTAG_TAP_HS_SPORT, JTAG_TAP_TCK1_PIN, GPIO_MODE_AF_PP);\
											GPIO_SetMode(JTAG_TAP_HS_SPORT, JTAG_TAP_TMS_PIN, GPIO_MODE_AF_PP);\
										}while(0)

#define JTAG_TAP_HS_PortIOFini()		do{\
											GPIO_SetMode(JTAG_TAP_HS_MPORT, JTAG_TAP_TCK_PIN, GPIO_MODE_IN_FLOATING);\
											GPIO_SetMode(JTAG_TAP_HS_MPORT, JTAG_TAP_TDO_PIN, GPIO_MODE_IN_FLOATING);\
											GPIO_SetMode(JTAG_TAP_HS_MPORT, JTAG_TAP_TDI_PIN, GPIO_MODE_IN_FLOATING);\
											GPIO_SetMode(JTAG_TAP_HS_SPORT, JTAG_TAP_TCK1_PIN, GPIO_MODE_IN_FLOATING);\
											GPIO_SetMode(JTAG_TAP_HS_SPORT, JTAG_TAP_TMS_PIN, GPIO_MODE_IN_FLOATING);\
										}while(0)

#define JTAG_TAP_HS_PortInit()			do{\
											RCC_APB2PeriphClockCmd(RCC_APB2Periph_SPI1, ENABLE);\
											RCC_APB1PeriphClockCmd(RCC_APB1Periph_SPI2, ENABLE);\
											GPIO_PinRemapConfig(GPIO_Remap_SPI1, ENABLE);\
											JTAG_TAP_HS_PortIOInit();\
										}while(0)
#define JTAG_TAP_HS_PortFini()			do{\
											JTAG_TAP_HS_PortIOFini();\
											RCC_APB2PeriphClockCmd(RCC_APB2Periph_SPI1, DISABLE);\
											RCC_APB1PeriphClockCmd(RCC_APB1Periph_SPI2, DISABLE);\
											GPIO_PinRemapConfig(GPIO_Remap_SPI1, DISABLE);\
										}while(0)

#define JTAG_TAP_HS_SetSpeed(div)		do{\
											SPI_Configuration(JTAG_TAP_HS_SPI_M, SPI_Mode_Master, (div),\
																SPI_FirstBit_LSB, SPI_CPOL_High, SPI_CPHA_2Edge);\
											SPI_Configuration(JTAG_TAP_HS_SPI_S, SPI_Mode_Slave,SPI_BaudRatePrescaler_2, \
																SPI_FirstBit_LSB, SPI_CPOL_High, SPI_CPHA_2Edge);\
										}while(0)

#define JTAG_TAP_HS_TMS_Out(tms)		JTAG_TAP_HS_SPI_S->DR = (tms)
#define JTAG_TAP_HS_TDI_Out(tdi)		JTAG_TAP_HS_SPI_M->DR = (tdi)
#define JTAG_TAP_HS_Out(tms, tdi)		do{JTAG_TAP_HS_TMS_Out(tms); JTAG_TAP_HS_TDI_Out(tdi);}while(0)
#define JTAG_TAP_HS_In()				JTAG_TAP_HS_SPI_M->DR

#define JTAG_TAP_HS_WaitTxReady()		while(!(JTAG_TAP_HS_SPI_M->SR & SPI_I2S_FLAG_TXE))
#define JTAG_TAP_HS_WaitRxReady()		while(!(JTAG_TAP_HS_SPI_M->SR & SPI_I2S_FLAG_RXNE))
#define JTAG_TAP_HS_WaitReady()			while(JTAG_TAP_HS_SPI_M->SR & SPI_I2S_FLAG_BSY)
/****************************** ADC ******************************/
#define TVCC_ADC_CHANNEL				8
#define TVCC_ADC_PORT					0

#define TVCC_SAMPLE_MIN_POWER			1800

#define TVCC_SAMPLE_DIV					2
#define TVCC_SAMPLE_VREF				3300
#define TVCC_SAMPLE_MAXVAL				4096

/****************************** LED ******************************/
#define LED_USB_PORT					0
#define LED_USB_PIN						ST_LINK_ST_LINK_LED

#define LED_POWER_INIT()				
#define LED_POWER_ON()					
#define LED_POWER_OFF()					

#define LED_STATE_INIT()				
#define LED_STATE_G_ON()				
#define LED_STATE_G_OFF()				

// LED_USB
#define LED_USB_INIT()					do {\
											LED_USB_OFF();\
											core_interfaces.gpio.init(LED_USB_PORT);\
											core_interfaces.gpio.config_pin(LED_USB_PORT, LED_USB_PIN, GPIO_OUTPP);\
										} while (0)
#define LED_USB_ON()					core_interfaces.gpio.out(LED_USB_PORT, 1 << LED_USB_PIN, 0)
#define LED_USB_OFF()					core_interfaces.gpio.out(LED_USB_PORT, 1 << LED_USB_PIN, 1 << LED_USB_PIN)

/****************************** KEY ******************************/
#define KEY_PORT						1
#define KEY_PIN							9
#define KEY_Init()						do {\
											core_interfaces.gpio.init(KEY_PORT);\
											core_interfaces.gpio.config_pin(KEY_PORT, KEY_PIN, GPIO_INPU);\
										} while (0)
#define KEY_Fini()						core_interfaces.gpio.config_pin(KEY_PORT, KEY_PIN, GPIO_INFLOAT)
#define KEY_IsDown()					!(core_interfaces.gpio.get(KEY_PORT, 1 << KEY_PIN) & (1 << KEY_PIN))

/****************************** USB *****************************/
#define USB_Pull_Init()					
#define USB_Connect()					
#define USB_Disconnect()				
