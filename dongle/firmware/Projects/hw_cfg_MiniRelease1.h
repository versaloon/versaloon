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
 **************************************************************************/

#ifndef HSE_VALUE
#define HSE_VALUE						((uint32_t)12000000)
#endif
#define OSC_HZ							HSE_VALUE

#define _SYS_FREQUENCY					72		// in MHz
#define _SYS_FLASH_VECTOR_TABLE_SHIFT	FLASH_LOAD_OFFSET // From board_defs.mk

/****************************** Abilities ******************************/
#define HW_HAS_USART					1
#define HW_HAS_SPI						1
#define HW_HAS_SDIO						0
#define HW_HAS_EBI						0
#define HW_HAS_IIC						1
#define HW_HAS_GPIO						1
#define HW_HAS_CAN						0
#define HW_HAS_PWM						1
#define HW_HAS_ADC						1
#define HW_HAS_DAC						0
#define HW_HAS_MICROWIRE				1
#define HW_HAS_DUSI						1
#define HW_HAS_JTAG						1
#define HW_HAS_ISSP						1
#define HW_HAS_C2						1
#define HW_HAS_MSP430_JTAG				1
#define HW_HAS_MSP430_SBW				0
#define HW_HAS_LPCICP					1
#define HW_HAS_SWD						1
#define HW_HAS_SWIM						1
#define HW_HAS_BDM						1
#define HW_HAS_POWERCONTROL				1
#define HW_HAS_CLKO						1

#define HW_HAS_BEEPER					0
#define HW_HAS_LEDARRAY					0
#define HW_HAS_7COLOR_LED				0

/****************************** Power ******************************/
#define PWREXT_EN_PORT					1
#define PWREXT_EN_PIN					8

#define PWREXT_INIT()					do {\
											core_interfaces.gpio.init(PWREXT_EN_PORT);\
											PWREXT_DISABLE();\
										} while (0)
#define PWREXT_ENABLE()					do {\
											core_interfaces.gpio.out(PWREXT_EN_PORT, 1 << PWREXT_EN_PIN, 0);\
											core_interfaces.gpio.config_pin(PWREXT_EN_PORT, PWREXT_EN_PIN, GPIO_OUTPP);\
										} while (0)
#define PWREXT_DISABLE()				core_interfaces.gpio.config_pin(PWREXT_EN_PORT, PWREXT_EN_PIN, GPIO_INFLOAT)

/****************************** USART ******************************/
#define USART_PORT						0
#define USART_RTS_PORT					0
#define USART_RTS_PIN					GPIO_TDI
#define USART_DTR_PORT					0
#define USART_DTR_PIN					GPIO_TMS

/****************************** SW ******************************/
#define SW_PORT							GPIOB
#define SW_PIN							GPIO_PIN_11
#define SW_RST_PORT						GPIOB
#define SW_RST_PIN						GPIO_PIN_10
#define SYNCSW_IN_PORT					GPIOB
#define SYNCSW_IN_PIN					GPIO_PIN_6
#define SYNCSW_OUT_PORT					GPIOB
#define SYNCSW_OUT_PIN					GPIO_PIN_4

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
#define SYNCSWPWM_GPIO_PIN				GPIO_PIN_6

#define SYNCSWPWM_GPIO_SETINPUT()		GPIO_SetMode(SYNCSWPWM_GPIO_PORT, SYNCSWPWM_GPIO_PIN, GPIO_MODE_IN_FLOATING)
#define SYNCSWPWM_GPIO_SETINPUT_PU()	GPIO_SetMode(SYNCSWPWM_GPIO_PORT, SYNCSWPWM_GPIO_PIN, GPIO_MODE_IPU)
#define SYNCSWPWM_GPIO_SETINPUT_PD()	GPIO_SetMode(SYNCSWPWM_GPIO_PORT, SYNCSWPWM_GPIO_PIN, GPIO_MODE_IPD)
#define SYNCSWPWM_GPIO_SETOUTPUT()		GPIO_SetMode(SYNCSWPWM_GPIO_PORT, SYNCSWPWM_GPIO_PIN, GPIO_MODE_OUT_PP)
#define SYNCSWPWM_GPIO_SET()			GPIO_SetPins(SYNCSWPWM_GPIO_PORT, SYNCSWPWM_GPIO_PIN)
#define SYNCSWPWM_GPIO_CLR()			GPIO_ClrPins(SYNCSWPWM_GPIO_PORT, SYNCSWPWM_GPIO_PIN)
#define SYNCSWPWM_GPIO_GET()			GPIO_GetInPins(SYNCSWPWM_GPIO_PORT, SYNCSWPWM_GPIO_PIN)

#define SYNCSWPWM_OUT_TIMER_MHZ			_SYS_FREQUENCY
#define SYNCSWPWM_OUT_TIMER				TIM3
#define SYNCSWPWM_OUT_TIMER_DMA_UPDATE	DMA1_Channel3
#define SYNCSWPWM_OUT_TIMER_DMA_COMPARE	DMA1_Channel6
#define SYNCSWPWM_IN_TIMER				TIM4
#define SYNCSWPWM_IN_TIMER_RISE_DMA		DMA1_Channel4
#define SYNCSWPWM_IN_TIMER_FALL_DMA		DMA1_Channel1

#define SYNCSWPWM_IN_TIMER_INIT()		do{\
											DMA_InitTypeDef DMA_InitStructure;\
											TIM_ICInitTypeDef TIM_ICInitStructure;\
											\
											RCC_AHBPeriphClockCmd(RCC_AHBPeriph_DMA1, ENABLE);\
											RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM4, ENABLE);\
											\
											DMA_DeInit(SYNCSWPWM_IN_TIMER_RISE_DMA);\
											DMA_StructInit(&DMA_InitStructure);\
											DMA_InitStructure.DMA_PeripheralBaseAddr = (uint32_t)&(SYNCSWPWM_IN_TIMER->CCR2);\
											DMA_InitStructure.DMA_DIR = DMA_DIR_PeripheralSRC;\
											DMA_InitStructure.DMA_BufferSize = 0;\
											DMA_InitStructure.DMA_PeripheralInc = DMA_PeripheralInc_Disable;\
											DMA_InitStructure.DMA_MemoryInc = DMA_MemoryInc_Enable;\
											DMA_InitStructure.DMA_PeripheralDataSize = DMA_PeripheralDataSize_HalfWord;\
											DMA_InitStructure.DMA_MemoryDataSize = DMA_MemoryDataSize_HalfWord;\
											DMA_InitStructure.DMA_Mode = DMA_Mode_Normal;\
											DMA_InitStructure.DMA_Priority = DMA_Priority_High;\
											DMA_InitStructure.DMA_M2M = DMA_M2M_Disable;\
											DMA_Init(SYNCSWPWM_IN_TIMER_RISE_DMA, &DMA_InitStructure);\
											DMA_Cmd(SYNCSWPWM_IN_TIMER_RISE_DMA, ENABLE);\
											\
											DMA_DeInit(SYNCSWPWM_IN_TIMER_FALL_DMA);\
											DMA_StructInit(&DMA_InitStructure);\
											DMA_InitStructure.DMA_PeripheralBaseAddr = (uint32_t)&(SYNCSWPWM_IN_TIMER->CCR1);\
											DMA_InitStructure.DMA_DIR = DMA_DIR_PeripheralSRC;\
											DMA_InitStructure.DMA_BufferSize = 0;\
											DMA_InitStructure.DMA_PeripheralInc = DMA_PeripheralInc_Disable;\
											DMA_InitStructure.DMA_MemoryInc = DMA_MemoryInc_Enable;\
											DMA_InitStructure.DMA_PeripheralDataSize = DMA_PeripheralDataSize_HalfWord;\
											DMA_InitStructure.DMA_MemoryDataSize = DMA_MemoryDataSize_HalfWord;\
											DMA_InitStructure.DMA_Mode = DMA_Mode_Normal;\
											DMA_InitStructure.DMA_Priority = DMA_Priority_High;\
											DMA_InitStructure.DMA_M2M = DMA_M2M_Disable;\
											DMA_Init(SYNCSWPWM_IN_TIMER_FALL_DMA, &DMA_InitStructure);\
											DMA_Cmd(SYNCSWPWM_IN_TIMER_FALL_DMA, ENABLE);\
											\
											TIM_ICStructInit(&TIM_ICInitStructure);\
											TIM_ICInitStructure.TIM_Channel = TIM_Channel_2;\
											TIM_ICInitStructure.TIM_ICPolarity = TIM_ICPolarity_Rising;\
											TIM_ICInitStructure.TIM_ICSelection = TIM_ICSelection_IndirectTI;\
											TIM_ICInitStructure.TIM_ICPrescaler = TIM_ICPSC_DIV1;\
											TIM_ICInitStructure.TIM_ICFilter = 0;\
											TIM_PWMIConfig(SYNCSWPWM_IN_TIMER, &TIM_ICInitStructure);\
											\
											TIM_SelectInputTrigger(SYNCSWPWM_IN_TIMER, TIM_TS_TI1FP1);\
											TIM_SelectSlaveMode(SYNCSWPWM_IN_TIMER, TIM_SlaveMode_Reset);\
											TIM_SelectMasterSlaveMode(SYNCSWPWM_IN_TIMER, TIM_MasterSlaveMode_Enable);\
											TIM_DMACmd(SYNCSWPWM_IN_TIMER, TIM_DMA_CC2, ENABLE);\
											TIM_DMACmd(SYNCSWPWM_IN_TIMER, TIM_DMA_CC1, ENABLE);\
											\
											TIM_PrescalerConfig(SYNCSWPWM_IN_TIMER, 0, TIM_PSCReloadMode_Immediate);\
											TIM_Cmd(SYNCSWPWM_IN_TIMER, ENABLE);\
										}while(0)
#define SYNCSWPWM_IN_TIMER_FINI()		do{\
											TIM_DeInit(SYNCSWPWM_IN_TIMER);\
											RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM4, DISABLE);\
											DMA_DeInit(SYNCSWPWM_IN_TIMER_RISE_DMA);\
											DMA_DeInit(SYNCSWPWM_IN_TIMER_FALL_DMA);\
											RCC_AHBPeriphClockCmd(RCC_AHBPeriph_DMA1, DISABLE);\
										}while(0)
#define SYNCSWPWM_IN_TIMER_RISE_DMA_INIT(l, a)	do{\
													SYNCSWPWM_IN_TIMER_RISE_DMA->CCR &= ~1;\
													SYNCSWPWM_IN_TIMER_RISE_DMA->CNDTR = (l);\
													SYNCSWPWM_IN_TIMER_RISE_DMA->CMAR = (uint32_t)(a);\
													SYNCSWPWM_IN_TIMER_RISE_DMA->CCR |= 1;\
												}while(0)
#define SYNCSWPWM_IN_TIMER_RISE_DMA_READY()		(DMA1->ISR & DMA1_FLAG_TC4)
#define SYNCSWPWM_IN_TIMER_RISE_DMA_RESET()		(DMA1->IFCR = DMA1_FLAG_TC4)
#define SYNCSWPWM_IN_TIMER_RISE_DMA_WAIT(dly)	do{\
													while(!SYNCSWPWM_IN_TIMER_RISE_DMA_READY() && --dly);\
													SYNCSWPWM_IN_TIMER_RISE_DMA_RESET();\
												}while(0)
#define SYNCSWPWM_IN_TIMER_FALL_DMA_INIT(l, a)	do{\
													SYNCSWPWM_IN_TIMER_FALL_DMA->CCR &= ~1;\
													SYNCSWPWM_IN_TIMER_FALL_DMA->CNDTR = (l);\
													SYNCSWPWM_IN_TIMER_FALL_DMA->CMAR = (uint32_t)(a);\
													SYNCSWPWM_IN_TIMER_FALL_DMA->CCR |= 1;\
												}while(0)
#define SYNCSWPWM_IN_TIMER_FALL_DMA_READY()		(DMA1->ISR & DMA1_FLAG_TC1)
#define SYNCSWPWM_IN_TIMER_FALL_DMA_RESET()		(DMA1->IFCR = DMA1_FLAG_TC1)
#define SYNCSWPWM_IN_TIMER_FALL_DMA_WAIT(dly)	do{\
													while(!SYNCSWPWM_IN_TIMER_FALL_DMA_READY() && --dly);\
													SYNCSWPWM_IN_TIMER_FALL_DMA_RESET();\
												}while(0)
#define SYNCSWPWM_IN_TIMER_DMA_INIT(l, a, b)	do{\
													SYNCSWPWM_IN_TIMER_RISE_DMA_INIT((l), (a));\
													SYNCSWPWM_IN_TIMER_FALL_DMA_INIT((l), (b));\
												}while(0)
#define SYNCSWPWM_IN_TIMER_DMA_WAIT(dly)	do{\
												SYNCSWPWM_IN_TIMER_RISE_DMA_WAIT(dly);\
												SYNCSWPWM_IN_TIMER_FALL_DMA_WAIT(dly);\
											}while(0)

#define SYNCSWPWM_OUT_TIMER_INIT(dma, p)	do{\
												DMA_InitTypeDef DMA_InitStructure;\
												TIM_TimeBaseInitTypeDef TIM_TimeBaseStructure;\
												TIM_OCInitTypeDef TIM_OCInitStructure;\
												\
												RCC_AHBPeriphClockCmd(RCC_AHBPeriph_DMA1, ENABLE);\
												RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM3, ENABLE);\
												\
												DMA_DeInit(dma);\
												DMA_StructInit(&DMA_InitStructure);\
												DMA_InitStructure.DMA_PeripheralBaseAddr = (uint32_t)&(SYNCSWPWM_OUT_TIMER->CCR1);\
												DMA_InitStructure.DMA_DIR = DMA_DIR_PeripheralDST;\
												DMA_InitStructure.DMA_BufferSize = 0;\
												DMA_InitStructure.DMA_PeripheralInc = DMA_PeripheralInc_Disable;\
												DMA_InitStructure.DMA_MemoryInc = DMA_MemoryInc_Enable;\
												DMA_InitStructure.DMA_PeripheralDataSize = DMA_PeripheralDataSize_HalfWord;\
												DMA_InitStructure.DMA_MemoryDataSize = DMA_MemoryDataSize_HalfWord;\
												DMA_InitStructure.DMA_Mode = DMA_Mode_Normal;\
												DMA_InitStructure.DMA_Priority = DMA_Priority_Medium;\
												DMA_InitStructure.DMA_M2M = DMA_M2M_Disable;\
												DMA_Init((dma), &DMA_InitStructure);\
												DMA_Cmd((dma), ENABLE);\
												\
												TIM_TimeBaseStructInit(&TIM_TimeBaseStructure);\
												TIM_TimeBaseStructure.TIM_Prescaler = 0;\
												TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;\
												TIM_TimeBaseStructure.TIM_Period = 0;\
												TIM_TimeBaseStructure.TIM_ClockDivision = 0;\
												TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;\
												TIM_TimeBaseInit(SYNCSWPWM_OUT_TIMER, &TIM_TimeBaseStructure);\
												\
												TIM_OCStructInit(&TIM_OCInitStructure);\
												TIM_OCInitStructure.TIM_OCMode = TIM_OCMode_PWM1;\
												TIM_OCInitStructure.TIM_OutputState = TIM_OutputState_Enable;\
												TIM_OCInitStructure.TIM_Pulse = 0;\
												TIM_OCInitStructure.TIM_OCPolarity = (p) ? TIM_OCPolarity_High : TIM_OCPolarity_Low;\
												TIM_OC1Init(SYNCSWPWM_OUT_TIMER, &TIM_OCInitStructure);\
												\
												TIM_OC1PreloadConfig(SYNCSWPWM_OUT_TIMER, TIM_OCPreload_Enable);\
												TIM_ARRPreloadConfig(SYNCSWPWM_OUT_TIMER, ENABLE);\
												if ((dma) == SYNCSWPWM_OUT_TIMER_DMA_UPDATE)\
												{\
													TIM_DMACmd(SYNCSWPWM_OUT_TIMER, TIM_DMA_Update, ENABLE);\
												}\
												else if ((dma) == SYNCSWPWM_OUT_TIMER_DMA_COMPARE)\
												{\
													TIM_DMACmd(SYNCSWPWM_OUT_TIMER, TIM_DMA_CC1, ENABLE);\
												}\
												TIM_Cmd(SYNCSWPWM_OUT_TIMER, ENABLE);\
												TIM_CtrlPWMOutputs(SYNCSWPWM_OUT_TIMER, ENABLE);\
											}while(0)
#define SYNCSWPWM_OUT_TIMER_FINI(dma)	do{\
											TIM_DeInit(SYNCSWPWM_OUT_TIMER);\
											RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM3, DISABLE);\
											DMA_DeInit(dma);\
											RCC_AHBPeriphClockCmd(RCC_AHBPeriph_DMA1, DISABLE);\
										}while(0)
#define SYNCSWPWM_OUT_TIMER_SetCycle(cycle)	do {\
												SYNCSWPWM_OUT_TIMER->ARR = (cycle);\
												SYNCSWPWM_OUT_TIMER->EGR = TIM_PSCReloadMode_Immediate;\
											} while (0)
#define SYNCSWPWM_OUT_TIMER_GetCycle()	SYNCSWPWM_OUT_TIMER->ARR
#define SYNCSWPWM_OUT_TIMER_GetRate()	SYNCSWPWM_OUT_TIMER->CCR1
#define SYNCSWPWM_OUT_TIMER_SetRate(r)	SYNCSWPWM_OUT_TIMER->CCR1 = (r)
#define SYNCSWPWM_OUT_TIMER_DMA_INIT(dma, l, a)	do{\
													SYNCSWPWM_OUT_TIMER->EGR = TIM_PSCReloadMode_Immediate;\
													(dma)->CCR &= ~1;\
													(dma)->CNDTR = (l);\
													(dma)->CMAR = (uint32_t)(a);\
													(dma)->CCR |= 1;\
												}while(0)
#define SYNCSWPWM_OUT_TIMER_DMA_UPDATE_READY()	(DMA1->ISR & DMA1_FLAG_TC3)
#define SYNCSWPWM_OUT_TIMER_DMA_UPDATE_RESET()	(DMA1->IFCR = DMA1_FLAG_TC3)
#define SYNCSWPWM_OUT_TIMER_DMA_UPDATE_WAIT()	do{\
													while(!SYNCSWPWM_OUT_TIMER_DMA_UPDATE_READY());\
													SYNCSWPWM_OUT_TIMER_DMA_UPDATE_RESET();\
												}while(0)
#define SYNCSWPWM_OUT_TIMER_DMA_COMPARE_READY()	(DMA1->ISR & DMA1_FLAG_TC6)
#define SYNCSWPWM_OUT_TIMER_DMA_COMPARE_RESET()	(DMA1->IFCR = DMA1_FLAG_TC6)
#define SYNCSWPWM_OUT_TIMER_DMA_COMPARE_WAIT()	do{\
													while(!SYNCSWPWM_OUT_TIMER_DMA_COMPARE_READY());\
													SYNCSWPWM_OUT_TIMER_DMA_COMPARE_RESET();\
												}while(0)

#define SYNCSWPWM_PORT_INIT()			GPIO_PinRemapConfig(GPIO_PartialRemap_TIM3, ENABLE)
#define SYNCSWPWM_PORT_FINI()			GPIO_PinRemapConfig(GPIO_PartialRemap_TIM3, DISABLE)

#define SYNCSWPWM_PORT_OD_INIT()		do{\
											SYNCSWPWM_PORT_INIT();\
											GPIO_SetPins(SYNCSW_OUT_PORT, SYNCSW_OUT_PIN);\
											GPIO_SetMode(SYNCSW_OUT_PORT, SYNCSW_OUT_PIN, GPIO_MODE_AF_OD);\
										}while(0)
#define SYNCSWPWM_PORT_PP_INIT()		do{\
											SYNCSWPWM_PORT_INIT();\
											GPIO_SetPins(SYNCSW_OUT_PORT, SYNCSW_OUT_PIN);\
											GPIO_SetMode(SYNCSW_OUT_PORT, SYNCSW_OUT_PIN, GPIO_MODE_AF_PP);\
										}while(0)
#define SYNCSWPWM_PORT_ODPP_FINI()		do{\
											GPIO_SetMode(SYNCSW_OUT_PORT, SYNCSW_OUT_PIN, GPIO_MODE_IN_FLOATING);\
											SYNCSWPWM_PORT_FINI();\
										}while(0)

/***************************** STM8_SWIM ******************************/
#define SWIM_SET()						SYNCSWPWM_GPIO_SETINPUT_PU()
#define SWIM_CLR()						do{\
											SYNCSWPWM_GPIO_CLR();\
											SYNCSWPWM_GPIO_SETOUTPUT();\
										} while (0)
#define SWIM_GET()						SYNCSWPWM_GPIO_GET()

/***************************** BDM ******************************/
#define BDM_SET()						SWIM_SET()
#define BDM_CLR()						SWIM_CLR()
#define BDM_GET()						SWIM_GET()

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

/***************************** DUSI ******************************/
#define DUSI_Config(d, fb, cpol, cpha)	do{\
											SPI_Configuration(JTAG_TAP_HS_SPI_M, SPI_Mode_Master, (d),\
																(fb), (cpol), (cpha));\
											SPI_Configuration(JTAG_TAP_HS_SPI_S, SPI_Mode_Slave,SPI_BaudRatePrescaler_2, \
																(fb), (cpol), (cpha));\
										} while (0)

#define DUSI_MasterOutByte(mo)			JTAG_TAP_HS_SPI_M->DR = (mo)
#define DUSI_SlaveOutByte(so)			JTAG_TAP_HS_SPI_S->DR = (so)
#define DUSI_MasterOutBytePtr(pmo)		DUSI_MasterOutByte(*(uint8_t *)(pmo))
#define DUSI_SlaveOutBytePtr(pso)		DUSI_SlaveOutByte(*(uint8_t *)(pso))
#define DUSI_OutByte(mo, so)			do{\
											DUSI_SlaveOutByte(so);\
											DUSI_MasterOutByte(mo);\
										} while (0)
#define DUSI_OutBytePtr(pmo, pso)		DUSI_OutByte(*(uint8_t*)pmo, *(uint8_t *)pso)

#define DUSI_MasterInByte(mi)			JTAG_TAP_HS_SPI_M->DR
#define DUSI_SlaveInByte(si)			JTAG_TAP_HS_SPI_S->DR
#define DUSI_MasterInBytePtr(pmi)		*(uint8_t *)(pmi) = JTAG_TAP_HS_SPI_M->DR
#define DUSI_SlaveInBytePtr(psi)		*(uint8_t *)(psi) = JTAG_TAP_HS_SPI_S->DR
#define DUSI_InBytePtr(pmi, psi)		do {\
											*(uint8_t *)(psi) = JTAG_TAP_HS_SPI_S->DR;\
											*(uint8_t *)(pmi) = JTAG_TAP_HS_SPI_M->DR;\
										} while (0)

/***************************** JTAG ******************************/
#define JTAG_TAP_PORT					GPIOB
#define JTAG_TAP_TCK_PIN				GPIO_PIN_13
#define JTAG_TAP_TDO_PIN				GPIO_PIN_14
#define JTAG_TAP_TDI_PIN				GPIO_PIN_15
#define JTAG_TAP_RTCK_PORT				GPIOA
#define JTAG_TAP_RTCK_PIN				GPIO_PIN_8

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

#define JTAG_TAP_HS_SetSpeed(div)		DUSI_Config(div, SPI_FirstBit_LSB, SPI_CPOL_High, SPI_CPHA_2Edge)

#define JTAG_TAP_HS_TMS_Out(tms)		JTAG_TAP_HS_SPI_S->DR = (tms)
#define JTAG_TAP_HS_TDI_Out(tdi)		JTAG_TAP_HS_SPI_M->DR = (tdi)
#define JTAG_TAP_HS_Out(tms, tdi)		do{JTAG_TAP_HS_TMS_Out(tms); JTAG_TAP_HS_TDI_Out(tdi);}while(0)
#define JTAG_TAP_HS_In()				JTAG_TAP_HS_SPI_M->DR

#define JTAG_TAP_HS_WaitTxReady()		while(!(JTAG_TAP_HS_SPI_M->SR & SPI_I2S_FLAG_TXE))
#define JTAG_TAP_HS_WaitRxReady()		while(!(JTAG_TAP_HS_SPI_M->SR & SPI_I2S_FLAG_RXNE))
#define JTAG_TAP_HS_WaitReady()			while(JTAG_TAP_HS_SPI_M->SR & SPI_I2S_FLAG_BSY)

/****************************** SPI ******************************/
#define SPI_PORT						1

#define SPI_MOSI_SET()					JTAG_TAP_TDI_SET()
#define SPI_MOSI_CLR()					JTAG_TAP_TDI_CLR()
#define SPI_MOSI_GET()					JTAG_TAP_TDI_GET()
#define SPI_MOSI_SETOUTPUT()			JTAG_TAP_TDI_SETOUTPUT()
#define SPI_MOSI_SETINPUT()				JTAG_TAP_TDI_SETINPUT()

#define SPI_MISO_GET()					JTAG_TAP_TDO_GET()
#define SPI_MISO_SETINPUT()				JTAG_TAP_TDO_SETINPUT()
#define SPI_MISO_SETINPUT_PU()			GPIO_SetMode(JTAG_TAP_PORT, JTAG_TAP_TDO_PIN, GPIO_MODE_IPU)

#define SPI_SCK_SET()					JTAG_TAP_TCK_SET()
#define SPI_SCK_CLR()					JTAG_TAP_TCK_CLR()
#define SPI_SCK_GET()					JTAG_TAP_TCK_GET()
#define SPI_SCK_SETOUTPUT()				JTAG_TAP_TCK_SETOUTPUT()
#define SPI_SCK_SETINPUT()				JTAG_TAP_TCK_SETINPUT()

#define SPI_SS_SETOUTPUT()				SW_RST_SETOUTPUT()
#define SPI_SS_SETINPUT()				SW_RST_SETINPUT()
#define SPI_SS_SET()					SW_RST_SET()
#define SPI_SS_CLR()					SW_RST_CLR()

#define SPI_Disable()					SPI_I2S_DeInit(SPI_Interface)
#define SPI_SetData(d)					SPI_Interface->DR = (d)
#define SPI_GetData()					SPI_Interface->DR
#define SPI_WaitReady()					while((SPI_Interface->SR & SPI_I2S_FLAG_BSY))
#define SPI_WaitRxReady()				while(!(SPI_Interface->SR & SPI_I2S_FLAG_RXNE))
#define SPI_WaitTxReady()				while(!(SPI_Interface->SR & SPI_I2S_FLAG_TXE))

#define SPI_AllInput()					do{\
											SPI_SCK_SETINPUT();\
											SPI_MISO_SETINPUT();\
											SPI_MOSI_SETINPUT();\
											RCC_APB1PeriphClockCmd(RCC_APB1Periph_SPI2, DISABLE);\
										}while(0)
#define SPI_AllSPIIO()					do{\
											SPI_SCK_SETOUTPUT();\
											SPI_MOSI_SETOUTPUT();\
											SPI_MISO_SETINPUT_PU();\
										}while(0)
#define SPI_AllSPIHW()					do{\
											RCC_APB1PeriphClockCmd(RCC_APB1Periph_SPI2, ENABLE);\
											GPIO_SetMode(JTAG_TAP_PORT, JTAG_TAP_TCK_PIN, GPIO_MODE_AF_PP);\
											GPIO_SetMode(JTAG_TAP_PORT, JTAG_TAP_TDO_PIN, GPIO_MODE_AF_PP);\
											GPIO_SetMode(JTAG_TAP_PORT, JTAG_TAP_TDI_PIN, GPIO_MODE_AF_PP);\
										}while(0)

/****************************** MicroWire ******************************/
#define MICROWIRE_SEL_SETOUTPUT()		SPI_SS_SETOUTPUT()
#define MICROWIRE_SEL_SETINPUT()		SPI_SS_SETINPUT()
#define MICROWIRE_SEL_SET()				SPI_SS_SET()
#define MICROWIRE_SEL_CLR()				SPI_SS_CLR()

#define MICROWIRE_SO_SETOUTPUT()		SPI_MOSI_SETOUTPUT()
#define MICROWIRE_SO_SETINPUT()			SPI_MOSI_SETINPUT()
#define MICROWIRE_SO_SET()				SPI_MOSI_SET()
#define MICROWIRE_SO_CLR()				SPI_MOSI_CLR()

#define MICROWIRE_SI_SETINPUT()			SPI_MISO_SETINPUT()
#define MICROWIRE_SI_GET()				SPI_MISO_GET()

#define MICROWIRE_SK_SETOUTPUT()		SPI_SCK_SETOUTPUT()
#define MICROWIRE_SK_SETINPUT()			SPI_SCK_SETINPUT()
#define MICROWIRE_SK_SET()				SPI_SCK_SET()
#define MICROWIRE_SK_CLR()				SPI_SCK_CLR()

/****************************** Reset ******************************/
#define RST_SET()						SW_SET()
#define RST_CLR()						SW_CLR()
#define RST_GET()						SW_GET()

#define RST_SETOUTPUT()					SW_SETOUTPUT()
#define RST_SETINPUT()					SW_SETINPUT_PU()

/************************** MSP430 JTAG ****************************/
#define MSP430_JTAG_TEST_SETOUTPUT()	JTAG_TAP_TRST_SETOUTPUT()
#define MSP430_JTAG_TEST_SETINPUT()		JTAG_TAP_TRST_SETINPUT()
#define MSP430_JTAG_TEST_SET()			JTAG_TAP_TRST_SET()
#define MSP430_JTAG_TEST_CLR()			JTAG_TAP_TRST_CLR()
#define MSP430_JTAG_TEST_GET()			JTAG_TAP_TRST_GET()

/************************** MSP430 SBW ****************************/
#define MSP430_SBWTDIO_SETOUTPUT()		SW_SETOUTPUT()
#define MSP430_SBWTDIO_SETINPUT()		SW_SETINPUT_PU()
#define MSP430_SBWTDIO_SET()			SW_SET()
#define MSP430_SBWTDIO_CLR()			SW_CLR()
#define MSP430_SBWTDIO_GET()			SW_GET()

#define MSP430_SBWTCK_SETOUTPUT()		MSP430_JTAG_TCK_SETOUTPUT()
#define MSP430_SBWTCK_SETINPUT()		MSP430_JTAG_TCK_SETINPUT()
#define MSP430_SBWTCK_SET()				MSP430_JTAG_TCK_SET()
#define MSP430_SBWTCK_CLR()				MSP430_JTAG_TCK_CLR()

/************************** PSoC1 ISSP ****************************/
#define ISSP_SDATA_SET()				SW_SET()
#define ISSP_SDATA_CLR()				SW_CLR()
#define ISSP_SDATA_GET()				SW_GET()
#define ISSP_SDATA_SETOUTPUT()			SW_SETOUTPUT()
#define ISSP_SDATA_SETINPUT()			SW_SETINPUT_PD()

#define ISSP_SCLK_SET()					JTAG_TAP_TCK_SET()
#define ISSP_SCLK_CLR()					JTAG_TAP_TCK_CLR()
#define ISSP_SCLK_SETOUTPUT()			JTAG_TAP_TCK_SETOUTPUT()
#define ISSP_SCLK_SETINPUT()			JTAG_TAP_TCK_SETINPUT()

#define ISSP_XRES_SET()					SW_RST_SET()
#define ISSP_XRES_CLR()					SW_RST_CLR()
#define ISSP_XRES_SETOUTPUT()			SW_RST_SETOUTPUT()
#define ISSP_XRES_SETINPUT()			SW_RST_SETINPUT_PU()

#define ISSP_PowerOn()					PWREXT_ENABLE()
#define ISSP_PowerOff()					PWREXT_DISABLE()

/****************************** C2 ******************************/
#define C2_C2CK_SET()					JTAG_TAP_TCK_SET()
#define C2_C2CK_CLR()					JTAG_TAP_TCK_CLR()
#define C2_C2CK_SETOUTPUT()				JTAG_TAP_TCK_SETOUTPUT()
#define C2_C2CK_SETINPUT()				JTAG_TAP_TCK_SETINPUT()

#define C2_C2D_SET()					SW_SET()
#define C2_C2D_CLR()					SW_CLR()
#define C2_C2D_GET()					SW_GET()
#define C2_C2D_SETOUTPUT()				SW_SETOUTPUT()
#define C2_C2D_SETINPUT()				SW_SETINPUT_PU()

/****************************** LPCICP ******************************/
#define LPCICP_PDA_SET()				SW_SET()
#define LPCICP_PDA_CLR()				SW_CLR()
#define LPCICP_PDA_GET()				SW_GET()
#define LPCICP_PDA_SETOUTPUT()			SW_SETOUTPUT()
#define LPCICP_PDA_SETINPUT()			SW_SETINPUT_PU()

#define LPCICP_PCL_SET()				JTAG_TAP_TCK_SET()
#define LPCICP_PCL_CLR()				JTAG_TAP_TCK_CLR()
#define LPCICP_PCL_SETOUTPUT()			JTAG_TAP_TCK_SETOUTPUT()
#define LPCICP_PCL_SETINPUT()			JTAG_TAP_TCK_SETINPUT()

#define LPCICP_RST_SET()				SW_RST_SET()
#define LPCICP_RST_CLR()				SW_RST_CLR()
#define LPCICP_RST_SETOUTPUT()			SW_RST_SETOUTPUT()
#define LPCICP_RST_SETINPUT()			SW_RST_SETINPUT_PU()

/****************************** IIC ******************************/
#define IIC_PORT						JTAG_TAP_PORT
#define IIC_SCL_PIN						JTAG_TAP_TMS_PIN
#define IIC_SDA_PIN						JTAG_TAP_TDO_PIN

#define IIC_PULL_INIT()					

#define IIC_SCL_INIT()					GPIO_SetMode(IIC_PORT, IIC_SCL_PIN, GPIO_MODE_OUT_OD)
#define IIC_SCL_FINI()					GPIO_SetMode(IIC_PORT, IIC_SCL_PIN, GPIO_MODE_IN_FLOATING)
#define IIC_SCL_SET()					GPIO_SetPins(IIC_PORT, IIC_SCL_PIN)
#define IIC_SCL_CLR()					GPIO_ClrPins(IIC_PORT, IIC_SCL_PIN)
#define IIC_SCL_GET()					GPIO_GetInPins(IIC_PORT, IIC_SCL_PIN)

#define IIC_SDA_INIT()					GPIO_SetMode(IIC_PORT, IIC_SDA_PIN, GPIO_MODE_OUT_OD)
#define IIC_SDA_FINI()					GPIO_SetMode(IIC_PORT, IIC_SDA_PIN, GPIO_MODE_IN_FLOATING)
#define IIC_SDA_SET()					GPIO_SetPins(IIC_PORT, IIC_SDA_PIN)
#define IIC_SDA_CLR()					GPIO_ClrPins(IIC_PORT, IIC_SDA_PIN)
#define IIC_SDA_GET()					GPIO_GetInPins(IIC_PORT, IIC_SDA_PIN)

/****************************** ADC ******************************/
#define TVCC_ADC_CHANNEL				8
#define TVCC_ADC_PORT					0

#define TVCC_SAMPLE_MIN_POWER			1800

#define TVCC_SAMPLE_DIV					2
#define TVCC_SAMPLE_VREF				3300
#define TVCC_SAMPLE_MAXVAL				4096

/****************************** LED ******************************/
#define LED_POWER_PORT					0
#define LED_POWER_PIN					15
#define LED_GREEN_PORT					1
#define LED_GREEN_PIN					5
#define LED_USB_PORT					0
#define LED_USB_PIN						14

#define LED_POWER_INIT()				do {\
											LED_POWER_OFF();\
											core_interfaces.gpio.init(LED_POWER_PORT);\
											core_interfaces.gpio.config_pin(LED_POWER_PORT, LED_POWER_PIN, GPIO_OUTPP);\
										} while (0)
#define LED_POWER_ON()					core_interfaces.gpio.out(LED_POWER_PORT, 1 << LED_POWER_PIN, 0)
#define LED_POWER_OFF()					core_interfaces.gpio.out(LED_POWER_PORT, 1 << LED_POWER_PIN, 1 << LED_POWER_PIN)

#define LED_STATE_INIT()				do {\
											LED_STATE_G_OFF();\
											core_interfaces.gpio.init(LED_GREEN_PORT);\
											core_interfaces.gpio.config_pin(LED_GREEN_PORT, LED_GREEN_PIN, GPIO_OUTPP);\
										} while (0)
#define LED_STATE_G_ON()				core_interfaces.gpio.out(LED_GREEN_PORT, 1 << LED_GREEN_PIN, 0)
#define LED_STATE_G_OFF()				core_interfaces.gpio.out(LED_GREEN_PORT, 1 << LED_GREEN_PIN, 1 << LED_GREEN_PIN)

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
#define USB_PULL_PORT					2
#define USB_PULL_PIN					13

#define USB_Pull_Init()					do{\
											core_interfaces.gpio.init(USB_PULL_PORT);\
											core_interfaces.gpio.clear(USB_PULL_PORT, 1 << USB_PULL_PIN);\
											core_interfaces.gpio.config_pin(USB_PULL_PORT, USB_PULL_PIN, GPIO_OUTPP);\
										} while (0)
#define USB_Connect()					core_interfaces.gpio.set(USB_PULL_PORT, 1 << USB_PULL_PIN)
#define USB_Disconnect()				core_interfaces.gpio.clear(USB_PULL_PORT, 1 << USB_PULL_PIN)
