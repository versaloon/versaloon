# options and settings
PROJECT	= Versaloon
EMBEDDED_VSPROG_EN = no

# 
# To select your board , set HW_BOARD to one of the below boards
# Available boards:
# NanoRelease1
# MiniRelease1
# ProRelease1(same for ProRelease3)
# STBee_Mini
# STM8S_Discovery
# STM8L_Discovery
# STM8SVL_Discovery
# STM32VL_Discovery
# STM32L_Discovery
# STM32F4_Discovery
# ST_Link
# STM32F0_Discovery
#
HW_BOARD = MiniRelease1

#
include	../../board_defs.mk
#

#
# available OPT:
# 0 - 3: low optimization - high optimization
# s: size optimization
OPT	= s

# set to yes to include USB library
INCLUDE_USB_LIB = yes

# directory define
PRJDIR	    = ..
SRCDIR      = ../../..
VSFDIR      = ../../../../../vsf
VSPROGDIR	= ../../../../../src
VSFCFGDIR   = ../vsf_cfg
OBJ_DIR     = .
COMPILER_DIR = .
APP_IFACES_DIR = $(SRCDIR)/Interfaces/APP
STM32_IFACES_DIR = $(VSFDIR)/interfaces/cpu/stm32
STM32_LIB_DIR = $(STM32_IFACES_DIR)/hw/STM32F10x_Lib

# User source files/includes/macros
# add .c source files to USR_SRCS
# add .h header files to USR_INCS
# add pre-defines to USR_DEFS
# add library directory to USR_LIBS
USR_SRCS+= $(SRCDIR)/main.c $(SRCDIR)/usb_protocol_Versaloon.c $(SRCDIR)/versaloon_usb/vsfusbd_Versaloon.c
USR_INCS+= -I $(PRJDIR)/.. -I $(SRCDIR)/versaloon_usb
USR_INCS+= -I $(STM32_IFACES_DIR)/hw
# compiler.h
USR_INCS+= -I $(VSFDIR)/compiler/ARMGCC
#USR_DEFS+= -DADD_USER_DEFINES_HERE
#USR_LIBS+= -L./

# compiler
USR_SRCS+= $(COMPILER_DIR)/compiler_string.c $(COMPILER_DIR)/syscall.c
USR_INCS+= -I $(COMPILER_DIR)

#stlink
USR_SRCS+= $(SRCDIR)/stlink/stlink.c
USR_INCS+= -I $(SRCDIR)/stlink
ifeq (${EMBEDDED_VSPROG_EN},no)
#add files needed by stlink
USR_SRCS+= $(VSPROGDIR)/target/core/arm_adi/adi_v5p1.c $(VSPROGDIR)/target/cortex-m/cm_common.c
USR_INCS+= -I $(VSPROGDIR)/target/core/arm_adi -I $(VSPROGDIR)/target/cortex-m
endif

#vsprog
ifeq (${EMBEDDED_VSPROG_EN},yes)
USR_SRCS+= $(SRCDIR)/app_io.c \
  $(VSPROGDIR)/scripts/scripts.c \
  $(VSPROGDIR)/scripts/interfaces_script.c \
  $(SRCDIR)/app_script/app_script.c \
  $(SRCDIR)/vsprog/vsprog.c \
  $(SRCDIR)/vsprog/driver/comport/comport.c \
  $(SRCDIR)/vsprog/tools/pgbar/pgbar.c \
  $(VSPROGDIR)/tools/memlist/memlist.c \
  $(VSPROGDIR)/tools/strparser/strparser.c \
  $(VSPROGDIR)/target/target.c \
  $(SRCDIR)/vsprog/target/target_data.c \
  $(SRCDIR)/vsprog/target/target_readconf.c \
  $(VSPROGDIR)/target/at89s5x/at89s5x.c \
  $(VSPROGDIR)/target/at91sam3/at91sam3.c \
  $(VSPROGDIR)/target/avr32/avr32.c \
  $(VSPROGDIR)/target/avr8/avr8.c \
  $(VSPROGDIR)/target/avr8/avr8_hvpp.c \
  $(VSPROGDIR)/target/avr8/avr8_hvsp.c \
  $(VSPROGDIR)/target/avr8/avr8_isp.c \
  $(VSPROGDIR)/target/avr8/avr8_jtag.c \
  $(VSPROGDIR)/target/avrxmega/avrxmega.c \
  $(VSPROGDIR)/target/c8051f/c8051f.c \
  $(VSPROGDIR)/target/c8051f/c8051f_c2.c \
  $(VSPROGDIR)/target/c8051f/c8051f_jtag.c \
  $(VSPROGDIR)/target/cfi/cfi.c \
  $(VSPROGDIR)/target/comisp/comisp.c \
  $(VSPROGDIR)/target/comisp/lpcarmisp.c \
  $(VSPROGDIR)/target/comisp/stm32isp.c \
  $(VSPROGDIR)/target/core/arm_adi/adi_v5p1.c \
  $(VSPROGDIR)/target/cortex-m/cm.c \
  $(VSPROGDIR)/target/cortex-m/cm_common.c \
  $(VSPROGDIR)/target/cortex-m/cm_at91sam3.c \
  $(VSPROGDIR)/target/cortex-m/cm_lm3s.c \
  $(VSPROGDIR)/target/cortex-m/cm_lpc1000.c \
  $(VSPROGDIR)/target/cortex-m/cm_stm32_fl.c \
  $(VSPROGDIR)/target/cortex-m/cm_stm32f1.c \
  $(VSPROGDIR)/target/cortex-m/cm_stm32f2.c \
  $(VSPROGDIR)/target/cortex-m/cm_stm32l1.c \
  $(VSPROGDIR)/target/df25xx/df25xx.c \
  $(VSPROGDIR)/target/ee24cxx/ee24cxx.c \
  $(VSPROGDIR)/target/ee93cx6/ee93cx6.c \
  $(VSPROGDIR)/target/hcs08/hcs08.c \
  $(VSPROGDIR)/target/hcs12/hcs12.c \
  $(VSPROGDIR)/target/lm3s/lm3s.c \
  $(VSPROGDIR)/target/lpc1000/lpc1000.c \
  $(VSPROGDIR)/target/lpc900/lpc900.c \
  $(VSPROGDIR)/target/msp430/JTAGfunc.c \
  $(VSPROGDIR)/target/msp430/msp430.c \
  $(VSPROGDIR)/target/msp430/msp430_bsl.c \
  $(VSPROGDIR)/target/msp430/msp430_jtagsbw.c \
  $(VSPROGDIR)/target/nand/nand.c \
  $(VSPROGDIR)/target/psoc1/psoc1.c \
  $(VSPROGDIR)/target/sd/sd.c \
  $(VSPROGDIR)/target/stm32f1/stm32f1.c \
  $(VSPROGDIR)/target/stm32f2/stm32f2.c \
  $(VSPROGDIR)/target/stm32l1/stm32l1.c \
  $(VSPROGDIR)/target/stm8/stm8.c \
  $(VSPROGDIR)/target/svf_player/byte_tap.c \
  $(VSPROGDIR)/target/svf_player/svf_parser.c \
  $(VSPROGDIR)/target/svf_player/svf_player.c \
  $(VSPROGDIR)/target/sst32hfxx/sst32hfxx.c
USR_INCS+= -I $(VSPROGDIR)/scripts \
  -I $(SRCDIR)/app_script \
  -I $(SRCDIR)/vsprog \
  -I $(SRCDIR)/vsprog/driver/comport \
  -I $(SRCDIR)/vsprog/tools/pgbar \
  -I $(VSPROGDIR)/tools/memlist \
  -I $(VSPROGDIR)/tools/strparser \
  -I $(VSPROGDIR)/target \
  -I $(VSPROGDIR)/target/core/arm_adi \
  -I $(VSPROGDIR)/target/comisp \
  -I $(VSPROGDIR)/target/cortex-m \
  -I $(VSPROGDIR)/target/at91sam3 \
  -I $(VSPROGDIR)/target/lm3s \
  -I $(VSPROGDIR)/target/lpc1000 \
  -I $(VSPROGDIR)/target/stm32f1 \
  -I $(VSPROGDIR)/target/stm32f2 \
  -I $(VSPROGDIR)/target/stm32l1
endif

#APP interfaces
USR_SRCS+= $(SRCDIR)/Interfaces/app_interfaces.c
USR_INCS+= -I $(SRCDIR)/Interfaces

USR_SRCS+= $(APP_IFACES_DIR)/GPIO/GPIO.c \
  $(APP_IFACES_DIR)/CLKO/CLKO.c \
  $(APP_IFACES_DIR)/USART/USART.c \
  $(APP_IFACES_DIR)/SPI/SPI.c \
  $(APP_IFACES_DIR)/SDIO/SDIO.c \
  $(APP_IFACES_DIR)/JTAG/JTAG_TAP.c \
  $(APP_IFACES_DIR)/SWD/SWD.c \
  $(APP_IFACES_DIR)/SWIM/SWIM.c \
  $(APP_IFACES_DIR)/DUSI/DUSI.c \
  $(APP_IFACES_DIR)/MicroWire/MicroWire.c \
  $(APP_IFACES_DIR)/PowerExt/PowerExt.c \
  $(APP_IFACES_DIR)/IIC/IIC.c \
  $(APP_IFACES_DIR)/C2/C2.c \
  $(APP_IFACES_DIR)/ISSP/ISSP.c \
  $(APP_IFACES_DIR)/LPC_ICP/LPC_ICP.c \
  $(APP_IFACES_DIR)/MSP430_JTAG/MSP430_JTAG.c \
  $(APP_IFACES_DIR)/BDM/BDM.c \
  $(APP_IFACES_DIR)/PWM/PWM.c \
  $(APP_IFACES_DIR)/EBI/EBI.c \
  $(APP_IFACES_DIR)/ADC/ADC.c
USR_INCS+= -I $(APP_IFACES_DIR) \
  -I $(APP_IFACES_DIR)/GPIO \
  -I $(APP_IFACES_DIR)/USART \
  -I $(APP_IFACES_DIR)/SPI \
  -I $(APP_IFACES_DIR)/JTAG \
  -I $(APP_IFACES_DIR)/SWD \
  -I $(APP_IFACES_DIR)/SWIM \
  -I $(APP_IFACES_DIR)/DUSI \
  -I $(APP_IFACES_DIR)/MicroWire \
  -I $(APP_IFACES_DIR)/PowerExt \
  -I $(APP_IFACES_DIR)/IIC \
  -I $(APP_IFACES_DIR)/C2 \
  -I $(APP_IFACES_DIR)/ISSP \
  -I $(APP_IFACES_DIR)/LPC_ICP \
  -I $(APP_IFACES_DIR)/MSP430_JTAG \
  -I $(APP_IFACES_DIR)/BDM \
  -I $(APP_IFACES_DIR)/PWM \
  -I $(APP_IFACES_DIR)/EBI \
  -I $(APP_IFACES_DIR)/ADC

#VSF config
USR_INCS+= -I $(VSFCFGDIR)

#VSF interfaces
USR_SRCS+= $(VSFDIR)/interfaces/interfaces.c
USR_INCS+= -I $(VSFDIR) -I $(VSFDIR)/interfaces

USR_SRCS+= $(STM32_IFACES_DIR)/core.c \
  $(STM32_IFACES_DIR)/clko/STM32_CLKO.c \
  $(STM32_IFACES_DIR)/gpio/STM32_GPIO.c \
  $(STM32_IFACES_DIR)/usbd/STM32_USBD.c \
  $(STM32_IFACES_DIR)/spi/STM32_SPI.c \
  $(STM32_IFACES_DIR)/timer/STM32_TIM.c \
  $(STM32_IFACES_DIR)/eint/STM32_EINT.c \
  $(STM32_IFACES_DIR)/adc/STM32_ADC.c \
  $(STM32_IFACES_DIR)/usart/STM32_USART.c \
  $(STM32_IFACES_DIR)/ebi/STM32_EBI.c \
  $(STM32_IFACES_DIR)/flash/STM32_FLASH.c \
  $(STM32_IFACES_DIR)/sdio/STM32_SDIO.c
USR_INCS+= -I $(STM32_IFACES_DIR) \
  -I $(STM32_IFACES_DIR)/gpio \
  -I $(STM32_IFACES_DIR)/usbd \
  -I $(STM32_IFACES_DIR)/spi \
  -I $(STM32_IFACES_DIR)/timer \
  -I $(STM32_IFACES_DIR)/eint \
  -I $(STM32_IFACES_DIR)/adc \
  -I $(STM32_IFACES_DIR)/usart \
  -I $(STM32_IFACES_DIR)/ebi \
  -I $(STM32_IFACES_DIR)/flash \
  -I $(STM32_IFACES_DIR)/sdio

#VSF dal
ifeq (${EMBEDDED_VSPROG_EN},yes)
USR_SRCS+= $(VSFDIR)/dal/dal.c \
  $(VSFDIR)/dal/df25xx/df25xx_drv.c \
  $(VSFDIR)/dal/df45xx/df45xx_drv.c \
  $(VSFDIR)/dal/ee24cxx/ee24cxx_drv.c \
  $(VSFDIR)/dal/ee93cx6/ee93cx6_drv.c \
  $(VSFDIR)/dal/sd/sd_common.c \
  $(VSFDIR)/dal/sd/sd_spi_drv.c \
  $(VSFDIR)/dal/sd/sd_sdio_drv.c \
  $(VSFDIR)/dal/mal/mal.c \
  $(VSFDIR)/dal/mic2826/mic2826_drv.c \
  $(VSFDIR)/dal/nrf24l01/nrf24l01_drv.c \
  $(VSFDIR)/dal/stream/stream.c \
  $(VSFDIR)/dal/usart_stream/usart_stream.c
else
USR_SRCS+= $(VSFDIR)/dal/usart_stream/usart_stream.c $(VSFDIR)/dal/stream/stream.c
USR_SRCS+= $(VSFDIR)/dal/mal/mal.c
endif

#VSF stack
USR_SRCS+= $(VSFDIR)/stack/usb_device/vsf_usbd.c \
  $(VSFDIR)/stack/usb_device/class/CDC/vsfusbd_CDC.c \
  $(VSFDIR)/stack/usb_device/class/CDC/vsfusbd_CDCACM.c \
  $(VSFDIR)/stack/usb_device/class/HID/vsfusbd_HID.c \
  $(VSFDIR)/stack/usb_device/class/MSC/vsfusbd_MSC_BOT.c \
  $(VSFDIR)/stack/usb_device/class/MSC/SCSI.c

#VSF tool
USR_SRCS+= $(VSFDIR)/tool/buffer/buffer.c $(VSFDIR)/tool/fakefat32/fakefat32.c
USR_INCS+= -I $(VSFDIR)/tool/list

# USB_TO_XXX
USR_SRCS+= $(SRCDIR)/USB_TO_XXX/USB_TO_XXX.c \
  $(SRCDIR)/USB_TO_XXX/USB_TO_CLKO.c \
  $(SRCDIR)/USB_TO_XXX/USB_TO_USART.c \
  $(SRCDIR)/USB_TO_XXX/USB_TO_SPI.c \
  $(SRCDIR)/USB_TO_XXX/USB_TO_IIC.c \
  $(SRCDIR)/USB_TO_XXX/USB_TO_GPIO.c \
  $(SRCDIR)/USB_TO_XXX/USB_TO_PWM.c \
  $(SRCDIR)/USB_TO_XXX/USB_TO_ADC.c \
  $(SRCDIR)/USB_TO_XXX/USB_TO_DAC.c \
  $(SRCDIR)/USB_TO_XXX/USB_TO_POWER.c \
  $(SRCDIR)/USB_TO_XXX/USB_TO_C2.c \
  $(SRCDIR)/USB_TO_XXX/USB_TO_ISSP.c \
  $(SRCDIR)/USB_TO_XXX/USB_TO_LPCICP.c \
  $(SRCDIR)/USB_TO_XXX/USB_TO_MSP430_JTAG.c \
  $(SRCDIR)/USB_TO_XXX/USB_TO_JTAG_HL.c \
  $(SRCDIR)/USB_TO_XXX/USB_TO_SWD.c \
  $(SRCDIR)/USB_TO_XXX/USB_TO_JTAG_LL.c \
  $(SRCDIR)/USB_TO_XXX/USB_TO_SWIM.c \
  $(SRCDIR)/USB_TO_XXX/USB_TO_JTAG_RAW.c \
  $(SRCDIR)/USB_TO_XXX/USB_TO_BDM.c \
  $(SRCDIR)/USB_TO_XXX/USB_TO_DUSI.c \
  $(SRCDIR)/USB_TO_XXX/USB_TO_MICROWIRE.c \
  $(SRCDIR)/USB_TO_XXX/USB_TO_EBI.c \
  $(SRCDIR)/USB_TO_XXX/CommandProcessor.c
USR_INCS+= -I $(SRCDIR)/USB_TO_XXX

# LIBSTM32_LIB
ifeq ($(TARGET_STM32),LowDensity)
  LIBSTM32_ASM_SRCS = \
    $(STM32_LIB_DIR)/Libraries/CMSIS/CM3/DeviceSupport/ST/STM32F10x/startup/gcc_ride7/startup_stm32f10x_ld.s
  LIBSTM32_DEFS = -DUSE_STDPERIPH_DRIVER -DSTM32F10X_LD
else
  ifeq ($(TARGET_STM32),LowDensityValueLine)
    LIBSTM32_ASM_SRCS = \
      $(STM32_LIB_DIR)/Libraries/CMSIS/CM3/DeviceSupport/ST/STM32F10x/startup/gcc_ride7/startup_stm32f10x_ld_vl.s
    LIBSTM32_DEFS = -DUSE_STDPERIPH_DRIVER -DSTM32F10X_LD_VL
  else
    ifeq ($(TARGET_STM32),MediumDensity)
	  LIBSTM32_ASM_SRCS = \
        $(STM32_LIB_DIR)/Libraries/CMSIS/CM3/DeviceSupport/ST/STM32F10x/startup/gcc_ride7/startup_stm32f10x_md.s
      LIBSTM32_DEFS = -DUSE_STDPERIPH_DRIVER -DSTM32F10X_MD
	else
	  ifeq ($(TARGET_STM32),MediumDensityValueLine)
	    LIBSTM32_ASM_SRCS = \
          $(STM32_LIB_DIR)/Libraries/CMSIS/CM3/DeviceSupport/ST/STM32F10x/startup/gcc_ride7/startup_stm32f10x_md_vl.s
        LIBSTM32_DEFS = -DUSE_STDPERIPH_DRIVER -DSTM32F10X_MD_VL
	  else
	    ifeq ($(TARGET_STM32),HighDensity)
		  LIBSTM32_ASM_SRCS = \
            $(STM32_LIB_DIR)/Libraries/CMSIS/CM3/DeviceSupport/ST/STM32F10x/startup/gcc_ride7/startup_stm32f10x_hd.s
          LIBSTM32_DEFS = -DUSE_STDPERIPH_DRIVER -DSTM32F10X_HD
		else
		  ifeq ($(TARGET_STM32),ConnectivityLine)
		    LIBSTM32_ASM_SRCS = \
              $(STM32_LIB_DIR)/Libraries/CMSIS/CM3/DeviceSupport/ST/STM32F10x/startup/gcc_ride7/startup_stm32f10x_cl.s
            LIBSTM32_DEFS = -DUSE_STDPERIPH_DRIVER -DSTM32F10X_CL
		  else
		    ifeq ($(TARGET_STM32),XLDensity)
			  LIBSTM32_ASM_SRCS = \
                $(STM32_LIB_DIR)/Libraries/CMSIS/CM3/DeviceSupport/ST/STM32F10x/startup/gcc_ride7/startup_stm32f10x_md.s
              LIBSTM32_DEFS = -DUSE_STDPERIPH_DRIVER -DSTM32F10X_XL
			else
		      Invalid STM32 Target
			endif
		  endif
		endif
	  endif
	endif
  endif
endif
LIBSTM32_SRCS = \
  $(STM32_LIB_DIR)/Libraries/CMSIS/CM3/CoreSupport/core_cm3.c \
  $(STM32_LIB_DIR)/Libraries/CMSIS/CM3/DeviceSupport/ST/STM32F10x/system_stm32f10x.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/misc.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_dma.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_gpio.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_rcc.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_spi.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_tim.c
LIBSTM32_INCS = \
  -I $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/inc \
  -I $(STM32_LIB_DIR)/Libraries/CMSIS/CM3/DeviceSupport/ST/STM32F10x \
  -I $(STM32_LIB_DIR)/Libraries/CMSIS/CM3/CoreSupport

# LIBSTM32_USB_LIB
ifeq (${INCLUDE_USB_LIB},yes)
  LIBSTM32_SRCS+= \
	$(STM32_LIB_DIR)/Libraries/STM32_USB-FS-Device_Driver/src/usb_mem.c \
	$(STM32_LIB_DIR)/Libraries/STM32_USB-FS-Device_Driver/src/usb_regs.c
  LIBSTM32_INCS+= \
    -I $(STM32_LIB_DIR)/Libraries/STM32_USB-FS-Device_Driver/inc
endif


LIBSTM32_OBJS = $(addprefix $(OBJ_DIR)/, $(notdir $(LIBSTM32_SRCS:.c=.o)))
USR_OBJS = $(addprefix $(OBJ_DIR)/, $(notdir $(USR_SRCS:.c=.o)))
ASM_SRCS = $(LIBSTM32_ASM_SRCS)
ASM_OBJS = $(addprefix $(OBJ_DIR)/, $(notdir $(LIBSTM32_ASM_SRCS:.s=.o)))


TCHAIN = arm-none-eabi

INCLUDE_DIRS = -I $(PRJDIR) -I $(SRCDIR) $(LIBSTM32_INCS) $(USR_INCS)
COMPILE_OPTS = -mcpu=cortex-m3 -mthumb -mfpu=vfp 
COMPILE_OPTS+= -nostartfiles -fno-common -fomit-frame-pointer -Wall -g -O$(OPT)
LIBRARY_DIRS = $(USR_LIBS)
PREDEFINES   = $(LIBSTM32_DEFS) $(USR_DEFS)

CC = $(TCHAIN)-gcc
CFLAGS = $(COMPILE_OPTS) $(INCLUDE_DIRS) $(PREDEFINES)

CXX = $(TCHAIN)-g++
CXXFLAGS = $(COMPILE_OPTS) $(INCLUDE_DIRS) $(PREDEFINES)

AS = $(TCHAIN)-gcc
ASFLAGS = $(COMPILE_OPTS) -c

LD = $(TCHAIN)-gcc
LDFLAGS = -Wl,--gc-sections,-Map=$(MAIN_OUT).map,-cref,-u,Reset_Handler $(INCLUDE_DIRS) $(LIBRARY_DIRS) -T $(LD_FILE)

OBJCP = $(TCHAIN)-objcopy

AR = $(TCHAIN)-ar
ARFLAGS = cr

MAIN_OUT = $(PROJECT)_GCC-$(HW_BOARD)-$(FLASH_LOAD_OFFSET)
MAIN_OUT_ELF = $(MAIN_OUT).elf
MAIN_OUT_BIN = $(MAIN_OUT).bin
MAIN_OUT_HEX = $(MAIN_OUT).hex

MAIN_OUT_PROG = $(MAIN_OUT_HEX)

STR_DIV	= ------------------------------------------------------------

# all

all: start gcc-info $(MAIN_OUT_ELF) $(MAIN_OUT_PROG) $(MAIN_OUT_BIN) size flash end

# misc

start:
	@echo Start Compiling Project $(PROJECT)
	@echo User Files:
	@echo $(USR_OBJS)
	@echo STM32 Library Files:
	@echo $(LIBSTM32_OBJS)

end:
	@echo $(STR_DIV)
	@echo bye!!

gcc-info:
	@echo $(STR_DIV)
	@echo gcc version is
	@$(CC) --version
	@echo $(STR_DIV)

# main

$(MAIN_OUT_ELF): $(ASM_OBJS) $(USR_OBJS) $(LIBSTM32_OBJS)
	$(LD) $(LDFLAGS) $(ASM_OBJS) $(LIBSTM32_OBJS) $(USR_OBJS) --output $@

$(MAIN_OUT_BIN): $(MAIN_OUT_ELF)
	$(OBJCP) -O binary $< $@

$(MAIN_OUT_HEX): $(MAIN_OUT_ELF)
	$(OBJCP) -O ihex $< $@

size: $(MAIN_OUT_ELF)
	@echo $(STR_DIV)
	size $(MAIN_OUT_ELF)

flash: $(MAIN_OUT_PROG)
#	@echo $(STR_DIV)
#	vsprog -sstm32 -G -mj -I $(MAIN_OUT_HEX) -oe -owf -ovf

$(ASM_OBJS): $(ASM_SRCS)
	$(CC) -c $(CFLAGS) $^

$(LIBSTM32_OBJS): $(LIBSTM32_SRCS)
	$(CC) -c $(CFLAGS) $^

$(USR_OBJS): $(USR_SRCS)
	$(CC) -c $(CFLAGS) $^

clean:
	-rm -f $(USR_OBJS) $(MAIN_OUT).map $(ASM_OBJS) $(LIBSTM32_OBJS) $(MAIN_OUT_ELF) $(MAIN_OUT_PROG) $(MAIN_OUT_BIN)
