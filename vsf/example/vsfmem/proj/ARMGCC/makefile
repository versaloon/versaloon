# options and settings
PROJECT	= demo

#
# available OPT:
# 0 - 3: low optimization - high optimization
# s: size optimization
OPT	= s

# stm32 library configurations
TARGET_CHIP = stm32
INCLUDE_USB_LIB = yes
FLASH_LOAD_OFFSET = 0x0000
HSE_VALUE = 8000000
LD_FILE = VSFCore_stm32.ld
TARGET_STM32 = HighDensity

# directory define
PRJDIR	    = ..
SRCDIR      = ../..
VSFDIR      = ../../../../../vsf
BSPDIR      = $(VSFDIR)/hardware/core/stm32/bsp
VSFCFGDIR   = $(BSPDIR)/vsf_cfg
OBJ_DIR     = .
COMPILER_DIR = $(VSFDIR)/compiler/ARMGCC
STM32_IFACES_DIR = $(VSFDIR)/interfaces/cpu/stm32
STM32_LIB_DIR = $(STM32_IFACES_DIR)/hw/STM32F10x_Lib

# User source files/includes/macros
# add .c source files to USR_SRCS
# add .h header files to USR_INCS
# add pre-defines to USR_DEFS
# add library directory to USR_LIBS
USR_SRCS+= $(SRCDIR)/main.c $(SRCDIR)/app_log.c
USR_INCS+= -I $(SRCDIR)
USR_INCS+= -I $(STM32_IFACES_DIR)/hw
USR_DEFS+= -D__TARGET_CHIP__=$(TARGET_CHIP) -DFLASH_LOAD_OFFSET=$(FLASH_LOAD_OFFSET) -DCORE_DEBUG=STM32_DBG_SWD -DHSE_VALUE=$(HSE_VALUE)
#USR_LIBS+= -L

# vsfmem ext board
USR_SRCS+= $(VSFDIR)/hardware/ext/memory/bsp/vsfmem.c
USR_INCS+= -I $(VSFDIR)/hardware/ext/memory/bsp/

# compiler
USR_SRCS+= $(COMPILER_DIR)/compiler_string.c $(COMPILER_DIR)/syscall.c
USR_INCS+= -I $(COMPILER_DIR) -I .

#APP interfaces
USR_SRCS+= $(BSPDIR)/app_interfaces.c
USR_INCS+= -I $(BSPDIR) -I $(BSPDIR)/APP

USR_SRCS+= $(BSPDIR)/APP/GPIO/GPIO.c \
  $(BSPDIR)/APP/USART/USART.c \
  $(BSPDIR)/APP/SPI/SPI.c \
  $(BSPDIR)/APP/SDIO/SDIO.c \
  $(BSPDIR)/APP/IIC/IIC.c \
  $(BSPDIR)/APP/EBI/EBI.c \
  $(BSPDIR)/APP/ADC/ADC.c
USR_INCS+= -I $(BSPDIR) \
  -I $(BSPDIR)/APP/GPIO \
  -I $(BSPDIR)/APP/USART \
  -I $(BSPDIR)/APP/SPI \
  -I $(BSPDIR)/APP/IIC \
  -I $(BSPDIR)/APP/EBI \
  -I $(BSPDIR)/APP/ADC

#VSF config
USR_INCS+= -I $(VSFCFGDIR)

#VSF interfaces
USR_SRCS+= $(VSFDIR)/interfaces/interfaces.c
USR_INCS+= -I $(VSFDIR) -I $(VSFDIR)/interfaces

USR_SRCS+= $(STM32_IFACES_DIR)/core.c \
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
USR_SRCS+= $(VSFDIR)/dal/dal.c \
  $(VSFDIR)/dal/df25xx/df25xx_drv.c \
  $(VSFDIR)/dal/df45xx/df45xx_drv.c \
  $(VSFDIR)/dal/ee24cxx/ee24cxx_drv.c \
  $(VSFDIR)/dal/sd/sd_common.c \
  $(VSFDIR)/dal/sd/sd_spi_drv.c \
  $(VSFDIR)/dal/sd/sd_sdio_drv.c \
  $(VSFDIR)/dal/mal/mal.c \
  $(VSFDIR)/dal/mic2826/mic2826_drv.c \
  $(VSFDIR)/dal/nrf24l01/nrf24l01_drv.c \
  $(VSFDIR)/dal/stream/stream.c \
  $(VSFDIR)/dal/usart_stream/usart_stream.c \
  $(VSFDIR)/dal/cfi/cfi_drv.c \
  $(VSFDIR)/dal/nand/nand_drv.c \
# MicroWire is not supported, so no ee93cx6 support
#  $(VSFDIR)/dal/ee93cx6/ee93cx6_drv.c

#VSF stack
USR_SRCS+= $(VSFDIR)/stack/usb_device/vsf_usbd.c \
  $(VSFDIR)/stack/usb_device/class/CDC/vsfusbd_CDCACM.c \
  $(VSFDIR)/stack/usb_device/class/HID/vsfusbd_HID.c \
  $(VSFDIR)/stack/usb_device/class/MSC/vsfusbd_MSC_BOT.c \
  $(VSFDIR)/stack/usb_device/class/MSC/SCSI.c

#VSF tool
USR_SRCS+= $(VSFDIR)/tool/buffer/buffer.c
USR_INCS+= -I $(VSFDIR)/tool/list

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

MAIN_OUT = $(PROJECT)_GCC
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
