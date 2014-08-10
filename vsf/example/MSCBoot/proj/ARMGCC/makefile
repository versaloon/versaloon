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
HSE_VALUE = 12000000
LD_FILE = stm32.ld
TARGET_STM32 = XLDensity

# directory define
PRJDIR	    = ..
SRCDIR      = ../..
VSFDIR      = ../../../../../vsf
VSFCFGDIR   = $(SRCDIR)/vsf_cfg
OBJ_DIR     = .
COMPILER_DIR = $(VSFDIR)/compiler/ARMGCC
STM32_IFACES_DIR = $(VSFDIR)/interfaces/cpu/stm32
STM32_LIB_DIR = $(STM32_IFACES_DIR)/hw/STM32F10x_Lib

# User source files/includes/macros
# add .c source files to USR_SRCS
# add .h header files to USR_INCS
# add pre-defines to USR_DEFS
# add library directory to USR_LIBS
USR_SRCS+= $(SRCDIR)/main.c
USR_INCS+= -I $(SRCDIR)
USR_INCS+= -I $(STM32_IFACES_DIR)/hw
USR_DEFS+= -D__TARGET_CHIP__=$(TARGET_CHIP) -DFLASH_LOAD_OFFSET=$(FLASH_LOAD_OFFSET) -DCORE_DEBUG=STM32_DBG_NONE -DHSE_VALUE=$(HSE_VALUE)
#USR_LIBS+= -L

# compiler
#USR_SRCS+= $(COMPILER_DIR)/compiler_string.c $(COMPILER_DIR)/syscall.c
USR_INCS+= -I $(COMPILER_DIR) -I .

#VSF config
USR_INCS+= -I $(VSFCFGDIR)

#VSF interfaces
USR_SRCS+= $(VSFDIR)/interfaces/interfaces.c
USR_INCS+= -I $(VSFDIR) -I $(VSFDIR)/interfaces

USR_SRCS+= $(STM32_IFACES_DIR)/core.c \
  $(STM32_IFACES_DIR)/gpio/STM32_GPIO.c \
  $(STM32_IFACES_DIR)/usbd/STM32_USBD.c \
  $(STM32_IFACES_DIR)/flash/STM32_FLASH.c \
  $(STM32_IFACES_DIR)/ebi/STM32_EBI.c
USR_INCS+= -I $(STM32_IFACES_DIR) \
  -I $(STM32_IFACES_DIR)/gpio \
  -I $(STM32_IFACES_DIR)/usbd \
  -I $(STM32_IFACES_DIR)/flash

#VSF dal
USR_SRCS+= $(VSFDIR)/dal/mal/mal.c

#VSF stack
USR_SRCS+= $(VSFDIR)/stack/usb_device/vsf_usbd.c \
  $(VSFDIR)/stack/usb_device/class/MSC/vsfusbd_MSC_BOT.c \
  $(VSFDIR)/stack/usb_device/class/MSC/SCSI.c

#VSF tool
USR_SRCS+= $(VSFDIR)/tool/buffer/buffer.c $(VSFDIR)/tool/fakefat32/fakefat32.c
USR_SRCS+= $(VSFDIR)/tool/mal_embflash/mal_embflash.c $(VSFDIR)/tool/mal_in_mal/mal_in_mal.c
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
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_rcc.c \
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
