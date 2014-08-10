# options and settings
PROJECT	= demo

#
# available OPT:
# 0 - 3: low optimization - high optimization
# s: size optimization
OPT	= s

# directory define
PRJDIR	    = ..
SRCDIR      = ../..
VSFDIR      = ../../../../../vsf
VSFCFGDIR   = $(VSFDIR)/example/vsf_cfg
VersaloonDIR= $(VSFDIR)/interfaces/cpu/pc
USBTOXXXDIR = $(VersaloonDIR)/usbtoxxx
OBJ_DIR     = .
COMPILER_DIR = .

# User source files/includes/macros
# add .c source files to USR_SRCS
# add .h header files to USR_INCS
# add pre-defines to USR_DEFS
# add library directory to USR_LIBS
USR_SRCS+= $(SRCDIR)/main.c
USR_INCS+= -I $(SRCDIR)
# compiler.h
USR_INCS+= -I $(VSFDIR)\compiler\GCC
#USR_DEFS+= -DADD_USER_DEFINES_HERE
#USR_LIBS+= -L./


#vsprog
USR_SRCS+= $(VersaloonDIR)/driver/usb/usbapi.c $(VersaloonDIR)/app_log.c
USR_INCS+= -I $(VersaloonDIR)/driver -I $(VersaloonDIR)/driver/usb -I $(VSFDIR)/example -I $(COMPILER_DIR) -I $(VSFCFGDIR) -I $(VSFDIR) -I $(VSFDIR)/interfaces/cpu/pc
USR_DEFS+= -DHAVE_CONFIG_H -D__TARGET_CHIP__=usbtoxxx
USR_LIBS+= -lusb

#interfaces
USR_SRCS+= $(VersaloonDIR)/versaloon.c \
  $(VersaloonDIR)/versaloon_libusb.c \
  $(USBTOXXXDIR)/usbtoadc.c \
  $(USBTOXXXDIR)/usbtoebi.c \
  $(USBTOXXXDIR)/usbtogpio.c \
  $(USBTOXXXDIR)/usbtoi2c.c \
  $(USBTOXXXDIR)/usbtomicrowire.c \
  $(USBTOXXXDIR)/usbtopwm.c \
  $(USBTOXXXDIR)/usbtospi.c \
  $(USBTOXXXDIR)/usbtousart.c \
  $(USBTOXXXDIR)/usbtoxxx.c
USR_INCS+= -I $(VersaloonDIR) -I $(USBTOXXXDIR) -I $(VSFDIR)/interfaces

#VSF dal
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
  $(VSFDIR)/dal/cfi/cfi_drv.c \
  $(VSFDIR)/dal/nand/nand_drv.c

#VSF tool
USR_SRCS+= $(VSFDIR)/tool/buffer/buffer.c
USR_INCS+= -I $(VSFDIR)/tool/list

USR_OBJS = $(addprefix $(OBJ_DIR)/, $(notdir $(USR_SRCS:.c=.o)))

TCHAIN = 

INCLUDE_DIRS = -I $(PRJDIR) -I $(SRCDIR) $(USR_INCS)
COMPILE_OPTS = -Wall -g -O$(OPT)
LIBRARY_DIRS = $(USR_LIBS)
PREDEFINES   = $(USR_DEFS)

CC = $(TCHAIN)gcc
CFLAGS = $(COMPILE_OPTS) $(INCLUDE_DIRS) $(PREDEFINES)

CXX = $(TCHAIN)g++
CXXFLAGS = $(COMPILE_OPTS) $(INCLUDE_DIRS) $(PREDEFINES)

AS = $(TCHAIN)gcc
ASFLAGS = $(COMPILE_OPTS) -c

LD = $(TCHAIN)gcc
LDFLAGS = -g -O$(OPT) -Wall $(INCLUDE_DIRS)

OBJCP = $(TCHAIN)objcopy

AR = $(TCHAIN)ar
ARFLAGS = cr

MAIN_OUT = $(PROJECT)_GCC
MAIN_OUT_EXE = $(MAIN_OUT).exe

STR_DIV	= ------------------------------------------------------------

# all

all: start gcc-info $(MAIN_OUT_EXE) end

# misc

start:
	@echo Start Compiling Project $(PROJECT)
	@echo User Files:
	@echo $(USR_OBJS)

end:
	@echo $(STR_DIV)
	@echo bye!!

gcc-info:
	@echo $(STR_DIV)
	@echo gcc version is
	@$(CC) --version
	@echo $(STR_DIV)

# main

$(MAIN_OUT_EXE): $(USR_OBJS)
	$(LD) $(LDFLAGS) -o $@ $(USR_OBJS) $(LIBRARY_DIRS)

$(MAIN_OUT_BIN): $(MAIN_OUT_EXE)
	$(OBJCP) -O binary $< $@

$(MAIN_OUT_HEX): $(MAIN_OUT_EXE)
	$(OBJCP) -O ihex $< $@

$(USR_OBJS): $(USR_SRCS)
	$(CC) -c $(CFLAGS) $^

clean:
	-rm -f $(USR_OBJS) $(MAIN_OUT).map $(MAIN_OUT_EXE)
