INCLUDES = \
    $(all_includes) \
    -I$(top_srcdir)/src/vsf_cfg \
    -I$(top_srcdir)/vsf/compiler/GCC \
    -I$(top_srcdir)/vsf/interfaces/cpu/pc \
    -I$(top_srcdir)/src \
    -I$(top_srcdir)/src/driver \
    -I$(top_srcdir)/src/programmer \
    -I$(top_srcdir)/src/programmer/interfaces \
    -I$(top_srcdir)/src/scripts \
    -I$(top_srcdir)/src/tools/strparser

METASOURCES = AUTO
noinst_LIBRARIES = libvsf.a

libvsf_a_SOURCES = \
    dal/dal.c \
    dal/mal/mal.c \
    dal/mic2826/mic2826_drv.c \
    dal/ee93cx6/ee93cx6_drv.c \
    dal/ee24cxx/ee24cxx_drv.c \
    dal/df25xx/df25xx_drv.c \
    dal/df45xx/df45xx_drv.c \
    dal/sd/sd_spi_drv.c \
    dal/sd/sd_common.c \
    dal/cfi/cfi_drv.c \
    dal/nand/nand_drv.c \
    dal/nrf24l01/nrf24l01_drv.c \
    dal/sst32hfxx/sst32hfxx_drv.c

MAINTAINERCLEANFILES = Makefile.in
