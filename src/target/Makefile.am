INCLUDES = \
    $(all_includes) \
    -I$(top_srcdir)/src/vsf_cfg \
    -I$(top_srcdir)/vsf \
    -I$(top_srcdir)/vsf/compiler/GCC \
    -I$(top_srcdir)/vsf/interfaces/cpu/pc \
    -I$(top_srcdir)/vsf/tool/list \
    -I$(top_srcdir)/src \
    -I$(top_srcdir)/src/programmer \
    -I$(top_srcdir)/src/programmer/interfaces \
    -I$(top_srcdir)/src/tools/memlist \
    -I$(top_srcdir)/src/tools/filelist \
    -I$(top_srcdir)/src/tools/fileparser \
    -I$(top_srcdir)/src/tools/pgbar \
    -I$(top_srcdir)/src/tools/strparser \
    -I$(top_srcdir)/src/tools/bufffunc \
    -I$(top_srcdir)/src/driver \
    -I$(top_srcdir)/src/driver/comport \
    -I$(top_srcdir)/src/target/core/arm_adi \
    -I$(top_srcdir)/src/target/comisp \
    -I$(top_srcdir)/src/target/cortex-m \
    -I$(top_srcdir)/src/target/stm32f1 \
    -I$(top_srcdir)/src/target/stm32f2 \
    -I$(top_srcdir)/src/target/stm32l1 \
    -I$(top_srcdir)/src/target/lpc1000 \
    -I$(top_srcdir)/src/target/at91sam3 \
    -I$(top_srcdir)/src/target/nuc100 \
    -I$(top_srcdir)/src/target/nuc400 \
    -I$(top_srcdir)/src/target/kinetis \
    -I$(top_srcdir)/src/target/avr32 \
    -I$(top_srcdir)/src/target/avrxmega \
    -I$(top_srcdir)/src/target/lm3s \
    -I$(top_srcdir)/src/target/sd \
    -I$(top_srcdir)/src/scripts \
    -I$(top_srcdir)/src/vsf

METASOURCES = AUTO
noinst_LIBRARIES = libtgt.a

libtgt_a_SOURCES = \
    target.c \
    target_conf.c \
    target_data.c \
    core/arm_adi/adi_v5p1.c \
    cortex-m/cm.c \
    cortex-m/cm_stm32_fl.c \
    cortex-m/cm_stm32f1.c \
    cortex-m/cm_stm32f2.c \
    cortex-m/cm_stm32l1.c \
    cortex-m/cm_lpc1000.c \
    cortex-m/cm_at91sam3.c \
    cortex-m/cm_lm3s.c \
    cortex-m/cm_nuc100.c \
    cortex-m/cm_nuc400.c \
    cortex-m/cm_kinetis.c \
    cortex-m/cm_common.c \
    at89s5x/at89s5x.c \
    avr8/avr8.c \
    avr8/avr8_hvpp.c \
    avr8/avr8_hvsp.c \
    avr8/avr8_isp.c \
    avr8/avr8_jtag.c \
    lpc900/lpc900.c \
    c8051f/c8051f.c \
    c8051f/c8051f_c2.c \
    c8051f/c8051f_jtag.c \
    comisp/comisp.c \
    comisp/lpcarmisp.c \
    comisp/stm32isp.c \
    msp430/JTAGfunc.c \
    msp430/msp430.c \
    msp430/msp430_jtagsbw.c \
    msp430/msp430_bsl.c \
    psoc1/psoc1.c \
    stm8/stm8.c \
    svf_player/svf_player.c \
    svf_player/byte_tap.c \
    svf_player/svf_parser.c \
    stm32f1/stm32f1.c \
    stm32f2/stm32f2.c \
    stm32l1/stm32l1.c \
    lpc1000/lpc1000.c \
    at91sam3/at91sam3.c \
    nuc100/nuc100.c \
    nuc400/nuc400.c \
    kinetis/kinetis.c \
    avr32/avr32.c \
    avrxmega/avrxmega.c \
    lm3s/lm3s.c \
    hcs08/hcs08.c \
    hcs12/hcs12.c \
    ee93cx6/ee93cx6.c \
    ee24cxx/ee24cxx.c \
    df25xx/df25xx.c \
    sd/sd.c \
    cfi/cfi.c \
    nand/nand.c \
    sst32hfxx/sst32hfxx.c

MAINTAINERCLEANFILES = Makefile.in
