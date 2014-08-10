INCLUDES = \
    $(all_includes) \
    -I$(top_srcdir)/src/vsf_cfg \
    -I$(top_srcdir)/vsf \
    -I$(top_srcdir)/vsf/compiler/GCC \
    -I$(top_srcdir)/vsf/interfaces/cpu/pc \
    -I$(top_srcdir)/src \
    -I$(top_srcdir)/src/programmer \
    -I$(top_srcdir)/src/programmer/interfaces \
    -I$(top_srcdir)/src/driver \
    -I$(top_srcdir)/src/tools/bufffunc \
    -I$(top_srcdir)/src/tools/strparser \
    -I$(top_srcdir)/src/sciprts/mic2826_script

METASOURCES = AUTO
noinst_LIBRARIES = libscripts.a

libscripts_a_SOURCES = \
    scripts.c \
    app_scripts.c \
    interfaces_script.c \
    mic2826_script/mic2826_script.c

#noinst_HEADERS = \
#    scripts.h

MAINTAINERCLEANFILES = Makefile.in
