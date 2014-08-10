INCLUDES = \
    $(all_includes) \
    -I$(top_srcdir)/src/vsf_cfg \
    -I$(top_srcdir)/vsf \
    -I$(top_srcdir)/vsf/compiler/GCC \
    -I$(top_srcdir)/vsf/interfaces/cpu/pc \
    -I$(top_srcdir)/vsf/tool/list \
    -I$(top_srcdir)/src/driver \
    -I$(top_srcdir)/src \
    -I$(top_srcdir)/src/tools/filelist \
    -I$(top_srcdir)/src/tools/hex \
    -I$(top_srcdir)/src/tools/s19 \
    -I$(top_srcdir)/src/tools/fileparser \
    -I$(top_srcdir)/src/tools/strparser \
    -I$(top_srcdir)/src/scripts

METASOURCES = AUTO
noinst_LIBRARIES = libtool.a

libtool_a_SOURCES = \
    hex/hex.c \
    memlist/memlist.c \
    pgbar/pgbar.c \
    filelist/filelist.c \
    fileparser/fileparser.c \
    strparser/strparser.c \
    s19/s19.c

#noinst_HEADERS = \
#    hex/hex.h \
#    memlist/memlist.h \
#    pgbar/pgbar.h \
#    filelist/filelist.h \
#    fileparser/fileparser.h

MAINTAINERCLEANFILES = Makefile.in
