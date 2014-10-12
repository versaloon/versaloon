SUBDIRS = driver programmer target tools scripts ../vsf

bin_PROGRAMS = vsprog

MAINFILE = vsprog.c app_log.c
vsprog_SOURCES = $(MAINFILE)

INCLUDES = \
    $(all_includes) \
    -I$(top_srcdir)/src/vsf_cfg \
    -I$(top_srcdir)/vsf \
    -I$(top_srcdir)/vsf/compiler/GCC \
    -I$(top_srcdir)/vsf/interfaces/cpu/pc \
    -I$(top_srcdir)/vsf/tool/list \
    -I$(top_srcdir)/src \
    -I$(top_srcdir)/src/tools/pgbar \
    -I$(top_srcdir)/src/tools/memlist \
    -I$(top_srcdir)/src/tools/filelist \
    -I$(top_srcdir)/src/tools/strparser \
    -I$(top_srcdir)/src/target \
    -I$(top_srcdir)/src/driver \
    -I$(top_srcdir)/src/programmer \
    -I$(top_srcdir)/src/programmer/interfaces \
    -I$(top_srcdir)/src/scripts

vsprog_CPPFLAGS = @CPPFLAGS@
vsprog_CPPFLAGS += -DPKGBLDDATE=\"`date +%F-%R`\"

if RELEASE
  vsprog_CPPFLAGS += -DRELSTR=\"Release\" -DPKGBLDREV=\"\"
else
  vsprog_CPPFLAGS += -DRELSTR=\"svn:\" -DPKGBLDREV=\"`$(top_srcdir)/guess-rev.sh $(top_srcdir)`\"
endif

vsprog_LDFLAGS = $(all_libraries)

vsprog_LDADD = \
    $(top_builddir)/src/scripts/libscripts.a \
    $(top_builddir)/src/programmer/libprog.a \
    $(top_builddir)/src/target/libtgt.a \
    $(top_builddir)/src/tools/libtool.a \
    $(top_builddir)/src/driver/libdrv.a \
    $(top_builddir)/vsf/libvsf.a \
    $(MINGWLDADD) -lusb-1.0 $(XML_LIBS)

MAINTAINERCLEANFILES = Makefile.in
